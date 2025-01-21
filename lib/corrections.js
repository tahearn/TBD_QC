/**
 * `valid.changes` (R) -> for each row, if the cell value is in a set of “bad” values,
 * then set it to new_value and record a comment in “.data.change”.
 *
 * In the CSV rule, `value_changed` is typically a comma separated string of values
 * to match. E.g. "777,888"
 * The logic in R is basically:
 *
 *   if rowValue is in get_range_values(params.value_changed):
 *       rowValue = newValue
 *       commentColumn = params.comment
 *
 * @param {Object} data splitted { columns, index, data }
 * @param {Object} params single rule object
 * @returns {Object} the updated data
 */
function validChanges(data, params) {
    const varIndex = data.columns.indexOf(params.variable);
    if (varIndex === -1) {
        console.log(`${params.variable} not present in data set, skipping this QC step`);
        return data;
    }

    // Ensure we have a comment column
    const commentColName = `${params.variable}.data.change`;
    let commentColIndex = data.columns.indexOf(commentColName);
    if (commentColIndex === -1) {
        commentColIndex = data.columns.length;
        data.columns.push(commentColName);
        for (const row of data.data) {
            row.push(null);
        }
    }

    // set of values that trigger the change
    const triggerVals = getRangeValues(params.value_changed).map(x => {
        // ensure consistent type
        if (typeof x === 'number') return x;
        const maybeNum = Number(x);
        return Number.isNaN(maybeNum) ? x : maybeNum;
    });
    const newVal = Number(params.new_value);

    // update each row
    for (let i = 0; i < data.data.length; i++) {
        const currentVal = data.data[i][varIndex];
        if (triggerVals.includes(currentVal)) {
            data.data[i][varIndex] = newVal;
            if (!data.data[i][commentColIndex]) {
                data.data[i][commentColIndex] = params.comment;
            } else {
                data.data[i][commentColIndex] += ` | ${params.comment}`;
            }
        }
    }
    return data;
}


/**
 * `range.changes` applies a custom expression for each row, e.g. “1 < height & height < 2”.
 * We will attempt to evaluate the expression in JavaScript. This is only a partial approach:
 * you must supply a rule.value_changed that is valid JavaScript condition logic, referencing
 * the variable name exactly.
 *
 * Example:
 *   variable: "height"
 *   value_changed: "height >= 0 && height < 10"
 *   new_value: "999"
 *   comment: "height was changed..."
 *
 * @param {Object} data splitted
 * @param {Object} params single rule object
 * @returns {Object} updated data
 */
function rangeChanges(data, params) {
    const varIndex = data.columns.indexOf(params.variable);
    if (varIndex === -1) {
        console.log(`${params.variable} not present in data set, skipping this QC step`);
        return data;
    }

    const commentColName = `${params.variable}.data.change`;
    let commentColIndex = data.columns.indexOf(commentColName);
    if (commentColIndex === -1) {
        commentColIndex = data.columns.length;
        data.columns.push(commentColName);
        for (const row of data.data) {
            row.push(null);
        }
    }

    // Try to build a function from the expression. E.g. param.value_changed => "height < 2"
    // We'll interpret the variable name as "val" inside a function body: (val) => val < 2
    let conditionFn;
    try {
        conditionFn = new Function('val', `return (${params.value_changed});`);
    } catch (err) {
        console.warn(`Cannot parse range condition: ${params.value_changed}`);
        conditionFn = () => false;
    }
    const newVal = Number(params.new_value);

    for (let i = 0; i < data.data.length; i++) {
        const currentVal = data.data[i][varIndex];
        // Evaluate condition
        if (conditionFn(currentVal)) {
            data.data[i][varIndex] = newVal;
            if (!data.data[i][commentColIndex]) {
                data.data[i][commentColIndex] = params.comment;
            } else {
                data.data[i][commentColIndex] += ` | ${params.comment}`;
            }
        }
    }
    return data;
}


/**
 * Cross-range changes “cross_range1.changes”. We check both the main variable
 * and cross_variable_1 conditions. If they meet the rule, we change the main variable
 * to `new_value`.
 *
 * The R code does something like:
 *   if ( row[variable] meets condition ) AND ( row[cross_variable_1] meets cross_variable_1_value ),
 *   then set variable to new_value and comment with params.comment
 *
 * For `value_changed`, we either interpret it as a set (like validChanges) or an expression (like rangeChanges).
 * For `cross_variable_1_value`, similarly.  Because the original CSV might say e.g.
 *   cross_variable_1_value = "parous == 1"
 *   or "parous == 888"
 * This function tries to interpret them in a simple manner.
 */
function crossRange1Changes(data, params) {
    const varIndex = data.columns.indexOf(params.variable);
    const cross1Index = data.columns.indexOf(params.cross_variable_1);

    if (varIndex === -1 || cross1Index === -1) {
        console.log(
            `${params.variable} or ${params.cross_variable_1} not present in dataset, skipping.`
        );
        return data;
    }

    const commentColName = `${params.variable}.data.change`;
    let commentColIndex = data.columns.indexOf(commentColName);
    if (commentColIndex === -1) {
        commentColIndex = data.columns.length;
        data.columns.push(commentColName);
        for (const row of data.data) {
            row.push(null);
        }
    }

    // Build condition function for the main variable
    // e.g. "age %in% c(666,777)" => we might do something simpler:
    // if value_changed is parseable as a direct JS expression, we do new Function
    let mainFn;
    if (params.value_changed.includes('&&') || params.value_changed.includes('<') || params.value_changed.includes('>')) {
        try {
            mainFn = new Function('val', `return (${params.value_changed});`);
        } catch {
            mainFn = () => false;
        }
    } else {
        // treat as array membership
        const triggers = getRangeValues(params.value_changed).map(x => +x || x);
        mainFn = (v) => triggers.includes(v);
    }

    // Build condition function for cross_variable_1_value
    let crossFn;
    if (params.cross_variable_1_value.includes('&&') || params.cross_variable_1_value.includes('<') || params.cross_variable_1_value.includes('>') || params.cross_variable_1_value.includes('==')) {
        try {
            crossFn = new Function('val', `return (${params.cross_variable_1_value});`);
        } catch {
            crossFn = () => false;
        }
    } else {
        const triggers2 = getRangeValues(params.cross_variable_1_value).map(x => +x || x);
        crossFn = (v) => triggers2.includes(v);
    }

    const newVal = Number(params.new_value);

    for (let i = 0; i < data.data.length; i++) {
        const mainVal = data.data[i][varIndex];
        const crossVal = data.data[i][cross1Index];
        if (mainFn(mainVal) && crossFn(crossVal)) {
            data.data[i][varIndex] = newVal;
            if (!data.data[i][commentColIndex]) {
                data.data[i][commentColIndex] = params.comment;
            } else {
                data.data[i][commentColIndex] += ` | ${params.comment}`;
            }
        }
    }

    return data;
}


/**
 * Maps the “type” field of a correction rule (e.g. “valid.changes”, “range.changes”, etc.)
 * to the corresponding function that implements it.
 */
const correctionFnMap = {
    'valid.changes': validChanges,
    'range.changes': rangeChanges,
    'cross_range1.changes': crossRange1Changes
    // if you have more like cross_range2.changes, etc., add them here
};


/**
 * Internal utility used by changes_qc() to apply a single row from the rules
 * to the data, by dispatching to the correct function (validChanges, rangeChanges, etc.).
 */
function runQaTypeChanges(data, params) {
    const fn = correctionFnMap[params.type];
    if (!fn) {
        console.warn(`No correction function mapped for type: ${params.type}`);
        return data;
    }
    return fn(data, params);
}


/**
 * Main entry point for applying correction rules to your dataset.
 *
 * 1) Remove existing “.data.change” columns
 * 2) For each row in `rules`, call the appropriate “type” function
 * 3) Re-arrange comment columns next to their variable columns
 * 4) Remove any empty “.data.change” columns
 *
 * @param {Object[]} rules - array of rules; each item must have
 *        at least { variable, value_changed, type, new_value, comment } or similar
 * @param {Object} data - splitted orientation
 * @returns {Object} updated data
 */
function changesQc(rules, data) {
    // 1) remove existing .data.change columns
    const removeIndices = [];
    data.columns.forEach((colName, idx) => {
        if (colName.endsWith('.data.change')) {
            removeIndices.push(idx);
        }
    });
    // remove from right to left
    removeIndices.reverse().forEach(rmIdx => {
        data.columns.splice(rmIdx, 1);
        for (const row of data.data) {
            row.splice(rmIdx, 1);
        }
    });

    // 2) apply each rule in order
    let updatedData = data;
    for (const rule of rules) {
        updatedData = runQaTypeChanges(updatedData, rule);
    }

    // 3) arrange comment columns
    updatedData = arrangeChangeComments(updatedData);

    // 4) remove any empty .data.change columns
    //    i.e. columns that are entirely null or undefined
    const remove2 = [];
    updatedData.columns.forEach((col, cIndex) => {
        if (col.endsWith('.data.change')) {
            let allEmpty = true;
            for (const rowArr of updatedData.data) {
                if (rowArr[cIndex] !== null && rowArr[cIndex] !== undefined) {
                    allEmpty = false;
                    break;
                }
            }
            if (allEmpty) {
                remove2.push(cIndex);
            }
        }
    });
    remove2.reverse().forEach(idx => {
        updatedData.columns.splice(idx, 1);
        for (const row of updatedData.data) {
            row.splice(idx, 1);
        }
    });

    return updatedData;
}


export { validChanges, rangeChanges, crossRange1Changes, changesQc };