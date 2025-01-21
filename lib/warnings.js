/**
 * Range warnings. The R code used e.g. “range.warnings” to say:
 *   if rowValue < min or rowValue > max (excluding special codes like 777/888),
 *   then we add a comment in `.data.warning`.
 *
 * This is simplified. I assume `params['Valid Value Lower']` and
 * `params['Valid Value Higher']` are numeric. I skip rowValue if it’s in {666,777,888}.
 */
function rangeWarnings(data, params) {
    const varIndex = data.columns.indexOf(params.Variable);
    if (varIndex === -1) {
        console.log(`${params.Variable} not present in data set, skipping this QC step`);
        return data;
    }
    // Prepare the comment column
    const commentCol = `${params.Variable}.data.warning`;
    let commentIdx = data.columns.indexOf(commentCol);
    if (commentIdx === -1) {
        commentIdx = data.columns.length;
        data.columns.push(commentCol);
        for (const row of data.data) {
            row.push(null);
        }
    }

    const minVal = Number(params['Valid Value Lower']);
    const maxVal = Number(params['Valid Value Higher']);
    const skipVals = [666, 777, 888]; // frequently used special codes

    for (let i = 0; i < data.data.length; i++) {
        const val = data.data[i][varIndex];
        if (val === undefined || val === null) continue;
        if (skipVals.includes(val)) continue;

        if (val < minVal || val > maxVal) {
            if (!data.data[i][commentIdx]) {
                data.data[i][commentIdx] = params.Comments;
            } else {
                data.data[i][commentIdx] += ` | ${params.Comments}`;
            }
        }
    }

    return data;
}


/**
 * valid.warnings checks that rowValue is in the “Valid Values” set.
 * If not, it logs a comment in `.data.warning`.
 */
function validWarnings(data, params) {
    const varIndex = data.columns.indexOf(params.Variable);
    if (varIndex === -1) {
        console.log(`${params.Variable} not present - skipping this QC step`);
        return data;
    }
    const commentCol = `${params.Variable}.data.warning`;
    let commentIdx = data.columns.indexOf(commentCol);
    if (commentIdx === -1) {
        commentIdx = data.columns.length;
        data.columns.push(commentCol);
        for (const row of data.data) {
            row.push(null);
        }
    }

    const validVals = getRangeValues(params['Valid Values']).map(x => +x || x);
    for (let i = 0; i < data.data.length; i++) {
        const val = data.data[i][varIndex];
        if (!validVals.includes(val)) {
            if (!data.data[i][commentIdx]) {
                data.data[i][commentIdx] = params.Comments;
            } else {
                data.data[i][commentIdx] += ` | ${params.Comments}`;
            }
        }
    }

    return data;
}


/**
 * Cross-range warnings. The CSV might specify a formula check, e.g. check that
 * “age_preg2 > age_preg1” or that if “baseline=0 then qcycle >=1,” etc.
 * I do a partial approach:
 * - I interpret params.Formula_Condition as a JS expression with variable references
 * - I interpret Cross Variable # / # Value in a minimal approach
 */
function crossrangeWarnings(data, params) {
    // We check if main variable + cross variables exist
    if (!data.columns.includes(params.Variable)) {
        console.log(`Either ${params.Variable} not present, skipping.`);
        return data;
    }
    const mainVarIdx = data.columns.indexOf(params.Variable);

    // Build up the comment col
    const commentCol = `${params.Variable}.data.warning`;
    let commentIdx = data.columns.indexOf(commentCol);
    if (commentIdx === -1) {
        commentIdx = data.columns.length;
        data.columns.push(commentCol);
        for (const row of data.data) {
            row.push(null);
        }
    }

    // We'll parse the formula condition into a function that references columns
    // named in `params.Formula_Variable`.
    // But we do not implement 100% coverage for R syntax.
    let formulaFn = () => false;
    if (params.Formula_Condition) {
        // Example: "subject_id %in% is.Unique(data$subject_id)" is R code – we cannot parse that well.
        // We'll do a minimal approach and skip complicated expressions.
        // We'll just try a direct new Function, searching for variable references.
        try {
            // In the CSV, "Formula_Condition" might have "qcycle >=1 && qcycle !=888", etc.
            // We'll create function with signature (row, data, columns).
            // Then we replace variable references with row lookups. This is quite advanced to do robustly.
            // For simplicity, do a naive approach: no rewriting, but the user must pass valid JS.
            formulaFn = new Function('row', 'cols', `return (${params.Formula_Condition});`);
        } catch (e) {
            console.warn('Failed to parse formula condition:', params.Formula_Condition);
        }
    }

    // For cross var 1, we see if it is in data; we parse cross var 1 value.
    // Then if row cross var 1 = that value, we check formula
    if (params['Cross Variable 1'] && data.columns.includes(params['Cross Variable 1'])) {
        const crossIdx = data.columns.indexOf(params['Cross Variable 1']);

        // parse cross var 1 value
        let crossValFn = () => false;
        try {
            crossValFn = new Function('val', `return (${params['Cross Variable 1 Value']});`);
        } catch (err) {
            // fallback to membership approach
            const triggers = getRangeValues(params['Cross Variable 1 Value']).map(x => +x || x);
            crossValFn = (v) => triggers.includes(v);
        }

        for (let i = 0; i < data.data.length; i++) {
            const crossVal = data.data[i][crossIdx];
            const formulaPass = formulaFn(data.data[i], data, data.columns);
            if (crossValFn(crossVal) && formulaPass) {
                if (!data.data[i][commentIdx]) {
                    data.data[i][commentIdx] = params.Comments;
                } else {
                    data.data[i][commentIdx] += ` | ${params.Comments}`;
                }
            }
        }
    }
    return data;
}


/** Map “QC Type” => which function to call. */
const warningFnMap = {
    'range.warnings': rangeWarnings,
    'valid.warnings': validWarnings,
    'crossrange.warnings': crossrangeWarnings
    // also "crossvalid1.warnings", "crossvalid2.warnings", etc.
    // in actual code, you would add them similarly
};


/**
 * Handler for applying a single warning rule to the dataset.
 */
function runQaTypeWarnings(data, params) {
    const fn = warningFnMap[params['QC Type']];
    if (!fn) {
        console.warn(`No warning function mapped for type: ${params['QC Type']}`);
        return data;
    }
    return fn(data, params);
}


/**
 * The main function for applying warning rules.
 *  1) Clear existing `.data.warning` columns
 *  2) For each rule, run the assigned function
 *  3) arrange the warning comment columns
 *  4) remove empty `.data.warning` columns
 *  5) Then filter the returned data so that only rows that have at least one
 *     comment (either data.change or data.warning) remain (the R code does
 *     something like that).
 *
 * In the original R code, the final data returned by warnings_qc() was
 * only those rows that had flagged warnings or changes. If you want to keep
 * the entire dataset, remove that final filter step.
 */
function warningsQc(rules, data) {
    // 1) remove existing .data.warning columns
    const removeIndices = [];
    data.columns.forEach((colName, idx) => {
        if (colName.endsWith('.data.warning')) {
            removeIndices.push(idx);
        }
    });
    removeIndices.reverse().forEach(rmIdx => {
        data.columns.splice(rmIdx, 1);
        for (const row of data.data) {
            row.splice(rmIdx, 1);
        }
    });

    // 2) apply each rule in order
    let updatedData = data;
    for (const rule of rules) {
        updatedData = runQaTypeWarnings(updatedData, rule);
    }

    // 3) arrange warning comment columns
    updatedData = arrangeWarningComments(updatedData);
    // 4) also re-arrange .data.change columns if needed
    updatedData = arrangeAllComments(updatedData);

    // remove empty .data.warning columns
    const remove2 = [];
    updatedData.columns.forEach((col, cIndex) => {
        if (col.endsWith('.data.warning')) {
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

    // 5) filter rows that have no changes or warnings, if you want the final subset
    //    This is the logic from R: "filter(!if_all(ends_with('data.change'|'data.warning'), is.na))"
    //    i.e. keep only rows that have at least one change or warning
    const changeWarningCols = updatedData.columns.filter(
        c => c.endsWith('.data.change') || c.endsWith('.data.warning')
    );
    const keepRows = [];
    for (let i = 0; i < updatedData.data.length; i++) {
        const rowArr = updatedData.data[i];
        // check if there's at least one non-null in the relevant columns
        let hasFlag = false;
        for (const colName of changeWarningCols) {
            const colIndex = updatedData.columns.indexOf(colName);
            if (rowArr[colIndex] !== null && rowArr[colIndex] !== undefined) {
                hasFlag = true;
                break;
            }
        }
        if (hasFlag) {
            keepRows.push(rowArr);
        }
    }
    updatedData.data = keepRows;

    return updatedData;
}


export {rangeWarnings, validWarnings, crossrangeWarnings, warningsQc};