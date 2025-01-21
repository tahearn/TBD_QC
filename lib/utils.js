/**
 * Processes an array of valid values for a variable— which may be represented either as an array, a string,
 * or a number— and returns it as an array of values. For example, if a variable has 0 and 1 as its valid values,
 * this may be written in the rules Excel file as "0,1". This function (`getRangeValues("0,1")`) will return this
 * string as an array `[0,1]`. This function helps in preliminary QC checks, where more than one value may be valid
 * for a variable.
 * @param {Array|string|number} valuesVector - A list of valid values for the variable, represented either as a
 *  number, a string, or an array.
 * @returns {Array} An array of all the separated valid values.
 */
function getRangeValues(valuesVector) {
    // Check if a string contains alphabetic characters
    const containsAlphabeticCharacters = (str) => /[A-Za-z]/.test(String(str));

    // If input is a single number, wrap it in an array, and return it
    if (typeof valuesVector === "number") {
        return [valuesVector];
    }

    // Convert string to array, splitting by comma if necessary
    if (typeof valuesVector === "string") {
        valuesVector = valuesVector.split(",").map((item) => item.trim());
    }

    // Ensure valuesVector is an array at this stage
    if (!Array.isArray(valuesVector)) {
        throw new Error("Input must be a number, string, or array");
    }

    // If any element in the array contains alphabetic characters
    if (valuesVector.some(containsAlphabeticCharacters)) {
        // Return all elements as strings
        return valuesVector.map(String);
    } else {
        // All elements are assumed to be either numeric or numeric strings. Convert them to numbers.
        return valuesVector.map((item) => {
            const num = parseFloat(item);
            return isNaN(num) ? item : num;
        });
    }
}


/**
 * Function to arrange the comment columns post QC filtering. This function rearranges a dataset containing
 * change comments and data columns, such that each change comment is paired with its corresponding data column.
 * @param data - The data, represented as a JSON in split orientation, where comment columns are to be arranged.
 * @returns {{data: *, columns: any[], index: *}} - data with the comment columns re-arranged after its
 *  corresponding variable.
 */
function arrangeChangeComments(data) {
    // Extract column names
    const allColumns = data.columns;

    // Separate comment columns and data columns
    const commentColumns = allColumns.filter((col) =>
        col.endsWith(".data.change")
    );
    const dataColumns = allColumns.filter((col) => !col.endsWith(".data.change"));

    // Create column pairs
    const columnPairs = dataColumns.map((col, index) => [
        commentColumns[index],
        col
    ]);

    // Create a new column order
    const newColumnOrder = columnPairs.flatMap((pair) => pair.filter(Boolean));

    // Rearrange columns in the data
    return {
        columns: newColumnOrder,
        index: data.index,
        data: data.data.map((row) => {
            const newRow = [];
            newColumnOrder.forEach((col) => {
                const colIndex = allColumns.indexOf(col);
                newRow.push(row[colIndex]);
            });
            return newRow;
        })
    };
}


/**
 * Function to arrange warning comment columns. This function rearranges a dataset containing warning comments
 * and data columns, such that each warning comment is paired with its corresponding data column.
 * @param data - The data, represented as a JSON in split orientation, where warning comment columns are to be
 *  arranged.
 * @returns {{data: *, columns: any[], index: *}|*} - data with the warning comment columns re-arranged after its
 *  corresponding variable.
 */
function arrangeWarningComments(data) {
    // Check if any column name contains 'warning'
    if (!data.columns.some(col => col.includes('warning'))) {
        return data;
    }

    // Extract column names
    const allColumns = data.columns;

    // Separate warning columns and data columns
    const warningColumns = allColumns.filter(col => col.endsWith('.data.warning'));
    const dataColumns = allColumns.filter(col => !col.endsWith('.data.change') && !col.endsWith('.data.warning'));

    // Create column pairs
    const columnPairs = dataColumns.map((col, index) => [warningColumns[index], col]);

    // Create a new column order
    const newColumnOrder = columnPairs.flatMap(pair => pair.filter(Boolean));

    // Add any remaining columns that weren't paired
    allColumns.forEach(col => {
        if (!newColumnOrder.includes(col)) {
            newColumnOrder.push(col);
        }
    });

    // Rearrange columns in the data
    return {
        columns: newColumnOrder,
        index: data.index,
        data: data.data.map(row => {
            const newRow = [];
            newColumnOrder.forEach(col => {
                const colIndex = allColumns.indexOf(col);
                newRow.push(row[colIndex]);
            });
            return newRow;
        })
    };
}


/**
 * Function to arrange all comment columns. This function rearranges a dataset containing change comments, warning
 * comments, and data columns, such that each comment is paired with its corresponding data column.
 * @param data - The data, represented as a JSON in split orientation, where all comment columns are to be arranged.
 * @returns {{data: *, columns: *[], index: *}|*}
 */
function arrangeAllComments(data) {
    // Check if any column name contains 'data.change'
    if (!data.columns.some(col => col.includes('data.change'))) {
        return data;
    }

    // Extract column names
    const allColumns = data.columns;

    // Separate comment columns and data columns
    const commentColumns = allColumns.filter(col => col.endsWith('.data.change'));
    const dataColumns = commentColumns.map(col => col.replace('.data.change', ''));

    // Create column pairs
    const columnPairs = dataColumns.map((col, index) => [commentColumns[index], col]);

    // Create a new column order
    let newColumnOrder = [];
    allColumns.forEach(col => {
        if (dataColumns.includes(col)) {
            const pair = columnPairs.find(pair => pair[1] === col);
            newColumnOrder.push(col);
            newColumnOrder.push(pair[0]);
        } else if (!commentColumns.includes(col)) {
            newColumnOrder.push(col);
        }
    });

    // Rearrange columns in the data
    return {
        columns: newColumnOrder,
        index: data.index,
        data: data.data.map(row => {
            return newColumnOrder.map(col => row[allColumns.indexOf(col)]);
        })
    };
}


/**
 * Simple function that returns all array elements that appear exactly once.
 * Equivalent to the R code is.Unique(). The result is the subset of `vector`
 * that is not duplicated.
 * @param {Array} vector
 * @returns {Array} all elements that appear exactly once in `vector`
 */
function isUnique(vector) {
    const counts = new Map();
    for (const v of vector) {
        const key = v !== null && v !== undefined ? String(v) : '__NUL__';
        counts.set(key, (counts.get(key) || 0) + 1);
    }
    const result = [];
    for (const v of vector) {
        const key = v !== null && v !== undefined ? String(v) : '__NUL__';
        if (counts.get(key) === 1) {
            result.push(v);
        }
    }
    return result;
}


/**
 * Ensure that any columns in `data.columns` whose lower-cased name matches the
 * “Variable Name” from dataDict are corrected to the exact case that appears in
 * dataDict. This is a rough approximation of R’s change_case_match() function.
 *
 * @param {Object} dataDict - Some data dictionary representation
 *   We expect an Array of objects: e.g. [ { 'Variable Name': 'qcycle', ... }, ... ]
 * @param {Object} data - A dataset in split orientation {columns, index, data}
 * @returns {Object} The same `data` object, possibly with renamed columns
 */
function changeCaseMatch(dataDict, data) {
    // Extract all variable names from dataDict
    // (We assume dataDict is an array of objects with a key "Variable Name".)
    const dictVarNames = dataDict
        .filter(row => row['Variable Name'] !== undefined && row['Variable Name'] !== null)
        .map(row => row['Variable Name']);

    // For each variable in dictVarNames, check if there's a case-insensitive match in data.columns
    const dataColumnsLower = data.columns.map(c => c.toLowerCase());
    const changesNeeded = [];
    for (const dictVarName of dictVarNames) {
        const lowerVar = dictVarName.toLowerCase();
        const colIndex = dataColumnsLower.indexOf(lowerVar);
        if (colIndex !== -1 && data.columns[colIndex] !== dictVarName) {
            // We want to rename data.columns[colIndex] to exactly dictVarName
            changesNeeded.push({from: data.columns[colIndex], to: dictVarName});
            data.columns[colIndex] = dictVarName;
        }
    }

    if (changesNeeded.length === 0) {
        console.log('No changes needed, all variable cases match!');
    } else {
        console.log('Cases for some variables in data will be changed:');
        console.log(changesNeeded);
    }

    return data;
}


/**
 * Lists the missing columns in `data` that appear in `dataDict` but not found in `data.columns`.
 *
 * @param {Object[]} dataDict - an array of objects, each having `'Variable Name'`
 * @param {Object} data - split orientation object
 * @returns {string | string[]} a message "No variables missing!" or an array of missing variable names
 */
function listMissingColumns(dataDict, data) {
    const dictVarNames = dataDict
        .filter(row => row['Variable Name'] !== undefined && row['Variable Name'] !== null)
        .map(row => row['Variable Name'].toLowerCase());

    const dataColsLower = data.columns.map(c => c.toLowerCase());

    const missingVars = dictVarNames.filter(v => !dataColsLower.includes(v));

    if (missingVars.length === 0) {
        return 'No variables missing!';
    } else {
        console.log('These variables are not present in the dataset:', missingVars);
        return missingVars;
    }
}


export {getRangeValues, arrangeChangeComments, arrangeWarningComments, arrangeAllComments, isUnique, changeCaseMatch, listMissingColumns};