/**
 * Summarize the `.data.change` columns in a split-orientation dataset.
 * We approximate the R code that uses tableby() by simply counting
 * how many times each distinct comment appears.  This function returns
 * an object describing the frequencies of each comment across
 * all `.data.change` columns.
 *
 * @param {Object} data - A dataset in split orientation
 * @returns {Object} summary structure (map of comment -> count)
 */
function changesSummary(data) {
    // Find columns that end with `.data.change` but exclude any that start with "Comments"
    const changeColumns = data.columns.filter(
        c => c.endsWith('.data.change') && !c.startsWith('Comments')
    );
    if (changeColumns.length === 0) {
        console.log('There were no updates made to rows!');
        return {Changes: 'There were no updates made to rows!'};
    }

    // Build up a frequency map
    const freqMap = new Map();
    for (const col of changeColumns) {
        const colIndex = data.columns.indexOf(col);
        if (colIndex < 0) continue; // should not happen
        for (const rowArr of data.data) {
            let commentVal = rowArr[colIndex];
            if (commentVal === null || commentVal === undefined) {
                continue;
            }
            // Possibly there are multiple pipe-separated comments
            const parts = String(commentVal).split(' | ');
            parts.forEach((p) => {
                const trimmed = p.trim();
                if (!freqMap.has(trimmed)) freqMap.set(trimmed, 0);
                freqMap.set(trimmed, freqMap.get(trimmed) + 1);
            });
        }
    }

    // We remove "No changes made" if that appears
    // or omit it from final. Then we return a plain object
    const summaryObj = {};
    for (const [comment, count] of freqMap.entries()) {
        if (comment === 'No changes made') continue;
        summaryObj[comment] = count;
    }

    if (Object.keys(summaryObj).length === 0) {
        return {Changes: 'No changes made'};
    }
    return summaryObj;
}


/**
 * Summarize the `.data.warning` columns in a split-orientation dataset.
 * Similar approach to changesSummary(), but for warnings.
 *
 * @param {Object} data - A dataset in split orientation
 * @returns {Object} summary structure (map of warning -> count)
 */
function warningsSummary(data) {
    const warningColumns = data.columns.filter(
        c => c.endsWith('.data.warning') && !c.startsWith('Comments')
    );
    if (warningColumns.length === 0) {
        return {Warnings: 'There were no warnings added!'};
    }

    const freqMap = new Map();
    for (const col of warningColumns) {
        const colIndex = data.columns.indexOf(col);
        if (colIndex < 0) continue;
        for (const rowArr of data.data) {
            let warnVal = rowArr[colIndex];
            if (warnVal === null || warnVal === undefined) {
                continue;
            }
            const parts = String(warnVal).split(' | ');
            parts.forEach((p) => {
                const trimmed = p.trim();
                if (!freqMap.has(trimmed)) freqMap.set(trimmed, 0);
                freqMap.set(trimmed, freqMap.get(trimmed) + 1);
            });
        }
    }

    // Filter out "No warnings detected"
    const summaryObj = {};
    for (const [comment, count] of freqMap.entries()) {
        if (comment === 'No warnings detected') continue;
        summaryObj[comment] = count;
    }

    if (Object.keys(summaryObj).length === 0) {
        return {Warnings: 'No warnings detected'};
    }
    return summaryObj;
}


/**
 * Summarize QC results for "Core" data. In the original R, this wrote an Excel
 * with multiple sheets. For now, we return an object containing:
 *  1) Missing Columns
 *  2) Changes Summary
 *  3) Warnings Summary
 *  4) (or any other categories you wish to parse)
 *
 * @param {Object[]} coreDict - array of dictionary row objects
 * @param {Object} qcData - dataset in split orientation
 * @param {string} studyName - name of the study
 * @returns {Object} an object summarizing the results
 */
function coreSummaryReport(coreDict, qcData, studyName) {
    const missingCols = listMissingColumns(coreDict, qcData);
    const changesSumm = changesSummary(qcData);
    const warningSumm = warningsSummary(qcData);

    return {
        studyName,
        missingColumns: missingCols,
        changes: changesSumm,
        warnings: warningSumm
        // You can add further breakdown by "Category" if needed
    };
}


/**
 * Summarize QC results for "Incident" data. Similar to coreSummaryReport.
 *
 * @param {Object[]} incidentDict - array of dictionary row objects
 * @param {Object} qcData - dataset in split orientation
 * @param {string} studyName
 * @returns {Object} an object summarizing the results
 */
function incidentSummaryReport(incidentDict, qcData, studyName) {
    const missingCols = listMissingColumns(incidentDict, qcData);
    const changesSumm = changesSummary(qcData);
    const warningSumm = warningsSummary(qcData);

    return {
        studyName,
        missingColumns: missingCols,
        changes: changesSumm,
        warnings: warningSumm
    };
}


export {changesSummary, warningsSummary, coreSummaryReport, incidentSummaryReport};