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
    const containsAlphabeticCharacters = (str) => /[A-Za-z]/.test(String(str));

    if (typeof valuesVector === "number") {
        return [valuesVector];
    }

    if (typeof valuesVector === "string") {
        valuesVector = valuesVector.split(",").map((item) => item.trim());
    }

    if (!Array.isArray(valuesVector)) {
        throw new Error("Input must be a number, string, or array");
    }

    if (valuesVector.some(containsAlphabeticCharacters)) {
        return valuesVector.map(String);
    } else {
        return valuesVector.map((item) => {
            const num = parseFloat(item);
            return isNaN(num) ? item : num;
        });
    }
}


export {getRangeValues};