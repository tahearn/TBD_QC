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


export {getRangeValues};