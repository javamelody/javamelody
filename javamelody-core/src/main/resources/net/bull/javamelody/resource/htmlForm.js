function validateElementNotEmpty(elementId, errorMsg) {
    const element = document.getElementById(elementId);
    if (element?.value.length === 0) {
        alert(errorMsg);
        element.focus();
        return false;
    }
    return true;
}
