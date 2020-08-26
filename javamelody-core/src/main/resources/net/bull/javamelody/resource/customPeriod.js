function validateCustomPeriodForm(ids, errorMsg) {
    periodForm = document.customPeriodForm;
    for (const id of ids) {
        if (!validateElementNotEmpty(id, errorMsg)) {
            return false;
        }
    }

    periodForm.period.value = periodForm.startDate.value + '|' + periodForm.endDate.value;
    return true;
}

function validateElementNotEmpty(elementId, errorMsg) {
    const element = document.getElementById(elementId);
    if (element?.value.length === 0) {
        alert(errorMsg);
        element.focus();
        return false;
    }
    return true;
}

window.addEventListener("load", function () {
    // On teste si l'élément <input type='date'> se transforme en <input type='text'
    const test = document.createElement('input');
    test.type = 'date';

    const scripts = document.body.getElementsByTagName('script');
    const lastScript = scripts[scripts.length - 1];
    const startDate = lastScript.getAttribute('data-start-date');
    const endDate = lastScript.getAttribute('data-end-date');

    // Si c'est le cas, cela signifie que l'élément (html5) n'est pas pris en charge
    if (test.type === 'text') {
        // si pas html5, on vide le champ pattern car il n'est pas au bon format
        // et on affiche le format en langue du navigateur
        document.customPeriodForm.pattern.value = '';
        document.getElementById('customPeriodPattern').style.display = 'inline';

        if (startDate) {
            document.customPeriodForm.startDate.value = startDate;
        }
        if (endDate) {
            document.customPeriodForm.endDate.value = endDate;
        }
    }
}, false);
