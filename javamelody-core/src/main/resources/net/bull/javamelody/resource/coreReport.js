function toggle(id) {
    const el = document.getElementById(id);
    if (el.getAttribute('class') === 'menuHide') {
        el.setAttribute('class', 'menuShow');
    } else {
        el.setAttribute('class', 'menuHide');
    }
}

function loadImages(elementId) {
    const descendents = document.getElementById(elementId).getElementsByTagName('*');
    for (let i = 0; i < descendents.length; i++) {
        const element = descendents[i];
        if (element instanceof HTMLImageElement && element.src === '') {
            element.src = element.dataset.src;
        }
    }
}

window.addEventListener("load", function () {
    document.getElementById('detailsGraphsA').href = 'javascript:loadImages(\'detailsGraphs\');showHide(\'detailsGraphs\')';
}, false);
