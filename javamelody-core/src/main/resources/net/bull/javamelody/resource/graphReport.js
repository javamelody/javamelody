function handleHideMaximumClick(checkbox) {
    const img = document.getElementById('img');
    if (checkbox.checked) {
        img.src = img.src + '&max=false&r=' + Math.random();
    } else {
        img.src = img.src.replace('&max=false', '');
    }
}

function scaleImage(v, min, max) {
    const images = document.getElementsByClassName('synthèse');
    const w = (max - min) * v + min;
    for (let i = 0; i < images.length; i++) {
        images[i].style.width = w + 'px';
    }
}

function getRequestParam(name) {
    const regRes = (new RegExp('[?&]' + encodeURIComponent(name) + '=([^&]*)')).exec(location.search)
    if (regRes) {
        return decodeURIComponent(regRes[1]);
    }
}

window.addEventListener("load", function () {

    let initialWidth;
    let initialHeight;

    if (navigator.appName === 'Microsoft Internet Explorer') {
        initialWidth = document.getElementById('img').width;
        initialHeight = document.getElementById('img').height;
    } else {
        initialWidth = Math.round(Number(Element.getStyle('img', 'width').replace('px', '')));
        initialHeight = Math.round(Number(Element.getStyle('img', 'height').replace('px', '')));
    }

    const slider = new Control.Slider('handle', 'track', {axis: 'horizontal', alignX: 0, increment: 2});

    slider.options.onSlide = function (value) {
        scaleImage(value, initialWidth, initialWidth / 2 * 3);
    }

    slider.options.onChange = function () {
        const width = Math.round(Number(Element.getStyle('img', 'width').replace('px', ''))) - 80;
        const height = Math.round(width * initialHeight / initialWidth) - 48;

        document.getElementById('img').src = '?graph='
            + getRequestParam('graph')
            + '&width=' + width + '&height=' + height;

        document.getElementById('img').style.width = '';
    }
},false);
