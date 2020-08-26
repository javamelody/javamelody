function showHide(id) {
    if (document.getElementById(id).style.display == 'none') {
        if (document.getElementById(id + 'Img') != null) {
            document.getElementById(id + 'Img').src = '?resource=bullets/minus.png';
        }
        try {
            Effect.SlideDown(id, {duration: 0.5});
        } catch (e) {
            document.getElementById(id).style.display = 'inline';
        }
    } else {
        if (document.getElementById(id + 'Img') != null) {
            document.getElementById(id + 'Img').src = '?resource=bullets/plus.png'
        }
        try {
            Effect.SlideUp(id, {duration: 0.5});
        } catch (e) {
            document.getElementById(id).style.display = 'none';
        }
    }
}

