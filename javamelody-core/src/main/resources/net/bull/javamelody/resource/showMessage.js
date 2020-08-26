window.addEventListener("load", function () {
    const scripts = document.body.getElementsByTagName('script');
    const lastScript = scripts[scripts.length - 1];
    
    const message = lastScript.getAttribute('data-message');
    const partToRedirectTo = lastScript.getAttribute('data-part-to-redirect-to');
    const anchorNameForRedirect = lastScript.getAttribute('data-anchor-name-for-redirect');

    alert(message);

    if (partToRedirectTo == null) {
        if (anchorNameForRedirect == null) {
            location.href = '?';
        } else {
            if (location.href.indexOf('?') !== -1) {
                location.href = location.href.substring(0, location.href.indexOf('?')) + '#' + anchorNameForRedirect;
            } else {
                location.href = '#' + anchorNameForRedirect;
            }
        }
    } else {
        location.href = '?part=' + partToRedirectTo;
    }

}, false);
