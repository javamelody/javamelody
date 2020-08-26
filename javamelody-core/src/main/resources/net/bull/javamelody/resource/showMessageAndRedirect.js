window.addEventListener("load", function () {
    const scripts = document.body.getElementsByTagName('script');
    const lastScript = scripts[scripts.length - 1];

    const message = lastScript.getAttribute('data-message');
    const redirectTo = lastScript.getAttribute('data-redirect-to');

    alert(message);
    location.href = redirectTo;
}, false);
