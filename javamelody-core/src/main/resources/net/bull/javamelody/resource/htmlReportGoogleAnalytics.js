window.addEventListener("load", function (event) {
    const scripts = document.body.getElementsByTagName('script');
    const lastScript = scripts[scripts.length - 1];
    const analyticsId = lastScript.getAttribute('data-analytics-id');

    const gaJsHost = (('https:' === document.location.protocol) ? 'https://ssl.' : 'http://www.');
    document.write(unescape("<script src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'></script>"));

    try {
        const pageTracker = _gat._getTracker(analyticsId);
        pageTracker._trackPageview()
    } catch (err) {
    }
}, false);
