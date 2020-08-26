function copyHash() {
    document.getElementById('hash').select();
    document.getElementById('hash').setSelectionRange(0, 99999); /*For mobile devices*/
    document.execCommand('copy');
}
