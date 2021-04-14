	function showHide(id){
	  if (document.getElementById(id).style.display=='none') {
	    if (document.getElementById(id + 'Img') != null) {
	      document.getElementById(id + 'Img').src='?resource=bullets/minus.png';
	    }
	    try {
	      document.getElementById(id).style.display=='inline';
	      Effect.SlideDown(id, { duration: 0.5 });
	    } catch (e) {
	      document.getElementById(id).style.display='inline';
	    }
	  } else {
	    if (document.getElementById(id + 'Img') != null) {
	      document.getElementById(id + 'Img').src='?resource=bullets/plus.png';
	    }
	    try {
	      Effect.SlideUp(id, { duration: 0.5 });
	    } catch (e) {
	      document.getElementById(id).style.display='none';
	    }
	  }
	}

	function loadImages(elementId) {
	  var descendents = document.getElementById(elementId).getElementsByTagName('*');
	  for (var i = 0; i < descendents.length; i++) {
	    var element = descendents[i];
	    if (element instanceof HTMLImageElement && element.src == '') {
	      element.src = element.dataset.src;
	    }
	  }
	}
	
	function toggleMenuBox() {
		var el = document.getElementById("menuBox");
		if (el.getAttribute('class') == 'menuHide') {
			el.setAttribute('class', 'menuShow');
		} else {
			el.setAttribute('class', 'menuHide');
		}
	}
