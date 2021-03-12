if(document.getElementById("ga-js")){
  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', document.getElementById("ga-js").getAttribute("data-analytics-id")]);
  _gaq.push(['_trackPageview']);
  _gaq.push(['_gat._forceSSL']);
}

(function(){
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

	document.observe('dom:loaded', function(){
	  // scriptaculous can't animate elements that use a CSS class that sets display: none
	  // so remove that CSS class and set the style property
	  $$('.displayNone').each(function(element){
	    element.style.display = "none";
	    element.classList.remove("displayNone");
	  });
	
	  $$('.selectDatabaseReport').invoke("observe", "change", function(){
	  	window.location.href='?part=database&request=' + this.selectedIndex;
	  });
	
	  $$('.selectDeploymentPeriod').invoke("observe", "change", function(){
	  	document.deploymentPeriodForm.submit();
	  });

	  $$("[data-margin-left-px]").each(function(element){
	  	element.style.marginLeft=element.getAttribute("data-margin-left-px") + "px";
	  });

	  $$(".rumData td[data-width-percent]").each(function(element){
	  	element.style.width=element.getAttribute("data-width-percent") + "%";
	  });

	  if(document.customPeriodForm) {
		var test = document.createElement('input'); test.type = 'date';
		if(test.type === 'text') {
		  document.customPeriodForm.pattern.value = '';
		  document.getElementById('customPeriodPattern').style.display='inline';
		}
		function validateCustomPeriodForm() {
		   periodForm = document.customPeriodForm;
		   if (periodForm.startDate.value.length == 0) {
		      alert('Dates are mandatory');
		      periodForm.startDate.focus();
		      return false;
		   }
		   if (periodForm.endDate.value.length == 0) {
		      alert('Dates are mandatory');
		      periodForm.endDate.focus();
		      return false;
		   }
		   periodForm.period.value=periodForm.startDate.value + '|' + periodForm.endDate.value;
		   return true;
		}
		$$('form[name="customPeriodForm"]').invoke("observe", "submit", function(event){
			if(! validateCustomPeriodForm()){
			  event.preventDefault();
		    }
		});
	  }
	
	  $$('a.back').invoke("observe", "click", function(event){
	    history.back();
	  	event.preventDefault();
	  });
	  
	  $$('.copyHash').invoke("observe", "click", function(event){
	    document.getElementById('hash').select();
	    document.getElementById('hash').setSelectionRange(0, 99999); /*For mobile devices*/
	    document.execCommand('copy');
	  	event.preventDefault();
	  });

	  $$('.alertDialogAndRedirect').invoke("observe", "click", function(event){
	  	alert(this.getAttribute('data-alert'));
	  	event.preventDefault();
	  });

	  $$('.confirm').invoke("observe", "click", function(event){
	  	if(! confirm(this.getAttribute('data-confirm'))){
	  		event.preventDefault();
  		}
	  });

	  $$('.showHide').invoke("observe", "click", function(event){
	  	var id = this.getAttribute("data-show-hide-id");
	  	if(!id) id = this.id;
	  	showHide(id);
	  	event.preventDefault();
	  });

	  $$('.customPeriod').invoke("observe", "click", function(event){
		showHide('customPeriod');
		document.customPeriodForm.startDate.focus();
	  	event.preventDefault();
	  });

	  $$('.addApplication').invoke("observe", "click", function(event){
		showHide('addApplication');
		document.appForm.appName.focus();
	  	event.preventDefault();
	  });

	  $$('.addAggregation').invoke("observe", "click", function(event){
		showHide('addAggregation');
		document.aggregationForm.appName.focus();
	  	event.preventDefault();
	  });

	  $$('.deploymentPeriod').invoke("observe", "click", function(event){
		showHide('customPeriod');
		document.customPeriodForm.startDate.focus();
	  	event.preventDefault();
	  });

	  $$('#detailsGraphsA').invoke("observe", "click", function(event){
		loadImages('detailsGraphs');
	  	event.preventDefault();
	  });

	  $$('.menuBoxToggle').invoke("observe", "click", function(event){
		var el = document.getElementById("menuBox");
		if (el.getAttribute('class') == 'menuHide') {
		  el.setAttribute('class', 'menuShow');
		} else {
		  el.setAttribute('class', 'menuHide');
		}
	  	event.preventDefault();
	  });

	  $$('form').invoke("observe", "submit", function(event){
		$$(this).select("[required][data-required-message]").each(function(element){
		  if(element.value.length == 0){
		    alert(element.getAttribute("data-required"));
		    event.preventDefault();
		  }
		});
	  });

	  $$('tr.odd,tr.even').invoke("observe", "mouseover", function(){
	  	this.classList.add("highlight");
	  });

	  $$('tr.odd,tr.even').invoke("observe", "mouseout", function(){
	  	this.classList.remove("highlight");
	  });

	  $$('.alertAndRedirect').each(function(element){
	  	alert(element.getAttribute('data-alert'));
	  	window.location.href = element.getAttribute('data-href');
	  });

	  if(document.getElementById('handle')) {
	    Event.observe(window, 'load', function() {
		    var graphName = document.getElementById('img').getAttribute('data-graph-name');
			if (navigator.appName == 'Microsoft Internet Explorer') {
			  initialWidth = document.getElementById('img').width;
			  initialHeight = document.getElementById('img').height;
			} else {
			  initialWidth = Math.round(Element.getStyle('img','width').replace('px',''));
			  initialHeight = Math.round(Element.getStyle('img','height').replace('px',''));
			}
			function handleHideMaximumClick(checkbox) {
			    var img = document.getElementById('img');
			    if (checkbox.checked) {
			        img.src = img.src + '\u0026max=false\u0026r=' + Math.random();
			    } else {
			        img.src = img.src.replace('\u0026max=false','');
			    }
			}
			function scaleImage(v, min, max) {
			    var images = document.getElementsByClassName('synthèse');
			    w = (max - min) * v + min;
			    for (i = 0; i < images.length; i++) {
			        images[i].style.width = w + 'px';
			    }
			}
			
			// 'animate' our slider
			var slider = new Control.Slider('handle', 'track', {axis:'horizontal', alignX: 0, increment: 2});
			
			// resize the image as the slider moves. The image quality would deteriorate, but it
			// would not be final anyway. Once slider is released the image is re-requested from the server, where
			// it is rebuilt from vector format
			slider.options.onSlide = function(value) {
			  scaleImage(value, initialWidth, initialWidth / 2 * 3);
			}
			
			// this is where the slider is released and the image is reloaded
			// we use current style settings to work the required image dimensions
			slider.options.onChange = function() {
			  // chop off "px" and round up float values
			  width = Math.round(Element.getStyle('img','width').replace('px','')) - 80;
			  height = Math.round(width * initialHeight / initialWidth) - 48;
			  // reload the images
			  // rq : on utilise des caractères unicode pour éviter des warnings
			  document.getElementById('img').src = '?graph=' + encodeURIComponent(graphName) + '\u0026width=' + width + '\u0026height=' + height;
			  document.getElementById('img').style.width = '';
			}
			$$('#cb').invoke("observe", "click", function(){
			  handleHideMaximumClick(this);
			});
		});
	  }
	});
})();
