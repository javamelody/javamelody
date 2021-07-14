// lightwindow.js v2.0
//
// Copyright (c) 2007 stickmanlabs
// Author: Kevin P Miller | http://www.stickmanlabs.com
// 
// LightWindow is freely distributable under the terms of an MIT-style license.
//
// I don't care what you think about the file size...
//   Be a pro: 
//	    http://www.thinkvitamin.com/features/webapps/serving-javascript-fast
//      http://rakaz.nl/item/make_your_pages_load_faster_by_combining_and_compressing_javascript_and_css_files
//

/*-----------------------------------------------------------------------------------------------*/

if(typeof Effect == 'undefined')
  throw("lightwindow.js requires including script.aculo.us' effects.js library!");

// This will stop image flickering in IE6 when elements with images are moved
try {
	document.execCommand("BackgroundImageCache", false, true);
} catch(e) {}

var lightwindow = Class.create();	
lightwindow.prototype = {
	//
	//	Setup Variables
	//
	element : null,
	contentToFetch : null,
	windowActive : false,
	dataEffects : [],
	dimensions : {
		cruft : null,
		container : null,
		viewport : {
			height : null,
			width : null,
			offsetTop : null,
			offsetLeft : null
		}
	},
	pagePosition : {
		x : 0,
		y : 0
	},
	pageDimensions : {
		width : null,
		height : null
	},
	preloadImage : [],
	preloadedImage : [],
	galleries : [],
	resizeTo : {
		height : null,
		heightPercent : null,
		width : null,
		widthPercent : null,
		fixedTop : null,
		fixedLeft : null
	},
	scrollbarOffset : 18,
	navigationObservers : {
		previous : null,
		next : null
	},
	containerChange : {
		height : 0,
		width : 0
	},
	activeGallery : false,
	galleryLocation : {
		current : 0,
		total : 0
	},
	//
	//	Initialize the lightwindow.
	//
	initialize : function(options) {
		this.options = Object.extend({
			resizeSpeed : 8,
			contentOffset : {
				height : 20,
				width : 20
			},
			dimensions : {
				image : {height : 250, width : 250},
				page : {height : 250, width : 250},
				inline : {height : 250, width : 250},
				media : {height : 250, width : 250},
				external : {height : 250, width : 250},
				titleHeight : 25
			},
			classNames : {	
				standard : 'lightwindow',
				action : 'lightwindow_action'
			},
			fileTypes : {
				page : ['asp', 'aspx', 'cgi', 'cfm', 'htm', 'html', 'pl', 'php4', 'php3', 'php', 'php5', 'phtml', 'rhtml', 'shtml', 'txt', 'vbs', 'rb'],
				media : ['aif', 'aiff', 'asf', 'avi', 'divx', 'm1v', 'm2a', 'm2v', 'm3u', 'mid', 'midi', 'mov', 'moov', 'movie', 'mp2', 'mp3', 'mpa', 'mpa', 'mpe', 'mpeg', 'mpg', 'mpg', 'mpga', 'pps', 'qt', 'rm', 'ram', 'swf', 'viv', 'vivo', 'wav'],
				image : ['bmp', 'gif', 'jpg', 'png', 'tiff']
			},
			mimeTypes : {
				avi : 'video/avi',
				aif : 'audio/aiff',
				aiff : 'audio/aiff',
				gif : 'image/gif',
				bmp : 'image/bmp',
				jpeg : 'image/jpeg',
				m1v : 'video/mpeg',
				m2a : 'audio/mpeg',
				m2v : 'video/mpeg',
				m3u : 'audio/x-mpequrl',
				mid : 'audio/x-midi',
				midi : 'audio/x-midi',
				mjpg : 'video/x-motion-jpeg',
				moov : 'video/quicktime',
				mov : 'video/quicktime',
				movie : 'video/x-sgi-movie',
				mp2 : 'audio/mpeg',
				mp3 : 'audio/mpeg3',
				mpa : 'audio/mpeg',
				mpa : 'video/mpeg',
				mpe : 'video/mpeg',
				mpeg : 'video/mpeg',
				mpg : 'audio/mpeg',
				mpg : 'video/mpeg',
				mpga : 'audio/mpeg',
				pdf : 'application/pdf',
				png : 'image/png',
				pps : 'application/mspowerpoint',
				qt : 'video/quicktime',
				ram : 'audio/x-pn-realaudio-plugin',
				rm : 'application/vnd.rn-realmedia',
				swf	: 'application/x-shockwave-flash',
				tiff : 'image/tiff',
				viv : 'video/vivo',
				vivo : 'video/vivo',
				wav : 'audio/wav',
				wmv : 'application/x-mplayer2'			
			},	
			classids : {
				mov : 'clsid:02BF25D5-8C17-4B23-BC80-D3488ABDDC6B',
				swf : 'clsid:D27CDB6E-AE6D-11cf-96B8-444553540000',
				wmv : 'clsid:6BF52A52-394A-11d3-B153-00C04F79FAA6'
			},
			codebases : {
				mov : 'http://www.apple.com/qtactivex/qtplugin.cab#version=6,0,2,0',
				swf : 'http://fpdownload.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=7,0,0,0',
				wmv : 'http://activex.microsoft.com/activex/controls/mplayer/en/nsmp2inf.cab#Version=6,4,5,715'
			},	
			viewportPadding : 10,
			EOLASFix : 'swf,wmv,fla,flv',
			overlay : {
				opacity : 0.7,
				image : '?resource=black.png',
				presetImage : '?resource=black-70.png'
			},
			skin : 	{
				main : 	'<div id="lightwindow_container" >'+
							'<div id="lightwindow_title_bar" >'+
								'<div id="lightwindow_title_bar_inner" >'+
									'<span id="lightwindow_title_bar_title"></span>'+
									'<a id="lightwindow_title_bar_close_link" >&nbsp;&nbsp;&nbsp;<img src="?resource=action_close.png" alt="close"/>&nbsp;&nbsp;&nbsp;</a>'+
								'</div>'+
							'</div>'+
							'<div id="lightwindow_stage" >'+
								'<div id="lightwindow_contents" >'+
								'</div>'+
								'<div id="lightwindow_navigation" >'+
									'<a href="#" id="lightwindow_previous" >'+
										'<span id="lightwindow_previous_title"></span>'+
									'</a>'+
									'<a href="#" id="lightwindow_next" >'+
										'<span id="lightwindow_next_title"></span>'+
									'</a>'+
									'<iframe name="lightwindow_navigation_shim" id="lightwindow_navigation_shim" src="about:blank" frameBorder="0" scrolling="no"></iframe>'+
								'</div>'+								
								'<div id="lightwindow_galleries">'+
									'<div id="lightwindow_galleries_tab_container" >'+
										'<a href="#" id="lightwindow_galleries_tab" >'+
											'<span id="lightwindow_galleries_tab_span" class="up" >Galleries</span>'+
										'</a>'+
									'</div>'+
									'<div id="lightwindow_galleries_list" >'+
									'</div>'+
								'</div>'+
							'</div>'+
							'<div id="lightwindow_data_slide" >'+
								'<div id="lightwindow_data_slide_inner" >'+
									'<div id="lightwindow_data_details" >'+
										'<div id="lightwindow_data_gallery_container" >'+
											'<span id="lightwindow_data_gallery_current"></span>'+
											' of '+
											'<span id="lightwindow_data_gallery_total"></span>'+
										'</div>'+
										'<div id="lightwindow_data_author_container" >'+
											'by <span id="lightwindow_data_author"></span>'+
										'</div>'+
									'</div>'+
									'<div id="lightwindow_data_caption" >'+
									'</div>'+
								'</div>'+
							'</div>'+
						'</div>',	
				loading : 	'<div id="lightwindow_loading" >'+
								'<img src="?resource=ajax-loading.gif" alt="loading" />'+
								//'<span>Loading or <a href="javascript: myLightWindow.deactivate();">Cancel</a></span>'+
								'<iframe name="lightwindow_loading_shim" id="lightwindow_loading_shim" src="about:blank" frameBorder="0" scrolling="no"></iframe>'+
							'</div>',
				iframe : 	'<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">'+
							'<html xmlns="http://www.w3.org/1999/xhtml">'+
								'<body>'+
									'{body_replace}'+
								'</body>'+
							'</html>',
				gallery : {
					top :		'<div class="lightwindow_galleries_list">'+
									'<h1>{gallery_title_replace}</h1>'+
									'<ul>',
					middle : 			'<li>'+
											'{gallery_link_replace}'+
										'</li>',
					bottom : 		'</ul>'+
								'</div>'
				}
			},
			formMethod : 'get',
			hideFlash : false,
			hideGalleryTab : false,
			showTitleBar : true,
			animationHandler : false,
			navigationHandler : false,
			transitionHandler : false,
			finalAnimationHandler : false,
			formHandler : false,
			galleryAnimationHandler : false,
			showGalleryCount : true
		}, options || {});
		this.duration = ((11-this.options.resizeSpeed)*0.15);
		this._setupLinks();
		this._getScroll();
		this._getPageDimensions();
		this._browserDimensions();
		this._addLightWindowMarkup(false);
		this._setupDimensions(); 
		this.buildGalleryList();
	},
	//
	//	Activate the lightwindow.
	//
	activate : function(e, link){		
		// Clear out the window Contents
		this._clearWindowContents(true);
			
		// Add back in out loading panel
		this._addLoadingWindowMarkup();

		// Setup the element properties
		this._setupWindowElements(link);
		
		// Setup everything
		this._getScroll();
		this._browserDimensions();
		this._setupDimensions();
		this._toggleTroubleElements('hidden', false);
		this._displayLightWindow('block', 'hidden');
		this._setStatus(true);
		this._monitorKeyboard(true);
		this._prepareIE(true);
		this._loadWindow();
	},
	//
	//	Turn off the window
	//
	deactivate : function(){
		// The window is not active
		this.windowActive = false;
		
		// There is no longer a gallery active
		this.activeGallery = false;
		if (!this.options.hideGalleryTab) {
			this._handleGalleryAnimation(false);
		}
		
		// Kill the animation
		this.animating = false;
		
		// Clear our element
		this.element = null;
		
		// hide the window.
		this._displayLightWindow('none', 'visible');
		
		// Clear out the window Contents
		this._clearWindowContents(false);
		
		// Stop all animation
		var queue = Effect.Queues.get('lightwindowAnimation').each(function(e){e.cancel();});
		
		// Undo the setup
		this._prepareIE(false);
		this._setupDimensions();
		this._toggleTroubleElements('visible', false);	
		this._monitorKeyboard(false);	
	},
	//
	//  Initialize specific window
	//
	createWindow : function(element, attributes) {
		this._processLink($(element));
	},
	//
	//  Open a Window from a hash of attributes
	//
	activateWindow : function(options) {
		this.element = Object.extend({
			href : null,
			title : null,
			author : null,
			caption : null,
			rel : null,
			top : null,
			left : null,
			type : null,
			showImages : null,
			height : null,
			width : null,
			loadingAnimation : null,
			iframeEmbed : null,
			form : null
		}, options || {});
		
		// Set the window type
		this.contentToFetch = this.element.href;
		this.windowType = this.element.type ? this.element.type : this._fileType(this.element.href);	
		
		// Clear out the window Contents
		this._clearWindowContents(true);
			
		// Add back in out loading panel
		this._addLoadingWindowMarkup();
		
		// Setup everything
		this._getScroll();
		this._browserDimensions();
		this._setupDimensions();
		this._toggleTroubleElements('hidden', false);
		this._displayLightWindow('block', 'hidden');
		this._setStatus(true);
		this._monitorKeyboard(true);
		this._prepareIE(true);
		this._loadWindow();
	},
	//
	//  Fire off our Form handler
	//
	submitForm : function(e) {
		if (this.options.formHandler) {
			this.options.formHandler(e);
		} else {
			this._defaultFormHandler(e);
		}
	},
	//
	//	Reload the window with another location
	//
	openWindow : function(element) {
		var element = $(element);

		// The window is active
		this.windowActive = true;
		
		// Clear out the window Contents
		this._clearWindowContents(true);
		
		// Add back in out loading panel
		this._addLoadingWindowMarkup();
		
		// Setup the element properties
		this._setupWindowElements(element);

		this._setStatus(true);
		this._handleTransition();
	},
	//
	//  Navigate the window
	//
	navigateWindow : function(direction) {
		this._handleNavigation(false);
		if (direction == 'previous') {
			this.openWindow(this.navigationObservers.previous);
		} else if (direction == 'next'){ 
			this.openWindow(this.navigationObservers.next);
		}
	},
	//
	//  Build the Gallery List and Load it
	//
	buildGalleryList : function() {
		var output = '';
		var galleryLink;
		for (i in this.galleries) {
			if (typeof this.galleries[i] == 'object') {
				output += (this.options.skin.gallery.top).replace('{gallery_title_replace}', unescape(i));
				for (j in this.galleries[i]) {
					if (typeof this.galleries[i][j] == 'object') {						
						galleryLink = '<a href="#" id="lightwindow_gallery_'+i+'_'+j+'" >'+unescape(j)+'</a>';
						output += (this.options.skin.gallery.middle).replace('{gallery_link_replace}', galleryLink);
					}
				}
				output += this.options.skin.gallery.bottom;
			}
		}
		new Insertion.Top('lightwindow_galleries_list', output);
		
		// Attach Events
		for (i in this.galleries) {
			if (typeof this.galleries[i] == 'object') {
				for (j in this.galleries[i]) {
					if (typeof this.galleries[i][j] == 'object') {
						Event.observe($('lightwindow_gallery_'+i+'_'+j), 'click', this.openWindow.bind(this, this.galleries[i][j][0]), false);
						$('lightwindow_gallery_'+i+'_'+j).onclick = function() {return false;};	
					}
				}
			}
		}
	},
	// 
	//  Set Links Up
	//
	_setupLinks : function() {
		var links = $$('.'+this.options.classNames.standard);
		links.each(function(link) {
			this._processLink(link);
		}.bind(this));	
	},
	//
	//  Process a Link
	//
	_processLink : function(link) {
		if ((this._fileType(link.getAttribute('href')) == 'image' || this._fileType(link.getAttribute('href')) == 'media')) {
			if (gallery = this._getGalleryInfo(link.rel)) {
				if (!this.galleries[gallery[0]]) {
					this.galleries[gallery[0]] = new Array();
				}
				if (!this.galleries[gallery[0]][gallery[1]]) {
					this.galleries[gallery[0]][gallery[1]] = new Array();
				}
				this.galleries[gallery[0]][gallery[1]].push(link);
			}
		}
		
		// Take care of our inline content
		var url = link.getAttribute('href');
		if (url.indexOf('?') > -1) {
			url = url.substring(0, url.indexOf('?'));
		}
		
		var container = url.substring(url.indexOf('#')+1);
		if(container != '' && $(container)) {
			$(container).setStyle({
				display : 'none'
			});
		}
		
		Event.observe(link, 'click', this.activate.bindAsEventListener(this, link), false);
		link.onclick = function() {return false;};		
	},
	//
	//	Setup our actions
	//
	_setupActions : function() {
		var links = $$('#lightwindow_container .'+this.options.classNames.action);
		links.each(function(link) {
			Event.observe(link, 'click', this[link.getAttribute('rel')].bindAsEventListener(this, link), false);
			link.onclick = function() {return false;};
		}.bind(this));
	},
	//
	//	Add the markup to the page.
	//
	_addLightWindowMarkup : function(rebuild) {
		var overlay = Element.extend(document.createElement('div'));
		overlay.setAttribute('id', 'lightwindow_overlay');		
		// FF Mac has a problem with putting Flash above a layer without a 100% opacity background, so we need to use a pre-made
		if (Prototype.Browser.Gecko) {
			overlay.setStyle({
				backgroundImage: 'url('+this.options.overlay.presetImage+')',
				backgroundRepeat: 'repeat',
				height: this.pageDimensions.height+'px'
			});			
		} else {
			overlay.setStyle({
				opacity: this.options.overlay.opacity,
				backgroundImage: 'url('+this.options.overlay.image+')',
				backgroundRepeat: 'repeat',
				height: this.pageDimensions.height+'px'
			});
		}
		
		var lw = document.createElement('div');
		lw.setAttribute('id', 'lightwindow');
		lw.innerHTML = this.options.skin.main;
		
		var body = document.getElementsByTagName('body')[0];
		body.appendChild(overlay);
		body.appendChild(lw);	
				
		if ($('lightwindow_title_bar_close_link')) {
			Event.observe('lightwindow_title_bar_close_link', 'click', this.deactivate.bindAsEventListener(this));
			$('lightwindow_title_bar_close_link').onclick = function() {return false;};
		}
			
		Event.observe($('lightwindow_previous'), 'click', this.navigateWindow.bind(this, 'previous'), false);
		$('lightwindow_previous').onclick = function() {return false;};		
		Event.observe($('lightwindow_next'), 'click', this.navigateWindow.bind(this, 'next'), false);
		$('lightwindow_next').onclick = function() {return false;};

		if (!this.options.hideGalleryTab) {
			Event.observe($('lightwindow_galleries_tab'), 'click', this._handleGalleryAnimation.bind(this, true), false);
			$('lightwindow_galleries_tab').onclick = function() {return false;};
		}
		
		// Because we use position absolute, kill the scroll Wheel on animations
		if (Prototype.Browser.IE) {
			Event.observe(document, 'mousewheel', this._stopScrolling.bindAsEventListener(this), false);
		} else {
			Event.observe(window, 'DOMMouseScroll', this._stopScrolling.bindAsEventListener(this), false);
		}
				
		Event.observe(overlay, 'click', this.deactivate.bindAsEventListener(this), false);
		overlay.onclick = function() {return false;};
	},
	//
	//  Add loading window markup
	//
	_addLoadingWindowMarkup : function() {
		$('lightwindow_contents').innerHTML += this.options.skin.loading;
	},
	//
	//  Setup the window elements
	//
	_setupWindowElements : function(link) {
		this.element = link;
		this.element.title = null ? '' : link.getAttribute('title');
		this.element.author = null ? '' : link.getAttribute('author');
		this.element.caption = null ? '' : link.getAttribute('caption');
		this.element.rel = null ? '' : link.getAttribute('rel');
		this.element.params = null ? '' : link.getAttribute('params');

		// Set the window type
		this.contentToFetch = this.element.href;
		this.windowType = this._getParameter('lightwindow_type') ? this._getParameter('lightwindow_type') : this._fileType(this.contentToFetch);	
	},
	//
	//  Clear the window contents out
	//
	_clearWindowContents : function(contents) {
		// If there is an iframe, its got to go
		if ($('lightwindow_iframe')) {
			Element.remove($('lightwindow_iframe'));
		}

		// Stop playing an object if its still around
		if ($('lightwindow_media_primary')) {
			try {
				$('lightwindow_media_primary').Stop();
			} catch(e) {}
			Element.remove($('lightwindow_media_primary'));
		}

		// Stop playing an object if its still around		
		if ($('lightwindow_media_secondary')) {
			try {
				$('lightwindow_media_secondary').Stop();
			} catch(e) {}
			Element.remove($('lightwindow_media_secondary'));
		}
		
		this.activeGallery = false;
		this._handleNavigation(this.activeGallery);
		
		if (contents) {
			// Empty the contents
			$('lightwindow_contents').innerHTML = '';
			
			// Reset the scroll bars
			$('lightwindow_contents').setStyle({
				overflow: 'hidden'
			});		
			
			if (!this.windowActive) {
				$('lightwindow_data_slide_inner').setStyle({
					display: 'none'
				});

				$('lightwindow_title_bar_title').innerHTML = '';
			}

			// Because of browser differences and to maintain flexible captions we need to reset this height at close
			$('lightwindow_data_slide').setStyle({
				height: 'auto'
			});
		}
		
		this.resizeTo.height = null;
		this.resizeTo.width = null;
	},
	//
	//	Set the status of our animation to keep things from getting clunky
	//
	_setStatus : function(status) {
		this.animating = status;
		if (status) {
			Element.show('lightwindow_loading');
		}
		if (!(/MSIE 6./i.test(navigator.userAgent))) {
			this._fixedWindow(status);
		}
	},
	//
	//  Make this window Fixed
	//
	_fixedWindow : function(status) {
		if (status) {
			if (this.windowActive) {
				this._getScroll();
				$('lightwindow').setStyle({
					position: 'absolute',
					top: parseFloat($('lightwindow').getStyle('top'))+this.pagePosition.y+'px',
					left: parseFloat($('lightwindow').getStyle('left'))+this.pagePosition.x+'px'
				});		
			} else {
				$('lightwindow').setStyle({
					position: 'absolute'
				});						
			}
		} else {
			if (this.windowActive) {
				this._getScroll();
				$('lightwindow').setStyle({
					position: 'fixed',
					top: parseFloat($('lightwindow').getStyle('top'))-this.pagePosition.y+'px',
					left: parseFloat($('lightwindow').getStyle('left'))-this.pagePosition.x+'px'
				});		
			} else {
				if ($('lightwindow_iframe')) {
					// Ideally here we would set a 50% value for top and left, but Safari rears it ugly head again and we need to do it by pixels
					this._browserDimensions();
				}
				$('lightwindow').setStyle({
					position: 'fixed',
					top: (parseFloat(this._getParameter('lightwindow_top')) ? parseFloat(this._getParameter('lightwindow_top'))+'px' : this.dimensions.viewport.height/2+'px'),
					left: (parseFloat(this._getParameter('lightwindow_left')) ? parseFloat(this._getParameter('lightwindow_left'))+'px' : this.dimensions.viewport.width/2+'px')
				});
			}
		}
	},
	//
	//	Prepare the window for IE.
	//
	_prepareIE : function(setup) {
		if (Prototype.Browser.IE) {
			var height, overflowX, overflowY;
			if (setup) { 
				var height = '100%';
			} else {
				var height = 'auto';
			}
			var body = document.getElementsByTagName('body')[0];
			var html = document.getElementsByTagName('html')[0];
			html.style.height = body.style.height = height;
		}
	},
	_stopScrolling : function(e) {
		if (this.animating) {
			if (e.preventDefault) {
				e.preventDefault();
			}
			e.returnValue = false;		
		}
	},
	//
	//	Get the scroll for the page.
	//
	_getScroll : function(){
      	if(typeof(window.pageYOffset) == 'number') {
        	this.pagePosition.x = window.pageXOffset;
        	this.pagePosition.y = window.pageYOffset;
      	} else if(document.body && (document.body.scrollLeft || document.body.scrollTop)) {
	       	this.pagePosition.x = document.body.scrollLeft;
        	this.pagePosition.y = document.body.scrollTop;
		} else if(document.documentElement) {
        	this.pagePosition.x = document.documentElement.scrollLeft;
        	this.pagePosition.y = document.documentElement.scrollTop;
      	}
	},
	//
	//	Reset the scroll.
	//
	_setScroll : function(x, y) {
		document.documentElement.scrollLeft = x; 
		document.documentElement.scrollTop = y; 
	},
	//
	//	Hide Selects from the page because of IE.
	//     We could use iframe shims instead here but why add all the extra markup for one browser when this is much easier and cleaner
	//
	_toggleTroubleElements : function(visibility, content){
		
		if (content) {
			var selects = $('lightwindow_contents').getElementsByTagName('select');
		} else {
			var selects = document.getElementsByTagName('select');
		}
		
		for(var i = 0; i < selects.length; i++) {
			selects[i].style.visibility = visibility;
		}
		
		if (!content) {
			if (this.options.hideFlash){
				var objects = document.getElementsByTagName('object');
				for (i = 0; i != objects.length; i++) {
					objects[i].style.visibility = visibility;
				}
				var embeds = document.getElementsByTagName('embed');
				for (i = 0; i != embeds.length; i++) {
					embeds[i].style.visibility = visibility;
				}
			}
			var iframes = document.getElementsByTagName('iframe');
			for (i = 0; i != iframes.length; i++) {
				iframes[i].style.visibility = visibility;
			}
		}
	},
	// 
	//  Get the actual page size
	//
	_getPageDimensions : function() {
		var xScroll, yScroll;
		if (window.innerHeight && window.scrollMaxY) {	
			xScroll = document.body.scrollWidth;
			yScroll = window.innerHeight + window.scrollMaxY;
		} else if (document.body.scrollHeight > document.body.offsetHeight){ 
			xScroll = document.body.scrollWidth;
			yScroll = document.body.scrollHeight;
		} else { 
			xScroll = document.body.offsetWidth;
			yScroll = document.body.offsetHeight;
		}

		var windowWidth, windowHeight;
		if (self.innerHeight) {	
			windowWidth = self.innerWidth;
			windowHeight = self.innerHeight;
		} else if (document.documentElement && document.documentElement.clientHeight) { 
			windowWidth = document.documentElement.clientWidth;
			windowHeight = document.documentElement.clientHeight;
		} else if (document.body) { 
			windowWidth = document.body.clientWidth;
			windowHeight = document.body.clientHeight;
		}	

		if(yScroll < windowHeight){
			this.pageDimensions.height = windowHeight;
		} else { 
			this.pageDimensions.height = yScroll;
		}

		if(xScroll < windowWidth){	
			this.pageDimensions.width = windowWidth;
		} else {
			this.pageDimensions.width = xScroll;
		}
	},
	//
	//	Display the lightWindow.
	//
	_displayLightWindow : function(display, visibility) {
		$('lightwindow_overlay').style.display = $('lightwindow').style.display = $('lightwindow_container').style.display = display;	
		$('lightwindow_overlay').style.visibility = $('lightwindow').style.visibility = $('lightwindow_container').style.visibility = visibility;
	},
	//
	//	Setup Dimensions of lightwindow.

	//
	_setupDimensions : function() {

		var originalHeight, originalWidth;
		switch (this.windowType) {
			case 'page' :
				originalHeight = this.options.dimensions.page.height;
				originalWidth = this.options.dimensions.page.width;
				break;

			case 'image' :
				originalHeight = this.options.dimensions.image.height;
				originalWidth = this.options.dimensions.image.width;
				break;
				
			case 'media' :
				originalHeight = this.options.dimensions.media.height;
				originalWidth = this.options.dimensions.media.width;
				break;
			
			case 'external' : 
				originalHeight = this.options.dimensions.external.height;
				originalWidth = this.options.dimensions.external.width;
				break;
				
			case 'inline' :
				originalHeight = this.options.dimensions.inline.height;
				originalWidth = this.options.dimensions.inline.width;
				break;
				
			default :
				originalHeight = this.options.dimensions.page.height;
				originalWidth = this.options.dimensions.page.width;
				break;
				
		}

		var offsetHeight = this._getParameter('lightwindow_top') ? parseFloat(this._getParameter('lightwindow_top'))+this.pagePosition.y : this.dimensions.viewport.height/2+this.pagePosition.y;
		var offsetWidth = this._getParameter('lightwindow_left') ? parseFloat(this._getParameter('lightwindow_left'))+this.pagePosition.x : this.dimensions.viewport.width/2+this.pagePosition.x;
		
		// So if a theme has say shadowed edges, they should be consistant and take care of in the contentOffset
		$('lightwindow').setStyle({
			top: offsetHeight+'px',
			left: offsetWidth+'px'
		});
		
		$('lightwindow_container').setStyle({
			height: originalHeight+'px',
			width: originalWidth+'px',
			left: -(originalWidth/2)+'px',
			top: -(originalHeight/2)+'px'
		});

		$('lightwindow_contents').setStyle({
			height: originalHeight+'px',
			width: originalWidth+'px'
		});
	},
	//
	//	Get the type of file.
	//
	_fileType : function(url) {
		var image = new RegExp("[^\.]\.("+this.options.fileTypes.image.join('|')+")\s*$", "i");
		if (image.test(url)) return 'image';
		if (url.indexOf('#') > -1 && (document.domain == this._getDomain(url))) return 'inline';		
		if (url.indexOf('?') > -1) url = url.substring(0, url.indexOf('?'));
		var type = 'unknown';
		var page = new RegExp("[^\.]\.("+this.options.fileTypes.page.join('|')+")\s*$", "i");
		var media = new RegExp("[^\.]\.("+this.options.fileTypes.media.join('|')+")\s*$", "i");
		if (document.domain != this._getDomain(url)) type = 'external';
	  	if (media.test(url)) type = 'media';
		if (type == 'external' || type == 'media') return type;
	  	if (page.test(url) || url.substr((url.length-1), url.length) == '/') type = 'page';
		return type;
	},
	//
	//  Get file Extension
	//
	_fileExtension : function(url) {
		if (url.indexOf('?') > -1) {
			url = url.substring(0, url.indexOf('?'));
		}
		var extenstion = '';
		for (var x = (url.length-1); x > -1; x--) {
			if (url.charAt(x) == '.') {
				return extenstion;
			}
			extenstion = url.charAt(x)+extenstion;
		}
	},
	//
	//	Monitor the keyboard while this lightwindow is up
	//
	_monitorKeyboard : function(status) {
		if (status) document.onkeydown = this._eventKeypress.bind(this); 
		else document.onkeydown = '';
	},
	//
	//  Perform keyboard actions
	//
	_eventKeypress : function(e) {
		if (e == null) {
			var keycode = event.keyCode;
		} else {
			var keycode = e.which;
		}
		
		switch (keycode) { 
			case 27: 
				this.deactivate(); 
				break;
			
			case 13:
				return;
				
			default:
				break;
		}
	
		// Gotta stop those quick fingers
		if (this.animating) {
			return false;
		}
		
		switch (String.fromCharCode(keycode).toLowerCase()) {
			case 'p':
				if (this.navigationObservers.previous) {
					this.navigateWindow('previous');
				}
				break;
				
			case 'n':
				if (this.navigationObservers.next) {
					this.navigateWindow('next');
				}
				break;
				
			default:
				break;
		}
	},
	//
	//	Get Gallery Information
	//
	_getGalleryInfo : function(rel) {
		if (!rel) return false;
		if (rel.indexOf('[') > -1) {
			return new Array(escape(rel.substring(0, rel.indexOf('['))), escape(rel.substring(rel.indexOf('[')+1, rel.indexOf(']'))));
		} else {
			return false;
		}
	},
	//
	//	Get the domain from a string.
	//
	_getDomain : function(url) {    
        var leadSlashes = url.indexOf('//');
        var domainStart = leadSlashes+2;
        var withoutResource = url.substring(domainStart, url.length);
        var nextSlash = withoutResource.indexOf('/');
        var domain = withoutResource.substring(0, nextSlash);
		if (domain.indexOf(':') > -1){
			var portColon = domain.indexOf(':');
			domain = domain.substring(0, portColon);
       	}
		return domain;
    },
	//
	//	Get the value from the params attribute string.
	//
	_getParameter : function(parameter, parameters) {
		if (!this.element) return false;
		if (parameter == 'lightwindow_top' && this.element.top) {
			return unescape(this.element.top);
		} else if (parameter == 'lightwindow_left' && this.element.left) {
			return unescape(this.element.left);
		} else if (parameter == 'lightwindow_type' && this.element.type) {
			return unescape(this.element.type);
		} else if (parameter == 'lightwindow_show_images' && this.element.showImages) {
			return unescape(this.element.showImages);
		} else if (parameter == 'lightwindow_height' && this.element.height) {
			return unescape(this.element.height);
		} else if (parameter == 'lightwindow_width' && this.element.width) {
			return unescape(this.element.width);
		} else if (parameter == 'lightwindow_loading_animation' && this.element.loadingAnimation) {
			return unescape(this.element.loadingAnimation);
		} else if (parameter == 'lightwindow_iframe_embed' && this.element.iframeEmbed) {
			return unescape(this.element.iframeEmbed);
		} else if (parameter == 'lightwindow_form' && this.element.form) {
			return unescape(this.element.form);
		} else {
			if (!parameters) {
				if (this.element.params) parameters = this.element.params;
				else return;
			}
			var value;
			var parameterArray = parameters.split(',');
			var compareString = parameter+'=';
			var compareLength = compareString.length;
			for (var i = 0; i < parameterArray.length; i++) {
				if (parameterArray[i].substr(0, compareLength) == compareString) {
					var currentParameter = parameterArray[i].split('=');
					value = currentParameter[1];
					break;
				}
			}
			if (!value) return false;
			else return unescape(value);
		}
	},
	//
	//  Get the Browser Viewport Dimensions
	//
	_browserDimensions : function() {
		if (Prototype.Browser.IE) {
            this.dimensions.viewport.height = document.documentElement.clientHeight;
            this.dimensions.viewport.width = document.documentElement.clientWidth;   
        } else {
            this.dimensions.viewport.height = window.innerHeight;
            this.dimensions.viewport.width = document.width || document.body.offsetWidth;
        }
	},
	//
	//  Get the scrollbar offset, I don't like this method but there is really no other way I can find.
	//
	_getScrollerWidth : function() {
	    var scrollDiv = Element.extend(document.createElement('div'));
		scrollDiv.setAttribute('id', 'lightwindow_scroll_div');
		scrollDiv.setStyle({
			position: 'absolute',
			top: '-10000px',
			left: '-10000px',
			width: '100px',
			height: '100px',
			overflow: 'hidden'
		});



	    var contentDiv = Element.extend(document.createElement('div'));
		contentDiv.setAttribute('id', 'lightwindow_content_scroll_div');
		contentDiv.setStyle({
			width: '100%',
			height: '200px'
		});

	    scrollDiv.appendChild(contentDiv);

		var body = document.getElementsByTagName('body')[0];
		body.appendChild(scrollDiv);

	    var noScroll = $('lightwindow_content_scroll_div').offsetWidth;
	    scrollDiv.style.overflow = 'auto';
    	var withScroll = $('lightwindow_content_scroll_div').offsetWidth;

	   	Element.remove($('lightwindow_scroll_div'));

	    this.scrollbarOffset = noScroll-withScroll;
	},
	

	//
	//  Add a param to an object dynamically created
	//
	_addParamToObject : function(name, value, object, id) {
		var param = document.createElement('param');
		param.setAttribute('value', value);
		param.setAttribute('name', name);
		if (id) {
			param.setAttribute('id', id);
		}
		object.appendChild(param);
		return object;
	},
	//
	//  Get the outer HTML of an object CROSS BROWSER
	//
	_outerHTML : function(object) {
 		if (Prototype.Browser.IE) {
			return object.outerHTML;
		} else {
			var clone = object.cloneNode(true);
			var cloneDiv = document.createElement('div');
			cloneDiv.appendChild(clone);
			return cloneDiv.innerHTML;
		}
	},
	//
	//  Convert an object to markup
	//
	_convertToMarkup : function(object, closeTag) {
		var markup = this._outerHTML(object).replace('</'+closeTag+'>', '');
		if (Prototype.Browser.IE) {
			for (var i = 0; i < object.childNodes.length; i++){
				markup += this._outerHTML(object.childNodes[i]);
			}
			markup += '</'+closeTag+'>';
		}
		return markup;
	},
	//
	//  Depending what type of browser it is we have to append the object differently... DAMN YOU IE!!
	//
	_appendObject : function(object, closeTag, appendTo) {
		if (Prototype.Browser.IE) {
			appendTo.innerHTML += this._convertToMarkup(object, closeTag);
			
			// Fix the Eolas activate thing but only for specified media, for example doing this to a quicktime film breaks it.
			if (this.options.EOLASFix.indexOf(this._fileType(this.element.href)) > -1) {
				var objectElements = document.getElementsByTagName('object');
				for (var i = 0; i < objectElements.length; i++) {
					if (objectElements[i].getAttribute("data")) objectElements[i].removeAttribute('data');
					objectElements[i].outerHTML = objectElements[i].outerHTML;
					objectElements[i].style.visibility = "visible";
				}
			}
		} else {
			appendTo.appendChild(object);	
		}	
	},
	//
	//  Add in iframe
	//
	_appendIframe : function(scroll) {
		var iframe = document.createElement('iframe');
		iframe.setAttribute('id', 'lightwindow_iframe');
		iframe.setAttribute('name', 'lightwindow_iframe');
		iframe.setAttribute('src', 'about:blank');
		iframe.setAttribute('height', '100%');
		iframe.setAttribute('width', '100%');
		iframe.setAttribute('frameborder', '0');
		iframe.setAttribute('marginwidth', '0');
		iframe.setAttribute('marginheight', '0');
		iframe.setAttribute('scrolling', scroll);	
		
		this._appendObject(iframe, 'iframe', $('lightwindow_contents'));
	},
	//
	//  Write Content to the iframe using the skin
	//
	_writeToIframe : function(content) {
		var template = this.options.skin.iframe;
		template = template.replace('{body_replace}', content); 
		if ($('lightwindow_iframe').contentWindow){
			$('lightwindow_iframe').contentWindow.document.open();
			$('lightwindow_iframe').contentWindow.document.write(template);
			$('lightwindow_iframe').contentWindow.document.close();
		} else {
			$('lightwindow_iframe').contentDocument.open();
			$('lightwindow_iframe').contentDocument.write(template);
			$('lightwindow_iframe').contentDocument.close();
		}
	},
	//
	//  Load the window Information
	//  
	_loadWindow : function() {
		switch (this.windowType) {
			case 'image' :

				var current = 0;
				var images = [];
				this.checkImage = [];
				this.resizeTo.height = this.resizeTo.width = 0;
				this.imageCount = this._getParameter('lightwindow_show_images') ? parseInt(this._getParameter('lightwindow_show_images')) : 1;

				// If there is a gallery get it
				if (gallery = this._getGalleryInfo(this.element.rel)) {	
					for (current = 0; current < this.galleries[gallery[0]][gallery[1]].length; current++) {
						if (this.contentToFetch.indexOf(this.galleries[gallery[0]][gallery[1]][current].href) > -1) {
							break;
						}
					}
					if (this.galleries[gallery[0]][gallery[1]][current-this.imageCount]) {
						this.navigationObservers.previous = this.galleries[gallery[0]][gallery[1]][current-this.imageCount];
					} else {
						this.navigationObservers.previous = false;
					}
					if (this.galleries[gallery[0]][gallery[1]][current+this.imageCount]) {
						this.navigationObservers.next = this.galleries[gallery[0]][gallery[1]][current+this.imageCount];
					} else {
						this.navigationObservers.next = false;
					}
					
					this.activeGallery = true;
				} else {
					this.navigationObservers.previous = false;
					this.navigationObservers.next = false;					

					this.activeGallery = false;
				}
				
				for (var i = current; i < (current+this.imageCount); i++) {
		
					if (gallery && this.galleries[gallery[0]][gallery[1]][i]) {
						this.contentToFetch = this.galleries[gallery[0]][gallery[1]][i].href;
						
						this.galleryLocation = {current: (i+1)/this.imageCount, total: (this.galleries[gallery[0]][gallery[1]].length)/this.imageCount};
											
						if (!this.galleries[gallery[0]][gallery[1]][i+this.imageCount]) {
							$('lightwindow_next').setStyle({
								display: 'none'
							});
						} else {
							$('lightwindow_next').setStyle({
								display: 'block'
							});
							$('lightwindow_next_title').innerHTML = this.galleries[gallery[0]][gallery[1]][i+this.imageCount].title;
						}
						
						if (!this.galleries[gallery[0]][gallery[1]][i-this.imageCount]) {
							$('lightwindow_previous').setStyle({
								display: 'none'
							});
						} else {
							$('lightwindow_previous').setStyle({
								display: 'block'
							});
							$('lightwindow_previous_title').innerHTML = this.galleries[gallery[0]][gallery[1]][i-this.imageCount].title;
						}
					}

					images[i] = document.createElement('img');
					images[i].setAttribute('id', 'lightwindow_image_'+i);
					images[i].setAttribute('border', '0');
					images[i].setAttribute('src', this.contentToFetch);
					$('lightwindow_contents').appendChild(images[i]);

					// We have to do this instead of .onload 
					this.checkImage[i] = new PeriodicalExecuter(function(i) {
						if (!(typeof $('lightwindow_image_'+i).naturalWidth != "undefined" && $('lightwindow_image_'+i).naturalWidth == 0)) {
	
							this.checkImage[i].stop();
	
							var imageHeight = $('lightwindow_image_'+i).getHeight();
							if (imageHeight > this.resizeTo.height) {
								this.resizeTo.height = imageHeight;
							}
							this.resizeTo.width += $('lightwindow_image_'+i).getWidth();
							this.imageCount--;
	
							$('lightwindow_image_'+i).setStyle({
								height: '100%'
							});
	
						 	if (this.imageCount == 0) {
								this._processWindow();
						 	}
						}
					
					}.bind(this, i), 1);			
				}


			break;
		
		case 'media' :			
		
			var current = 0;
			this.resizeTo.height = this.resizeTo.width = 0;

			// If there is a gallery get it
			if (gallery = this._getGalleryInfo(this.element.rel)) {	
				for (current = 0; current < this.galleries[gallery[0]][gallery[1]].length; current++) {
					if (this.contentToFetch.indexOf(this.galleries[gallery[0]][gallery[1]][current].href) > -1) {
						break;
					}
				}
				
				if (this.galleries[gallery[0]][gallery[1]][current-1]) {
					this.navigationObservers.previous = this.galleries[gallery[0]][gallery[1]][current-1];
				} else {
					this.navigationObservers.previous = false;
				}
				if (this.galleries[gallery[0]][gallery[1]][current+1]) {
					this.navigationObservers.next = this.galleries[gallery[0]][gallery[1]][current+1];
				} else {
					this.navigationObservers.next = false;
				}
		
				this.activeGallery = true;
			} else {
				this.navigationObservers.previous = false;
				this.navigationObservers.next = false;
				
				this.activeGallery = false;
			}
		

			if (gallery && this.galleries[gallery[0]][gallery[1]][current]) {
				this.contentToFetch = this.galleries[gallery[0]][gallery[1]][current].href;

				this.galleryLocation = {current: current+1, total: this.galleries[gallery[0]][gallery[1]].length};
				
				if (!this.galleries[gallery[0]][gallery[1]][current+1]) {
					$('lightwindow_next').setStyle({
						display: 'none'
					});
				} else {
					$('lightwindow_next').setStyle({
						display: 'block'
					});
					$('lightwindow_next_title').innerHTML = this.galleries[gallery[0]][gallery[1]][current+1].title;
				}
				
				if (!this.galleries[gallery[0]][gallery[1]][current-1]) {
					$('lightwindow_previous').setStyle({
						display: 'none'
					});
				} else {
					$('lightwindow_previous').setStyle({
						display: 'block'
					});
					$('lightwindow_previous_title').innerHTML = this.galleries[gallery[0]][gallery[1]][current-1].title;
				}
			}
			
			if (this._getParameter('lightwindow_iframe_embed')) {
				this.resizeTo.height = this.dimensions.viewport.height;
				this.resizeTo.width = this.dimensions.viewport.width;	
			} else {
				this.resizeTo.height = this._getParameter('lightwindow_height');
				this.resizeTo.width = this._getParameter('lightwindow_width');				
			}
			
			this._processWindow();
			
			break;

		case 'external' :		

			this._appendIframe('auto');

			this.resizeTo.height = this.dimensions.viewport.height;
			this.resizeTo.width = this.dimensions.viewport.width;
						
			this._processWindow();

			break;
				
		case 'page' :	
			
			var newAJAX = new Ajax.Request(
				this.contentToFetch, {
					method: 'get', 
					parameters: '', 
					onComplete: function(response) {
						$('lightwindow_contents').innerHTML += response.responseText;
						this.resizeTo.height = $('lightwindow_contents').scrollHeight+(this.options.contentOffset.height);
						this.resizeTo.width = $('lightwindow_contents').scrollWidth+(this.options.contentOffset.width);
						this._processWindow();
					}.bind(this)
				}
			);
			
			break;
			
		case 'inline' : 
		
			var content = this.contentToFetch;
			if (content.indexOf('?') > -1) {
				content = content.substring(0, content.indexOf('?'));
			}
			content = content.substring(content.indexOf('#')+1);
			
			new Insertion.Top($('lightwindow_contents'), $(content).innerHTML);
			
			this.resizeTo.height = $('lightwindow_contents').scrollHeight+(this.options.contentOffset.height);
			this.resizeTo.width = $('lightwindow_contents').scrollWidth+(this.options.contentOffset.width);
			
			this._toggleTroubleElements('hidden', true); 			
			this._processWindow();
			
			break;
			
		default : 
			throw("Page Type could not be determined, please amend this lightwindow URL "+this.contentToFetch);
			break;
		}
	},
	//
	//  Resize the Window to fit the viewport if necessary
	//
	_resizeWindowToFit : function() {
		if (this.resizeTo.height+this.dimensions.cruft.height > this.dimensions.viewport.height) {
			var heightRatio = this.resizeTo.height/this.resizeTo.width;
			this.resizeTo.height = this.dimensions.viewport.height-this.dimensions.cruft.height-(2*this.options.viewportPadding);
			// We only care about ratio's with this window type			
			if (this.windowType == 'image' || (this.windowType == 'media' && !this._getParameter('lightwindow_iframe_embed'))) {
				this.resizeTo.width = this.resizeTo.height/heightRatio;
				$('lightwindow_data_slide_inner').setStyle({
					width: this.resizeTo.width+'px'
				});			
			}
		} 
		if (this.resizeTo.width+this.dimensions.cruft.width > this.dimensions.viewport.width) {
			var widthRatio = this.resizeTo.width/this.resizeTo.height;
			this.resizeTo.width = this.dimensions.viewport.width-2*this.dimensions.cruft.width-(2*this.options.viewportPadding);
			// We only care about ratio's with this window type
			if (this.windowType == 'image' || (this.windowType == 'media' && !this._getParameter('lightwindow_iframe_embed'))) {
				this.resizeTo.height = this.resizeTo.width/widthRatio;
				$('lightwindow_data_slide_inner').setStyle({
					height: this.resizeTo.height+'px'
				});
			}
		}
			
	},
	//
	//  Set the Window to a preset size
	//
	_presetWindowSize : function() {
		if (this._getParameter('lightwindow_height')) {
			this.resizeTo.height = parseFloat(this._getParameter('lightwindow_height'));
		}
		if (this._getParameter('lightwindow_width')) {
			this.resizeTo.width = parseFloat(this._getParameter('lightwindow_width'));
		}
	},
	//
	//  Process the Window
	//
	_processWindow : function() {
		// Clean out our effects
		this.dimensions.dataEffects = [];

		// Set up the data-slide if we have caption information
		if (this.element.caption || this.element.author || (this.activeGallery && this.options.showGalleryCount)) {
			if (this.element.caption) {
				$('lightwindow_data_caption').innerHTML = this.element.caption;
				$('lightwindow_data_caption').setStyle({
					display: 'block'
				});
			} else {
				$('lightwindow_data_caption').setStyle({
					display: 'none'
				});				
			}
			if (this.element.author) {
				$('lightwindow_data_author').innerHTML = this.element.author;
				$('lightwindow_data_author_container').setStyle({
					display: 'block'
				});
			} else {
				$('lightwindow_data_author_container').setStyle({
					display: 'none'
				});				
			}
			if (this.activeGallery && this.options.showGalleryCount) {
				$('lightwindow_data_gallery_current').innerHTML = this.galleryLocation.current;
				$('lightwindow_data_gallery_total').innerHTML = this.galleryLocation.total;
				$('lightwindow_data_gallery_container').setStyle({
					display: 'block'
				});
			} else {
				$('lightwindow_data_gallery_container').setStyle({
					display: 'none'
				});				
			}

			$('lightwindow_data_slide_inner').setStyle({
				width: this.resizeTo.width+'px',
				height: 'auto',
				visibility: 'visible',
				display: 'block'
			});
			$('lightwindow_data_slide').setStyle({
				height: $('lightwindow_data_slide').getHeight()+'px',
				width: '1px',
				overflow: 'hidden',
				display: 'block'
			});
		} else {
			$('lightwindow_data_slide').setStyle({
				display: 'none',
				width: 'auto'
			});
			$('lightwindow_data_slide_inner').setStyle({
				display: 'none',
				visibility: 'hidden',
				width: this.resizeTo.width+'px',
				height: '0px'
			});
		}
				
		if (this.element.title != 'null') {		
			$('lightwindow_title_bar_title').innerHTML = this.element.title;
		} else {
			$('lightwindow_title_bar_title').innerHTML = '';
		}
		
		var originalContainerDimensions = {height: $('lightwindow_container').getHeight(), width: $('lightwindow_container').getWidth()};
		// Position the window
    	$('lightwindow_container').setStyle({
			height: 'auto',
			// We need to set the width to a px not auto as opera has problems with it
			width: $('lightwindow_container').getWidth()+this.options.contentOffset.width-(this.windowActive ? this.options.contentOffset.width : 0)+'px'
		});
		var newContainerDimensions = {height: $('lightwindow_container').getHeight(), width: $('lightwindow_container').getWidth()};
 		
		// We need to record the container dimension changes
		this.containerChange = {height: originalContainerDimensions.height-newContainerDimensions.height, width: originalContainerDimensions.width-newContainerDimensions.width};

		// Get out general dimensions
		this.dimensions.container = {height: $('lightwindow_container').getHeight(), width: $('lightwindow_container').getWidth()};
		this.dimensions.cruft = {height: this.dimensions.container.height-$('lightwindow_contents').getHeight()+this.options.contentOffset.height, width: this.dimensions.container.width-$('lightwindow_contents').getWidth()+this.options.contentOffset.width};
		
		// Set Sizes if we need too
		this._presetWindowSize();
		this._resizeWindowToFit(); // Even if the window is preset we still don't want it to go outside of the viewport

		if (!this.windowActive) {
			// Position the window
		   	$('lightwindow_container').setStyle({
				left: -(this.dimensions.container.width/2)+'px',
				top: -(this.dimensions.container.height/2)+'px'
			});
		}
	   	$('lightwindow_container').setStyle({
			height: this.dimensions.container.height+'px',
			width: this.dimensions.container.width+'px'
		});
		
		// We are ready, lets show this puppy off!
		this._displayLightWindow('block', 'visible');
		this._animateLightWindow();
	},
	//
	//  Fire off our animation handler
	//
	_animateLightWindow : function() {
		if (this.options.animationHandler) {
			this.options.animationHandler().bind(this);
		} else {
			this._defaultAnimationHandler();
		}
	},
	//
	//  Fire off our transition handler
	//
	_handleNavigation : function(display) {
		if (this.options.navigationHandler) {
			this.options.navigationHandler().bind(this, display);
		} else {
			this._defaultDisplayNavigation(display);
		}
	},
	//
	//  Fire off our transition handler
	//
	_handleTransition : function() {
		if (this.options.transitionHandler) {
			this.options.transitionHandler().bind(this);
		} else {
			this._defaultTransitionHandler();
		}
	},
	//
	//  Handle the finish of the window animation
	// 
	_handleFinalWindowAnimation : function(delay) {
		if (this.options.finalAnimationHandler) {
			this.options.finalAnimationHandler().bind(this, delay);
		} else {
			this._defaultfinalWindowAnimationHandler(delay);
		}		
	},
	//
	//  Handle the gallery Animation
	// 
	_handleGalleryAnimation : function(list) {
		if (this.options.galleryAnimationHandler) {
			this.options.galleryAnimationHandler().bind(this, list);
		} else {
			this._defaultGalleryAnimationHandler(list);
		}		
	},
	//
	//  Display the navigation 
	//
	_defaultDisplayNavigation : function(display) {
		if (display) {
			$('lightwindow_navigation').setStyle({
				display: 'block',
				height: $('lightwindow_contents').getHeight()+'px',
				width: '100%',
				marginTop: this.options.dimensions.titleHeight+'px'
			});			
		} else {
			$('lightwindow_navigation').setStyle({
				display: 'none',
				height: 'auto',
				width: 'auto'
			});			
		}
	},
	//
	//  This is the default animation handler for LightWindow
	//
	_defaultAnimationHandler : function() {	
		// Now that we have figures out the cruft lets make the caption go away and add its effects
		if (this.element.caption || this.element.author || (this.activeGallery && this.options.showGalleryCount)) {
			$('lightwindow_data_slide').setStyle({
				display: 'none',
				width: 'auto'
			});
			this.dimensions.dataEffects.push(
				new Effect.SlideDown('lightwindow_data_slide', {sync: true}),
				new Effect.Appear('lightwindow_data_slide', {sync: true, from: 0.0, to: 1.0})
			);
		}

		// Set up the Title if we have one
		$('lightwindow_title_bar_inner').setStyle({
			height: '0px',
			marginTop: this.options.dimensions.titleHeight+'px'
		});
		
		// We always want the title bar as well
		this.dimensions.dataEffects.push(
			new Effect.Morph('lightwindow_title_bar_inner', {sync: true, style: {height: this.options.dimensions.titleHeight+'px', marginTop: '0px'}}),
		 	new Effect.Appear('lightwindow_title_bar_inner', {sync: true, from: 0.0, to: 1.0})
		);		
		
		if (!this.options.hideGalleryTab) {
			this._handleGalleryAnimation(false);
			if ($('lightwindow_galleries_tab_container').getHeight() == 0) {
				this.dimensions.dataEffects.push(
					new Effect.Morph('lightwindow_galleries_tab_container', {sync: true, style: {height: '20px', marginTop: '0px'}})
				);
				$('lightwindow_galleries').setStyle({
					width: '0px'
				});
			}
		}
		
		var resized = false;
		var ratio = this.dimensions.container.width-$('lightwindow_contents').getWidth()+this.resizeTo.width+this.options.contentOffset.width;
		if (ratio != $('lightwindow_container').getWidth()) {
			new Effect.Parallel([
					new Effect.Scale('lightwindow_contents', 100*(this.resizeTo.width/$('lightwindow_contents').getWidth()), {scaleFrom: 100*($('lightwindow_contents').getWidth()/($('lightwindow_contents').getWidth()+(this.options.contentOffset.width))), sync: true,  scaleY: false, scaleContent: false}),
					new Effect.Scale('lightwindow_container', 100*(ratio/(this.dimensions.container.width)), {sync: true, scaleY: false, scaleFromCenter: true, scaleContent: false})
				], {
					duration: this.duration, 
					delay: 0.25,
					queue: {position: 'end', scope: 'lightwindowAnimation'}
				}
			);		
		}
		
		ratio = this.dimensions.container.height-$('lightwindow_contents').getHeight()+this.resizeTo.height+this.options.contentOffset.height;
		if (ratio != $('lightwindow_container').getHeight()) {
			new Effect.Parallel([
					new Effect.Scale('lightwindow_contents', 100*(this.resizeTo.height/$('lightwindow_contents').getHeight()), {scaleFrom: 100*($('lightwindow_contents').getHeight()/($('lightwindow_contents').getHeight()+(this.options.contentOffset.height))), sync: true, scaleX: false, scaleContent: false}),
					new Effect.Scale('lightwindow_container', 100*(ratio/(this.dimensions.container.height)), {sync: true, scaleX: false, scaleFromCenter: true, scaleContent: false})
				], {
					duration: this.duration, 
					afterFinish: function() {				
						if (this.dimensions.dataEffects.length > 0) {
							if (!this.options.hideGalleryTab) {
								$('lightwindow_galleries').setStyle({
									width: this.resizeTo.width+'px'
								});
							}
							new Effect.Parallel(this.dimensions.dataEffects, {
									duration: this.duration,
									afterFinish: function() {
										this._finishWindow();
									}.bind(this),
									queue: {position: 'end', scope: 'lightwindowAnimation'} 
								}
							);
						}
					}.bind(this), 
					queue: {position: 'end', scope: 'lightwindowAnimation'} 
				}
			);
			resized = true;
		}
		
		// We need to do our data effect since there was no resizing
		if (!resized && this.dimensions.dataEffects.length > 0) {	
			new Effect.Parallel(this.dimensions.dataEffects, {
					duration: this.duration,
					beforeStart: function() {
						if (!this.options.hideGalleryTab) {
							$('lightwindow_galleries').setStyle({
								width: this.resizeTo.width+'px'
							});
						}
						if (this.containerChange.height != 0 || this.containerChange.width != 0) {
							new Effect.MoveBy('lightwindow_container', this.containerChange.height, this.containerChange.width, {transition: Effect.Transitions.sinoidal});
						}
					}.bind(this),			
					afterFinish: function() {
						this._finishWindow();
					}.bind(this),
					queue: {position: 'end', scope: 'lightwindowAnimation'} 
				}
			);
		}			
		
	},
	//
	//  Finish up Window Animation
	//
	_defaultfinalWindowAnimationHandler : function(delay) {
		if (this.windowType == 'media' || this._getParameter('lightwindow_loading_animation')) {	
			// Because of major flickering with the overlay we just hide it in this case
			Element.hide('lightwindow_loading');
			this._handleNavigation(this.activeGallery);
			this._setStatus(false);
		} else {
			Effect.Fade('lightwindow_loading', {
				duration: 0.75,
				delay: 1.0, 
				afterFinish: function() {
					// Just in case we need some scroll goodness (this also avoids the swiss cheese effect)
					if (this.windowType != 'image' && this.windowType != 'media' && this.windowType != 'external') {
						$('lightwindow_contents').setStyle({
							overflow: 'auto'
						});
					}
					this._handleNavigation(this.activeGallery);
					this._defaultGalleryAnimationHandler();
					this._setStatus(false);
				}.bind(this),
				queue: {position: 'end', scope: 'lightwindowAnimation'}
			});
		}
	},
	//
	//  Handle the gallery Animation
	//
	_defaultGalleryAnimationHandler : function(list) {
		if (this.activeGallery) {
			$('lightwindow_galleries').setStyle({
				display: 'block',
				marginBottom: $('lightwindow_data_slide').getHeight()+this.options.contentOffset.height/2+'px'
			});
			$('lightwindow_navigation').setStyle({
				height: $('lightwindow_contents').getHeight()-20+'px'
			});
		} else {
			$('lightwindow_galleries').setStyle({
				display: 'none'
			});	
			$('lightwindow_galleries_tab_container').setStyle({
				height: '0px',
				marginTop: '20px'
			});
			$('lightwindow_galleries_list').setStyle({
				height: '0px'
			});
			return false;
		}
		
		if (list) {
			if ($('lightwindow_galleries_list').getHeight() == 0) {
				var height = $('lightwindow_contents').getHeight()*0.80;
				$('lightwindow_galleries_tab_span').className = 'down';
			} else {
				var height = 0;
				$('lightwindow_galleries_tab_span').className = 'up';
			}

			new Effect.Morph('lightwindow_galleries_list', {
				duration: this.duration,
				transition: Effect.Transitions.sinoidal,
				style: {height: height+'px'},
				beforeStart: function() {
					$('lightwindow_galleries_list').setStyle({
						overflow: 'hidden'
					});					
				},
				afterFinish: function() {
					$('lightwindow_galleries_list').setStyle({
						overflow: 'auto'
					});
				},
				queue: {position: 'end', scope: 'lightwindowAnimation'}
			});	
		}
		
		
	},
	//
	//  Default Transition Handler
	//
	_defaultTransitionHandler : function() {
		// Clean out our effects
		this.dimensions.dataEffects = [];

		// Now that we have figures out the cruft lets make the caption go away and add its effects
		if ($('lightwindow_data_slide').getStyle('display') != 'none') {
			this.dimensions.dataEffects.push(
				new Effect.SlideUp('lightwindow_data_slide', {sync: true}),
				new Effect.Fade('lightwindow_data_slide', {sync: true, from: 1.0, to: 0.0})
			);
		}
		
		if (!this.options.hideGalleryTab) {
			if ($('lightwindow_galleries').getHeight() != 0 && !this.options.hideGalleryTab) {
				this.dimensions.dataEffects.push(
					new Effect.Morph('lightwindow_galleries_tab_container', {sync: true, style: {height: '0px', marginTop: '20px'}})
				);
			}
			
			if ($('lightwindow_galleries_list').getHeight() != 0) {
				$('lightwindow_galleries_tab_span').className = 'up';
				this.dimensions.dataEffects.push(
					new Effect.Morph('lightwindow_galleries_list', {
						sync: true, 
						style: {height: '0px'},
						transition: Effect.Transitions.sinoidal,
						beforeStart: function() {
							$('lightwindow_galleries_list').setStyle({
								overflow: 'hidden'
							});					
						},
						afterFinish: function() {
							$('lightwindow_galleries_list').setStyle({
								overflow: 'auto'
							});
						}
					})
				);
			}
		}
		
		// We always want the title bar as well
		this.dimensions.dataEffects.push(
			new Effect.Morph('lightwindow_title_bar_inner', {sync: true, style: {height: '0px', marginTop: this.options.dimensions.titleHeight+'px'}}),
		 	new Effect.Fade('lightwindow_title_bar_inner', {sync: true, from: 1.0, to: 0.0})
		);

		new Effect.Parallel(this.dimensions.dataEffects, {
				duration: this.duration,
				afterFinish: function() {
					this._loadWindow();
				}.bind(this),
				queue: {position: 'end', scope: 'lightwindowAnimation'} 
			}
		);	
	},
	//
	//	Default Form handler for LightWindow
	//
	_defaultFormHandler : function(e) {
		var element = Event.element(e).parentNode;
		var parameterString = Form.serialize(this._getParameter('lightwindow_form', element.getAttribute('params')));
		if (this.options.formMethod == 'post') {
			var newAJAX = new Ajax.Request(element.href, { 
				method: 'post', 
				postBody: parameterString, 
				onComplete: this.openWindow.bind(this, element)
			});
		} else if (this.options.formMethod == 'get') {
			var newAJAX = new Ajax.Request(element.href, { 
				method: 'get', 
				parameters: parameterString, 
				onComplete: this.openWindow.bind(this, element)
			});
		}
	},
	// 
	//  Wrap everything up
	//
	_finishWindow : function() {
		if (this.windowType == 'external') {
			// We set the externals source here because it allows for a much smoother animation
			$('lightwindow_iframe').setAttribute('src', this.element.href);
			this._handleFinalWindowAnimation(1);	
		} else if (this.windowType == 'media') {

			var outerObject = document.createElement('object');
			outerObject.setAttribute('classid', this.options.classids[this._fileExtension(this.contentToFetch)]);
			outerObject.setAttribute('codebase', this.options.codebases[this._fileExtension(this.contentToFetch)]);
			outerObject.setAttribute('id', 'lightwindow_media_primary');
			outerObject.setAttribute('name', 'lightwindow_media_primary');
			outerObject.setAttribute('width', this.resizeTo.width);
			outerObject.setAttribute('height', this.resizeTo.height);
			outerObject = this._addParamToObject('movie', this.contentToFetch, outerObject);
			outerObject = this._addParamToObject('src', this.contentToFetch, outerObject);
			outerObject = this._addParamToObject('controller', 'true', outerObject);
			outerObject = this._addParamToObject('wmode', 'transparent', outerObject);
			outerObject = this._addParamToObject('cache', 'false', outerObject);
			outerObject = this._addParamToObject('quality', 'high', outerObject);

			if (!Prototype.Browser.IE) {
				var innerObject = document.createElement('object');
				innerObject.setAttribute('type', this.options.mimeTypes[this._fileExtension(this.contentToFetch)]);
				innerObject.setAttribute('data', this.contentToFetch);
				innerObject.setAttribute('id', 'lightwindow_media_secondary');
				innerObject.setAttribute('name', 'lightwindow_media_secondary');
				innerObject.setAttribute('width', this.resizeTo.width);
				innerObject.setAttribute('height', this.resizeTo.height);
				innerObject = this._addParamToObject('controller', 'true', innerObject);
				innerObject = this._addParamToObject('wmode', 'transparent', innerObject);
				innerObject = this._addParamToObject('cache', 'false', innerObject);
				innerObject = this._addParamToObject('quality', 'high', innerObject);
			
				outerObject.appendChild(innerObject);
			}	
			
			if (this._getParameter('lightwindow_iframe_embed')) {
				this._appendIframe('no');
				this._writeToIframe(this._convertToMarkup(outerObject, 'object'));
			} else {
				this._appendObject(outerObject, 'object', $('lightwindow_contents'));
			}

			this._handleFinalWindowAnimation(0);
		} else {
			this._handleFinalWindowAnimation(0);
		}

		// Initialize any actions
		this._setupActions();
	}
}

/*-----------------------------------------------------------------------------------------------*/

Event.observe(window, 'load', lightwindowInit, false);

//
//	Set up all of our links
//
var myLightWindow = null;
function lightwindowInit() {
	myLightWindow = new lightwindow();
}