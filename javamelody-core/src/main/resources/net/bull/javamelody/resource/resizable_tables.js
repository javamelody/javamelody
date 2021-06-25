// http://userscripts.org/scripts/show/26900

// **** ADAPTED FROM HTML DEMO AT.... *******
// DEMO = http://bz.var.ru/comp/web/resizable.html
// JS = http://bz.var.ru/comp/web/resizable-tables.js
//  ******* ORIGINAL SCRIPT HEADER *******
// Resizable Table Columns.
//  version: 1.0
//
// (c) 2006, bz
//
// 25.12.2006:  first working prototype
// 26.12.2006:  now works in IE as well but not in Opera (Opera is @#$%!)
// 27.12.2006:  changed initialization, now just make class='resizable' in table and load script
// ==========================================
// Greasemonkey Changelog
// 20080520 adapted by gollum for use in Greasemonkey scripts
// stripped out the cookie code
// removed references to "this" function calls
// added eventListeners/removeEventListeners
// wrapped all in anonymous function
// added script auto-updater

//(function() {

  function preventEvent(e) {
  	var ev = e || window.event;
  	if (ev.preventDefault) ev.preventDefault();
  	else ev.returnValue = false;
  	if (ev.stopPropagation)
  		ev.stopPropagation();
  	return false;
  }

  function getWidth(x) {
  	if (x.currentStyle)
  		// in IE
  		var y = x.clientWidth - parseInt(x.currentStyle["paddingLeft"]) - parseInt(x.currentStyle["paddingRight"]);
  		// for IE5: var y = x.offsetWidth;
  	else if (window.getComputedStyle)
  		// in Gecko
  		var y = document.defaultView.getComputedStyle(x,null).getPropertyValue("width");
  	return y || 0;
  }

  // main class prototype
  function ColumnResize(table) {
  	if (table.tagName != 'TABLE') return;

  	// this.id = table.id;

  	// ============================================================
  	// private data
  	// var self = this;

  	var dragColumns  = table.rows[0].cells; // first row columns, used for changing of width
  	if (!dragColumns) return; // return if no table exists or no one row exists

  	var dragColumnNo; // current dragging column
  	var dragX;        // last event X mouse coordinate

  	// var saveOnmouseup;   // save document onmouseup event handler
  	// var saveOnmousemove; // save document onmousemove event handler
  	var saveBodyCursor;  // save body cursor property

  	// ============================================================
  	// methods

  	// ============================================================
  	// do changes columns widths
  	// returns true if success and false otherwise
  	var changeColumnWidth = function(no, w) {
  		if (!dragColumns) return false;

  		if (no < 0) return false;
  		if (dragColumns.length < no) return false;

  		if (parseInt(dragColumns[no].style.width) <= -w) return false;
  		if (dragColumns[no+1] && parseInt(dragColumns[no+1].style.width) <= w) return false;

  		dragColumns[no].style.width = parseInt(dragColumns[no].style.width) + w +'px';
  		if (dragColumns[no+1])
  			dragColumns[no+1].style.width = parseInt(dragColumns[no+1].style.width) - w + 'px';

  		return true;
  	}

  	// ============================================================
  	// do drag column width
  	var columnDrag = function(e) {
  		var e = e || window.event;
  		var X = e.clientX || e.pageX;
  		if (!changeColumnWidth(dragColumnNo, X-dragX)) {
  			// stop drag!
  			stopColumnDrag(e);
  		}

  		dragX = X;
  		// prevent other event handling
  		preventEvent(e);
  		return false;
  	}

  	// ============================================================
  	// stops column dragging
  	var stopColumnDrag = function(e) {
  		var e = e || window.event;
  		if (!dragColumns) return;

  		// restore handlers & cursor
  		myRemoveEventListener(document, "mouseup", stopColumnDrag);
  		myRemoveEventListener(document, "mousemove", columnDrag);
  		document.body.style.cursor = saveBodyCursor;

  		preventEvent(e);
  	}

  	// ============================================================
  	// init data and start dragging
  	var startColumnDrag = function(e) {
  		var e = e || window.event;

  		// remember dragging object
  		dragColumnNo = (e.target || e.srcElement).parentNode.parentNode.cellIndex;
  		dragX = e.clientX || e.pageX;

  		// set up current columns widths in their particular attributes
  		// do it in two steps to avoid jumps on page!
  		var colWidth = new Array();
  		for (var i=0; i<dragColumns.length; i++)
  			colWidth[i] = parseInt( getWidth(dragColumns[i]) );
  		for (var i=0; i<dragColumns.length; i++) {
  			dragColumns[i].width = ""; // for sure
  			dragColumns[i].style.width = colWidth[i] + "px";
  		}

  		// saveOnmouseup       = document.onmouseup;
  		myAddEventListener(document, "mouseup", stopColumnDrag);

  		saveBodyCursor             = document.body.style.cursor;
  		document.body.style.cursor = 'w-resize';

  		// fire!
  		// saveOnmousemove      = document.onmousemove;
  		myAddEventListener(document, "mousemove", columnDrag);

  		preventEvent(e);
  	}

  	// prepare table header to be draggable
  	// it runs during class creation
  	for (var i=0; i<dragColumns.length; i++) {
  		var div = document.createElement("div");
  		div.style.position = "relative";
  		div.style.height = "100%";
  		div.style.width = "100%";
  		var innerDiv = document.createElement("div");
  		innerDiv.style.position = "absolute";
  		innerDiv.style.height = "100%";
  		innerDiv.style.width = "5px";
  		innerDiv.style.marginRight = "-5px";
  		innerDiv.style.left = "100%";
  		innerDiv.style.top = "0px";
  		innerDiv.style.cursor = "w-resize";
  		innerDiv.style.zIndex = "10";
  		div.appendChild(innerDiv);
  		while (dragColumns[i].childNodes.length) { div.appendChild(dragColumns[i].firstChild); }
  		dragColumns[i].appendChild(div);
  			// BUGBUG: calculate real border width instead of 5px!!!
  			tmp = dragColumns[i].firstChild.firstChild;
  			myAddEventListener(tmp, "mousedown", startColumnDrag);
  		}
  }
  
  function myAddEventListener(target, event, handler) {
		if (target.addEventListener) {
			target.addEventListener(event, handler, false);
		} else if (tmp.attachEvent) {
			// pour MSIE
			target.attachEvent("on" + event, handler);
		}
  }

  function myRemoveEventListener(target, event, handler) {
		if (target.removeEventListener) {
			target.removeEventListener(event, handler, false);
		} else if (tmp.attachEvent) {
			// pour MSIE
			target.detachEvent("on" + event, handler);
		}
}

  // select all tables and make resizable those that have 'sortable' class
  var resizableTables = new Array();
  function ResizableColumns() {
  	var tables = document.getElementsByTagName('table');
	for (var i=0; i<tables.length; i++) {
  		if (tables[i].className.search(/\bsortable\b/) != -1) {
  		 	// generate id
  			if (!tables[i].id) tables[i].id = 'table'+(i+1);
  			// make table resizable
  			resizableTables[resizableTables.length] = new ColumnResize(tables[i]);
  		}
  	}
  //	alert(resizableTables.length + ' tables was added.');
  }
  
//})();

//============================================================
//
// Usage. In your html code just include the follow:
//
//============================================================
// <table class='resizable'>
// ...
// </table>
//============================================================