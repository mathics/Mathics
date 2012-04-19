/**
    Mathics: a general-purpose computer algebra system
    Copyright (C) 2011 Jan Pöschko

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
**/

var deleting;
var blurredElement;

var movedItem;

var clickedQuery;

var lastFocus = null;

function getLetterWidth(element) {
	var letter = $E('span', $T('m'));
	letter.setStyle({
		fontFamily: element.getStyle('font-family'),
		fontSize: element.getStyle('font-size')
	});
	var parent = $$('body')[0];
	parent.appendChild(letter);
	var width = letter.getWidth();
	parent.removeChild(letter);
	delete letter;
	return width;
}

function refreshInputSize(textarea) {
	var letterWidth = getLetterWidth(textarea);
	var width = textarea.getWidth() - 15;
	var lines = textarea.value.split('\n');
	var lineCount = 0;
	for (var index = 0; index < lines.length; ++index) {
		var line = lines[index];
		lineCount += Math.ceil(1.0 * (line.length + 1) * letterWidth / width);
	}
	textarea.rows = lineCount;
}

function refreshInputSizes() {
	$$('textarea.request').each(function(textarea) {
		refreshInputSize(textarea);
	});
}

function inputChange(event) {
	refreshInputSize(this);
}

function isEmpty(textarea) {
	return textarea.value.strip() == '' && !textarea.submitted;
}

function prepareText(text) {
	if (text == '') {
		text = String.fromCharCode(160);
	}
	return text;
	
	/*
	// Place &shy; between every two characters.
	// Problem: Copy & paste yields weird results!
	var result = '';
	for (var index = 0; index < text.length; ++index) {
		result += text.charAt(index);
		if (index < text.length - 1)
			result += String.fromCharCode(173); // &shy;
	}
	return result;
	*/
}

function getDimensions(math, callback) {
	//var container = $('calc_container');
	var all = $('calc_all').cloneNode(true);
	all.id = null;
	var body = $$('body')[0];
	body.appendChild(all);
	var container = all.select('.calc_container')[0];
	//container.deleteChildNodes();
	//alert("get dim");
	container.appendChild(translateDOMElement(math));
	//alert("append");
	//alert("appended");

	/*var pos = $('calc_container').cumulativeOffset();
	var next = $('calc_next').cumulativeOffset();
	var below = $('calc_below').cumulativeOffset();
	var result = [next.left - pos.left, below.top - pos.top + 10];
	container.deleteChildNodes();
	return result;*/
	
	MathJax.Hub.Queue(["Typeset",MathJax.Hub,container]);
	MathJax.Hub.Queue(function() {
		var pos = container.cumulativeOffset();
		var next = all.select('.calc_next')[0].cumulativeOffset();
		var below = all.select('.calc_below')[0].cumulativeOffset();
		var width = next.left - pos.left + 4;
		var height = below.top - pos.top + 20;
		//var result = [width, height];
		//container.deleteChildNodes();
		//return result;
		//alert(height);
		body.removeChild(all);
		callback(width, height);
		//callback(0, 0);
	});
}

function drawMeshGradient(ctx, points) {
	function color(c, a) {
		var result = 'rgba(' + Math.round(c[0]*255) + ', ' + Math.round(c[1]*255) + ', ' +
			Math.round(c[2]*255) + ', ' + a + ')';
		return result;
	}
	
	var grad1 = ctx.createLinearGradient(0, 0, 0.5, 0.5);
	grad1.addColorStop(0, color(points[0][1], 1));
	grad1.addColorStop(1, color(points[0][1], 0));
	var grad2 = ctx.createLinearGradient(1, 0, 0, 0);
	grad2.addColorStop(0, color(points[1][1], 1));
	grad2.addColorStop(1, color(points[1][1], 0));
	var grad3 = ctx.createLinearGradient(0, 1, 0, 0);
	grad3.addColorStop(0, color(points[2][1], 1));
	grad3.addColorStop(1, color(points[2][1], 0));
	
	ctx.save();
	ctx.setTransform(points[1][0][0]-points[0][0][0], points[1][0][1]-points[0][0][1],
			points[2][0][0]-points[0][0][0], points[2][0][1]-points[0][0][1], points[0][0][0], points[0][0][1]);
	
	ctx.beginPath();
	ctx.moveTo(0, 0);
	ctx.lineTo(1, 0);
	ctx.lineTo(0, 1);
	ctx.closePath();

	ctx.globalCompositeOperation = "lighter";
	ctx.fillStyle = grad1;
	ctx.fill();
	ctx.fillStyle = grad2;
	ctx.fill();
	ctx.fillStyle = grad3;
	ctx.fill();
	ctx.restore();
}

function createMathNode(nodeName) {
	//alert('create ' + nodeName);
	//var node = document.getElementById('prototype_' + nodeName).cloneNode(false);
	//node.removeAttribute('id');
	/*if (nodeName == 'canvas' && MathJax.Hub.Browser.isOpera) {
		var unsupported = document.createElementNS('http://www.w3.org/1999/xhtml', 'div')
		unsupported.appendChild($T('Gradients not supported by Opera'));
		return unsupported;
	}*/
	if (['svg', 'g', 'rect', 'circle', 'polyline', 'polygon', 'path', 'ellipse', 'foreignObject'].include(nodeName))
		return document.createElementNS("http://www.w3.org/2000/svg", nodeName);
	else {
		return document.createElement(nodeName);
	}
	//alert('created');
}

var objectsPrefix = 'math_object_';
var objectsCount = 0;
var objects = {};

function translateDOMElement(element, svg) {
	if (element.nodeType == 3) {
		var text = element.nodeValue;
		return $T(text);
	}
	var nodeName = element.nodeName;
	if (nodeName != 'meshgradient') {
		var dom = createMathNode(element.nodeName);
		for (var i = 0; i < element.attributes.length; ++i) {
			var attr = element.attributes[i];
			if (attr.nodeName != 'ox' && attr.nodeName != 'oy')
				dom.setAttribute(attr.nodeName, attr.nodeValue);
		}
	}
	if (nodeName == 'foreignObject') {
		dom.setAttribute('width', svg.getAttribute('width'));
		dom.setAttribute('height', svg.getAttribute('height'));
		dom.setAttribute('style', dom.getAttribute('style') + '; text-align: left; padding-left: 2px; padding-right: 2px;');
		//var x = parseFloat(element.getAttribute('x'));
		//var y = parseFloat(element.getAttribute('y'));
		var ox = parseFloat(element.getAttribute('ox'));
		var oy = parseFloat(element.getAttribute('oy'));
		dom.setAttribute('ox', ox);
		dom.setAttribute('oy', oy);
		/*var dim = getDimensions(element.childNodes[0]);
		var w = dim[0];
		var h = dim[1];
		x = x - w/2.0 - ox*w/2.0;
		y = y - h/2.0 + oy*h/2.0;
		dom.setAttribute('x', x + 'px');
		dom.setAttribute('y', y + 'px');*/
		/*getDimensions(element.childNodes[0], function(w, h) {
			x = x - w/2.0 - ox*w/2.0;
			y = y - h/2.0 + oy*h/2.0;
			dom.setAttribute('x', x + 'px');
			dom.setAttribute('y', y + 'px');
			//dom.setAttribute('ox', '0px');
			//dom.setAttribute('oy', '0px');
		});*/
	}
	if (nodeName == 'mo') {
		var op = element.childNodes[0].nodeValue;
		if (op == '[' || op == ']' || op == '{' || op == '}' || op == String.fromCharCode(12314) || op == String.fromCharCode(12315))
			dom.setAttribute('maxsize', '3');
		//else if (op == '(' || op == ')')
		//	dom.setAttribute('maxsize', '0');
	}
	if (nodeName == 'meshgradient') {
		if (!MathJax.Hub.Browser.isOpera) {
			var data = element.getAttribute('data').evalJSON();
			var div = document.createElementNS('http://www.w3.org/1999/xhtml', 'div');
			var foreign = document.createElementNS('http://www.w3.org/2000/svg', 'foreignObject');
			foreign.setAttribute('width', svg.getAttribute('width'));
			foreign.setAttribute('height', svg.getAttribute('height'));
			foreign.setAttribute('x', '0px');
			foreign.setAttribute('y', '0px');
			foreign.appendChild(div);
			
			var canvas = createMathNode('canvas');
			canvas.setAttribute('width', svg.getAttribute('width'));
			canvas.setAttribute('height', svg.getAttribute('height'));
			div.appendChild(canvas);
			
			//alert("get context");
			//if (canvas.getContext) {
				//alert("get");
			var ctx = canvas.getContext('2d');
			//alert("draw");
			for (var index = 0; index < data.length; ++index) {
				var points = data[index];
				if (points.length == 3) {
					drawMeshGradient(ctx, points);
				}
			}
			//}
			//alert("drawn");
			
			dom = foreign;
		}
	}
	var object = null;
	if (nodeName == 'svg') {
		object = createMathNode('mspace');
		//dom.appendChild(document.createTextNode('SVG'));
		//object.id = 'svgnode';
		//dom.width = 200;
		//dom.style.width = '200px';
		/*dom.width = '200px';
		dom.height = '200px';
		dom.width = 200;
		dom.height = 200;*/
		//alert(dom.getAttribute('width'));
		object.setAttribute('width', dom.getAttribute('width') + 'px');
		object.setAttribute('height', dom.getAttribute('height') + 'px');
		svg = dom;
		//return dom;
	}
	var rows = [[]];
	$A(element.childNodes).each(function(child) {
		//dom.appendChild(translateDOMElement(child, svg));
		if (child.nodeName == 'mspace' && child.getAttribute('linebreak') == 'newline')
			rows.push([]);
		else
			rows[rows.length - 1].push(child);
	});
	var childParent = dom;
	if (nodeName == 'math') {
		var mstyle = createMathNode('mstyle');
		mstyle.setAttribute('displaystyle', 'true');
		dom.appendChild(mstyle);
		childParent = mstyle;
	}
	if (rows.length > 1) {
		var mtable = createMathNode('mtable');
		mtable.setAttribute('rowspacing', '0');
		mtable.setAttribute('columnalign', 'left');
		var nospace = 'cell-spacing: 0; cell-padding: 0; row-spacing: 0; row-padding: 0; border-spacing: 0; padding: 0; margin: 0';
		mtable.setAttribute('style', nospace);
		rows.each(function(row) {
			var mtr = createMathNode('mtr');
			mtr.setAttribute('style', nospace);
			var mtd = createMathNode('mtd');
			row.each(function(element) {
				mtd.setAttribute('style', nospace);
				mtd.appendChild(translateDOMElement(element, svg));
			});
			mtd.appendChild($T('\n'));
			mtr.appendChild(mtd);
			mtable.appendChild(mtr);
		});
		childParent.appendChild(mtable);
	} else
		rows[0].each(function(element) {
			childParent.appendChild(translateDOMElement(element, svg));
		});
	if (object) {
		//object.object = dom;
		var id = objectsCount++;
		object.setAttribute('id', objectsPrefix + id);
		//objects.push(dom);
		objects[id] = dom;
		return object;
	}
	return dom;
}

function createLine(value) {
	if (value.startsWith('<math')) {
		//alert('stringToDOM');
		//var dom = stringToDOM(value);
		var dom = document.createElement('div');
		//alert('created dom');
		// updating with &nbsp; in the value does not work 
		//value = value.gsub(/&nbsp;/, " ");
		//alert(value);
		dom.updateDOM(value);
		//alert('updated dom');
		
		var result = translateDOMElement(dom.childNodes[0]);
		//alert('translated');
		return result;	
	} else {
		var lines = value.split('\n');
		var p = $E('p');
		for (var index = 0; index < lines.length; ++index) {
			p.appendChild($T(prepareText(lines[index])));
			if (index < lines.length - 1)
				p.appendChild($E('br'));
		}
		return p;
	}
}

function setResult(ul, results) {
	//alert('set result');
	results.each(function(result) {
		var resultUl = $E('ul', {'class': 'out'});
		result.out.each(function(out) {
			var li = $E('li', {'class': (out.message ? 'message' : 'print')});
			if (out.message)
				li.appendChild($T(out.prefix + ': '));
			//alert(out.text);
			li.appendChild(createLine(out.text));
			//alert(li);
			resultUl.appendChild(li);
		});
		//alert('out created');
		if (result.result != null) {
			var li = $E('li', {'class': 'result'}, createLine(result.result));
			resultUl.appendChild(li);
		}
		//alert('result created');
		ul.appendChild($E('li', {'class': 'out'}, resultUl));
	});
	//MathJax.Hub.Typeset(ul);
	//alert('typeset');
	MathJax.Hub.Queue(["Typeset", MathJax.Hub, ul]);
	//$('svgnode').appendChild($T('text'));
	//$('svgnode').style.width = '200px';
	//alert('call next queue');
	MathJax.Hub.Queue(function() {
		ul.select('.mspace').each(function(mspace) {
			//alert(mspace.object);
			var id = mspace.getAttribute('id').substr(objectsPrefix.length);
			//alert(id);
			var object = objects[id];
			//alert(object);
			mspace.appendChild(object);
			//MathJax.Hub.Queue(["Typeset", MathJax.Hub, object]);
			//ul.appendChild($E('li', {}, object));
			objects[id] = null;
		});
		//alert('converted mspaces');
		//alert('converted labels etc');
		/*x = x - w/2.0 - ox*w/2.0;
		y = y - h/2.0 + oy*h/2.0;
		dom.setAttribute('x', x + 'px');
		dom.setAttribute('y', y + 'px');*/
		//$$('.')
	});
	//alert('next queue');
	if (!MathJax.Hub.Browser.isOpera) {
		// Opera 11.01 Build 1190 on Mac OS X 10.5.8 crashes on this call for Plot[x,{x,0,1}]
		// => leave inner MathML untouched
		MathJax.Hub.Queue(["Typeset", MathJax.Hub, ul]);
	}
	MathJax.Hub.Queue(function() {
		ul.select('foreignObject >span >nobr >span.math').each(function(math) {
			//math.style.fontSize = '100%';
			var content = math.childNodes[0].childNodes[0].childNodes[0];
			math.removeChild(math.childNodes[0]);
			math.insertBefore(content, math.childNodes[0]);

			var foreignObject = math.parentNode.parentNode.parentNode;
			var dimensions = math.getDimensions();
			var w = dimensions.width + 4;
			var h = dimensions.height + 4;
			//alert(w + ' x ' + h);
			var x = parseFloat(foreignObject.getAttribute('x').substr());
			var y = parseFloat(foreignObject.getAttribute('y'));
			var ox = parseFloat(foreignObject.getAttribute('ox'));
			var oy = parseFloat(foreignObject.getAttribute('oy'));
			x = x - w/2.0 - ox*w/2.0;
			y = y - h/2.0 + oy*h/2.0;
			//alert(w + ' x ' + h + ' -> ' + x + ', ' + y);
			foreignObject.setAttribute('x', x + 'px');
			foreignObject.setAttribute('y', y + 'px');
		});
	});
	//objects = [];
}

function submitQuery(textarea, onfinish) {
	//alert("submit");
	$('welcomeContainer').fade({duration: 0.5});
	
	//alert("submit " + textarea.value);
	
	textarea.li.addClassName('loading');
	new Ajax.Request('/ajax/query/', {
		method: 'post',
		parameters: {
			query: textarea.value
		},
		onSuccess: function(transport) {
			//alert("response: " + transport.responseText);
			textarea.ul.select('li[class!=request][class!=submitbutton]').invoke('deleteElement');
			if (!transport.responseText) {
				// A fatal Python error has occured, e.g. on 4.4329408320439^43214234345
				// ("Fatal Python error: mp_reallocate failure")
				// -> print overflow message
				//transport.responseText = ('{result: "Overflow[]", out: [{message: true, symbol: "General", tag: "ovfl", text: "Overflow occured in computation"}]}');
				//transport.responseText = ('{results: [{"result": "Overflow[]", out: [{message: true, prefix: "General::ovfl", symbol: "General", tag: "ovfl", text: "Overflow occured in computation"}]}]}');
				transport.responseText = '{"results": [{"out": [{"prefix": "General::noserver", "message": true, "tag": "noserver", "symbol": "General", "text": "<math><mrow><mtext>No server running.</mtext></mrow></math>"}]}]}';
			}
			var response = transport.responseText.evalJSON();
			setResult(textarea.ul, response.results);
			//alert("result set");
			textarea.submitted = true;
			textarea.results = response.results;
			var next = textarea.li.nextSibling;
			if (next)
				next.textarea.focus();
			else
				createQuery();
			//alert("success");
		},
		onFailure: function(transport) {
			textarea.ul.select('li[class!=request]').invoke('deleteElement');
			var li = $E('li', {'class': 'serverError'}, $T("Sorry, an error occured while processing your request!"));
			textarea.ul.appendChild(li);					
			textarea.submitted = true;					
		},
		onComplete: function() {
			textarea.li.removeClassName('loading');
			if (onfinish)
				onfinish();
		}
	});
}

function getSelection() {
	// TODO
}

function keyDown(event) {
	//var textarea = this;
	var textarea = lastFocus;
	if (!textarea)
		return;
	refreshInputSize(textarea);
	
	if (event.keyCode == Event.KEY_RETURN && (event.shiftKey || event.keyLocation == 3)) {
		if (!Prototype.Browser.IE)
			event.stop();
		
		var query = textarea.value.strip();
		if (query) {
			submitQuery(textarea);
		}
	} else if (event.keyCode == Event.KEY_UP) {
		if (textarea.selectionStart == 0 && textarea.selectionEnd == 0) {
			if (isEmpty(textarea)) {
				if (textarea.li.previousSibling)
					textarea.li.previousSibling.textarea.focus();
			} else
				createQuery(textarea.li);
		}
	} else if (event.keyCode == Event.KEY_DOWN) {
		if (textarea.selectionStart == textarea.value.length && textarea.selectionEnd == textarea.selectionStart) {
			if (isEmpty(textarea)) {
				if (textarea.li.nextSibling)
					textarea.li.nextSibling.textarea.focus();
			} else
				createQuery(textarea.li.nextSibling);
		}
	} else
		if (isGlobalKey(event))
			event.stop();
		
		//globalKeyUp(event);
		//globalKeyUp.bindAsEventListener(textarea)(event);
		// to prevent Ctrl+? to have a "meaning" in the textarea
}

function deleteMouseDown(event) {
	if (event.isLeftClick())
		deleting = true;
}

function deleteClick(event) {
	if (lastFocus == this.li.textarea)
		lastFocus = null;
	this.li.deleteElement();
	deleting = false;
	if (blurredElement) {
		blurredElement.focus();
		blurredElement = null;
	}
	if ($('queries').childElements().length == 0)
		createQuery();
}

function moveMouseDown(event) {
	movedItem = this.li;
	movedItem.addClassName('moving');
}

function moveMouseUp(event) {
	if (movedItem) {
		movedItem.removeClassName('moving');
		movedItem.textarea.focus();
		movedItem = null;
	}
}

function onFocus(event) {
	var textarea = this;
	textarea.li.addClassName('focused');
	lastFocus = textarea;
}

function onBlur(event) {
	var textarea = this;
	blurredElement = textarea;
	if (!deleting && textarea.li != movedItem && isEmpty(textarea) && $('queries').childElements().length > 1) {
		// Delay deletion of element, otherwise new element won't get focus.
		//textarea.li.deleteElement();
		textarea.li.hide();
		if (textarea == lastFocus)
			lastFocus = null;
		window.setTimeout(function() {
			textarea.li.deleteElement();
		}, 10);
	}
	textarea.li.removeClassName('focused');
}

function createSortable() {
	//alert("create sortable");
	Position.includeScrollOffsets = true;
	//alert("create");
  Sortable.create('queries', {
    handle: 'move',
    scroll: 'document',
    scrollSensitivity: 1	// otherwise strange flying-away of item at top
    /*onChange: function(item) {
	    movedItem = item;
	  },

	  onUpdate: function() {
	  	movedItem = null;
	  }*/
  });
  //alert("created");
}

var queryIndex = 0;

function createQuery(before, noFocus, updatingAll) {
	var ul, textarea, moveHandle, deleteHandle, submitButton;
	// Items need id in order for Sortable.onUpdate to work.
	var li = $E('li', {'id': 'query_' + queryIndex++, 'class': 'query'},
		ul = $E('ul', {'class': 'query'},
			$E('li', {'class': 'request'},
				textarea = $E('textarea', {'class': 'request'}),
				$E('span', {'class': 'submitbutton', 'title': "Submit [Shift+Return]"},
					submitButton = $E('span', $T('='))
				)
			)
		),
		moveHandle = $E('span', {'class': 'move'}),
		deleteHandle = $E('span', {'class': 'delete', 'title': "Delete"}, $T(String.fromCharCode(215)))
	);
	textarea.rows = 1;
	textarea.ul = ul;
	textarea.li = li;
	textarea.submitted = false;
	moveHandle.li = li;
	deleteHandle.li = li;
	li.textarea = textarea;
	li.ul = ul;
	if (before)
		$('queries').insertBefore(li, before);
	else
		$('queries').appendChild(li);	
	if (!updatingAll)
		refreshInputSize(textarea);
	new Form.Element.Observer(textarea, 0.2, inputChange.bindAsEventListener(textarea));
	//textarea.observe('keydown', keyDown.bindAsEventListener(textarea));
	textarea.observe('focus', onFocus.bindAsEventListener(textarea));
	textarea.observe('blur', onBlur.bindAsEventListener(textarea));
	li.observe('mousedown', queryMouseDown.bindAsEventListener(li));
	deleteHandle.observe('click', deleteClick.bindAsEventListener(deleteHandle));
	deleteHandle.observe('mousedown', deleteMouseDown.bindAsEventListener(deleteHandle));
	moveHandle.observe('mousedown', moveMouseDown.bindAsEventListener(moveHandle));
	moveHandle.observe('mouseup', moveMouseUp.bindAsEventListener(moveHandle));
	$(document).observe('mouseup', moveMouseUp.bindAsEventListener($(document)));
	submitButton.observe('mousedown', function() {
		//alert('click');
		if (textarea.value.strip())
			submitQuery(textarea);
		else
			window.setTimeout(function() {
				//alert("focus");
				textarea.focus();
				//alert("focussed");
			}, 10);
	});
	//submitButton.observe('mousedown', queryMouseDown.bindAsEventListener(li));
	if (!updatingAll) {
		createSortable();
		// calling directly fails in Safari on document loading
		//window.setTimeout(createSortable, 10);
	}
	//textarea.focus();
	//alert("focus");
	// Immediately setting focus doesn't work in IE.
	if (!noFocus)
		window.setTimeout(function() {
			//alert("focus");
			textarea.focus();
			//alert("focussed");
		}, 10);
	return li;
}

var mouseDownEvent = null;

function documentMouseDown(event) {
	//alert('click');
	if (event.isLeftClick()) {
		if (clickedQuery) {
			clickedQuery = null;
			mouseDownEvent = null;
			return;
		}
		event.stop(); // strangely, doesn't work otherwise
		mouseDownEvent = event;
	}
}

function documentClick(event) {
	// In Firefox, mousedown also fires when user clicks scrollbars.
	// -> listen to click
	event = mouseDownEvent;
	if (!event)
		return;
	if ($('queries').childElements().length == 1 && isEmpty($('queries').childElements()[0].textarea)) {
		$('queries').childElements()[0].textarea.focus();
		return;
	}
	var offset = $('document').cumulativeOffset();
	var y = event.pointerY() - offset.top + $('document').scrollTop;
	var element = null;
	$('queries').childElements().each(function(li) {
		var offset = li.positionedOffset(); // margin-top: 10px
		if (offset.top + 20 > y) {
			element = li;
			throw $break;
		}
	});
	createQuery(element);
}

function queryMouseDown(event) {
	// Don't propagate to documentMouseDown
	//event.stop();
	clickedQuery = this;
}

function focusLast() {
	if (lastFocus)
		lastFocus.focus();
	else
		createQuery();
}

function isGlobalKey(event) {
	if (event.ctrlKey) {
		switch(event.keyCode) {
		case 68:
		case 67:
		case 83:
		case 79:
			return true;
		}
	}
	return false;
}

function globalKeyUp(event) {
	//alert(event.keyCode);
	// F1: 112
	if (!popup && event.ctrlKey) {
		switch (event.keyCode) {
		case 68: // D
			$('search').select();
			event.stop();
			break;
		case 67: // C
			focusLast();
			event.stop();
			break;
		case 83: // S
			showSave();
			break;
		case 79: // O
			showOpen();
			break;
		}
	}
}

function domLoaded() {
	MathJax.Hub.Config({
		//delayJaxRegistration: true,
		"HTML-CSS": {
			imageFont: null,
	  	showMathMenu: false
	  },
	  MMLorHTML: {
	    //
	    //  The output jax that is to be preferred when both are possible
	    //  (set to "MML" for native MathML, "HTML" for MathJax's HTML-CSS output jax).
	    //
	    prefer: {
	      MSIE:    "HTML",
	      Firefox: "HTML",
	      Opera:   "HTML",
	      //Safari:  "HTML",
	      other:   "HTML"
	    }
	  }
	});
	MathJax.Hub.Configured();
	
	if ($('welcomeBrowser'))
		if (!(Prototype.Browser.WebKit || Prototype.Browser.MobileSafari || Prototype.Browser.Gecko))
			$('welcomeBrowser').show();
	
	$$('body')[0].observe('resize', refreshInputSizes);
	
	if ($('queriesContainer')) {
		$('queriesContainer').appendChild($E('ul', {'id': 'queries'}));
		
		$('document').observe('mousedown', documentMouseDown.bindAsEventListener($('document')));
		$('document').observe('click', documentClick.bindAsEventListener($('document')));
		
		$(document).observe('keydown', keyDown.bindAsEventListener());
		if (Prototype.Browser.IE) {
			document.body.addEventListener('keydown', function(event) {
			//$(document).observe('keydown', function(event) {
				if (event.keyCode == Event.KEY_RETURN && event.shiftKey) {
					event.stopPropagation();
					//event.cancelBubble = true;
					event.preventDefault();
					keyDown(event);
					//alert("stop");
					//return false;
				}
			}, true);
		}
		if (Prototype.Browser.Opera || Prototype.Browser.IE) {
			/*document.body.addEventListener('keypress', function(event) {
				if (event.keyCode == Event.KEY_RETURN && event.shiftKey) {
					event.stopPropagation();
					//event.cancelBubble = true;
					event.preventDefault();
					//alert("stop");
					return false;
				}
			}, true);*/
			// Opera needs another hook so it doesn't insert newlines after Shift+Return
			$(document).observe('keypress', function(event) {
				if (event.keyCode == Event.KEY_RETURN && event.shiftKey)
					event.stop();
			}.bindAsEventListener());
		}
		
		$(document).observe('keyup', globalKeyUp.bindAsEventListener($('document')));
		
		if (!loadLink())
			createQuery();
		
		/*window.setTimeout(function() {
			$$('textarea')[0].focus();
			alert("focussed");
		}, 1000);*/
	}
}

$(document).observe('dom:loaded', domLoaded);
// Konqueror won't fire dom:loaded, so we still need body.onload.

//$(document).observe('resize', refreshInputSizes);
window.onresize = refreshInputSizes;
