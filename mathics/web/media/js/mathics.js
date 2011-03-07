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

function getDimensions(math) {
	var container = $('calc_container');
	container.deleteChildNodes();
	container.appendChild(translateDOMElement(math));
	var pos = $('calc_container').cumulativeOffset();
	var next = $('calc_next').cumulativeOffset();
	var below = $('calc_below').cumulativeOffset();
	var result = [next.left - pos.left, below.top - pos.top + 10];
	container.deleteChildNodes();
	return result;
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
	var node = document.getElementById('prototype_' + nodeName).cloneNode(false);
	node.removeAttribute('id');
	return node;
}

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
		dom.setAttribute('style', dom.getAttribute('style') + '; text-align: left');
		var x = parseFloat(element.getAttribute('x'));
		var y = parseFloat(element.getAttribute('y'));
		var ox = parseFloat(element.getAttribute('ox'));
		var oy = parseFloat(element.getAttribute('oy'));
		var dim = getDimensions(element.childNodes[0]);
		var w = dim[0];
		var h = dim[1];
		x = x - w/2.0 - ox*w/2.0;
		y = y - h/2.0 + oy*h/2.0;
		dom.setAttribute('x', x + 'px');
		dom.setAttribute('y', y + 'px');
	}
	if (nodeName == 'mo') {
		var op = element.childNodes[0].nodeValue;
		if (op == '[' || op == ']' || op == '{' || op == '}' || op == String.fromCharCode(12314) || op == String.fromCharCode(12315))
			dom.setAttribute('maxsize', '3');
		else if (op == '(' || op == ')')
			dom.setAttribute('maxsize', '0');
	}
	if (nodeName == 'meshgradient') {
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
		var ctx = canvas.getContext('2d');
		
		for (var index = 0; index < data.length; ++index) {
			var points = data[index];
			if (points.length == 3) {
				drawMeshGradient(ctx, points);
			}
		}
		
		dom = foreign;
	}
	if (nodeName == 'svg')
		svg = dom;
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
	return dom;
}

function createLine(value) {
	if (value.startsWith('<math')) {
		var dom = stringToDOM(value);
		
		var result = translateDOMElement(dom.childNodes[0]);
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
	results.each(function(result) {
		var resultUl = $E('ul', {'class': 'out'});
		result.out.each(function(out) {
			var li = $E('li', {'class': out.message ? 'message': 'print'});
			if (out.message)
				li.appendChild($T(out.prefix + ': '));
			li.appendChild(createLine(out.text));
			resultUl.appendChild(li);
		});
		if (result.result != null) {
			var li = $E('li', {'class': 'result'}, createLine(result.result));
			resultUl.appendChild(li);
		}
		ul.appendChild($E('li', {'class': 'out'}, resultUl));
	});
}

function submitQuery(textarea, query, onfinish) {
	$('welcome').fade({duration: 0.5});
	
	textarea.li.addClassName('loading');
	new Ajax.Request('/ajax/query/', {
		method: 'post',
		parameters: {
			query: textarea.value
		},
		onSuccess: function(transport) {
			textarea.ul.select('li[class!=request]').invoke('deleteElement');
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
			textarea.submitted = true;
			var next = textarea.li.nextSibling;
			if (next)
				next.textarea.focus();
			else
				createQuery();
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
	var textarea = this;
	refreshInputSize(textarea);
	
	if (event.keyCode == Event.KEY_RETURN && event.shiftKey) {
		event.stop();
		
		var query = textarea.value.strip();
		if (query) {
			submitQuery(textarea, query);
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
	Position.includeScrollOffsets = true;
	
  Sortable.create('queries', {
    handle: 'move',
    scroll: 'document',
    scrollSensitivity: 1	// otherwise strange flying-away of item at top
    /*onChange: function(item) {
	    //movedItem = item;
	  },

	  onUpdate: function() {
	  	//movedItem = null;
	  }*/
  });
}

function createQuery(before, noFocus, updatingAll) {
	var ul, textarea, moveHandle, deleteHandle;
	// Items need id in order for Sortable.onUpdate to work.
	var li = $E('li', {/*'id': 'query_' + queryIndex++,*/ 'class': 'query'},
		ul = $E('ul', {'class': 'query'},
			$E('li', {'class': 'request'},
				textarea = $E('textarea', {'class': 'request'})
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
	textarea.observe('keydown', keyDown.bindAsEventListener(textarea));
	textarea.observe('focus', onFocus.bindAsEventListener(textarea));
	textarea.observe('blur', onBlur.bindAsEventListener(textarea));
	li.observe('mousedown', queryMouseDown.bindAsEventListener(li));
	deleteHandle.observe('click', deleteClick.bindAsEventListener(deleteHandle));
	deleteHandle.observe('mousedown', deleteMouseDown.bindAsEventListener(deleteHandle));
	moveHandle.observe('mousedown', moveMouseDown.bindAsEventListener(moveHandle));
	moveHandle.observe('mouseup', moveMouseUp.bindAsEventListener(moveHandle));
	$(document).observe('mouseup', moveMouseUp.bindAsEventListener($(document)));
	if (!updatingAll)
		createSortable();
	//textarea.focus();
	// Immediately setting focus doesn't work in IE.
	if (!noFocus)
		window.setTimeout(function() {
			textarea.focus();
		}, 10);
	return li;
}

var mouseDownEvent = null;

function documentMouseDown(event) {
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
	$$('body')[0].observe('resize', refreshInputSizes);
	
	if ($('queriesContainer')) {
		$('queriesContainer').appendChild($E('ul', {'id': 'queries'}));
		
		$('document').observe('mousedown', documentMouseDown.bindAsEventListener($('document')));
		$('document').observe('click', documentClick.bindAsEventListener($('document')));
		
		$(document).observe('keyup', globalKeyUp.bindAsEventListener($('document')));
		
		if (!loadLink())
			createQuery();
	}
}

$(document).observe('dom:loaded', domLoaded);
// Konqueror won't fire dom:loaded, so we still need body.onload.

//$(document).observe('resize', refreshInputSizes);
window.onresize = refreshInputSizes;
