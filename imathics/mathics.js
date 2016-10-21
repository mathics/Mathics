/*
IMPORTANT

in order for mathjax hub queue to work, Jupyter must _not_ set its web font to "STIX-Web"

                "HTML-CSS": {
                    availableFonts: [],
                    imageFont: null,
                    preferredFont: null,
                    webFont: null,
                    styles: {'.MathJax_Display': {"margin": 0}},
                    linebreaks: { automatic: true }
                },

*/


function $A(iterable) {
  if (!iterable) return [];
  if (iterable.toArray) return iterable.toArray();
  var length = iterable.length || 0, results = new Array(length);
  while (length--) results[length] = iterable[length];
  return results;
}

var HTML_ENTITIES = {
	'amp': 38,
	'gt': 62,
	'lt': 60,
	'quot': 34,
	'nbsp': 160,
	'ndash': 8211,
	'mdash': 8212,
	'euro': 8364,
	'copy': 169,
	'trade': 8482,
	'hellip': 8230,
	'ldquo': 8220,
	'rdquo': 8221,
	'bdquo': 8222,
	'reg': 174,
	'larr': 8592,
	'rarr': 8594
};

function $E(tag, properties) {
	var children;
	var isDOMElement = function(object) {
		return object && !!object.nodeType;
	};
	if (isDOMElement(properties)) {
		children = $A(arguments).slice(1);
		properties = {};
	} else
		children = $A(arguments).slice(2);
	if (tag == 'a' && properties.href == null)
		properties.href = 'javascript:;';
	var element = document.createElement(tag);

	for (var key in properties) {
		if (properties.hasOwnProperty(key)) {
			element.setAttribute(key, properties[key]);
		}
	}

	children.forEach(function(child) {
		if (child)
			element.appendChild(child);
	});
	return element;
}

function $T(text) {
	return document.createTextNode(text);
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
	var all = $('calc_all').cloneNode(true);
	all.id = null;
	var body = $$('body')[0];
	body.appendChild(all);
	var container = all.select('.calc_container')[0];
	container.appendChild(translateDOMElement(math));

	MathJax.Hub.Queue(["Typeset", MathJax.Hub, container]);
	MathJax.Hub.Queue(function() {
		var pos = container.cumulativeOffset();
		var next = all.select('.calc_next')[0].cumulativeOffset();
		var below = all.select('.calc_below')[0].cumulativeOffset();
		var width = next.left - pos.left + 4;
		var height = below.top - pos.top + 20;
		body.removeChild(all);
		callback(width, height);
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
	if (['svg', 'g', 'rect', 'circle', 'polyline', 'polygon', 'path', 'ellipse', 'foreignObject'].indexOf(nodeName) != -1)
		return document.createElementNS("http://www.w3.org/2000/svg", nodeName);
	else {
		return document.createElement(nodeName);
	}
}

function debug(s) {
    // alert(s);
}

var mathicsIdName = 'mathics_id';
var objectsCount = 0;
var objects = {};

function translateDOMElement(element, svg) {
	if (element.nodeType == 3) {
		var text = element.nodeValue;
		return $T(text);
	}
	var dom = null;
	var nodeName = element.nodeName.toLowerCase();
	if (nodeName == 'mtext' && element.childNodes[0].nodeName.toLowerCase() == 'img') {
	    debug('detected special case <mtext><img></mtext>');
	    return translateDOMElement(element.childNodes[0]);  // remove <mtext> here
	}

	if (nodeName != 'meshgradient' && nodeName != 'graphics3d') {
		dom = createMathNode(element.nodeName);
		for (var i = 0; i < element.attributes.length; ++i) {
			var attr = element.attributes[i];
			if (attr.nodeName != 'ox' && attr.nodeName != 'oy')
				dom.setAttribute(attr.nodeName, attr.nodeValue);
		}
	}
	if (nodeName == 'foreignobject') {
		dom.setAttribute('width', svg.getAttribute('width'));
		dom.setAttribute('height', svg.getAttribute('height'));
		dom.setAttribute('style', dom.getAttribute('style') + '; text-align: left; padding-left: 2px; padding-right: 2px;');
		var ox = parseFloat(element.getAttribute('ox'));
		var oy = parseFloat(element.getAttribute('oy'));
		dom.setAttribute('ox', ox);
		dom.setAttribute('oy', oy);
	}
	if (nodeName == 'mo') {
		var op = element.childNodes[0].nodeValue;
		if (op == '[' || op == ']' || op == '{' || op == '}' || op == String.fromCharCode(12314) || op == String.fromCharCode(12315))
			dom.setAttribute('maxsize', '3');
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

			var ctx = canvas.getContext('2d');
			for (var index = 0; index < data.length; ++index) {
				var points = data[index];
				if (points.length == 3) {
					drawMeshGradient(ctx, points);
				}
			}

			dom = foreign;
		}
	}
	var object = null;
	if (nodeName == 'graphics3d') {
		var data = element.getAttribute('data').evalJSON();
		var div = document.createElement('div');
		drawGraphics3D(div, data);
		dom = div;
	}
	if (nodeName == 'svg' || nodeName == 'graphics3d' || nodeName == 'img') {
		// create <mspace> that will contain the graphics
		debug('looking at node ' + nodeName);
		object = createMathNode('mspace');
		var width, height;
		if (nodeName == 'svg' || nodeName == 'img') {
			width = dom.getAttribute('width');
			height = dom.getAttribute('height');
			debug('found ' + nodeName + ': ' + width + '/' + height);
		} else {
			// TODO: calculate appropriate height and recalculate on every view change
			width = height = '400';
		}
		object.setAttribute('width', width  + 'px');
		object.setAttribute('height', height + 'px');
	}
	if (nodeName == 'svg')
		svg = dom;
	var rows = [[]];
	$A(element.childNodes).forEach(function(child) {
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
		rows.forEach(function(row) {
			var mtr = createMathNode('mtr');
			mtr.setAttribute('style', nospace);
			var mtd = createMathNode('mtd');
			mtd.setAttribute('style', nospace);
			row.forEach(function(element) {
				var elmt = translateDOMElement(element, svg);
				if (nodeName == 'mtext') {
					// wrap element in mtext
					var outer = createMathNode('mtext');
					outer.appendChild(elmt);
					elmt = outer;
				}
				mtd.appendChild(elmt);
			});
			mtr.appendChild(mtd);
			mtable.appendChild(mtr);
		});
		if (nodeName == 'mtext') {
			// no mtable inside mtext, but mtable instead of mtext
			dom = mtable;
		} else
			childParent.appendChild(mtable);
	} else
		rows[0].forEach(function(element) {
			childParent.appendChild(translateDOMElement(element, svg));
		});
	if (object) {
		var id = objectsCount++;
		// debug('adding object to id ' + id);
		object.setAttribute(mathicsIdName, id);
		objects[id] = dom;
		debug('setting object ' + id + ' to ' + dom + '/' + JSON.stringify(objects));
		return object;
	}
	return dom;
}

function createLine(value) {
	if (value.startsWith('<math')) {
		//value = '<math><mspace width="350.000000px" height="350.000000px" id="math_object_0"></mspace></math>';
		// value = '<math><mtable><mtr><mtd><mn>1</mn></mtd></mtr></mtable></math>';

		var dom = document.createElement('div');

        var updateDOM = function(element, content) {
            // convert named entities to numerical entities before calling update
            content = content.replace(/&([a-zA-Z]+);/g, function(match, contents, offset, s) {
                var code = HTML_ENTITIES[contents];
                return "&#" + code + ";";
            });
            element.innerHTML = content;
            // updateElement(element, content);
        };

		updateDOM(dom, value);

		var el = translateDOMElement(dom.childNodes[0]);

		var container = document.createElement('div');
		container.appendChild(el);
		container.setAttribute("style", "display:none;");

		debug('mathjax hub typeset: ' + container.innerHTML);

    	MathJax.Hub.Queue(['Typeset', MathJax.Hub, container]);

    	afterProcessResult(container);
		//MathJax.Hub.Register.StartupHook("End",function () {
		//});

		return container;
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

function afterProcessResult(container, command) {
	// command is either 'Typeset' (default) or 'Rerender'
	if (!command)
		command = 'Typeset';

    function relayout2() {
		Array.prototype.forEach.call(container.querySelectorAll('foreignObject >span >nobr >span.math'), function(math) {
			var content = math.childNodes[0].childNodes[0].childNodes[0];
			math.removeChild(math.childNodes[0]);
			math.insertBefore(content, math.childNodes[0]);

			if (command == 'Typeset') {
				// recalculate positions of insets based on ox/oy properties
				var foreignObject = math.parentNode.parentNode.parentNode;
				var dimensions = math.getDimensions();
				var w = dimensions.width + 4;
				var h = dimensions.height + 4;
				var x = parseFloat(foreignObject.getAttribute('x').substr());
				var y = parseFloat(foreignObject.getAttribute('y'));
				var ox = parseFloat(foreignObject.getAttribute('ox'));
				var oy = parseFloat(foreignObject.getAttribute('oy'));
				x = x - w/2.0 - ox*w/2.0;
				y = y - h/2.0 + oy*h/2.0;
				foreignObject.setAttribute('x', x + 'px');
				foreignObject.setAttribute('y', y + 'px');
			}
		});
	}

    var state = {'retries': 0};

	function relayout() {
		// inject SVG and other non-MathML objects into corresponding <mspace>s

        if (container.querySelector('.MathJax_Error')) {
            debug('mathjax hub: MathJax_Error error found');
            // we're too early, try again later
            if (++state.retries < 2) {
                MathJax.Hub.Queue(relayout);
            } else {
                container.setAttribute('style', ''); // display
            }
        } else {
            var selector = 'script[type="math/mml"]';

            debug('mathjax hub callback: ' + container.querySelectorAll(selector).length + '/' + container.innerHTML);

            /* example:
            <script type="math/mml" id="MathJax-Element-2"><math><mstyle displaystyle="true">
            <mtable><mtr><mtd><mspace width="350.000000px" height="350.000000px" mathics_id="0"></mspace>
            */

            /*Array.prototype.forEach.call(container.querySelectorAll(selector), function(script) {
                var el = document.createElement('html');
                el.innerHTML = script.innerHTML;

                Array.prototype.forEach.call(el.querySelectorAll('mspace'), function(mspace) {
                    var id = mspace.getAttribute(mathicsIdName);
                    var object = objects[parseInt(id)];
                    debug('mathjax hub callback for ' + id + " with object " + JSON.stringify(object) + " / " + JSON.stringify(objects));
                    if (object) {
                        mspace.appendChild(object);
                    }
                });


                //script.innerHTML = '<math><mtext>12345</mtext></math>'; // el.innerHTML;
            });*/

            relayout2();

            container.setAttribute('style', ''); // display
        }
	}

	MathJax.Hub.Queue(relayout);
}
