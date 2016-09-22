var deleting;
var blurredElement;

var movedItem;

var clickedQuery;

var lastFocus = null;

var welcome = true;

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

  $$('#queries ul').each(function(ul) {
    afterProcessResult(ul, 'Rerender');
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
  if (['svg', 'g', 'rect', 'circle', 'polyline', 'polygon', 'path', 'ellipse', 'foreignObject'].include(nodeName))
    return document.createElementNS("http://www.w3.org/2000/svg", nodeName);
  else {
    return document.createElement(nodeName);
  }
}

var objectsPrefix = 'math_object_';
var objectsCount = 0;
var objects = {};

function translateDOMElement(element, svg) {
  if (element.nodeType == 3) {
    var text = element.nodeValue;
    return $T(text);
  }
  var dom = null;
  var nodeName = element.nodeName;
  if (nodeName != 'meshgradient' && nodeName != 'graphics3d') {
    dom = createMathNode(element.nodeName);
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
  if (nodeName == 'svg' || nodeName == 'graphics3d' || nodeName.toLowerCase() == 'img') {
    // create <mspace> that will contain the graphics
    object = createMathNode('mspace');
    var width, height;
    if (nodeName == 'svg' || nodeName.toLowerCase() == 'img') {
      width = dom.getAttribute('width');
      height = dom.getAttribute('height');
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
  $A(element.childNodes).each(function(child) {
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
      mtd.setAttribute('style', nospace);
      row.each(function(element) {
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
    rows[0].each(function(element) {
      childParent.appendChild(translateDOMElement(element, svg));
    });
  if (object) {
    var id = objectsCount++;
    object.setAttribute('id', objectsPrefix + id);
    objects[id] = dom;
    return object;
  }
  return dom;
}

function convertMathGlyphs(dom) {
    // convert mglyphs to their classic representation (<svg> or <img>), so the new mglyph logic does not make
    // anything worse in the classic Mathics frontend for now. In the long run, this code should vanish.

    var MML = "http://www.w3.org/1998/Math/MathML";
    var glyphs = dom.getElementsByTagName("mglyph");
    for (var i = 0; i < glyphs.length; i++) {
        var glyph = glyphs[i];
        var src = glyph.getAttribute('src');
        if (src.startsWith('data:image/svg+xml;base64,')) {
            var svgText = atob(src.substring(src.indexOf(",") + 1));
            var mtable =document.createElementNS(MML, "mtable");
            mtable.innerHTML = '<mtr><mtd>' + svgText + '</mtd></mtr>';
            var svg = mtable.getElementsByTagNameNS("*", "svg")[0];
            svg.setAttribute('width', glyph.getAttribute('width'));
            svg.setAttribute('height', glyph.getAttribute('height'));
            glyph.parentNode.replaceChild(mtable, glyph);
        } else if (src.startsWith('data:image/')) {
            var img = document.createElement('img');
            img.setAttribute('src', src)
            img.setAttribute('width', glyph.getAttribute('width'));
            img.setAttribute('height', glyph.getAttribute('height'));
            glyph.parentNode.replaceChild(img, glyph);
        }
    }
}

function createLine(value) {
  if (value.startsWith('<math')) {
    var dom = document.createElement('div');
    dom.updateDOM(value);
    convertMathGlyphs(dom);
    return translateDOMElement(dom.childNodes[0]);
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

function afterProcessResult(ul, command) {
  // command is either 'Typeset' (default) or 'Rerender'
  if (!command)
    command = 'Typeset';
  MathJax.Hub.Queue([command, MathJax.Hub, ul]);
  MathJax.Hub.Queue(function() {
    // inject SVG and other non-MathML objects into corresponding <mspace>s
    ul.select('.mspace').each(function(mspace) {
      var id = mspace.getAttribute('id').substr(objectsPrefix.length);
      var object = objects[id];
      mspace.appendChild(object);
    });
  });
  if (!MathJax.Hub.Browser.isOpera) {
    // Opera 11.01 Build 1190 on Mac OS X 10.5.8 crashes on this call for Plot[x,{x,0,1}]
    // => leave inner MathML untouched
    MathJax.Hub.Queue(['Typeset', MathJax.Hub, ul]);
  }
  MathJax.Hub.Queue(function() {
    ul.select('foreignObject >span >nobr >span.math').each(function(math) {
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
  });
}

function setResult(ul, results) {
  results.each(function(result) {
    var resultUl = $E('ul', {'class': 'out'});
    result.out.each(function(out) {
      var li = $E('li', {'class': (out.message ? 'message' : 'print')});
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
  afterProcessResult(ul);
}

function submitQuery(textarea, onfinish) {
  if (welcome) {
  	$('welcomeContainer').fade({duration: 0.2});
    if ($('hideStartupMsg').checked) localStorage.setItem('hideMathicsStartupMsg', 'true');
    welcome = false;
    $('logo').removeClassName('load');
  }

  textarea.li.addClassName('loading');
  $('logo').addClassName('working');
  new Ajax.Request('/ajax/query/', {
    method: 'post',
    parameters: {
      query: textarea.value
    },
    onSuccess: function(transport) {
      textarea.ul.select('li[class!=request][class!=submitbutton]').invoke('deleteElement');
      if (!transport.responseText) {
        // A fatal Python error has occurred, e.g. on 4.4329408320439^43214234345
        // ("Fatal Python error: mp_reallocate failure")
        // -> print overflow message
        transport.responseText = '{"results": [{"out": [{"prefix": "General::noserver", "message": true, "tag": "noserver", "symbol": "General", "text": "<math><mrow><mtext>No server running.</mtext></mrow></math>"}]}]}';
      }
      var response = transport.responseText.evalJSON();
      setResult(textarea.ul, response.results);
      textarea.submitted = true;
      textarea.results = response.results;
      var next = textarea.li.nextSibling;
      if (next)
        next.textarea.focus();
      else
        createQuery();
    },
    onFailure: function(transport) {
      textarea.ul.select('li[class!=request]').invoke('deleteElement');
      var li = $E('li', {'class': 'serverError'}, $T("Sorry, an error occurred while processing your request!"));
      textarea.ul.appendChild(li);
      textarea.submitted = true;
    },
    onComplete: function() {
      textarea.li.removeClassName('loading');
      $('logo').removeClassName('working');
      if (onfinish)
        onfinish();
    }
  });
}

function getSelection() {
  // TODO
}

function keyDown(event) {
  var textarea = lastFocus;
  if (!textarea)
    return;
  refreshInputSize(textarea);

  if (event.keyCode == Event.KEY_RETURN && (event.shiftKey || event.location == 3)) {
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
  });
}

var queryIndex = 0;

function createQuery(before, noFocus, updatingAll) {
  var ul, textarea, moveHandle, deleteHandle, submitButton;
  // Items need id in order for Sortable.onUpdate to work.
  var li = $E('li', {'id': 'query_' + queryIndex++, 'class': 'query'},
    ul = $E('ul', {'class': 'query'},
      $E('li', {'class': 'request'},
        textarea = $E('textarea', {'class': 'request', 'spellcheck': 'false'}),
        $E('span', {'class': 'submitbutton', 'title': "Evaluate [Shift+Return]"},
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
  textarea.observe('focus', onFocus.bindAsEventListener(textarea));
  textarea.observe('blur', onBlur.bindAsEventListener(textarea));
  li.observe('mousedown', queryMouseDown.bindAsEventListener(li));
  deleteHandle.observe('click', deleteClick.bindAsEventListener(deleteHandle));
  deleteHandle.observe('mousedown', deleteMouseDown.bindAsEventListener(deleteHandle));
  moveHandle.observe('mousedown', moveMouseDown.bindAsEventListener(moveHandle));
  moveHandle.observe('mouseup', moveMouseUp.bindAsEventListener(moveHandle));
  $(document).observe('mouseup', moveMouseUp.bindAsEventListener($(document)));
  submitButton.observe('mousedown', function() {
    if (textarea.value.strip())
      submitQuery(textarea);
    else
      window.setTimeout(function() {
        textarea.focus();
      }, 10);
  });
  if (!updatingAll) {
    createSortable();
    // calling directly fails in Safari on document loading
    //window.setTimeout(createSortable, 10);
  }
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
    // case 67:
    case 83:
    case 79:
      return true;
    }
  }
  return false;
}

function globalKeyUp(event) {
  if (!popup && event.ctrlKey) {
    switch (event.keyCode) {
    case 68: // D
      showDoc();
      $('search').select();
      event.stop();
      break;
    // case 67: // C
    // 	focusLast();
    // 	event.stop();
    // 	break;
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
    "HTML-CSS": {
      imageFont: null,
    	linebreaks: { automatic: true }
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
        other:   "HTML"
      }
    }
  });
  MathJax.Hub.Configured();

  if (localStorage.getItem('hideMathicsStartupMsg') === 'true') {
    $('welcome').hide();
  }

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
        if (event.keyCode == Event.KEY_RETURN && event.shiftKey) {
          event.stopPropagation();
          event.preventDefault();
          keyDown(event);
        }
      }, true);
    }
    if (Prototype.Browser.Opera || Prototype.Browser.IE) {
      // Opera needs another hook so it doesn't insert newlines after Shift+Return
      $(document).observe('keypress', function(event) {
        if (event.keyCode == Event.KEY_RETURN && event.shiftKey)
          event.stop();
      }.bindAsEventListener());
    }

    $(document).observe('keyup', globalKeyUp.bindAsEventListener($('document')));

    if (!loadLink())
      createQuery();
  }
}

$(document).observe('dom:loaded', domLoaded);
// Konqueror won't fire dom:loaded, so we still need body.onload.

window.onresize = refreshInputSizes;
