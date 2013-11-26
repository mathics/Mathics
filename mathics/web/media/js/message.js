function grayOut(vis, options) {
  // Pass true to gray out screen, false to ungray
  // options are optional.  This is a JSON object with the following (optional) properties
  // opacity:0-100         // Lower number = less grayout higher = more of a blackout 
  // zindex: #             // HTML elements with a higher zindex appear on top of the gray out
  // bgcolor: (#xxxxxx)    // Standard RGB Hex color code
  // grayOut(true, {'zindex':'50', 'bgcolor':'#0000FF', 'opacity':'70'});
  // Because options is JSON opacity/zindex/bgcolor are all optional and can appear
  // in any order.  Pass only the properties you need to set.
  var options = options || {}; 
  var zindex = options.zindex || 50;
  var opacity = options.opacity || 70;
  var opaque = (opacity / 100);
  var bgcolor = options.bgcolor || '#000000';
  var dark = $('dark');
  if (!dark) {
    // The dark layer doesn't exist, it's never been created.  So we'll
    // create it here and apply some basic styles.
    // If you are getting errors in IE see: http://support.microsoft.com/default.aspx/kb/927917
    var tbody = $$("body")[0];
    var tnode = new Element('div', {'class': 'dark', 'id': 'dark'}).hide(); // Create the layer.
    tbody.appendChild(tnode);                            // Add it to the web page
    dark = tnode;
  }
  if (vis) {
    dark.style.zIndex = zindex;        
    dark.style.backgroundColor = bgcolor;  
    dark.setOpacity(opaque);
    if (navigator.userAgent.indexOf('Konqueror') == -1)
    	// don't use dark in Konqueror - opacity doesn't seem to work...
    	dark.show();
  } else {
  	dark.hide();
  }
}

var popup;

function showPopup(element, options) {
	options = options || {};
	var body = $$('body')[0];
	var container = new Element('div', {'class': 'popupContainer'});
	var div = new Element('div', {'class': 'popup'});
	// to fix IE SELECT z-index bug, see http://drupal.org/node/84608
	var frameContainer = new Element('div', {'class': 'popupFrameContainer'});
	var frame = new Element('iframe', {'class': 'popupFrame'});
	element = $(element).show();
	div.appendChild(element);
	container.appendChild(div);
	Element.insert(body, {'bottom': container});
	var dimensions = div.getDimensions();
	frame.setStyle({'width': dimensions.width + 'px', 'height': dimensions.height + 'px'});
	Element.insert(frameContainer, {'top': frame});
	if (Prototype.Browser.IE)
		Element.insert(body, {'bottom': frameContainer});
	grayOut(true, {zindex: 9});
	frame.scrollIntoView();
	
	var submit = element.select('input.submit, button.submit')[0];
	var onSubmit = function(event) {
		if (event.keyCode == Event.KEY_RETURN) {
			submit.onclick();
		}
	}.bindAsEventListener(body);
	if (submit && submit.onclick)
		$(document).observe('keydown', onSubmit);
	
	var cancel = element.select('input.cancel, button.cancel')[0];
	var onCancel = function(event) {
		if (event.keyCode == Event.KEY_ESC) {
			cancel.onclick();
		}
	}.bindAsEventListener(body);
	if (cancel && cancel.onclick)
		$(document).observe('keydown', onCancel);
	
	var input = element.select('input')[0];
	if (input)
		input.activate();
	
	popup = [[container, frameContainer], options, onSubmit, onCancel];
	
	return popup;
}

function hidePopup() {
	var containers = popup[0];
	var options = popup[1];
	var onSubmit = popup[2];
	var onCancel = popup[3];
	
	containers.each(function(item) {
		item.select('input, textarea, button').invoke('blur');
		item.hide();
	});
	
	grayOut(false);
	$(document).stopObserving('keydown', onSubmit);
	$(document).stopObserving('keydown', onCancel);
	
	popup = null;
}

var dialogYesCallback;
var dialogNoCallback;

function dialogYes() {
	hidePopup();
	dialogYesCallback();
}

function dialogNo() {
	hidePopup();
	dialogNoCallback();
}

function showDialog(title, text, yesCaption, noCaption, yesCallback, noCallback) {
	if (!noCallback)
		noCallback = Prototype.emptyFunction;
	
	var dialog = $('dialog');
	dialog.select('h1')[0].setText(title);
	dialog.select('p')[0].setText(text);
	dialog.select('input.submit')[0].value = yesCaption;
	dialog.select('input.cancel')[0].value = noCaption;
	dialogYesCallback = yesCallback;
	dialogNoCallback = noCallback;
	
	dialogPopup = showPopup(dialog);
	
	return dialogPopup;
}

function askQuestion(title, text, buttons, onYes, onNo) {
	if (!onNo)
		onNo = Prototype.emptyFunction;
	return showDialog(title, text, [
	  {caption: buttons[0], callback: onYes},
	  {caption: buttons[1], callback: onNo}
	]);
}
