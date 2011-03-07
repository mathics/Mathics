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

var ElementMethods = {
	setText: function(element, text) {
		element.deleteChildNodes();
		element.appendChild(document.createTextNode(text));
		return element;
	},
	
	getText: function(element) {
		if (element.nodeName.toLowerCase() == 'textarea')
			// in case of a textarea
			return element.value;
		else
			return element.childNodes[0].data;
	},
	
	deleteChildNodes: function(element) {
	  while (element.hasChildNodes())
	  	element.removeChild(element.lastChild);
	  return element;
	},
	
	setClassName: function(element, className, yesno) {
		if (yesno)
			element.addClassName(className);
		else
			element.removeClassName(className);
		return element;
	},
	
	isShown: function(element, recursionDepth) {
		return element.ancestors().invoke('visible').all();
	},
	
	tryFocus: function(element) {
		if (element.isShown()) {
			try {
				element.focus();
				return true;
			} catch (e) {
				return false;
			}
		} else
			return false;
	},
	
	deleteElement: function(element) {
		if (element.parentNode)
			element.parentNode.removeChild(element);
	},
	
	updateDOM: function(element, content) {
		var dom = stringToDOM(content);
		element.deleteChildNodes();
		element.appendChild(dom);
	},
	
	replaceDOM: function(element, content) {
		var dom = stringToDOM(content);
		element.parentNode.replaceChild(dom, element);
		
	},
	
	scrollIntoView: function(element) {
		var offset = element.cumulativeOffset(); 
		window.scrollTo(offset.left, offset.top);
	},
	
	setEnabled: function(element, enabled) {
		if (element.enable && element.disable) {
			if (enabled)
				element.enable();
			else
				element.disable();
		}
		if (enabled)
			element.removeClassName('disabled');
		else
			element.addClassName('disabled');
		element.childElements().each(function(child) {
			child.setEnabled(enabled);
		});
	}
};

Element.addMethods(ElementMethods);

Object.extend(Object, {
	isDOMElement: function(object) {
		return object && !!object.nodeType;
	}
});

// untested!
/*Object.extend(Function.prototype, {
	curryArray: function(args) {
		var that = this;
		return function() {
			return that.apply(this, args.concat($A(arguments)));
		}		
	},
	
	curryNamed: function(extra) {
		extra = $H(extra);
		var names = this.argumentNames();
		var args = names.map(function(element) {
			return extra.get(element);
		});
		return this.curryArray(args);
	}
});*/

var Publisher = Class.create({
	initialize: function() {
		this.events = $H();
	},
	
	fire: function(event) {
		var subscribers = this.events.get(event) || $A();
		var args = $A(arguments).slice(1);
		subscribers.each(function(subscriber) {
			subscriber.apply(this, args);
		}.bind(this));
	},
	
	observe: function(event, subscriber) {
		var existing = this.events.get(event);
		if (!existing)
			this.events.set(event, $A([subscriber]));
		else
			existing.push(subscriber);
	}
});

function exists(type) {
  return type != "undefined";
}

function dateToStr(date) {
	return date.getFullYear() + '-' + (date.getMonth() + 1) + '-' + date.getDate(); 
}

function dateFromJSON(json) {
	if (json) {
		parts = json.split('-');
		return new Date(parts[0], parts[1] - 1, parts[2]);
	} else
		return null;
}

var lastCanceledLog = 0;

function log(message) {
	var now = new Date().getTime();
	if (now - lastCanceledLog < 100) {
		lastCanceledLog = new Date().getTime();
		return false;
	}
	message += '\n\n---\n' + 'Arguments: ' + Object.inspect($A(log.caller.arguments));
	if (!window.confirm(message))
		lastCanceledLog = new Date().getTime();
	return true;
}

function $E(tag, properties) {
	var children;
	if (Object.isDOMElement(properties)) {
		children = $A(arguments).slice(1);
		properties = {};
	} else
		children = $A(arguments).slice(2);
	if (tag == 'a' && properties.href == null)
		properties.href = 'javascript:;';
	var element = new Element(tag, properties);
	children.each(function(child) {
		if (child)
			element.appendChild(child);
	});
	return element;
}

function $T(text) {
	return document.createTextNode(text);
}

function submitForm(form, url, onSuccess, extraData) {
	var params = {};
	form = $(form);
	form.select('input').each(function(input) {
		params[input.name] = input.value;
	});
	form.select('input[type="text"]').each(function(input) {
		input.blur();
	});
	form.select('input, button').each(function(input) {
		input.disable();
	});
	if (!extraData)
		extraData = {};
	params = $H(params).merge(extraData);
	new Ajax.Request(url, {
		method: 'post',
		parameters: params,
		onSuccess: function(transport) {
			var response = transport.responseText.evalJSON();
			form.select('ul.errorlist').invoke('deleteElement');
			var errors = false;
			var errorFocus = false;
			if (response.form.fieldErrors)
				$H(response.form.fieldErrors).each(function(pair) {
					errors = true;
					var errorlist = $E('ul', {'class': 'errorlist'});
					var input = form.select('[name="' + pair.key + '"]')[0];
					pair.value.each(function(msg) {
						errorlist.appendChild($E('li', $T(msg)));
					});
					input.insert({before: errorlist});
					if (!errorFocus)
						errorFocus = input;
				});
			if (response.form.generalErrors) {
				var errorlist = $E('ul', {'class': 'errorlist'});
				response.form.generalErrors.each(function(msg) {
					errors = true;
					errorlist.appendChild($E('li', $T(msg)));
				});
				form.insert({top: errorlist});
				if (!errorFocus) {
					var firstInput = form.select('input')[0];
					if (firstInput)
						errorFocus = firstInput;
				}
			}
			form.select('input').each(function(input) {
				input.enable();
			});
			if (errorFocus) {
				errorFocus.activate();
			}	
			if (!errors) {
				response.values = params;
				onSuccess(response);
			}
		},
		onComplete: function() {
			form.select('input').each(function(input) {
				input.enable();
			});
		}
	})
}
