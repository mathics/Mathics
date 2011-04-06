
var HTML_ENTITIES = $H({
	'amp': '&',
	'gt': '>',
	'lt': '<',
	'quot': '"',
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
});
HTML_ENTITIES.each(function(pair) {
	var value = pair.value;
	if (isFinite(value))
		HTML_ENTITIES.set(pair.key, String.fromCharCode(value));
});

function stringToDOM (inputString) {
// Die Funktion erwartet einen validen XHTML-String, der als DOM zurück gegeben wird
	
	
	var normaliseString = function (str) {
	// erwartet einen String und gibt ihn bereinigt von Zeilenumbrüchen zurück
		
		//str = str.replace(/\r/g, " ");
		//str = str.replace(/\n/g, " ");
		
		// CHANGED: normalize whitespace (multiple spaces treated literally by IE!)
		//str = str.replace(/\s\s/g, " ");
		str = str.replace(/\s+/g, " ");
		
		//alert(str);
	
		return str;
	};
	
	
	
	var deEntity = function (str) {
	// erwartet einen String und wandelt Entitäten um
		
		// // CHANGED: use Prototype's unescape
		
		/*str = str.replace(/&amp;/g, "&");
		str = str.replace(/&gt;/g, ">");
		str = str.replace(/&lt;/g, "<");
		str = str.replace(/&nbsp;/g, String.fromCharCode(160));
		str = str.replace(/&quot;/g, '"');*/
		
		/*HTML_ENTITIES.each(function(pair) {
			str = str.replace('&' + pair.key + ';', pair.value);
		});*/
		
		//str = str.unescapeHTML(); // NO! (internally uses innerHTML!)
		
		str = str.gsub(/&([a-zA-Z]+);/, function(match) {
			return HTML_ENTITIES.get(match[1]);
		});
		
		// NEW: unescape numbered entities
		str = str.gsub(/&#([0-9]+);/, function(match) {
			return String.fromCharCode(match[1]);
		});
		
		return str;
	};
	
	
	
	var deSpace = function (str) {
	// erwartet einen String und gibt ihn bereinigt von Leerzeichen zurück
		
		str = str.replace(/ /g, "");
	
		return str;
	};
	
	
	
	var processTag = function (str) {
	// parst das Innere eines Tags und gibt ein Objekt zurück
	
		// Objektcontainer erzeugen
		var newDOM = document.createDocumentFragment();
		
		// suche Leerzeichen
		var space = str.indexOf(' ');
		
		// wenn das Tag keine Attribute hat
		if (space === -1) {
			
			var tagName = str; //.toLowerCase();			
			newDOM.appendChild(document.createElement(tagName));
			
		} else { // wenn das Tag Attribute hat
			
			// Tag-Namen herausfinden
			tagName = deSpace(str.substring(0, space)); //.toLowerCase();
			
			
			// Weiche für den IE, weil er type bei inputs nicht setzen kann
			if (document.all && tagName === 'input') {
				newDOM.appendChild(document.createElement('<' + str + '/>'));
				return newDOM;
			}
			
			
			// bis zum ersten Leerzeichen löschen
			str = str.substring(space + 1);
			
			// Tag einhängen
			newDOM.appendChild(document.createElement(tagName));
				
			// Attribute auslesen
			while (str.length > 0) {
			
				// suche nach '='
				var equal = str.indexOf('=');
			
				if (equal >= 0) {
					
					// Attributenname
					var attributeName = deSpace(str.substring(0, equal)); //.toLowerCase();
					
					// bis zum ersten '"' löschen					
					var quote = str.indexOf('"');
					str = str.substring(quote + 1);

					// bis zum zweiten '"' geht der Attributwert
					quote = str.indexOf('"');
					
					var attributeValue = deEntity(str.substring(0, quote));

					// Bis zum nächsten Attribut löschen
					str = str.substring(quote + 2);
					
					// CHANGED: class handling
					
					if (attributeName == 'class') {
						$w(attributeValue).each(function(className) {
							$(newDOM.lastChild).addClassName(className);
							//alert(className);
						});
						//newDOM.lastChild.className = attributeValue;
						//newDOM.lastChild.className = 'class1 class2';
					} else {					
						// Weiche für den IE, er kann style-Attribute nicht verarbeiten...
						if (document.all && attributeName === 'style') {
							// Attribut für letzten Knoten setzen
							newDOM.lastChild.style.cssText = attributeValue;
							
						} else {
							// Attribut für letzten Knoten setzen
							newDOM.lastChild.setAttribute(attributeName, attributeValue);					
						}
					}
				
				} else {
					break;
				}
			}
		}
		
		return newDOM;
	
	};
	
	
	
	var findEndTag = function (innerStr, str, lastTag) {
	
		// alle Kommentare und leeren Tags löschen
		var funcInnerStr = innerStr;
		var funcStr = str;
	
		lastTag = lastTag; //.toLowerCase();
			
		// wo ist das schließende Tag?
		var nextClosingTag = funcStr.indexOf('</' + lastTag + '>');
			
		// funcInnerStr und func Str auffrischen
		funcInnerStr = funcInnerStr.concat(funcStr.substring(0, nextClosingTag));
		funcStr = funcStr.substring(nextClosingTag);
			
		while (funcInnerStr.indexOf('<' + lastTag) != -1) {
		// solange öfnnende Tags in innerStr vorhanden sind
		
			// öffnendes Tag löschen
			funcInnerStr = funcInnerStr.substring(funcInnerStr.indexOf('<' + lastTag));
			funcInnerStr = funcInnerStr.substring(funcInnerStr.indexOf('>') + 1);

			// schließendes Tag löschen
			funcStr = funcStr.substring(funcStr.indexOf('>') + 1);
			
			// nächstes schließendes suchen
			nextClosingTag = funcStr.indexOf('</' + lastTag + '>');
			
			// bis zum schließenden in den innerStr übernehmen und bis zum schließenden im str löschen
			funcInnerStr = funcInnerStr.concat(funcStr.substring(0, nextClosingTag));
			funcStr = funcStr.substring(nextClosingTag);
			
		}
		
		// Position des schließenden Tags innerhalb von str zurück geben
		return str.length - funcStr.length;
	
	};
	
	
	
	var parseString = function (str) {
	// geht den String von vorne durch und legt ein Objekt an
	
		// Objektcontainer erzeugen
		var newDOM = document.createDocumentFragment();
	
		//alert("parse " + str);
		
		// Schleifen sie!
		while (str && str.length > 0) {

			var lowerThan = str.indexOf("<");
		
			// < ist nicht vorhanden --> Textknoten
			if (lowerThan === -1) {
				
				str = deEntity(str);
				
				newDOM.appendChild(document.createTextNode(str));
				str = null;
				
			}
				
			// wenn vor dem ersten Tag nur Text kommt
			if (lowerThan > 0) {
					
				// es wird der Teil bis zum nächsten Tag abgetrennt und 
				// als Textknoten angehangen
				var newTextFrag = deEntity(str.substring(0, lowerThan));
				newDOM.appendChild(document.createTextNode(newTextFrag));
			
				// Reststring speichern
				str = str.substring(lowerThan);
				
			}
				
			// wenn das Tag am Anfang steht
			if (lowerThan === 0) {
				
				var comment = str.indexOf('<!--');
				
				// wenn das Tag ein Kommentar ist
				if (comment === 0) {
					
					var commentEnd = str.indexOf('-->');
					var commentContent = str.substring(4, commentEnd);
					commentContent = deEntity(commentContent);			
					
					newDOM.appendChild(document.createComment(commentContent));
					
					str = str.substring(commentEnd + 3);
				
				} else {
				// wenn es ein anderes Tag ist
				
					var greaterThan = str.indexOf('>');
					
					// wenn es ein leeres Tag ist
					if (str.substring(greaterThan - 1, greaterThan) === '/') {
					
						var emptyTagEnd = str.indexOf('/>');
						var emptyTagContent = str.substring(1, emptyTagEnd);
						
						// Tag einfügen
						newDOM.appendChild(processTag(emptyTagContent));
						
						// Reststring speichern
						str = str.substring(emptyTagEnd + 2);						
						
					} else {
					// wenn es ein normales Tag ist
					
						var normalTagEnd = str.indexOf('>');
						var normalTagContent = str.substring(1, normalTagEnd);
						var tmpNewDOM = document.createDocumentFragment();
						
						// Tag einfügen
						tmpNewDOM.appendChild(processTag(normalTagContent));
						
						// Reststring ohne Taganfang speichern
						str = str.substring(normalTagEnd + 1);
						
						// bis zum nächsten schließenden Tag speichern
						var innerStr = str.substring(0, str.indexOf('</'));
						str = str.substring(str.indexOf('</'));
						
						// wenn in innerStr ein Tag steckt
						if (innerStr.indexOf('<') != -1) {
							
							var lastTag = tmpNewDOM.lastChild.nodeName;
							
							var posOfEndTag = findEndTag(innerStr, str, lastTag) ;
 
							innerStr = innerStr.concat(str.substring(0, posOfEndTag));
							str = str.substring(posOfEndTag);
													
						} 
						
						// schließendes Tag löschen
						str = str.substring(str.indexOf('>') + 1);
											
						// an temporären DOM anhängen
						tmpNewDOM.lastChild.appendChild(parseString(innerStr));
						
						// temporären DOM in VaterDOM einhängen
						newDOM.appendChild(tmpNewDOM);
					
					}				
				}				
			}	
		}
		
		return newDOM;
		
	};
	
	
	// der eigentliche Programmaufruf
	// CHANGED
	var newDOM = parseString(normaliseString(inputString));
	//var newDOM = parseString(inputString);
	
	// gib das Stöckchen
	return newDOM;
	
}




function DOMtoString (nodeObj) {
// Diese Funktion erwartet ein DOM-Objekt und gibt es als XHTML-String zurück

	
	
	var entify = function (str) {
	// erwartet einen String und wandelt Entitäten um
		
		str = str.replace(/&/g, "&amp;");
		str = str.replace(/>/g, "&gt;");
		str = str.replace(/</g, "&lt;");
		str = str.replace(/\"/g, '&quot;');
		
		return str;
	};
	
	
	
	var parseDOM = function (node) {
	
		var children = node.childNodes;
		var string = '';
		
		for (var i = 0; i < children.length; i++) {
		
			var whatType = children[i].nodeType;
			
			switch (whatType) {
			
				case 1:
				// Elementknoten	
					var tagName = children[i].nodeName; //.toLowerCase();
					var tagAttributes = children[i].attributes;
					
					string = string.concat('<' + tagName);
					
					if (tagAttributes.length > 0) {
					// Wenn Attribute vorhanden sind
									
						for (var ii = 0; ii < tagAttributes.length; ii++) {
						
							if (document.all) {
							// für den IE 6 und höher...
								
								if (tagAttributes[ii].nodeName &&
									tagAttributes[ii].nodeValue !== null && 
									// nodeValue != '' wegen IE nicht !== ''
									tagAttributes[ii].nodeValue != '' &&
									(tagAttributes[ii].nodeName != 'contentEditable' &&
									tagAttributes[ii].nodeValue != 'inherit') &&
									(tagAttributes[ii].nodeName != 'shape' &&
									tagAttributes[ii].nodeValue != 'rect')) {	
										
									string = string.concat(' ' + tagAttributes[ii].nodeName /*.toLowerCase()*/ + 
											'="' + entify(tagAttributes[ii].nodeValue) + '"');

								}

								if (tagAttributes[ii].nodeName === 'style' &&
									children[i].style.cssText !== null && 
									children[i].style.cssText.length !== 0) {
								
									string = string.concat(' style="' + 
									children[i].style.cssText /*.toLowerCase()*/ + ';"');	
										
								}
														
							} else {

								// NEW: no attributes starting with "-"
								if (tagAttributes[ii].nodeName != '' && tagAttributes[ii].nodeName[0] == '-')
									continue;
								
								string = string.concat(' ' + tagAttributes[ii].nodeName /*.toLowerCase()*/ + 
									'="' + entify(tagAttributes[ii].nodeValue) + '"');
																
							}
						}
					} 
					
					if (tagName === 'meta' || tagName === 'img' || 
						tagName === 'br' || tagName === 'input' ||
						tagName === 'link' || tagName === 'hr') {
						
						string = string.concat(' />');
						
					} else {					
					
						string = string.concat('>' + parseDOM(children[i]) +
							'</' + tagName + '>');
						
					}
					
					break;
					
				case 3:
				// Textknoten
					string = string.concat(entify(children[i].nodeValue));
					
					break;
					
				case 8:
				// Kommentarknoten
					string = string.concat('<!--' + entify(children[i].nodeValue) + '-->');
					
					break;
			
			}
			
		}
		
		return string;
		
	};
	
	
	
	var xhtmlString = parseDOM(nodeObj);
	
	return xhtmlString;

}
