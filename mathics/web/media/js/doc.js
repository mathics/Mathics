var docLoaded = false;
var lastSearchValue = '';

function showPage(response) {
	if ($('doc')) {
		$('doc').updateDOM(response.content);
	}

    $$('li.test p').each(function(test){
        test.insert($E('span',
                    {'class': 'submitbutton', 'title': "Run this example!"},
					submitButton = $E('span', $T('='))
				));

        test.observe('mouseover', function(e){
            $(test).addClassName('focused');
        });
        test.observe('mouseout', function(e){
            $(test).removeClassName('focused');
        });

        $(test).descendants()[1].observe('click', function(){
            var query = $(test).descendants()[0].innerHTML;
            query = query.replace(/\xA0/g, ' ');
            query = query.unescapeHTML();
            setQueries([query]);
        });
    });

	$$('ul.test').each(function(test) {
		var id = test.id.substr(5); // 'test_...'
		var data = response.data[id];
		setResult(test, data.results);
	});

}

function loadDoc(page) {
	new Ajax.Request('/ajax/doc' + page, {
		method: 'get',
		onSuccess: function(transport) {
			docLoaded = true;
			var response = transport.responseText.evalJSON();
			showPage(response);
			$$('#doc *')[0].scrollIntoView();
		}
	});
}

function showDoc() {
	$('document').addClassName('doc');
	$('code').addClassName('doc');
	$('doc').show();
	$('doclink').addClassName('active');
  $('doclink').select('i')[0].removeClassName('fa-question-circle-o');
  $('doclink').select('i')[0].addClassName('fa-question-circle');
  $('search').addClassName('shown');
  // $('search').focus();
	if (!docLoaded)
		loadDoc('/');
}

function hideDoc() {
	$('doc').hide();
	$('document').removeClassName('doc');
	$('code').removeClassName('doc');
	$('doclink').removeClassName('active');
  $('doclink').select('i')[0].removeClassName('fa-question-circle');
  $('doclink').select('i')[0].addClassName('fa-question-circle-o');
  $('search').removeClassName('shown');
}

function toggleDoc() {
	if ($('doc').visible())
		hideDoc();
	else
		showDoc();
    $('search').select();
}

function searchChange(event) {
	var query = $('search').value.strip();
	if (!$('search').hasClassName('empty')) {
		if (query) {
			new Ajax.Request('/ajax/doc/search/', {
				method: 'get',
				parameters: {
					query: query
				},
				onSuccess: function(transport) {
					docLoaded = true;
					var response = transport.responseText.evalJSON();
					showPage(response);
					showDoc();
				}
			});
		} else if (lastSearchValue != ''){
			hideDoc();
			loadDoc('/');
		}
		lastSearchValue = query;
	} else
		lastSearchValue = '';
}

function searchFocus() {
	if ($('search').hasClassName('empty')) {
		$('search').value = '';
		$('search').removeClassName('empty');
	}
}

function searchBlur() {
	if (!$('search').value) {
		$('search').addClassName('empty');
		$('search').value = "\uf002";
	}
}

function searchKeyUp(event) {
	if (event.keyCode == Event.KEY_ESC) {
		event.stop();
		$('search').value = '';
		hideDoc();
		loadDoc('/');
		focusLast();
	}
}

function searchKeyDown(event) {
	if (isGlobalKey(event))
		event.stop();
}

function initDoc() {
	if (!$('search'))
		return;
	new Form.Element.Observer('search', 0.2, searchChange.bindAsEventListener($('search')));
	$('search').observe('focus', searchFocus);
	$('search').observe('blur', searchBlur);
	$('search').observe('keydown', searchKeyDown.bindAsEventListener($('search')));
	$('search').observe('keyup', searchKeyUp.bindAsEventListener($('search')));
	$('search').value = '';
	searchBlur();
}

$(document).observe('dom:loaded', initDoc);
