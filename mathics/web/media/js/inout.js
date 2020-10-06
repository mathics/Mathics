function showSave() {
	requireLogin("You must login to save worksheets online.", function() {
		showPopup($('save'));
	});
}

function openWorksheet(name) {
	hidePopup();
	new Ajax.Request('/ajax/open/', {
		method: 'post',
		parameters: {
			'name': name
		},
		onSuccess: function(transport) {
			var response = transport.responseText.evalJSON();
			if ($('document').visible())
				setContent(response.content);
			else
				$('codetext').value = response.content;
		}
	})
}

function showOpen() {
	requireLogin("You must login to open online worksheets.", function() {
		new Ajax.Request('/ajax/getworksheets/', {
			method: 'get',
			onSuccess: function(transport) {
				var response = transport.responseText.evalJSON();
				var tbody = $('openFilelist');
				tbody.deleteChildNodes();
				response.worksheets.each(function(worksheet) {
					tbody.appendChild($E('tr', $E('td',
						$E('a', {'href': 'javascript:openWorksheet("' + worksheet.name + '")'},
							$T(worksheet.name)
						)
					)));
				});
				showPopup($('open'));
			}
		});
	});
}

function cancelSave() {
	hidePopup();
}

function cancelOpen() {
	hidePopup();
}

function save(overwrite) {
	if (!overwrite)
		overwrite = '';
	var content;
	if ($('document').visible())
		content = getContent();
	else
		content = $('codetext').value;
	submitForm('saveForm', '/ajax/save/', function(response) {
		if (!checkLogin(response))
			return;
		cancelSave();
		if (response.result == 'overwrite') {
			showDialog("Overwrite worksheet", "There already exists a worksheet with the name '" +
				response.form.values.name + "'. Do you want to overwrite it?",
				'Yes, overwrite it', 'No, cancel', function() {
					save(true);
				});
		}
	}, {
		'content': content,
		'overwrite': overwrite
	});
}

function switchCode() {
	if ($('document').visible()) {
		$('document').hide();
		var content = getContent();
		$('codetext').value = content;
		$('code').show();
		$('codelink').setText("Interactive mode");
	} else {
		var content = $('codetext').value;
		setContent(content);
		function load() {
			$('code').hide();
			$('document').show();
			$('codelink').setText("View/edit code");
		}
		load();
	}
}

function getContent() {
	var queries = [];
	$('queries').childElements().each(function(query) {
		var item = {};
		var textarea = query.select('textarea.request')[0];
		item.request = textarea.value;
		item.results = textarea.results;
		queries.push(item);
	});
	var content = Object.toJSON(queries);

	return content;
}

function setContent(content) {
	$('queries').deleteChildNodes();

	$('welcome').hide();

	var queries = content.evalJSON();
	queries.each(function(item) {
		var li = createQuery(null, true, true);
		li.textarea.value = item.request;
		if( item.results != undefined ) {
			setResult(li.ul, item.results);
			li.textarea.results = item.results;
		}
	});

	createSortable();

	refreshInputSizes();

	lastFocus = null;
	if ($('queries').lastChild)
		$('queries').lastChild.textarea.focus();
}

function createLink() {
	var queries = new Array();
	$('queries').childElements().each(function(query) {
		var text = query.select('textarea.request')[0].getText();
		queries[queries.length] = 'queries=' + encodeURIComponent(text);
	});
	var query = queries.join('&');
	location.hash = '#' + btoa(query); //encodeURI(query);
}

function setQueries(queries) {
	var list = [];
	queries.each(function(query) {
		var li = createQuery(null, true, true);
		li.textarea.value = query;
		list.push({'li': li, 'query': query});
	});
	refreshInputSizes();
	function load(index) {
		if (index < list.length) {
			var item = list[index];
			submitQuery(item.li.textarea, function() {
				load(index + 1);
			});
		} else {
			createSortable();
			lastFocus = null;
			if ($('queries').lastChild)
				$('queries').lastChild.textarea.focus();
		}
	}
	load(0);
}

function loadLink() {
	var hash = location.hash;
	if (hash && hash.length > 1) {
		var params = atob(hash.slice(1)).split('&');
		var queries = [];
		params.each(function(param) {
			if (param.startsWith('queries=')) {
				param = param.slice(8);
				param = decodeURIComponent(param);
				if (param != "")
					queries.push(param);
			}
		});
		setQueries(queries);
		return queries.length > 0;
	} else
		return false;
}

function showGallery() {
	setQueries([
	    '$UseSansSerif = True (* Use Sans Serif font *)',
	    '(* Calculation *)',
	    'Sin[Pi]',
	    "E ^ (Pi I) (* Euler's famous equation *)",
	    'N[E, 30] (* 30-digit Numeric approximation of E *)',
	    '30! (* Factorial *)',
	    '% // N',
	    'Sum[2 i + 1, {i, 0, 10}] (* Sum of 1st n odd numbers (n+1)**2 *)',
	    'n = 8; 2 ^ # & /@ Range[0, n] (* Powers of 2 *)',
	    'Total[%] (* Sum is 2 ^ n - 1 *)',
	    '(* Symbolic Manipulation *)',
	    'Apart[1 / (x^2 + 5x + 6)]',
	    'Cancel[x / x ^ 2]',
	    'Expand[(x + y)^ 3]',
	    'Factor[x ^ 2 + 2 x + 1]',
	    'Simplify[5*Sin[x]^2 + 5*Cos[x]^2]',
	    '(* Calculus *)',
	    "Sin'[x]",
	    "Sin''[x]",
	    'D[Sin[2x] + Log[x] ^ 2, x]',
	    'Integrate[Tan[x] ^ 5, x]',
	    '(* Linear Algebra *)',
	    'MagicSquare = {{2, 7, 6}, {9, 5, 1}, {4, 3, 8}}; MatrixForm[MagicSquare]',
	    'LinearSolve[MagicSquare, {1, 1, 1}] // MatrixForm',
	    'Eigenvalues[MagicSquare]',
	    '(* Chemical Data *)',
	    '(* 2nd and 3rd Row of Periodic Table *)',
	    'Grid[{Table[ElementData[i], {i, 3, 10}], Table[ElementData[i], {i, 11, 18}]}]',
	    'ListLinePlot[Table[ElementData[z, "MeltingPoint"], {z, 118}]]',
	    '(* Some graphs *)',
	    'ListPlot[{Sqrt[Range[40]], Log[Range[40, 80]]}]',
	    'ListLinePlot[Table[Sin[x], {x,-5, 5, 0.5}], Filling->Axis]',
	    'Plot[{Sin[x], Cos[x], Tan[x]}, {x, -3Pi, 3Pi}]',
	    'Plot[{Abs[Sin[x]], Abs[Cos[x]]}, {x,0,4 Pi}, Mesh->Full]',
	    'BarChart[{{1, 2, 3}, {2, 3, 4}}]',
	    'Graphics[Line[AnglePath[Table[1.7, {50}]]]]',
	    'Graphics[Table[{EdgeForm[{GrayLevel[0, 0.5]}], Hue[(-11+q+10r)/72, 1, 1, 0.6], Disk[(8-r){Cos[2Pi q/12], Sin [2Pi q/12]}, (8-r)/3]}, {r, 6}, {q, 12}]]',
	    '(* 3D graphics *)',
	    'Graphics3D[Sphere[{0, 0, 0}, 1]]',
	    'Plot3D[Exp[x] Cos[y], {x, -2, 1}, {y, -Pi, 2 Pi}, PlotPoints->21]',
            'Plot3D[Sin[x y], {x, -2, 2}, {y, -2, 2}, Mesh->Full, PlotPoints->21]',
	    'Graphics3D[Polygon[{{0,0,0}, {0,1,1}, {1,0,0}}]]'
	]);
}
