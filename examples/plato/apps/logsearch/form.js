function add_option(item) {
    var op = document.createElement("option");
    var fl = document.getElementById("file_list");
    op.value = item;
    op.text = item;
    fl.add(op);
}
function is_empty_string(value) {
    return (!value || value == "");
}
// ugly
var flip = true;
function send_query(button) {
    var fl = document.getElementById("file_list");
    var q = document.getElementById("search_query");
    var reg = document.getElementById("as_regexp");
    var files = [];
    for (i = 0; i < fl.options.length; i++) {
	if (fl.options[i].selected) {
	    files.push(fl.options[i].value);
	}
    }
    if (files.length == 0) {
	alert("Select file(s)");
	return;
    }
    if (is_empty_string(q.value)) {
	alert("Search query is required");
	return;
    }
    var c = document.getElementById("content_container");
    while (c.firstChild) {
	c.removeChild(c.firstChild);
    }
    submit_async_query("/logsearch/search",
		       {"files": files,
			"query": q.value,
			"regexp": reg.checked },
		       render);
}

function class_change(e, className) {
    return function() {
	e.className = className;
    };
}

function render(result) {
    var c = document.getElementById("content_container");
    for (var i = 0; i < result.length; i++) {
	var div = document.createElement("div");
	var lines = result[i].split("\n");
	var first = document.createElement("span");
	var text = document.createTextNode(lines[0]);

	first.className = "first_line";
	first.appendChild(text);
	div.appendChild(first);

	var rest = document.createElement("div");
	rest.className = "rest_container_hide";
	div.appendChild(rest);
	for (var j = 1; j < lines.length; j++) {
	    var rest_line = document.createElement("span");
	    var line = lines[j];

	    if (is_empty_string(line)) continue;

	    var text2 = document.createTextNode(line);
	    rest_line.className = "rest_line";
	    rest_line.appendChild(text2);
	    rest.appendChild(rest_line);
	}
	first.addEventListener('click', 
			       class_change(rest, "rest_container_show"));
	rest.addEventListener('click', 
			      class_change(rest, "rest_container_hide"));
	div.className = (flip) ? "log0" : "log1";
	flip = !flip;
	c.appendChild(div);
    }
}
