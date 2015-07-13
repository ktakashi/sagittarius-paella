function load_lists() {
    var xmlhttp = new XMLHttpRequest();
    xmlhttp.open("GET", "/memo/list", true);
    xmlhttp.send();

    xmlhttp.onreadystatechange = function() {
	if (xmlhttp.readyState == 4) {
	    var div = document.getElementById("memo-list");
	    div.innerHTML = ""; // don't need
	    if (xmlhttp.status != 200) {
		div.innerHTML = "failed to load";
		return;
	    }
	    var lists = JSON.parse(xmlhttp.responseText);
	    var ul = document.createElement("ul");
	    for (var i = 0; i < lists.length; i++) {
		var item = lists[i];
		// item ::= {title : "", file : ""}
		var li = document.createElement("li");
		var file = encode_for_cgi(item['file']);
		var title = encode_for_cgi(item['title']);
		li.innerHTML = 
		    '<div>' +
		    '<a href="javascript:void(0)"' +
		    ' onclick="load_file(\'' + title + '\')">' 
		    + item['title'] + '</a> ' +
		    ' <a href="javascript:void(0)"' +
		    ' onclick="edit_file(\'' + title + '\')">edit</a>' +
		    ' <a href="javascript:void(0)"' +
		    ' onclick="remove_file(\'' + title + '\')">remove</a>' +
		    '</div>';
		ul.appendChild(li);
	    }
	    div.appendChild(ul);
	}
    }
}

function load_file(file) {
    var xmlhttp = new XMLHttpRequest();
    xmlhttp.open("GET", "/memo/load?name=" + file + "&markdown=true", true);
    xmlhttp.send();

    xmlhttp.onreadystatechange = function() {
	if (xmlhttp.readyState == 4) {
	    var div = document.getElementById("memo-view");
	    var memo = JSON.parse(xmlhttp.responseText);
	    div.innerHTML = memo['content'];
	}
    }
}

function edit_file(file) {
    var xmlhttp = new XMLHttpRequest();
    xmlhttp.open("GET", "/memo/load?name=" + file + "&markdown=false", false);
    xmlhttp.send();

    var memo = JSON.parse(xmlhttp.responseText);
    document.getElementById('name').value = memo['name'];
    document.getElementById('tags').value = memo['tags'];
    document.getElementById('content').value = memo['content'];
}

function remove_file(title) {
    if (confirm('Remove ' + title + '?')) {
	var xmlhttp = new XMLHttpRequest();
	xmlhttp.open("GET", "/memo/remove?title=" + title, false);
	xmlhttp.send();
	
	var div = document.getElementById("memo-view");
	div.innerHTML = xmlhttp.responseText;

	load_lists();
    }
}

function is_empty_string(value) {
    return (!value || value == "");
}

function encode_for_cgi(s) {
    return encodeURIComponent(s).replace(/%20/g, '+');
}

function submit_memo(name, tags, memo) {
    var nameValue = name.value;
    var tagValue = tags.value;
    var memoValue = memo.value;
    if (is_empty_string(nameValue)) {
	alert("name must be set");
	return;
    }
    if (is_empty_string(memoValue)) {
	alert("memo must be set");
	return;
    }

    var xmlhttp = new XMLHttpRequest();
    xmlhttp.open("POST", "/memo", false);
    xmlhttp.setRequestHeader("Content-type",
			     "application/x-www-form-urlencoded");
    xmlhttp.send("name=" + encode_for_cgi(nameValue) + "&" +
		 "tags=" + encode_for_cgi(tagValue) + "&" +
		 "content=" + encode_for_cgi(memoValue));
    name.value = "";
    tags.value = "";
    memo.value = "";
    load_lists();
}
