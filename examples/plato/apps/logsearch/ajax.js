function async_load_list(endpoint, proc) {
    var xmlhttp = new XMLHttpRequest();
    xmlhttp.open("GET", endpoint, true);
    xmlhttp.send();
    xmlhttp.onreadystatechange = function() {
	if (xmlhttp.readyState == 4) {
	    var lists = JSON.parse(xmlhttp.responseText);
	    for (var i = 0; i < lists.length; i++) {
		proc(lists[i]);
	    }
	}
    }
}

function encode_for_cgi(s) {
    return encodeURIComponent(s).replace(/%20/g, '+');
}

function handle_result(result, proc)
{
    proc(result.result);
    if (!result.done) {
	setTimeout(function() {
	    submit_async_query(result.next, result.query, proc)
	}, result.wait);
    }
    return true;
}

function submit_async_query(endpoint, json_query, proc) {
    var xmlhttp = new XMLHttpRequest();
    xmlhttp.open("POST", endpoint, false);
    xmlhttp.setRequestHeader("Content-type",
			     "application/x-www-form-urlencoded");
    xmlhttp.send("json=" + encode_for_cgi(JSON.stringify(json_query)));
    if (xmlhttp.status == 200) {
	var result = JSON.parse(xmlhttp.responseText);
	return handle_result(result, proc);
    }
    return false;
}
