var xhr = false;

if (window.XMLHttpRequest) {
    xhr = new XMLHttpRequest();
} else if (window.ActiveXObject) {
    xhr = new ActiveXObject("Microsoft.XMLHTTP");
}

function getNames() {
    if (xhr) {
        url = "http://127.0.0.1:9191/name";
        xhr.open("GET", url);
        xhr.onreadystatechange = callback;

        xhr.send(null);
    }
}

function callback() {
    if (xhr.readyState == 4 && xhr.status == 200) {
        call(xhr.responseText);
    }
}

function call() {
    if ((xhr.readyState == 4) && (xhr.status == 200)) {
        var arr = (xhr.responseText.trim()).split(",");

        for (i = 0; i < arr.length; i++) {
            // console.log(arr[i]);
            arr[i] = arr[i].replace("\"", "");
            arr[i] = arr[i].replace("\"", "");
            arr[i] = arr[i].replace("[", "");
            arr[i] = arr[i].replace("]", "");
            document.getElementById('names').options[i] = new Option(arr[i], arr[i]);
            // console.log(arr[i]);
        }
    }
}

function plot() {
    format = "blank";

    if (document.getElementById("c1").checked) {
        format = "T20I";
    } else if (document.getElementById("c2").checked) {
        format = "ODI";
    } else if (document.getElementById("c3").checked) {
        format = "Test";
    }
	else{
		
		format = "T20I";
	}

    url = "http://127.0.0.1:9191/" + document.getElementById("typeofplot").value
    url = url + "?name=" + document.getElementById("names").value + "&format=" + format;

    document.getElementById("plot1").src = url;
}
