 var items = tipuesearch['pages'];
 var documents = tipuesearch["pages"]
 var counter = 0

 for (item in documents){
     documents[item]['id'] = counter;
     counter = counter +1;
 }

 var idx = lunr(function () {
   this.ref('id')
   this.field('title')
   this.field('url')
   this.field('text', { boost: 10 })
   this.field('tags')

   items.forEach(function (doc) {
     this.add(doc)
   }, this)
})

function lunr_search(term) {
    document.getElementById('lunrsearchresults').innerHTML = '<ul></ul>';
    if(term) {
	document.getElementById('lunrsearchresults').innerHTML = "<p>Search results for '" + term + "'</p>" + document.getElementById('lunrsearchresults').innerHTML;
	//put results on the screen.
	var results = idx.search(term);
	if(results.length>0){
	    //console.log(idx.search(term));
	    //if results
	    for (var i = 0; i < results.length; i++) {
		// more statements
		var ref = results[i]['ref'];
		var url = documents[ref]['url'];
		var title = documents[ref]['title'];
		var body = documents[ref]['text'].substring(0,160)+'...';
		document.querySelectorAll('#lunrsearchresults ul')[0].innerHTML = document.querySelectorAll('#lunrsearchresults ul')[0].innerHTML + "<li class='lunrsearchresult'><a href='" + url + "'><span class='title'>" + title + "</span></a><br /><span class='body'>"+ body +"</span><br /><span class='url'>"+ url +"</span></li>";
	    }
	} else {
	    document.querySelectorAll('#lunrsearchresults ul')[0].innerHTML = "<li class='lunrsearchresult'>No results found...</li>";
	}
    }
    return false;
}

function getQueryVariable(variable) {
  var query = window.location.search.substring(1);
  var vars = query.split('&');

  for (var i = 0; i < vars.length; i++) {
    var pair = vars[i].split('=');

    if (pair[0] === variable) {
      return decodeURIComponent(pair[1].replace(/\+/g, '%20'));
    }
  }
}

var searchTerm = getQueryVariable('q');
if (searchTerm) {
  lunr_search(searchTerm)
}
