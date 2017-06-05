getWidgets = function(im){
  # keep the first part of mine url (e.g. http://www.flymine.org/)
  mine.url = substr(im$mine, start = 1, stop = gregexpr("/",im$mine)[[1]][length(gregexpr("/",im$mine)[[1]])])
  
  # get widget results and convert rirectly to R objects with jsonlite::fromJSON
  res = jsonlite::fromJSON(txt = paste0(mine.url, "query/service/widgets?format=json"))
  
  res$widgets
}
