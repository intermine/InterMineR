doEnrichment = function(
  im, # Mine to use
  genelist = NULL, # The name of the list to investigate, optional unless identifiers is NULL.
  ids = NULL, # Comma-separated list of InterMine object IDs, optional unless list name is NULL.
  widget = NULL, # The name of the enrichment widget to display. Use getWidgets for available enrichment type widgets for the respective Mine.
  population = NULL, # The name of the list to use as the background population
  maxp = 0.05, # The maximum p-value of results to display. The range is 0.0 - 1.0
  correction = "None", # The error correction algorithm to use. Alternatively use "Benjamini Hochberg", "Bonferroni" or "None"
  filter = NULL, # An optional filter that some widgets accept. Use getWidgets for available filters of the respective enrichment widget.
  output = "xml" # output format which will be be processed to data.frame. Alternatively use "json".
) {
  
  # Assign the parameters of the enrichment query in a list
  queryEnrich = list(
    genelist = genelist,
    ids = ids,
    widget = widget,
    population = population,
    maxp = maxp,
    correction = correction,
    filter = filter,
    output = output
  ) 
  
  # Create enrichment query character string
  enq = ""
  
  # add list OR ids
  if(!is.null(queryEnrich[["genelist"]]) & is.null(queryEnrich[["ids"]])){
    enq = paste0("list=",queryEnrich[["genelist"]])
  } else if(is.null(queryEnrich[["genelist"]]) & !is.null(queryEnrich[["ids"]])){
    enq = paste0("ids=",queryEnrich[["ids"]])
  } else {
    stop("Set values for either list or ids in query")
  }
  
  # add widget
  enq = paste(enq, paste0("widget=",queryEnrich[["widget"]]), sep = "&")
  
  # add population
  if(!is.null(queryEnrich[["population"]])){
    enq = paste(enq, paste0("population=",queryEnrich[["population"]]), sep = "&")
  }
  
  # add maxp
  enq = paste(enq, paste0("maxp=",queryEnrich[["maxp"]]), sep = "&")
  
  # add error correction algorithm
  enq = paste(enq, paste0("correction=",queryEnrich[["correction"]]), sep = "&")
  
  # add filter
  if(!is.null(queryEnrich[["filter"]])){
    enq = paste(enq, paste0("filter=",queryEnrich[["filter"]]), sep = "&")
  }
  
  # add output
  enq = paste(enq, paste0("format=",queryEnrich[["output"]]), sep = "&")
  
  # percent-encode query string 
  enq.string = URLencode(enq)
  
  # keep the first part of mine url (e.g. http://www.flymine.org/)
  mine.url = substr(im$mine, start = 1, stop = gregexpr("/",im$mine)[[1]][length(gregexpr("/",im$mine)[[1]])])
  
  if(output == "xml") {
    # perform GET request
    r = GET(paste0(mine.url,"query/service/list/enrichment?",enq.string))
    
    # extract content from request
    res = content(r)
    res.xml <- xmlRoot(xmlParse(res))
    
    # convert xml results to data.frame
    if(length(getNodeSet(res.xml, "//result")) > 0){
      answer = xmlToDataFrame(res.xml, stringsAsFactors=FALSE)
    } else {
      # no results
      answer = NULL
    }
  
  } else if (output == "json"){
    # perform request and convert json results in data.frame with
    # jsonlite::fromJSON function
    r = jsonlite::fromJSON(txt = paste0(mine.url,"query/service/list/enrichment?",enq.string))
    
    if(length(r$results) > 0){
      # edit to be the same data.frame output as xml
      answer = r$results[,c(4,3,1,2)]
      colnames(answer) = c("identifier", "description", "pValue", "count")
    } else {
      # no results
      answer = NULL
    }
  }
  return(answer)
}
