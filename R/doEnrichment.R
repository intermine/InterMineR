doEnrichment = function(
  im,
  genelist = NULL,
  ids = NULL,
  widget = NULL,
  population = NULL,
  maxp = 0.05,
  correction = "Holm-Bonferroni",
  filter = NULL,
  output = "xml"
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
  
  mine.url = im$mine
  
  if(output == "xml") {
    # perform GET request
    r = GET(paste0(mine.url,"/service/list/enrichment?",enq.string))
    stop_for_status(r)
    
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
    # Set xml as default because jsonlite interferes with RJSONIO!
    # r = jsonlite::fromJSON(txt = paste0(mine.url,"/service/list/enrichment?",enq.string))
    r = fromJSON(txt = paste0(mine.url,"/service/list/enrichment?",enq.string))
    
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
