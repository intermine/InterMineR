#' @export
setGeneric("doEnrichment", function(object,...){
  standardGeneric("doEnrichment")
})

#' @rdname webservice-methods
#' @exportMethod doEnrichment
setMethod(
  "doEnrichment",
  signature(object = "Service"),
  function(object,
           genelist = NULL,
           ids = NULL,
           widget = NULL,
           population = NULL,
           maxp = 0.05,
           correction = "Benjamini Hochberg",
           filter = NULL,
           organism = NULL){
    
    if(!is.null(ids) & length(ids)>1){
      
      # get defined Mine's widgets
      g = as.data.frame(getWidgets(list(object@mine,object@token)))
      # index with the defined widget
      #ind.widget = which(g$name == widget)
      ind.widget = g$name == widget
      # get feature (e.g. "Gene", "Protein", "SNP")
      feature = as.character(g$targets[ind.widget])
      
      # run getIds function in the background to get the InterMine object ids
      # as comma-separated character string
      string = getIds(im = list(object@mine,object@token),
                      feature = feature,
                      values = ids,
                      organism = organism)
      
    } else if(!is.null(ids) & length(ids)==1){
      # assign directly the comma-separated string of ids if user has defined so
      string = ids
    } else {
      string = NULL
    }
    
    # Assign the parameters of the enrichment query in a list
    queryEnrich = list(
      genelist = genelist,
      ids = string, #ids,
      widget = widget,
      population = population,
      maxp = maxp,
      correction = correction,
      filter = filter,
      output = "xml"
    ) 
    
    # Create enrichment query character string
    enq = ""
    
    # add list OR ids
    if(!is.null(queryEnrich[["genelist"]]) & is.null(queryEnrich[["ids"]])){
      enq = paste0("list=",queryEnrich[["genelist"]])
    } else if(is.null(queryEnrich[["genelist"]])& !is.null(queryEnrich[["ids"]])){
      enq = paste0("ids=",queryEnrich[["ids"]])
    } else {
      stop("Set values for either list or ids in query")
    }
    
    # add widget
    enq = paste(enq, paste0("widget=",queryEnrich[["widget"]]), sep = "&")
    
    # add population
    if(!is.null(queryEnrich[["population"]])){
      enq = paste(enq, 
                  paste0("population=",queryEnrich[["population"]]), 
                  sep = "&")
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
    
    mine.url = object@mine
    
    # if(output == "xml") {
    
    # perform GET request
    r = GET(paste0(mine.url,"/service/list/enrichment?",enq.string),
            add_headers(Authorization = paste("Token",object@token, sep = " ")))
    stop_for_status(r)
    
    # extract content from request
    res = httr::content(r)
    res.xml <- xmlRoot(xmlParse(res))
    
    # get populationCount and notAnalysed values from xml attributes
    #ind.populationCount = which(names(xmlAttrs(res.xml)) == "populationCount")
    #ind.notAnalysed = which(names(xmlAttrs(res.xml)) == "notAnalysed")
    ind.populationCount = names(xmlAttrs(res.xml)) == "populationCount"
    ind.notAnalysed = names(xmlAttrs(res.xml)) == "notAnalysed"
    
    
    # convert xml results to data.frame
    if(length(getNodeSet(res.xml, "//result")) > 0){
      answer = xmlToDataFrame(res.xml, stringsAsFactors=FALSE)
    } else {
      # no results
      answer = NULL
    }
    
    # store parameters
    parameters = c(
      genelist = genelist,
      ids = string, #ids,
      widget = widget,
      population = population,
      maxp = maxp,
      correction = correction,
      filter = filter
    )
    
    #if(length(ind.populationCount) == 0 & length(ind.notAnalysed) == 0){
    if(sum(ind.populationCount) == 0 & sum(ind.notAnalysed) == 0){
      answer = list(
        data = answer,
        im = list(object@mine,object@token),
        parameters = parameters)
    } else {
      answer = list(
        data = answer,
        populationCount = as.numeric(xmlAttrs(res.xml)[ind.populationCount]),
        notAnalysed = as.numeric(xmlAttrs(res.xml)[ind.notAnalysed]),
        im = list(object@mine,object@token),
        parameters = parameters)
    }
    
    # } else if (output == "json"){
    #  # perform request and convert json results in data.frame with
    #  # jsonlite::fromJSON function
    #  # Set xml as default because jsonlite interferes with RJSONIO!
    #  r = jsonlite::fromJSON(txt = paste0(mine.url,"/service/list/enrichment?",
    #  enq.string))
    #  
    #  if(length(r$results) > 0){
    #    # edit to be the same data.frame output as xml
    #    answer = r$results[,c(4,3,1,2)]
    #    colnames(answer) = c("identifier", "description", "pValue", "count")
    #  } else {
    #    # no results
    #    answer = NULL
    #  }
    # }
    
    return(answer)
  }
)

######EXAMPLE#######
# HumanMine
im.human = initInterMine(listMines()["HumanMine"],"F16793D0k4BaF5hbe3s0") #change here to your token
class(im.human)
#introduce the name of a genelist that you have created or not
enrichResults.HumanMine <- doEnrichment(im.human,genelist = "copy_of_intersected",widget = "go_enrichment_for_gene") 

#' @rdname webservice-methods
#' @exportMethod list_manager
setMethod(
  "list_manager",
  signature(object = "Service"),
  function(object,...){
    return(new("ListManager",
               DEFAULT_LIST_NAME = 'my_list',
               DEFAULT_DESCRIPTION = 'List created with Python client library',
               
               INTERSECTION_PATH = '/lists/intersect/json',
               UNION_PATH = '/lists/union/json',
               DIFFERENCE_PATH = '/lists/diff/json',
               SUBTRACTION_PATH = '/lists/subtract/json',
               mine = object@mine,
               token = object@token))
    
})
