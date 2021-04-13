#' @import RCurl
#' @import httr
#' @import XML
#' @import xml2
#' @import RJSONIO
#' @import Biostrings

#' @export
##3 - Template
#<servlet-name>ws-template</servlet-name>
#<url-pattern>/service/templates/*</url-pattern>
getTemplates <- function(im, format="data.frame", timeout=3) {
  # JSON
  if (format == "list") {
    r <- GET(paste(im@mine, "/service/templates?format=json", sep=""))
    stop_for_status(r)
    template.string <- content(r, "text")
    res <- fromJSON(template.string)$templates
    res
    # XML
  } else {
    r <- GET(paste(im@mine, "/service/templates?format=xml", sep=""))
    stop_for_status(r)
    template <- content(r)
    res <- listTemplateSummary(template)
    res
  }
}

###

listTemplateSummary <- function(template) {
  doc <- xmlTreeParse(template)
  r <- xmlRoot(doc)
  template.attr <- xmlApply(r, xmlAttrs)
  template.df <- do.call(rbind, template.attr)
  rownames(template.df) <- NULL
  data.frame(template.df[,c(1,2)], stringsAsFactors=FALSE)
}

#' @export
getTemplateQuery <- function(im, name, timeout=3){
  r <- GET(paste(im@mine, "/service/templates/", name, "?format=json", sep=""))
  stop_for_status(r)
  ql <- httr::content(r, "text")
  jsonTemplate <- fromJSON(ql)$template
  jsonTemplate
}

###

##4 - Query
#<servlet-name>ws-query-results</servlet-name>
#<url-pattern>/service/query/results</url-pattern>
#runQuery

#' @export
newQuery <- function(name="", view=character(), sortOrder="", longDescription="",
                     constraintLogic=NULL) {
  nq <- list()
  nq[["name"]] <- name
  nq[["select"]] <- paste(view,collapse=" ")
  nq[["description"]] <- longDescription
  nq[["orderBy"]] <- sortOrder
  nq[["where"]] <- NULL
  nq[["constraintLogic"]] <- constraintLogic
  
  nq
}

###

queryList2XML <- function(ql){
  
  nq <- newXMLNode("query")
  xmlAttrs(nq)[["name"]] <- ql$name
  xmlAttrs(nq)[["model"]] <- "genomic"
  xmlAttrs(nq)[["view"]] <- paste(ql$select,collapse=" ")
  
  if(!is.null(ql$description)){
    xmlAttrs(nq)[["longDescription"]] <- ql$description
  }
  
  if(!is.null(ql$orderBy)){
    orderByString = paste(names(ql$orderBy[[1]]), ql$orderBy, collapse=" ")
    xmlAttrs(nq)[["sortOrder"]] <- orderByString
  }
  
  if(!is.null(ql$where)){
    for(i in 1:length(ql[["where"]])){
      cnc <- newXMLNode("constraint")
      xmlAttrs(cnc)[["path"]] <- ql[["where"]][[i]][["path"]]
      if (!is.null(ql[["where"]][[i]][["type"]])) {
        xmlAttrs(cnc)[["type"]] <- ql[["where"]][[i]][["type"]]
      }
      # query constraints on TYPE don't have these attributes
      # so skip them. Should test for NULL instead.
      if (is.null(ql[["where"]][[i]][["type"]])) {
        # value
        if(!is.null(ql[["where"]][[i]][["value"]])){
          xmlAttrs(cnc)[["value"]] <- ql[["where"]][[i]][["value"]]
        }
        # loopPath
        if(!is.null(ql[["where"]][[i]][["loopPath"]])){
          xmlAttrs(cnc)[["loopPath"]] <- ql[["where"]][[i]][["loopPath"]]
        }
        # code
        xmlAttrs(cnc)[["code"]] <- paste(ql[["where"]][[i]][["code"]],
                                         collapse=" ")
        # op
        xmlAttrs(cnc)[["op"]] <- paste(ql[["where"]][[i]][["op"]],
                                       collapse=" ")
        # extraValue
        xmlAttrs(cnc)[["extraValue"]] = paste(
          ql[["where"]][[i]][["extraValue"]], collapse=" ")
      }
      addChildren(nq, kids=list(cnc), at=xmlSize(nq))
    }
  }
  
  if(!is.null(ql$constraintLogic)){
    xmlAttrs(nq)[["constraintLogic"]] <- ql$constraintLogic
  }
  
  nq
}
