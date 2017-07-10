# define function for creating the XML query string from an InterMineR_query object
# ql is assigned with the object of 'InterMineR_query' class
# index points to the constraint that has multiple values!
# value2 is assigned with one of the multiple values through iteration

InterMineR_Query2XML <- function(ql, index, value2){
  nq <- newXMLNode("query")
  xmlAttrs(nq)[["name"]] <- ql@name
  xmlAttrs(nq)[["model"]] <- "genomic"
  xmlAttrs(nq)[["view"]] <- paste(ql@select,collapse=" ")
  if(!is.null(ql@description)){
    xmlAttrs(nq)[["longDescription"]] <- ql@description
  }
  if(!is.null(ql@orderBy)){
    orderByString = paste(names(ql@orderBy[[1]]), ql@orderBy, collapse=" ")
    xmlAttrs(nq)[["sortOrder"]] <- orderByString
  }
  
  if(!is.null(ql@where)){
    for(i in 1:length(ql@where)){
      cnc <- newXMLNode("constraint")
      xmlAttrs(cnc)[["path"]] <- ql@where[[i]][["path"]]
      if (!is.null(ql@where[[i]][["type"]])) {
        xmlAttrs(cnc)[["type"]] <- ql@where[[i]][["type"]]
      }
      # query constraints on TYPE don't have these attributes
      # so skip them. Should test for NULL instead.
      if (is.null(ql@where[[i]][["type"]])) {
        
        # check if index exists
        if(!missing(index)){
          # identify which constraint has multiple values
          if(i == index){
            # assign one value at a time
            xmlAttrs(cnc)[["value"]] <- value2
          } else {
            xmlAttrs(cnc)[["value"]] <- ql@where[[i]][["value"]]
          }
        } else {
          xmlAttrs(cnc)[["value"]] <- ql@where[[i]][["value"]]
        }
        
        
        xmlAttrs(cnc)[["code"]] <- paste(ql@where[[i]][["code"]],collapse=" ")
        xmlAttrs(cnc)[["op"]] <- paste(ql@where[[i]][["op"]],collapse=" ")
        xmlAttrs(cnc)[["extraValue"]] <- paste(ql@where[[i]][["extraValue"]],collapse=" ")
      }
      addChildren(nq, kids=list(cnc), at=xmlSize(nq))
    }
  }
  
  #if(!is.null(ql@constraintLogic)){
  #  xmlAttrs(nq)[["constraintLogic"]] <- ql@constraintLogic
  #}
  
  return(nq)
}
