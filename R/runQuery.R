#' @name runQuery
#' @rdname runQuery
#' @docType methods
#' @aliases runQuery runQuery,ANY,InterMineR-method runQuery,ANY,list-method
#' @import S4Vectors
#' @import methods
#' @export

# methods for InterMineR class
# runQuery

setGeneric("runQuery",function(im, qry, timeout=60) standardGeneric("runQuery"))

#' @exportMethod runQuery

# set runQuery method for class InterMineR
setMethod(
  "runQuery",
  signature(qry = "InterMineR"),
  function(im, qry, timeout=60){

    # retrieve the length of value for each constraint
    value.length = c()
    constraints.with.values = c()

    for(i in seq(length(slot(qry,"where")))){

      # check if inherited constraints have value
      if("value" %in% names(slot(qry,"where")[[i]])){

        constraints.with.values = c(constraints.with.values, i)

        value.length = c(value.length,
                         length(slot(qry,"where")[[i]][["value"]])
        )
      }
    }

    # check if more than one constraints have multiple values
    if(sum(value.length > 1) > 1){
      stop("Only one of the query contraints can possess multiple values!")

      # check if one constraint has multiple values
    } else if(any(value.length > 1)){

      # identify contraint with multiple values
      #ind = constraints.with.values[which(value.length > 1)]
      ind = constraints.with.values[value.length > 1]

      answer.list = list(NULL)
      # iterate through multiple values
      for(y in seq(length(slot(qry,"where")[[ind]][["value"]]))){

        # get value
        v = slot(qry,"where")[[ind]][["value"]][y]

        # get XML query string
        query = InterMineR_Query2XML(qry, index = ind, value2 = v)

        # run InterMineR query
        query.unencoded <- toString.XMLNode(query)
        
        query.str <- URLencode(toString.XMLNode(query))
        query.str <- gsub("&", '%26', query.str)
        query.str <- gsub(";", '%3B', query.str)

        r <- GET(paste(im$mine, "/service/query/results?query=",
                       query.str,"&format=xml",sep=""))
        #If there's any HTTP error, print the query as well for easier debugging.
        stop_for_status(r, paste("query", query.unencoded))
        
        res <- content(r)
        res.xml <- xmlRoot(xmlParse(res))

        if (length(getNodeSet(res.xml, "//Result")) > 0) {
          answer = xmlToDataFrame(res.xml, stringsAsFactors=FALSE)
          colnames(answer) <- strsplit(xmlAttrs(query)[["view"]],
                                       "\\s+", perl=TRUE)[[1]]
        } else {
          # no results
          answer=NULL
        }

        # save in list
        answer.list[[y]] = answer

      }

      # rbind all results to data.frame and return
      answer.df = do.call(rbind, answer.list)

      return(answer.df)

    } else {

      # get XML query string
      query = InterMineR_Query2XML(qry)

      # run query
      query.str <- URLencode(toString.XMLNode(query))
      query.str <- gsub("&", '%26', query.str)
      query.str <- gsub(";", '%3B', query.str)

      r <- GET(paste(im$mine, "/service/query/results?query=",
                     query.str,"&format=xml",sep=""))
      stop_for_status(r)
      res <- content(r)
      res.xml <- xmlRoot(xmlParse(res))

      if (length(getNodeSet(res.xml, "//Result")) > 0) {
        answer = xmlToDataFrame(res.xml, stringsAsFactors=FALSE)
        colnames(answer) <- strsplit(xmlAttrs(query)[["view"]],
                                     "\\s+", perl=TRUE)[[1]]
      } else {
        # no results
        answer=NULL
      }

      # return answer
      return(answer)

    }
  }
)

# set runQuery method for class list
setMethod(
  "runQuery",
  signature(qry = "list"),
  function(im, qry, timeout=60){

    if (is.list(qry)) {
      # convert to XML to run in intermine
      query <- queryList2XML(qry)
    } else if(isXMLString(qry)) {
      query <- xmlParseString(qry)
    }

    answer <- NULL

    query.unencoded <- toString.XMLNode(query)

    query.str <- URLencode(query.unencoded)
    query.str <- gsub("&", '%26', query.str)
    query.str <- gsub(";", '%3B', query.str)

    r <- GET(paste(im$mine, "/service/query/results?query=",
                   query.str,"&format=xml",sep=""))

    #If there's any HTTP error, print the query as well for easier debugging.
    stop_for_status(r, paste("query", query.unencoded))

    res <- content(r)
    res.xml <- xmlRoot(xmlParse(res))

    if (length(getNodeSet(res.xml, "//Result")) > 0) {
      answer = xmlToDataFrame(res.xml, stringsAsFactors=FALSE)
      colnames(answer) <- strsplit(xmlAttrs(query)[["view"]],
                                   "\\s+", perl=TRUE)[[1]]
    } else {
      # no results
      answer=NULL
    }

    return(answer)

  }
)

