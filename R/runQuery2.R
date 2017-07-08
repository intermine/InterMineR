# Define function for running queries using 'InterMineR_query' objects
# The results are objects of 'InterMineR_result' class

runQuery2 = function(
  im,
  qry,
  timeout=60
){
  
  if(class(qry) == "InterMineR_query"){
    
    # retrieve the length of value for each constraint
    value.length = c()
    constraints.with.values = c()
    
    for(i in seq(length(qry@where))){
      # check if inherited constraints have value
      if("value" %in% names(qry@where[[i]])){
        
        constraints.with.values = c(constraints.with.values, i)
        
        value.length = c(value.length, 
                         length(qry@where[[i]]$value)
        )
      }
    }
    
    # check if more than one constraints have multiple values
    if(sum(value.length > 1) > 1){
      stop("only one of the query contraints can possess multiple values")
      
      # check if one constraint has multiple values
    } else if(any(value.length > 1)){
      
      # identify contraint with multiple values
      ind = constraints.with.values[which(value.length > 1)]
      
      answer.list = list(NULL)
      # iterate through multiple values
      for(y in seq(length(qry@where[[ind]]$value))){
        
        # get value
        v = qry@where[[ind]]$value[y]
        
        # get XML query string
        query = InterMineR_Query2XML(qry, index = ind, value2 = v)
        
        # run InterMineR query
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
        
        # save in list
        answer.list[[y]] = answer
        
      }
      
      # rbind all results to data.frame
      answer.df = do.call(rbind, answer.list)
      
      # create query.log data.frame
      l = list(NULL)
      count = 0
      for(j in seq(length(qry@where))){
        
        x = qry@where[[j]]
        
        if("value" %in% names(x)){
          if(length(x$value) > 1){
            x$value = paste(x$value, collapse = ",")
          }
          count = count + 1
          l[[count]] = data.frame(x[which(names(x) %in% c("path","op", "value", "code"))])
          
        }
      }
      
      
      # set and return object of the formal class InterMineR_result
      answer.final = new(
        "InterMineR_result",
        result = answer.df,
        constraints = plyr::rbind.fill(l)
      )
      
      return(answer.final)
      
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
      
      # create query.log data.frame
      l = list(NULL)
      count = 0
      for(j in seq(length(qry@where))){
        
        x = qry@where[[j]]
        
        if("value" %in% names(x)){
          if(length(x$value) > 1){
            x$value = paste(x$value, collapse = ",")
          }
          count = count + 1
          l[[count]] = data.frame(x[which(names(x) %in% c("path","op", "value", "code"))])
          
        }
      }
      
      # set and return object of the formal class InterMineR_result
      answer.final = new(
        "InterMineR_result",
        result = answer,
        constraints = plyr::rbind.fill(l)
      )
      
      return(answer.final)
      
    }
    
  } else {
    stop("qry argument must be assigned with an object of class 'InterMineR_query'")
  }
}
