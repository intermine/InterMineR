#' @export
getWidgets = function(im){
  
  mine.url = im$mine
  # make GET request
  r = GET(paste0(mine.url, "/service/widgets?format=json"))
  stop_for_status(r)
  
  # format with content
  mine.script = content(r, "text")
  
  # RJSONIO::fromJSON instead of jsonlite::fromJSON (interferes with getModel)
  res = fromJSON(mine.script)
  
  # get widget results and convert rirectly to R objects with 
  # res = jsonlite::fromJSON(txt = paste0(mine.url, 
  # "query/service/widgets?format=json"))
  
  # format to data.frame:
  t = res$widgets
  
  if(length(t) == 0){
    res2 = NULL
    return(res2)
  } else {
    # get all unique names
    ind.names = c()
    for(i in seq(length(t))){
      ind.names = c(ind.names, names(t[[i]]))
    }
    
    ind.names = unique(ind.names)
    
    # empty list
    l = as.list(seq(length(t)))
    
    # iterate through res list
    for(j in seq(length(t))){
      
      l.append = c()
      
      for(y in seq(length(ind.names))){
        
        n = ind.names[y]
        item = unlist(t[[j]][n])
        
        if(is.null(item)){
          l.append = c(l.append, NA)
          names(l.append)[y] = n
        } else if(length(item) >= 2) {
          l.append = c(l.append, paste(item, collapse = " & "))
          names(l.append)[y] = n
        } else {
          l.append = c(l.append,item)
          names(l.append)[y] = n
        }
      }
      
      l[[j]] = l.append
      
    }
    
    # rbind lists to data.frame
    res2 = do.call(rbind, l)
    
    return(res2)
  }
}
