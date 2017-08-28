#' @import RCurl
#' @import XML
#' @import RJSONIO
#' @import sqldf
#' @import Biostrings
#' @importFrom igraph graph.data.frame
#' @importFrom igraph shortest.paths

#' @export
listMines = function(){
  # retrieve information from InterMine registry
  r = GET("http://registry.intermine.org/service/instances")
  stop_for_status(r)
  
  # get the url for every Mine
  res = httr::content(r)
  
  # urls = sapply(res$instances, function(x){x$url})
  urls = vapply(res$instances, function(x){x$url}, character(1))
  
  # assign Mine names to urls
  # names(urls) = sapply(res$instances, function(x){x$name})
  names(urls) = vapply(res$instances, function(x){x$name}, character(1))
  
  return(urls)
}

#' @export
##0 - Initilization
# initialize the base and token for future reuse
initInterMine <- function(mine = listMines()["HumanMine"], token=""){
  im <- list(mine = mine, token = token)
  im
}

#' @export
##1 - Version
#the implementation of the web service:
#<servlet-name>ws-version</servlet-name>
#<url-pattern>/service/version/*</url-pattern>
getVersion <- function(im, timeout=3){
  r <- GET(paste(im$mine, "/service/version", sep = ""))
  stop_for_status(r)
  v <- content(r)
  v$version
}

#' @export
getRelease <- function(im, timeout=3){
  r <- GET(paste(im$mine, "/service/version/release", sep = ""))
  stop_for_status(r)
  v <- content(r)
  v$version
}

#' @export
##2 - Model
#the implementation of the web service:
#<servlet-name>ws-model</servlet-name>
#<url-pattern>/service/model/*</url-pattern>
getModel <- function(im, timeout=3){
  r <- GET(paste(im$mine, "/service/model", sep=""))
  stop_for_status(r)
  model.string <- content(r, "text")
  model <- fromJSON(model.string)$model$classes
  res <- listModelSummary(model)
  res
}

###

listModelSummary <- function(model){
  class.name <- names(model)
  class.parent <- lapply(class.name, function(x) {
    y <- model[[x]][["extends"]]
    if(is.list(y)){
      y <- NA
    }
    y
  })
  
  # class.name <- rep(class.name, sapply(class.parent, length))
  class.name <- rep(class.name, vapply(class.parent, length, 1))
  
  class.parent <- unlist(class.parent)
  igr <- graph.data.frame(data.frame(
    parent=class.parent[which(!is.na(class.parent))],
    name=class.name[which(!is.na(class.parent))]),
    vertices=data.frame(unique(c(class.name,
                                 class.parent[which(!is.na(class.parent))]))))
  
  igr.sp <- shortest.paths(igr, mode="in")
  att <- lapply(class.name, 
                function(x){
                  data.frame(
                    do.call(rbind,model[[x]][["attributes"]]), 
                    stringsAsFactors=FALSE)
                })
  
  names(att) <- class.name
  
  att.ext <- rep(list(NULL), length(class.name))
  att.ext <- lapply(class.name, function(x){
    ext <- colnames(igr.sp)[which(is.finite(igr.sp[x, ]))]
    
    y <- unique(do.call(rbind, att[ext]))
    y <- cbind(class=rep(x, nrow(y)), y, stringsAsFactors=FALSE)
    colnames(y) <- c("type", "child_name", "child_type")
    y <- y[order(y$child_name),, drop=FALSE]
    rownames(y) <- NULL
    y
  })
  att.ext <- do.call(rbind, att.ext)
  att.ext$child_type <- ""
  rownames(att.ext) <- NULL
  
  # Error occuring when using HumanMine:
  # The fourth column of the att.ext variable is redundant and will prevent the
  # rbind(att.ext, ref.ext, col.ext) below!!!
  
  # columns 2 and 4 contain identical information for HumanMine!
  # all(tolower(att.ext[,2]) %in% gsub(" ", "", tolower(att.ext[,4])))
  
  # Therefore, we keep only the first 3 columns from the att.ext variable:
  att.ext = att.ext[,1:3]
  
  ref <- lapply(class.name, function(x) {
    y <- model[[x]][["references"]]
    if(length(y)==0){
      z <- data.frame(
        matrix(character(0), 0, 2, 
               dimnames=list(NULL,c("name", "referencedType")))
      )
    } else {
      z1 <- names(y)
      #z2 <- sapply(y, function(ye)
      #  ye[["referencedType"]])
      z2 <- vapply(y, function(ye){ye[["referencedType"]]}, character(1))
      z <- data.frame(name=z1, referencedType=z2)
    }
    z
  })
  names(ref) <- class.name
  
  ref.ext <- rep(list(NULL), length(class.name))
  
  ref.ext <- lapply(class.name, function(x) {
    ext <- colnames(igr.sp)[which(is.finite(igr.sp[x, ]))]
    y <- unique(do.call(rbind, ref[ext]))
    y <- cbind(class=rep(x, nrow(y)), y, stringsAsFactors=FALSE)
    colnames(y) <- c("type", "child_name", "child_type")
    y <- y[order(y$child_name),, drop=FALSE]
    rownames(y) <- NULL
    y
  })
  
  ref.ext <- do.call(rbind, ref.ext)
  rownames(att.ext) <- NULL
  
  col <- lapply(class.name, function(x) {
    y <- model[[x]][["collections"]]
    if(length(y)==0){
      z <- data.frame(
        matrix(character(0), 0, 2,
               dimnames=list(NULL,c("name", "referencedType")))
      )
    } else {
      z1 <- names(y)
      #z2 <- sapply(y, function(ye) ye[["referencedType"]])
      z2 <- vapply(y, function(ye) {ye[["referencedType"]]}, character(1))
      z <- data.frame(name=z1, referencedType=z2)
    }
    z
  })
  names(col) <- class.name
  
  col.ext <- rep(list(NULL), length(class.name))
  
  col.ext <- lapply(class.name, function(x) {
    ext <- colnames(igr.sp)[which(is.finite(igr.sp[x, ]))]
    y <- unique(do.call(rbind, col[ext]))
    y <- cbind(class=rep(x, nrow(y)), y, stringsAsFactors=FALSE)
    colnames(y) <- c("type", "child_name", "child_type")
    y <- y[order(y$child_name),, drop=FALSE]
    rownames(y) <- NULL
    y
  })
  col.ext <- do.call(rbind, col.ext)
  rownames(col.ext) <- NULL
  res <- rbind(att.ext, ref.ext, col.ext)
  rownames(res) <- NULL
  res <- sqldf("select * from res order by type, child_type")
  res
}
