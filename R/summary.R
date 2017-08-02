#' @name summary
#' @rdname summary
#' @docType methods
#' @aliases summary summary,ANY-method summary,InterMineR-method
#' @export

# methods for InterMineR class
# summary

if (is.null(getGeneric("summary"))) setGeneric("summary", function(object,...){standardGeneric("summary")}))

# set summary method for class InterMineR
setMethod(
  "summary",
  signature(object = "InterMineR"),
  function(object,...){
    
    # create query.log data.frame
    l = list(NULL)
    count = 0
    
    for(j in seq(length(object@where))){
      
      x = object@where[[j]]
      
      if("value" %in% names(x)){
        if(length(x$value) > 1){
          x$value = paste(x$value, collapse = ",")
        }
        count = count + 1
        l[[count]] = data.frame(x[which(names(x) %in% c("path","op", "value", "code"))])
        
      }
    }
    
    return(do.call(rbind,l))
    
  }
)
