#' @rdname InterMineR-methods
#' @import S4Vectors
#' @import methods

# 1. extract name from InterMineR-class

#' @export
#' @aliases getName-methods getName,InterMineR-method

setGeneric("getName", function(object,...){
  standardGeneric("getName")
})

#' @exportMethod getName
setMethod(
  "getName",
  signature(object = "InterMineR"),
  function(object,...){
    object@name
  }
)

# 2. extract description from InterMineR-class

#' @export
#' @aliases getDescription-methods getDescription,InterMineR-method

setGeneric("getDescription", function(object,...){
  standardGeneric("getDescription")
})

#' @exportMethod getDescription
setMethod(
  "getDescription",
  signature(object = "InterMineR"),
  function(object,...){
    object@description
  }
)

# 3. extract select from InterMineR-class

#' @export
#' @aliases getSelect-methods getSelect,InterMineR-method

setGeneric("getSelect", function(object,...){
  standardGeneric("getSelect")
})

#' @exportMethod getSelect
setMethod(
  "getSelect",
  signature(object = "InterMineR"),
  function(object,...){
    object@select
  }
)

# 4. extract getOrderBy from InterMineR-class

#' @export
#' @aliases getOrderBy-methods getOrderBy,InterMineR-method

setGeneric("getOrderBy", function(object,...){
  standardGeneric("getOrderBy")
})

#' @exportMethod getOrderBy
setMethod(
  "getOrderBy",
  signature(object = "InterMineR"),
  function(object,...){
    object@orderBy
  }
)

# 5. extract getWhere from InterMineR-class

#' @export
#' @aliases getWhere-methods getWhere,InterMineR-method

setGeneric("getWhere", function(object,...){
  standardGeneric("getWhere")
})

#' @exportMethod getWhere
setMethod(
  "getWhere",
  signature(object = "InterMineR"),
  function(object,...){
    object@where
  }
)
