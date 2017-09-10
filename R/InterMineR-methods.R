#' @name InterMineR-methods
#' @aliases getName-methods
#' @aliases getName,InterMineR-method
#' @aliases getDescription-methods
#' @aliases getDescription,InterMineR-method
#' @aliases getSelect-methods
#' @aliases getSelect,InterMineR-method
#' @aliases getOrderBy-methods
#' @aliases getOrderBy,InterMineR-method
#' @aliases getWhere-methods
#' @aliases getWhere,InterMineR-method

#' @rdname InterMineR-methods
#' @export
# 1. extract name from InterMineR-class
setGeneric("getName", function(object,...){
  standardGeneric("getName")
})

#' @rdname InterMineR-methods
#' @exportMethod getName
setMethod(
  "getName",
  signature(object = "InterMineR"),
  function(object,...){
    object@name
  }
)

#' @rdname InterMineR-methods
#' @export
# 2. extract description from InterMineR-class
setGeneric("getDescription", function(object,...){
  standardGeneric("getDescription")
})

#' @rdname InterMineR-methods
#' @exportMethod getDescription
setMethod(
  "getDescription",
  signature(object = "InterMineR"),
  function(object,...){
    object@description
  }
)

#' @rdname InterMineR-methods
#' @export
# 3. extract select from InterMineR-class
setGeneric("getSelect", function(object,...){
  standardGeneric("getSelect")
})

#' @rdname InterMineR-methods
#' @exportMethod getSelect
setMethod(
  "getSelect",
  signature(object = "InterMineR"),
  function(object,...){
    object@select
  }
)

#' @rdname InterMineR-methods
#' @export
# 4. extract getOrderBy from InterMineR-class
setGeneric("getOrderBy", function(object,...){
  standardGeneric("getOrderBy")
})

#' @rdname InterMineR-methods
#' @exportMethod getOrderBy
setMethod(
  "getOrderBy",
  signature(object = "InterMineR"),
  function(object,...){
    object@orderBy
  }
)

#' @rdname InterMineR-methods
#' @export
# 5. extract getWhere from InterMineR-class
setGeneric("getWhere", function(object,...){
  standardGeneric("getWhere")
})

#' @rdname InterMineR-methods
#' @exportMethod getWhere
setMethod(
  "getWhere",
  signature(object = "InterMineR"),
  function(object,...){
    object@where
  }
)
