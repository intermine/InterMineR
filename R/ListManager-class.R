#' @rdname webservice-class
#' @import S4Vectors
#' @import methods
#' @export

setClass(
  "ListManager",
  representation(
    DEFAULT_LIST_NAME = "character",
    DEFAULT_DESCRIPTION = "character",
    
    INTERSECTION_PATH = "character",
    UNION_PATH = "character",
    DIFFERENCE_PATH = "character",
    SUBTRACTION_PATH = "character",
    mine = "character",
    token = "character"
  )
)

