#' @rdname webservice-class
#' @import S4Vectors
#' @import methods
#' @export

setClass(
  "Service",
  representation(
    mine = "character",
    token = "character"
  )
)

