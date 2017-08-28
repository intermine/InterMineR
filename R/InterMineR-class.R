#' @rdname InterMineR-class
#' @import S4Vectors
#' @import methods
#' @export

setClass(
  "InterMineR",
  representation(
    name = "character",
    description = "character",
    select = "character",
    orderBy = "list",
    where = "list"
  )
)
