#' @rdname InterMineR-class
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
