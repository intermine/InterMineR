#' @rdname InterMineR_query
#' @export
setClass(
  "InterMineR_query",
  representation(
    name = "character",
    description = "character",
    select = "character",
    orderBy = "list",
    where = "list"
  )
)
