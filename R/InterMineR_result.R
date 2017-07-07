#' @rdname InterMineR_result
#' @export
setClass(
  "InterMineR_result",
  representation(
    result = "data.frame",
    constraints = "data.frame"
  )
)
