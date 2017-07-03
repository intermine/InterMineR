getDatasets = function(im, 
                       type, 
                       child_name, 
                       value,
                       op){
  
  queryDatasets = newQuery()
  
  # set columns
  queryDatasets$select = c(
    paste0(type, c(".dataSets.name",
                   ".dataSets.description",
                   ".dataSets.url")
           )
  )
  
  # set sort order
  sortOrder = "ASC"
  names(sortOrder) = paste0(type, ".dataSets.name")
  
  queryDatasets$orderBy = list(sortOrder)
  
  # set constraints
  if(op == "LOOKUP"){
    constraint = list(
      path = type,
      op = op,
      value = value,
      code = "A"
    )
  } else {
    constraint = list(
      path = paste(type, child_name, sep = "."),
      op = op,
      value = value,
      code = "A"
    )
  }
  
  queryDatasets$where = list(constraint)
  
  # run query
  res = runQuery(im, queryDatasets)
  
}
