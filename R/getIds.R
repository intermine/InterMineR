# create function to generate comma-separated character string of InterMine object ids in the background
getIds = function(im, feature, values, organism = NULL){
  
  # optionally limit features (e.g. Gene, Protein, SNP) to a specific organism
  if(is.null(organism)){
    # set constraints
    constraints = setConstraints(
      paths = feature,
      operators = "LOOKUP",
      values = list(values)
    )
  } else {
    # set constraints
    constraints = setConstraints(
      paths = c(feature, paste0(feature,".organism.name")),
      operators = c("LOOKUP", "="),
      values = list(values, organism)
    )
  }
  
  # set query
  query = setQuery(
    select = c(paste0(feature,".id")),
    where = constraints
  )
  
  # get ids
  result = runQuery(
    im = im,
    query
  )
  
  # create comma-separated character string of ids
  string = paste(as.character(result[,1]),collapse = ",")
  
  return(string)
}
