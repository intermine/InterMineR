#' @export
# define function to flatter the values of a column into comma-separated 
# strings based
# on another column, which is used as index
simplifyResult = function(
  dataset,
  index_column,
  values_column,
  returnInDataframe = FALSE
) {
  
  # get index for columns
  # for index
  #if(class(index_column) %in% c("integer", "numeric")){
  if(is.integer(index_column) | is.numeric(index_column)){
    index = index_column
  } else {
    #index = which(colnames(dataset) == index_column)
    index = colnames(dataset) == index_column
  }
  
  # for values 
  #if(class(values_column) %in% c("integer", "numeric")){
  if(is.integer(values_column) | is.numeric(values_column)){
    values = values_column
  } else {
    #values = which(colnames(dataset) == values_column)
    values = colnames(dataset) == values_column
  }
  
  # collapse to comma-separated string
  simplified_results = tapply(
    X = dataset[,values],
    INDEX = dataset[,index],
    FUN = function(x){
      paste(x, collapse = ",")
    }
  )
  
  if(returnInDataframe){
    
    res2 = c()
    for(i in seq(nrow(dataset))){
      ind = which(rownames(simplified_results) %in% dataset[i,index])
      res2 = c(res2, simplified_results[ind])
    }
    
    dataset$simplified_results = res2
    
    return(dataset)
    
  } else {
    return(data.frame(simplified_results))
  }
}
