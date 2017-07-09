# Define function for setting constraints for 'InterMineR_query'
setConstraints = function(
  paths,
  operators,
  values,
  modifyQueryConstraints,
  m.index
){
  
  # check if modifyQueryConstraints has been assigned
  if(missing(modifyQueryConstraints)){
    
    # check if all arguments have the same length
    length.arguments = c(
      length(paths),
      length(operators),
      length(values))
    
    if(length(unique(length.arguments)) != 1){
      stop("All arguments in setConstraints() must have the same length")
    }
    
    # check paths, operators and values
    if(class(paths) != "character"){
      stop("paths argument must be of the class character")
    }
    if(class(operators) != "character"){
      stop("operators argument must be of the class character")
    }
    if(class(values) != "list"){
      stop("values argument must be of the class list")
    }
    
    #check the length of its object of the argument values (list)
    length.values = sapply(values, length)
    
    if(sum(length.values > 1) > 1){
      stop("Within the values list only one object can be of length greater than one!")
    }
    
    where.result = list(NULL)
    # iterate through argument values
    for(j in seq(unique(length.arguments))){
      where.result[[j]] = list(
        path = paths[j],
        op = operators[j],
        value = values[[j]],
        code = LETTERS[j])
    }
    
    return(where.result)
    
  } else {
    
    # check if m.index exists and is of the right class
    if(missing(m.index) | !class(m.index)%in%c("numeric", "integer")){
      stop("assign m.index argument with a numeric or integer vector")
    }
    
    # check if m.index is less than the legth of modifyQueryConstraints query constraints
    if(length(modifyQueryConstraints$where)<m.index){
      stop("m.index value can not be greater than the length of the constraints, which are to be modified")
    }
    
    where.result = modifyQueryConstraints
    
    # check each argument and replace the appropriate constraint if it exists
    if(!missing(paths)){
      
      if(class(paths) != "character"){
        stop("paths argument must be of the class character")
      }
      
      for(i in seq(length(paths))){
        where.result$where[[m.index[i]]]$path = paths[i]
      }
    }
    
    if(!missing(operators)){
      
      if(class(operators) != "character"){
        stop("operators argument must be of the class character")
      }
      
      for(i in seq(length(operators))){
        where.result$where[[m.index[i]]]$op = operators[i]
      }
    }
    
    if(!missing(values)){
      
      if(class(values) != "list"){
        stop("values argument must be of the class list")
      }
      
      for(i in seq(length(values))){
        where.result$where[[m.index[i]]]$value = values[[i]]
      }
    }
    
    return(where.result$where)
  }
}
