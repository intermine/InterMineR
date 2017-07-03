# Define function for convertion of InterMineR objects to GRanges
convertToGRanges = function(
  dataset,
  seqnames,
  start, 
  end,
  names,
  strand,
  columnsAsMetadata = NULL,
  listAsMetadata = NULL,
  seqnamesInterMineChromosome = TRUE
){
  
  ###
  
  # seqnames must be of class character
  if(class(seqnames) == "character"){
    
    # get values for seqnames
    # check if seqnames is assigned with the name of a column
    if(length(seqnames) == 1){
      
      ind.seqnames = which(colnames(dataset) %in% seqnames)
      
      seqnames.vector = dataset[,ind.seqnames]
      
    } else if(length(seqnames) == nrow(dataset)){
      
      seqnames.vector = seqnames
      
    } else {
      stop("seqnames argument must be assigned with the name of column from the dataset or with a character vector with length equal to the rows of the dataset")
    }
  } else {
    stop("assign seqnames with a value or vector of the character class")
  }
  
  # check if chromosomes are chromosome.primaryIdentifiers returned by InterMineR
  if(seqnamesInterMineChromosome){
    seqnames.vector = paste0("chr", seqnames.vector)
  }
  
  ###
  
  if(length(start) == 1 & class(start) == "character"){
    
    ind.start = which(colnames(dataset) %in% start)
    start.vector = as.numeric(dataset[,ind.start])
    
  } else if(length(start) == nrow(dataset)){
    start.vector = as.numeric(start)
  } else {
    stop("start argument must be assigned with the name of a column from the dataset or with a vector with length equal to the number of rows of the dataset")
  }
  
  ###
  
  if(length(end) == 1 & class(end) == "character"){
    
    ind.end = which(colnames(dataset) %in% end)
    end.vector = as.numeric(dataset[,ind.end])
    
  } else if(length(end) == nrow(dataset)){
    end.vector = as.numeric(end)
  } else {
    stop("end argument must be assigned with the name of a column from the dataset or with a vector with length equal to the number of rows of the dataset")
  }
  
  ###
  
  # names must be of class character
  if(class(names) == "character"){
    
    # get values for names
    # check if names is assigned with the name of a column
    if(length(names) == 1){
      
      ind.names = which(colnames(dataset) %in% names)
      names.vector = dataset[,ind.names]
      
    } else if(length(names) == nrow(dataset)){
      
      names.vector = names
      
    } else {
      stop("names argument must be assigned with the name of column from the dataset or with a character vector with length equal to the rows of the dataset")
    }
  } else {
    stop("assign names with a value or vector of the class character")
  }
  
  ###
  
  # strand must be of class character
  if(class(strand) == "character"){
    
    # get values for strand
    # check if strand is assigned with the name of a column
    if(length(strand) == 1){
      
      ind.strand = which(colnames(dataset) %in% strand)
      strand.vector = dataset[,ind.strand]
      
    } else if(length(strand) == nrow(dataset)){
      
      strand.vector = strand
      
    } else {
      stop("strand argument must be assigned with the name of column from the dataset or with a character vector with length equal to the rows of the dataset")
    }
  } else {
    stop("assign strand with a value or vector of the class character")
  }
  
  # replace strand values 1, -1 and "" with +, - and * respectively
  if(any(c("1","-1","") %in% strand.vector)){
    
    strand.vector = gsub("-1", "-", strand.vector)
    strand.vector = gsub("1", "+", strand.vector)
    strand.vector[!strand.vector %in% c("+","-")] = "*"
    
  }
  
  # check if strand values are "+", "-" or "*"
  if(!all(strand.vector %in% c("+", "-", "*"))){
    stop('strand values must be "+", "-" or "*"')
  }
  
  ###
  
  # create GRanges object
  res = GRanges(
    seqnames = seqnames.vector,
    ranges = IRanges(
      start = start.vector,
      end = end.vector,
      names = names.vector
    ),
    strand = strand.vector
  )
  
  ###
  
  metadata.vector1 = NULL
  metadata.vector2 = NULL
  
  # assign metadata
  # use dataset column names
  if(!is.null(columnsAsMetadata)){
    if(all(columnsAsMetadata %in% colnames(dataset))){
      
      ind.columnsAsMetadata = which(colnames(dataset) %in% columnsAsMetadata)
      d = dataset[,ind.columnsAsMetadata]
      names(d) = columnsAsMetadata
      
      metadata.vector1 = d
      
    } else {
      stop("assign columnsAsMetadata with names of columns from the dataset")
    }
  }
  
  # assign metadata
  # use list with vectors
  
  if(!is.null(listAsMetadata)){
    
    if(all(lapply(listAsMetadata, length) == nrow(dataset))){
      
      metadata.vector2 = do.call(cbind, listAsMetadata)
      
    } else {
      stop("assign listAsMetadata with a list of vectors that have length equal to the number of rows of the dataset")
    }
  }
  
  if(!is.null(metadata.vector1) & is.null(metadata.vector2)){
    mcols(res) = metadata.vector1
  } else if(!is.null(metadata.vector2) & is.null(metadata.vector1)){
    mcols(res) = metadata.vector2
  } else if(!is.null(metadata.vector1) & !is.null(metadata.vector2)){
    mcols(res) = cbind(metadata.vector1, metadata.vector2)
  }
  
  return(res)
  
}
