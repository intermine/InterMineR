#' @import IRanges
#' @import GenomicRanges
#' @export

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
      
      #ind.seqnames = which(colnames(dataset) %in% seqnames)
      ind.seqnames = colnames(dataset) %in% seqnames
      
      seqnames.vector = dataset[,ind.seqnames]
      
    } else if(length(seqnames) == nrow(dataset)){
      
      seqnames.vector = seqnames
      
    } else {
      stop("assign seqnames with a column name or a character vector of length 
           equal to the number of dataset rows")
    }
  } else {
    stop("assign seqnames with a character value or vector")
  }
  
  #check if chromosomes are chromosome.primaryIdentifiers returned by InterMine
  if(seqnamesInterMineChromosome){
    seqnames.vector = paste0("chr", seqnames.vector)
  }
  
  ###
  
  if(length(start) == 1 & class(start) == "character"){
    
    #ind.start = which(colnames(dataset) %in% start)
    ind.start = colnames(dataset) %in% start
    
    start.vector = as.numeric(dataset[,ind.start])
    
  } else if(length(start) == nrow(dataset)){
    start.vector = as.numeric(start)
  } else {
    stop("assign start with a column name or a vector of length equal to the 
         number of dataset rows")
  }
  
  ###
  
  if(length(end) == 1 & class(end) == "character"){
    
    #ind.end = which(colnames(dataset) %in% end)
    ind.end = colnames(dataset) %in% end
    
    end.vector = as.numeric(dataset[,ind.end])
    
  } else if(length(end) == nrow(dataset)){
    end.vector = as.numeric(end)
  } else {
    stop("assign end with a column name or a vector of length equal to the 
         number of dataset rows")
  }
  
  ###
  
  # names must be of class character
  if(class(names) == "character"){
    
    # get values for names
    # check if names is assigned with the name of a column
    if(length(names) == 1){
      
      #ind.names = which(colnames(dataset) %in% names)
      ind.names = colnames(dataset) %in% names
      
      names.vector = dataset[,ind.names]
      
    } else if(length(names) == nrow(dataset)){
      
      names.vector = names
      
    } else {
      stop("assign names with a column name or a character vector of length 
           equal to the dataset rows")
    }
  } else {
    stop("assign names with a character value or vector")
  }
  
  ###
  
  # strand must be of class character
  if(class(strand) == "character"){
    
    # get values for strand
    # check if strand is assigned with the name of a column
    if(length(strand) == 1){
      
      #ind.strand = which(colnames(dataset) %in% strand)
      ind.strand = colnames(dataset) %in% strand
      
      strand.vector = dataset[,ind.strand]
      
    } else if(length(strand) == nrow(dataset)){
      
      strand.vector = strand
      
    } else {
      stop("assign strand with a column name or a character vector of length 
           equal to the dataset rows")
    }
  } else {
    stop("assign strand with a character value or vector")
  }
  
  # replace strand values 1, -1 and "" with +, - and * respectively
  # if(any(c("1","-1","") %in% strand.vector)){
  
  strand.vector = gsub("-1", "-", strand.vector)
  strand.vector = gsub("1", "+", strand.vector)
  strand.vector[!strand.vector %in% c("+","-")] = "*"
  
  # }
  
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
      
      #ind.columnsAsMetadata = which(colnames(dataset) %in% columnsAsMetadata)
      ind.columnsAsMetadata = colnames(dataset) %in% columnsAsMetadata
      d = dataset[,ind.columnsAsMetadata]
      names(d) = columnsAsMetadata
      
      metadata.vector1 = d
      
    } else {
      stop("assign columnsAsMetadata with column names from the dataset")
    }
  }
  
  # assign metadata
  # use list with vectors
  
  if(!is.null(listAsMetadata)){
    
    if(all(lapply(listAsMetadata, length) == nrow(dataset))){
      
      metadata.vector2 = do.call(cbind, listAsMetadata)
      
    } else {
      stop("assign listAsMetadata with a list of vectors that have length 
           equal to the number of dataset rows")
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
