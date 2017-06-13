# function for retrieving Gene.id
getGeneIds = function(im, genes, organism){
  
  # Define new query to look for Gene.id
  queryGeneId = newQuery()
  
  # set columns
  queryGeneId$select = c(
    "Gene.primaryIdentifier",
    "Gene.secondaryIdentifier",
    "Gene.symbol",
    "Gene.id"
  )
  
  # set sort order
  queryGeneId$orderBy = list(c(Gene.secondaryIdentifier = "ASC"))
  
  # set organism constraint
  organismConstraint = list(
    path = "Gene.organism.name",
    op = "=",
    value = organism,
    code = "B"
  )
  
  # save genes with unique results
  unique_results = list(NULL)
  ind.unique = c()
  
  # save genes with multiple results
  multiple_results = list(NULL)
  ind.multiple = c()
  
  # save index of genes with no Gene.id result
  ind.no_results = c()
  
  # iterate through the input genes
  for(j in seq(length(genes))){
    
    # set Gene constraint
    geneConstraint = list(
      path = "Gene",
      op = "LOOKUP",
      value = genes[j],
      code = "A"
    )
    
    # assign constraints
    queryGeneId$where = list(geneConstraint, organismConstraint)
    
    # run query
    res = runQuery(im, queryGeneId)
    
    # if res is NULL or Gene.id is missing
    if(is.null(res)){
      
      ind.no_results = c(ind.no_results, j)
      
    } else if(nrow(res) > 1) { # save in multiple results
      
      multiple_results[[j]] = res
      ind.multiple = c(ind.multiple, j)
      
    } else if(nrow(res) == 1){ # save in unique results
      
      g = res$Gene.id
      
      if(g == ""){
        
        ind.no_results = c(ind.no_results, j)
        
      } else {
        
        unique_results[[j]] = res
        ind.unique = c(ind.unique, j)
        
      }
    }
  }
  
  # create data.frame of unique results
  res2 = do.call(rbind,unique_results[ind.unique])
  
  # create string for doEnrichment() function with Gene.id column
  doEnrichment.string = paste(res2$Gene.id, collapse = ",")
  
  # return all results
  results = list(
    unique.results = res2,
    doEnrichment.string = doEnrichment.string,
    multiple.results = do.call(rbind, multiple_results[ind.multiple]),
    genes.with.no.results = genes[ind.no_results]
  )
  
  print(paste0(as.character(length(ind.multiple)), " genes have returned multiple results."))
  print(paste0(as.character(length(ind.no_results)), " genes have returned no results."))
  
  return(results)
}
