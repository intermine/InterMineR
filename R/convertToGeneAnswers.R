convertToGeneAnswers = function(
  enrichmentResult,
  geneInput,
  geneInputType,
  geneExprProfile,
  annLib,
  categoryType,
  enrichCategoryChildName
){
  
  if(isNamespaceLoaded('GeneAnswers')){
    
    # get InterMine instance
    im = enrichmentResult$im
    
    # replace factors with characters
    j = sapply(geneInput, is.factor)
    geneInput[j] = lapply(geneInput[j], as.character)
    
    # check if expression profile exists
    if(missing(geneExprProfile)){
      geneExprProfile.value = NULL
    } else {
      geneExprProfile.value = geneExpProfile
    }
    
    # check if enrichCategoryChildName exist in widgets
    g = as.data.frame(getWidgets(enrichmentResult$im))
    
    if("enrichIdentifier" %in% colnames(g)){
      ind.widget = which(g$name == enrichmentResult$parameters['widget'])
      
      if(missing(enrichCategoryChildName) & is.na(g[ind.widget,"enrichIdentifier"])){
        
        stop("Default enrichIdentifier from getWidgets function is NA. 
             Assign manually enrichCategoryChildName with the appropriate enrichment category identifier.")
        
      } else if(missing(enrichCategoryChildName)){
        
        enrichCategoryChildName = paste(
          as.character(g[ind.widget, "targets"]),
          as.character(g[ind.widget,"enrichIdentifier"]),
          sep = "."
          )
        
      }
    } else if(!"enrichIdentifier" %in% colnames(g) & missing(enrichCategoryChildName)){
      
      stop("Default enrichIdentifier from getWidgets function is missing. 
           Assign manually enrichCategoryChildName with the appropriate enrichment category identifier.")
      
    }
    
    # set query for geneInput using type and categoryType (the latter defined by the user)
    # set constraints
    constraints = setConstraints(
      paths = "Gene",
      operators = "LOOKUP",
      values = list(as.character(geneInput[,1]))
    )
    
    # set query
    q = setQuery(
      select = c(geneInputType,
                 enrichCategoryChildName),
      where = constraints
    )
    
    # run query
    res = runQuery(
      im,
      qry = q
    )
    
    # construct genesInCategory list
    if(!any(enrichmentResult$data$identifier %in% res[,2])){
      stop('enrichmentResult identifiers do not match categoryType child_name! Check again the available identifiers with getModel(im).')
    }
    
    genesInCategory = list(NULL)
    for(i in seq(length(enrichmentResult$data$identifier))){
      
      # iterate through each identifier (category)
      id = enrichmentResult$data$identifier[i]
      # index
      ind.identifier = which(res[,2] == id)
      # save matching input values
      v = as.character(res[ind.identifier,1])
      
      genesInCategory[[i]] = v
    }
    
    # delete res
    rm(res)
    
    # assign the repective category name to each vector
    names(genesInCategory) = enrichmentResult$data$identifier
    
    # build enrichmentInfo data.frame
    percent_in_list = (as.numeric(enrichmentResult$data$count)/nrow(geneInput))
    percent_in_genome = (as.numeric(enrichmentResult$data$populationAnnotationCount)/
                           enrichmentResult$populationCount)
    
    p1 = percent_in_list/(1-percent_in_list)
    p2 = percent_in_genome/(1-percent_in_genome)
    
    odds_ratio = p1/p2
    
    # get p-values before correction algorithm
    for(p in c("genelist", "ids", "widget", "population", "maxp", "correction", "filter")){
      if(p %in% names(enrichmentResult$parameters)){
        assign(p, enrichmentResult$parameters[p])
      } else {
        assign(p, NULL)
      }
    }
    
    uncorrected_res = doEnrichment(
      im = enrichmentResult$im,
      genelist = genelist,
      ids = ids,
      widget = widget,
      population = population,
      maxp = as.numeric(maxp),
      correction = "None",
      filter = filter
    )
    
    initial_p_value = c()
    #go.test = c()
    for(j in seq(nrow(enrichmentResult$data))){
      
      ind.GO = which(uncorrected_res$data$identifier %in% enrichmentResult$data$identifier[j])
      
      initial_p_value = c(initial_p_value,
                          uncorrected_res$data$pValue[ind.GO])
      #go.test = c(go.test, 
      #            uncorrected_res$data$identifier[ind.GO])
    }
    
    enrichmentInfo = data.frame(
      as.numeric(enrichmentResult$data$count),
      percent_in_list,
      percent_in_genome,
      percent_in_list/percent_in_genome,
      odds_ratio,
      as.numeric(initial_p_value),
      as.numeric(enrichmentResult$data$pValue),
      stringsAsFactors = FALSE
    )
    
    names(enrichmentInfo) = c("genes in Category",
                              "percent in the observed List",
                              "percent in the genome",
                              "fold of overrepresents",
                              "odds ratio",
                              "p value",
                              "fdr p value")
    
    rownames(enrichmentInfo) = enrichmentResult$data$identifier
    
    answer = new(
      "GeneAnswers",
      geneInput = geneInput,
      testType = "hyperG",
      pvalueT = as.numeric(enrichmentResult$parameters["maxp"]),
      genesInCategory = genesInCategory,
      geneExprProfile = geneExprProfile.value,
      annLib = annLib,
      categoryType = categoryType,
      enrichmentInfo = enrichmentInfo
    )
    
  } else {
    stop("Install and load GeneAnswers package before running convertToGeneAnswers function!")
  }
  
}
