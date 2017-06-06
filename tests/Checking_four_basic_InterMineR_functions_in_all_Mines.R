library(InterMineR)

# index variables: Iterate through all available mines and return TRUE/FALSE values for
# the call of each function
ind.getmodel = c()
ind.template = c()
ind.random.template.query.name = c()
ind.template.query = c()
ind.run.random.query = c()

# iterate through available Mines
for(i in seq(length(listMines()))){
  
  # load Mine
  im = initInterMine(mine = listMines()[i])
  
  # apply getModel
  model = try(getModel(im))
  
  # T/F for returning a model with getModel
  ind.getmodel = c(ind.getmodel, class(model) != "try-error")
  
  # apply getTemplates
  template = try(getTemplates(im))
  
  # T/F for returning available templates with getTemplate function
  ind.template = c(ind.template, class(template) != "try-error")
  
  
  if(class(template) != "try-error"){
    
    # Get gene-related templates
    gene.templates = template[grep("gene", template$name, ignore.case=TRUE),]
    
    # Get random gene-related query name
    gt = gene.templates[sample(seq(nrow(gene.templates)),1),1]
    
    # save random gene-related template query name
    ind.random.template.query.name = c(ind.random.template.query.name, gt)
    
    # Get random template query with getTemplate function
    random.query = try(getTemplateQuery(
      im,
      name = gt
    ))
    
    # T/F for returning random gene-related template query
    ind.template.query = c(ind.template.query, class(template) != "try-error")
    
    # Run  random gene-related query
    res.random.query = try(runQuery(im, qry = random.query))
    
    # T/F for running random gene-related query
    ind.run.random.query = c(ind.run.random.query, class(res.random.query) != "try-error")
    
  } else {
    
    ind.random.template.query.name = c(ind.random.template.query.name, "No_template")
    ind.template.query = c(ind.template.query, "No_template")
    ind.run.random.query = c(ind.run.random.query, "No_template")
    
  } 
  
}

# Get results for all four functions
functions_results = try(data.frame(Mines = listMines(), 
                               Returned_Model = ind.getmodel,
                               Returned_Templates = ind.template,
                               Random_Template_Query = ind.random.template.query.name,
                               Returned_Random_Gene_Template_Query = ind.template.query,
                               Ran_Successfully_Random_Gene_Template_Query = ind.run.random.query))

#try(functions_results)

# SessionInfo
sessionInfo()
