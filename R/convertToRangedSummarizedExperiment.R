# Define function for converting InterMine experimental data to RangedSummarizedExperiment
# Dataset argument takes data.frame object as input

convertToRangedSummarizedExperiment = function(
  dataset,
  SampleColumn,
  GeneColumn,
  ValueColumn,
  OrganismValue,
  colsForSampleMetadata
){
  
  # get index for SampleColumn and GeneColumn
  if(class(SampleColumn) %in% c("integer", "numeric")){
    sc = SampleColumn
  } else if (class(SampleColumn) == "character"){
    sc = which(colnames(dataset) == SampleColumn)
  }
  
  if(class(GeneColumn) %in% c("integer", "numeric")){
    gc = GeneColumn
  } else if (class(GeneColumn) == "character"){
    gc = which(colnames(dataset) == GeneColumn)
  }
  
  if(class(ValueColumn) %in% c("integer", "numeric")){
    vc = ValueColumn
  } else if (class(ValueColumn) == "character"){
    vc = which(colnames(dataset) == ValueColumn)
  }
  
  # make sample names unique for each gene
  unique.sample.names = c()
  for(j in unique(dataset[,gc])){
    
    # index for samples per gene
    ind.gene = dataset[,gc] == j
    
    # assign suffixes to make sample names unique
    unique.sample.names = c(unique.sample.names,
           make.unique(dataset[ind.gene,sc])
    )
  }
  
  dataset[,sc] = unique.sample.names
  
  # get count.table for RangedSummarizedExperiment object with the reshape function
  count.table = reshape(
    data = dataset[,c(gc, sc, vc)],
    idvar = colnames(dataset)[sc],
    timevar = colnames(dataset)[gc],
    direction = "wide"
  )
  
  # set sample names in rows
  rownames(count.table) = count.table[,1]
  count.table = count.table[,2:ncol(count.table)]
  
  # set gene names in columns
  colnames(count.table) = unique(dataset[,gc])
  
  # convert count.table NA values to zero and all values to class numeric
  for(i in seq(ncol(count.table))){
    ind = is.na(count.table[,i])
    count.table[ind,i] = "0"
    count.table[,i] = as.numeric(count.table[,i])
  }
  
  # reverse count.table to set samples in columns and genes in rows!
  count.table = t(count.table)
  
  # create GRanges object for the rowRanges of RangedSummarizedExperiment object
  # create new InterMineR query to retrieve the exons of each gene
  # the results will be converted to GRanges and subsequently to GRangesList object
  # for rowRanges
  
  # define new query
  geneExonQuery = newQuery()
  
  # set columns for the query
  geneExonQuery$select = c(
    "Gene.exons.chromosomeLocation.start",
    "Gene.exons.chromosomeLocation.end",
    "Gene.exons.chromosomeLocation.strand",
    "Gene.exons.chromosome.primaryIdentifier",
    "Gene.symbol",
    "Gene.secondaryIdentifier" 
  )
  
  # set sort order
  geneExonQuery$orderBy = list(
    c(Gene.secondaryIdentifier = "ASC")
  )
  
  # set organism constraint
  organismConstraint = list(
    path = "Gene.organism.name",
    op = "=",
    value = OrganismValue,
    code = "B"
  )
  
  l.exons = list(NULL)
  ind.names = c()
  for(j in seq(length(unique(dataset[,gc])))){
    
    g = unique(dataset[,gc])[j]
    
    # set gene constraint
    geneConstraint = list(
      path = 'Gene',
      op = "LOOKUP",
      value = g,
      code = "A"
    )
    
    # set constraints
    geneExonQuery$where = list(
      geneConstraint,
      organismConstraint
    )
    
    # run query
    d = runQuery(im.fly, geneExonQuery)
    
    # convert exon results to GRanges object
    d2 = convertToGRanges(
      dataset = d,
      seqnames = "Gene.exons.chromosome.primaryIdentifier",
      start = "Gene.exons.chromosomeLocation.start",
      end = "Gene.exons.chromosomeLocation.end",
      strand = "Gene.exons.chromosomeLocation.strand",
      names = "Gene.exons.chromosome.primaryIdentifier",
      columnsAsMetadata = c("Gene.symbol","Gene.secondaryIdentifier")
    )
    
    l.exons[[j]] = d2
    ind.names = c(ind.names, g)
  }
  
  # convert to GRangesList
  l.exons = GRangesList(l.exons)
  names(l.exons) = ind.names
  
  # create DataFrame object for colData of RangedSummarizedExperiment object
  colMetaData = list(NULL)
  
  for(i in colsForSampleMetadata){
    
    j = which(colsForSampleMetadata == i)
    
    colMetaData[[j]] = tapply(
      X = dataset[,i],
      INDEX = dataset[,sc],
      function(x){
        
        if(length(unique(x)) == 1){
          y = unique(x)
        } else {
          y = paste0(unique(x), collapse = " & ")
        }
        
        return(y)
      }
    )
    
  }
  
  # cbind data to data.frame
  colMetaData = do.call(cbind, colMetaData)
  
  # change column names
  colnames(colMetaData) = colnames(dataset)[colsForSampleMetadata]
  
  # set the rows of colMetaData to the same order as the columns of count.table
  ind.colMetaData = c()
  for(i in seq(ncol(count.table))){
    
    ind.colMetaData = c(ind.colMetaData, 
                        which(rownames(colMetaData) == colnames(count.table)[i])
                        )
    
  }
  
  colMetaData = colMetaData[ind.colMetaData,]
  
  # convert to DataFrame class
  colMetaData = DataFrame(colMetaData)
  
  # create RangedSummarizedExperiment object
  result = SummarizedExperiment(
    assays = list(counts = count.table),
    rowRanges = l.exons,
    colData = colMetaData
  )
  
  return(result)
  
}
