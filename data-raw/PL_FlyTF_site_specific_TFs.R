PL_FlyTF_site_specific_TFs = read.csv("PL_FlyTF_site_specific_TFs.csv", 
                             header = TRUE)
                        
organism.column = c()
for(i in seq(nrow(PL_FlyTF_site_specific_TFs))){
  
  organism.column = c(organism.column,
                      paste(PL_FlyTF_site_specific_TFs[i,5],
                            PL_FlyTF_site_specific_TFs[i,6],
                            collapse = " ")
  )
}

PL_FlyTF_site_specific_TFs = PL_FlyTF_site_specific_TFs[,-c(5,6)]
PL_FlyTF_site_specific_TFs = cbind(PL_FlyTF_site_specific_TFs, organism.column)

colnames(PL_FlyTF_site_specific_TFs) = c("Index",
                                "Gene.secondaryIdentifier",
                                "Gene.symbol",
                                "Gene.primaryIdentifier",
                                "Gene.organism.name")

save(PL_FlyTF_site_specific_TFs, file = "data/PL_FlyTF_site_specific_TFs.rdata")
                             
