PL_DiabetesGenes = read.csv("PL_DiabetesGenes.csv", header = FALSE)

colnames(PL_DiabetesGenes) = c("Gene.symbol",
                                "Gene.name",
                                "Gene.primaryIdentifier",
                                "Gene.secondaryIdentifier",
                                "Gene.length",
                                "Gene.organism.name")

save(PL_DiabetesGenes, file = "data/PL_DiabetesGenes.rdata")
