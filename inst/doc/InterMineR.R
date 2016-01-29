### R code from vignette source 'InterMineR.Rnw'

###################################################
### code chunk number 1: InterMineR.Rnw:28-29
###################################################
options(continue="  ")


###################################################
### code chunk number 2: InterMineR.Rnw:52-53
###################################################
options(width=108)


###################################################
### code chunk number 3: InterMineR.Rnw:60-62
###################################################
library(InterMineR)
listMines()


###################################################
### code chunk number 4: InterMineR.Rnw:67-69
###################################################
im <- initInterMine(mine=listMines()["HumanMine"])
im


###################################################
### code chunk number 5: InterMineR.Rnw:75-77
###################################################
template <- getTemplates(im)
head(template)


###################################################
### code chunk number 6: InterMineR.Rnw:82-83
###################################################
template[grep("gene", template$name, ignore.case=T),]


###################################################
### code chunk number 7: InterMineR.Rnw:88-90
###################################################
queryGeneLoc <- getTemplateQuery(im, "Gene_Location")
queryGeneLoc


###################################################
### code chunk number 8: InterMineR.Rnw:105-107
###################################################
model <- getModel(im)
head(model)


###################################################
### code chunk number 9: InterMineR.Rnw:112-113
###################################################
model[which(model$type=="Gene"),]


###################################################
### code chunk number 10: InterMineR.Rnw:118-119
###################################################
model[which(model$type=="Location"),]


###################################################
### code chunk number 11: InterMineR.Rnw:126-128
###################################################
resGeneLoc <- runQuery(im, queryGeneLoc)
resGeneLoc


###################################################
### code chunk number 12: InterMineR.Rnw:135-139
###################################################
queryGeneLoc$constraints[1, "value"]="ABO"
queryGeneLoc$constraints
resGeneLoc <- runQuery(im, queryGeneLoc)
resGeneLoc


###################################################
### code chunk number 13: InterMineR.Rnw:145-146
###################################################
model[which(model$type=="Organism"),]


###################################################
### code chunk number 14: InterMineR.Rnw:151-155
###################################################
queryGeneLoc$view <- c(queryGeneLoc$view, "Gene.organism.name")
queryGeneLoc$view
resGeneLoc <- runQuery(im, queryGeneLoc)
resGeneLoc


###################################################
### code chunk number 15: InterMineR.Rnw:160-165
###################################################
newConstraint <- c("Gene.organism.name", "=", "Homo sapiens", "B", "")
queryGeneLoc$constraints <- rbind(queryGeneLoc$constraints, newConstraint)
queryGeneLoc$constraints
resGeneLoc <- runQuery(im, queryGeneLoc)
resGeneLoc


###################################################
### code chunk number 16: InterMineR.Rnw:170-174
###################################################
queryGeneLoc$constraintLogic <- "A and B"
queryGeneLoc$constraintLogic
resGeneLoc <- runQuery(im, queryGeneLoc)
resGeneLoc


###################################################
### code chunk number 17: InterMineR.Rnw:183-187
###################################################
queryGeneSeq <- getTemplateQuery(im, "Gene_Location")
queryGeneSeq$constraints[1, "value"]="ABO"
newConstraint <- c("Gene.organism.name", "=", "Homo sapiens", "B", "")
queryGeneSeq$constraints <- rbind(queryGeneSeq$constraints, newConstraint)


###################################################
### code chunk number 18: InterMineR.Rnw:192-193
###################################################
queryGeneSeq$view <- c("Gene.symbol")


###################################################
### code chunk number 19: InterMineR.Rnw:198-200
###################################################
resGeneSeq <- runQuery(im, queryGeneSeq, format="sequence")
resGeneSeq


###################################################
### code chunk number 20: InterMineR.Rnw:208-213
###################################################
queryGeneLoc <- getTemplateQuery(im, "Gene_Location")
queryGeneLoc$constraints[1, "value"]="ABO"
newConstraint <- c("Gene.organism.name", "=", "Homo sapiens", "B", "")
queryGeneLoc$constraints <- rbind(queryGeneLoc$constraints, newConstraint)
resGeneLoc <- runQuery(im, queryGeneLoc)


###################################################
### code chunk number 21: InterMineR.Rnw:218-219
###################################################
queryNeighborGene <- newQuery()


###################################################
### code chunk number 22: InterMineR.Rnw:224-227
###################################################
queryNeighborGene$view <- c("Gene.primaryIdentifier", "Gene.symbol", "Gene.chromosome.primaryIdentifier", 
                            "Gene.locations.start", "Gene.locations.end", "Gene.locations.strand")
queryNeighborGene$view


###################################################
### code chunk number 23: InterMineR.Rnw:232-242
###################################################
newConstraint1 <- c("Gene.chromosome.primaryIdentifier", "=", 
                    resGeneLoc[1, "Gene.chromosome.primaryIdentifier"], "A", "")
newConstraint2 <- c("Gene.locations.start", ">=", 
                    as.numeric(resGeneLoc[1, "Gene.locations.start"])-50000, "B", "")
newConstraint3 <- c("Gene.locations.end", "<=", 
                    as.numeric(resGeneLoc[1, "Gene.locations.end"])+50000, "C", "")
newConstraint4 <- c("Gene.organism.name", "=", "Homo sapiens", "D", "")
queryNeighborGene$constraints <- rbind(queryNeighborGene$constraints, 
                                       newConstraint1, newConstraint2, newConstraint3, newConstraint4)
queryNeighborGene$constraints


###################################################
### code chunk number 24: InterMineR.Rnw:247-249
###################################################
queryNeighborGene$sortOrder <- "Gene.locations.start asc"
queryNeighborGene$sortOrder


###################################################
### code chunk number 25: InterMineR.Rnw:254-256
###################################################
resNeighborGene <- runQuery(im, queryNeighborGene)
resNeighborGene


###################################################
### code chunk number 26: X
###################################################
resNeighborGene$Gene.locations.strand[which(resNeighborGene$Gene.locations.strand==1)]="+"
resNeighborGene$Gene.locations.strand[which(resNeighborGene$Gene.locations.strand==-1)]="-"
gene.idx <- which(nchar(resNeighborGene$Gene.symbol)==0)
resNeighborGene$Gene.symbol[gene.idx]=resNeighborGene$Gene.primaryIdentifier[gene.idx]
#resNeighborGene
require(Gviz)
annTrack <- AnnotationTrack(start=resNeighborGene$Gene.locations.start, 
                            end=resNeighborGene$Gene.locations.end, 
                            strand=resNeighborGene$Gene.locations.strand, 
                            chromosome=resNeighborGene$Gene.chromosome.primaryIdentifier[1],
                            genome="hg19", name="around ABO",id=resNeighborGene$Gene.symbol)
#annTrack


###################################################
### code chunk number 27: InterMineR.Rnw:277-278
###################################################
plotTracks(annTrack, shape="box", showFeatureId=T, fontcolor="black")


###################################################
### code chunk number 28: InterMineR.Rnw:286-288
###################################################
queryGeneGO <- getTemplateQuery(im, "Gene_GO")
queryGeneGO


###################################################
### code chunk number 29: InterMineR.Rnw:293-295
###################################################
queryGeneGO$view <- queryGeneGO$view[2:5]
queryGeneGO$view


###################################################
### code chunk number 30: InterMineR.Rnw:300-302
###################################################
queryGeneGO$constraints[1, "value"]="ABO"
queryGeneGO$constraints


###################################################
### code chunk number 31: InterMineR.Rnw:307-309
###################################################
resGeneGO <- runQuery(im, queryGeneGO)
resGeneGO


###################################################
### code chunk number 32: InterMineR.Rnw:318-320
###################################################
queryGOGene <- getTemplateQuery(im, "GOterm_Gene")
queryGOGene


###################################################
### code chunk number 33: InterMineR.Rnw:325-327
###################################################
queryGOGene$view <- queryGOGene$view[2:5]
queryGOGene$view


###################################################
### code chunk number 34: InterMineR.Rnw:332-334
###################################################
queryGOGene$constraints[1, "value"]="metal ion binding"
queryGOGene$constraints


###################################################
### code chunk number 35: InterMineR.Rnw:339-341
###################################################
resGOGene <- runQuery(im, queryGOGene)
head(resGOGene)


###################################################
### code chunk number 36: InterMineR.Rnw:354-356
###################################################
sessionInfo()
warnings()


