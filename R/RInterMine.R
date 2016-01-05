# require(RCurl)
# require(XML)
# require(RJSONIO)
# require(sqldf)
# require(igraph)
#require(Biostrings)


listMines <- function(){
      mines <- c('http://www.flymine.org/flymine',              
              'http://www.mousemine.org/mousemine',
              'http://ratmine.mcw.edu/ratmine',
              'http://www.wormbase.org/tools/wormmine',
              'http://yeastmine.yeastgenome.org/yeastmine',
              'http://zebrafishmine.org',
              'http://targetmine.mizuguchilab.org/targetmine',
              'http://mitominer.mrc-mbu.cam.ac.uk/release-3.1',
              'http://www.humanmine.org/humanmine',
              'http://www.cbrc.kaust.edu.sa/indigo',
              'https://apps.araport.org/thalemine',
              'http://medicmine.jcvi.org/medicmine',
              'http://phytozome.jgi.doe.gov/phytomine') 
        
      names(mines) <- c('FlyMine',
        'MouseMine',
        'RatMine',
        'WormMine',
        'YeastMine',
        'ZebraFishMine',
        'TargetMine',
        'MitoMiner',
        'HumanMine',
        'indigoMine',
        'thalemine',
        'medicmine',
        'PhytoMine')
      mines
}


##0 - Initilization
# initialize the base and token for future reuse
initInterMine <- function(mine = listMines()["HumanMine"], token=""){
      im <- list(mine = mine, token = token)
      im
}


##1 - Version
#the implementation of the web service:
#<servlet-name>ws-version</servlet-name>
#<url-pattern>/service/version/*</url-pattern>
getVersion <- function(im, timeout=3){
      v <- getURL(paste(im$mine, "/service/version", sep=""), .opts = list(timeout = timeout))
     
      if(length(grep("^\\[ERROR\\]", v))>0){
            print(v)
            return(NULL)
      }
      as.integer(gsub("\n","",v))
}

getRelease <- function(im, timeout=3){
      v <- getURL(paste(im$mine, "/service/version/release", sep=""), .opts = list(timeout = timeout))
      if(length(grep("^\\[ERROR\\]", v))>0){
            print(v)
            return(NULL)
      }
    
      gsub("\n","",v)
}


##2 - Model
#the implementation of the web service:
#<servlet-name>ws-model</servlet-name>
#<url-pattern>/service/model/*</url-pattern>
getModel <- function(im, timeout=3){
      model.string <- getURL(paste(im$mine, "/service/model/json", sep=""), .opts = list(timeout = timeout))
      if(length(grep("^\\[ERROR\\]", model.string))>0){
            print(model.string)
            return(NULL)
      }
      model <- fromJSON(model.string)$model$classes
      
      res <- listModelSummary(model)
  
     
#     if(format=="json"){
#            model.string <- getURL(paste(im$mine, "/service/model/json", sep=""), .opts = list(timeout = timeout))

#            res <- fromJSON(model.string)$model$classes
            
            
#       }else if(format=="xml"){
#             model.string <- getURL(paste(im$mine, "/service/model/xml", sep=""), .opts = list(timeout = timeout))
#             res <- xmlParse(model.string)
#      }
    
      res
}

listModelSummary <- function(model){
      class.name <- names(model)
      class.parent <- lapply(class.name, function(x) {
            y <- model[[x]][["extends"]]
            if(is.list(y)){
                  y <- NA
            }
            y
      })
      
      class.name <- rep(class.name, sapply(class.parent, length))
      class.parent <- unlist(class.parent)
     
     
      igr <- graph.data.frame(data.frame(
          parent=class.parent[which(!is.na(class.parent))], 
          name=class.name[which(!is.na(class.parent))]), vertices=data.frame(unique(c(class.name, class.parent[which(!is.na(class.parent))]))))
                              
      igr.sp <- shortest.paths(igr, mode="in")
    
    
      att <- lapply(class.name, function(x) data.frame(do.call(rbind, model[[x]][["attributes"]]), stringsAsFactors=F))
      names(att) <- class.name
      
      att.ext <- rep(list(NULL), length(class.name))
      att.ext <- lapply(class.name, function(x){
            ext <- colnames(igr.sp)[which(is.finite(igr.sp[x, ]))]
            
            y <- unique(do.call(rbind, att[ext]))
            y <- cbind(class=rep(x, nrow(y)), y, stringsAsFactors=F)
            colnames(y) <- c("type", "child_name", "child_type")

            
            
            y <- y[with(y, order(child_name)),]   
            rownames(y) <- NULL
            y
            
      })
      att.ext <- do.call(rbind, att.ext)
      att.ext$child_type <- ""
      rownames(att.ext) <- NULL
      
     
      ref <- lapply(class.name, function(x){
            y <- model[[x]][["references"]]
            if(length(y)==0){
                  z <- data.frame(matrix(character(0), 0, 2,dimnames=list(NULL, c("name", "referencedType"))))
            }else{
                  z1 <- names(y)
                  z2 <- sapply(y, function(ye) ye[["referencedType"]])
                  z <- data.frame(name=z1, referencedType=z2)
            }
            z
      })
      names(ref) <- class.name
      
      ref.ext <- rep(list(NULL), length(class.name))
      
      ref.ext <- lapply(class.name, function(x){
          ext <- colnames(igr.sp)[which(is.finite(igr.sp[x, ]))]
          y <- unique(do.call(rbind, ref[ext]))

          y <- cbind(class=rep(x, nrow(y)), y, stringsAsFactors=F)
          colnames(y) <- c("type", "child_name", "child_type")
          y <- y[with(y, order(child_name)),]   
          rownames(y) <- NULL
          y
          
      })
      
      ref.ext <- do.call(rbind, ref.ext)
      rownames(att.ext) <- NULL
      
      col <- lapply(class.name, function(x){
            y <- model[[x]][["collections"]]
            if(length(y)==0){
                  z <- data.frame(matrix(character(0), 0, 2,dimnames=list(NULL, c("name", "referencedType"))))
            }else{
                  z1 <- names(y)
                  z2 <- sapply(y, function(ye) ye[["referencedType"]])
                  z <- data.frame(name=z1, referencedType=z2)
            }
            z
      })
      names(col) <- class.name
      
      col.ext <- rep(list(NULL), length(class.name))
      
      col.ext <- lapply(class.name, function(x){
            ext <- colnames(igr.sp)[which(is.finite(igr.sp[x, ]))]
            y <- unique(do.call(rbind, col[ext]))
            y <- cbind(class=rep(x, nrow(y)), y, stringsAsFactors=F)
            colnames(y) <- c("type", "child_name", "child_type")
            y <- y[with(y, order(child_name)),]   
            rownames(y) <- NULL
            y
            
      })
      col.ext <- do.call(rbind, col.ext)
      rownames(col.ext) <- NULL
    
    
      #res <- lapply(class.name, function(x){
      #    list(attributes=att.ext[[x]], 
               #extended.attribute=att.ext[[x]],
      #         references=ref.ext[[x]], 
               #extended.reference=ref.ext[[x]],
      #         collections=col.ext[[x]])
               #extended.collection=col.ext[[x]])
      #})
     
      res <- rbind(att.ext, ref.ext, col.ext)
      
      rownames(res) <- NULL
      res <- sqldf("select * from res order by type, child_type")

     
      res
}


##3 - Template
#<servlet-name>ws-template</servlet-name>
#<url-pattern>/service/templates/*</url-pattern>
getTemplates <- function(im, format="data.frame", timeout=3){
      if(format=="data.frame"){
            template.string <- getURL(paste(im$mine, "/service/templates/xml", sep=""), .opts = list(timeout = timeout))
            if(length(grep("^\\[ERROR\\]", template.string))>0){
                  print(template.string)
                  return(NULL)
            }
            template <- xmlParse(template.string)
            res <- listTemplateSummary(template)
      }else if(format == "list"){
            template.string <- getURL(paste(im$mine, "/service/templates/json", sep=""), .opts = list(timeout = timeout))
            if(length(grep("^\\[ERROR\\]", template.string))>0){
                  print(template.string)
                  return(NULL)
            }
            res <- fromJSON(template.string)$templates
        
      }
    
      res
}

listTemplateSummary <- function(template){
      template.attr <- xmlApply(xmlRoot(template), xmlAttrs)
      template.df <- do.call(rbind, template.attr)
      rownames(template.df) <- NULL
      data.frame(template.df[,c(1,2)], stringsAsFactors=F)
}

getTemplateQuery <- function(im, name, timeout=3){
      template <- getTemplates(im, "list")
      
      ql <- template[[name]]
      ql$constraints <- do.call(rbind, ql$constraints)
      if(!("extraValue" %in% colnames(ql$constraints))){
            
            ql$constraints <- cbind(ql$constraints, extraValue=rep("", nrow(ql$constraints)))
      }

      ql
}


##4 - Query
#<servlet-name>ws-query-results</servlet-name>
#<url-pattern>/service/query/results</url-pattern>
runQuery <- function(im, qry, format="data.frame", timeout=60){
      if(is.list(qry)){
            query <- queryList2XML(qry)
      }else if(isXMLString(qry)){
            query <- xmlParseString(qry)
      }
    
      answer <- NULL
#       if(format=="xml"){
#             res <- getURL(URLencode(paste(im$mine, "/service/query/results?query=", 
#                                           toString.XMLNode(query),"&format=xml",sep="")),
#                           ssl.verifypeer = FALSE, useragent = "R", timeout=timeout
#                           )
#             
#             res.xml <- xmlRoot(xmlParse(res))
#             
#             answer <- res.xml
#       }
      if(format=="data.frame"){
            query.str <- URLencode(toString.XMLNode(query))
            query.str <- gsub("&", '%26', query.str)
            query.str <- gsub(";", '%3B', query.str)
            
            res <- getURL(paste(im$mine, "/service/query/results?query=", 
                        query.str,"&format=xml",sep=""),
                        ssl.verifypeer = FALSE, useragent = "R", timeout=timeout
                  )
            if(length(grep("^\\[ERROR\\]", res))>0){
                  print(res)
                  return(NULL)
            }
            res.xml <- xmlRoot(xmlParse(res))
      
            if(names(res.xml[1])=="error"){
                  answer = NULL
            }else{
                  if(length(getNodeSet(res.xml, "//Result"))>0){  
                        answer= xmlToDataFrame(res, stringsAsFactors=F)
                        colnames(answer) <- strsplit(xmlAttrs(query)[["view"]],"\\s+", perl=T)[[1]]
                  }
                  else{
                        answer=NULL
                  }
            }
      }
      else if(format=="sequence"){
            query.str <- URLencode(toString.XMLNode(query))
            query.str <- gsub("&", '%26', query.str)
            query.str <- gsub(";", '%3B', query.str)
            res <- getURL(URLencode(paste(im$mine, "/service/query/results/fasta?query=", 
                                          toString.XMLNode(query),sep="")),
                          ssl.verifypeer = FALSE, useragent = "R", timeout=timeout
                          )
            if(length(grep("^\\[ERROR\\]", res))>0){
                  print(res)
                  return(NULL)
            }
            lines <- strsplit(res,'\\n')
            
            idx <- grep('>', lines[[1]])
            
            dset <- NULL
            if(length(idx) > 0){
                  seq <- character(length(idx))
                  idx <- c(idx, length(lines[[1]])+1)
                  for(i in 1:(length(idx)-1)){
                        seq[i] <- paste(lines[[1]][(idx[i]+1):(idx[i+1]-1)], collapse="")
                  }
                  dset <- BStringSet(seq)
                  names(dset) <- gsub("^>", "", lines[[1]][idx[-length(idx)]])
            }
            answer <- dset
            
      }

      answer
}


queryXML2List <- function(qx){
      qxl <- xmlToList(xmlParseString(qx))
      ql <- newQuery()
      
#       ql$name <- xmlAttrs(qx)["name"][1]
#       ql$view <- strsplit(xmlAttrs(qx)["view"], "\\s+", perl=T)[[1]]
#       ql$description <- xmlAttrs(qx)["longDescription"][1]
#       
#       ql$sortOrder <- xmlAttrs(qx)["sortOrder"][1]
#       ql$constraints <- do.call(rbind, lapply(getNodeSet(qx, "//constraint"), xmlAttrs))
#       ql$constraintLogic <- xmlAttrs(qx)["constraintLogic"][1]
      
      ql$name <- qxl$.attrs["name"]
      names(ql$name) <- NULL
      ql$view <- strsplit(qxl$.attrs["view"], "\\s+", perl=T)[[1]]
      ql$description <- qxl$.attrs["longDescription"]
      names(ql$description) <- NULL
      ql$sortOrder <- qxl$.attrs["sortOrder"][1]
      names(ql$sortOrder) <- NULL
     
      ql$constraints <- do.call(rbind, qxl[which(names(qxl)=="constraint")])
      rownames(ql$constraints) <- NULL
      ql$constraintLogic <- qxl$.attrs["constraintLogic"]
      names(ql$constraintLogic) <- NULL
            
      ql
}


queryList2XML <- function(ql){
      nq <- newXMLNode("query")
      xmlAttrs(nq)[["name"]] <- ql$name
      xmlAttrs(nq)[["model"]] <- "genomic"
      xmlAttrs(nq)[["view"]] <- paste(ql$view,collapse=" ")
      if(!is.null(ql$description)){
            xmlAttrs(nq)[["longDescription"]] <- ql$description
      }
      if(!is.null(ql$sortOrder)){
            xmlAttrs(nq)[["sortOrder"]] <- ql$sortOrder
      }
      
      
      for(i in 1:nrow(ql$constraints)){
            cnc <- newXMLNode("constraint")
            xmlAttrs(cnc)[["path"]] <- ql$constraints[i, "path"]
           
            xmlAttrs(cnc)[["op"]] <- ql$constraints[i, "op"]
            xmlAttrs(cnc)[["value"]] <- ql$constraints[i, "value"]
            xmlAttrs(cnc)[["code"]] <- ql$constraints[i, "code"]
            if("extraValue" %in% colnames(ql$constraints)){
                  xmlAttrs(cnc)[["extraValue"]] <- ql$constraints[i, "extraValue"]
            }
            
            addChildren(nq, kids=list(cnc), at=xmlSize(nq))
      }
      
      if(!is.null(ql$constraintLogic)){
            xmlAttrs(nq)[["constraintLogic"]] <- ql$constraintLogic
      }
      
      nq    
}

newQuery <- function(name="", view=character(), sortOrder="", longDescription="", 
                     constraints=matrix(character(0), 0, 5,dimnames=list(NULL, c('path', 'op', 'value', 'code', 'extraValue'))),
                     constraintLogic=NULL){
      nq <- list()
      nq$name <- name
      nq$view <- paste(view,collapse=" ")
      nq$description <- longDescription
      nq$sortOrder <- sortOrder
      nq$constraints <- constraints
      nq$constraintLogic <- constraintLogic
      
      nq
}


##5 - List
#<servlet-name>ws-available-lists</servlet-name>
#<url-pattern>/service/lists/*</url-pattern>
getLists <- function(im, timeout=3){
      res <- getURL(paste(im$mine, "/service/lists?token=",im$token, sep=""), .opts = list(timeout = timeout))
      if(length(grep("^\\[ERROR\\]", res))>0){
            print(res)
            return(NULL)
      }
      res.list <- lapply(fromJSON(res)$lists, 
                       function(x) sapply(x, 
                                          function(y) ifelse(is.list(y), paste(unlist(y), collapse="|"), y)))

      res.df <- data.frame(do.call(rbind, res.list), stringsAsFactors=F)
     
      res.df
}

newList <- function(im, name, gene,  organism="H.+sapiens",  description="", timeout=30){
      url <- paste(im$mine, "/service/lists/json?name=", name, "&description=",description,"&type=Gene",
                 "&extraValue=", organism,  "&token=", im$token, sep="")

      if(is.vector(gene)){
            fh <- fileUpload(filename="",contents=paste(gene, collapse=" "), contentType="text/plain")
        
            res <- postForm(url, identifiers=fh, .opts = list(timeout = timeout))
      }else if(is.character(gene) & length(gene)>0){
            fh <- fileUpload(gene, contentType="text/plain")
            res <- postForm(url, identifiers=fh, .opts = list(timeout = timeout))
      }
    
      res.list <- fromJSON(res)

      res.list    
}

#<servlet-name>ws-rename-list</servlet-name>
#<url-pattern>/service/lists/rename/*</url-pattern>
renameList <- function(im, old.name, new.name, timeout=3){
      res <- getURL(URLencode(paste(im$mine, "/service/lists/rename/json?oldname=", old.name, 
                        "&newname=",new.name,"&token=",im$token,sep="")),.opts = list(timeout = timeout))
      if(length(grep("^\\[ERROR\\]", res))>0){
            print(res)
            return(NULL)
      }
      res.list <- fromJSON(res)
    
      res.list
}

deleteList <- function(im, name, timeout=3){
      res <- httpDELETE(paste(im$mine, "/service/lists/json?name=", name, 
                                  "&format=tab&token=",im$token,sep=""), .opts=list(timeout = timeout))
    
      fromJSON(res)
}


###6 Region
getRegionFeature <- function(im, regions, featureType, organism="H. sapiens", extension=100, isInterbase=F, timeout=60){
      feature.type <- paste(sapply(featureType, function(x) paste('"', x, '"', sep="")), collapse=',')
      regions <- paste(sapply(regions, function(x) paste('"', x, '"', sep="")), collapse=',')
    
      termDataTmp <- paste('"extension":', extension, ',',
                         '"featureTypes": [', paste(feature.type, collapse=','), '],',
                         '"regions":[', paste(regions, collapse=','), '],',
                         '"isInterbase":', ifelse(isInterbase, 'true', 'false'), ',',
                         '"organism":"', organism, '"',
                         sep='')

      termData <- paste('{',termDataTmp,'}',sep="",collapse="")
    
      res <- getURL(URLencode(paste(im$mine, "/service/regions/bed?query=", termData, "&token=",im$token, sep='')),
                  .opts = list(timeout = timeout))
      
      if(length(grep("^\\[ERROR\\]", res))>0){
            print(res)
            return(NULL)
      }
      lines <- strsplit(res,'\\n')
      if(length(lines[[1]])<5){
            return(NULL)
      }
      rows <- lines[[1]][5:length(lines[[1]])]
      rec <- data.frame(do.call(rbind, strsplit(rows, '\\t')),stringsAsFactors=F)
      
      colnames(rec) <- c('chrom', 'start', 'end', tolower(featureType), 'score', 'strand')
    
      
      
      if(tolower(featureType)=="exon"){
            qry <- newQuery('gr', 
                     c('Exon.primaryIdentifier','Exon.transcripts.primaryIdentifier',
                    'Exon.gene.primaryIdentifier','Exon.gene.symbol'))
            qry$constraints <- rbind(qry$constraints, c('Exon', 'LOOKUP', paste(rec$exon,collapse=','), "A", ""))
            qry.res <- runQuery(im, qry)
    
            join.res <- NULL
            if(!is.null(qry.res)){
                  colnames(qry.res) <- c('exon', 'transcript','gene','symbol')
                  join.res <- sqldf('select a.*, b.transcript, b.gene, b.symbol from rec a inner join "qry.res" b 
                          on a.exon=b.exon')
        
                  join.res$symbol[grep('^\\s*$', join.res$symbol)] <- join.res$gene[grep('^\\s*$', join.res$symbol)]
                  join.res$start <- as.numeric(join.res$start)
                  join.res$end <- as.numeric(join.res$end)
            }
            rec <- join.res
      }

      rec
}


getRegionSequence <- function(im, regions, organism="H. sapiens", extension=100, isInterbase=F, timeout=60){
      
      regions <- paste(sapply(regions, function(x) paste('"', x, '"', sep="")), collapse=',')
      
      termDataTmp <- paste('"extension":', extension, ',',
                           '"regions":[', paste(regions, collapse=','), '],',
                                                '"isInterbase":', ifelse(isInterbase, 'true', 'false'), ',',
                                                '"organism":"', organism, '"',
                                                sep='')
      
      termData <- paste('{',termDataTmp,'}',sep="",collapse="")
      
      res <- getURL(URLencode(paste(im$mine, "/service/regions/sequence?query=", termData, "&token=",im$token, sep='')),
                    .opts = list(timeout = timeout))
      
      if(length(grep("^\\[ERROR\\]", res))>0){
            print(res)
            return(NULL)
      }
      
      lines <- strsplit(res,'\\n')
      
      idx <- grep('>', lines[[1]])
      
      dsest <- NULL
      if(length(idx) > 0){
            seq <- character(length(idx))
            idx <- c(idx, length(lines[[1]])+1)
            for(i in 1:(length(idx)-1)){
                  seq[i] <- paste(lines[[1]][(idx[i]+1):(idx[i+1]-1)], collapse="")
            }
            dset <- BStringSet(seq)
            names(dset) <- gsub("^>", "", lines[[1]][idx[-length(idx)]])
      }

      dset
}


##7 Enrichment analysis
## GO enrichment: ontology="go_enrichment_for_gene", subcategory=c('biological_process','cellular_component','molecular_function')
## Pathway enrichment: ontology="pathway_enrichment", subcategory=c('All','KEGG pathways data set','Reactome data set')
## Protein domain enrichment: ontology="prot_dom_enrichment_for_gene"
## Publication enrichment: ontology="publication_enrichment"
doEnrichment <- function(im, genelist, ontology, subcategory='', maxp=0.05, 
              correction=c('Holm-Bonferroni', 'Benjamini and Hochberg', 'Bonferroni', 'None'),
                          timeout=60){
      name <- paste(ontology, "_", paste(sample(c(0:9, letters, LETTERS),
                           12, replace=TRUE),
                    collapse=""),sep='')
      newList(im, name, genelist)
            
      if(length(correction) == 4){
            correction=correction[1]
      }
      
      res <- getURL(URLencode(paste(im$mine, "/service/list/enrichment?widget=", ontology, "&list=", name, 
                  "&filter=", subcategory, "&maxp=", maxp, "&correction=", correction, "&token=",im$token, sep='')),
                    .opts = list(timeout = timeout))
      if(length(grep("^\\[ERROR\\]", res))>0){
            print(res)
            return(NULL)
      }
      status <- fromJSON(res)

      result <- list()
      if(status$wasSuccessful){
            result <- status$results
      }
      
      
      deleteList(im, name)
      data.frame(do.call(rbind, result), stringsAsFactors=F)
}
