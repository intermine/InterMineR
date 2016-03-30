##3 - Template
#<servlet-name>ws-template</servlet-name>
#<url-pattern>/service/templates/*</url-pattern>
getTemplates <- function(im, format="data.frame", timeout=3) {
    if (format=="data.frame") {
        r <- GET(paste(im$mine, "/service/templates?format=xml", sep=""))
        stop_for_status(r)
        template <- content(r)
        res <- listTemplateSummary(template)
    } else if (format == "list") {
        r <- GET(paste(im$mine, "/service/templates?format=json", sep=""))
        stop_for_status(r)
        template.string <- content(r, "text")
        res <- fromJSON(template.string)$templates
    }
    res
}

listTemplateSummary <- function(template) {
    doc <- xmlTreeParse(template)
    r <- xmlRoot(doc)
    template.attr <- xmlApply(r, xmlAttrs)
    template.df <- do.call(rbind, template.attr)
    rownames(template.df) <- NULL
    data.frame(template.df[,c(1,2)], stringsAsFactors=FALSE)
}

getTemplateQuery <- function(im, name, timeout=3){
    template <- getTemplates(im, "list")
    ql <- template[[name]]
    ql$where <- do.call(rbind, ql$where)
    if(!("extraValue" %in% colnames(ql$where))){
        ql$where <- cbind(ql$where, extraValue=rep("", nrow(ql$where)))
    }
    ql
    print(ql)
}

##4 - Query
#<servlet-name>ws-query-results</servlet-name>
#<url-pattern>/service/query/results</url-pattern>
runQuery <- function(im, qry, format="data.frame", timeout=60){

    if (is.list(qry)) {
        query <- queryList2XML(qry)
    } else if(isXMLString(qry)) {
        query <- xmlParseString(qry)
    }
    print(query)
    answer <- NULL

    if (format=="data.frame") {
        query.str <- URLencode(toString.XMLNode(query))
        query.str <- gsub("&", '%26', query.str)
        query.str <- gsub(";", '%3B', query.str)

        r <- GET(paste(im$mine, "/service/query/results?query=",
            query.str,"&format=xml",sep=""))
        stop_for_status(r)
        res <- content(r)
        res.xml <- xmlRoot(xmlParse(res))
        if(names(res.xml[1])=="error"){
            answer = NULL
        } else {
            if (length(getNodeSet(res.xml, "//Result")) > 0) {
                answer= xmlToDataFrame(res, stringsAsFactors=FALSE)
                colnames(answer) <- strsplit(xmlAttrs(query)[["view"]],
                    "\\s+", perl=TRUE)[[1]]
            } else {
                answer=NULL
            }
        }
    } else if (format=="sequence") {
        query.str <- URLencode(toString.XMLNode(query))
        query.str <- gsub("&", '%26', query.str)
        query.str <- gsub(";", '%3B', query.str)
        r <- GET(URLencode(paste(im$mine,
            "/service/query/results/fasta?query=",
            toString.XMLNode(query),sep="")))
        stop_for_status(r)
        res <- str(content(r, "text"))
        lines <- strsplit(res,'\\n')
        idx <- grep('>', lines[[1]])
        dset <- NULL
        if (length(idx) > 0) {
            seq <- character(length(idx))
            idx <- c(idx, length(lines[[1]])+1)
            for (i in 1:(length(idx)-1)) {
                seq[i] <- paste(lines[[1]][(idx[i]+1):(idx[i+1]-1)],
                collapse="")
            }
            dset <- BStringSet(seq)
            names(dset) <- gsub("^>", "", lines[[1]][idx[-length(idx)]])
        }
        answer <- dset
    }
    answer
}

queryXML2List <- function(qx) {
    qxl <- xmlToList(xmlParseString(qx))
    ql <- newQuery()

    ql$name <- qxl$.attrs["name"]
    names(ql$name) <- NULL
    ql$view <- strsplit(qxl$.attrs["view"], "\\s+", perl=TRUE)[[1]]
    ql$description <- qxl$.attrs["longDescription"]
    names(ql$description) <- NULL
    ql$sortOrder <- qxl$.attrs["sortOrder"][1]
    names(ql$sortOrder) <- NULL

    ql$constraint <- do.call(rbind, qxl[which(names(qxl)=="constraint")])
    rownames(ql$constraint) <- NULL
    ql$constraintLogic <- qxl$.attrs["constraintLogic"]
    names(ql$constraintLogic) <- NULL

    ql
}

queryList2XML <- function(ql){
  
    nq <- newXMLNode("query")
    xmlAttrs(nq)[["name"]] <- ql$name
    xmlAttrs(nq)[["model"]] <- "genomic"
    xmlAttrs(nq)[["view"]] <- paste(ql$select,collapse=" ")
    
    if (!is.null(ql$description)) {
        xmlAttrs(nq)[["longDescription"]] <- ql$description
    }
    if (!is.null(ql$orderBy)) {
        #xmlAttrs(nq)[["sortOrder"]] <- c(ql$orderBy)
      xmlAttrs(nq) <- c(sortOrder = paste(ql$orderBy,collapse=" "))
    }
    if (!is.null(ql$where)) {
        for(i in 1:nrow(ql$where)) {
            cnc <- newXMLNode("constraint")
            xmlAttrs(cnc)[["path"]] <- paste(ql$where[i, "path"],collapse=" ")
            xmlAttrs(cnc)[["op"]] <- paste(ql$where[i, "op"],collapse=" ")
            xmlAttrs(cnc)[["value"]] <- paste(ql$where[i, "value"],collapse=" ")
            xmlAttrs(cnc)[["code"]] <- paste(ql$where[i, "code"],collapse=" ")

            if ("extraValue" %in% colnames(ql$where)) {
              xmlAttrs(cnc)[["extraValue"]] <- paste(ql$where[i, "extraValue"],collapse=" ")
            }
            
            addChildren(nq, kids=list(cnc), at=xmlSize(nq))
        }
    }
    if (!is.null(ql$constraintLogic)) {
        xmlAttrs(nq)[["constraintLogic"]] <- ql$constraintLogic
    }
    nq
}

newQuery <- function(name="", view=character(), sortOrder="",
    longDescription="", where=matrix(character(0), 0, 5,dimnames =
    list(NULL, c('path', 'op', 'value', 'code', 'extraValue'))),
    constraintLogic = NULL) {
        nq <- list()
        nq$name <- name
        nq$view <- paste(view,collapse=" ")
        nq$description <- longDescription
        nq$sortOrder <- sortOrder
        nq$where <- where
        nq$constraintLogic <- constraintLogic

        nq
}
