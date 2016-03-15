##3 - Template
#<servlet-name>ws-template</servlet-name>
#<url-pattern>/service/templates/*</url-pattern>
getTemplates <- function(im, format="data.frame", timeout=3){
    if (format=="data.frame") {
        r <- GET(paste(im$mine, "/service/templates?format=xml", sep=""))
        stop_for_status(r)
        template <- content(r)
        res <- listTemplateSummary(template)
    } else if(format == "list") {
        r <- GET(paste(im$mine, "/service/templates?format=json", sep=""))
        stop_for_status(r)
        template.string <- str(content(r))
        res <- fromJSON(template.string)$templates
    }
    res
}

listTemplateSummary <- function(template) {
    #xmlfile <- xmlParse(template, asText = TRUE)
    #out <- getNodeSet(xmlfile, "//*[name()='template']", fun=xmlToList)
    #df <- data.frame(do.call(rbind, out))
    #df[,c(1,2)]

    #ldply(xmlToList(xmlfile), data.frame)

    #templateList <- xpathSApply(xmlfile, c('//template-queries'), xmlValue)
    #templateList

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

    answer <- NULL

    if (format=="data.frame") {
        query.str <- utilities.URLencode(toString.XMLNode(query))
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
        query.str <- utilities.URLencode(toString.XMLNode(query))
        query.str <- gsub("&", '%26', query.str)
        query.str <- gsub(";", '%3B', query.str)
        r <- GET(utilities.URLencode(paste(im$mine,
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
    xmlAttrs(nq)[["view"]] <- paste(ql$view,collapse=" ")
    if (!is.null(ql$description)) {
        xmlAttrs(nq)[["longDescription"]] <- ql$description
    }
    if (!is.null(ql$sortOrder)) {
        xmlAttrs(nq)[["sortOrder"]] <- ql$sortOrder
    }
    if (!is.null(ql$where)) {
        for(i in 1:nrow(ql$where)) {
            cnc <- newXMLNode("constraint")
            xmlAttrs(cnc)[["path"]] <- ql$where[i, "path"]
            xmlAttrs(cnc)[["op"]] <- ql$where[i, "op"]
            xmlAttrs(cnc)[["value"]] <- ql$where[i, "value"]
            xmlAttrs(cnc)[["code"]] <- ql$where[i, "code"]
            if("extraValue" %in% colnames(ql$where)){
                xmlAttrs(cnc)[["extraValue"]] <- ql$where[i, "extraValue"]
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
    longDescription="", constraints=matrix(character(0), 0, 5,dimnames =
    list(NULL, c('path', 'op', 'value', 'code', 'extraValue'))),
    constraintLogic = NULL) {
        nq <- list()
        nq$name <- name
        nq$view <- paste(view,collapse=" ")
        nq$description <- longDescription
        nq$sortOrder <- sortOrder
        nq$where <- constraints
        nq$constraintLogic <- constraintLogic

        nq
}
