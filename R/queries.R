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
        print(template.string)
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
    r <- GET(paste(im$mine, "/service/templates/", name, "?format=xml", sep=""))
    stop_for_status(r)
    template <- content(r)
    xmlTemplate <- xmlRoot(xmlParse(template))
    xmlTemplate.query <- xmlTemplate[[1]];
}

##4 - Query
#<servlet-name>ws-query-results</servlet-name>
#<url-pattern>/service/query/results</url-pattern>
runQuery <- function(im, query, timeout=60){
    answer <- NULL

    query.str <- URLencode(toString.XMLNode(query))
    query.str <- gsub("&", '%26', query.str)
    query.str <- gsub(";", '%3B', query.str)

    r <- GET(paste(im$mine, "/service/query/results?query=",
            query.str,"&format=xml",sep=""))
    stop_for_status(r)
    res <- content(r)
    res.xml <- xmlRoot(xmlParse(res))
  
    if (length(getNodeSet(res.xml, "//Result")) > 0) {
        answer = xmlToDataFrame(res.xml, stringsAsFactors=FALSE)
        colnames(answer) <- strsplit(xmlAttrs(query)[["view"]],
            "\\s+", perl=TRUE)[[1]]
    } else {
        # no results
        answer=NULL
    }
    answer
}
