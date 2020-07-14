library(httr)
library(InterMineR)
library(xml2) #it is HTML
library(rvest)
library(XML) #to parse XML and HTML 
library(jsonlite)
library(iterators)
library(repr)

LIST_CREATION_PATH = '/lists'
DEFAULT_DESCRIPTION = 'List created with R client library'
DEFAULT_LIST_NAME = 'my_list'
PLAIN_TEXT = "text/plain"

#dateCreated
#size ---> length(content)
#authorized
#name
#description ---> DEFAULT_DESCRIPTION = 'List created with R client library'
#id
#type
#title
#status
#timestamp
#tags

post_content<-function(url, body, mimetype, charset = "utf-8"){
  #content_type<-sprintf("%s;charset=%s",mimetype,charset) #headers
  return(POST(uri, 
         body = body, 
         encode = "json", 
         add_headers(Authorization = paste("Token","F16793D0k4BaF5hbe3s0", sep = " "))))
 
}

create_list<-function(content,
                      list_type='',
                      name=NULL,
                      description=NULL,
                      tags=list(),
                      add=list()){
  
  " content: The source of the identifiers for this list.
  This can be:
  * A string with white-space separated terms.
  * The name of a file that contains the terms.
  * A file-handle like thing (something with a 'read' method)
  * An iterable of identifiers
  * A query with a single column.
  * Another list."
  
  if(is.null(description)){
    description<-DEFAULT_DESCRIPTION
  }
  
  if(is.null(name)){
    name<-get_unused_list_name() #arguments missing
  }
  else{
    name<-get_unused_list_name(name) #arguments missing
  }
  
  if(length(content)==0){
    warning("Please create a valid list with at least one element and create the list again.")
    return() 
  }
  
  item_content<-content
  #File like thing
  try(ids<-read(item_content))
  #File like thing
  try(ids<-file(item_content,encoding = "UTF-8"))
  #List like thing
  try(expr = {
    idents<-iter(item_content)
    ids<-paste(mapply('"%s"',idents),sep="\n")
  })
  #String like thing
  try(ids<-gsub(" ","",item_content))
  
  if(exists(ids)){
    query_form <- list(name,list_type,description,tags)
    names(query_form) <- c("name", "type", "description", "tags")
    uri<-paste0(modify_url(initInterMine(mine=listMines()[mine],token=Token)$mine),
                "/service", 
                LIST_CREATION_PATH,
                "?")
    i=1
    for (element in query_form)
    #this line gives error because I can't get the names
      uri= uri+names(query_form)[i]+"="+element+"&" 
      i=i+1
    }
    substr(uri, 1, nchar(uri)-1) #substract the last &
    
    data<-post_content(uri, ids, PLAIN_TEXT, charset = "utf-8")
    
  }
  else{
    warning('Cannot create list from '
            + repr(item_content))
  }
  
  
}

#Outside the function I'm trying different ways to POST
#OPTION A
query_formA <- newXMLNode("list")
xmlAttrs(query_formA)[["name"]]<-"name"
xmlAttrs(query_formA)[["type"]]<-"type"
xmlAttrs(query_formA)[["description"]]<-"description"
xmlAttrs(query_formA)[["tags"]]<-"tags"
queryformA.unencoded <- toString.XMLNode(query_formA)
queryformA.str <- URLencode(queryformA.unencoded)
queryformA.str <- gsub("&", '%26', queryformA.str)
queryformA.str <- gsub(";", '%3B', queryformA.str)

uri<-paste0(modify_url(initInterMine(mine=listMines()["HumanMine"],token="F16793D0k4BaF5hbe3s0")$mine),
            "/service", 
            LIST_CREATION_PATH,
            "?")
POST(paste(uri,queryformA.str,"&format=xml",sep = ""), query = ids,
     encode = "json", 
     add_headers(Authorization = paste("Token","F16793D0k4BaF5hbe3s0", sep = " ")))




#From now on I use this url and change other parameters
uri<-"https://www.humanmine.org/humanmine/service/lists?name=name1&type=list_type1"
#OPTION B
POST(url = paste(uri,"&token=F16793D0k4BaF5hbe3s0",sep = ""), body = list(ids_prueba="ids_prueba"),
     encode = "json", 
     add_headers(Authorization = paste("Token","F16793D0k4BaF5hbe3s0", sep = " ")))

#OPTION C
ids_2<-list(data=c(1,2,3,4,5,6))
r<-POST(url = paste(uri,"&token=F16793D0k4BaF5hbe3s0",sep = ""),body = ids_2, encode = "json", multipart=FALSE,
        add_headers(Authorization = paste("Token","F16793D0k4BaF5hbe3s0", sep = " ")))
content(r)
http_status(r)

#OPTION D: I try to upload a file
list_1<-c(1,2,3,4,5,6)
save(list_1,file = "values_list.Rdata")
POST(url = paste(uri,"&token=F16793D0k4BaF5hbe3s0",sep = ""),
     body=list(name="test_1.csv", filedata=upload_file("values_list.Rdata", "text/csv")),
     add_headers(Authorization = paste("Token","F16793D0k4BaF5hbe3s0", sep = " ")))

#OPTION E
request_body<-data.frame(data=list_1)
request_body_json<-toJSON(list(documents = request_body), auto_unbox = TRUE)
POST(url = paste(uri,"&token=F16793D0k4BaF5hbe3s0",sep = ""),
     body=request_body_json, config = 
     add_headers(Authorization = paste("Token","F16793D0k4BaF5hbe3s0", sep = " ")))
#OPTION F
POST(paste(uri,"&token=F16793D0k4BaF5hbe3s0",sep = ""),add_headers(Authorization = paste("Token","F16793D0k4BaF5hbe3s0", sep = " ")), encode = "form")

#RCurl package (in progress)
library(RCurl)
postForm( uri = paste(uri,"&token=F16793D0k4BaF5hbe3s0",sep = ""),
          token = "F16793D0k4BaF5hbe3s0", 
          style = "POST", 
          .opts = curlOptions(verbose = TRUE))
