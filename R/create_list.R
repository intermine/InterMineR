#' @import httr
#' @import InterMineR
#create_list: create a new list by uploading a set of identifiers
#content parameter can be the result of a query obtained with runQuery() or a string of identifiers separated by commas
create_list<-function(content,mine,list_type,Token,name=NULL,description=NULL,organism=NULL){
  im<-initInterMine(listMines()[mine])
  uri<-paste0(im$mine,"/service/lists?")
  
  if(is.null(name)){
    name<-get_unused_list_name(mine,Token)
  }
  else{
    name<-get_unused_list_name(mine,Token,name)
  }
  
  if(is.null(description)){
    description<-"List created with R Studio client library"
  }
  
  if(is.list(content)){
    ids<-list()
    for (row in content) {
      ids<-append(ids,row)
      
    }
    content<-NULL 
    for (id in ids) {
      content<-paste(content,id,sep = ",")
      
    }
    content<-substr(content, 2, nchar(content))
  }
  
  POST(url = paste0(uri, "name=",name,"&description=",URLencode(description),"&type=", list_type, "&organism=", organism), 
       body = content, #these are ids
       add_headers(Authorization = paste("Token",Token, sep = " "),
                   'Content-Type' = "text/plain"))
  
}