#' @import httr
#' @import InterMineR
#GET_api_list: returns the response object of the Request
GET_api_list <- function(url,token,list_path) {
  GET(paste0(url,list_path), add_headers(Authorization = paste("Token",token, sep = " ")))
}

#get_list: Return a list from the service by name, if it exists
get_list<-function(mine, Token, list_name){
  url<-paste0(modify_url(initInterMine(mine=listMines()[mine],token=Token)$mine),"/service")
  LIST_PATH = '/lists'
  resp_list<-GET_api_list(url,token = Token, list_path = LIST_PATH)
  content_list_parsed<-content(resp_list, "parsed", encoding = "ISO-8859-1")
  
  for (list in content_list_parsed$lists){
    if(list$name == list_name){
      return(list)
    }
  }
}
#further improvements: include an error warning if the list doesn't exist
#example
resp<-get_list("HumanMine","F16793D0k4BaF5hbe3s0", "UpinPancreas")
resp

#get_unused_list_name: Checks if a list exists by name and it it does it, provides a default name
DEFAULT_LIST_NAME = 'my_list'
get_unused_list_name<-function(mine,Token,given_name=DEFAULT_LIST_NAME){
  url<-paste0(modify_url(initInterMine(mine=listMines()[mine],token=Token)$mine),"/service")
  LIST_PATH = '/lists'
  resp_list<-GET_api_list(url,token = Token, list_path = LIST_PATH)
  content_list_parsed<-content(resp_list, "parsed", encoding = "ISO-8859-1")
  
  list_names<-list()
  
  for (list in content_list_parsed$lists){
    list_names<-append(list_names,list$name)
  }
  counter<-1
  name<-DEFAULT_LIST_NAME
  if(is.element(given_name,list_names)){
    given_name<-DEFAULT_LIST_NAME
    while(is.element(name,list_names)){
      name<-paste0(DEFAULT_LIST_NAME, counter)
      given_name<-name
      counter<-counter+1
    }
  }
  return(given_name)
}
#example
get_unused_list_name("HumanMine","F16793D0k4BaF5hbe3s0","PL_obesityMonogen_ORahilly09")