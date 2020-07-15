#' @import httr
#' @import InterMineR

#delete_lists: Deletes the lists passed as the first argument.Only deletes lists that belong to the user.  
#The first argument need to be of the form c("list_name_1","list_name_2","list_name_3")
delete_lists<-function(lists, mine, Token){
  #all the names 
  url<-paste0(modify_url(initInterMine(mine=listMines()[mine],token=Token)$mine),"/service")
  LIST_PATH = '/lists'
  resp_list<-GET_api_list(url,token = Token, list_path = LIST_PATH)
  content_list_parsed<-content(resp_list, "parsed", encoding = "ISO-8859-1")
  all_names<-list()
  for (list in content_list_parsed$lists){
    all_names<-append(all_names,list$name)
  }
  #all the names of template lists
  url2<-paste0(modify_url(initInterMine(mine=listMines()[mine],token=Token)$mine),"/service")
  resp_list2<-GET(paste0(url2,LIST_PATH))
  content_list_parsed2<-content(resp_list2, "parsed", encoding = "ISO-8859-1")
  all_names_templates<-list()
  for (list in content_list_parsed2$lists){
    all_names_templates<-append(all_names_templates,list$name)
  }
  
  for (l in lists){
    name<-l
    if (!(name %in% all_names)){
      warning(sprintf("%s does not exist - skipping", name))    
      next
    }
    if (name %in% all_names_templates){
      warning(sprintf("%s is a template list that cannot be deleted", name))
      next
    }
    warning(sprintf("deleting %s", name))
    uri<-paste0(modify_url(initInterMine(mine=listMines()[mine],token=Token)$mine),"/service", LIST_PATH, "?name=", name, "&token=", Token)
    DELETE(uri,add_headers(Authorization = paste("Token",Token, sep = " ")))
  }
  
  #refresh lists/update 
  #PATCH(uri,add_headers(Authorization = paste("Token",Token, sep = " "))) Gives status code 501
  #PUT(uri,add_headers(Authorization = paste("Token",Token, sep = " "))) Gives status code 400
  GET(paste0(modify_url(initInterMine(mine=listMines()[mine],token=Token)$mine),"/service", '/lists', "?", "&token=", Token),add_headers(Authorization = paste("Token",Token, sep = " ")))
}

#example1: Two lists created by the user
delete_lists(c("intersect_1", "intersect_2"),"HumanMine", "F16793D0k4BaF5hbe3s0")
#example2: A list not created by the user
delete_lists(c("PL_DiabesityGWAS_pval-4"),"HumanMine", "F16793D0k4BaF5hbe3s0")

