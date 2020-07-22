#' @import httr
#' @import InterMineR

#do_operation: creates a new list results of an operation, it shouldn't be called directly
do_operation<-function(mine,Token,path,operation,lists,name,description,tags){
  lists_names<-NULL
  for (l in lists){
    lists_names<-paste(lists_names,l,sep = ";")
  }
  lists_names<-substr(lists_names, 2, nchar(lists_names))
  
  list_names_description<-make_list_names(lists)
  if (is.null(description)){
    description <- sprintf("%s of %s", operation, paste(list_names_description, collapse = " "))
  }
  if (is.null(name)){
    name<-get_unused_list_name(mine,Token) 
  }else{
    name<-get_unused_list_name(mine,Token,name)
  }
  uri<-paste0(modify_url(initInterMine(mine=listMines()[mine],token=Token)$mine),
              "/service", 
              path,
              "?")
  
  return(POST(paste0(uri,"name=",name,"&lists=",lists_names, "&description=", 
                     URLencode(description),"&tags=",tags),
              add_headers(Authorization = paste("Token",Token, sep = " "))))
}

#make_list_names: turns a list of things into a list of list names
make_list_names<-function(lists){
  list_names<-list()
  for (l in lists){
    try(list_names<-append(list_names, l))
    #maybe more assumptions are needed
  }
  return(list_names)
}

#intersect: creates new lists which contain only those items which are members of all the source lists.
intersect<-function(mine,Token,lists, name=NULL, description=NULL, tags=list()){
  INTERSECTION_PATH = "/lists/intersect/json"
  return(do_operation(mine,Token,INTERSECTION_PATH, "Intersection", lists, name, description, tags))
}
#example:
intersect("HumanMine","F16793D0k4BaF5hbe3s0",c("diabetesGenes","UpinPancreas"),"intersect_list")

#union: creates new lists which contain all the members contained in the set of input lists
union<-function(mine,Token,lists, name=NULL, description=NULL, tags=list()){
  UNION_PATH = "/lists/union/json"
  return(do_operation(mine,Token,UNION_PATH, "Union", lists, name, description, tags))
}
#example:
union("HumanMine","F16793D0k4BaF5hbe3s0",c("diabetesGenes","UpinPancreas"),"union_list")

#difference: creates new lists which only contain members which are not shared by an even number of lists
difference<-function(mine,Token,lists, name=NULL, description=NULL, tags=list()){
  DIFFERENCE_PATH = "/lists/diff/json"
  return(do_operation(mine,Token,DIFFERENCE_PATH, "Difference", lists, name, description, tags))
}
#example:
difference("HumanMine","F16793D0k4BaF5hbe3s0",c("diabetesGenes","UpinPancreas"),"diff_list")

#subtract: creates new lists which contain only those elements which are present in one set of lists, 
#and none of those elements which are present in another set of lists. 
#This is what is typically thought of as subtraction, or more technically, the asymmetric difference of two sets.
subtract<-function(mine,Token,lefts,rights, name=NULL, description=NULL, tags=list()){
  SUBTRACTION_PATH = "/lists/subtract/json"
  left_names_description = make_list_names(lefts)
  right_names_description = make_list_names(rights)
  
  left_names<-NULL
  for (l in lefts){
    left_names<-paste(left_names,l,sep = ";")
  }
  left_names<-substr(left_names, 2, nchar(left_names))
  
  right_names<-NULL
  for (l in rights){
    right_names<-paste(right_names,l,sep = ";")
  }
  right_names<-substr(right_names, 2, nchar(right_names))
  
  if (is.null(description)){
    description <- sprintf("Subtraction of %s from %s", paste(right_names_description,sep = "and"), paste(left_names_description, sep = "and"))
  }
  if (is.null(name)){
    name<-get_unused_list_name(mine,Token) 
  }else{
    name<-get_unused_list_name(mine,Token,name)
  }
  uri<-paste0(modify_url(initInterMine(mine=listMines()[mine],token=Token)$mine),
              "/service", 
              SUBTRACTION_PATH,
              "?")
  return(POST(paste0(uri,"name=",name,"&description=", 
                     URLencode(description),"&references=",left_names,"&subtract=",right_names,
                     "&tags=",tags),
              add_headers(Authorization = paste("Token",Token, sep = " "))))
}
#example:
subtract("HumanMine","F16793D0k4BaF5hbe3s0",lefts = c("diabetesGenes"), rights = c("UpinPancreas"),"subtr_list")