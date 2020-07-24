#' @import httr
#' @import InterMineR
#GET_api_list: returns the response object of the Request
#' @export
setGeneric("GET_api_list", function(object,...){
  standardGeneric("GET_api_list")
})

#' @rdname ListManager-methods
#' @exportMethod GET_api_list
setMethod(
  "GET_api_list",
  signature(object = "ListManager"),
  function(object,...){
    GET(paste0(object@mine,"/service",object@LIST_PATH), add_headers(Authorization = paste("Token",object@token, sep = " ")))
  })

#get_list: Return a list from the service by name, if it exists
#' @export
setGeneric("get_list", function(object,...){
  standardGeneric("get_list")
})

#' @rdname ListManager-methods
#' @exportMethod get_list
setMethod(
  "get_list",
  signature(object = "ListManager"),
  function(object,list_name){
    resp_list<-GET_api_list(object)
    content_list_parsed<-content(resp_list, "parsed", encoding = "ISO-8859-1")
    
    exist<-FALSE
    
    for (list in content_list_parsed$lists){
      if(list$name == list_name){
        return(list)
        exist<-TRUE 
      }
    }
    
    if(exist==FALSE){
      warning(paste0("List",list_name,"doesn't exist."))
      }
    })

####example of get_list####
im.human.list<-list_manager(initInterMine(listMines()["HumanMine"],"F16793D0k4BaF5hbe3s0"))
class(im.human.list)
resp<-get_list(im.human.list, "UpinPancreas")
resp


#get_unused_list_name: Checks if a list exists by name and it it does it, provides a default name
#' @export
setGeneric("get_unused_list_name", function(object,...){
  standardGeneric("get_unused_list_name")
})

#' @rdname ListManager-methods
#' @exportMethod get_unused_list_name
setMethod(
  "get_unused_list_name",
  signature(object = "ListManager"),
  function(object,given_name='my_list'){  
    resp_list<-GET_api_list(object)
    content_list_parsed<-content(resp_list, "parsed", encoding = "ISO-8859-1")
  
    list_names<-list()
    
    for (list in content_list_parsed$lists){
      list_names<-append(list_names,list$name)
    }
    counter<-1
    
    name<-object@DEFAULT_LIST_NAME
    
    if(is.element(given_name,list_names)){
      
      given_name<-object@DEFAULT_LIST_NAME
      
      while(is.element(name,list_names)){
    
        name<-paste0(object@DEFAULT_LIST_NAME, counter)
        given_name<-name
        counter<-counter+1
        
      }
    }
    
    return(given_name)
    })

####example of get_unused_list_name####
get_unused_list_name(im.human.list,"PL_obesityMonogen_ORahilly09")

#delete_lists: Deletes the lists passed as the first argument.Only deletes lists that belong to the user.  
#The first argument need to be of the form c("list_name_1","list_name_2","list_name_3")
#' @export
setGeneric("delete_lists", function(object,...){
  standardGeneric("delete_lists")
})

#' @rdname ListManager-methods
#' @exportMethod delete_lists
setMethod(
  "delete_lists",
  signature(object = "ListManager"),
  function(object,lists){ 
  #all the names 
  resp_list<-GET_api_list(object)
  content_list_parsed<-content(resp_list, "parsed", encoding = "ISO-8859-1")
  
  all_names<-list()
  
  for (list in content_list_parsed$lists){
      all_names<-append(all_names,list$name)
    }
  
  #all the names of template lists
  
  url2<-paste0(object@mine,"/service",object@LIST_PATH)
  resp_list2<-GET(url2)
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
    
    uri<-paste0(object@mine,"/service", object@LIST_PATH, "?name=", name, "&token=", object@token)
    
    DELETE(uri,add_headers(Authorization = paste("Token",object@token, sep = " ")))
    
  }
  
    #refresh lists/update 
  
  #PATCH(uri,add_headers(Authorization = paste("Token",Token, sep = " "))) Gives status code 501
  
  #PUT(uri,add_headers(Authorization = paste("Token",Token, sep = " "))) Gives status code 400
  
  GET(paste0(object@mine,"/service", '/lists', "?", "&token=", object@token),
      add_headers(Authorization = paste("Token",object@token, sep = " ")))
})



####example1: Two lists created by the user####
delete_lists(im.human.list,c("intersect_1", "intersect_2"))

####example2: A list not created by the user####
delete_lists(im.human.list,c("PL_DiabesityGWAS_pval-4"))

#create_list: create a new list by uploading a set of identifiers
#content parameter can be the result of a query obtained with runQuery() or a string of identifiers separated by commas
#' @export
setGeneric("create_list", function(object,...){
  standardGeneric("create_list")
})

#' @rdname ListManager-methods
#' @exportMethod create_list
setMethod(
  "create_list",
  signature(object = "ListManager"),
  function(object,content, list_type,name=NULL,description=NULL,organism=NULL){ 

    uri<-paste0(object@mine,"/service/lists?")
    if(is.null(name)){
      name<-get_unused_list_name(object) #this function is created in GET_api_list-get_list-get_unused_list_name.R
    }
    else{
      name<-get_unused_list_name(object,name)
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
         add_headers(Authorization = paste("Token",object@token, sep = " "),
                     'Content-Type' = "text/plain"))
  })



####example: query1DiabetesResults is defined from the notebook "Workshop_Workflow_PAX6"####
query1Diabetes <- setQuery( 
  # here we're choosing which columns of data we'd like to see
  select = c("Gene.primaryIdentifier", "Gene.symbol"),
  # set the logic for constraints. The first constraint is the first path+operator+value, 
  # e.g. Gene.organism.name = Homo sapiens, and the second constraint is the combination 
  # of the second path+operator+value, e.g. Gene.diseases.name CONTAINS diabetes
  where = setConstraints(
    paths = c("Gene.organism.name", "Gene.diseases.name"),
    operators = c("=", "CONTAINS"),
    values = list("Homo sapiens","diabetes")
  )
)
query1DiabetesResults <- runQuery(list(mine=im.human@mine,token=im.human@token),query1Diabetes)
create_list(im.human.list,content = query1DiabetesResults, list_type = "Gene", name = "my_list")

#do_operation: creates a new list results of an operation, it shouldn't be called directly
#' @export
setGeneric("do_operation", function(object,...){
  standardGeneric("do_operation")
})

#' @rdname ListManager-methods
#' @exportMethod do_operation
setMethod(
  "do_operation",
  signature(object = "ListManager"),
  function(object,path,operation,lists,name,description,tags){

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
      name<-get_unused_list_name(object) 
    }else{
      name<-get_unused_list_name(object,name)
    }
    
    uri<-paste0(object@mine,
                "/service", 
                path,
                "?")
    
    return(POST(paste0(uri,"name=",name,"&lists=",lists_names, "&description=", 
                       URLencode(description),"&tags=",tags),
                add_headers(Authorization = paste("Token",object@token, sep = " "))))
  })



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
#' @export
setGeneric("intersect", function(object,...){
  standardGeneric("intersect")
})

#' @rdname ListManager-methods
#' @exportMethod intersect
setMethod(
  "intersect",
  signature(object = "ListManager"),
  function(object,lists, name=NULL, description=NULL, tags=list()){
    return(do_operation(object,object@INTERSECTION_PATH, "Intersection", lists, name, description, tags))
  })

####example of intersect####
intersect(im.human.list,c("diabetesGenes","UpinPancreas"),"intersect_list")

#union: creates new lists which contain all the members contained in the set of input lists
#' @export
setGeneric("union", function(object,...){
  standardGeneric("union")
})

#' @rdname ListManager-methods
#' @exportMethod union
setMethod(
  "union",
  signature(object = "ListManager"),
  function(object,lists, name=NULL, description=NULL, tags=list()){
    return(do_operation(object,object@UNION_PATH, "Union", lists, name, description, tags))
  })

####example of union####
union(im.human.list,c("diabetesGenes","UpinPancreas"),"union_list")

#difference: creates new lists which only contain members which are not shared by an even number of lists
#' @export
setGeneric("difference", function(object,...){
  standardGeneric("difference")
})

#' @rdname ListManager-methods
#' @exportMethod difference
setMethod(
  "difference",
  signature(object = "ListManager"),
  function(object,lists, name=NULL, description=NULL, tags=list()){
    return(do_operation(object,object@DIFFERENCE_PATH, "Difference", lists, name, description, tags))
  })

####example of difference####
difference(im.human.list,c("diabetesGenes","UpinPancreas"),"diff_list")

#subtract: creates new lists which contain only those elements which are present in one set of lists, 
#and none of those elements which are present in another set of lists. 
#This is what is typically thought of as subtraction, or more technically, the asymmetric difference of two sets.
#' @export
setGeneric("subtract", function(object,...){
  standardGeneric("subtract")
})

#' @rdname ListManager-methods
#' @exportMethod subtract
setMethod(
  "subtract",
  signature(object = "ListManager"),
  function(object,lefts,rights, name=NULL, description=NULL, tags=list()){

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
      description <- sprintf("Subtraction of %s from %s", 
                             paste(right_names_description,sep = "and"), 
                             paste(left_names_description, sep = "and"))
      }
    
    if (is.null(name)){
      name<-get_unused_list_name(object) 
    }else{
      name<-get_unused_list_name(object,name)
      }
    
    uri<-paste0(object@mine,"/service", object@SUBTRACTION_PATH,"?")
    
    return(POST(paste0(uri,"name=",name,"&description=", 
                       URLencode(description),"&references=",left_names,"&subtract=",right_names,
                       "&tags=",tags),
                add_headers(Authorization = paste("Token",object@token, sep = " "))))
    
  })

####example of subtract####
subtract(im.human.list,lefts = c("diabetesGenes"), rights = c("UpinPancreas"),"subtr_list")
