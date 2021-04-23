\name{ListManager-methods}
\docType{methods}

\alias{ListManager-methods}
\alias{get_list}
\alias{get_list-methods}
\alias{get_list,ListManager-method}
\alias{delete_lists}
\alias{delete_lists-methods}
\alias{delete_lists,ListManager-method}
\alias{create_list}
\alias{create_list-methods}
\alias{create_list,ListManager-method}
\alias{intersect}
\alias{intersect-methods}
\alias{intersect,ListManager-method}
\alias{union}
\alias{union-methods}
\alias{union,ListManager-method}
\alias{difference}
\alias{difference-methods}
\alias{difference,ListManager-method}
\alias{subtract}
\alias{subtract-methods}
\alias{subtract,ListManager-method}

\title{
Methods for managing and doing operations with \code{\link{ListManager-class}} objects.
}

\description{
ListManager constitutes a class used to store the information required for managing lists contents and performing operations. The methods that enable to treat with lists and do operations are described below:
}

\section{Methods}{
  \bold{Class-specific methods:}
  \describe{
     
     \item{\code{get_list(ListManager-class, list_name)}:}{
     Return a list from the service by name, if it exists. 
      \arguments{
        \item{list_name}{
          the name of an existing list
        }
      }
     }
     
     \item{\code{delete_lists(ListManager-class, lists)}:}{
     Deletes the lists passed as the second argument. Only deletes lists that belong to the user. The second argument need to be of the form c("list_name_1","list_name_2","list_name_3").  
      \arguments{
        \item{lists}{
          a list containing the names of the lists to be deleted.
        }
      }
     }
     
     \item{\code{create_list(ListManager-class, content, list_type, name = NULL, description = NULL, organism = NULL))}:}{
     Creates a new list by uploading a set of identifiers. 
      \arguments{
        \item{content}{
          the result of a query obtained with \code{\link{runQuery}} or a lists of identifiers.
        }
        \item{list_type}{
          the type of objects to include in the list. 
        }
         \item{name}{
          the name for the new list. If none is provided one will be generated. 
        }
         \item{description}{
          a description for the list (free text).
        }
        \item{organism}{
          the name of the organism.
        }
      }
     }
     
     \item{\code{intersect(ListManager-class, lists, name = NULL, description = NULL, tags = list())}:}{
     Creates new lists which contain only those items which are members of all the source lists.
      \arguments{
        \item{lists}{
          the source lists to be intersected.
        }
         \item{name}{
          the name for the new list. If none is provided one will be generated. 
        }
         \item{description}{
          a description for the list (free text).
        }
        \item{tags}{
          a set of strings to use as tags.
        }
      }
     }
     
     \item{\code{union(ListManager-class, lists, name = NULL, description = NULL, tags = list())}:}{
     Creates new lists which contain all the members contained in the set of input lists.
      \arguments{
        \item{lists}{
          the source lists to be unified.
        }
         \item{name}{
          the name for the new list. If none is provided one will be generated. 
        }
         \item{description}{
          a description for the list (free text).
        }
        \item{tags}{
          a set of strings to use as tags.
        }
      }
     }

     \item{\code{difference(ListManager-class, lists, name = NULL, description = NULL, tags = list())}:}{
     Creates new lists which only contain members which are not shared by an even number of lists.
      \arguments{
        \item{lists}{
          the source lists.
        }
         \item{name}{
          the name for the new list. If none is provided one will be generated. 
        }
         \item{description}{
          a description for the list (free text).
        }
        \item{tags}{
          a set of strings to use as tags.
        }
      }
     }
     
     \item{\code{subtract(ListManager-class, lefts, rights, name = NULL, description = NULL, tags = list())}:}{
     Creates new lists which contain only those elements which are present in one set of lists, and none of those elements which are present in another set of lists. This is what is typically thought of as subtraction, or more technically, the asymmetric difference of two sets.
       \arguments{
        \item{lefts}{
          the source lists.
        }
        \item{rights}{
          the substraction lists.
        }
         \item{name}{
          the name for the new list. If none is provided one will be generated. 
        }
         \item{description}{
          a description for the list (free text).
        }
        \item{tags}{
          a set of strings to use as tags.
        }
      }
     }
  }
}

\author{
InterMine Team
}

\seealso{
\code{\link{ListManager-class}}, \code{\link{webservice-methods}}
}

\examples{
# get ListManager class
im <- initInterMine(listMines()["HumanMine"],"YOUR TOKEN")
im.human.list <- list_manager(im)
class(im.human.list)

# create "diabetesGenes" list, and another one without specifying the name 
query1Diabetes <- setQuery( 
  select = c("Gene.primaryIdentifier", "Gene.symbol"),
  where = setConstraints(
    paths = c("Gene.organism.name", "Gene.diseases.name"),
    operators = c("=", "CONTAINS"),
    values = list("Homo sapiens","diabetes")
  )
)
query1DiabetesResults <- runQuery(im, query1Diabetes)
create_list(im.human.list,content = query1DiabetesResults, list_type = "Gene", name = "diabetesGenes")

query2UpInPancreasConstraint = setConstraints(
  paths = c("Gene", 
            "Gene.proteinAtlasExpression.level", 
            "Gene.proteinAtlasExpression.level", 
            "Gene.proteinAtlasExpression.tissue.name"),
  operators = c("IN", rep("=", 3)),
  values = list("PL_Pax6_Targets", "Medium", "High", "Pancreas")
)
query2UpInPancreas = newQuery(
  view = c("Gene.primaryIdentifier",
           "Gene.symbol",
           "Gene.proteinAtlasExpression.cellType",
           "Gene.proteinAtlasExpression.level",
           "Gene.proteinAtlasExpression.tissue.name"
  ),
  constraintLogic = "A and (B or C) and D"
)
query2UpInPancreas$where <- query2UpInPancreasConstraint
query2UpInPancreasResults <-  runQuery(im = im, qry = query2UpInPancreas)
ids_query2UpInPancreasResults <- list(query2UpInPancreasResults["Gene.primaryIdentifier"])
create_list(im.human.list,ids_query2UpInPancreasResults,list_type="Gene") 
#this will create a list with the default name "my_list"

# example of intersect
intersect(im.human.list, c("diabetesGenes","my_list"),"intersect_list")

#example of union
union(im.human.list, c("diabetesGenes","my_list"), "union_list")

#example of difference
difference(im.human.list,c("diabetesGenes","my_list"),"diff_list")

#example of subtract
subtract(im.human.list,lefts = c("diabetesGenes"), rights = c("my_list"),"subtr_list")

# get "my_list" list
resp <- get_list(im.human.list, "my_list")

#delete "my_list" and "diabetesGenes" lists
delete_lists(im.human.list, c("my_list", "diabetesGenes"))
#trying to delete a list not created by the user gives an error
delete_lists(im.human.list, c("PL_DiabesityGWAS_pval-4"))
}