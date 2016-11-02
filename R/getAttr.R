#' getAttr function
#'
#' Description: This function retrieves all or some attributes and relationships of one or more biological elements
#' Parametres
#' @param attributes. Vector of attributes to be obtained
#' @param filters. Vector of filters to make specific searches
#' @param values. Vector of values associated with each filter
#' @param mart. Type of information to be retrieved. (Gene, Promoter, TU, Operon, ...)
#' @param cond. Type of logical condition  AND or OR
#' @keywords cats
#' @export
#' @examples
#' getAtt(attributes = c("gene_id", "gene_name", "gene_type"),
#' filters = c("gene_strand", "gene_type"),
#' values = c("forward", "Pseudo Gene"),
#' mart = "gene",
#' cond = AND)

library("RSQLite")
library("roxygen2")
library("devtools")


#paths
regulon<-dbConnect(SQLite(),dbname="/home/cbarbere/Descargas/Lab_Com/regulondb_92_sqlite3.db")

getAttr<-function(attributes,filters = NULL, values = NULL, mart, cond= "AND"){
  ###Check if mart exist
  if(!dbExistsTable(regulon, mart)){
    stop("The mart you use dont exist in the database",call.= FALSE)
  }
  ###Check if theres filters to use
 # if (filters==NULL & values==NULL) {
  # query=paste("SELECT",attri," FROM", mart,";")
  # return(dbGetQuery(regulon, query))
 # }
  ##Check the values and the filters are the same size
  if (length(filters)!=length(values)){
    stop("The filters and the values arent the same size")
    }
  ###Check if filters exists
  if (!(all(filters%in% (dbListFields(regulon,mart))))){
    stop("The filters you use dont exist in the mart. You can use ListFilters to check the filters in your attribute",call.=FALSE)
  }
  ###Chek if al attributes exists
  if (!(all(attributes%in% (dbListFields(regulon,mart))))){
    stop("The atributes you use dont exist in the mart. You can use listAttributes() to make check the attributes in you mart",call.=FALSE)
  }
  ###Generar querry
  attri<-paste(attributes, collapse="," )
  x<-paste(" ",filters[1], " = '",values[1],"'", sep="")
  #mapply :( )
  for (i in 2:length(filters)) {
    x<-c((paste(" ", filters[i], " = '",values[i],"' ",sep="") ),x)
  }
  condition<-paste(x,collapse=cond )
  query=paste("SELECT",attri," FROM", mart, "WHERE" ,condition,";")
  DF<-dbGetQuery(regulon, query)
  if (is.na(DF[1,1])){
      stop("You currend query dont have any result")
    }else{
      return (DF)
    }
  }
