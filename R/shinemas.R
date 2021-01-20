#' Query the database SHiNeMaS throught an API and format data for specific R packages
#'
#' @description
#' \code{shinemas} queries the database SHiNeMaS throught an API and formats data for specific R packages
#' 
#' @param db_url url of the database
#' 
#' @param user user name
#' 
#' @param password password
#' 
#' @param token token to connect to the database
#' 
#' 
#' @param query_type name of the query. Possible values are
#' \itemize{
#'  \item PPBstats
#'  \itemize{
#'   \item PPBstats_data_network_unipart_seed_lots
#'   \item PPBstats_data_agro
#'   \item PPBstats_data_agro_SR
#'   \item PPBstats_data_agro_HA
#'   }
#' }
#' 
#' @param specie filter: a vector of species to keep
#' 
#' @param project filter: a vector of projects to keep
#' 
#' @param variable filter: a vector of variables to keep. 
#' Each element must be of the form "name$type", name being the name of the variable in ShiNeMas and type being the type of variable as designated in ShiNeMaS
#' 
#' @param germplasm filter: a vector of germplasms to keep
#' 
#' @param germplasm_type filter: a vector of germplasm types to keep
#' 
#' @param relation_type filter: a vector of relation types to keep
#' 
#' @param location filter: a vector of locations to keep
#' 
#' @param year filter: a vector of years to keep
#' 
#' @return 
#' The function returns a dataframe formated for specific query type and packages
#'  
#' @details 
#' \itemize{
#'  \item \strong{SHiNeMaS}: The database SHiNeMaS (Seed History and Network Management System) is available here: \url{https://sourcesup.renater.fr/frs/?group_id=2295}.
#'  The query is based on a webservice developped within SHiNeMaS v2 by Laetitia Courgey and Yannick de Oliveira.
#'  The token can be taken through the web interface of SHiNeMaS by cliking "Get Token" on the top left side of the page.
#'  \item \strong{query_type}: The query_type is under the format [package-name]_[query-name].
#'  For example PPBstats_data_agro stands for data_agro format for package PPBstats.
#'  \item \strong{Package supported}:
#'  \itemize{
#'   \item See \url{https://priviere.github.io/shinemas2R/#packages-supported} for more details
#'   \item PPBstats
#'   \itemize{
#'    \item For query_type = PPBstats_data_agro and PPBstats_data_agro_HA 
#'     \itemize{
#'     \item filter year refers to sown year of seed-lot parent or year of selection of seed-lot parent
#'     \item relation_type is either reproduction or selection
#'    }
#'   }
#'  }
#' }
#' 
#' @examples 
#' \dontrun{
#' data_agro = shinemas2R::shinemas(
#'   db_url = "shinemas.local.seed",
#'   user = "wheat",
#'   password = "ppb",
#'   token = "1234",
#'   query_type = "PPBstats_data_agro"
#' )
#' 
#' library(PPBstats)
#' data = format_data_PPBstats(data_agro, type = "data_agro")
#' }
#' 
#' @author Pierre Riviere, Gaelle van Frank
#' 
#' @import httr
#' @import jsonlite
#' @import plyr
#' @import tidyverse
#' 
#' @export
#' 
shinemas = function(
  db_url = "shinemas.local.seed",
  user = "wheat",
  password = "ppb",
  token = "1234",
  query_type = "PPBstats_data_agro",
  specie = NULL,
  project = NULL,
  variable = NULL,
  germplasm = NULL,
  germplasm_type = NULL,
  relation_type = NULL,
  location = NULL,
  year = NULL
  ){
  
  match.arg(query_type, c(
    "PPBstats_data_network_unipart_seed_lots", 
    "PPBstats_data_agro", 
    "PPBstats_data_agro_SR", 
    "PPBstats_data_agro_HA")
    )
  
  # Set filters
  filters <- list()
  if(!is.null(specie)){filters <- c(filters, "species"=paste("[",paste(specie,collapse=","),"]",sep=""))}
  if(!is.null(project)){filters <- c(filters, "project"=paste("[",paste(project,collapse=","),"]",sep=""))}
  if(!is.null(variable)){filters <- c(filters, "variables"=paste("[",paste(variable,collapse=","),"]",sep=""))}
  if(!is.null(germplasm)){filters <- c(filters, "germplasm"=paste("[",paste(germplasm,collapse=","),"]",sep=""))}
  if(!is.null(germplasm_type)){filters <- c(filters, "germplasm_type"=paste("[",paste(germplasm_type,collapse=","),"]",sep=""))}
  if(!is.null(relation_type)){filters <- c(filters, "relation_type"=paste("[",paste(relation_type,collapse=","),"]",sep=""))}
  if(!is.null(location)){filters <- c(filters, "location"=paste("[",paste(location,collapse=","),"]",sep=""))}
  if(!is.null(year)){filters <- c(filters, "event_year"=paste("[",paste(year,collapse=","),"]",sep=""))}
  
  
  # Functions
  format_data_shinemas <- function(data){
    # Format data as a unnested dataframe
    d <- as_tibble(data$data)
    vec_columns <- c("species","projects","seed_lot_child","germplasm","seed_lot_parent","type","year","location","block","X","Y")
    D <-  d[,vec_columns]
    variables <- grep("raw_data", colnames(d))
    for(i in variables){
      t <- d[,c(vec_columns, colnames(d)[i])]
      VAR <- strsplit(colnames(d)[i],"[.]")[[1]][2]
      t <- unnest(t, cols = colnames(t)[ncol(t)])
      
      # Get all methods for an individual in one line
      t <- split(t, f=t$method)
      t <- lapply(t, function(x){
        colnames(x)[grep(paste("date","value",sep="|"), colnames(x))] = paste(VAR, unique(x$method),colnames(x)[grep(paste("date","value",sep="|"), colnames(x))],sep=".")
        x <- x[,-grep("method",colnames(x))]
        return(x)
      })
      
      # Merge values of different methods
      a <- Reduce(function(...) merge(..., all=T), t)
      D <- merge(D,a, all=T)
    }
    
    # Delete row with empty variable value
    to_get <- grep("value",colnames(D))
    if(length(to_get) > 0){
      D <- D %>% filter_at(vars(colnames(D)[to_get]),any_vars(!is.na(.)))
    }
    
    return(D)
  }
  get_data_from_shinemas = function(db_url, user, password, token, query, filters){
    url_shinemas = paste(db_url, "/wsR/",query,"?token=", token, sep = "")
    
    # Get data
    if(length(filters) > 0){
      data_shinemas <- httr::GET(url_shinemas, authenticate(user, password), query = filters)
    }else{
      data_shinemas <- httr::GET(url_shinemas, authenticate(user, password))
    }
    data <- jsonlite::fromJSON(httr::content(data_shinemas, as = "text"), flatten = T)
    if(query == "data_agro"){d <- format_data_shinemas(data)}

    Date <- data_shinemas$date
    if(status_code(data_shinemas) != 200){warning("There was something wrong with the query"); return(data)}
    
    # If several pages, request all pages
    if(data$information$nb_pages > 1){
      data <- lapply(1:data$information$nb_pages, function(i){
        if(length(filters) > 0){
          data_shinemas <- httr::GET(paste(url_shinemas,"&page=",i,sep=""), authenticate(user, password), query = filters)
        }else{
          data_shinemas <- httr::GET(paste(url_shinemas,"&page=",i,sep=""), authenticate(user, password))
        }
        d <- jsonlite::fromJSON(httr::content(data_shinemas, as = "text"))
        if(query == "data_agro"){d <- format_data_shinemas(d)}else{d <- d$data}
        d <- list("data" = d, "Status" = status_code(data_shinemas))
        return(d)
      })
    }
    
    if("information" %in% names(data)){  # Only 1 page
      D <- data$data
      info <- data$information
    }else{  # Several pages : concatenate all dataframes into one
      D <- lapply(data, function(d){return(d$data)})
      info <- lapply(data, function(d){return(d$Status)})
    }
    if(class(D) != "list"){D <- list(D)}
    d <- do.call(rbind.fill, D)
    info <- c(list("date" = Date, "filters" = filters), list("info" = info))
    
    return(list("data" = d, "information" = info))
    }
  
  
  # Queries
  # query_type == "PPBstats_data_network_unipart_seed_lots" ----------
  if( query_type == "PPBstats_data_network_unipart_seed_lots"){
    
    data = get_data_from_shinemas(db_url, user, password, token, query = "data_network_unipart_seed_lots", filters)
    
    colnames(data$data)[grep("project", colnames(data$data))] <- "project"
    
    vec_fac = c("specie", "project", "seed_lot_parent", "seed_lot_child", "relation_type", "relation_year_start", "relation_year_end", "germplasm_parent", 
                "location_parent", "year_parent", "germplasm_child", "location_child", "year_child", "species_parent",
                "project_parent", "germplasm_type_parent", 
                "comments_parent", "species_child", "project_child", "germplasm_type_child", "comments_child")
    for(v in vec_fac){ data$data[,v] = as.factor(as.character(data$data[,v])) }
    
    vec_num = c("long_parent", "lat_parent", "long_child", "lat_child", 
                "alt_parent", "total_generation_nb_parent", "local_generation_nb_parent", "generation_confidence_parent", 
                "alt_child", "total_generation_nb_chill", "local_generation_nb_child", "generation_confidence_child")
    for(v in vec_num){ data$data[,v] = as.numeric(as.character(data$data[,v])) }
    
  }
  
  # query_type == "PPBstats_data_agro" ----------
  if( query_type == "PPBstats_data_agro"){
    
    data = get_data_from_shinemas(db_url, user, password, token, query = "data_agro", filters)
    
    colnames(data$data)[grep("seed_lot_child", colnames(data$data))] <- "seed_lot"
    colnames(data$data)[grep("project", colnames(data$data))] <- "project"
    
    vec_fac = c("species", "project", "seed_lot", "seed_lot_parent", "location", "year", "germplasm", "block", "X", "Y")
    for(v in vec_fac){ data$data[,v] = as.factor(as.character(data$data[,v])) }
    
    if(FALSE){ ## Pas dans les noms de colonne
      vec_num = c("lat", "long")
      for(v in vec_num){ data$data[,v] = as.numeric(as.character(data$data[,v])) }
    }
    
  }
  
  # query_type == "PPBstats_data_SR" ----------
  if( query_type == "PPBstats_data_agro_SR"){
    
    data = get_data_from_shinemas(db_url, user, password, token, query = "data_agro_sr", filters)
    
    colnames(data$data)[grep("seed_lot_child", colnames(data$data))] <- "seed_lot"
    colnames(data$data)[grep("project", colnames(data$data))] <- "project"
    
    vec_fac = c("species", "project", "seed_lot", "seed_lot_parent", "location", "year", "germplasm", "block", "X", "Y", "expe_id", "group", "version")
    for(v in vec_fac){ data$data[,v] = as.factor(as.character(data$data[,v])) }
    
    vec_num = c("lat", "long")
    for(v in vec_num){ data$data[,v] = as.numeric(as.character(data$data[,v])) }
    
  }
  
  # query_type == "PPBstats_data_HA" ----------
  if( query_type == "PPBstats_data_agro_HA"){
    
    data = get_data_from_shinemas(db_url, user, password, token, query = "data_agro_ha", filters)
    
    colnames(data$data)[grep("seed_lot_child", colnames(data$data))] <- "seed_lot"
    colnames(data$data)[grep("project", colnames(data$data))] <- "project"
    
    vec_fac = c("species", "project", "seed_lot", "seed_lot_parent", "location", "year", "germplasm", "block", "X", "Y", "origin")
    for(v in vec_fac){ data$data[,v] = as.factor(as.character(data$data[,v])) }
    
    vec_num = c("lat", "long")
    for(v in vec_num){ data$data[,v] = as.numeric(as.character(data$data[,v])) }
    
  }
  
  return(data)
}
