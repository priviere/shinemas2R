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
#' @param specie filter: specie to keep
#' 
#' @param project filter: project to keep
#' 
#' @param variable filter: variable to keep
#' 
#' @param germplasm filter: germplasm to keep
#' 
#' @param germplasm_type filter: germplasm type to keep
#' 
#' @param relation_type filter: relation type to keep
#' 
#' @param location filter: location to keep
#' 
#' @param year filter: year to keep
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
  
  filters <- list("specie" = specie,
                  "project" = project,
                  "variable" = variable,
                  "germplasm" = germplasm,
                  "germplasm_type" = germplasm_type,
                  "relation_type" = relation_type,
                  "location" = location,
                  "year" = year)
  
  get_data_from_shinemas = function(db_url, user, password, token, query, filters){
    url_shinemas = paste(db_url, "/wsR/",query,"?token=", token, sep = "")
    
    # Set filters
    Filters <- list()
    if(!is.null(filters$specie)){Filters <- c(Filters, "species"=paste("[",filters$specie,"]",sep=""))}
    if(!is.null(filters$project)){Filters <- c(Filters, "project"=paste("[",filters$project,"]",sep=""))}
    if(!is.null(filters$variable)){Filters <- c(Filters, "variable"=paste("[",filters$variable,"]",sep=""))}
    if(!is.null(filters$germplasm)){Filters <- c(Filters, "germplasm"=paste("[",filters$germplasm,"]",sep=""))}
    if(!is.null(filters$germplasm_type)){Filters <- c(Filters, "germplasm_type"=paste("[",filters$germplasm_type,"]",sep=""))}
    if(!is.null(filters$relation_type)){Filters <- c(Filters, "relation_type"=paste("[",filters$relation_type,"]",sep=""))}
    if(!is.null(filters$location)){Filters <- c(Filters, "location"=paste("[",filters$location,"]",sep=""))}
    if(!is.null(filters$event_year)){Filters <- c(Filters, "event_year"=paste("[",filters$event_year,"]",sep=""))}
    
    # Get data
    if(length(Filters) > 0){
      data_shinemas <- httr::GET(url_shinemas, authenticate(user, password), query = Filters)
    }else{
      data_shinemas <- httr::GET(url_shinemas, authenticate(user, password))
    }
    data <- jsonlite::fromJSON(httr::content(data_shinemas, as = "text"), flatten = T)
    
    Date <- data_shinemas$date
    if(status_code(data_shinemas) != 200){warning("There was something wrong with the query"); return(data)}
    
    # If several pages, request all pages
    if(data$information$nb_pages > 1){
      data <- lapply(1:data$information$nb_pages, function(i){
        if(length(Filters) > 0){
          data_shinemas <- httr::GET(paste(url_shinemas,"&page=",i,sep=""), authenticate(user, password), query = Filters)
        }else{
          data_shinemas <- httr::GET(paste(url_shinemas,"&page=",i,sep=""), authenticate(user, password))
        }
        d <- jsonlite::fromJSON(httr::content(data_shinemas, as = "text"))
        d$information <- c(d$information, list("Status" = status_code(data_shinemas)))
        return(d)
      })
    }
    
    if("information" %in% names(data)){  # Only 1 page
      D <- data$data
      info <- data$information
    }else{  # Several pages : concatenate all dataframes into one
      D <- lapply(data, function(d){return(d$data)})
      info <- lapply(data, function(d){return(d$information)})
    }
    d <- do.call(rbind.fill, D)
    info <- c(list("date" = Date, "filters" = Filters), list("info" = info))
    
    return(list("data" = d, "information" = info))
    }
  
  # query_type == "PPBstats_data_network_unipart_seed_lots" ----------
  if( query_type == "PPBstats_data_network_unipart_seed_lots"){
    
    data = get_data_from_shinemas(db_url, user, password, token, query = "data_network_unipart_seed_lots", filters)
    
    vec_fac = c("specie", "project", "seed_lot_parent", "seed_lot_child", "relation_type", "relation_year_start", "relation_year_end", "germplasm_parent", 
                "location_parent", "year_parent", "germplasm_child", "location_child", "year_child", "species_parent", "project_parent", "germplasm_type_parent", 
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
    
    vec_fac = c("species", "project", "seed_lot", "seed_lot_parent", "location", "year", "germplasm", "block", "X", "Y")
    for(v in vec_fac){ data$data[,v] = as.factor(as.character(data$data[,v])) }
    
    if(FALSE){ ## Pas dans les noms de colonne
      vec_num = c("lat", "long")
      for(v in vec_num){ data$data[,v] = as.numeric(as.character(data$data[,v])) }
    }
    
  }
  
  # query_type == "PPBstats_data_SR" ----------
  if( query_type == "PPBstats_data_agro_SR"){
    
    data = get_data_from_shinemas(db_url, user, password, token, query = "data_agro_SR", filters)
    
    vec_fac = c("specie", "project", "seed_lot", "location", "year", "germplasm", "block", "X", "Y", "expe_id", "group", "version")
    for(v in vec_fac){ data$data[,v] = as.factor(as.character(data$data[,v])) }
    
    vec_num = c("lat", "long")
    for(v in vec_num){ data$data[,v] = as.numeric(as.character(data$data[,v])) }
    
  }
  
  # query_type == "PPBstats_data_HA" ----------
  if( query_type == "PPBstats_data_agro_HA"){
    
    data = get_data_from_shinemas(db_url, user, password, token, query = "data_agro_HA", filters)
    
    vec_fac = c("specie", "project", "seed_lot", "location", "year", "germplasm", "block", "X", "Y", "origin", "version")
    for(v in vec_fac){ data$data[,v] = as.factor(as.character(data$data[,v])) }
    
    vec_num = c("lat", "long")
    for(v in vec_num){ data$data[,v] = as.numeric(as.character(data$data[,v])) }
    
  }
  
  return(data)
}
