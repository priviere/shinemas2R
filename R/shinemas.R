#' Query SHiNeMaS and format data for specific R packages
#'
#' @description
#' \code{shinemas} queries SHiNeMaS and formats data for specific R packages
#' 
#' @param db url of the database
#' 
#' @param id id to connect to the database
#' 
#' @param pwd password to connect to the database
#' 
#' @param query_type name of the query. Possible values are
#' \itemize{
#'  \item PPBstats_data_network_unipart_seed_lot
#'  \item PPBstats_data_agro
#'  \item PPBstats_data_SR
#'  \item PPBstats_data_HA
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
#' The function return a dataframe formated for specific query type and packages
#'  
#' @details 
#' The database SHiNeMaS (Seed History and Network Management System) is available here: \url{https://sourcesup.renater.fr/frs/?group_id=2295}
#' 
#' The query is based on a webservice ... TO COMPLETE
#' 
#' The query_type is under the format [package-name]_[query-name].
#' For example PPBstats_data_agro stands for data_agro format for package PPBstats.
#' 
#' PPBstats is an  R package for Participatory Plant Breeding statisticial analyses. 
#' More information are available here: \url{https://priviere.github.io/PPBstats_web_site/}
#' 
#' @examples 
#' \dontrun{
#' data_agro = shinemas(
#' db = "shinemas.local.seed",
#' id = "user",
#' pwd = "toto",
#' query = "PPBstats_data_agro"
#' )
#' 
#' library(PPBstats)
#' data = format_data_PPBstats(data_agro, type = "data_agro")
#' }
#' 
#' @author Pierre Riviere and Laetitia Courgey
#' 
#' @export
#' 
shinemas = function(
  db = "shinemas.local.seed",
  id = "user",
  pwd = "toto",
  query_type = "data_agro",
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
    "PPBstats_data_network_unipart_seed_lot", 
    "PPBstats_data_agro", 
    "PPBstats_data_SR", 
    "PPBstats_data_HA")
    )
  
  if( query_type == "PPBstats_data_network_unipart_seed_lot"){
    
  }
  
  if( query_type == "PPBstats_data_agro"){
    
  }
  
  if( query_type == "PPBstats_data_SR"){
    
  }
  
  if( query_type == "PPBstats_data_HA"){
    
  }
  
  data = NULL
    
  return(data)
}
