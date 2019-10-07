library("httr")
library("jsonlite")
library("dplyr")
library("ggplot2")
library("lubridate")
library("proto")
library("scales")

regions <- function(){ 
  url <- "https://api.carbonintensity.org.uk/regional/"
  regions_are <- jsonlite::fromJSON(url)
  regions_are <- regions_are[["data"]][["regions"]][[1]][c(1,3)]
  return(regions_are)
}

check_fuel_region <- function(region = "Scotland"){
  #Make region to lowercase
  region <- tolower(region)
  #Check if region is valid
  regions <- function(){ 
    url <- "https://api.carbonintensity.org.uk/regional/"
    regions_are <- jsonlite::fromJSON(url)
    regions_are <- regions_are[["data"]][["regions"]][[1]][c(1,3)]
    regions_are <- as.data.frame(regions_are)
    regions_are$shortname <- tolower(regions_are$shortname)
    return(regions_are)
  }
  regions <- regions()
  stopifnot(region %in% regions$shortname)
  region_id <- regions[1][regions[2] == region] 
  #Make the new url
  url <- paste("https://api.carbonintensity.org.uk/regional/regionid/", region_id, sep = "")
  #Call the url and check http_status
  response <- httr::GET(url)
  stopifnot(httr::http_status(response)$category == "Success")
  #Read JSON
  resp_JSON <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  #Get Fuels
  energy_fuel <- as.data.frame(resp_JSON[["data"]][["data"]][[1]][["generationmix"]])
  return(energy_fuel)
}

pie_API <- function(region){
  df <- check_fuel_region(region)
    ggplot2::ggplot(data = df , ggplot2::aes(x ="" , y = perc , fill = fuel))+
    ggplot2::geom_bar(stat = "identity", color = "white")+ 
    ggplot2::coord_polar(theta = "y", start = 0)+
    ggplot2::geom_text(ggplot2::aes(label = perc), position = ggplot2::position_stack(vjust = 0.55), size = 4 , check_overlap = TRUE)+
    ggplot2::scale_fill_brewer(palette="Set1")+
    ggplot2::theme_void()
  
}

