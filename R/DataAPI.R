library("httr")
library("jsonlite")
library("dplyr")
library("ggplot2")
library("lubridate")

check_fuel_region <- function(region){
  #Make region to lowercase
  region <- tolower(region)
  #Check if region is valid
  regions <- function(){ 
    url <- "https://api.carbonintensity.org.uk/regional/"
    regions_are <- jsonlite::fromJSON(url)
    regions_are <- tolower(as.vector(regions_are$data$regions[[1]][["shortname"]]))
    return(regions_are)
  }
  regions <- regions()
  stopifnot(region %in% regions)
  #Make the new url
  url <- paste("https://api.carbonintensity.org.uk/regional/", region, sep = "")
  #Call the url and check http_status
  response <- httr::GET(url)
  stopifnot(httr::http_status(response)$category == "Success")
  #Read JSON
  resp_JSON <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  #Get Fuels
  energy_fuel <- as.data.frame(resp_JSON$data$data[[1]][["generationmix"]])
  return(energy_fuel)
}

plot_fuels_region <- function(region){
  df <- check_fuel_region(region)
  ggplot2::ggplot(data = df, ggplot2::aes(x = fuel, y = perc)) +
    ggplot2::geom_point(ggplot2::aes(size = perc, col = fuel, alpha = perc))
}