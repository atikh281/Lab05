library("httr")
library("jsonlite")
library("dplyr")
library("ggplot2")
library("lubridate")
library("testthat")

#' @title A function to know the regions admitted by check_fuel_region_time().
#' @return  A data frame with two columns, regionid and shortname. 
#' @export regions
regions <- function(){ 
  url <- "https://api.carbonintensity.org.uk/regional/"
  regions_are <- jsonlite::fromJSON(url)
  regions_are <- regions_are[["data"]][["regions"]][[1]][c(1,3)]
  return(regions_are)
}

#' @title Get the electricity sources used between specified datetimes for a specified region.
#' @param region A number (regionid) or a string (region short name). If nothing specified, the default is "Scotland". See \code{regions()} to know the admitted values.
#' @param from A string with shape yyyy-mm-ddTHH:mmZ, representing the start datetime. 
#' @param to A string with shape yyyy-mm-ddTHH:mmZ, representing the end datetime. The end date time can be max. 1 month after \code{from} date. 
#' @return  A data frame with two columns, fuel and percentage. Which represents the percentage of the fuels used to generate electricity in the specified datetime.  
#' @examples
#' check_fuel_region_time()
#' check_fuel_region_time("london", from = "2018-11-16T06:00Z", to = "2018-12-15T06:00Z")
#' @export check_fuel_region_time

check_fuel_region_time <- function(region = "Scotland", from = "now", to = "tomorrow"){
  stopifnot(is.character(region) || is.numeric(region), is.character(from), is.character(to))
  
  #Check if region is valid
  if (is.character(region) == TRUE) {
    
    #Make region to lowercase
    region <- tolower(region)
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
  }
  else{
    region_id <- region
  }
  #Get London date and time
  if (from == "now") {
    urltime <- "http://worldtimeapi.org/api/timezone/Europe/London"
    from <- jsonlite::fromJSON(urltime)
    from <- lubridate::as_datetime(from$datetime)
    from <- lubridate::ymd_hms(from)
    fromtext <- format.Date(from, c("%Y-%m-%dT%H:%mZ"))
  }
  
  else{
    numvector <- c(0:9)
    stopifnot(
      nchar(from) == 17, 
      strsplit(from, "")[[1]][11] == "T", 
      strsplit(from, "")[[1]][17] == "Z",
      sum(strsplit(from, "")[[1]][1:17] %in% numvector) == 12
    )
    from <- lubridate::parse_date_time(from, "%y-%m-%d %H:%M")
    fromtext <- format.Date(from, c("%Y-%m-%dT%H:%mZ"))
  }
  
  if (to == "tomorrow"){
    to <- from + 24 * 3600
    totext <- format.Date(to, c("%Y-%m-%dT%H:%mZ"))
  }
  
  else {
    numvector <- c(0:9)
    stopifnot(
      nchar(to) == 17, 
      strsplit(to, "")[[1]][11] == "T", 
      strsplit(to, "")[[1]][17] == "Z",
      sum(strsplit(to, "")[[1]][1:17] %in% numvector) == 12
      )
    to <- lubridate::parse_date_time(to, "%y-%m-%d %H:%M")
    totext <- format.Date(to, c("%Y-%m-%dT%H:%mZ"))
  }
  
  url <- paste("https://api.carbonintensity.org.uk/regional/intensity", fromtext, totext, "regionid", region_id, sep = "/")
  #Call the url and check http_status
  response <- httr::GET(url)
  stopifnot(httr::http_status(response)$category == "Success")
  #Read JSON
  resp_JSON <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  resp_JSON <- resp_JSON[["data"]][["data"]][["generationmix"]]
  
  energydf <- matrix(nrow = length(resp_JSON), ncol = length(resp_JSON[[1]][[2]]))
  for (i in 1:length(resp_JSON)){
    for (k in 1:length(resp_JSON[[i]][[2]])){
      energydf[i,k] <- resp_JSON[[i]][[2]][k]
    }
  }
  namesvector <- resp_JSON[[1]][[1]]
  colnames(energydf) <- namesvector
  energy_fuel_day <- as.data.frame(energydf)
  #Get Fuels
  energy_fuel <- data.frame(fuel = namesvector, perc = unname(colSums(energydf)/sum(energydf)*100), stringsAsFactors = FALSE)
  return(energy_fuel)
  }

#' @title Pie plot, the electricity sources used between specified datetimes for a specified region.
#' @param region A number (regionid) or a string (region short name). If nothing specified, the default is "Scotland". See \code{regions()} to know the admitted values.
#' @param from A string with shape yyyy-mm-ddTHH:mmZ, representing the start datetime. 
#' @param to A string with shape yyyy-mm-ddTHH:mmZ, representing the end datetime. The end date time can be max. 1 month after \code{from} date. 
#' @return  A pie chart showing different fuel sources and percentages. Represents the percentage of the fuels used to generate electricity in the specified datetime.  
#' @examples
#' pie_API_2("London", from = "2018-11-16T06:00Z", to = "2018-12-15T06:00Z")
#' @export pie_API_2



pie_API_2 <- function(region, from, to){
  df <- check_fuel_region_time(region, from, to)
  ggplot2::ggplot(data = df , ggplot2::aes(x ="" , y = perc , fill = fuel))+
    ggplot2::geom_bar(stat = "identity", color = "white")+ 
    ggplot2::coord_polar(theta = "y", start = 0)+
    ggplot2::geom_text(ggplot2::aes(label = perc), position = ggplot2::position_stack(vjust = 0.55), size = 4 , check_overlap = TRUE)+
    ggplot2::scale_fill_brewer(palette="Set1")+
    ggplot2::theme_void()
}
 

test_that("Check that column 1 is Numeric", {
  expect_equal(is.numeric(regions()$regionid), TRUE)
})
test_that("Check that column 2 is character", {
  expect_equal(is.character(regions()$shortname), TRUE)
})

test_that("Check that sum of perc is 100", {
  expect_equal(sum(check_fuel_region_time()$perc), 100)
})

test_that("Check the different kinds of fuel", {
  expect_equal(check_fuel_region_time()$fuel,c("biomass","coal","imports","gas","nuclear","other","hydro","solar","wind"))
})

