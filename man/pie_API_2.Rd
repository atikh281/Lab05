% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataAPI.R
\name{pie_API_2}
\alias{pie_API_2}
\title{Pie plot, the electricity sources used between specified datetimes for a specified region.}
\usage{
pie_API_2(region, from, to)
}
\arguments{
\item{region}{A number (regionid) or a string (region short name). If nothing specified, the default is "Scotland". See \code{regions()} to know the admitted values.}

\item{from}{A string with shape yyyy-mm-ddTHH:mmZ, representing the start datetime.}

\item{to}{A string with shape yyyy-mm-ddTHH:mmZ, representing the end datetime. The end date time can be max. 1 month after \code{from} date.}
}
\value{
A pie chart showing different fuel sources and percentages. Represents the percentage of the fuels used to generate electricity in the specified datetime.
}
\description{
Pie plot, the electricity sources used between specified datetimes for a specified region.
}
\examples{
pie_API_2("London", from = "2018-11-16T06:00Z", to = "2018-12-15T06:00Z")
}
