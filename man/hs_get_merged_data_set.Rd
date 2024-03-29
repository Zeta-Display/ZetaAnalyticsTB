% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/00_helper_data_preparation.R
\name{hs_get_merged_data_set}
\alias{hs_get_merged_data_set}
\title{Generates merged data set on sales.}
\usage{
hs_get_merged_data_set(
  data_sales1,
  data_sales2,
  list_test_weeks,
  time_frq = "weekly"
)
}
\arguments{
\item{data_sales1}{data on sales of relevant products (per butik, vecka,
datum, and timme)}

\item{data_sales2}{data on sales of all products (per butik, vecka, datum,
and timme)}

\item{list_test_weeks}{output from \code{\link[=get_meta_data]{get_meta_data()}}}

\item{time_frq}{character setting the time frequency of the data set
to be merged: either 'weekly' which generates weekly frequencies and counts
of relevant products per zoner, kampanj and skylttyp or 'daily' for daily
counts and frequencies}
}
\value{
the data set computed as outlined in the description
}
\description{
Uses first argument to get information on sales (count) of relevant products
and second data set on counts of all products sold, and both to compute
conversion rates (count/total) of relevant products. The third arguments
identifies the different product sets of "relevant products" depending on
zoner, kampanj or skylttyp.
}
