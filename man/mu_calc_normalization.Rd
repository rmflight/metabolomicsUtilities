% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalization.R
\name{mu_calc_normalization}
\alias{mu_calc_normalization}
\title{calculate sample normalization}
\usage{
mu_calc_normalization(intensity_df)
}
\arguments{
\item{intensity_df}{the data.frame of intensities}
}
\value{
data.frame
}
\description{
Given a set of peak intensities, calculate normalization factors using
the log-median of the intensities.
}
