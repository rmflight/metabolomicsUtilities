% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lipid-up-down-plots.R
\name{mu_summarize_up_down}
\alias{mu_summarize_up_down}
\title{summarize the up and down-changed}
\usage{
mu_summarize_up_down(log_df_annotations)
}
\arguments{
\item{log_df_annotations}{list output from `add_annotations`}
}
\value{
list
}
\description{
Given the output of `add_annotations`, gets the numbers of up- and down-changed
entries for each class, and then for the various `chain*` and `length*` within
each class.
}
