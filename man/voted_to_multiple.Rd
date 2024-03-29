% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classification_voting.R
\name{voted_to_multiple}
\alias{voted_to_multiple}
\title{transform multiple to a single entry}
\usage{
voted_to_multiple(voted_categories, split_by = "emf")
}
\arguments{
\item{voted_categories}{data.frame of voted results}

\item{split_by}{the variable to use the split on "emf"s, default is "emf"}
}
\value{
data.frame with "multiple" replacing tied entries
}
\description{
Takes classifications where there are multiple tied max entries, and transforms them into a "multiple".
}
