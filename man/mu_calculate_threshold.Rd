% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalization.R
\name{mu_calculate_threshold}
\alias{mu_calculate_threshold}
\title{calculate imputed threshold}
\usage{
mu_calculate_threshold(intensity_df, value_transform = 1/2)
}
\arguments{
\item{intensity_df}{intensity values across many samples}

\item{value_transform}{how much to scale the value (default = 1/2)}
}
\value{
double
}
\description{
Calculate a reasonable threshold value to replace zeros
}
\details{
Not reported or missing values in our data cause all kinds of problems,
  and for data with proportional error, log-transforms mean we can't just set
  them to 0 either. In addition, just setting to zero inflates the differences
  of values. So, a reasonable value for noise is 1/2 of the lowest observed value
  in a *normal* like distribution. To achieve that for data with proportional data,
  we do a log-transform first. Because we want to use *all* the data across samples,
  we might have some weird outliers too. So we don't use the values directly
  from the distrubtion, but use `boxplot.stats` to get a reasonable handle
  on the distribution as well, and take a fraction of the lowest value in the
  distribution description.
}
