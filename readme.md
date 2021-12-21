# Metabolomics Utilities

Provides a set of utilities useful for general working with metabolomics data.

## Installation

To install this package, use `devtools`.

```
git clone https://gitlab.cesb.uky.edu/rmflight/metabolomicsUtilities
cd metabolomicsUtilities
R
BiocInstaller::biocLite("IRanges")
devtools::install()
```

## Log Transform

Often we want to log-transform our data, and add a small constant to the data
to avoid `Inf` or `NA` values. The `log_with_min` function provides that.

```r
# read data into a matrix or vector
x <- data()
log_x <- log_with_min(x)
```

The default is to add a value that is 10exp-3 smaller than the smallest non-zero
value in the data.

## M/Z Values Within M/Z Ranges

If one has m/z ranges that you want to determine that m/z values are within 
(like our HPD sites), this is very useful. We make use of the `IRanges` package
from *Bioconductor*. Lets imagine you've generated a matrix where the first
column is the start of the m/z range, and the second is the end of the m/z range.

```r
# read in hpd sites from a set of json files
hpd_site_files <- dir("json_loc")
hpd_sites <- lapply(hpd_site_files, function(in_file){
  jsonlite::fromJSON(in_file)$stitch_regions
})

hpd_sites <- do.call(rbind, hpd_sites)

hpd_regions <- mz_to_iranges(hpd_sites)

raw_data <- read.table("raw_data_file")

raw_mz <- mz_to_iranges(raw_data$mz)

is_hpd <- raw_mz %within% hpd_regions
```

Note that in both cases, the **mz** gets multiplied by **10,000**, because the
`IRanges` expects integer based ranges. This factor of 10,000 should be enough
to preserve the correct number of digits to detect intersections.
