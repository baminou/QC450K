\name{QC450k-package}
\alias{QC450k-package}
\alias{QC450k}
\docType{package}
\title{
Quality Control for the 450k array
}
\description{
This package has functions for quality control testing of the Illumina 450k array. The quality control
is based on the control probes on the array and uses the value of the idat files. The tests done are for:
- Bisulifte conversion I
- Bisulfite conversion II
- Extension
- Hybridization
- Negative
- Non-Polymoriphic
- Specificity I
- Specificity II
- Staining
- Target Removal
. All the tests are more define in the functions descriptions.
}
\details{
\tabular{ll}{
Package: \tab QC450k\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-04-07\cr
License: \tab What license is it under?\cr
}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Brice Aminou

Maintainer: Brice Aminou <brice.aminou@gmail.com>
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\examples{
library(shinyMethyl)
library(minfiData)

summary <- shinySummarize(RGsetEx)

staining(summary,plot=TRUE)
extension(summary,plot=TRUE)
hybridization(summary,plot=TRUE)
}
