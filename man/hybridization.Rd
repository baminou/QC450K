\name{hybridization}
\alias{hybridization}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hybridaztion Quality Control
}
\description{
Hybridization controls test the overall performance of the Infinium Assay using synthetic targets instead of amplified DNA. These synthetic targets complement the sequence on the array perfectly, allowing the probe to extend on the synthetic target as a template.
Synthetic targets are present in the hybridization buffer (RA1) at three levels, monitoring the response from high-concentration (5 pM), medium concentration (1 pM), and low-concentration (0.2 pM) targets. All bead type IDs should result in signal with various intensities, correspondig to the concentrations of the initial synthetic targets.
The performance of the hybridization controls should be monitored only in the green channel.
}
\usage{
hybridization(shinySummarizedData, sd.multiplier = 6, plot = FALSE, output_file = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{shinySummarizedData}{
Output of the shinySummarize function in the library shinyMethyl.
}
  \item{sd.multiplier}{
A value to multiplie the standard deviation for changing stringency. Default=6
}
  \item{plot}{
If TRUE, the hybridization is plotted
}
  \item{output_file}{
If != NULL, the plot will be save in the specified file.
}
}
\details{
This control takes the hybridaztion probes on the red channel as low-level intensity. It compares if the intensity of the probes in the Green channel are higher than the mean of the red channel probes + the standard deviation * sd.multiplier.
}
\value{
Return TRUE if every concentrations are distinguishable.
}
\author{
Brice Aminou, brice.aminou@gmail.com
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
library("shinyMethyl")
library("minfiData")

summary <- shinySummarize(RGsetEx)

hybridization(summary)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
