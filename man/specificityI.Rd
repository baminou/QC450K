\name{specificityI}
\alias{specificityI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Specificity I Quality Control
}
\description{
Quality control based on the specificity controls. The converted probes must be significally different from the unconverted probes in the green and in the red channel.
}
\usage{
specificityI(shinySummarizedData, channel = 0, plot = FALSE, p.treshold = 0.01, output_file = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{shinySummarizedData}{
Output of the shinySummarize function in the library shinyMethyl.
}
  \item{channel}{
0=Green, 1=Red
}
  \item{plot}{
If TRUE, the bisulfite conversion I is plotted
}
  \item{p.treshold}{
P-value treshold for outliers. Usually 0.01 or 0.05.
}
  \item{output_file}{
If != NULL, the plot will be save in the specified file.
}
}
\details{
Specificity controls are designed to monitor potential non-specific primer extension
for Infinium I and Infinium II assay probes. Specificity controls are designed against
non-polymorphic T sites.
}
\value{
Return TRUE if PM are distinguishable from background.
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

specificityI(summary)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
