\name{specificityII}
\alias{specificityII}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Specificity II Quality Control
}
\description{
Specificity controls are designed to monitor potential non-specific primer extension for Infinium I and Infinium II assay probes. Specificity controls are designed against non-polymorphic T sites.
These controls are designed to monitor extension specificity for Infinium II probes and check for potentioal non-specific detection of methylation signal over unmethylated background. Specificity II probes should incorporate the "A" base across the non-polymorphic T and have intensity in the Red channel. In case of non-specific incorporation of the "G" base, the probe will have elevated signal in the green channel.
}
\usage{
specificityII(shinySummarizedData, channel = 0, plot = FALSE, p.treshold = 0.01, min.intensity = 2000, output_file = NULL)
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
  \item{min.intensity}{
Minimum intensity required to detect a signal.
}
  \item{output_file}{
If != NULL, the plot will be save in the specified file.
}
}
\value{
Return TRUE if all specific sites are distinguishable.
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

specificityII(summary)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
