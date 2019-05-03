\name{targetRemoval}
\alias{targetRemoval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Target Removal Quality Controls
}
\description{
Quality control based on the target removal. The target removal must result in low signal compared to the hybridization controls in red and green channels.
}
\usage{
targetRemoval(shinySummarizedData, sd.multiplier = 6, plot = FALSE, output_file = NULL)
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
If TRUE, the target removal is plotted
}
  \item{output_file}{
If != NULL, the plot will be save in the specified file.
}
}
\value{
Return TRUE if the target removal is correctly done for each sample.
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

targetRemoval(summary)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
