\name{bisulfiteConversionII}
\alias{bisulfiteConversionII}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bisulfite Conversion II Quality Control
}
\description{
Quality control based on the bisulfite conversion II. If the bisulfite conversion reaction was successful, the "A" base will get incorporated and the probe will have intensity in the Red channel. If the sample has unconverted DNA, the "G" base will get incorporated accross the unconverted cytosine, and the probe will have elevated signal in the Green channel.
}
\usage{
bisulfiteConversionII(shinySummarizedData, plot = FALSE, p.treshold = 0.01, output_file = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{shinySummarizedData}{
Output of the shinySummarize function in the library shinyMethyl.
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
These controls use Infinium II probe design and single base extension to monitor
efficiency of bisulfite conversion. If the bisulfite conversion reaction was successful,
the "A" base will get incorporated and the probe will have intensity in the Red
channel. If the sample has unconverted DNA, the "G" base will get incorporated
across the unconverted cytosine, and the probe will have elevated signal in the
Green channel.
}
\value{
Return if each samples are outliers (TRUE) or not (FALSE) according to this quality control.
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

bisulfiteConversionII(summary)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
