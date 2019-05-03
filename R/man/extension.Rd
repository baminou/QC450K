\name{extension}
\alias{extension}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extension Quality Control
}
\description{
Extension controls test the extension efficiency of A, T, C and G nucleotides from a hairpin probe, and are therefore sample-independent. The performance of the extension conrols should be monitored in the red (A,T) and green (C,G) channels.
}
\usage{
extension(shinySummarizedData, sd.multiplier = 6, plot = FALSE, output_file = NULL)
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
If TRUE, the extension is plotted
}
  \item{output_file}{
If != NULL, the plot will be save in the specified file.
}
}
\details{
This control test is done by taking in the green channel the intensity of C and G in the green channel and A and T in the red channel as low level intensity. It takes the mean and add the multiplied standard deviation to see if the intensity detected for the bases C, G (Green) or A, T (Red) is different.
}
\value{
Return TRUE if each extensions are distinguishable accross each samples.
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

extension(summary)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Extension }
\keyword{ Quality Control }% __ONLY ONE__ keyword per line
