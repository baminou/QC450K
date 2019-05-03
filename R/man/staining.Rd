\name{staining}
\alias{staining}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Staining Quality Controls
}
\description{
Staining controls are used to examine the efficiency of the staining step in both the red and green channels. These controls are independent of the hybridization and extension step. The performance of the staining controls should be monitored in both the red channel and the green channel.

}
\usage{
staining(shinySummarizedData, sd.multiplier = 6, plot = FALSE, output_file = NULL)
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
If TRUE, the bisulfite conversion I is plotted
}
  \item{output_file}{
If != NULL, the plot will be save in the specified file.
}
}
\details{
This control is done by calculating the mean and standard variation of the background. The standard deviation is then multiplied by a selected multiplier and added to the mean. For each sample, we want to see if the intensity of Biotin and DNP is greater than the value found. If the value is greater, the sample passed the test. If not, it fails the test. You can add more stringency by increasing the standard deviation multiplier.
}
\value{
Return TRUE if the staining is correctly done for each sample.
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

staining(summary)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
