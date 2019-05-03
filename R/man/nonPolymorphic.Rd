\name{nonPolymorphic}
\alias{nonPolymorphic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Non-polymorphic Quality Controls
}
\description{
Non-polymorphic controls test the overall performance of the assay, from amplification to detection, by querying a particular base in a non-polymorphic region of the genome. One non-polymorphic control has been designed for each of the four nucleotides (A, T, C, G).
}
\usage{
nonPolymorphic(shinySummarizedData, sd.multiplier = 6,plot = FALSE, output_file = NULL)
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
If TRUE, the non-polymorphic control is plotted
}
  \item{output_file}{
If != NULL, the plot will be save in the specified file.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Return if the intensity is distinguishable (TRUE) between bases. 
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

nonPolymorphic(summary)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
