\name{bisulfiteConversionI}
\alias{bisulfiteConversionI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bisulfite Conversion I}
\description{
Quality control based on the bisulfite conversion I. The converted probes must be significally different from the unconverted probes in the green and in the red channel.
}
\usage{
bisulfiteConversionI(shinySummarizedData, plot = FALSE, p.treshold = 0.01, output_file = NULL)
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
These controls use Infinium I probe design and allele-specific single base extension
to monitor efficiency of bisulfite conversion. If the bisulfite conversion reaction was
successful, the "C" (Converted) probes will match the converted sequence and get
extended. If the sample has unconverted DNA, the "U" (Unconverted) probes will
get extended. There are no underlying C bases in the primer landing sites, except
for the query site itself. Performance of bisulfite conversion controls C1, C2 and C3
should be monitored in the Green channel, and controls C4, C5 and C6 should be
monitored in Red channel.
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

bisulfiteConversionI(summary)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Bisulfite Conversion }
\keyword{ Quality Control }% __ONLY ONE__ keyword per line
