\name{negative}
\alias{negative}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Negative Quality Control
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
negative(shinySummarizedData, channel = 0, plot = FALSE, p.treshold = 0.01, output_file = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{shinySummarizedData}{
%%     ~~Describe \code{shinySummarizedData} here~~
}
  \item{channel}{
%%     ~~Describe \code{channel} here~~
}
  \item{plot}{
%%     ~~Describe \code{plot} here~~
}
  \item{p.treshold}{
%%     ~~Describe \code{p.treshold} here~~
}
  \item{output_file}{
%%     ~~Describe \code{output_file} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (shinySummarizedData, channel = 0, plot = FALSE, p.treshold = 0.01, 
    output_file = NULL) 
{
    channelErrorVerification(channel)
    answer <- TRUE
    set.seed(2)
    if (channel == 0) {
        neg <- data.frame(shinySummarizedData@greenControls$NEGATIVE)
    }
    else {
        neg <- data.frame(shinySummarizedData@redControls$NEGATIVE)
    }
    if (plot == T) {
        if (!is.null(output_file)) {
            png(filename = output_file)
        }
        if (channel == 0) {
            matplot(colMeans(neg), type = "p", pch = 22, col = "black", 
                bg = rainbow(1), main = "NEGATIVE - Green", ylab = "Intensity", 
                xlab = "Sample", ylim = c(0, max(colMeans(neg))))
        }
        else {
            matplot(colMeans(neg), type = "p", pch = 22, col = "black", 
                bg = rainbow(1), main = "NEGATIVE - Red", ylab = "Intensity", 
                xlab = "Sample", ylim = c(0, max(colMeans(neg))))
        }
        points(colSds(as.matrix(neg)), type = "p", pch = 22, 
            col = "black", bg = "black")
        abline(v = 1:(length(summarized.data@sampleNames)), col = "grey")
        legend("topright", c("Average", "StdDev"), pch = 22, 
            pt.bg = c(rainbow(1), "black"))
        if (!is.null(output_file)) {
            dev.off()
        }
    }
    return(FALSE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
