\name{ridit}
\alias{ridit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to implement ridit scoring
}
\description{
This function takes data
and applies ridit scoring as developed by Bross, 1958 and
revised by Brockett, et al, 2002. It returns a matrix of ridit scores
for all variables.
}
\usage{
ridit(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The system takes as an argument a data frame.
The first column should be a vector of IDs.
All other columns should be vectors of variables that can
be "scored" using ridit, meaning numberical variables.
Factors and binary variables should be converted to numerical
values (0,1 or 1,2,3,4,5 etc)
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
Bross, I. D. (1958). How to use ridit analysis. Biometrics, 18-38.

Brockett, P. L., Derrig, R. A., Golden, L. L., Levine, A., & Alpert, M. (2002).
  Fraud classification using principal component analysis of RIDITs.
  Journal of Risk and Insurance, 69(3), 341-371.

Lieberthal, R. D. (2008). Hospital quality: A PRIDIT approach.
  Health services research, 43(3), 988-1005.
}
\author{
Robert D. Lieberthal
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
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
