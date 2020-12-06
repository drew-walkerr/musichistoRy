\name{get_history}
\alias{get_history}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{get_history
%%  ~~function to do ... ~~
}
\description{
This function pulls the listening history of a last.fm username. Be sure to put username in quotes!
Returns a dataframe titled my_data of user listening history.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_history(user)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{user}{a character string representing the last.fm username
%%     ~~Describe \code{x} here~~
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
\references{uses scrobbler::download_scrobbles(), pulling API Key from package API
%% ~put references to the literature/web site here ~
}
\author{Drew Walker
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
get_history("mylastfmusername")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
