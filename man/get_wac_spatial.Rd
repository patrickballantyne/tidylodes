\name{get_wac_spatial}
\alias{get_wac_spatial}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
get_wac_spatial
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function allows users to convert output dataframe(s) of get_wac_data or get_jobsector_wac into simple features, by joining the dataframe onto
a census block simple feature for the state of interest, using the TIGRIS package to obtain the census blocks sf.
}
\usage{
get_wac_spatial(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{x} here~~
This function requires as input a cleaned WAC dataframe either as a direct output of get_wac_data, or of get_jobsector_wac.
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
Returns WAC data in a spatial (simple features) format, that can be easily manipulated, mapped or used in spatial operations.
}
\references{
%% ~put references to the literature/web site here ~
LODES data available to download manually from: https://lehd.ces.census.gov/data/lodes/LODES7/ For more information on the TIGRIS package
visit: https://CRAN.R-project.org/package=tigris
}
\author{
%%  ~~who you are~~
Ballantyne, Patrick
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Obtain a WAC dataset
df <- get_wac_data("ak", "2013")

## Convert to simple features
sf <- get_wac_spatial(df)
}
