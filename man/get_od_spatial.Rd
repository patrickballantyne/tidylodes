\name{get_od_spatial}
\alias{get_od_spatial}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
get_od_spatial

}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Function that converts output of get_od_data or get_od_subset into a format that enables plotting of job flows from residence to workplace areas. The function creates centroids for each census block and joins the coordinates of these onto the workplace and residence census blocks.
}
\usage{
get_od_spatial(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{x} here~~
Requires as input an output dataframe of get_od_data or get_od_subset.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a dataframe with two individual simple feature collection columns containing the point geometries for the workplace area census blocks and the residence area census blocks.
}
\references{
%% ~put references to the literature/web site here ~
LODES data available to download manually from: https://lehd.ces.census.gov/data/lodes/LODES7/
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
## Get OD data for Illinois, for 2014
df <- get_od_data("il", "2014")

## Convert to format that enables linestring plotting
df2 <- get_od_spatial(df)
}

