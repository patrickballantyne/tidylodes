\name{get_od_subset}
\alias{get_od_subset}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
get_od_subset
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Function that takes the output of get_od_data, and subsets it to include only rows of data where flows are greater than a specified threshold.
}
\usage{
get_od_subset(df, flow_threshold)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{x} here~~
Function requires as input the output dataframe of get_od_data
}

  \item{flow_threshold}{
%%
Function requires specification of a threshold of job flows. The function will subset the dataframe based on this value, to keep only rows of data
with flows greater than the specified value.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
Returns a dataframe with identical column structure of outputs of get_od_data, but with fewer rows as determined by value.
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

## Subset to include only flows of over 45
df2 <- get_od_subset(df, flow_threshold = 45)
}
