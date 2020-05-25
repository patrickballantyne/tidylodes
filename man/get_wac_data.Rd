\name{get_wac_data}
\alias{get_wac_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
get_wac_data
}
\description{
This function enables users to extract datasets on Workplace Area Characteristics (WAC) at a census block level for a chosen state, and
for a chosen year between 2002-17. The WAC dataset(s) give employment estimates for a variety of specific job sectors (e.g. manufacturing),
and estimates of total jobs, for each census block that is classified as a 'workplace area'. This function grabs WAC data for the state and year
chosen, and cleans it returning the output as a dataframe.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_wac_data(state_name, year)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{state_name}{
%%     ~~Describe \code{x} here~~
Users need to give the lowercase abbreviated state name of any US state, to enable the function to grab WAC data for that state.
  }
  \item{year}{
%%
Users need to give a year between 2002-2017, to enable the function to grab the WAC data for that year, and for the state identified
with the state_name argument.
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
A dataframe of cleaned WAC data for the chosen state and chosen year.
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
## e.g. Get WAC data for delaware from 2015
df <- get_wac_data("de", "2015")
}

