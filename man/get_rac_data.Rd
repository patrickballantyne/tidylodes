\name{get_rac_data}
\alias{get_rac_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
get_rac_data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function enables users to extract datasets on Residence Area Characteristics (RAC) at a census block level for a chosen state, and
for a chosen year between 2002-17. The RAC dataset(s) give employment estimates for a variety of specific job sectors (e.g. manufacturing),
and estimates of total jobs, for each census block that is classified as a 'residence area'. This function grabs RAC data for the state and year
chosen, and cleans it returning the output as a dataframe.
}
\usage{
get_rac_data(state_name, year)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{state_name}{
%%     ~~Describe \code{x} here~~
Users need to give the lowercase abbreviated state name of any US state, to enable the function to grab RAC data for that state.
}

  \item{year}{
%%
Users need to give a year between 2002-2017, to enable the function to grab the RAC data for that year, and for the state identified
with the state_name argument.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
A dataframe of cleaned RAC data for the chosen state and chosen year.
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
## e.g. Get RAC data for New Jersey from 2008
df <- get_rac_data("nj", "2008")
}


