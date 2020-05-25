\name{get_od_data}
\alias{get_od_data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
get_od_data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function enables users to extract job flows from Residence Areas (Origin Areas) to Workplace areas (Destination Areas) for a chosen state and a chosen year. The function grabs the OD data, merges with a lookup of census block geographies and processes the dataset. The output dataframe contains a "Total_Jobs" column which indicates the total estimated job flows from a residence area to a workplace area. These dataframes are bigger than those obtained by get_wac_data or get_rac_data as they contain multiple rows of data for each residence area, corresponding to the various workplace areas that job flows occur between.


}
\usage{
get_od_data(state_name, year, main = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{state_name}{
%%     ~~Describe \code{x} here~~
Users need to give the lowercase abbreviated state name of any US state, to enable the function to grab OD data for that state.
}

  \item{year}{
%%
Users need to give a year between 2002-2017, to enable the function to grab the OD data for that year, and for the state identified
with the state_name argument.
  }

  \item{main}{
%%
As default, main is set to 'T', which means the function is grabbing OD flows where residence and workplace areas are in the same state. Setting main to 'F' will give OD data where the workplace areas are in the chosen state, but the residence areas are outside the state.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
Function returns a cleaned OD dataset for the chosen state and chosen year.
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
## Get OD data for Illinois, from 2014
df <- get_od_data("il", "2014", main = T)

## Get OD data for Illinois, from 2014, but where residence areas are outside Illinois state
df2 <- get_od_data("il", "2014", main = F)

}

