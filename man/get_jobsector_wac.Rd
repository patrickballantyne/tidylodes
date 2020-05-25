\name{get_jobsector_wac}
\alias{get_jobsector_wac}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
get_jobsector_rac
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function allows users to subset the output dataframe of get_wac_data to focus on one specific job sector. This will be useful for those
using WAC data to perform analysis on the geography of specific job types.
}
\usage{
get_jobsector_wac(df, job_code, job_proportion = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{x} here~~
The input for this function is the output dataframe from get_wac_data.
}

  \item{job_code}{
%%
Here the user can select a specific job code from the output dataframe of get_wac_data, which enables the function to drop all other
job sectors, but keep total jobs and the chosen job sector. Users should use colnames(df) to obtain a list of all available job codes.
  }

  \item{job_proportion}{
%%
This argument enables users to calculate the proportion of the chosen job sector in relation to the total number of jobs in each census block.
By default the argument is 'T', so will calculate a job_proportion column unless set to 'F'.
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
A dataframe of cleaned WAC data, focusing on a specific job sector. If job_proportion = T then the dataframe will also contain an additional
column where the proportion of total jobs that the chosen job sector occupies is calculated.
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
## Users need to use the get_wac_data function first:
df <- get_wac_data("de", "2015")
## Get a list of all available job sectors
colnames(df)

## Use the get_jobsector_wac function to extract retail trade jobs, and calculate the proportion of retail trade jobs
df2 <- get_jobsector_wac(df, job_code = "Retail_Trade", job_proportion = T)
}

