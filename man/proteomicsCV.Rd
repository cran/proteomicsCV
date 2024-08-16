\name{proteomicsCV}
\alias{protLogCV}
\alias{protCV}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
protLogCV
protCV
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Calculates the percentage CV for intensity based proteomic data.
}
\usage{
protLogCV(logData, base)
protCV(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
    input dataframe of the intensity values.
  }
  \item{logData}{
    input dataframe of the log transformed intensity values.
  }
  \item{base}{
    numerical base of the logarithm that was used to transform the data. Values that are accepted are 2 (for log2) and 10 (for log10)
  }
}
%-\details{
%%  ~~ If necessary, more details than the description above ~~
%-}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
returns a list of percentage CVs
}
%-\references{
%% ~put references to the literature/web site here ~
%-}
\author{
%%  ~~who you are~~
Alejandro J. Brenes
}

%-\note{
%%  ~~further notes~~
%-}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%-\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%-}
\examples{

library(proteomicsCV)
log_intensity_df<-data.frame(intensity1=c(16.93157,26.57542,19.90749,18.86056),
                         intensity2=c(17.16546,27.77706,19.45854,19.60721),
                         intensity3=c(17.04260,27.21697,19.70314,16.19530),
                         intensity4=c(17.08473,26.99766,17.93342,17.97693))

# log formula with the data already transformed to log2
cvs<-protLogCV(log_intensity_df, 2)
# log formula with the data not log transformed
not_log_intensity_df<-data.frame(intensity1=c(125000,100000000,983450,475987),
                         intensity2=c(147000,230000000,720450,798656),
                         intensity3=c(135000,156000000,853566,75036),
                         intensity4=c(139000,134000000,250321,257986))
# base formula with raw intensity (no log transformation)
raw_cvs<-protCV(not_log_intensity_df)

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
\keyword{ ~proteomics }
\keyword{ ~CV }


