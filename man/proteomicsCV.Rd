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
protLogCV(data, log_transformed)
protCV(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{x} here~~
input dataframe of the intensity values. These should be normalised already for optimal results.
}
\item{log_transformed}{
'no' for data that has not been log transformed.
'yes' for natural log transformed data. (log() function in R)
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
intensity_df<-data.frame(intensity1=c(23.88,23.55,23.41,23.15),
                         intensity2=c(23.98,23.56,23.43,23.29),
                         intensity3=c(23.94,23.635,23.37,23.21),
                         intensity4=c(23.81,23.57,23.54,23.08))

# log formula with the data already transformed to natural log
cvs<-protLogCV(intensity_df, "yes")
# log formula with the data not log transformed
not_log_intensity_df<-exp(1)^intensity_df
cvs<-protLogCV(not_log_intensity_df,"no")
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


