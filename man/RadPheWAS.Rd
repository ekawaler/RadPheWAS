\name{RadPheWAS-package}
\alias{RadPheWAS-package}
\alias{RadPheWAS}
\docType{package}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
RadPheWAS package - Just trying to make an example.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RadPheWAS-package(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
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
\references{
%% ~put references to the literature/web site here ~
}
\author{
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
\donttest{
#Load the PheWAS package
library(RadPheWAS)
#Set the random seed so it is replicable
set.seed(1)
#Generate some example data
ex=generate_example()
#Extract the two parts from the returned list
id.icd9.count=ex$id.icd9.count
genotypes=ex$genotypes
#Create the PheWAS code table- translates the icd9s, adds
#exclusions, and reshapes to a wide format
phenotypes=create_phewas_table(id.icd9.count)
#Run the PheWAS
results=phewas(phenotypes,genotypes,cores=1,
  significance.threshold=c("bonferroni"))
#Plot the results
phewasManhattan(results, annotate.angle=0,
  title="My Example PheWAS Manhattan Plot")
#Add PheWAS descriptions
results_d=addPhewasDescription(results)
#List the significant results
results_d[results_d$bonferroni&!is.na(results_d$p),]
#List the top 10 results
results_d[order(results_d$p)[1:10],]
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
