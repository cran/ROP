\name{rop}
\alias{rop}
\title{Launch of the analysis}
\description{Launch function of the analysis}
\usage{rop(fic, output_folder, mini, maxi, nbCycles, typesVariables)}
\arguments{
  \item{fic}{Name of the file to analyze in csv format (for example titanic.csv)}
  \item{output_folder}{Path to the output folder}
  \item{mini}{Value of the minimum weighting}
  \item{maxi}{Value of the maximum weighting}
  \item{nbCycles}{Maximum number of cycles to apply}
  \item{typesVariables}{Boolean vector tracking qualitative (F) and quantitative (T) variables}
}
\value{No value returned}
\author{Jean-Michel Nguyen, Daniel Antonioli}
\note{The results of the analysis are stored in 2 files:
<file name> result.csv:
  contains for each stage and cycle of the analysis the coefficients retained, the threshold, the values of Se and Sp
<file name> solution.csv:
  contains the detail of the values of each individual for the solution obtained for each cycle and stage}
\examples{
rop(
  fic = system.file("extdata", "titanic.csv", package = "ROP"),
  output_folder = tempdir(),
  mini = -1,
  maxi = 1,
  nbCycles = 2,
  typesVariables = c(FALSE, FALSE, FALSE)
)

}
\keyword{package}
