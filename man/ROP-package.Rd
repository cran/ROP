\name{ROP-package}
\alias{ROP-package}
\alias{ROP}
\docType{package}
\title{
\packageTitle{ROP}
}
\description{
\packageDescription{ROP}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{ROP}
\packageIndices{ROP}

}
\author{
\packageAuthor{ROP}

Maintainer: \packageMaintainer{ROP}
}
\keyword{ package }
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
