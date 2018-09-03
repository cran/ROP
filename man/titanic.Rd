\name{titanic}
\alias{titanic}
\docType{data}
\title{Data file}
\description{File containing the data to be analyzed}
\usage{
data(titanic)
}
\format{
The data file is in CSV format with a semicolon separator for columns.
Decimal values use the point as a decimal separator.
The first line must include the field names.
The file includes n + 2 fields, where 'n' is the number of variables.
The format is as follows:
	\describe{
		\item{\code{Id}}{Case identification number}
		\item{\code{ETAT}}{Etat \code{Sick} \code{Not Sick} of the case}
		\item{\code{Class}}{Class}
		\item{\code{Sexe}}{Sex}
		\item{\code{Age}}{Age}
	}
}
\author{Jean-Michel Nguyen, Daniel Antonioli}
\examples{
data(titanic)
}
\keyword{ ~kwd1 }

