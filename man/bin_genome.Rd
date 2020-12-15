\name{bin_genome}
\alias{bin_genome}
\title{
Bin the genome
}
\description{
Bin the genome
}
\usage{
bin_genome(species = "hg19", bins = 2000, bin_size = NULL, ...)
}
\arguments{

  \item{species}{Abbreviation of the genome, pass to \code{\link[circlize]{read.chromInfo}}.}
  \item{bins}{Number of bins. The final number of bins is approximately equal to it.}
  \item{bin_size}{Size of the bins. If \code{bin_size} is set, \code{bins} is ignored.}
  \item{...}{All pass to \code{\link[circlize]{read.chromInfo}}. E.g. you can set a subset of chromosomes there.}

}
\value{
A \code{\link[GenomicRanges:GRanges-class]{GRanges}} object of the genomic bins.
}
\examples{
# There is no example
NULL

}
