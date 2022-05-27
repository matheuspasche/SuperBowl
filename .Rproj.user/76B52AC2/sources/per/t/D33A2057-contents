
#' nucleotides
#' This function translates DNAs to DNAs
#' @param dna character with nucleotides
#'
#' @return 
#' @export
#'
#' @examples
#' nucleotides("CGAT")
nucleotides <- function(dna) {
if(!is.character(dna)){
  stop("Please insert character nucleotides, such as ACGT")
}
  
  
  old <- "GCTA"
  new <- "CGAU"
  
  translate <- chartr(
    old = old,
    new = new,
    x = dna
  ) 
  
  return(translate)
}

##
library(testthat)
test_that("Google, translate dna", {
  expect_equal(nucleotides("GACATGG"), "CUGUACC")
  expect_equal(nucleotides("AAATTT"), "UUUAAA")
  expect_error(nucleotides(1234))
})