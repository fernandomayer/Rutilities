run.all.chunks <- function(Rmd, envir = globalenv()){
    ## Function to run ALL chunks in a Rmd file
    ## Based on
    ## http://stackoverflow.com/questions/24753969/knitr-run-all-chunks-in-an-rmarkdown-document
    tempR <- tempfile(tmpdir = ".", fileext = ".R")
    on.exit(unlink(tempR))
    knitr::purl(Rmd, output = tempR)
    sys.source(tempR, envir = envir)
}

