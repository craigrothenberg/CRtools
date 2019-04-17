round2 <- function( vec , digits=0 ){
  vec0 <- vec
  eps <- 10^(-10)
  vec <- abs(vec)
  vec <- vec*10^digits
  vec2 <- vec-floor(vec)
  #    vec <- floor( vec ) + ifelse( vec2 < .5 , 0 , 1 )
  vec <- floor( vec ) + ifelse( ( vec2 - .5 ) < - eps, 0 , 1 )
  vec.round <- sign(vec0) * vec / 10^digits
  return(vec.round)
}

# convert number to percent as character variable
# input:
# numbertobeconvertedtocharacter = numeric value
# numberofdecimals = number of decimal places in percentage (default: 0)
# output: character with that number written as a percent
pct <- function(numbertobeconvertedtocharacter, numberofdecimals = 0){
  return(paste0(round2(numbertobeconvertedtocharacter*100,numberofdecimals),"%"))
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

ProjectBuilder <- function(){
  dir.create("data")
  dir.create("code")
  dir.create("figures")
  dir.create("output")

  file.create("code\\_import.R")
  fileConn.import <- file("code\\_import.R")
  writeLines(c("library(tidyverse)","library(CRtools)"),fileConn.import)

  file.create("code\\_libs.R")
  fileConn.import <- file("code\\_libs.R")
  writeLines(c("library(tidyverse)","library(CRtools)"),fileConn.import)
  
  file.create("code\\_documentation.R")
  fileConn.documentation <- file("code\\_documentation.R")
  writeLines(c("dat.doc <- as.list(NA)","dat.doc[['Notes']] <- 'Enter Notes Here'"),fileConn.documentation)

  file.create("code\\_analysis.R")
  file.create("code\\_functions.R")

  file.edit("code\\_import.R")
}
