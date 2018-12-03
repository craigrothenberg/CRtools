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

df1 <- data.frame(x=rep(1:5,2),y=c(1,2,3,NA,5,6,7,8,9,10),z = c(1,1,1,2,2,2,1,2,1,2))
