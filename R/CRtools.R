# same as round() function, but rounds .5 up to higher number, rather than rounding even
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
# 2020-04-06 rounds p values below 0.0001 to '<0.0001'; converts to character
pRound <- function( vec , digits=4 ){
  vec0 <- vec
  eps <- 10^(-10)
  vec <- abs(vec)
  vec <- vec*10^digits
  vec2 <- vec-floor(vec)
  #    vec <- floor( vec ) + ifelse( vec2 < .5 , 0 , 1 )
  vec <- floor( vec ) + ifelse( ( vec2 - .5 ) < - eps, 0 , 1 )
  vec.round <- sign(vec0) * vec / 10^digits
  if(vec.round<0.0001){return('<0.0001')} else{return(as.character(vec.round))}
}

# convert number to percent as character variable
# input:
# numbertobeconvertedtocharacter = numeric value
# numberofdecimals = number of decimal places in percentage (default: 0)
# output: character with that number written as a percent
pct <- function(numbertobeconvertedtocharacter, numberofdecimals = 0){
  return(paste0(round2(numbertobeconvertedtocharacter*100,numberofdecimals),"%"))
}

# capitalizes first letters of strings
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# creates subfolder directories and basic files for project
  # works in conjunction with custom shortcuts and custom pipe (not included in this R code)
CRprojectBuilder <- function(){
  dir.create("data")
  dir.create("code")
  dir.create("figures")
  dir.create("output")
  dir.create("docs")

  if(!file.exists("code\\_import.R")){
    file.create("code\\_import.R")
    fileConn.import <- file("code\\_import.R")
    writeLines(c("library(tidyverse)","library(CRtools)","library(cowplot)","library(ggthemes)","library(readxl)","library(scales)","library(skimr)","library(car)","library(ggfortify)","library(lubridate)","library(data.table)","","# import ----"),fileConn.import)
    close(fileConn.import)
  }

  if(!file.exists("code\\_libs.R")){
    file.create("code\\_libs.R")
    fileConn.libs <- file("code\\_libs.R")
    writeLines(c("library(tidyverse)","library(CRtools)","library(cowplot)","library(ggthemes)","library(readxl)","library(scales)","library(skimr)","library(car)","library(ggfortify)","library(lubridate)","library(data.table)"),fileConn.libs)
    close(fileConn.libs)
  }

  if(!file.exists("code\\_documentation.R")){
    file.create("code\\_documentation.R")
    fileConn.documentation <- file("code\\_documentation.R")
    writeLines(c("dat.doc <- vector('list')","dat.doc[['Notes']] <- 'Enter Notes Here'"),fileConn.documentation)
    close(fileConn.documentation)
  }


  if(!file.exists("code\\_analysis.R")){
      file.create("code\\_analysis.R")
  }

  if(!file.exists("code\\_functions.R")){
      file.create("code\\_functions.R")
  }

  if(!file.exists("code\\_diagnostics.R")){
      file.create("code\\_diagnostics.Rmd")
  }


  if(!file.exists("code\\_old code.R")){
      file.create("code\\_old code.R")
  }

  file.edit("code\\_import.R")
  file.edit("code\\_libs.R")
}

# summarise mathematical models made using things like glm() or glmer() for easier understanding
  # requires tidyverse (dplyr and tibble specifically)
CRmodelSummary.logistic <- function(myModel,rounding = 4,simpleResultOutput = T){
  myModelSummary <- myModel %>%
    summary %>%
    coefficients %>%
    as.data.frame %>%
    rownames_to_column() %>%
    rename("p value" = `Pr(>|z|)`,
           Coefficient = Estimate) %>%
    mutate_at(vars(Coefficient,`Std. Error`,`z value`),~round(.,rounding))

  myModelOddsRatios <- exp(myModel %>%
                             summary %>%
                             coefficients %>%
                             .[,1]) %>%
    as.data.frame %>%
    rownames_to_column() %>%
    rename("Odds Ratio" = ".") %>%
    mutate("Odds Ratio" = round2(`Odds Ratio`,rounding))

  myModelOddsRatioConfidenceInterval <- exp(confint(myModel)) %>%
    as.data.frame %>%
    rownames_to_column() %>%
    rename("Lower OR Confidence Limit" = "2.5 %") %>%
    rename("Upper OR Confidence Limit" = "97.5 %") %>%
    mutate_at(vars(contains("Confidence Limit")),~round2(.,rounding))

  myModelResults <- left_join(myModelSummary,myModelOddsRatios,by = "rowname") %>%
    left_join(myModelOddsRatioConfidenceInterval, by = "rowname") %>%
    rename(variableName = "rowname") %>%
    rowwise() %>%
    mutate(`p value.numeric` = `p value`) %>%
    mutate(`p value` = ifelse(`p value`<0.0001,"<0.0001",as.character(round2(`p value`,rounding)))) %>%
    ungroup()

  if(simpleResultOutput %in% T){
    myModelResults <- myModelResults %>%
      select(variableName,`p value`,`Odds Ratio`,`Lower OR Confidence Limit`,`Upper OR Confidence Limit`)
  }

  return(myModelResults)
}

# 2020-02-21 epic color palette

epic_color_palette <- c(
  rgb(0,133,242,maxColorValue=255),
  rgb(221,41,157,maxColorValue=255),
  rgb(105,195,0,maxColorValue=255),
  rgb(180,41,204,maxColorValue=255),
  rgb(0,191,212,maxColorValue=255),
  rgb(255,146,0,maxColorValue=255),
  rgb(106,76,224,maxColorValue=255),
  rgb(24,194,149,maxColorValue=255),
  rgb(217,78,111,maxColorValue=255),
  rgb(36,164,238,maxColorValue=255)
)

# 2020-06-09 adding cut2 from Hmisc so I don't have to load a package that conflicts with dplyr
cut2 <- function (x, cuts, m = 150, g, levels.mean = FALSE, digits, minmax = TRUE,
          oneval = TRUE, onlycuts = FALSE, formatfun = format, ...)
{
  if (inherits(formatfun, "formula")) {
    if (!requireNamespace("rlang"))
      stop("Package 'rlang' must be installed to use formula notation")
    formatfun <- getFromNamespace("as_function", "rlang")(formatfun)
  }
  method <- 1
  x.unique <- sort(unique(c(x[!is.na(x)], if (!missing(cuts)) cuts)))
  min.dif <- min(diff(x.unique))/2
  min.dif.factor <- 1
  if (missing(digits))
    digits <- if (levels.mean)
      5
  else 3
  format.args <- if (any(c("...", "digits") %in%
                         names(formals(args(formatfun))))) {
    c(digits = digits, list(...))
  }
  else {
    list(...)
  }
  oldopt <- options("digits")
  options(digits = digits)
  on.exit(options(oldopt))
  xlab <- attr(x, "label")
  if (missing(cuts)) {
    nnm <- sum(!is.na(x))
    if (missing(g))
      g <- max(1, floor(nnm/m))
    if (g < 1)
      stop("g must be >=1, m must be positive")
    options(digits = 15)
    n <- table(x)
    xx <- as.double(names(n))
    options(digits = digits)
    cum <- cumsum(n)
    m <- length(xx)
    y <- as.integer(ifelse(is.na(x), NA, 1))
    labs <- character(g)
    cuts <- approx(cum, xx, xout = (1:g) * nnm/g, method = "constant",
                   rule = 2, f = 1)$y
    cuts[length(cuts)] <- max(xx)
    lower <- xx[1]
    upper <- 1e+45
    up <- low <- double(g)
    i <- 0
    for (j in 1:g) {
      cj <- if (method == 1 || j == 1)
        cuts[j]
      else {
        if (i == 0)
          stop("program logic error")
        s <- if (is.na(lower))
          FALSE
        else xx >= lower
        cum.used <- if (all(s))
          0
        else max(cum[!s])
        if (j == m)
          max(xx)
        else if (sum(s) < 2)
          max(xx)
        else approx(cum[s] - cum.used, xx[s], xout = (nnm -
                                                        cum.used)/(g - j + 1), method = "constant",
                    rule = 2, f = 1)$y
      }
      if (cj == upper)
        next
      i <- i + 1
      upper <- cj
      y[x >= (lower - min.dif.factor * min.dif)] <- i
      low[i] <- lower
      lower <- if (j == g)
        upper
      else min(xx[xx > upper])
      if (is.na(lower))
        lower <- upper
      up[i] <- lower
    }
    low <- low[1:i]
    up <- up[1:i]
    variation <- logical(i)
    for (ii in 1:i) {
      r <- range(x[y == ii], na.rm = TRUE)
      variation[ii] <- diff(r) > 0
    }
    if (onlycuts)
      return(unique(c(low, max(xx))))
    flow <- do.call(formatfun, c(list(low), format.args))
    fup <- do.call(formatfun, c(list(up), format.args))
    bb <- c(rep(")", i - 1), "]")
    labs <- ifelse(low == up | (oneval & !variation), flow,
                   paste("[", flow, ",", fup, bb, sep = ""))
    ss <- y == 0 & !is.na(y)
    if (any(ss))
      stop(paste("categorization error in cut2.  Values of x not appearing in any interval:\n",
                 paste(format(x[ss], digits = 12), collapse = " "),
                 "\nLower endpoints:", paste(format(low,
                                                    digits = 12), collapse = " "), "\nUpper endpoints:",
                 paste(format(up, digits = 12), collapse = " ")))
    y <- structure(y, class = "factor", levels = labs)
  }
  else {
    if (minmax) {
      r <- range(x, na.rm = TRUE)
      if (r[1] < cuts[1])
        cuts <- c(r[1], cuts)
      if (r[2] > max(cuts))
        cuts <- c(cuts, r[2])
    }
    l <- length(cuts)
    k2 <- cuts - min.dif
    k2[l] <- cuts[l]
    y <- cut(x, k2)
    if (!levels.mean) {
      brack <- rep(")", l - 1)
      brack[l - 1] <- "]"
      fmt <- do.call(formatfun, c(list(cuts), format.args))
      labs <- paste("[", fmt[1:(l - 1)], ",",
                    fmt[2:l], brack, sep = "")
      if (oneval) {
        nu <- table(cut(x.unique, k2))
        if (length(nu) != length(levels(y)))
          stop("program logic error")
        levels(y) <- ifelse(nu == 1, c(fmt[1:(l - 2)],
                                       fmt[l]), labs)
      }
      else levels(y) <- labs
    }
  }
  if (levels.mean) {
    means <- tapply(x, y, function(w) mean(w, na.rm = TRUE))
    levels(y) <- do.call(formatfun, c(list(means), format.args))
  }
  attr(y, "class") <- "factor"
  if (length(xlab))
    label(y) <- xlab
  y
}
