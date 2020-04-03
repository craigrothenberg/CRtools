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
ProjectBuilder <- function(){
  dir.create("data")
  dir.create("code")
  dir.create("figures")
  dir.create("output")
  dir.create("docs")

  file.create("code\\_import.R")
  fileConn.import <- file("code\\_import.R")
  writeLines(c("library(tidyverse)","library(CRtools)","library(cowplot)","library(ggthemes)","library(readxl)","library(scales)"),fileConn.import)

  file.create("code\\_libs.R")
  fileConn.import <- file("code\\_libs.R")
  writeLines(c("library(tidyverse)","library(CRtools)","library(cowplot)","library(ggthemes)","library(readxl)","library(scales)"),fileConn.import)

  file.create("code\\_documentation.R")
  fileConn.documentation <- file("code\\_documentation.R")
  writeLines(c("dat.doc <- vector('list')","dat.doc[['Notes']] <- 'Enter Notes Here'"),fileConn.documentation)

  file.create("code\\_analysis.R")
  file.create("code\\_functions.R")

  file.edit("code\\_import.R")
  file.edit("code\\_libs.R")
}

# summarise mathematical models made using things like glm() or glmer() for easier understanding
  # requires tidyverse (dplyr and tibble specifically)
CRmodelSummary <- function(myModel,rounding = 4){
  myModelSummary <- myModel %>%
    summary %>%
    coefficients %>%
    as.data.frame %>%
    rownames_to_column() %>%
    rename("p value" = `Pr(>|z|)`,
           Coefficient = Estimate) %>%
    mutate_at(vars(Coefficient,`Std. Error`,`z value`),list(~round(.,rounding)))

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
    mutate_at(vars(contains("Confidence Limit")),list(~round2(.,rounding)))

  myModelResults <- left_join(myModelSummary,myModelOddsRatios,by = "rowname") %>%
    left_join(myModelOddsRatioConfidenceInterval, by = "rowname") %>%
    rename(variableName = "rowname") %>%
    rowwise() %>%
    mutate(`p value` = ifelse(`p value`<0.0001,"<0.0001",as.character(round2(`p value`,rounding)))) %>%
    ungroup()
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


# 2020-04-03 check each variable in dataframe for missings, zeroes, negative numbers, outliers
CRdataCheck <- function(myData){

  CRoutput <- vector('list')

  # meanOfEachNumericVariable <- myData %>% summarise_all(~mean(.,na.rm=T))
  # sdOfEachNumericVariable <- myData %>% summarise_all(~sd(.,na.rm=T))

  missingness <- myData %>% summarise_all(list(~length(.[is.na(.)]))) %>% gather() %>% rename(missingness = 'value')
  negatives <- myData %>% summarise_all(list(~length(.[is.numeric(.) & !is.na(.) & .<0]))) %>% gather() %>% rename(negatives = 'value')
  outliers <- myData %>% summarise_all(list(~length(.[
    is.numeric(.) &
      !is.na(.) &
      abs(.) > mean(.,na.rm=T) + {sd(.,na.rm=T)*2}
  ]))) %>%
    gather() %>% rename(outliers = 'value')

  dataCheck <- full_join(
    missingness,
    negatives,
    by = 'key'
  ) %>%
    full_join(
      outliers,
      by = 'key'
    ) %>%
    rowwise() %>%
    filter_at(vars(-key),any_vars(!. %in% 0))

  CRoutput[['Diagnostics']] <- dataCheck
  CRoutput[['Outliers']] <- vector('list')

  for(i in dataCheck$key){
    CRoutput$Outliers[[i]] <-
      myData %>%
        select(.data[[i]]) %>%
        mutate(rowNumber=1:n()) %>%
        filter(abs(.data[[i]])>mean(.data[[i]] + {sd(.data[[i]],na.rm=T)*2}))
      }
  return(CRoutput)
}
# z <- CRdataCheck(mtcars)
