#' Read in the ACT fixed width file
#'
#' @param file The file path to the file
#' @param year The school year for the mapping
#' @param blanks Boolean option to include the blank columns in the resulting data frame
#' @param scores_only Boolean option to only select scores.
#' @return A data frame containing the data from the fixed width file provided by the ACT
#' @examples
#'
#' read_ACT(filepath, mapping="19-20")
read_ACT<- function(file = "fileName", year="19-20", blanks=F, scores_only=T){
  df <- get_mapping(year)
  if(!is.data.frame(df)){
    stop("Given Mapping isn't a dataframe. Have you entered the year correctly?")
  }

  #reads in the file
  temp <- read.fwf(file=file, widths = df$widthsACT, col.names = df$namesACT)
  # checks to see if blank columns are needed
  if(blanks){return(temp)}
  #if not  removes the columns named blank
  temp %>%
    dplyr::select(-(dplyr::starts_with("blank")))->temp

  #only grab student names and scores
  if(scores_only){
    temp %>%
      dplyr::select(lastName, firstName, dateOfBirth, stateID, testDate,
             "composite"= scaleComposite,
             "english" = scaleEnglish,
             "mathmatics"=scaleMath,
             "reading"= scaleReading,
             "science"= scaleScience,
             "writing subscore"= scoreWR
      )->temp
  }

  return(temp)
}


#' Select the mapping to use when reading the ACT fixed width file
#'
#' @param year The school year for the mapping
#' @return A data frame containing column names and widths
get_mapping <- function(year="19-20"){
  switch(year,
         "18-19"= return(actMap18_19),  #sep2018-aug2019
         "19-20"= return(actMap19_20),  #sep2019-aug2020
         stop('No Mapping found, have you entered the year correctly?') #error catching
  )
  return(NULL)
}
