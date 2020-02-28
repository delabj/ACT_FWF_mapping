#' Translate comment code to plain text comment
#'
#' @param comment_code A two character code.
#' @return A character with the text of the comment
#' @examples
#'
#' get_comment("33")
#' get_comment("--")
get_comment <- function(comment_code="--"){
  if(nchar(comment_code) != 2){
    stop('Invalid Comment Code Code. Comment code should be 2 characters')
  }

  ## if the code is -- there is no comment
  if(comment_code=="--"){
    return("NA")
  }
  ## if the comment codes need to be between 01 and 99 if this isn't the case something is wrong
  else if(!(as.numeric(comment_code)%in%c(1:99))){
    stop('Invalid Comment Code Code values should be between 01 and 99')
  }

  comment <- switch(comment_code,
         "01" = "The pages submitted for the Writing Test could not be scored. No score is possible if the pages were left blank or were marked void at the test center, or if the essay is illegible, is not written in English, or does not respond to the prompt. In any of these cases, no Combined English/Writing score or Writing subscore can be reported.",
         "02" = "Combined English/Writing score and Writing subscore can be reported only when there is a valid English score. Because there were no responses to any items on the multiple-choice English Test, no Combined English/ Writing or Writing subscore can be reported.",
         "20" = "Your essay responded to the prompt by taking a position on the issue.",
         "21" = "Your essay responded to the prompt by taking a clear position on the issue.",
         "22" = "Your essay acknowledged counterarguments on the issue but did not discuss them.",
         "23" = "Your essay showed recognition of the complexity of the issue by addressing counterarguments.",
         "24" = "Your essay showed recognition of the complexity of the issue by partially evaluating",
         "25" = "Your essay addressed the complexity of the issue by fully responding to counterarguments.",
         "26" = "Your essay addressed the complexity of the issue by evaluating its implications.",
         "30" = "Your essay provided very little writing about your ideas. Try to write more about the topic.",
         "31" = "The ideas in your essay needed to be more fully explained and supported with more details.",
         "32" = "Your essay used some specific details, reasons, and examples, but it needed more of them.",
         "33" = "Your essay adequately supported general statements with specific reasons, examples, and details.",
         "34" = "General statements in your essay were well supported with specific reasons, examples, and details.",
         "35" = "Your essay effectively supported general statements with specific reasons, examples, and details.",
         "40" = "Your writing did not maintain a focus on the issue. Try to plan your essay before you write.",
         "41" = "Your essay focused on the general topic rather than on the specific issue in the prompt.",
         "42" = "Your essay maintained focus on the specific issue in the prompt.",
         "50" = "Your essay lacked organization. Try to plan and arrange your ideas logically.",
         "51" = "Your essay was not clearly organized. Try to plan and arrange your ideas logically.",
         "52" = "Your essay showed basic organizational structure, but the ideas needed to be more clearly connected.",
         "53" = "The organization of your essay was adequate, but the rigid structure seemed to limit discussion.",
         "54" = "Your essay was well organized, making it easy to understand logical relationships among ideas.",
         "55" = "The logical sequence of ideas in your essay fit its persuasive purpose well.",
         "60" = "Grammar, spelling, and punctuation errors made your essay difficult to understand.",
         "61" = "Grammar, spelling, and punctuation errors were distracting. Proofread your writing.",
         "62" = "Using correct grammar and more varied sentence structures would improve your essay.",
         "63" = "Using more varied sentence structures would make your essay clearer and more engaging.",
         "64" = "Using more sentence variety and precise word choice would make your essay clearer and more engaging.",
         "65" = "Some varied sentences structures and precise word choice added clarity and interest to your writing.",
         "66" = "Your essay showed a good command of language by using varied sentences and precise word choice.",
         TRUE = "NA"
         )




  return(comment)
}

## creates a string of comments based on the comment field
TranslateComment <- function(comment="--------"){
  if(comment=="--------"){
    "No Essay Comments"
  }
  else{
    comments <- ""
    for(i in 1:4){
      stop <- i*2
      start <-  stop-1
      comments <- paste(comments, get_comment(substr(comment, start, stop)))
    }
    return(comments)
  }
}

## transform column of comment codes into comments
## THIS IS DPLYR COMPATABLE! :):):)
FindComments <- function(.data, ...){
  .data$commentText <- lapply(.data$essayComments, TranslateComment)
  return(.data)
}

## UTCI
# 0:Below Proficent, 1:Proficent,  2:Above Proficent, -: unable to caluclate
GetUTCI <- function(textUTCI="-"){
  switch(as.character(textUTCI),
         "0" = return("Below Proficent"),
         "1" = return("Proficent"),
         "2" = return("Above Proficent"),
         "-" = return("Unable to calculate"),
         stop('Invalid UTCI code') #error catching
  )
}

#Tranforms the column of  UTCI into text
FindUTCI <- function(.data, ...){
  .data$UTCIText <- lapply(test2$UCTI, GetUTCI)
  return(.data)
}



## PTCRI
# 0:Below Proficent, 1:Proficent,  2:Above Proficent, -: unable to caluclate
GetPTCRI <- function(textPTCRI="-"){
  switch(as.character(textPTCRI),
         "0" = return("= Unlikely to obtain an NCRC"),
         "1" = return("Likely to obtain a Bronze level NCRC"),
         "2" = return("Likely to obtain a Silver level NCRC"),
         "3" = return("Likely to obtain a Gold level NCRC"),
         "4" = return("Likely to obtain a Platinum level NCRC"),
         "-" = return("Unable to calculate"),
         stop('Invalid PTCRI code') #error catching
  )
}

#Tranforms the column of  PTCRI into text
FindPTCRI <- function(.data, ...){
  .data$PTCRIText <- lapply(.data$PTCRI, GetPTCRI)
  return(.data)
}

runMapR <- function(){
  appDir <- system.file("shinyapps","ShinyMapR", package = "ACTmapR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ACTmapR`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}


GetReligAffil <- function(textReligAffil="NA"){
  df <- readRDS(file="~/ACTmapR/data/religAffiliation.rds")

  if(textReligAffil %in% df$Code){
    return(df$Response[df$Code==textReligAffil])
  }
  else{
    return(NA)
  }

}


FindUTCI <- function(.data, ...){
  .data$religiousAffiliation <- lapply(.data$religiousAffiliation, GetReligAffil)
  return(.data)
}
