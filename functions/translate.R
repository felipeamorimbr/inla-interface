translate <- function(words, language, dictionary = NULL){
  if(is.null(dictionary))
    return(words)
  if(language == "en")
    return(words)
  return(dictionary[[language]][[which(dictionary$en == words)]])
}
