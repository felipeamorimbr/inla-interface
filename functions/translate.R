translate <- function(words, language, dictionary = NULL){
  if(is.null(dictionary))
    return(words)
  return(dictionary[[language]][[which(dictionary$en == words)]])
}
