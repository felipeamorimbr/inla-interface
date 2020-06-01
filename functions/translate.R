translate <- function(words, language, dictionary){
  return(dictionary[[language]][[which(dictionary$en == words)]])
}