extract_words <- function(modal){
  words_to_translate <- readLines(modal) #Reading the modal file
  words_to_translate <- words_to_translate %>% #Extracting only the words
    grep(pattern = "translate", value = TRUE) %>%
    gsub(pattern = '.*translate\\("', replacement = "") %>%
    gsub(pattern = '",.*', replacement = "") %>%
    unique()
  return(words_to_translate)
}
