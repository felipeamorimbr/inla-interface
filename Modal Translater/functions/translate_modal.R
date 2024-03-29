# Function to translate modals
translate_modal <- function(words_to_translate, json) {
  # words_to_translate <- readLines(modal) #Reading the modal file
  # words_to_translate <- words_to_translate %>% #Extracting only the words
  #   grep(pattern = "translate", value = TRUE) %>%
  #   gsub(pattern = '.*translate\\("', replacement = "") %>%
  #   gsub(pattern = '",.*', replacement = "") %>%
  #   unique()

  # Definying the languages to translate
  translate_languages <- c(
    "af", "sq", "am", "ar", "hy", "az", "eu", "be", "bn", "bs",
    "bg", "ca", "ceb", "ny", "zh-CN", "zh-TW", "co", "hr", "cs",
    "da", "nl", "eo", "et", "tl", "fi", "fr", "fy", "gl", "ka",
    "de", "el", "gu", "ht", "ha", "haw", "iw", "hi", "hmn", "hu",
    "is", "ig", "id", "ga", "it", "ja", "jw", "kn", "kk", "km", "rw",
    "ko", "ku", "ky", "lo", "la", "lv", "lt", "lb", "mk", "mg", "ms",
    "ml", "mt", "mi", "mr", "mn", "my", "ne", "no", "or", "ps", "fa",
    "pl", "pt", "pa", "ro", "ru", "sm", "gd", "sr", "st", "sn", "sd",
    "si", "sk", "sl", "so", "es", "su", "sw", "sv", "tg", "ta", "tt",
    "te", "th", "tr", "tk", "uk", "ur", "ug", "uz", "vi", "cy", "xh",
    "yi", "yo", "zu", "he", "zh"
  )
  
  #Removing Duplicates
  words_to_translate_u <- unique(words_to_translate)
  
  
  #Organaizing the data.frame output
  words_translated <- data.frame(matrix(nrow = length(words_to_translate_u), ncol = 111)) 
  colnames(words_translated) <- c("en", translate_languages)
  words_translated$en <- words_to_translate_u
  
  #Get autorization to use the Google API
  gl_auth(json)
  
  aux_translate <- 0
  #Translating 
  #110
  for(i in 1:110){
    aux_translate <- googleLanguageR::gl_translate(t_string = words_to_translate_u,
                                                   target = translate_languages[i],
                                                   source = "en")
    words_translated[[i+1]] <- aux_translate[[1]]
  }
  
  return(words_translated)
}
