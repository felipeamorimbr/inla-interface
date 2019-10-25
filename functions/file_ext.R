#function from tools package that recive a character and returns the characters after the "."
#used to returns extension from a file

file_ext <- function (x) 
{
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}
