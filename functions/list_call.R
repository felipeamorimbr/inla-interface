#Function that recive a list and returns call used to make the list
list_call <- function(data = list()){
  return(noquote(eval(call("paste0",  call("list", data) , collapse = ""))))
}

#Testing
x <- list(a = 1, b = 2, d = list(j = 25), e = c(1,2,23), e = noquote("loggamma"))
list_call(x)

