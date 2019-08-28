#Function that returns call of a list
list.call <- function(data = list()){
  return(eval(call("paste0",  call("list", data) , collapse = "")))
}

#Testing
x <- list(a = 1, b = 2, d = list(j = 25), e = c(1,2,23))
list.call(x)
