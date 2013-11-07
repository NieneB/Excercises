# Author: Niene Boeijen
# November 2013
# requires
# year = numeric
# Quick description of what the function does. 

is.leap <- function(year) {
  if(is.numeric(year)) {
    if(year > 1582) {
      out <- ifelse(year%%100 == 0, year%%400 == 0, year%%4 == 0) 
    }
    else if(year <= 1582) {
      out <- paste(year," is out of valid range")
    }
  }
  else if(is.character(year)) {
    stop("Object of class numeric expected!")
  }
  return(out)
}

is.leap(2000)
is.leap(2002)
is.leap("niene")
is.leap(1582)
is.leap(1996)
is.leap(1652)
is.leap(1999)
is.leap(1998)
is.leap(2)



