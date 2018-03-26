Reverse <- function(x){
x <- substring(x,1:nchar(x),1:nchar(x))

for (i in 1:length(x)) {
 if ( x[i] == "A") 
   x[i] <- "T"
  else if (x[i] == "T") 
   x[i] <- "A"
  else if (x[i] == "C") 
   x[i] <- "G"
  else if (x[i] == "G")
   x[i] <- "C"
}
x <- x %>% rev() %>%
  paste(collapse = "")
return (x)}
