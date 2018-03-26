PatternCount <- function(Text, Pattern) {
  count <- c()
  for (i in 1:(nchar(Text)-nchar(Pattern)+1)) {
    if ( substr ( Text , i , i + nchar ( Pattern ) - 1) == Pattern)
      count[i] <- TRUE
    else count[i] <- FALSE}
 position <- c(0:(nchar(Text)-nchar(Pattern)))
 SumData <- data.frame(position,count)
 SumData %>% filter(count == "TRUE") %>%
   select(position) %>%
   unlist() %>%
   unname()
 return(SumData)
}
1+1

SumData <- PatternCount("ATCGATCGATCGTAGCATCGATCGAATGATCAAGATCGCGGGCTAGATGATCAAG","ATGATCAAG")

