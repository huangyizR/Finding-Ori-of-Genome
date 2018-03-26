PatternCount <- function(Text, Pattern) {
  count <- 0
  for (i in 1:(nchar(Text)-nchar(Pattern)+1)) {
    if ( substr ( Text , i , i + nchar ( Pattern ) - 1) == Pattern)
      count <- count + 1} 
  return(count)
}

FrequentWords <- function ( Text , k ) {
  Fre <- c()
  counts <- c()
  for (i in 1:(nchar(Text)-k+1)) {
    Pattern <- substr ( Text , i , i + k -1)
    counts [i] <- PatternCount(Text, Pattern)
    maxCount <- max(count)
  }
  for (i in 1:(nchar(Text)-k+1)) {
    if (counts [i] == max(counts))
    Fre [i] <- substr ( Text , i , i + k - 1 )
  } 
  Factor_Fre <- factor(Fre)
  return (Factor_Fre)
}
