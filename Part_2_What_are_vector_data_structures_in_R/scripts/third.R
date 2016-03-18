third <- function(x) {
  if (length(x) < 3)
    stop("Fewer than three elements in object")
  for (i in 1:length(x)) {
    if (i == 3) break
  }
  cat ("Third element is: ")
  return (x[i])
}

b <- c(c(5:9),c(3:5)); b
c <- c(1,2,3); c

third(b)
third(c)
