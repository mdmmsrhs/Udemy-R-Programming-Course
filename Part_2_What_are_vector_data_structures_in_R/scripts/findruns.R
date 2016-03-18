# Function to return the starting elements of runs of 1's of a given length

findruns <- function(x,k) { # x is vector, k is length of run
  n <- length(x) # length of vector
  runs <- NULL
  for (i in 1:(n-k+1)) { # check as far as need 
    
    # determines whether all of the k values starting at
    # x[i]...x[i], x[i+1],x[i+k-1] are 1's
    # expression 'x[i:(i+k-1)] gives us this range in x,
    # we apply all to see if there is a run there
    
    if (all(x[i:(i+k-1)]==1) runs <- c(runs,i) # this method is inefficient because it recreates "runs" 
                                              # anew every time the loop executes.  R does this whenever a 
                                              # variable or vector is changed, rather than simply modifying it.
  }
  return(runs)
}


y <- c(1,0,0,1,1,1,0,1,1);y

findruns1(y,3)
findruns1(y,2)
findruns1(y,6)

z <- c(0,0,1,1,1,0,0,0,1,1,1,0,1,1,1,1);z

findruns1(z,3)
findruns1(z,4)

# A more efficient findruns function using vectors #

## Program findruns1.r
findruns1 <- function(x,k=3) {
  n <- length(x)
  # set up space of vector of length n
  # so no inefficientb vector re-allocations during loop
  runs <- vector(length=n) 
  count <- 0
  for (i in 1:(n-k+1)) {
    if (all(x[i:(i+k-1)]==1)) {
      count <- count + 1
      # Here we simply fill up runs
      runs[count] <- i 
    }
  }
  if (count > 0) {
    # Here we redefine runs to remove unused portion of vector
    runs <- runs[1:count] 
  } else runs <- NULLptm <- as.matrix(read.csv(file="C:/acmacs/Landscapes/HI_Panel_EXP580_0dpi_013015_post_titres.csv",header = TRUE, sep=','))

  return(runs)
}