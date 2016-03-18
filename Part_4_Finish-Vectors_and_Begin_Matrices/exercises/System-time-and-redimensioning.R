##################################################
#####        SYSTEM  TIME  FUNCTION          #####
##################################################
#####      REDIMENSIONING  A  VECTOR         #####
##################################################

## The system.time() function is a handy tool
## for checking the efficiency of your programs

require(stats)
# mad() computes median absolute deviation
# runif() generates a uniform sample
system.time(for(i in 1:100) mad(runif(1000)))

# ?mad ... if want to look mad() up

# ?runif ... go on, don't be lazy !

## Create a function calculates mean 
## of student's t distribution over and over
exT <- function(n = 10000) {
  # Purpose: Test if system.time works ok; n: loop size
  system.time(for(i in 1:n) x <- mean(rt(1000, df=4)))
}

# Run the following statements:
exT() #- about 4 secs on a 2.5GHz Xeon
# user  system elapsed 
# 3.83    0.00    3.84 
# program takes about 3.83 seconds to run

system.time(exT())    #~ +/- same
# user time is for the function
# system time is 'other stuff' computer busy with
# elapsed is total time
# user  system elapsed 
# 3.89    0.00    3.88 

#### REDIMENSIONING AN ARRAY (Vector in this case)

# This can help you make some of your programs
# run faster. The following two programs produce
# the same result, but the first is faster:

## PROGRAM 1
faster <- function() {
   n <- 1000000
   # fills up x with successive
   # integers up to one million
   x <- rep(0, n)
   for (i in 1:n) {
     x[i] <- i
   }
}

faster()
system.time(faster())

## PROGRAM 2
slower <- function() {
  # note am only filling to 100,000
  n <- 100000
  x <- 1
  for (i in 2:n) {
   x[i] <- i
  }
}

system.time(slower())

## Why is Program #2 so much slower?
## How do you 'fix it' to make it faster?