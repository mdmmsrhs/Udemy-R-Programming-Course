##################################################
#####          LOOPING  WITH  FOR            #####
##################################################
#####      SEVERAL  FOR  LOOP  EXERCISES     #####
##################################################

# The for loop command has the following form,
# where x is a vector:

# for (x in vector) {
#   expression_1
#   ...
# }

# When executed, the for command executes the 
# group of expressions within the braces { },
# once for each element of vector. The grouped 
# expressions can use x, which takes on each of
# the values of the elements of vector as the 
# loop is repeated.

#### EXAMPLE: SUMMING A VECTOR

# example uses a loop to sum the elements of a 
# vector. Note that we use function 'cat'
# (for concatenate) to display the values of 
# certain variables. The advantage of cat() over
# show() is that it allows us to combine text
# and variables together. The combination of 
# characters \n (backslash-n) is used to 'print'
# a new line.

vector.sum <- function(x_list) {
   sum_x <- 0
   for (x in x_list) {
     sum_x <- sum_x + x
     cat("The current loop element is", x, "\n")
     cat("The cumulative total is", sum_x, "\n")
     }
}

(x_list <- seq(1, 9, by = 2))
vector.sum(x_list)

## Please note we create these functions for
## instruction. You can do this most easily by
sum(x_list)

## Give vector.sum() a 'test drive'.

## EXAMPLE: Compute n factorial (n!)

# The following program calculates n!
# clear the workspace
rm(list=ls())

nfactor <- function() {
   # Input
   n <- 6
   # Calculation
   n_factorial <- 1
   for (i in 1:n) {
     n_factorial <- n_factorial * i
   }
   # Output
   show(n_factorial)
}

nfactor()

# Note the easy way using prod(1:n).
prod(1:6)
