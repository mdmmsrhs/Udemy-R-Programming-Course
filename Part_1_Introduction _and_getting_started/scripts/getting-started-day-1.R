#################################################
####            GETTING  STARTED             ####
#################################################

# Interactive versus Batch Modes of Running R

# This is interactive, executing scripts and
# observing the results

# Lets find mean of 1000 normally distributed values
# that should approximate a mean of 10 and a standard
# deviation of 3

mean(rnorm(1000,10,3))

# What is the variance?
var(rnorm(1000,10,3))

# What is standard deviation?
sd(rnorm(1000,10,3))

# Why are the valuues different with each execution?

# What does rnorm() do?
?rnorm()

# Let's look at:
rnorm(10)

mean(rnorm(10))

# What does the [1] mean in each line of output above?

## RUNNING R IN BATCH MODE

# You can also store R commands in a file. By convention,
# R code files have suffix .R or .r

## Look at the code in z.R, then source it:

## first see what files are there
list.files()

## then source it
source("z.R")

# then what files are there?
list.files()

########## A  FIRST  R SESSION
# Make a simple data set
x <- c(1,2,4)

# Now we can do the following
q <- c(x,x,8);q

# previous two commands do same thing as
(q <- c(x,x,8))

# can evaluate any variable by calling it
x

# formally "prints" the variable
print(x)

# can access individual elements with []
x[3]

# can use indexes to "subset"
x[2:3]

# can determine mean and standard deviation of data set
mean(x)
sd(x)

# but if want to save one of the values,
# must ASSIGN it to a variable
y <- mean(x)
y

# Let's do something with one of R' internal data sets
# that are generally used for demo's:
data()

# One of data sets is 'Nile', contains data
# on flow of Nile from 1871-1970
Nile

# Let's plot a histogram of this data
hist(Nile)

# can change number of bins
# using the 'breaks' argument:
hist(Nile, breaks = 15)

# can do lots of other things with the plot.


# To quit R, can call q() on Windows
## or CTRL-D in Linux or CMD-D on a Mac

## Preview of Important R Data Structures

## VECTORS, the R Workhorse

# Scalars

x <- 8
x

# Character Strings: Single-element vectors of mode character

# are actually single-element vectors of mode character
x <- c(5,12,13)

x

length(x)

mode(x)

y <- "abc"

length(y)

mode(y)

z <- c("abc", "29 88")

length(z)

mode(z)

# R has various string-manipulation functions
# Many deal with putting strings together or
# taking them apart as follows:

u <- paste("abc","de","f") # concatenate the strings

u

v <- strsplit(u," ") # split the string according to blanks

v # but strsplit returns a list structure

## MATRICES

## An R matrix corresponds to the mathematical construct of
## the same name: a rectangualr array of numbers. Technically,
## a matrix is a vector, but with two additional attributes:
## number of rows and number of columns.

m <- rbind(c(1,4),c(2,2))
m

# matrix multiplication
m %*% c(1,1)

# matrices are indexed with double subscripting
m[1,2]

m[2,2]

# can extract submatrices from a matrix like extracting
# subvectors from vectors

m[1,]

m[,2]

############# LISTS

# Heterogeneous
# Analogous to a C struct for C/C++ programmers

# List elements are accessed using two-part names
# indicated with the dollar sign $ in R:

x <- list(u=2, v="abc")
x

x$u # expression refers to the u 'component' in the list x

x$v # the other component in the list x

# lists are often 'packaged up' to return all the data
# from a complex functions....we will see a lot of this

hist(Nile) # produces a histogram

# but

hn <- hist(Nile) # assigns a list object to variable hn

# what does the 'hn' list object look like?:

print(hn)

# these components describe characteristics of the histogram

# could also evoke object by
hn

# or more compactly using str() 'structure' function
str(hn)

#######  DATA FRAMES

# data sets are data frame objects

# a data frame is a list, with each component (column)
# representing a different vector of data

# we create a data frame:
d <- data.frame(list(kids=c("Jack","Jill"),ages=c(12,10)))
d

d$ages

# typically read in data frames, don't create them like this

###########   CLASSES

# R is object-oriented

# Objects are instances of Classes

# Classes are more abstract than these data types

# Most of R built on so-called S3 classes (comes from old
# S language, version 3 which was inspiration for R)

##  INTRODUCTION TO FUNCTIONS

# Heart of R programming consists of writing functions
# Function is a group of instruction that takes inputs,
# uses them to compute other values, and returns a result.

# Here we define a function called outcount()... it counts
# the odd numbers in a vector of integers

## Program oddcount.R
oddcount <- function(x)  {
  k <- 0  # assign 0 to k
  for (n in x)  {
    if (n %% 2 == 1) k <- k+1  # %% is the modulo operator
  }
  return(k)
}

# call it
oddcount(c(1,3,5))

oddcount(c(1,2,3,7,9))

# 38 modulo 7
38 %% 7

# can comment all of the code in oddcount.R:

# name of function is assigned
# keyword 'function' tells R what you are doing
# formal argument list follows keyword 'function'
oddcount <- function(x)  {
  k <- 0  # assign 0 to k
  # for loop sets n equal to x[1]
  # then tests that value for being odd or even
  for (n in x)  {
    # if n is odd, the modulo will be 1
    # and will increment k by 1
    if (n %% 2 == 1) k <- k+1  # %% is the modulo operator
  }
  # k has counted by the number of odd elements
  # return() function sends it out
  # return statement is optional
  return(k)
}

# R functions simply return the value
# of the last line executed

# 'x' is a 'formal argument' or 'formal parameter'
# of the function 'oddcount()'

# c(1,3,5) is the 'actual argument' or 'calling argument'

######  Variable Scope
# A variable visible only within a function is said
# to be 'local' to that function

# In addcount, k and n are local variables

# Their values inside of the function oddcount()
# is restricted to 'within' the function.
# They 'go away' as soon as the function completes:

oddcount(c(1,2,3,7,9))
n

# all formal parameters are local; Are 'scoping rules'

# Variables created in the global environment are 'global'

# Here y is a global variable:
f <- function(x) return (x+y)
y <- 3
f(5)

# The 'inside' (scope) of function f can 'see'
# the value of the global y variable

# Can write a global variable from within a function
# using the superassignment operator, <<- .

##### Default arguments
# Many functions have 'default' (valued) arguments

g <- function(x,y=2,z=T) {...}

# If formal parameters 'y' and 'z' are not called
# in the function call for g(), ie g(6), then x=6
# Then y defaults to 2 and z defaults to 'TRUE'

# what about g(12,z=FALSE)?
# x takes on value of 12
# y takes on default value of 2
# the z default value of 'TRUE' is overridden
# with a value of FALSE.

#### WE CAN CONTINUE WITH FUNCTIONS IF WE HAVE TIME
