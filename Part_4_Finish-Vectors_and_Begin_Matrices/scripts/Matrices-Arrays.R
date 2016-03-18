################################################
#######       MATRICES AND ARRAYS       ########
################################################

# Matrix is a vector with two additional attributes:
# 1) the number of rows; and 2) the number of columns

# Marices have modes, ie numeric, character

### CREATING MATRICES

# Matrix function

# Internal storage matrix is 'column-major-order'
# which means it stores by columns: 1, then 2, etc

(y <- matrix(c(1,2,3,4),nrow=2,ncol=2))

# can leave off one dimen or other

(y <- matrix(c(1,2,3,4),ncol=2))

# returns second column
y[,2]

# can specify individual elements
(y <- matrix(nrow=2,ncol=2))
y[1,1] <- 1
y[2,1] <- 2
y[1,2] <- 3
y[2,2] <- 4
y

# can override column-major-order with byrow = T

m <- matrix(c(1,2,3,4,5,6),nrow=2,byrow=T)
m

### GENERAL MATRIX OPERATIONS

# Performing Linear Algebra

y%*%y # mathematical matrix multiplication

3*y # mathematical multiplication of matrix by scalar

y+y # mathematical matrix addition

# Matrix indexing
(z <- matrix(c(1,2,3,4,1,1,0,0,1,0,1,0), nrow=4, ncol=3))
# extracting columns as submatrix
z[,2:3]

(y <- matrix(c(11,21,31,12,22,32),nrow=3,ncol=2))
# extracting rows
y[2:3,]
y[2:3,2]

# can assign values to submatrices
y
y[c(1,3),] <- matrix(c(1,1,8,12),nrow=2)
y

# another example
x <- matrix(nrow=3,ncol=3)
x
y <- matrix(c(4,5,2,3),nrow=2)
y

x[2:3,2:3] <- y
x

## Filtering on Matrices

# Filtering can be done with matrices. You must be careful
# Let's start with a simple example:
x <- matrix(c(1,2,3,2,3,4),nrow=3)
x

x[x[,2] >= 3,] # second and third rows or original matrix x are TRUE

# another example

z <- c(5,12,13)
x[z %% 2 == 1,] # first and third row of x are TRUE

# can apply vector operations to them:

m <- matrix(1:6,nrow=3)
m

m[m[,1] > 1 & m[,2] > 5,] # only third row true for both conditions

# another

m <- matrix(c(5,2,9,-1,10,11),nrow=3)
m

which(m > 2) # is returning m values as a vector

### EXTENDED EXAMPLE: Generating a Covariance Matix

# Covariance matrix is always symmetric
# element in row 1, col 2 = element in row 2, col 1

# Working with n-variate normal distribution
# with n rows and n columns 
# If we want each of n variables to have variance=1
# with correlation rho. For n=3 and rho=0.2 we have

#   1  0.2  0.2
# 0.2    1  0.2
# 0.2  0.2    1

# can generate this kind of matrix with this code:

makecov <- function(rho,n) { # col(), row() returns col#, row#
  m <- matrix(nrow=n,ncol=n)
  m <- ifelse(row(m) == col(m),1,rho) # row(m) returns matrix
  return(m) # integers values, each one showing row # of corr ele of m
  }

makecov(0.2,3)

# For example, can use this  property:
z <- matrix(3:11,nrow=3)
z  # z is 3 x 3
row(z) # returns the indexes of rows in all columns
row(m)
m
m <- matrix(c(m,4,5,3),nrow=3)
m # none of the element values of m is same as z
z # none of element values of z same as m
col(m) # returns the indexes of columns in all columns
row(z)==col(m) # always true where row = col, false elsewhere
row(m)==col(m) # same return same thing

rho = 0.2
ifelse(row(m)==col(m),1,rho) # puts 1's in diagonal

## we will come back to this property of matrices

##### Applying Functions to Matrix Rows and Columns
# apply() instructs R to call a user-specified
# function of each of the rows or columns

# General form is apply(m,dimcode,f,fargs)
# m is matrix
# dimcode is 1 for rows and 2 for columns
# f is function to be applied
# fargs optional arguments for f function

z <- matrix(1:6, nrow=3, ncol=2)
z

# mean of columns (colMeans() does same thing)
apply(z,2,mean)

# The apply() function is used for applying functions 
# to the rows or columns of matrices or dataframes: 
( X <- matrix(1:24,nrow=4))
#      [,1] [,2] [,3] [,4] [,5] [,6]
#	[1,]   1    5    9   13   17   21
#	[2,]   2    6   10   14   18   22
#	[3,]   3    7   11   15   19   23
#	[4,]   4    8   12   16   20   24 

# Apply a sum() function across the rows or columns: 
apply(X,1,sum)
# [1] 66 72 78 84

apply(X,2,sum)
# [1] 10 26 42 58 74 90

# Apply functions to individual elements (rows)
apply(X,1,sqrt)

# Apply sqrt to columns
apply(X,2,sqrt)

# Apply your own function definition within apply
apply(X,1,function(x)x^2+x)

# another user-supplied function with apply()
f <- function(x) x/c(2,8)
z
y <- apply(z,1,f)
y

# function f() divides two-column vector by 
# vector (2,8); apply transposes it; if you want
# it the way it was
t(apply(z,1,f))

# if the function returns a scalar, the final
# result will be a vector, not a matrix

### FINDING CLOSEST PAIR OF VERTICES IN GRAPH

# Look at an example of finding distances
# between cities....element in row i, column j
# is distance between city i and j

## Program mind
# returns the minimum value of d[i,j], i != j, 
# and the row/col attaining
# that minimum, for square symmetric matrix d; 
# no special policy on ties
# way code is written, only works due to symmetry
mind <- function(d) {
  n <- nrow(d)
  # add a column to identify row number for apply()
  dd <- cbind(d,1:n) # 1:n is added row number to d
  # next statement finds minimum in each row
  # and then find smallest in the minima
  # but code becomes complex
  wmins <- apply(dd[-n,],1,imin) # finds minimum in a row
  # wmins will be 2xn, 1st row being indices and 
  # 2nd being values
  i <- which.min(wmins[2,]) # which.min is an r function
  j <- wmins[1,i] 
  return(c(d[i,j],i,j)) # returns values of minimum
                        # and where it occurs in matrix
}

# apply calls imin() which finds the minimum in a row
# imin returns both the minimum value and the index

# finds the location, value of the minimum in a row x
imin <- function(x) { 
                     lx <- length(x)
                     i <- x[lx]  # original row number
                     j <- which.min(x[(i+1):(lx-1)]) 
                     k <- i+j 
                     return(c(k,x[k]))
}

# Here we put it to use
# need to find the minimum nonzer element in the matrix
q <- matrix(c(0,12,13,8,20,12,0,15,28,88,13,15,0,6,9,8,28,6,0,33,20,88,9,33,0),ncol=5)
q
# call our mind() function
mind(q)

# is complex because minimum value might not
# be unique. If it is unique, is much easier
# if not unique, which() returns multiple row/col pairs

## Program minda
minda <- function(d) {
  smallest <- min(d)
  # this line determines index of element in d
  # that is minimum...arr.ind means 'matrix index'
  # otherwise d treated as a vector
  ij <- which(d == smallest,arr.ind=TRUE)
  return(c(smallest,ij))
}

# minda probably slower (though more compact)
# because it cycles twice: (1) to find minimum;
# (2) for which()

######  NAMING MATRIX ROWS AND COLUMNS

z <- matrix(1:4,2,2);z

colnames(z)
# NULL
colnames(z) <- c("a","b")
z
#     a b
#[1,] 1 3
#[2,] 2 4
colnames(z)
#[1] "a" "b"

z[,"a"]
#[1] 1 2

# another matrix
x <- matrix(rpois(20,1.5),nrow=4)
x
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    1    0    2    5    3
# [2,]    1    1    3    1    3
# [3,]    3    1    0    2    2
# [4,]    1    0    2    1    0

# We want to label the rows ‘Trial.1’ etc.:
rownames(x) <- rownames(x,do.NULL=FALSE,prefix="Trial.")
x
?rownames
#          [,1] [,2] [,3] [,4] [,5]
# Trial.1    1    0    2    5    3
# Trial.2    1    1    3    1    3
# Trial.3    3    1    0    2    2
# Trial.4    1    0    2    1    0

# We want drug names for the columns:
colnames(x) <- c("aspirin","paracetamol","nurofen","hedex","placebo")
x
?colnames
#          aspi..  pare.. nuro.. hede.. plac..
# Trial.1     1       0      2      5      3
# Trial.2     1       1      3      1      3
# Trial.3     3       1      0      2      2
# Trial.4     1       0      2      1      0

# Could also use the dimnames function:
dimnames(x) <- list(NULL,paste("drug.",1:5,sep=""))
x

#      drug.1  drug.2 drug.3 drug.4 drug.5
# [1,]     1       0      2      5      3
# [2,]     1       1      3      1      3
# [3,]     3       1      0      2      2
# [4,]     1       0      2      1      0

# Use rbind and cbind to add rows and columns: 
x <- rbind(x,apply(x,2,mean))
x <- cbind(x,apply(x,1,var))
x
#      [,1] [,2] [,3] [,4] [,5]     [,6]
# [1,]  1.0  0.0 2.00 5.00    3  3.70000
# [2,]  1.0  1.0 3.00 1.00    3  1.20000
# [3,]  3.0  1.0 0.00 2.00    2  1.30000 
# [4,]  1.0  0.0 2.00 1.00    0  0.70000
# [5,]  1.5  0.5 1.75 2.25    2  0.45625

# Label variance and means columns and rows
colnames(x) <- c(1:5,"variance")
rownames(x) <- c(1:4,"mean")
x
#      1    2    3    4 5 variance
# 1  1.0  0.0 2.00 5.00 3  3.70000
# 2  1.0  1.0 3.00 1.00 3  1.20000
# 3  3.0  1.0 0.00 2.00 2  1.30000 
# 4  1.0  0.0 2.00 1.00 0  0.70000
# mean 1.5 0.5 1.75 2.25 2 0.45625

# Higher Dimensional Arrays
# three students with scores on 2 tests

firsttest <- matrix(c(46,21,50,30,25,50),
                    nrow=3)
firsttest

secondtest <- matrix(c(46,41,50,43,35,50),
                     nrow=3)
secondtest

# Put both tests in one data structure,name it 
# 'tests'...is a 3-dimensional array

tests <- array(data=c(firsttest,secondtest),
               dim=c(3,2,2))
tests
attributes(tests)

# Arrays are numeric objects with dimension attributes
array <- 1:25
is.matrix(array)
# [1] FALSE

dim(array)
# NULL

# The vector has no (i.e. NULL) dimensional attributes. So:
dim(array) <- c(5,5)
dim(array)
# [1] 5 5

is.matrix(array)
# [1] TRUE

array
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    1    6   11   16   21
# [2,]    2    7   12   17   22
# [3,]    3    8   13   18   23 
# [4,]    4    9   14   19   24
# [5,]    5   10   15   20   25

is.table(array)
# [1] FALSE

# Three dimensional array of letters
a <- letters[1:24]
dim(a) <- c(4,2,3)
a

# How do these expressions evaluate?:
a[,,1:2]
a[,,3]
a[3,,]