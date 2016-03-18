#############################################
#####        PROGRAMMING IN R           #####
#############################################

# Before we get into vectors, a couple more words
# about help
help(seq)

# There is also a function example()
# Note examples at end of sequence() help screen
example(seq)

# Let's try dnorm()
help(dnorm)

example(dnorm)

# What about rnorm()?

# example() runs the examples at the end of the
# help 'cheat sheet'....very useful

# We look at help for perspective plots:
?persp

example(persp)

# There is also a demo() function for most
# packages....with persp() it extends the examples

demo(persp)

############################################
#######           VECTORS           ########
############################################

# The vector is the fundamental data type in R.

# First we take a look at the c() (combine)
# function or operator used to create vectors

# Then we look at how vectors relate to other
# data types in R

# Then we look at three topics central to R
# Programming:

# 1) Recycling: the automatic lengthening of
#    vectors in certain settings.
# 2) Filtering: the extraction of vector subsets.
# 3) Vectorization: element-wise application of
#    functions to vectors.

c(1,7:9)
# [1] 1 7 8 9

c(1:5, 10.5, "next")
# [1] "1"    "2"    "3"    "4"    "5"    "10.5" "next"

## uses with a single argument to drop attributes
x <- 1:4

names(x) <- letters[1:4]
x
# a b c d 
# 1 2 3 4 

c(x)          # has names
# a b c d 
# 1 2 3 4 

as.vector(x)  # no names
# [1] 1 2 3 4

# give it dimension, no longer a vector
dim(x) <- c(2,2)

x
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4

## append to a list:
ll <- list(A = 1, c="C")

## do *not* use
c(ll, d = 1:3) # which is == c(ll, as.list(c(d=1:3))

## but rather
c(ll, d = list(1:3))# c() combining two lists

## SCALARS, VECTORS, ARRAYS, AND MATRICES

# R variable types are called 'modes'
# All elements in a vector must have same mode:
# integer, numeric (floating-point), character
# (string), logical (Boolean), complex, and raw.

# typeof() function checks the mode.

### Adding and Deleting Vector Elements

# Vectors are stored in arrays, contiguously,
# and so you cannot insert or delete elements
# without reassigning (recreating) the vector.

# inserts '289' as fourth element
a <- c(96,12,55,38,65);a

# vector 'a' is a new vector
a <- c(a[1:3],289,a[4:5]);a

# We did not 'change' the vector stored in 'a',
# we created a new vector and stored it in 'a'.

# May seem subtle, but has implications, especially
# for performance issues using R, as we see later.

#### Obtaining the length of a vector

# use the length() function

b <- c( c(1:5), c(2:4))
length(b)
b

# don't need to run length() for this simple example,
# but when writing general function code, you will
# find this function comes in handy w/ vector arguments.

# Let's say we want a function to return value of
# 3rd element of any vector:

third <- function(x){
  # without length() here would need another argument for length
  for (i in 1:length(x)){
    # could not write above line 'for (n in x)'
    # and still retrieve index of desired element
    if (x[i] == 3) break # breaks out of the loop
  }
  cat("Third element is: ")
  return(i)
}

# call it:
b;third(b)

# In above example, also must be careful that length(x) is not 0
x <- c(); x
length(x)

# call the function
third(x)

#### Matrices and Arrays as Vectors

# Are actually vectors, too, with extra class attributes
# Matrices have numbers of rows and columns
# Arrays are simply multi-dimensional (3,4,5, more...)

# So everything we claim is true with a vector is true
# for matrices and arrays, also

# Consider:
m <- matrix(1:9,3,3);m

# can reference by row and column:
m[2,2]

# or also as all columns 'stacked':
m[5]

# 3 x 3 matrix m is stored as nine-element vector,
# column-wise, as (1,2,3,4,5,6,7,8,9)

# Is represented as
show.matrix <- function(x){
  print(x)
  for (i in 1:length(x)){
    print(x[i]) # then change print to 'cat'
  }
}

# we call it:
show.matrix(m)

# we add a vector of nine integers to m:
m + 10:18

# you then have (11,13,15,17,19,21,23,25,27)
# but R knows you are working with a matrix
# and returns a 3 x 3 result

###### Declarations

# Compiled languages require you to declare variables
# warn interpreter, compiler of variable's existence

# With scripting languages (Python, Perl, R), you
# do not declare variables
# Here is no prior reference to z and is ok
z <- 3

# BUT, if you reference specific elements of a vector,
# you must 'warn' R. The following will not work
w[1] <- 5  # won't work if 'w' does not exist
w[2] <- 12 # won't work if 'w' does not exist

# these do not work due to R's functional language
# nature. The reading and writing of individual vector
# elements are handled by functions. If R does not
# know of the vector to use, these functions have
# nothing to act on.

# However, note that z exists, is one element:
z; length(z)
z[3] <- 4
z; length(z)

# Go back to w, does not exist
w
w <- vector(length=2)
w
w[1] <- 15
w
w[2] <- 12
w

# The following will also work:
d
d <- c(23,26);d

# it works because we create the vector on right-hand side
# and then bind to d

# However, just as variables are not declared, they are
# also not constrained to mode
x <- c(1,5);x
typeof(x)
x <- 'abc';x
typeof(x)

# or
x <- c(1,5);x
typeof(x)
as.character(x)
typeof(as.character(x))
x
typeof(x)

###### Recycling

# When applying an operation to two vectors that ordinarily
# requires them to be same length, R automatically recycles,
# or repeats, the shorter one, until it is long enough to
# match the longer one:
c(3,2,7,6) + c(2,4,1)

# The shorter vector gets recycled, so R sees operation as:
c(3,2,7,6) + c(2,4,1,2) # with two elements left over: 4 and 1

# if they are multiples, no warning:
c(3,2,6,7,9,4) + c(3,6,1)

# also works with matrices
x <- matrix(1:6,3,2);x
x + c(1,2) # are adding a vector to a matrix

##### Common Vector Operations

# Vector Arithmetic of Logical Operations
# All operators, like '+' are actually functions
2+6
"+"(2,6)
?"+"

# Scalars are one-element vectors, so we can add vectors
# and '+' operation is applied elementwise
x <- c(5,3,2);x
x + c(2,4,0)

# If you know linear algebra, you may be surprised at what
# happens when we multiply two vectors
x
x * c(1,2,3)

# It performs the multiplication element by element

# Other numeric operators work the same way

x <- c(1,2,4);x
x / c(5,4,-1)

##### Vector Indexing
# Allows us to form subvectors by selecting elements of another
# vector for specific indices: format is vector1[vector2]

y <- c(1.2,3.9,0.4,0.12);y
y[2:3]
v <- 3:4;v
y[v]

# Negative subscripts mean "include everything else"
z <- c(12,5,64);z
z[-1] # include all but 1
z[-1:-2] # include all but 1 and 2

z[-1:2] # returns an error

# length() function can come in handy

w <- c(2:9, 26);w
w[2:(length(w)-2)] # removes first and last two elements

# Generating Useful Vecotrs: The ':' Operator

# ':' produces a vector over a range of numbers
4:9
6:2 # reverses enumeration

# Can cause issues in looping as we see later

# seq() is a generalization of ':' which generates
# a sequence in arithmetic progression

# elements one unit apart
3:8

# can put them 3 units apart with seq()
seq(from=15,to=36,by=3)

# spacing does not have to use integer values
seq(from= 1.5,to=3,length=10)

# Repeating Vector Constants with rep()

# rep() function, called both repeat and replicate, can be
# used for convenience to put same constant into long vectors.

# The call form is rep(x,times) which creates a vector of
# times*length(x) elements...that is, times copies of x

# repeats 5 6 times
x <- rep(5,6);x

# repeats vector (5,12,13) three times
rep(c(5,12,13),3)

# repeats sequence 1 to 4 two times
rep(1:4,2)

# Is use named argument 'each' it interleaves copies of x
rep(c(5,12,13),each=2)

### Using all() and any()
# 'shortcut' functions return TRUE or FALSE if
# all, or any, of their arguments are true, respectively
x <- 3:20;x
any(x > 8)  # yep
any(x > 23) # nope
all(x > 13) # nope
all(x > 2)  # yep

# Extended Example: Finding Runs of Consecutive Ones
# Suppose we want to find runs of consecutive 1's in
# vectors that consist of just 0's and 1's

# vector (1,0,0,1,1,1,0,1,1) has run of length 3 starting 
# at position 4, and runs of length 2 beginning at indices 
# 4, 5 and 8.

# Here is our code:
## Program findruns.r 
findruns <- function(x,k) { # x is vector, k is length of run
  n <- length(x) # so n is length of vectors
  runs <- NULL # initialize to NULL
  for (i in 1:(n-k+1)) { # check as far as need
    # determines whether all of the k values starting at
    # x[i]...x[i],x[i+1],x[i+k-1] are 1's
    # expression 'x[i:(i+k-1)]' gives us this range in x, 
    # we apply all to see if there is a run there
    if (all(x[i:(i+k-1)]==1)) runs <- c(runs,i)
  }
  return(runs)
}

y <- (c(1,0,0,1,1,1,0,1,1));y
findruns(y,3) # one begins at 4
findruns(y,2) # three begin at 4, 5 and 8
findruns(y,6) # there aren't any

# it works but is a buildup of the vector runs
# has to reallocate a new vector in the call(runs,i)

# no problem on the short end, could cause a problem
# if had lots and lots of data.

# Can fix by preallocating the memory space to runs

## Program findruns1.r
findruns1 <- function(x,k) {
  n <- length(x)
  # set up space of vector of length n
  # so no new allocations during loop
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
  } else runs <- NULL
  return(runs)
}

# Latter version reduces the number of memory allocations

#### Extended Example: Predicting Discrete-Valued Time Series

d1 <- read.csv(file = "weather.csv")
head(d1)
str(d1)
today <- ifelse(d1$RainToday=="No",0,1)
tomorrow <- ifelse(d1$RainTomorrow=="No",0,1)

# Have 0- and 1-valued data, one per time period, 1 rain, 0 no
# Want to predict what will happen tomorrow...for some number
# k, predict tomorrow based on weather in last k days.

# Use 'majority rule': If number of 1's in previous k periods 
# is at least k/2 we predict next day to be 1, otherwise 0.

# How choose k? Use a training set. Ask how various values
# of k would have performed when we know the outcome.

# Weather: have 500 days of data are consider k = 3. So we
# 'predict' each day from previous 3 days and see how we did.

# Get error rate for k = 3. Then do k = 1, k = 2, etc up to 
# some maximum value of k. Then use one value of k that 
# works best in future.

# Here is naive approach to code this scenario:
## Program preda.r
preda <- function(x,k) {
  n <- length(x)
  k2 <- k/2
  # the vector pred will contain our predicted values
  pred <- vector(length=n-k)
  for (i in 1:(n-k)) {
    # heart of program: are predicting day i+k from k days 
    # previous and storing results in pred[i] so we count
    # 1's in those days
    # which is sum(x[i:(i+(k-1))])
    if (sum(x[i:(i+(k-1))]) >= k2) pred[i] <- 1 else pred[i] <- 0 
  }
  # pred has predicted values; x[(k+1):n] has actual 
  # values for days in question; subtracting gives 
  # us -1,0, or 1. Only 0 is a good prediction; 
  # abs() gets rid of -1's, still 0 is good one.
  return(mean(abs(pred-x[(k+1):n])))
  # mean() gives us the proportion
}

preda(today,2)
preda(today,3)
preda(today,4)
preda(today,5)

# preda.r does a lot of duplicate computation. For 
# successive values of i in the loop, sum() is being 
# called on vectors that differ by only two elements. 
# Except when k is very small, this slows
# things down a lot.

# So we re-write it so that at each iteration of the 
# loop we update the previous sum, instead of computing 
# a new sum "from scratch"
## Program predb.r
predb <- function(x,k) {
  n <- length(x)
  k2 <- k/2
  pred <- vector(length=n-k)
  sm <- sum(x[1:k])
  if (sm >= k2) pred[1] <- 1 else pred[1] <- 0
  if (n-k >= 2) {
    for (i in 2:(n-k)) {
      # this line is key: we update sm by subtracting the
      # oldest element making up the sum (x[i-1]) and 
      # adding the new one (x[i+k-1])
      sm <- sm + x[i+k-1] - x[i-1] # more efficent
      if (sm >= k2) pred[i] <- 1 else pred[i] <- 0
    }
  }
  return(mean(abs(pred-x[(k+1):n])))
}

predb(today,2)
predb(today,3)
predb(today,4)
predb(today,5)

# use cumsum() forms cumulative sums from a vector
y <- c(5,2,-3,8)
cumsum(y)

# replace expression sum(x[i:(i+(k-1)) in preda with cumsum()

## Program predc.r
predc <- function(x,k) {
  n <- length(x)
  k2 <- k/2
  # the vector red will contain our predicted values
  pred <- vector(length=n-k)
  csx <- c(0,cumsum(x))
  for (i in 1:(n-k)) {
    if (csx[i+k] - csx[i] >= k2) pred[i] <- 1 else pred[i] <- 0
  }
  return(mean(abs(pred-x[(k+1):n])))
}

predc(today,2)
predc(today,3)
predc(today,4)
predc(today,5)

# predc.r requires just one subtration operation per loop
# iteration, compared to two in predb.r

######## VECTORIZED OPERATIONS

# Means a function applied to a vector is actually applied
# individually to each element

# Vector In, Vector Out

# Comparison done on each pair of elements
u <- c(5,2,8)
v <- c(1,3,9)
u > v

# Using a vectorized operation in a function speeds things
w <- function(x) return(x+1)
w(u)

# w() uses + which is vectorized, so w() is vectorized 
# as well

# What about vectorized functions that appear to use 
# 'scalars':
f <- function(x,c) return((x+c)^2)
f(1:3,0)
f(1:3,1)

# we intended for c to be scalar but this works too
f(1:3,1:3) # ends up adding the elements of the two 
           # vectors first

# If you want to restrict c to scalars, could do
f <- function(x,c) {
  if (length(c) !=1) stop("vector c not allowed")
  return((x+c)^2)
}

f(1:3,2)
f(1:3,1:3)

### A Vectorized If-then-else: The ifelse() Function

# The if-then-else function in R is vectorized
# form is ifelse(b,u,v) where b Boolean vector, 
# u and v are vectors

# return value is also a vector; element i is u[i] if b[i]
# is true, or v[i] if b[i] is false
x <- 1:10
# returns 5 for even elements, 12 for odd elements:
y <- ifelse(x %% 2 == 0,5,12) # %% is modulo operator
y

x <- c(5,2,9,12)
# first two elements multiplied by 3 because FALSE
ifelse(x > 6,2*x,3*x)

# Is potentially much faster than 'standard' if-then-else
# functions because it is vectorized

### AN EXTENDED EXAMPLE: A MEASURE OF ASSOCIATION

## Alternatives to Correlation, to Pearson product-moment
## For example, Spearman rank correlation which is robust
## to outliers

## We propose a new one to illustrate R code and ifelse()

## Consider vectors x and y, which are time series, say for
## measurements of air temperature and pressure collected
## once each hour.

## We define measure of association between them to be the
## fraction of time x and y increase together, or decrease
## together....that is, the proportion of i for which
## y[i+1]-y[i] has the same sign as x[i+1]-x[i]:

## Program findud.r
# findud() converts vector v to 1s, 0s, representing an 
# element increasing or not, relative to the previous one; 
# output length is 1 less than input
findud <- function(v) {
  # this line and next recode 
  vud <- v[-1] - v[-length(v)]  # subtracts each element 
  # from previous into 1's and -1's
  return(ifelse(vud > 0,1,-1))  
}

udcorr <- function(x,y) {
  # put x and y into a list so no need to process them twice w findud
  ud <- lapply(list(x,y),findud) # note lapply() returns a list
                  # with components of 1 and -1 coded vectors
  # next line finds the fraction of matches
  # ud[[1]] == ud[[2]] returns vector of TRUE and FALSE values
  # which are treated as 1 and 0 values by mean and gives 
  # us our fraction:
  return(mean(ud[[1]] == ud[[2]]))
}

x <- c(5,12,13,3,6,0,1,15,16,8,88)

y <- c(4,2,3,23,6,10,11,12,6,3,2)

## In x and y above, they increase together 3 times
## and decrease tgether once, out of 10 chances.

# So the measure of association is 4/10 = 0.4.

# We need to recode x and y into sequences of 1's and -1's
# which we do as explained in the functions above

udcorr(x,y)

# return(mean(ud[[1]] == ud[[2]])) above returns fractions 
# of matches

### AN EXTENDED EXAMPLE: RECODING A DATA SET

# Due to the vector nature of the arguments, 
# you can nest ifelse() operations.
# In the following example, gender
# is coded as M, F, or I (for infant). 
# We wish to recode those characters as 1, 2,
# or 3. The real data set consists of more than 
# 4,000 observations, but for our example
# we'll say we have just a few, stored in g:
  
g <- c("M","F","F","I","M","M","F")
g

ifelse(g == "M",1,ifelse(g == "F",2,3)) # principle of lazy evaluation:
# expression is not computed unless needed

# since these vectors could be columns of matrices
# abalone data in matrix ab

# gender stored in first colum
ab <- read.csv(file="Abalone.data", header=FALSE)
names(ab)
ab <- as.matrix(read.csv(file="Abalone.data", header=FALSE))
head(ab)

# we could recode
ab[,1] <- ifelse(ab[,1] == "M",1,ifelse(ab[,1] == "F",2,3))
head(ab)

# Start Over

## Abalone data set extended example
## Program abal.r

# Want to plot diameter versus length with separate
# plot males and females

aba <- read.csv("abalone.data",header=T,as.is=T)
colnames(aba, do.NULL = TRUE, prefix = "col")
colnames(aba) <- c("Gender","Length","Diameter","Height","WholeWt","ShuckedWt","ViscWt","ShellWt","Rings")
grps <- list()
for (gen in c("M","F")) grps[[gen]] <- which(aba[,1]==gen)
abam <- aba[grps$M,]
abaf <- aba[grps$F,]
plot(abam$Length,abam$Diameter)
plot(abaf$Length,abaf$Diameter,pch="x",new=FALSE)

pchvec <- ifelse(aba$Gender == "M","o","x")
plot(aba$Length,aba$Diameter,pch=pchvec)

# Testing Vector Equality

# Suppose we wish to test whether two 
# vectors are equal. The naive approach,
# using ==, won't work.
x <- 1:3
y <- c(1,3,4)
x == y

# Are dealing with vectorization and "==" is a function
"=="(3,2)
i <- 2
"=="(i,2)

# "==" applies to each element of the function
# can use all()

x <- 1:3
y <- c(1,3,4)
x == y

all(x == y)

# or better still, the identical() function
identical(x,y)

# But be careful,:
x <- 1:2    # produces integers
y <- c(1,2) # produces floating point numbers
x
y
identical(x,y) # not the same

# What happened ?!
typeof(x)
typeof(y)
