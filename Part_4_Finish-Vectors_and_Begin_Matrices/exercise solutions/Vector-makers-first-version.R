##################################################
#####     USER-DEFINED  VECTOR  MAKERS       #####
##################################################
#####            FIRST  VERSION              #####
##################################################

# Create a user-defined function "vector.maker.num()"
# that generates a vector of numbers. Do not use 
# the function vector() inside of any of these 
# user-defined functions!  vector.maker.num() 
# should have no formal arguments. When it is 
# called, it should return a vector of 5 random 
# numbers from the set {1,2,3,4,5,6,7,8,9,10}.

vector.maker.num <- function() {
  sample(1:10,5)
}

vector.maker.num()

# Create a user-defined function "vector.maker.alph()"
# that generates a vector of characters. Here is a 
# helpful tip: the native R object 'letters' returns 
# the 26 lower-case letters of the alphabet as 
# character elements. vector.maker.alph() should 
# have no formal arguments. When it is called, it 
# should return a vector of 5 random letters.

vector.maker.alph <- function() {
  sample(letters,5)
}

vector.maker.alph()

# Create a user-defined function "vector.maker.bool()" 
# that generates a vector of logical values (T's 
# and F's). vector.maker.bool() should have no 
# formal arguments. When it is called, it should 
# return a vector of 5 random values of TRUE or 
# FALSE.

vector.maker.bool <- function() {
  sample(c(rep(T,5),rep(F,5)),5)
}

vector.maker.bool()

# Modify each of your three functions so that each 
# accepts an optional argument 'len' which is the 
# length of the vector (that is, the number of 
# elements in the vector) that is returned. Let's 
# say that the permissible range for the optional 
# length argument value is from 5 to 20. As an 
# optional argument, when the user does not include 
# the len= argument when calling one of the vector 
# maker functions, the function should still 'work' 
# and return a vector of 5 elements by default.
#------------------------------------------------
# for sequence 1 to 10, len cannot > 10 (not 20)
# len is optional argument with default value 5
# get error message if len < 5 or len > 10
vector.maker.num1 <- function(len=5) {
  if(len<5) {
    # show() displays an object, by printing,
    # plotting or whatever suits its class
    show("len is too short!")
  } else if (len>10) {
    show("len is too long!")
  } else {
    sample(1:10,len)
  } 
}
?show
vector.maker.num1(11)
#------------------------------------------------
vector.maker.alph1 <- function(len=5) {
  # error message if len > 20
  if(len<5) {
    show("You want too few letters!")
  } else if (len>20) {
    show("You want too many letters!")
  } else {
    sample(letters,len)
  }
}
?system.time
vector.maker.alph1()
#------------------------------------------------
vector.maker.bool1 <- function(len=5) {
  # Need to sample from a pool of at least 20
  if(len<5) {
    show("Out of bounds on low side!")
  } else if (len>20) {
    show("Out of bounds on high side!")
  } else {
    sample(c(rep(T,10),rep(F,10)),len)
  }
}

vector.maker.bool1(4)