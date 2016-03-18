####################################################
#####    HOMEMADE TWO SAMPLE T-TEST FUNCTION   #####
####################################################
?vector
# The Long Way:

T.test <- function(y1, y2){ # In body of function
  # first find n1 and n2
  n1  <- length(y1) 
  n2  <- length(y2)
  y.bar1 <- mean(y1)
  y.bar2 <- mean(y2)
  # then calculate the numerator of t
  t.num <- (y.bar1 - y.bar2)
  s1  <- var(y1)
  s2  <- var(y2)
  # Then calculate pooled variance and hence s
  s.sq.num <- ((n1-1)*s1)+((n2-1)*s2)
  s.sq.den <- (n1-1)+(n2-1)
  s.sq <- s.sq.num/s.sq.den
  s <- sqrt(s.sq)
  # Calculate value of t and return it
  t.value <- t.num/(s*sqrt(1/n1 + 1/n2))
  return(t.value)
}

male <- rnorm(1000,0,100)
hist(male)
summary(male)

female <- rnorm(1000,10,100)
hist(female)
summary(female)

# This one is homemade:
tstat <- T.test(female,male); tstat

# Compare homemade results to the t.test() function in R
tst <- t.test(female,male); tst 

# Homemade function can be written more compactly as:

T.test2 <- function(y1, y2) {
  n1  <- length(y1); n2  <- length(y2)
  yb1 <- mean(y1);   yb2 <- mean(y2)
  s1  <- var(y1);    s2  <- var(y2)
  s <- ((n1-1)*s1 + (n2-1)*s2)/(n1+n2-2)
  t.value <- (yb1 - yb2)/sqrt(s*(1/n1 + 1/n2))
  t.value
}

tstat <- T.test2(female,male); tstat