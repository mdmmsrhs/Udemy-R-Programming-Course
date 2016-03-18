T.test <- function(y1,y2) {
  if(!hasArg(y1) || !hasArg(y2))
    stop("Two datasets required")
  
  sig <- function(p) {
    sg <- FALSE
    if(p < 0.05) sg <- TRUE
    return(sg)
  }

  n1 <- length(y1) # number of elemnts in y1
  n2 <- length(y2) # number of elemnts in y2
  y.bar1 <- mean(y1)
  y.bar2 <- mean(y2)
  s1 <- var(y1)
  s2 <- var(y2)
  s.sq <- (((n1 - 1)*(s1)) + ((n2 - 1)*(s2))) / ((n1 - 1) + (n2 -1))
  s <- sqrt(s.sq)

  t <- (y.bar1 - y.bar2) / (s * sqrt((1 / n1) + (1 / n2)))
  p <- 2*pt(-abs(t),df=(length(test1)+length(test2))-1)
  a <- sig(p)
  
  out <- list(t, p, a)
  names(out)[[1]] <- "Value of Students T returned"
  names(out)[[2]] <- "Probability of Significance"
  names(out)[[3]] <- "Is Significant?"
  
  return(out) # return is optional but left here for clarity
}

test1 <- rnorm(1000,0,50)
test2 <- rnorm(1000,5,50)
test3 <- rnorm(1000,20,50)
options(scipen=4)

par(mfrow=c(1,3))
hist(test1,breaks=30,border=2)
hist(test2,breaks=30,border=3)
hist(test3,breaks=30,border=4)
summary(test1) ; summary(test2) ; summary(test3)
#Homegrown T Test Function
T.test(test1,test2)
T.test(test1,test3)
T.test(test1)

#Internal t Test Function
t.test(test1,test2)
t.test(test1,test3)

