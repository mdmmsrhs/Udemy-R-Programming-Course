sig <- function(p) {
  sg <- FALSE
  if(p < 0.05) sg <- TRUE
  return(sg)
}

sig(0.01)
sig(0.06)