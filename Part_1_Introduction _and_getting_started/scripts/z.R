########################################################
#####                                              #####
#####       FILE Z.R GENERATES A GRAPH             #####
#####                                              #####
########################################################

# are Comments...anything after pound sign '#' is ignored
# by the R interpreter

## Check the default directory
getwd()

## Set it to one we will use in this course
setwd("C:/rPrograms")

# call pdf() function to tell R to save graph
# in pdf file 'xh.pdf'
pdf("xh.pdf")  # set graphical output file

# Generate 100 N(0,1) random normal variates 
# and plot their histogram
hist(rnorm(100))

# closes the graphical device we are using
# which is the file 'xh.pdf' in this case.
# This command is mechanism that actually
# causes the file to be written to disk.
dev.off()  # close the graphical output file
