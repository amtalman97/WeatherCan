
#
# various coding tips and tricks
#  
# some random data generation
d <- cbind.data.frame(group = c("A","A","C","Z"), size = c(55,34,23,67), 
                      weight = rnorm(4, 1,4))
str(d) # check the structure

# let's add a column for results
d$results <- NA
# how many unique groups are there?
group.number <- unique(d$group)
## [1] A C Z
# vector to store results, no entry for each row
store.result <- rep(NA, nrow(d))

# let's make a loop to go through all the rows
# assigned different calculations depening in the group
# pretty random actions, how I like it.
for(i in 1:nrow(d)){
  # assigned a number to i (between 1 and nrow(d) ) is handy to work with a
  # single row. You take control of the 'loop'
  # i <- 1

  # let's check what group we are dealing with in row 'i'
  d$group[i]
  
  
  # if statement. Returns TRUE or FALSE. If TRUE it will run the code between {}
  # in case of A
  if(d$group[i] == "A"){
    d$results[i] <- store.result[i] <- d$size[i] * d$weight[i]
  }
  
  # in case of C
  if(d$group[i] == "C"){
    d$results[i] <- store.result[i] <- d$size[i] / d$weight[i]
  }
  
  # in case of Z
  if(d$group[i] == "Z"){
    d$results[i] <- store.result[i] <- d$size[i] + d$weight[i]
  }
}

d$more.data <- rnorm(nrow(d), 4, 3)

columns.compare <- c(2,3,4,5)
store.t <- rep(NA, length(columns.compare)-1)
country.names <- c("wwo","hjk","rr", "oo")

?merge

#for(i in 1: length(columns.compare)-1 ){
  
  i <- 1
  model.t <- lm(d[,columns.compare[1]] ~ d[, columns.compare[i + 1] ])
  
  pdf(paste(country.names[i], ".pdf", sep = ""))
  plot( d[,columns.compare[1]], d[, columns.compare[i + 1] ],las = 1, 
        xlab = "var.x", ylab = "var.x", col = "red", pch = 16,
        main = country.names[i])
  dev.off()
  store.t[i] <- model.t$coefficients[1]
#}


# substring

# string of characters
t <- c("This is pretty awesome")
substr(t, 1, 6)



