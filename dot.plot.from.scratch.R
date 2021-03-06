# 
#
# Building a graph from scratch
# To show how R allows you to customise your graphs


# --- SIMULATE DATA ---
# vector for the x values: 
# the () around x <- c(1:10) makes R show the output
(x <- c(1:10))

# a set of 10 random numbers between 3 and 10 
(y <- runif(10, 3,10))

# --- start plotting -------
# lets plot this 
plot(x,y)


# what does R really do?
# ADVANCED: trick to get the min and max values for the x and y axis
xlim.t <- range(x)
ylim.t <- range(y)

# plot an empty plot
plot(NA,  ylim = ylim.t, xlim = xlim.t, xlab = "", ylab = "", xaxt = "n", 
     yaxt = "n", bty = "n")  # 

# draw linea round the plotting are (set by the xlim and ylim as defined above)
box()


# R basically provides you with a frame to draw on
# let's draw the same points as in the plot(x,y)
points(x,y)


# add a single point
points(3, 5, pch = 19, col = "purple", cex = 3)

# add axis
axis(1); axis(2);axis(4)
# hopefully this gives you an idea about the moudaliry of an R plot.
# many of the different components can be customised
# in the next section I show how you can do some fun changes to your plot
# to make it much better than the standard version


# --- MAKING THE GRAPH ---
# lets use plot and take control over the plotting
plot(x,y)

# how can we manipulate this?
# use ? to see what the plot function has to offer
?plot
# for many tweaks such as line and symbol types, check
?par

# I dislike the orientation of the y labes, change to horizontal (las = 1)
# also, like the circle to be filled (pch = 19) and red (col = red)
plot(x, y, las = 1, pch = 19, col = "red")

# R by default determines the sizes of the axis, but we can set this
# y axis is too short ylim = c(0, 12)
plot(x,y, las = 1, pch = 19, ylim = c(0,12), col = "red")

# don't like the y label, use ylab = (or xlab for the x-axis, main for the main title)
plot(x,y, las = 1, pch = 19, ylim = c(0, 12), col = "red", ylab= "random stuff", main="how graphs work")


#to many tickmarks for my taste. R lets you suppress any axis (xaxt = "n" for xor yaxt for y) and determines yourself 
# how you like it using axis() 
plot(x, y, las = 1, pch = 19, ylim = c(0, 12), col = "red", yaxt = "n")
# ok, axis ticks and labels gone
# let's make my own. First argument gives which axis you want: 1 = x, 2=y,3 top, 4 right side
axis(2, c(2, 6, 10), labels = c("low","medium","high"))
abline(h=c(2, 6, 10), col = "red")
# lets add some more data
y2 <- runif(10, 0,12)
# make sure this still falls with the range as set by ylim!!!
# lets plot y2 againts x (make sure this has the same number of entries (use length()))
# and make them nice and big (use lwd) 
points(x, y2, pch = 19, col = "purple", cex = 4)

# we can add a line wherever we'd like
lines(c(3, 7.5), c(6, 8), col= "grey", lwd = 3, lty = "dashed")

# this is panel A, so we like to have an A in the left top. at coordinates 1.5, 11.5
# but pretty big! (use cex (1 is standard))
text(1.5, 11.5, "A", cex = 2)

# add a rather random legend (just to show how it works)
legend("topright", c("P", "R"), pch = c(19, 19), col = c("purple", "red"))


