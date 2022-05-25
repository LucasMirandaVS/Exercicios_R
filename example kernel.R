d <- density(mtcars$wt)

plot(d, main= "Weight")
polygon(d, col = "red", border = "blue")