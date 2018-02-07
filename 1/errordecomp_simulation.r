gety <- function(x) {
	# n <-lengths(x)
	n = 201

	# generate noise
	noise = rnorm(n, mean=0,sd=1)

	# calculate y-value
	y = 0.2*x + sin(x) + noise

	# print(y-0.2*x+sin(x))
	return(y)
}

y.hats = c()
es = c()

for (i in 0:999) {
	# generate x
	x = seq(-10,10,by=0.1)

	# generate y
	y = gety(x)

	# create dataframe for x,y
	df1 = data.frame(x,y)

	# fit line of best fit
	f.hat = lm(y ~ x + sin(x), df1)

	# simulate & predict new datapoint at x=7
	y.new = gety(7)[1]
	df2 = data.frame(x=7,y=y.new)
	y.hats[i] = predict(f.hat, newdata=df2)
	es[i] = (y.new - y.hats[i])**2
}

# display results
summary(f.hat)
plot(x,y)
abline(f.hat)

# calculate mean of predictions at x=7
f.bar = mean(y.hats)
print(f.bar)

# calculate variance of predictions at x=7
f.var = var(y.hats)
print(f.var)

# TODO
# calculate bias
# f.bias = 
# print(f.bias)

# compare to theoretical quantities
# what would happen if only saw half x values to find the line of best fit?