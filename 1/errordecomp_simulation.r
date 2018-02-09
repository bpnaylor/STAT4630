gety <- function(x,n) {
	# generate noise
	noise = rnorm(n, mean=0,sd=1)

	# calculate y-value
	y = 0.2*x + sin(x) + noise

	# print(y-0.2*x+sin(x))
	return(y)
}

f.star = 0.2*7 + sin(7)
y.hats = c()
se = c()
residuals = c()

for (i in 0:999) {
	# generate x
	x = seq(-10,10,by=0.1)

	# generate y
	y = gety(x,200)

	# create dataframe for x,y
	df1 = data.frame(x,y)

	# fit line of best fit
	f.hat = lm(y ~ x + sin(x), df1)

	# simulate & predict new datapoint at x=7
	y.new = gety(7,1)
	df2 = data.frame(x=7,y=y.new)
	y.hats[i] = predict(f.hat, newdata=df2)
	se[i] = (y.new - y.hats[i])**2
	residuals[i] = (y.new - f.star)**2

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

# calculate bias
f.bias = (f.bar - f.star)**2
print(f.bias)

# # calculate standard error
# se.bar = mean(se)
# print(se.bar)

# compare to theoretical quantities
# mean of predictions at x=7
# Theoretical: E[Y | x = 7] = E[f(7) + \epsilon] = f^*(7) = 0.2(7) + sin(7) = 2.056987
# Simulation: E[\hat{f}(x) | x=7] = \bar{f}(7) = f.bar = 2.060483

# variance of predictions at x=7
# Theoretical:
# Simulation: 

# theoretical bias:
# bias = E[]

# what would happen if only saw half x values to find the line of best fit?