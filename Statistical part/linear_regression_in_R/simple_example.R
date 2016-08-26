data(women)    # R demo dataset with only two columns
str(women)     # get quick summary of dataset structure
attach(women)  # make the namespace of "women" accessible

pdf("women.pdf")
plot(women, pch=20, col="blue") # plot the dataset
dev.off()


pdf("women1.pdf")
plot(women, pch=20, col="blue") # plot the dataset

linear.model = lm( weight ~ height )  # linear model: weight vs height
abline(linear.model, col="red") # superimpose a red line (the fit)
dev.off()

summary(linear.model)

pdf("women2.pdf")

plot(women, pch=20, col="blue") # plot the dataset
abline(linear.model, col="red") # superimpose a red line (the fit)

model.without.intercept = lm( weight ~ height - 1 )
abline(model.without.intercept, col="green") # superimpose a green line
summary(model.without.intercept)

quadratic.model.without.intercept = lm( weight ~ height + I(height^2) - 1 )
summary(quadratic.model.without.intercept)

cubic.model.without.intercept = lm( weight ~ height + I(height^2)  + I(height^3) - 1 )
summary(cubic.model.without.intercept)

quadratic.predictions = predict( quadratic.model.without.intercept, data.frame( height = height ) )
lines(height, quadratic.predictions, col="blue") # superimpose a blue line

cubic.predictions = predict( cubic.model.without.intercept, data.frame( height = height ) )
lines(height, cubic.predictions, col="purple") # superimpose a blue line

legend("topleft",
       c("linear with itercept", "linear without intercept", "quadratic without intercept", "cubic without intercept"),
       col=c("red", "green", "blue", "purple"), lwd=3)
dev.off()


pdf("women3.pdf")

plot( height, model.without.intercept$residuals,
      type="o", pch=20, col="green", main = "residuals" )

lines( height, linear.model$residuals,
      type="o", pch=20, col="red")

lines( height, quadratic.model.without.intercept$residuals,
      type="o", pch=20, col="blue")

lines( height, cubic.model.without.intercept$residuals,
      type="o", pch=20, col="purple")

legend("topleft",
       c("linear with itercept", "linear without intercept", "quadratic without intercept", "cubic without intercept"),
       col=c("red", "green", "blue", "purple"), lwd=3)
dev.off()



