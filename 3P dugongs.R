attach(dugong)
#plot basic curve
plot(x= Age, y=Length, xlab = "Age/Years", ylab = "Length/m",main = "Maturation of Dugongs 3-Parameter Asymptotitc Model", ylim = c(1.5,3.0), pch = 16, cex = 1,text(25,2.3, expression(y==a-be^-cx)))
#calcluate value of c 
a <- max(dugong$Length)
c <--log((2.72-2.32)/1.7)/12.0
#create 3 param model 
model3Param <- nls(Length~a-b*exp(-c*Age),start = list(a=2.72, b=1.70, c= 0.1205766))
summary(model3Param)
#predic x and y values from the model 
xpred <- seq(0,32,0.01)
ypred <- predict(model3Param, list(Age=xpred))
lines(xpred,ypred,col="darkgreen")
cbind(coef(model3Param), confint(model3Param))
#fitting the curves with CIs 
curve(2.55860841-0.83924655 * exp(-0.08752864 * x), from = 0, to = 31,col = "red", add = T, lty = "dashed")
curve(2.8209863-1.1072364 * exp(-0.2054748 * x), from = 0, to = 31,col = "blue", add = T, lty = "dashed")
