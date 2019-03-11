attach(dugong)
plot(x= Age, y=Length, xlab = "Age/Years", ylab = "Length/m",main = "Dugongs 2-Parameter Asymptotitc Model", ylim = c(1.5,3.0), pch = 16, cex = 1,text(25,2.3, expression(y==a(1-e^-bx))))
#calculate b x = 2.4 y = 2.02
b <- (-log(1-2.4/2.72))/2.02
#build 2 param model
model2Param <- nls(Length~a*(1-exp(-b*Age)), data = dugong, start = list(a=2.72,b= 1.059439))
summary(model2Param)
cbind(coef(model2Param), confint(model2Param))
curve(2.4438939 * (1-exp(-0.9588507 * x)), from = 0, to = 31,col = "darkgreen", add = T)
curve(2.3712513 * (1-exp(-0.7779374 * x)), from = 0, to = 31,col= "red", lty = "dashed", add = T)
curve(2.517077*(1-exp(-1.209606 * x)), from = 0, to = 31,col = "blue", lty = "dashed", add = T)