attach(dugong)
plot(x= Age, y=Length, xlab = "Age/years", ylab = "Length/m",main = "Maturation of Dugongs Michealis-Menten Curve", pch = 16, cex = 1, text(25,2.3, expression(y== frac(ax, 1+bx))))
#Calcluate value of b, picking values from estimated coord on graph (x=5, y=2.19), asymptote=2.72
b <-(2.27/2.19) - (1/5)
MMmodel <- nls(Length~a*Age/(1+b*Age), data = dugong, start = list(a=2.72,b=1.033333)))
summary(MMmodel)
xpred <- seq(0,31,0.01)
ypred <- predict(MMmodel, list(Age=xpred))
lines(xpred,ypred,col="darkgreen")
cbind(coef(MMmodel), confint(MMmodel))
curve(3.378036 * x/(1+1.275503 * x), from = 0, to = 31, col = "blue",add = T, lty = "dashed")
                                             curve(5.311953 * x/(1+2.084315 * x), from = 0, to = 31, col = "blue",add = T, lty = "dashed")
  