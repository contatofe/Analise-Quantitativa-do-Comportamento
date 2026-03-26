library(readxl)
#----------------#
# Staddon (1968) #
#----------------#

#Plot 1
data = readxl::read_excel("Staddon_1968.xlsx")
data$x_prop = data$n3/(data$n3+data$n11)
data$y_prop = data$r3/(data$r3+data$r11)
plot(NA, xlim = c(0, 1), ylim = c(0, 1), axes =
       FALSE,
     xlab = "",
     ylab = "")
title(xlab = "Prop. SR+ in A", line = 2.5,
      cex.lab = 2)
title(ylab = "Prop. Rs in A", line = 2, cex.lab
      = 2)
title(main = "Staddon (1968)", cex.main = 2)
points(data$x_prop, data$y_prop,
       pch = 21, col = "white", bg = "black",
       cex = 1.5)
axis(1, at = 0:1, cex.axis = 1.2)
axis(2, at = 0:1, cex.axis = 1.2)
abline(a = 0, b = 1, lty = 2)

#Plot 2

plot(NA, xlim = c(0, 30), ylim = c(0,
                                   200), axes = FALSE,
     xlab = "",
     ylab = "")
title(xlab = "SR+[A]/SR+[B]", line =
        3.5, cex.lab = 2)
title(ylab = "B[A]/B[B]", line = 2.5,
      cex.lab = 2)
title(main = "Staddon (1968)", cex.main
      = 2)
points(data$x, data$y,
       pch = 21, col = "white", bg =
         "black", cex = 1.5)
axis(1, at = seq(0, 30, by = 5),
     cex.axis = 1.2)
axis(2, at = seq(0, 200, by = 50),
     cex.axis = 1.2)
abline(a = 0, b = 1, lty = 2)

#-----------------------------
# Manipulando parâmetros
# -----------------------------
par(mfrow = c(2, 1))
b = 2.5
a = 0.3
x = seq(0, 100, by = 0.01)
y_prop = (b*(x/(1-x))^a)/(1 +
                            b*(x/(1-x))^a)
y_ratio = b*(x)^a
plot(x, y_prop, main = "Prop.", xlim
     = c(0, 1), ylim = c(0, 1))
plot(x, y_ratio, main = "Ratio")


## Análise Staddoon
par(mfrow = c(1,1))
y.rate <- data$n11/(data$n3+data$n11)
x.rate <- data$r11/(data$r3+data$r11)
# y.rate <- data$n11/(data$n3)
# x.rate <- data$r11/(data$r3)
plot(NA, xlim=c(0,1), ylim=c(0,1), axes=FALSE,
     ylab="B Proportion",
     xlab="R Proportion")
axis(1)
axis(2)
abline(coef = c(0,1), lty=2)
points(x.rate, y.rate)
## Plot Log10 Staddon
y.rate <- log10(data$n11/data$n3)
x.rate <- log10(data$r11/data$r3)
plot(NA, xlim=c(-2,4), ylim=c(-2,4), axes=FALSE,
     ylab=expression(log[10](B[1]/B[2])),
     xlab=expression(log[10](R[1]/R[2])))
axis(1)
axis(2)
abline(coef = c(0,1), lty=2)
points(x.rate, y.rate)
fit<-lm(y.rate ~ x.rate)
print(summary(fit))
points(x.rate, predict(fit), type="l", lwd=2)
## Banda de confiança
band <- predict(fit, interval = "confidence")
lwr <- band[, "lwr"]
upr <- band[, "upr"]
x <- x.rate
ord <- order(x)
x <- x[ord]
lwr <- lwr[ord]
upr <- upr[ord]
polygon(c(x, rev(x)), c(lwr, rev(upr)),
        col = scales::alpha("darkslategray", 0.15), border = NA)



