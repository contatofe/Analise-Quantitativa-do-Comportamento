#Distribuições

set.seed(123)
# distribuição uniforme
unif = runif(n = 1500, min = 0.5, 120)
hist(unif, main = "Distribuição uniforme")
# distribuição exponencial
exp = rexp(n = 1500, rate = 1/60)
hist(exp, main = "Distribuição exponencial")
# distribuição gamma
gam = rgamma(n = 1500, 60)
hist(gam, main = "Distribuição gamma")


# Dados de Hernnstein

plot(NA, xlim = c(0,100), ylim = c(0, 100), axes =
       FALSE,
     xlab = "% REINFORCEMENTS ON KEY A",
     ylab = "% RESPONSES ON KEY A")

axis(1, at = seq(0, 100, by = 10), pos=0, tck=0.02)
axis(2, at = seq(0, 100, by = 10), pos=0, las = 2,tck=0.015)
axis(3, at = seq(0, 100, by = 10), pos=100, labels = FALSE, tck=0.015)
axis(4, at = seq(0, 100, by = 10), pos=100, labels = FALSE, tck=0.015)
rug(seq(0,100, by=5), side = 1, pos = 0, tck=0.008)
rug(seq(0,100, by=5), side = 2, pos = 0, tck=0.008)
rug(seq(0,100, by=5), side = 3, pos = 100, tck=0.008)
rug(seq(0,100, by=5), side = 4, pos = 100, tck=0.008)
segments(0, 0, 100, 100)
pch <- c(21, 19, 4)
for (i in 1:3) {
  pigeon <- unique(data$Pigeon)[i]
  individual.data <- data[which(data$Pigeon == pigeon),]
  points(individual.data$SRKeyA, individual.data$RKeyA,pch=pch[i])
  points(individual.data$SRKeyA, individual.data$RKeyA,type="l", lty=i)
}
legend(5, 95, border = FALSE, pch = c(19, 21, 4),
       legend = c("055", "231", "641"), lty=1:3)


#Quadrados dos resíduos

x <- data$SRKeyA
y <- data$RKeyA
yhat <- b0 + b1 * x
res <- y - yhat
segments(x0 = x, y0 = yhat,
         x1 = x, y1 = y,
         col = alpha("firebrick", 0.8),
         lwd = 1.5)
for(i in seq_along(x)) {
  side <- abs(res[i])
  if(side > 0) {
    if(res[i] > 0) {
      # ponto acima da reta -> quadrado sobe a partir da reta
      rect(xleft = x[i],
           ybottom = yhat[i],
           xright = x[i] + side,
           ytop = yhat[i] + side,
           border = alpha("dodgerblue4", 0.9),
           col = alpha("dodgerblue3", 0.15),
           lwd = 1.5)
    } else {
      # ponto abaixo da reta -> quadrado desce a partir da reta
      rect(xleft = x[i],
           ybottom = yhat[i] - side,
           xright = x[i] + side,
           ytop = yhat[i],
           border = alpha("dodgerblue4", 0.9),
           col = alpha("dodgerblue3", 0.15),
           lwd = 1.5)
    }
  }
}

#Regressão Linear

model = lm(data$RKeyA ~ data$SRKeyA, data = data)

plot(NA, xlim = c(0,100), ylim = c(0,100), axes = FALSE,
     xlab = "% REINFORCEMENTS ON KEY A",
     ylab = "% RESPONSES ON KEY A")
axis(1, at = seq(0, 100, by = 10))
axis(2, at = seq(0, 100, by = 10), las = 2)
points(data$SRKeyA, data$RKeyA, pch=21,
       bg=alpha("darkslategray",0.25),
       col=alpha("darkslategray",0.75), cex=1.5)
coef <- summary(model)
a <- coef$coefficients[2]
b <- coef$coefficients[1]
xlim_curve <- range(data$SRKeyA)
curve(a*x+b, add=TRUE,
      col="darkslategray", lwd=2, from =
        xlim_curve[1], to = xlim_curve[2])

#Banda de confiança

band <- predict(fit, interval =
                  "confidence")
lwr <- band[, "lwr"]
upr <- band[, "upr"]
x <- data$SRKeyA
ord <- order(x)
x <- x[ord]
lwr <- lwr[ord]
upr <- upr[ord]
polygon(c(x, rev(x)), c(lwr,
                        rev(upr)),
        col =
          alpha("darkslategray", 0.15),
        border = NA)


#Hipérbole de Herrnstein

k <- 100
Re <- 5
R1 <- seq(0, 200, by=0.5)
B1 <- k*R1/(R1+Re)
plot(R1,B1, type ="l",
     xlab = expression('R'[1]),
     ylab = expression('B'[1]), axes
     = FALSE,
     ylim = c(0,100))
text(100,50,"HERRNSTEIN'S
HYPERBOLA")
text(100,40,expression(B==k*R[1]/(R[
  1]+R[e])))
axis(1)
axis(2)