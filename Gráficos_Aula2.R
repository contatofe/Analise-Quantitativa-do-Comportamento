#Plot de Latência

latency = c(160, 30, 60, 15, 28,
            20, 30, 22, 11, 15,
            20, 12, 10, 14, 10,
            8, 8, 5, 10, 8, 6, 6, 7)
plot(latency, xlab = "Tentativas", ylab ="Latência",main = "", type = "l", lwd =1.25, axes = FALSE, cex.lab = 1.25)
points(latency, pch = 1)
axis(1, at = seq(0, 23, by = 1), pos = 0)
axis(2, at = seq(0, 160, by = 20), pos = 0)


# Plot de Registro Cumulativo

responses = sample(c(0,1), 200, replace = TRUE, prob =c(0.6, 0.4))

plot(cumsum(responses), type = "l", xlab = "Tempo (segundos)", ylab = "Respostas acumuladas", main = "Registro Cumulativo")

for (i in 1:length(responses)) {
      
   r = responses[i]
   outcome = 0
   
   if (r > 0) {
      outcome = sample(c(0,1), 1, prob = c(0.75, 0.15))
      }
   
   if (outcome > 0) {segments(i, tail(cumsum(responses[1:i]), n = 1), i +1.5, tail(cumsum(responses[1:i]), n = 1) - 1.5)
     }
}

# Plot de Probabilidade

set.seed(123)
size = 30
coin = sample(c(0, 1), size = size, replace =TRUE,
              prob = c(0.5, 0.5))
updated_probability =cumsum(coin)/1:length(coin)
plot(updated_probability, type = "l",
     ylab = "Probabilidade", xlab ="Tentativa",
     axes = FALSE
)
abline(h = 0.5, col = "salmon", lty = 2)
axis(1); axis(2)
text(size/2, 0.8,
     paste0("P(E) = ",
            round(tail(updated_probability, 1), 2)),
     cex = 2)

#Plot de Taxa por Sessão

set.seed(123)
rate = runif(15, min = 12, max = 20)
plot(rate, ylab = "Taxa", xlab = "Sessão",
     axes = FALSE, ylim = c(0, 20), pch = 19)
points(rate, type = "l")
axis(1, at = 0:15)
axis(2)


# Plot de IRTs

set.seed(123)
long_irts = rexp(40, rate = 0.01)
short_irts = rexp(120, rate =0.65)
irts = sample(c(short_irts,
                long_irts))
plot(hist(irts, breaks = seq(0,450, by = 3.5)), ylim = c(0, 10),
     main = "Histograma de IRTs",
     ylab = "Frequência", xlab = "IRT(s)")

# Sobrevivência direto

F = ecdf(irts)
S = function(x) 1 - F(x)
plot(S,
     from = min(irts), to =max(irts),
     xlab = "IRT (s)",
     ylab = "Proporção de IRTs > x")

# Sobrevivência com Log e linha de referência

F = ecdf(irts)
x = sort(irts)
S = 1 - F(x)
plot(x, S, type = "s", log ="y",
     xlab = "IRT (s)",
     ylab = "Proporção de IRTs
> x",
     xlim = c(0,290),
     ylim = c(.0075, 1))

fit = nls(
  S ~ (1 - p) * exp(-w * x) + p *exp(-b * x),
  start = list(p = 0.3, w = 1, b =0.05),
  algorithm = "port",
  lower = c(0, 0, 0),
  upper = c(1, Inf, Inf)
)
curve(
  predict(fit, newdata =data.frame(x = x)),
  add = TRUE, lwd = 3, col =
    "darkred"
)