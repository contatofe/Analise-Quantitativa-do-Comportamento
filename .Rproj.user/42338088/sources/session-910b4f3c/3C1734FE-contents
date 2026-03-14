#Exercício 4 - Lista 02

#Importando datasets das 5 sessões

S1 = read.csv("data_rats_S1.csv", sep = ";")
S2 = read.csv("data_rats_S2.csv", sep = ";")
S3 = read.csv("data_rats_S3.csv", sep = ";")
S4 = read.csv("data_rats_S4.csv", sep = ";")
S5 = read.csv("data_rats_S5.csv", sep = ";")

View(S1)

#Removendo "início da sessão" (111) e "final da sessão dos datasets" (999)

S1 = subset(S1, !(event %in% c("111","999")))
S2 = subset(S2, !(event %in% c("111","999")))
S3 = subset(S3, !(event %in% c("111","999")))
S4 = subset(S4, !(event %in% c("111","999")))
S5 = subset(S5, !(event %in% c("111","999")))

#Substituindo valores 200 por 0 e valores 400 por 1 e transformando centisegundos em segundos

datasets = list(S1, S2, S3, S4, S5)

datasets = lapply(datasets, function(df) {
  #Transforma 200 em 0
  df$event[df$event == 200] = 0
  #Transforma 400 em 1
  df$event[df$event == 400] = 1
  #Divide por centisegundos por 100 (para segundos)
  df$time_stamp <- df$time_stamp / 100
  df
})

list2env(setNames(datasets, c("S1","S2","S3","S4","S5")), envir = .GlobalEnv)




#Plotando gráfico de registro cumulativo

#Plotando sessão 1
plot(S1$time_stamp,
     cumsum(S1$event),
       type = "l",
       xlab = "Tempo (segundos)",
       ylab = "Respostas acumuladas",
       main = "Registro Cumulativo - Sessão 01")

#Plotando pontos de Reforço
points(S1$time_stamp[S1$event == 1],
       cumsum(S1$event)[S1$event == 1],
       pch = 16,
       col = rgb(0,0,0,0.4),
       cex = 0.6)

#Plotando legenda no canto esquerdo superior
legend("topleft",
       legend = "Reforço",
       col = rgb(0,0,0,0.4),
       pch = 16)

#Plotando sessão 2
plot(S2$time_stamp,
     cumsum(S2$event),
     type = "l",
     xlab = "Tempo (segundos)",
     ylab = "Respostas acumuladas",
     main = "Registro Cumulativo - Sessão 02")

points(S2$time_stamp[S2$event == 1],
       cumsum(S2$event)[S2$event == 1],
       pch = 16,
       col = rgb(0,0,0,0.4),
       cex = 0.6)

legend("topleft",
       legend = "Reforço",
       col = rgb(0,0,0,0.4),
       pch = 16)

#Plotando sessão 3
plot(S3$time_stamp,
     cumsum(S3$event),
     type = "l",
     xlab = "Tempo (segundos)",
     ylab = "Respostas acumuladas",
     main = "Registro Cumulativo - Sessão 03")

points(S3$time_stamp[S3$event == 1],
       cumsum(S3$event)[S3$event == 1],
       pch = 16,
       col = rgb(0,0,0,0.4),
       cex = 0.6)

legend("topleft",
       legend = "Reforço",
       col = rgb(0,0,0,0.4),
       pch = 16)

#Plotando sessão 4
plot(S4$time_stamp,
     cumsum(S4$event),
     type = "l",
     xlab = "Tempo (segundos)",
     ylab = "Respostas acumuladas",
     main = "Registro Cumulativo - Sessão 04")

points(S4$time_stamp[S4$event == 1],
       cumsum(S4$event)[S4$event == 1],
       pch = 16,
       col = rgb(0,0,0,0.4),
       cex = 0.6)

legend("topleft",
       legend = "Reforço",
       col = rgb(0,0,0,0.4),
       pch = 16)

#Plotando sessão 5
plot(S5$time_stamp,
     cumsum(S5$event),
     type = "l",
     xlab = "Tempo (segundos)",
     ylab = "Respostas acumuladas",
     main = "Registro Cumulativo - Sessão 05")

points(S5$time_stamp[S5$event == 1],
       cumsum(S5$event)[S5$event == 1],
       pch = 16,
       col = rgb(0,0,0,0.4),
       cex = 0.6)

legend("topleft",
       legend = "Reforço",
       col = rgb(0,0,0,0.4),
       pch = 16)

#Plotando sessões sobrepostas
plot(S1$time_stamp,
     cumsum(S1$event),
     type = "l",
     col = rgb(1,0,0,0.4),
     xlab = "Tempo (segundos)",
     ylab = "Respostas acumuladas",
     main = "Registro Cumulativo")

lines(S2$time_stamp, cumsum(S2$event), col = rgb(0,0,1,0.5))
lines(S3$time_stamp, cumsum(S3$event), col = rgb(0,0.6,0,0.5))
lines(S4$time_stamp, cumsum(S4$event), col = rgb(1,0.5,0,0.5))
lines(S5$time_stamp, cumsum(S5$event), col = rgb(0.6,0,0.6,0.5))

legend("topleft",
       legend = c("Sessão 1","Sessão 2","Sessão 3","Sessão 4","Sessão 5"),
       col = c(rgb(1,0,0),
               rgb(0,0,1),
               rgb(0,0.6,0),
               rgb(1,0.5,0),
               rgb(0.6,0,0.6)),
       lty = 1)




#Taxa de repostas por sessão

#A taxa de respostas é medida em Frequência/Tempo.

S1_taxa = sum(S1$event == 0)
S2_taxa = sum(S2$event == 0)
S3_taxa = sum(S3$event == 0)
S4_taxa = sum(S4$event == 0)
S5_taxa = sum(S5$event == 0)

S1_taxa
S2_taxa
S3_taxa
S4_taxa
S5_taxa

#Caso quisermos a resposta por minuto, ao invés de termos ela por sessão, podemos fazer o seguinte:

S1_taxa_min = S1_taxa/(max(S1$time_stamp)/60)
S2_taxa_min = S2_taxa/(max(S2$time_stamp)/60)
S3_taxa_min = S3_taxa/(max(S3$time_stamp)/60)
S4_taxa_min = S4_taxa/(max(S4$time_stamp)/60)
S5_taxa_min = S5_taxa/(max(S5$time_stamp)/60)

S1_taxa_min
S2_taxa_min
S3_taxa_min
S4_taxa_min
S5_taxa_min



#Histograma de IRTs

#Criando coluna com a diferença dos time_stamps
S1$IRT = c(0, diff(S1$time_stamp))
S2$IRT = c(0, diff(S2$time_stamp))
S3$IRT = c(0, diff(S3$time_stamp))
S4$IRT = c(0, diff(S4$time_stamp))
S5$IRT = c(0, diff(S5$time_stamp))

#Plotando sessão 1

plot(hist(S1$IRT, breaks = 20),
     ylim = c(0,700),
     xlim = c(0,50),
     main = "Histograma de IRTs - Sessão 01",
     ylab = "Frequência", xlab = "IRT(s)")

#Plotando sessão 2

plot(hist(S2$IRT,  breaks = 20),
     ylim = c(0,700),
     xlim = c(0,50),
     main = "Histograma de IRTs - Sessão 02",
     ylab = "Frequência", xlab = "IRT(s)")

#Plotando sessão 3

plot(hist(S3$IRT,  breaks = 20),
     ylim = c(0,700),
     xlim = c(0,50),
     main = "Histograma de IRTs - Sessão 03",
     ylab = "Frequência", xlab = "IRT(s)")

#Plotando sessão 4

plot(hist(S4$IRT,  breaks = 20),
     ylim = c(0,700),
     xlim = c(0,50),
     main = "Histograma de IRTs - Sessão 04",
     ylab = "Frequência", xlab = "IRT(s)")

#Plotando sessão 5

plot(hist(S5$IRT,  breaks = 20),
     ylim = c(0,700),
     xlim = c(0,50),
     main = "Histograma de IRTs - Sessão 05",
     ylab = "Frequência", xlab = "IRT(s)")


#Plotando sessões sobrepostas

#Criando os dados de cada histograma
h1 = hist(S1$IRT, breaks = 20, plot = FALSE)
h2 = hist(S2$IRT, breaks = 20, plot = FALSE)
h3 = hist(S3$IRT, breaks = 20, plot = FALSE)
h4 = hist(S4$IRT, breaks = 20, plot = FALSE)
h5 = hist(S5$IRT, breaks = 20, plot = FALSE)

#Plotando o primeiro histograma
plot(h1$mids, h1$counts, type="l", col="red",
     ylim = c(0, 700),
     xlim = c(0,50),
     xlab="IRT (s)", ylab="Frequência",
     main="Comparação entre sessões")

#Plotando os demais histogramas
lines(h2$mids, h2$counts, col="blue")
lines(h3$mids, h3$counts, col="darkgreen")
lines(h4$mids, h4$counts, col="orange")
lines(h5$mids, h5$counts, col="purple")

#Criando legenda
legend("topright",
       legend=c("S1","S2","S3","S4","S5"),
       col=c("red","blue","darkgreen","orange","purple"),
       lty=1)


#Gráfico de sobrevivência dos IRTs

#Calculando e Plotando Sessão 01

#Função que responde qual valor é ≤ x
F = ecdf(S1$IRT)
#organiza os IRTs em ordem crescente
x = sort(S1$IRT)
#Função de sobrevivência
S = 1 - F(x)

#Plotando o gráfico
plot(x, S,
     type = "s",
     #log = "y",
     xlab = "IRT (s)",
     ylab = "Proporção de IRTs > x",
     main = "Sobrevivência dos IRTs - Sessão 01",
     xlim = c(0,50),
     ylim = c(.0075, 1))


#Calculando e Plotando Sessão 02

F = ecdf(S2$IRT)
x = sort(S2$IRT)
S = 1 - F(x)

plot(x, S,
     type = "s",
     xlab = "IRT (s)",
     ylab = "Proporção de IRTs > x",
     main = "Sobrevivência dos IRTs - Sessão 02",
     xlim = c(0,50),
     ylim = c(.0075, 1))


#Calculando e Plotando Sessão 03

F = ecdf(S3$IRT)
x = sort(S3$IRT)
S = 1 - F(x)

plot(x, S,
     type = "s",
     xlab = "IRT (s)",
     ylab = "Proporção de IRTs > x",
     main = "Sobrevivência dos IRTs - Sessão 03",
     xlim = c(0,50),
     ylim = c(.0075, 1))


#Calculando e Plotando Sessão 04

F = ecdf(S4$IRT)
x = sort(S4$IRT)
S = 1 - F(x)

plot(x, S,
     type = "s",
     xlab = "IRT (s)",
     ylab = "Proporção de IRTs > x",
     main = "Sobrevivência dos IRTs - Sessão 04",
     xlim = c(0,50),
     ylim = c(.0075, 1))


#Calculando e Plotando Sessão 05

F = ecdf(S5$IRT)
x = sort(S5$IRT)
S = 1 - F(x)

plot(x, S,
     type = "s",
     xlab = "IRT (s)",
     ylab = "Proporção de IRTs > x",
     main = "Sobrevivência dos IRTs - Sessão 05",
     xlim = c(0,50),
     ylim = c(.0075, 1))