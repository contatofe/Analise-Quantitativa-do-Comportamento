#Importações

data = read_xlsx('data_ml.xlsx')
View(data)

#Construindo Taxa de Reforços
data$TaxaR1 = data$R1/(data$R1 + data$R2)

#Construindo Taxa de Respostas
data$TaxaB1 = data$B1/(data$B1 + data$B2)

#Plotando gráfico das % da Alternativa 1

plot(x = data$TaxaR1, y = data$TaxaB1,
     xlab = "% Reforços na alternativa 1", ylab = "% Respostas na alternativa 1",
     xlim = c(0, 1), ylim = c(0,1),
     main = "Gráfico de Reforço x Respostas")

#Plotando linha correspondente à Lei da igualação estrita

lines(c(0,1),c(0,1), col="gray")

#Ajustando a um modelo linear com a taxa relativa

data$Taxa_r_R1 = data$R1/data$R2
data$Taxa_r_B1 = data$B1/data$B2

model = lm(data$Taxa_r_B1 ~ data$Taxa_r_R1, data = data)
summary(model)

plot(x = data$Taxa_r_R1, y = data$Taxa_r_B1,
     xlab = "Razão de reforços (R1/R2)", ylab = "Razão de respostas (B1/B2)",
     abline(model, col="gray"),
     main = "Gráfico de Reforço x Respostas")

# Ajustando Banda de confiança

band = predict(model, interval = "confidence")
lwr = band[, "lwr"]
upr = band[, "upr"]
x = data$Taxa_r_R1
ord = order(x)
x = x[ord]
lwr = lwr[ord]
upr = upr[ord]
polygon(c(x, rev(x)), c(lwr,rev(upr)),
        col =  rgb(0.5,0.5,0.5,0.1),
        border = NA)

#Avaliando independência dos erros
  
  plot(residuals(model),
       xlab = "Ordem das observações",
       ylab = "Resíduos",
       main = "Inspeção de independência dos erros")
  
  abline(h = 0, col = "gray")
  
#Avaliando Homocedasticidade e normalidade

plot(model)
  
# Hipérbole de Hernnstein

#Hipérbole 1
k = 100
Re = 5
R1 = seq(0, 200, by=0.5)
B1 = k*R1/(R1+Re)
plot(R1,B1, type ="l",
     xlab = expression('R'[1]),
     ylab = expression('B'[1]), axes= FALSE,
     ylim = c(0,100),
     main = "Comparação de parâmetros na Hipérbole de Herrnstein")
axis(1)
axis(2)

#Hipérbole 2
k = 70
B2 = k*R1/(R1+Re)
lines(R1,B2, col = "red")

#Hipérbole 3
k = 100
Re = 1
B3 = k*R1/(R1+Re)
lines(R1,B3, col = "blue")

#Hipérbole 4
Re = 10
B4 = k*R1/(R1+Re)
lines(R1,B4, col = "darkgreen")

#Legenda

legend("bottomright",
       legend=c("k = 100 | Re = 5","k = 70 | Re = 5","k = 100 | Re = 1","k = 100 | Re = 10"),
       col=c("black","red","blue","darkgreen"),
       lwd=2)