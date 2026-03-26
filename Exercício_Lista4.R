#Importando dados

library(readxl)

sheets = excel_sheets("data_jeab612.xlsx")

for (i in seq_along(sheets)) {
  name = sheets[i]
  new_name = paste0("df", name)
  
  assign(
    new_name,
    read_excel("data_jeab612.xlsx", sheet = name)
  )
}

# Excluindo nota de cada tabela

for (name in sheets) {
  
  nome_df = paste0("df", name)
  df = get(nome_df)
  
  # remove última linha
  
  df = df[-nrow(df), ]
  assign(nome_df, df)
}

df001

#1 gráfico por imagem

par(mfrow = c(1, 1))

# Gráfico de proporção de df001

df001$prop_B1 = df001$B1 / (df001$B1 + df001$B2)
df001$prop_R1 = df001$R1 / (df001$R1 + df001$R2)

plot(x = 100*df001$prop_R1, y = 100*df001$prop_B1,
     xlim = c(0,100), ylim = c(0,100),
     main = "Proporção entre B1 e R1",
     xlab = "% Reforços da alternativa 1", ylab = "% Respostas da alternativa 1",
     abline(0,1, col="grey")
     )

# Gráfico de razão de df001

df001$raz_B1 = df001$B1 / df001$B2
df001$raz_R1 = df001$R1 / df001$R2

plot(x = df001$raz_R1, y = df001$raz_B1,
     xlim = c(0,2.5), ylim = c(0,2.5),
     main = "Razão entre B1 e R1",
     xlab = "Razão de Reforços da alternativa 1", ylab = "Razão de Respostas da alternativa 1",
     abline(0,1, col="grey")
)

# Gráfico log de df001

df001$log_B1 = log10(df001$raz_B1)
df001$log_R1 = log10(df001$raz_R1)

plot(x = df001$log_R1, y = df001$log_B1,
     xlim = c(-2,1), ylim = c(-2,1),
     ylab=expression(log[10](B[1]/B[2])),
     xlab=expression(log[10](R[1]/R[2])),
     main = "Log da Razão entre B1 e R1",
     abline(0,1, col="grey")
     )


# Regressão Linear de log da Razão

par(mfrow = c(1, 3))

plot(x = df001$log_R1, y = df001$log_B1,
     xlim = c(-2,1), ylim = c(-2,1),
     ylab=expression(log[10](B[1]/B[2])),
     xlab=expression(log[10](R[1]/R[2])),
     main = "Log da Razão entre B1 e R1",
     abline(0,1, col="grey")
)

y.rate = log10(df001$raz_B1)
x.rate = log10(df001$raz_R1)  

valid = is.finite(x.rate) & is.finite(y.rate)

x.clean = x.rate[valid]
y.clean = y.rate[valid]

fit = lm(y.clean ~ x.clean, na.action = na.omit)
print(summary(fit))
points(x.clean, predict(fit), type="l")

band = predict(fit, interval = "confidence")
lwr = band[, "lwr"]
upr = band[, "upr"]
x = x.clean
ord = order(x)
x = x[ord]
lwr = lwr[ord]
upr = upr[ord]
polygon(c(x, rev(x)), c(lwr, rev(upr)),
        col = scales::alpha("darkslategray", 0.15), border = NA)

#Outros modelos lineares para prop e raz


y.rate_prop = 100*df001$prop_B1
x.rate_prop = 100*df001$prop_R1

fit_prop = lm(y.rate_prop ~ x.rate_prop)

y.rate_raz = (df001$raz_B1)
x.rate_raz = (df001$raz_R1)  

valid = is.finite(df001$raz_R1) & is.finite(df001$raz_B1)

x.clean_raz = x.rate_raz[valid]
y.clean_raz = y.rate_raz[valid]

fit_raz = lm(y.clean_raz ~ x.clean_raz, na.action = na.omit)

print(summary(fit))
print(summary(fit_prop))
print(summary(fit_raz))


#Plot proporção vs modelo linear

plot(x = (100*df001$prop_R1), y = (100*df001$prop_B1),
     xlim = c(0,100), ylim = c(0,100),
     main = "Proporção entre B1 e R1",
     xlab = "% Reforços da alternativa 1", ylab = "% Respostas da alternativa 1",
     abline(0,1, col="grey")
)

points(x.rate_prop, predict(fit_prop), type="l")
ord = order(x.rate_prop)
x_ord = x.rate_prop[ord]
y_pred = predict(fit_prop)[ord]
band = predict(fit_prop, interval = "confidence")
lwr = band[ord, "lwr"]
upr = band[ord, "upr"]

points(x_ord, y_pred, type="l")
polygon(c(x_ord, rev(x_ord)), c(lwr, rev(upr)),
        col = scales::alpha("darkslategray", 0.15), border = NA)


#Plot razão vs modelo linear

plot(x = df001$raz_R1, y = df001$raz_B1,
     xlim = c(-2,1.2), ylim = c(-2,1.2),
     main = "Razão entre B1 e R1",
     xlab = "Razão de Reforços da alternativa 1", ylab = "Razão de Respostas da alternativa 1",
     abline(0,1, col="grey")
)


points(x.clean_raz, predict(fit_raz), type="l")
band = predict(fit_raz, interval = "confidence")
lwr = band[, "lwr"]
upr = band[, "upr"]
x = x.clean_raz
ord = order(x)
x = x[ord]
lwr = lwr[ord]
upr = upr[ord]
polygon(c(x, rev(x)), c(lwr, rev(upr)),
        col = scales::alpha("darkslategray", 0.15), border = NA)


#Plot log razão vs modelo linear

plot(x = df001$log_R1, y = df001$log_B1,
     xlim = c(-2,1.2), ylim = c(-2,1.2),
     ylab=expression(log[10](B[1]/B[2])),
     xlab=expression(log[10](R[1]/R[2])),
     main = "Log da Razão entre B1 e R1",
     abline(0,1, col="grey")
)

points(x.clean, predict(fit), type="l")
band = predict(fit, interval = "confidence")
lwr = band[, "lwr"]
upr = band[, "upr"]
x = x.clean
ord = order(x)
x = x[ord]
lwr = lwr[ord]
upr = upr[ord]
polygon(c(x, rev(x)), c(lwr, rev(upr)),
        col = scales::alpha("darkslategray", 0.15), border = NA)


# 4 gráficos por imagem

par(mfrow = c(2, 2))

# Gráfico de proporção de df001, df002, df003, df004

dados = list(df001, df002, df003, df004)

for (i in seq_along(dados)) {
  dados[[i]]$prop_B1 = dados[[i]]$B1 / (dados[[i]]$B1 + dados[[i]]$B2)
  dados[[i]]$prop_R1 = dados[[i]]$R1 / (dados[[i]]$R1 + dados[[i]]$R2)

  plot(x = dados[[i]]$prop_R1, y = dados[[i]]$prop_B1,
       xlim = c(0,1), ylim = c(0,1),
       main = paste0("Proporção entre B1 e R1 - Participante ", i), cex.main = 0.8,
       xlab = "% Reforços da alternativa 1", ylab = "% Respostas da alternativa 1",
       cex.lab = 0.7,
       abline(0,1, col="grey")
  )
  
}

# Gráfico de razão de df001, df002, df003, df004

for (i in seq_along(dados)) {
  dados[[i]]$raz_B1 = dados[[i]]$B1 / dados[[i]]$B2
  dados[[i]]$raz_R1 = dados[[i]]$R1 / dados[[i]]$R2
  
  plot(x = dados[[i]]$raz_R1, y = dados[[i]]$raz_B1,
       xlim = c(0,3), ylim = c(0,3),
       main = paste0("Razão entre B1 e R1 - Participante ", i), cex.main = 0.8,
       xlab = "Razão de Reforços da alternativa 1", ylab = "Razão de Respostas da alternativa 1",
       cex.lab = 0.7,
       abline(0,1, col="grey")
  )
}

# Gráfico log de df001, df002, df003, df004

for (i in seq_along(dados)) {
  dados[[i]]$log_B1 = log10((dados[[i]]$B1) / (dados[[i]]$B2))
  dados[[i]]$log_R1 = log10((dados[[i]]$R1) / (dados[[i]]$R2))
  
  plot(x = dados[[i]]$log_R1, y = dados[[i]]$log_B1,
       xlim = c(-2,2), ylim = c(-2,2),
       main = paste0("Log B1/R1 - Participante ", i), cex.main = 0.8,
       ylab=expression(log[10](B[1]/B[1] + B[2])),
       xlab=expression(log[10](R[1]/R[1] + R[2])),
       cex.lab = 0.7,
       abline(0,1, col="grey")
  )
}

# Regressão Linear de log da Razão de df001, df002, df003, df004

for (i in seq_along(dados)) {
  dados[[i]]$log_B1 = log10((dados[[i]]$B1) / (dados[[i]]$B2))
  dados[[i]]$log_R1 = log10((dados[[i]]$R1) / (dados[[i]]$R2))
  
  plot(x = dados[[i]]$log_R1, y = dados[[i]]$log_B1,
       xlim = c(-2,2), ylim = c(-2,2),
       main = paste0("Log B1/R1 - Participante ", i), cex.main = 0.8,
       ylab=expression(log[10](B[1]/B[1] + B[2])),
       xlab=expression(log[10](R[1]/R[1] + R[2])),
       cex.lab = 0.7,
       abline(0,1, col="grey")
  )
  
  y.rate = dados[[i]]$log_B1
  x.rate = dados[[i]]$log_R1  
  
  valid = is.finite(x.rate) & is.finite(y.rate)
  
  x.clean = x.rate[valid]
  y.clean = y.rate[valid]
  
  fit = lm(y.clean ~ x.clean, na.action = na.omit)
  print(summary(fit))
  points(x.clean, predict(fit), type="l")
  
}

