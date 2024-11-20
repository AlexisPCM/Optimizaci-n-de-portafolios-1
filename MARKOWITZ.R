install.packages('quantmod')
install.packages('ggplot2')
install.packages('corrplot')
install.packages('ggcorrplot')
install.packages('quadprog')
install.packages('moments')
install.packages('fredr')
library(quantmod)
library(ggplot2)
library(corrplot)
library(ggcorrplot)
library(moments)
library(quadprog)
library(fredr)
#library(quantmod)
#
options(scipen = 999)
#Obtener tickets y precios diarios 
tickers <- c("SPY", "ASML", "TGT", "DIS", "NVDA", "MA", "HD", "COST", "PBR","AAPL")
getSymbols(tickers, from = "2014-08-08", to = '2024-08-08')


#Construcción de matrices de precios y rendimientos 
P = do.call(merge, lapply(tickers, function(x) Ad(get(x))))
R = na.omit(ROC(P, type ="continuous"))
#Cantidad de activos 
n<-ncol(R)

#Cambio de nombres en la matriz de rendimientos 
colnames(R) <- tickers 

#Matriz de correlación
corr<-cor(R) 

ggcorrplot(corr, hc.order = FALSE, type = "lower",
           lab = TRUE)


#Descripción estadistica de las acciones

stats <- data.frame(
  Annual_Return = paste(formatC((colMeans(R) * 252)*100, format = 'f' , digits=2),'%', sep=""),
  Annual_Volatility = paste(formatC(apply(R, 2, sd) * sqrt(252)*100,format = 'f',digits = 2),'%',sep = ""),
  Min = paste(formatC(apply(R, 2, min)*100,format = 'f',digits = 2),'%',sep = ""),
  Max = paste(formatC(apply(R, 2, max)*100,format = 'f',digits = 2),'%',sep = ""),
  Skewness = formatC(apply(R, 2, skewness),format = 'f',digits = 2),
  Kurtosis = formatC(apply(R, 2, kurtosis),format = 'f',digits = 2)
)

par(mfrow = c(1, 1))
#SPY
hist(R$SPY, breaks = 20, probability = T, 
     col = rgb(0.2, 0.4, 0.6, 0.5), border = "white", 
     main = "RETORNOS SPY", xlab = "Retornos", ylab = "Densidad")
curve(dnorm(x, mean = mean(R$SPY), sd = sd(R$SPY)), 
      col = "darkblue", lwd = 2, add = TRUE)

abline(v = mean(R$SPY), col = "darkblue", lwd = 2, lty = 3)
abline(v = quantile(R$SPY, 0.95), col = "darkblue", lwd = 2, lty = 4)
abline(v = quantile(R$SPY, 0.05), col = "darkblue", lwd = 2, lty = 4)

#ASML
hist(R$ASML, breaks = 20, probability = T, 
     col = rgb(0.2, 0.4, 0.6, 0.5), border = "white", 
     main = "RETORNOS ASML", xlab = "Retornos", ylab = "Densidad")
curve(dnorm(x, mean = mean(R$ASML), sd = sd(R$ASML)), 
      col = "darkblue", lwd = 2, add = TRUE)

abline(v = mean(R$ASML), col = "darkblue", lwd = 2, lty = 3)
abline(v = quantile(R$ASML, 0.95), col = "darkblue", lwd = 2, lty = 4)
abline(v = quantile(R$ASML, 0.05), col = "darkblue", lwd = 2, lty = 4)

#
#TGT
hist(R$TGT, breaks = 20, probability = T, 
     col = rgb(0.2, 0.4, 0.6, 0.5), border = "white", 
     main = "RETORNOS TGT", xlab = "Retornos", ylab = "Densidad")
curve(dnorm(x, mean = mean(R$TGT), sd = sd(R$TGT)), 
      col = "darkblue", lwd = 2, add = TRUE)

abline(v = mean(R$TGT), col = "darkblue", lwd = 2, lty = 3)
abline(v = quantile(R$TGT, 0.95), col = "darkblue", lwd = 2, lty = 4)
abline(v = quantile(R$TGT, 0.05), col = "darkblue", lwd = 2, lty = 4)

#DIS
hist(R$DIS, breaks = 20, probability = T, 
     col = rgb(0.2, 0.4, 0.6, 0.5), border = "white", 
     main = "RETORNOS DIS", xlab = "Retornos", ylab = "Densidad")
curve(dnorm(x, mean = mean(R$DIS), sd = sd(R$DIS)), 
      col = "darkblue", lwd = 2, add = TRUE)

abline(v = mean(R$DIS), col = "darkblue", lwd = 2, lty = 3)
abline(v = quantile(R$DIS, 0.95), col = "darkblue", lwd = 2, lty = 4)
abline(v = quantile(R$DIS, 0.05), col = "darkblue", lwd = 2, lty = 4)


#NVDA
hist(R$NVDA, breaks = 20, probability = T, 
     col = rgb(0.2, 0.4, 0.6, 0.5), border = "white", 
     main = "RETORNOS NVDA", xlab = "Retornos", ylab = "Densidad")
curve(dnorm(x, mean = mean(R$NVDA), sd = sd(R$NVDA)), 
      col = "darkblue", lwd = 2, add = TRUE)

abline(v = mean(R$NVDA), col = "darkblue", lwd = 2, lty = 3)
abline(v = quantile(R$NVDA, 0.95), col = "darkblue", lwd = 2, lty = 4)
abline(v = quantile(R$NVDA, 0.05), col = "darkblue", lwd = 2, lty = 4)

#MA
hist(R$MA, breaks = 20, probability = T, 
     col = rgb(0.2, 0.4, 0.6, 0.5), border = "white", 
     main = "RETORNOS MA", xlab = "Retornos", ylab = "Densidad")
curve(dnorm(x, mean = mean(R$MA), sd = sd(R$MA)), 
      col = "darkblue", lwd = 2, add = TRUE)

abline(v = mean(R$NVDA), col = "darkblue", lwd = 2, lty = 3)
abline(v = quantile(R$NVDA, 0.95), col = "darkblue", lwd = 2, lty = 4)
abline(v = quantile(R$NVDA, 0.05), col = "darkblue", lwd = 2, lty = 4)

#HD
hist(R$HD, breaks = 20, probability = T, 
     col = rgb(0.2, 0.4, 0.6, 0.5), border = "white", 
     main = "RETORNOS HD", xlab = "Retornos", ylab = "Densidad")
curve(dnorm(x, mean = mean(R$HD), sd = sd(R$HD)), 
      col = "darkblue", lwd = 2, add = TRUE)

abline(v = mean(R$HD), col = "darkblue", lwd = 2, lty = 3)
abline(v = quantile(R$HD, 0.95), col = "darkblue", lwd = 2, lty = 4)
abline(v = quantile(R$HD, 0.05), col = "darkblue", lwd = 2, lty = 4)

#COST
hist(R$COST, breaks = 20, probability = T, 
     col = rgb(0.2, 0.4, 0.6, 0.5), border = "white", 
     main = "RETORNOS COST", xlab = "Retornos", ylab = "Densidad")
curve(dnorm(x, mean = mean(R$COST), sd = sd(R$COST)), 
      col = "darkblue", lwd = 2, add = TRUE)

abline(v = mean(R$SPYG), col = "darkblue", lwd = 2, lty = 3)
abline(v = quantile(R$SPYG, 0.95), col = "darkblue", lwd = 2, lty = 4)
abline(v = quantile(R$SPYG, 0.05), col = "darkblue", lwd = 2, lty = 4)

#PBR
hist(R$PBR, breaks = 20, probability = T, 
     col = rgb(0.2, 0.4, 0.6, 0.5), border = "white", 
     main = "RETORNOS PBR", xlab = "Retornos", ylab = "Densidad")
curve(dnorm(x, mean = mean(R$PBR), sd = sd(R$PBR)), 
      col = "darkblue", lwd = 2, add = TRUE)

abline(v = mean(R$VOW), col = "darkblue", lwd = 2, lty = 3)
abline(v = quantile(R$VOW, 0.95), col = "darkblue", lwd = 2, lty = 4)
abline(v = quantile(R$VOW, 0.05), col = "darkblue", lwd = 2, lty = 4)


#AAPL
hist(R$AAPL, breaks = 20, probability = T, 
     col = rgb(0.2, 0.4, 0.6, 0.5), border = "white", 
     main = "RETORNOS AAPL", xlab = "Retornos", ylab = "Densidad")
curve(dnorm(x, mean = mean(R$AAPL), sd = sd(R$AAPL)), 
      col = "darkblue", lwd = 2, add = TRUE)

abline(v = mean(R$AAPL), col = "darkblue", lwd = 2, lty = 3)
abline(v = quantile(R$AAPL, 0.95), col = "darkblue", lwd = 2, lty = 4)
abline(v = quantile(R$AAPL, 0.05), col = "darkblue", lwd = 2, lty = 4)



#Vector de rendimiento esperado, desviación estandar y matriz de varianzas y covarianzas 
mu<- colMeans(R) * 252  
sd_activos <- apply(R, 2, sd) * sqrt(252) 
Sigma <- var(R) * (252)

# Funciones:
p_r<-function(w) sum(w*mu)                # Retorno portafolio
p_sd<-function(w) sqrt(t(w)%*%Sigma%*%w)  # Volatilidad portafolio
p_MV<-function(mu_T) {
  v1 <- rep(1, n)
  A <-as.numeric(t(v1)%*%solve(Sigma)%*%v1)
  B <-as.numeric(t(mu)%*%solve(Sigma)%*%v1)
  C <-as.numeric(t(v1)%*%solve(Sigma)%*%mu)
  D <-as.numeric(t(mu)%*%solve(Sigma)%*%mu)
  delta <- A*D - B*C
  lambda1 <- (D-mu_T*C) / delta
  lambda2 <- (mu_T*A - B) / delta
  w_MV <- lambda1*solve(Sigma)%*%v1 + lambda2*solve(Sigma)%*%mu
  # w_MV <- as.numeric(w_MV)
  return(w_MV)
}                # PMV retorno deseado
p_t<-function(r_f) {
  A = t(rep(1, n))%*%(solve(Sigma)%*%rep(1, n))
  B = t(mu)%*%(solve(Sigma)%*%rep(1, n))
  w_t = solve(Sigma)%*%(mu-r_f*rep(1, n))/(B - A*r_f)[1]
  return(w_t)
} 

#Objetivo, risk free y tasa de prestamo
r_t=0.25 # Objetivo de retorno
fredr_set_key("68e87f30c04d6445523e5b380e080e84")
r_f_US <- fredr("EFFR", observation_start = Sys.Date() -4)$value
r_f=0.015 # Libre de riesgo
r_w=0.035 # Tasa de préstamo

#PORTAFOLIOS VECTORES W
w_naive=rep(1 / n, n)
w_u=p_MV(r_t)
w_mvg<-solve(Sigma)%*%rep(1, n) / as.numeric(t(rep(1, n)) %*% solve(Sigma) %*% rep(1, n))
w_t=p_t(r_f)
w=c(0.025, 0.025, 0.05, 0.05, 0.5, 0.025, 0.05, 0.025, 0.05, 0.2)

p_r(w_mvg)
p_sd(w_mvg)

# betas
R_t <- xts(R %*% w_t, order.by = index(R)) 
# beta mercado
beta_M <- apply(R, 2, function(x) cov(x, R[,"SPY"]) / var(R[,"SPY"]))
beta_s <- apply(R, 2, function(x) cov(x, R_t) / var(R_t))

# Espacio

plot(NULL, xlim = c(0, 1), ylim = c(0, 1.5),
     xlab = "σ",
     ylab = "E(r)",
     main = "Rendimiento vs Volatilidad")


# Frontera de portafolio eficientes:
mu_targets = seq(p_r(w_mvg), 100, 0.01)
sd_targets = sapply(mu_targets, function(x) p_sd(p_MV(x)))

mu_i=seq(-1, 2, 0.01)                            
sd_i=sapply(mu_i, function(x) p_sd(p_MV(x)))     
lines(sd_i, mu_i,col = 'red', lwd= 0.5, lty=1)
lines(sd_targets, mu_targets, col = 'red', lwd= 3, lty=3)


# Sharpe 
S_t = (p_r(w_t) - r_f) / p_sd(w_t)
S_mvg = (p_r(w_mvg) - r_f) / p_sd(w_mvg)

# CML (Capital Market Line)
sd_pm = seq(0, 1, by = 0.01)
r_i_t = r_f + (S_t * sd_pm)
lines(sd_pm, r_i_t, col = '#6C7B8B', lwd= 1, lty=6 )


# CAL (Capital Allocation Line - MVG)
r_i_mvg = r_f + S_mvg * sd_pm
lines(sd_pm, r_i_mvg, col = "darkblue", lwd=0.5,lty = 2) 

# SML (Security Market Line)
beta_s = seq(r_f, 2, by = 0.1)
SML = r_f + beta_s * (p_r(w_t) - r_f)
lines(beta_s, SML, col ='purple', lty = 2)




#plot(NULL, xlim = c(0.0, 0.45), ylim = c(0, 0.90),
#     xlab = "σ",
#     ylab = "E(r)",
#     main = "Set de oportunidad de inversión y portafolios sleccionados")


# activos individuales

text(sd_activos, mu , labels = colnames(R), , cex = 0.6, srt=0, col= rgb(0,0,1, alpha=0.4))  # Nombres y valores

#portafolio con restriccones especificas
MV_restringido  <- function(ro) {
  Dmat <- Sigma
  dvec <- rep(0, n)
  # Restricciomes:
  # (1) sum(w)=1
  # (2) sum(w*mu)=mu_T
  # (3) No short-selling: w_i >= 0
  # (4) 
  z<-(diag(n)*(-1))
  Amat<-t(rbind(rep(1, n), mu, diag(n),z))
  bvec<-c(1, r_t  , rep(0, n),rep(-0.17, n))
  meq=2
  RES<- solve.QP(Dmat, dvec, Amat, bvec, meq)$solution
  return(RES)
}


MVres<-MV_restringido(r_t)                              

points(p_sd(w_naive), p_r(w_naive), pch = 20,col ='Darkblue')
text(p_sd(w_naive), p_r(w_naive), labels = "Naive", pos = 4, cex = 0.7,)

points(p_sd(w_u), p_r(w_u), pch = 20,col ='Darkblue')
text(p_sd(w_u), p_r(w_u), labels = 'Objetivo' , pos =4, cex = 0.7, srt= 45)

points(p_sd(w_mvg), p_r(w_mvg), pch = 20,col ='Darkblue')
text(p_sd(w_mvg), p_r(w_mvg), labels = "W_MVG", pos = 4, cex = 0.7)

points(p_sd(w_t), p_r(w_t), pch = 20,col ='Darkblue')
text(p_sd(w_t), p_r(w_t), labels = "W_T", pos = 4, cex = 0.7)

points(p_sd(w), p_r(w), pch = 20,col ='Darkblue')
text(p_sd(w), p_r(w), labels = "W", pos = 4, cex = 0.7)

points(p_sd(MVres), p_r(MVres), pch = 20,col ='Darkblue')
text(p_sd(MVres), p_r(MVres), labels = "W restringido", pos = 4, cex = 0.7)

legend(0,1.5, legend = c("Frontera eficiente", "Capital Market Line","Capital allocation line -MVG",'Security Market Line'), bty = "n",
       lwd = 2, cex = 0.5, col = c("red", '#6C7B8B', "darkblue",'purple'), lty = c(3, 6, 2,2))

port<-c('NAIVE', 'Objetivo', 'MVG','Tangencia','W','MV_rest')
rendp<-c(p_r(w_naive),p_r(w_u),p_r(w_mvg),p_r(w_t),p_r(w),p_r(MVres))
sdp<-c(p_sd(w_naive),p_sd(w_u),p_sd(w_mvg),p_sd(w_t),p_sd(w),p_sd(MVres))

# Métricas:
pesos_portafolios<-data.frame(
  asset = colnames(R),
  mu = paste(formatC(mu*100, format = 'f', digits = 2),"%", sep = ""),
  sd = paste(formatC(sd_activos*100, format = 'f', digits = 2),"%", sep = ""),
  w_naive = paste(formatC(w_naive*100, format = 'f', digits = 2),"%", sep = ""),         
  w_u = paste(formatC(w_u*100, format = 'f', digits = 2),"%", sep = ""),                 
  w_mvg = paste(formatC(w_mvg*100, format = 'f', digits = 2),"%", sep = ""), 
  w_t = paste(formatC(w_t*100, format = 'f', digits = 2),"%", sep = ""),    
  w = paste(formatC(w*100, format = 'f', digits = 2),"%", sep = ""),
  W_rest=paste(formatC(MVres*100, format = 'f', digits = 2),"%", sep = "")
)


rsd_portafolios<-data.frame(
  Portafolio=port,
  Rendimiento= paste(formatC(rendp*100,format = 'f',digits = 2),'%',sep = ""),
  Volatilidad= paste(formatC(sdp*100,format = 'f',digits = 2),'%',sep = "")  
)

w_final=MV_restringido(0.30)

#Metricas

Sharpe_wfin<-(p_r(w_final) - r_f) / p_sd(w_final)
Sharpe_w_naive<-(p_r(w_naive) - r_f) / p_sd(w_naive)
Sharpe_w_u<-(p_r(w_u) - r_f) / p_sd(w_u)
Sharpe_w_mvg<-(p_r(w_mvg) - r_f) / p_sd(w_mvg)
Sharpe_w_t<-(p_r(w_t) - r_f) / p_sd(w_t)
Sharpe_w<-(p_r(w) - r_f) / p_sd(w)

Div_rat_wfin=sd_activos["SPY"] / p_sd(w_final)
Div_rat_w_naive=sd_activos["SPY"] / p_sd(w_naive)
Div_rat_w_u=sd_activos["SPY"] / p_sd(w_u)
Div_rat_w_mvg=sd_activos["SPY"] / p_sd(w_mvg)
Div_rat_w_t=sd_activos["SPY"] / p_sd(w_t)
Div_rat_w=sd_activos["SPY"] / p_sd(w)

beta=cov(R_t, R[,"SPY"]) / var(R[,"SPY"])
Treynor_ratio <- (p_r(w_t) - r_f) / beta


#Alpha de jensen
market_returns <- R[,"SPY"]
alpha <- p_r(w_final) - (r_f + beta * (mean(market_returns) * 252 - r_f))


#Traking error
Tracking_error <- sqrt(mean((R_t - market_returns)^2)) * sqrt(252)

var_95 <- quantile(R_t, probs = 0.05)

rend_w_final<- R%*%w_final

market_returns <- R[,"SPY"]

ts_rend_w_final<-ts(rend_w_final,
                    start = '2014-08-11',
                    end = '')
portafolios <- list(
  Naive = w_naive,
  MVG = w_mvg,
  MV_mu = w_u,
  Tang = w,
  Final = w_final
)

portafolio_metrics <- do.call(rbind, lapply(names(portafolios), function(name) {
  w <- portafolios[[name]]
  
  c(
    Portafolio = name,
    Retorno = round(p_r(w),2),
    Volatilidad = round(p_sd(w),2),
    VaR_95 = round(quantile(R %*% w, probs = 0.05),2),
    Sharpe =round( (p_r(w) - r_f) / p_sd(w),2),
    Div_Ratio = round(sd(R[,"NVDA"]) / p_sd(w),2),
    setNames(as.list(round(w,2)), tickers)  
  )
}))
t(portafolio_metrics)


# Versus índice
retorno_portafolio <- R %*% w_final  # Retornos diarios del portafolio
precio_portafolio <- cumprod(1 + retorno_portafolio)
precio_portafolio <- precio_portafolio * as.numeric(first(P$SPY.Adjusted) / first(precio_portafolio))
precio_portafolio <- xts(precio_portafolio, order.by = index(R))
fechas_comunes <- index(P) %in% index(precio_portafolio)
P <- P[fechas_comunes]
precio_portafolio <- precio_portafolio[index(P)]
P$Final <- precio_portafolio

plot(P$SPY.Adjusted, type = "l", col = "blue", lwd = 2, xlab = "Fecha", ylab = "Precio acumulado",
     main = "SPY vs w final")
lines(P$Final, col = "red", lwd = 2)

# Simulación

n_days <- 252*10     
n_sim <- 100       
mu_port <- p_r(w_final) / 252  
sd_port <-p_sd(w_final) / sqrt(252)  

# Simulación de Monte Carlo
set.seed(123)  
simulated_returns <- matrix(rnorm(n_days * n_sim, mean = mu_port, sd = sd_port), ncol = n_sim)

simulated_prices <- apply(simulated_returns, 2, function(x) cumprod(1 + x))

matplot(simulated_prices, type = "l", col = rgb(0.8, 0.8, 0.8, 0.5), lty = 1,
        xlab = "Días", ylab = "Retorno acumulado", main = "Simulación Monte Carlo del Portafolio")
lines(rowMeans(simulated_prices), col = "red", lwd = 2)


