"Progetto di Metodi Probabilistici e Statistici per i Mercati Finanziari

Il modello ARCH è un modello 'Autoregressive Conditional Heteroskedasticity' che assume 
che la varianza del termine corrente sia una funzione non lineare della varianza dei rumori precedenti. 
Un aspetto rilevante dei processi ARCH è la capacità di modellare il clustering della volatilità; 
questo significa che le serie storiche finanziarie possono mostrare periodi in cui la volatilità è
elevata e seguita da periodi di volatilità relativamente bassa. 

Il modello GARCH è un modello 'Generalized Autoregressive Conditional Heteroskedasticity' ed 
è un'estensione del modello ARCH che consente di modellare la varianza del processo in base non solo 
ai valori precedenti del processo, ma anche ai valori precedenti della varianza.
"

author_content <- "Author: Melissa Petrolo"
content <- "University of Roma \"Tor Vergata\" - \u0040 Metodi Probabilistici e Statistici per i Mercati Finanziari 2022-2023"

# verifica se il pacchetto è già stato installato
if (!requireNamespace("skewtDist", quietly = TRUE)) {
  devtools::install_github("dan9401/skewtDist")
}
if (!requireNamespace("FitAR", quietly = TRUE)) {
  devtools::install_github("cran/FitAR")
}

" Librerie necessarie"
library(stats)
library(skewtDist) # libreria utilizzata per la distribuzione t-student asimmetrica
library(ggplot2)
library(gridExtra)
library(cowplot)
library(lmtest) # library(skedastic)
library(skedastic) # White test
library(rugarch)
library(tseries)
library(FitAR)
library(crayon)
library(DescTools)
library(fitdistrplus)
library(urca)
library(goftest)
library(glogis)
library(pracma)

##########################################################################################################################
#################################ff#########################################################################################
"Genero tre diverse traiettorie per tre distribuzioni differenti: 
t-student simmetrica, t-student asimmetrica, distribuzione normale"
samples <-500 # numero di campioni
n <- samples -1  # numero di campioni meno X0
df <- 5 # gradi di libertà

modello <- list('simulazione'=list(), 'stimati'=list())
########## DISTRIBUZIONE NORMALE
set.seed(10)
dist_normal1 <- rnorm(n = n, mean = 0, sd = 1)
set.seed(20)
dist_normal2 <- rnorm(n = n, mean = 0, sd = 1)
set.seed(30)
dist_normal3 <- rnorm(n = n, mean = 0, sd = 1)

type_dist = "Normal Distribution"
title_content <- bquote(atop(.(content), paste("Histogram of ", .(n) ," samples generated from the ", .(type_dist))))

subtitle_content <- bquote(paste("First sample of the ", .(type_dist)))
# creo il primo plot dei campioni generati dalla distribuzione normale
hist_dist_normal1 <- ggplot(data.frame(value = dist_normal1), aes(x = value)) + 
  geom_histogram(binwidth = 0.2, alpha = 0.7) +
  labs(subtitle=subtitle_content, caption=" ") +
  xlab("Samples") + ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

subtitle_content <- bquote(paste("Second sample of the ", .(type_dist)))
# creo il secondo plot dei campioni generati dalla distribuzione normale
hist_dist_normal2 <- ggplot(data.frame(value = dist_normal2), aes(x = value)) + 
  geom_histogram(binwidth = 0.2, alpha = 0.7) +
  labs(subtitle=subtitle_content, caption=" ") +
  xlab("Samples") + ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

subtitle_content <- bquote(paste("Third sample of the ", .(type_dist)))
# creo il terzo plot dei campioni generati dalla distribuzione normale
hist_dist_normal3 <- ggplot(data.frame(value = dist_normal3), aes(x = value)) + 
  geom_histogram(binwidth = 0.2, alpha = 0.7) +
  labs(subtitle=subtitle_content, caption=author_content) +
  xlab("Samples") + ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_norm <- plot_grid(hist_dist_normal1, hist_dist_normal2, hist_dist_normal3, ncol = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
final_plot_dist_norm <- grid.arrange(title_content_gtable, plots_dist_norm, ncol = 1, heights = c(0.2, 1))

########## DISTRIBUZIONE T-STUDENT SIMMETRICA
set.seed(10)
dist_t_student_symmetric1 <- rt(n = n, df = df)
set.seed(20)
dist_t_student_symmetric2 <- rt(n = n, df = df)
set.seed(30)
dist_t_student_symmetric3 <- rt(n = n, df = df)

type_dist = "Symmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Histogram of ", .(n) ," samples generated from the ", .(type_dist))))

subtitle_content <- bquote(paste("First sample of the ", .(type_dist)))
# creo il primo plot dei campioni generati dalla distribuzione t-student simmetrica
hist_dist_symmetric <- ggplot(data.frame(value = dist_t_student_symmetric1), aes(x = value)) + 
  geom_histogram(binwidth = 0.2, alpha = 0.7) +
  labs(subtitle=subtitle_content, caption=" ") +
  xlab("Samples") + ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

subtitle_content <- bquote(paste("Second sample of the ", .(type_dist)))
# creo il secondo plot dei campioni generati dalla distribuzione t-student simmetrica
hist_dist_symmetric1 <- ggplot(data.frame(value = dist_t_student_symmetric2), aes(x = value)) + 
  geom_histogram(binwidth = 0.2, alpha = 0.7) +
  labs(subtitle=subtitle_content, caption=" ") +
  xlab("Samples") + ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

subtitle_content <- bquote(paste("Third sample of the ", .(type_dist)))
# creo il terzo plot dei campioni generati dalla distribuzione t-student simmetrica
hist_dist_symmetric2 <- ggplot(data.frame(value = dist_t_student_symmetric3), aes(x = value)) + 
  geom_histogram(binwidth = 0.2, alpha = 0.7) +
  #ggtitle("\n\n") +
  labs(subtitle=subtitle_content, caption=author_content) +
  xlab("Samples") + ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_symmetric <- plot_grid(hist_dist_symmetric, hist_dist_symmetric1, hist_dist_symmetric2, ncol = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
final_plot_dist_t_student_symmetric1 <- grid.arrange(title_content_gtable, plots_dist_symmetric, ncol = 1, heights = c(0.2, 1))

########## DISTRIBUZIONE T-STUDENT ASIMMETRICA
set.seed(10)
# alpha: parametro skewness con 0 < alpha < 1; 
# nu1: nu1 > 0, grado di libertà per la coda sinistra 
# nu2: nu2 > 0, grado di libertà per la coda destra
dist_t_student_asymmetric1 <- rast(n = n, mu = 0, s = 1, alpha = 0.9, nu1 = 5, nu2 = Inf, pars = NULL)
set.seed(20)
dist_t_student_asymmetric2 <- rast(n = n, mu = 0, s = 1, alpha = 0.9, nu1 = 5, nu2 = Inf, pars = NULL)
set.seed(30)
dist_t_student_asymmetric3 <- rast(n = n, mu = 0, s = 1, alpha = 0.9, nu1 = 5, nu2 = Inf, pars = NULL)

type_dist = "Asymmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Histogram of ", .(n) ," samples generated from the ", .(type_dist))))

subtitle_content <- bquote(paste("First sample of the ", .(type_dist)))
# creo il primo plot dei campioni generati dalla distribuzione t-student asimmetrica
hist_dist_asymmetric <- ggplot(data.frame(value = dist_t_student_asymmetric1), aes(x = value)) + 
  geom_histogram(binwidth = 0.2, alpha = 0.7) +
  labs(subtitle=subtitle_content, caption=" ") +
  xlab("Samples") + ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

subtitle_content <- bquote(paste("Second sample of the ", .(type_dist)))
# creo il secondo plot dei campioni generati dalla distribuzione t-student asimmetrica
hist_dist_asymmetric1 <- ggplot(data.frame(value = dist_t_student_asymmetric2), aes(x = value)) + 
  geom_histogram(binwidth = 0.2, alpha = 0.7) +
  labs(subtitle=subtitle_content, caption=" ") +
  xlab("Samples") + ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

subtitle_content <- bquote(paste("Third sample of the ", .(type_dist)))
# creo il terzo plot dei campioni generati dalla distribuzione t-student asimmetrica
hist_dist_asymmetric2 <- ggplot(data.frame(value = dist_t_student_asymmetric3), aes(x = value)) + 
  geom_histogram(binwidth = 0.2, alpha = 0.7) +
  #ggtitle("\n\n") +
  labs(subtitle=subtitle_content, caption=author_content) +
  xlab("Samples") + ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_asymmetric <- plot_grid(hist_dist_asymmetric, hist_dist_asymmetric1, hist_dist_asymmetric2, ncol = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
final_plot_dist_t_student_asymmetric1 <- grid.arrange(title_content_gtable, plots_dist_asymmetric, ncol = 1, heights = c(0.2, 1))

##########################################################################################################################
##########################################################################################################################
sigmasquared_t <- rep(NA,n)
W_t <- rep(NA,n)
X_t <- rep(NA,n)

" Funzione che implementa il modello Arch restituendo X_t per i diversi valori di q
# a0: è uno scalare
# aq: è uno scalare se q=1; altrimenti è un vettore
# X0: valore inziale
# W_t: distribuzione normale, t-student simmetrica o t-student asimmetrica
# q: assegnare valori 1 o 2"
model_arch <- function(a0, aq, X0, W_t, q){
  # genero X_t[1] attraverso la sola conoscenza di X0
  sigmasquared_t[1] <- (a0 + aq[1] * X0^2)
  X_t[1] <- sqrt(sigmasquared_t[1]) * W_t[1]
  
  if (q==1) { # calcolo sigmasquared_t = a0 + a1 * X_t[i-1]^2
  # per ogni elemento di W_t determino il valore associato di X_t
   for (i in 2:length(W_t)) {
    sigmasquared_t[i] <- a0 + aq[1] * (X_t[i-1]^2)
    X_t[i] <- W_t[i] * sqrt(sigmasquared_t[i])
   }
  } else if (q==2) { # calcolo sigmasquared_t = a0 + a1 * X_t[i-1]^2 + a2 * X_t[i-2]^2
    if (length(aq)<2){
      stop("Numero di valori deve essere uguale a 2")
    }
    # calcolo X_t[2] considerando X_t[1] e X0
    sigmasquared_t[2] <- a0 + aq[1] * (X_t[1]^2) + aq[2] * (X0^2)
    X_t[2] <- sqrt(sigmasquared_t[2]) * W_t[2]
    # per ogni elemento di W_t determino il valore associato di X_t
    for (i in 3:length(W_t)) {
      sigmasquared_t[i] <- a0 + aq[1] * (X_t[i-1]^2) + aq[2] * (X_t[i-2]^2)
      X_t[i] <- W_t[i] * sqrt(sigmasquared_t[i])
    }
  }
  X_t <- c(X0, X_t)
  return(X_t)
}

" Funzione che implementa il modello Garch restituendo X_t per i diversi valori di p e q
# a0: scalare
# aq: è uno scalare se q=1; altrimenti è un vettore
# bp: è uno scalare se p=1; altrimenti è un vettore
# X0, sigmasquared0: valori iniziali
# W_t: distribuzione normale, t-student simmetrica o t-student asimmetrica
# p, q: assegnare valori 1 o 2"
model_garch <- function(a0, aq, bp, X0, sigmasquared0, W_t, q, p){
  # genero il valore di Xt_1 attraverso la sola conoscenza di X0 e sigmasquared0
  sigmasquared_t[1] <- (a0 + aq[1] * X0^2 + bp[1] * sigmasquared0)
  X_t[1] <- sqrt(sigmasquared_t[1]) * W_t[1]
  
  if (p==1 && q==1) { # calcolo sigmasquared_t = a0 + a1 * X_t[i-1]^2 + b1 * sigmasquared_t[i-1]
    # per ogni elemento di W_t determino il valore associato di X_t
    for (i in 2:length(W_t)) {
      sigmasquared_t[i] <- a0 + aq[1] * (X_t[i-1]^2) + bp[1] *(sigmasquared_t[i-1])
      X_t[i] <- W_t[i] * sqrt(sigmasquared_t[i])
    }
  } else if (p==1 && q == 2){ # calcolo sigmasquared_t = a0 + a1 * X_t[i-1]^2 + a2 * X_t[i-2] + b1 * sigmasquared_t[i-1]
    if (length(aq)<2){
      stop("Numero di valori 'aq' deve essere uguale a 2")
    }
    # calcolo X_t[2] considerando X_t[1] e X0
    sigmasquared_t[2] <- (a0 + aq[1] * X_t[1]^2 + aq[2] * X0^2 + bp[1] * sigmasquared_t[1])
    X_t[2] <- sqrt(sigmasquared_t[2]) * W_t[2]
    # per ogni elemento di W_t determino il suo valore di X_t
    for (i in 3:length(W_t)) { 
      sigmasquared_t[i] <- a0 + aq[1] * (X_t[i-1]^2) + aq[2] * (X_t[i-2]^2) + bp *(sigmasquared_t[i-1])
      X_t[i] <- W_t[i] * sqrt(sigmasquared_t[i])
    }
  } else if (p==2 && q==1){ # calcolo sigmasquared_t = a0 + a1 * X_t[i-1]^2 + b1 * sigmasquared_t[i-1] + b2 * sigmasquared_t[i-2] 
    if (length(bp)<2){
      stop("Numero di valori 'bp' deve essere uguale a 2")
    }
    # calcolo X_t[2] considerando X_t[1], sigmasquaed_t[1] e sigmasquared0
    sigmasquared_t[2] <- (a0 + aq[1] * X_t[1]^2 + bp[1] * sigmasquared_t[1] + bp[2] * sigmasquared0)
    X_t[2] <- sqrt(sigmasquared_t[2]) * W_t[2]
    # per ogni elemento di W_t determino il suo valore di X_t
    for (i in 3:length(W_t)) {
      sigmasquared_t[i] <- (a0 + aq[1] * X_t[i-1]^2 + bp[1] * sigmasquared_t[i-1] + bp[2] * sigmasquared_t[i-2])
      X_t[i] <- W_t[i] * sqrt(sigmasquared_t[i])
    }
  } else if (p==2 && q==2){ # calcolo sigmasquared_t = a0 + a1 * X_t[i-1]^2 + a2 * X_t[i-2]^2 + b1 * sigmasquared_t[i-1] + b2 * sigmasquared_t[i-2] #b1 * sigmasquared_t[i-1] 
    if (length(aq)<2 || length(bp)<2){
      stop("Numero di valori 'as' e 'bs' deve essere uguale a 2")
    }
    # calcolo X_t[2] considerando X_t[1], X0, sigmasquared_t[1] e sigmasquared0
    sigmasquared_t[2] <- (a0 + aq[1] * X_t[1]^2 + aq[2] * X0^2 + bp[1] * sigmasquared_t[1] + bp[2] * sigmasquared0)
    X_t[2] <- sqrt(sigmasquared_t[2]) * W_t[2]
    # per ogni elemento di W_t determino il valore associato di X_t 
    for (i in 3:length(W_t)) {
      sigmasquared_t[i] <- (a0 + aq[1] * X_t[i-1]^2 + aq[2] * (X_t[i-2]^2) + bp[1] * sigmasquared_t[i-1] + bp[2] * sigmasquared_t[i-2])
      X_t[i] <- W_t[i] * sqrt(sigmasquared_t[i])
    }
  } 
  X_t <- c(X0, X_t)
  return(X_t)
}

# Consider the fit of the empirical distribution of the residuals with a Generalized Student distribution still by the function
# fitdistrplus::fitdist(). Following Venables and Ripley, we have to introduce the generalized Student distribution
# (see https://https://en.wikipedia.org/wiki/Student%27s_t-distribution). This is defined in terms of the R component functions of the 
# standard Student distribution, dt(), pt(), qt(), rt(), and the location, scale, and degrees of freedom parameters, m, s, and df, by the 
# functions
dt_ls <- function(x, m, s, df)	1/s*dt((x-m)/s, df)
pt_ls <- function(q, m, s, df)  pt((q-m)/s, df)
qt_ls <- function(p, m, s, df)  qt(p, df)*s+m
rt_ls <- function(n, m, s, df)  rt(n,df)*s+m
# In this case, the location parameter m is just the mean of the generalized Student distribution, but the scale parameter s, is given by
#
# s=sigma*sqrt((df-2)/df)
#
# where sigma is the standard deviation parameter of the distribution.
#############################################################################################################
#############################################################################################################
##########################################################################################################################
##########################################################################################################################
" Definiamo valori iniziali di X0, sigmasquared0, a0.
  Definiamo parametri a1, a2, b1, b2 in modo tale da soddisfare la condizione di stazionarietà: 
  - Modello Arch: 1 - a1*σ^2 > 0 -> 1 > a1*σ^2 con q=1
  - Modello Arch: 1 - (a1 + a2)*σ^2 > 0 -> 1 > (a1 + a2)*σ^2 con q=2
  - Modello Garch: 1 - a1*σ^2 - b1 > 0 -> 1 > a1*σ^2 + b1 con q=1 & p=1
  - Modello Garch: 1 - a1*σ^2 - b1 - b2 > 0 -> 1 > a1*σ^2 + b1 + b2 con q=1 & p=2
  - Modello Garch: 1 - (a1 + a2)*σ^2 - b1 > 0 -> 1 > (a1 + a2)*σ^2 + b1 con q=2 & p=1
  - Modello Garch: 1 - (a1 + a2)*σ^2 - b1 - b2 > 0 -> 1 > (a1 + a2) *σ^2 + b1 + b2 con q=2 & p=2
"
X0 <- 0
sigmasquared0 <- 0
a0 <- 0.3

a1 <- 0.1
b1 <- 0.3
aq <- c(0.1, 0.2)
bp <- c(0.1, 0.2)

##########################################################################################################################
"Costruisco un processo ARCH e GARCH per ogni distribuzione considerando q=1 e p=1 (solo per il modello GARCH)"

q <- 1
p <- 1 # utilizzato solo per Garch

############################################## " MODELLO ARCH "

modello[['simulazione']] <- list('arch_q1'=list('a0'=a0, 'a1'=a1, 'q'=q))
type_model = substitute(paste0("Model ARCH(", q, ")"))

########## DISTRIBUZIONE NORMALE
modello[['simulazione']][['arch_q1']] <- append(modello[['simulazione']][['arch_q1']], list('normale'=list()))
# Prima traiettoria
sigmasquaredW <- var(dist_normal1)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_normal_arch1_q1 <- model_arch(a0, a1, X0, dist_normal1, q)

modello[['simulazione']][['arch_q1']][['normale']] <- append(modello[['simulazione']][['arch_q1']][['normale']], list('1'=list('Xt'=Xt_normal_arch1_q1, 'stazionarietà'=a1*sigmasquaredW)))

# Seconda traiettoria
sigmasquaredW <- var(dist_normal2)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_normal_arch2_q1 <- model_arch(a0, a1, X0, dist_normal2, q)

modello[['simulazione']][['arch_q1']][['normale']] <- append(modello[['simulazione']][['arch_q1']][['normale']], list('2'=list('Xt'=Xt_normal_arch2_q1, 'stazionarietà'=a1*sigmasquaredW)))

# Terza traiettoria
sigmasquaredW <- var(dist_normal3)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_normal_arch3_q1 <- model_arch(a0, a1, X0, dist_normal3, q)

modello[['simulazione']][['arch_q1']][['normale']] <- append(modello[['simulazione']][['arch_q1']][['normale']], list('3'=list('Xt'=Xt_normal_arch3_q1, 'stazionarietà'=a1*sigmasquaredW)))

# creo il primo plot del modello Arch(1) con la distribuzione normale
plot_dist_normal1 <- ggplot(data.frame(value = Xt_normal_arch1_q1, index = seq_along(Xt_normal_arch1_q1)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione normale
plot_dist_normal2 <- ggplot(data.frame(value = Xt_normal_arch2_q1, index = seq_along(Xt_normal_arch2_q1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione normale
plot_dist_normal3 <- ggplot(data.frame(value = Xt_normal_arch3_q1, index = seq_along(Xt_normal_arch3_q1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# Grid plot
# creo la figura utilizzando una griglia con i tre plot generati
type_dist = "Normal Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)), " of a ", .(type_dist)) ))
subtitle_content <- (paste("path length ", (samples), " sample points"))
plots_dist_norm <- plot_grid(plot_dist_normal1, plot_dist_normal2, plot_dist_normal3, nrow = 3)
plots <- plots_dist_norm +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=author_content) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(plots)
  
##########  DISTRUBUZIONE T-STUDENT SIMMETRICA
modello[['simulazione']][['arch_q1']] <- append(modello[['simulazione']][['arch_q1']], list('simmetrico'=list()))

# Prima traiettoria
sigmasquaredW <- var(dist_t_student_symmetric1)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_symmetric_arch1_q1 <- model_arch(a0, a1, X0, dist_t_student_symmetric1, q)

modello[['simulazione']][['arch_q1']][['simmetrico']] <- append(modello[['simulazione']][['arch_q1']][['simmetrico']], list('1'=list('Xt'=Xt_t_student_symmetric_arch1_q1, 'stazionarietà'=a1*sigmasquaredW)))

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_symmetric2)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_symmetric_arch2_q1 <- model_arch(a0, a1, X0, dist_t_student_symmetric2, q)

modello[['simulazione']][['arch_q1']][['simmetrico']] <- append(modello[['simulazione']][['arch_q1']][['simmetrico']], list('2'=list('Xt'=Xt_t_student_symmetric_arch2_q1, 'stazionarietà'=a1*sigmasquaredW)))

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_symmetric3)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_symmetric_arch3_q1 <- model_arch(a0, a1, X0, dist_t_student_symmetric3, q)

modello[['simulazione']][['arch_q1']][['simmetrico']] <- append(modello[['simulazione']][['arch_q1']][['simmetrico']], list('3'=list('Xt'=Xt_t_student_symmetric_arch3_q1, 'stazionarietà'=a1*sigmasquaredW)))

# creo il primo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric <- ggplot(data.frame(value = Xt_t_student_symmetric_arch1_q1, index = seq_along(Xt_t_student_symmetric_arch1_q1)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric1 <- ggplot(data.frame(value = Xt_t_student_symmetric_arch2_q1, index = seq_along(Xt_t_student_symmetric_arch2_q1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric2 <- ggplot(data.frame(value = Xt_t_student_symmetric_arch3_q1, index = seq_along(Xt_t_student_symmetric_arch3_q1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# Grid plot
# creo la figura utilizzando una griglia con i tre plot generati
type_dist = "Symmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)), " of a ", .(type_dist)) ))
subtitle_content <- (paste("path length ", (samples), " sample points"))
plots_dist_t_student_symmetric1 <- plot_grid(plot_t_student_symmetric, plot_t_student_symmetric1, plot_t_student_symmetric2, nrow = 3)
plots <- plots_dist_t_student_symmetric1 +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=author_content) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(plots)

##########  DISTRUBUZIONE T-STUDENT  ASIMMETRICA
modello[['simulazione']][['arch_q1']] <- append(modello[['simulazione']][['arch_q1']], list('asimmetrico'=list()))

# Prima traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric1)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_asymmetric_arch1_q1 <- model_arch(a0, a1, X0, dist_t_student_asymmetric1, q)

modello[['simulazione']][['arch_q1']][['asimmetrico']] <- append(modello[['simulazione']][['arch_q1']][['asimmetrico']], list('1'=list('Xt'=Xt_t_student_asymmetric_arch1_q1, 'stazionarietà'=a1*sigmasquaredW)))

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric2)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_asymmetric_arch2_q1 <- model_arch(a0, a1, X0, dist_t_student_asymmetric2, q)

modello[['simulazione']][['arch_q1']][['asimmetrico']] <- append(modello[['simulazione']][['arch_q1']][['asimmetrico']], list('2'=list('Xt'=Xt_t_student_asymmetric_arch2_q1, 'stazionarietà'=a1*sigmasquaredW)))

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric3)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_asymmetric_arch3_q1 <- model_arch(a0, a1, X0, dist_t_student_asymmetric3, q)

modello[['simulazione']][['arch_q1']][['asimmetrico']] <- append(modello[['simulazione']][['arch_q1']][['asimmetrico']], list('3'=list('Xt'=Xt_t_student_asymmetric_arch3_q1, 'stazionarietà'=a1*sigmasquaredW)))

# creo il primo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric <- ggplot(data.frame(value = Xt_t_student_asymmetric_arch1_q1, index = seq_along(Xt_t_student_asymmetric_arch1_q1)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric1 <- ggplot(data.frame(value = Xt_t_student_asymmetric_arch2_q1, index = seq_along(Xt_t_student_asymmetric_arch2_q1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric2 <- ggplot(data.frame(value = Xt_t_student_asymmetric_arch3_q1, index = seq_along(Xt_t_student_asymmetric_arch3_q1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# Grid plot
# creo la figura utilizzando una griglia con i tre plot generati
type_dist = "Asymmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))
subtitle_content <- (paste("path length ", (samples), " sample points"))
plots_dist_t_student_asymmetric1 <- plot_grid(plot_t_student_asymmetric, plot_t_student_asymmetric1, plot_t_student_asymmetric2, nrow = 3)
plots <- plots_dist_t_student_asymmetric1 +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=author_content) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, size = 10),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(plots)

############################################## " MODELLO GARCH "

modello[['simulazione']] <- append(modello[['simulazione']], list('garch_q1_p1'=list('a0'=a0, 'a1'=a1, 'b1'=b1, 'q'=q, 'p'=p)))
type_model = substitute(paste0("Model GARCH(", q, ",", p, ")"))

##########  DISTRUBUZIONE NORMALE
modello[['simulazione']][['garch_q1_p1']] <- append(modello[['simulazione']][['garch_q1_p1']], list('normale'=list()))

# Prima traiettoria
sigmasquaredW <- var(dist_normal1)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_normal_garch1_q1_p1  <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_normal1, q, p)

modello[['simulazione']][['garch_q1_p1']][['normale']] <- append(modello[['simulazione']][['garch_q1_p1']][['normale']], list('1'=list('Xt'=Xt_normal_garch1_q1_p1, 'stazionarietà'=a1*sigmasquaredW)))

# Seconda traiettoria
sigmasquaredW <- var(dist_normal2)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_normal_garch2_q1_p1  <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_normal2, q, p)

modello[['simulazione']][['garch_q1_p1']][['normale']] <- append(modello[['simulazione']][['garch_q1_p1']][['normale']], list('2'=list('Xt'=Xt_normal_garch2_q1_p1, 'stazionarietà'=a1*sigmasquaredW)))

# Terza traiettoria
sigmasquaredW <- var(dist_normal3)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_normal_garch3_q1_p1  <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_normal3, q, p)

modello[['simulazione']][['garch_q1_p1']][['normale']] <- append(modello[['simulazione']][['garch_q1_p1']][['normale']], list('3'=list('Xt'=Xt_normal_garch3_q1_p1, 'stazionarietà'=a1*sigmasquaredW)))

# creo il primo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal1 <- ggplot(data.frame(value = Xt_normal_garch1_q1_p1 , index = seq_along(Xt_normal_garch1_q1_p1 )), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal2 <- ggplot(data.frame(value = Xt_normal_garch2_q1_p1 , index = seq_along(Xt_normal_garch1_q1_p1 )), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal3 <- ggplot(data.frame(value = Xt_normal_garch3_q1_p1 , index = seq_along(Xt_normal_garch1_q1_p1 )), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# Grid plot
# creo la figura utilizzando una griglia con i tre plot generati
type_dist = "Normal Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)), " of a ", .(type_dist))))
subtitle_content <- (paste("path length ", (samples), " sample points"))
plots_dist_norm <- plot_grid(plot_dist_normal1, plot_dist_normal2, plot_dist_normal3, nrow = 3)
plots <- plots_dist_norm +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=author_content) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, size = 10),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(plots)

##########  DISTRUBUZIONE T-STUDENT  SIMMETRICA
modello[['simulazione']][['garch_q1_p1']] <- append(modello[['simulazione']][['garch_q1_p1']], list('simmetrico'=list()))

# Prima traiettoria
sigmasquaredW <- var(dist_t_student_symmetric1)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_symmetric_garch1_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_symmetric1, q, p)

modello[['simulazione']][['garch_q1_p1']][['simmetrico']] <- append(modello[['simulazione']][['garch_q1_p1']][['simmetrico']], list('1'=list('Xt'=Xt_t_student_symmetric_garch1_q1_p1, 'stazionarietà'=a1*sigmasquaredW)))

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_symmetric2)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_symmetric_garch2_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_symmetric2, q, p)

modello[['simulazione']][['garch_q1_p1']][['simmetrico']] <- append(modello[['simulazione']][['garch_q1_p1']][['simmetrico']], list('2'=list('Xt'=Xt_t_student_symmetric_garch2_q1_p1, 'stazionarietà'=a1*sigmasquaredW)))

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_symmetric3)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_symmetric_garch3_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_symmetric3, q, p)

modello[['simulazione']][['garch_q1_p1']][['simmetrico']] <- append(modello[['simulazione']][['garch_q1_p1']][['garch_q1_p1']][['simmetrico']], list('3'=list('Xt'=Xt_t_student_symmetric_garch3_q1_p1, 'stazionarietà'=a1*sigmasquaredW)))

# creo il primo plot del modello Garch(1,1) con la distribuzione t-student simmetrica
plot_t_student_symmetric <- ggplot(data.frame(value = Xt_t_student_symmetric_garch1_q1_p1, index = seq_along(Xt_t_student_symmetric_garch1_q1_p1)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Garch(1,1) con la distribuzione t-student simmetrica
plot_t_student_symmetric1 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch2_q1_p1, index = seq_along(Xt_t_student_symmetric_garch2_q1_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Garch(1,1) con la distribuzione t-student simmetrica
plot_t_student_symmetric2 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch3_q1_p1, index = seq_along(Xt_t_student_symmetric_garch3_q1_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# Grid plot
# creo la figura utilizzando una griglia con i tre plot generati
type_dist = "Symmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))
subtitle_content <- (paste("path length ", (samples), " sample points"))
plots_dist_t_student_symmetric1 <- plot_grid(plot_t_student_symmetric, plot_t_student_symmetric1, plot_t_student_symmetric2, nrow = 3)
plots <- plots_dist_t_student_symmetric1 +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=author_content) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, size = 10),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(plots)

##########  DISTRUBUZIONE T-STUDENT  ASIMMETRICA
modello[['simulazione']][['garch_q1_p1']] <- append(modello[['simulazione']][['garch_q1_p1']], list('asimmetrico'=list()))

# Prima traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric1)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_asymmetric_garch1_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)

modello[['simulazione']][['garch_q1_p1']][['asimmetrico']] <- append(modello[['simulazione']][['garch_q1_p1']][['asimmetrico']], list('1'=list('Xt'=Xt_t_student_asymmetric_garch1_q1_p1, 'stazionarietà'=a1*sigmasquaredW)))

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric2)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_asymmetric_garch2_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_asymmetric2, q, p)

modello[['simulazione']][['garch_q1_p1']][['asimmetrico']] <- append(modello[['simulazione']][['garch_q1_p1']][['asimmetrico']], list('2'=list('Xt'=Xt_t_student_asymmetric_garch2_q1_p1, 'stazionarietà'=a1*sigmasquaredW)))
# Terza traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric3)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_asymmetric_garch3_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_asymmetric3, q, p)

modello[['simulazione']][['garch_q1_p1']][['asimmetrico']] <- append(modello[['simulazione']][['garch_q1_p1']][['asimmetrico']], list('3'=list('Xt'=Xt_t_student_asymmetric_garch3_q1_p1, 'stazionarietà'=a1*sigmasquaredW)))

# creo il primo plot del modello Garch(1,1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch1_q1_p1, index = seq_along(Xt_t_student_asymmetric_garch1_q1_p1)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Garch(1,1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric1 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch2_q1_p1, index = seq_along(Xt_t_student_asymmetric_garch2_q1_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Garch(1,1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric2 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch3_q1_p1, index = seq_along(Xt_t_student_asymmetric_garch3_q1_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# Grid plot
# creo la figura utilizzando una griglia con i tre plot generati
type_dist = "Asymmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))
subtitle_content <- (paste("path length ", (samples), " sample points"))
plots_dist_t_student_asymmetric1 <- plot_grid(plot_t_student_asymmetric, plot_t_student_asymmetric1, plot_t_student_asymmetric2, nrow = 3)
plots <- plots_dist_t_student_asymmetric1 +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=author_content) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, size = 10),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(plots)

##########################################################################################################################
"Costruisco un processo GARCH per ogni distribuzione considerando q=1 e p=2 (solo per il modello GARCH)"

q <- 1
p <- 2 # utilizzato solo per Garch

############################################## " MODELLO GARCH "

modello[['simulazione']] <- append(modello[['simulazione']], list('garch_q1_p2'=list('a0'=a0, 'a1'=a1, 'b1'=bp[1], 'b2'=bp[2], 'q'=q, 'p'=p)))
type_model = substitute(paste0("Model GARCH(", q, ",", p, ")"))

##########  DISTRUBUZIONE NORMALE
modello[['simulazione']][['garch_q1_p2']] <- append(modello[['simulazione']][['garch_q1_p2']], list('normale'=list()))

# Prima traiettoria
sigmasquaredW <- var(dist_normal1)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW + bp[1] + bp[2]))
Xt_normal_garch1_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_normal1, q, p)

modello[['simulazione']][['garch_q1_p2']][['normale']] <- append(modello[['simulazione']][['garch_q1_p2']][['normale']], list('1'=list('Xt'=Xt_normal_garch1_q1_p2, 'stazionarietà'=a1*sigmasquaredW + bp[1] + bp[2])))

# Seconda traiettoria
sigmasquaredW <- var(dist_normal2)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW + bp[1] + bp[2]))
Xt_normal_garch2_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_normal2, q, p)

modello[['simulazione']][['garch_q1_p2']][['normale']] <- append(modello[['simulazione']][['garch_q1_p2']][['normale']], list('2'=list('Xt'=Xt_normal_garch2_q1_p2, 'stazionarietà'=a1*sigmasquaredW + bp[1] + bp[2])))

# Terza traiettoria
sigmasquaredW <- var(dist_normal3)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW + bp[1] + bp[2]))
Xt_normal_garch3_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_normal3, q, p)

modello[['simulazione']][['garch_q1_p2']][['normale']] <- append(modello[['simulazione']][['garch_q1_p2']][['normale']], list('3'=list('Xt'=Xt_normal_garch3_q1_p2, 'stazionarietà'=a1*sigmasquaredW + bp[1] + bp[2])))

# creo il primo plot del modello Garch(1,2) con la distribuzione normale
plot_dist_normal1 <- ggplot(data.frame(value = Xt_normal_garch1_q1_p2, index = seq_along(Xt_normal_garch1_q1_p2)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Garch(1,2) con la distribuzione normale
plot_dist_normal2 <- ggplot(data.frame(value = Xt_normal_garch2_q1_p2, index = seq_along(Xt_normal_garch2_q1_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Garch(1,2) con la distribuzione normale
plot_dist_normal3 <- ggplot(data.frame(value = Xt_normal_garch3_q1_p2, index = seq_along(Xt_normal_garch3_q1_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# Grid plot
# creo la figura utilizzando una griglia con i tre plot generati
type_dist = "Normal Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)), " of a ", .(type_dist))))
subtitle_content <- (paste("path length ", (samples), " sample points"))
plots_dist_norm <- plot_grid(plot_dist_normal1, plot_dist_normal2, plot_dist_normal3, nrow = 3)
plots <- plots_dist_norm +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=author_content) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, size = 10),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(plots)

##########  DISTRUBUZIONE T-STUDENT  SIMMETRICA
modello[['simulazione']][['garch_q1_p2']] <- append(modello[['simulazione']][['garch_q1_p2']], list('simmetrico'=list()))

# Prima traiettoria
sigmasquaredW <- var(dist_t_student_symmetric1)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW + bp[1] + bp[2]))
Xt_t_student_symmetric_garch1_q1_p2<- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_symmetric1, q, p)

modello[['simulazione']][['garch_q1_p2']][['simmetrico']] <- append(modello[['simulazione']][['garch_q1_p2']][['simmetrico']], list('1'=list('Xt'=Xt_t_student_symmetric_garch1_q1_p2, 'stazionarietà'=a1*sigmasquaredW + bp[1] + bp[2])))

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_symmetric2)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW + bp[1] + bp[2]))
Xt_t_student_symmetric_garch2_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_symmetric2, q, p)

modello[['simulazione']][['garch_q1_p2']][['simmetrico']] <- append(modello[['simulazione']][['garch_q1_p2']][['simmetrico']], list('2'=list('Xt'=Xt_t_student_symmetric_garch2_q1_p2, 'stazionarietà'=a1*sigmasquaredW + bp[1] + bp[2])))

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_symmetric3)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW + bp[1] + bp[2]))
Xt_t_student_symmetric_garch3_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_symmetric3, q, p)

modello[['simulazione']][['garch_q1_p2']][['simmetrico']] <- append(modello[['simulazione']][['garch_q1_p2']][['simmetrico']], list('3'=list('Xt'=Xt_t_student_symmetric_garch3_q1_p2, 'stazionarietà'=a1*sigmasquaredW + bp[1] + bp[2])))

# creo il primo plot del modello Garch(1,2) con la distribuzione t-student simmetrica
plot_t_student_symmetric <- ggplot(data.frame(value = Xt_t_student_symmetric_garch1_q1_p2, index = seq_along(Xt_t_student_symmetric_garch1_q1_p2)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Garch(1,2) con la distribuzione t-student simmetrica
plot_t_student_symmetric1 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch2_q1_p2, index = seq_along(Xt_t_student_symmetric_garch2_q1_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Garch(1,2) con la distribuzione t-student simmetrica
plot_t_student_symmetric2 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch3_q1_p2, index = seq_along(Xt_t_student_symmetric_garch3_q1_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# Grid plot
# creo la figura utilizzando una griglia con i tre plot generati
type_dist = "Symmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))
subtitle_content <- (paste("path length ", (samples), " sample points"))
plots_dist_t_student_symmetric1 <- plot_grid(plot_t_student_symmetric, plot_t_student_symmetric1, plot_t_student_symmetric2, nrow = 3)
plots <- plots_dist_t_student_symmetric1 +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=author_content) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, size = 10),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(plots)

##########  DISTRUBUZIONE T-STUDENT  ASIMMETRICA
modello[['simulazione']][['garch_q1_p2']] <- append(modello[['simulazione']][['garch_q1_p2']], list('asimmetrico'=list()))

# Prima traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric1)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW + bp[1] + bp[2]))
Xt_t_student_asymmetric_garch1_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)

modello[['simulazione']][['garch_q1_p2']][['asimmetrico']] <- append(modello[['simulazione']][['garch_q1_p2']][['asimmetrico']], list('1'=list('Xt'=Xt_t_student_asymmetric_garch1_q1_p2, 'stazionarietà'=a1*sigmasquaredW + bp[1] + bp[2])))

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric2)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW + bp[1] + bp[2]))
Xt_t_student_asymmetric_garch2_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_asymmetric2, q, p)

modello[['simulazione']][['garch_q1_p2']][['asimmetrico']] <- append(modello[['simulazione']][['garch_q1_p2']][['asimmetrico']], list('2'=list('Xt'=Xt_t_student_asymmetric_garch2_q1_p2, 'stazionarietà'=a1*sigmasquaredW + bp[1] + bp[2])))

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric3)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW + bp[1] + bp[2]))
Xt_t_student_asymmetric_garch3_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_asymmetric3, q, p)

modello[['simulazione']][['garch_q1_p2']][['asimmetrico']] <- append(modello[['simulazione']][['garch_q1_p2']][['asimmetrico']], list('3'=list('Xt'=Xt_t_student_asymmetric_garch3_q1_p2, 'stazionarietà'=a1*sigmasquaredW + bp[1] + bp[2])))

# creo il primo plot del modello Garch(1,2) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch1_q1_p2, index = seq_along(Xt_t_student_asymmetric_garch1_q1_p2)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Garch(1,2) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric1 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch2_q1_p2, index = seq_along(Xt_t_student_asymmetric_garch2_q1_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Garch(1,2) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric2 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch3_q1_p2, index = seq_along(Xt_t_student_asymmetric_garch3_q1_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# Grid plot
# creo la figura utilizzando una griglia con i tre plot generati
type_dist = "Asymmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))
subtitle_content <- (paste("path length ", (samples), " sample points"))
plots_dist_t_student_asymmetric1 <- plot_grid(plot_t_student_asymmetric, plot_t_student_asymmetric1, plot_t_student_asymmetric2, nrow = 3)
plots <- plots_dist_t_student_asymmetric1 +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=author_content) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, size = 10),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(plots)

##########################################################################################################################
"Costruisco un processo ARCH e GARCH per ogni distribuzione considerando q=2 e p=1 (solo per il modello GARCH)"

q <- 2
p <- 1 # utilizzato solo per Garch

############################################## " MODELLO ARCH "

modello[['simulazione']] <- append(modello[['simulazione']], list('arch_q2'=list('a0'=a0, 'a1'=aq[1], 'a2'=aq[2], 'q'=q, 'p'=0)))
type_model = substitute(paste0("Model ARCH(", q, ")"))

##########  DISTRIBUZIONE NORMALE
modello[['simulazione']][['arch_q2']] <- append(modello[['simulazione']][['arch_q2']], list('normale'=list()))

# Prima traiettoria
sigmasquaredW <- var(dist_normal1)
print(paste("Verifico condizione di stazionerietà: ",(aq[1]+aq[2])*sigmasquaredW))
Xt_normal_arch1_q2 <- model_arch(a0, aq, X0, dist_normal1, q)

modello[['simulazione']][['arch_q2']][['normale']] <- append(modello[['simulazione']][['arch_q2']][['normale']], list('1'=list('Xt'=Xt_normal_arch1_q2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW)))

# Seconda traiettoria
sigmasquaredW <- var(dist_normal2)
print(paste("Verifico condizione di stazionerietà: ",(aq[1]+aq[2])*sigmasquaredW))
Xt_normal_arch2_q2 <- model_arch(a0, aq, X0, dist_normal2, q)

modello[['simulazione']][['arch_q2']][['normale']] <- append(modello[['simulazione']][['arch_q2']][['normale']], list('2'=list('Xt'=Xt_normal_arch2_q2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW)))

# Terza traiettoria
sigmasquaredW <- var(dist_normal3)
print(paste("Verifico condizione di stazionerietà: ",(aq[1]+aq[2])*sigmasquaredW))
Xt_normal_arch3_q2 <- model_arch(a0, aq, X0, dist_normal3, q)

modello[['simulazione']][['arch_q2']][['normale']] <- append(modello[['simulazione']][['arch_q2']][['normale']], list('3'=list('Xt'=Xt_normal_arch3_q2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW)))

# creo il primo plot del modello Arch(2) con la distribuzione normale
plot_dist_normal1 <- ggplot(data.frame(value = Xt_normal_arch1_q2, index = seq_along(Xt_normal_arch1_q2)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(2) con la distribuzione normale
plot_dist_normal2 <- ggplot(data.frame(value = Xt_normal_arch2_q2, index = seq_along(Xt_normal_arch1_q2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(2) con la distribuzione normale
plot_dist_normal3 <- ggplot(data.frame(value = Xt_normal_arch3_q2, index = seq_along(Xt_normal_arch1_q2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# Grid plot
# creo la figura utilizzando una griglia con i tre plot generati
type_dist = "Normal Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))
subtitle_content <- (paste("path length ", (samples), " sample points"))
plots_dist_norm <- plot_grid(plot_dist_normal1, plot_dist_normal2, plot_dist_normal3, nrow = 3)
plots <- plots_dist_norm +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=author_content) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, size = 10),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(plots)

##########  DISTRUBUZIONE T-STUDENT SIMMETRICA
modello[['simulazione']][['arch_q2']] <- append(modello[['simulazione']][['arch_q2']], list('simmetrico'=list()))

# Prima traiettoria
sigmasquaredW <- var(dist_t_student_symmetric1)
print(paste("Verifico condizione di stazionerietà: ",(aq[1]+aq[2])*sigmasquaredW))
Xt_t_student_symmetric_arch1_q2 <- model_arch(a0, aq, X0, dist_t_student_symmetric1, q)

modello[['simulazione']][['arch_q2']][['simmetrico']] <- append(modello[['simulazione']][['arch_q2']][['simmetrico']], list('1'=list('Xt'=Xt_t_student_symmetric_arch1_q2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW)))

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_symmetric2)
print(paste("Verifico condizione di stazionerietà: ",(aq[1]+aq[2])*sigmasquaredW))
Xt_t_student_symmetric_arch2_q2 <- model_arch(a0, aq, X0, dist_t_student_symmetric2, q)

modello[['simulazione']][['arch_q2']][['simmetrico']] <- append(modello[['simulazione']][['arch_q2']][['simmetrico']], list('2'=list('Xt'=Xt_t_student_symmetric_arch2_q2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW)))

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_symmetric3)
print(paste("Verifico condizione di stazionerietà: ",(aq[1]+aq[2])*sigmasquaredW))
Xt_t_student_symmetric_arch3_q2 <- model_arch(a0, aq, X0, dist_t_student_symmetric3, q)

modello[['simulazione']][['arch_q2']][['simmetrico']] <- append(modello[['simulazione']][['arch_q2']][['simmetrico']], list('3'=list('Xt'=Xt_t_student_symmetric_arch3_q2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW)))

# creo il primo plot del modello Arch(2) con la distribuzione t-student simmetrica
plot_t_student_symmetric <- ggplot(data.frame(value = Xt_t_student_symmetric_arch1_q2, index = seq_along(Xt_t_student_symmetric_arch1_q2)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(2) con la distribuzione t-student simmetrica
plot_t_student_symmetric1 <- ggplot(data.frame(value = Xt_t_student_symmetric_arch2_q2, index = seq_along(Xt_t_student_symmetric_arch2_q2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(2) con la distribuzione t-student simmetrica
plot_t_student_symmetric2 <- ggplot(data.frame(value = Xt_t_student_symmetric_arch3_q2, index = seq_along(Xt_t_student_symmetric_arch3_q2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# Grid plot
# creo la figura utilizzando una griglia con i tre plot generati
type_dist = "Symmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))
subtitle_content <- (paste("path length ", (samples), " sample points"))
plots_dist_t_student_symmetric1 <- plot_grid(plot_t_student_symmetric, plot_t_student_symmetric1, plot_t_student_symmetric2, nrow = 3)
plots <- plots_dist_t_student_symmetric1 +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=author_content) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, size = 10),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(plots)

##########  DISTRUBUZIONE T-STUDENT  ASIMMETRICA
modello[['simulazione']][['arch_q2']] <- append(modello[['simulazione']][['arch_q2']], list('asimmetrico'=list()))

# Prima traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric1)
print(paste("Verifico condizione di stazionerietà: ",(aq[1]+aq[2])*sigmasquaredW))
Xt_t_student_asymmetric_arch1_q2 <- model_arch(a0, aq, X0, dist_t_student_asymmetric1, q)

modello[['simulazione']][['arch_q2']][['asimmetrico']] <- append(modello[['simulazione']][['arch_q2']][['asimmetrico']], list('1'=list('Xt'=Xt_t_student_asymmetric_arch1_q2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW)))

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric2)
print(paste("Verifico condizione di stazionerietà: ",(aq[1]+aq[2])*sigmasquaredW))
Xt_t_student_asymmetric_arch2_q2 <- model_arch(a0, aq, X0, dist_t_student_asymmetric2, q)

modello[['simulazione']][['arch_q2']][['asimmetrico']] <- append(modello[['simulazione']][['arch_q2']][['asimmetrico']], list('2'=list('Xt'=Xt_t_student_asymmetric_arch2_q2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW)))

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric3)
print(paste("Verifico condizione di stazionerietà: ",(aq[1]+aq[2])*sigmasquaredW))
Xt_t_student_asymmetric_arch3_q2 <- model_arch(a0, aq, X0, dist_t_student_asymmetric3, q)

modello[['simulazione']][['arch_q2']][['asimmetrico']] <- append(modello[['simulazione']][['arch_q2']][['asimmetrico']], list('3'=list('Xt'=Xt_t_student_asymmetric_arch3_q2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW)))

# creo il primo plot del modello Arch(2) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric <- ggplot(data.frame(value = Xt_t_student_asymmetric_arch1_q2, index = seq_along(Xt_t_student_asymmetric_arch1_q2)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(2) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric1 <- ggplot(data.frame(value = Xt_t_student_asymmetric_arch2_q2, index = seq_along(Xt_t_student_asymmetric_arch2_q2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(2) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric2 <- ggplot(data.frame(value = Xt_t_student_asymmetric_arch3_q2, index = seq_along(Xt_t_student_asymmetric_arch3_q2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# Grid plot
# creo la figura utilizzando una griglia con i tre plot generati
type_dist = "Asymmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))
subtitle_content <- (paste("path length ", (samples), " sample points"))
plots_dist_t_student_asymmetric1 <- plot_grid(plot_t_student_asymmetric, plot_t_student_asymmetric1, plot_t_student_asymmetric2, nrow = 3)
plots <- plots_dist_t_student_symmetric1 +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=author_content) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, size = 10),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(plots)

############################################## " MODELLO GARCH "

modello[['simulazione']] <- append(modello[['simulazione']], list('garch_q2_p1'=list('a0'=a0, 'a1'=aq[1], 'a2'=aq[2], 'b1'=b1, 'q'=q, 'p'=p)))
type_model = substitute(paste0("Model GARCH(", q, ",", p, ")"))

##########  DISTRUBUZIONE NORMALE
modello[['simulazione']][['garch_q2_p1']] <- append(modello[['simulazione']][['garch_q2_p1']], list('normale'=list()))

# Prima traiettoria
sigmasquaredW <- var(dist_normal1)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + b1))
Xt_normal_garch1_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_normal1, q, p)

modello[['simulazione']][['garch_q2_p1']][['normale']] <- append(modello[['simulazione']][['garch_q2_p1']][['normale']], list('1'=list('Xt'=Xt_normal_garch1_q2_p1, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + b1)))

# Seconda traiettoria
sigmasquaredW <- var(dist_normal2)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + b1))
Xt_normal_garch2_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_normal2, q, p)

modello[['simulazione']][['garch_q2_p1']][['normale']] <- append(modello[['simulazione']][['garch_q2_p1']][['normale']], list('2'=list('Xt'=Xt_normal_garch2_q2_p1, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + b1)))

# Terza traiettoria
sigmasquaredW <- var(dist_normal3)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + b1))
Xt_normal_garch3_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_normal3, q, p)

modello[['simulazione']][['garch_q2_p1']][['normale']] <- append(modello[['simulazione']][['garch_q2_p1']][['normale']], list('3'=list('Xt'=Xt_normal_garch3_q2_p1, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + b1)))

# creo il primo plot del modello Garch(2,1) con la distribuzione normale
plot_dist_normal1 <- ggplot(data.frame(value = Xt_normal_garch1_q2_p1, index = seq_along(Xt_normal_garch1_q2_p1)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Garch(2,1) con la distribuzione normale
plot_dist_normal2 <- ggplot(data.frame(value = Xt_normal_garch2_q2_p1, index = seq_along(Xt_normal_garch1_q2_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Garch(2,1) con la distribuzione normale
plot_dist_normal3 <- ggplot(data.frame(value = Xt_normal_garch3_q2_p1, index = seq_along(Xt_normal_garch1_q2_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# Grid plot
# creo la figura utilizzando una griglia con i tre plot generati
type_dist = "Normal Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)), " of a ", .(type_dist))))
subtitle_content <- (paste("path length ", (samples), " sample points"))
plots_dist_norm <- plot_grid(plot_dist_normal1, plot_dist_normal2, plot_dist_normal3, nrow = 3)
plots <- plots_dist_norm +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=author_content) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, size = 10),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(plots)

##########  DISTRUBUZIONE T-STUDENT  SIMMETRICA
modello[['simulazione']][['garch_q2_p1']] <- append(modello[['simulazione']][['garch_q2_p1']], list('simmetrico'=list()))

# Prima traiettoria
sigmasquaredW <- var(dist_t_student_symmetric1)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + b1))
Xt_t_student_symmetric_garch1_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_symmetric1, q, p)

modello[['simulazione']][['garch_q2_p1']][['simmetrico']] <- append(modello[['simulazione']][['garch_q2_p1']][['simmetrico']], list('1'=list('Xt'=Xt_t_student_symmetric_garch1_q2_p1, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + b1)))

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_symmetric2)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + b1))
Xt_t_student_symmetric_garch2_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_symmetric2, q, p)

modello[['simulazione']][['garch_q2_p1']][['simmetrico']] <- append(modello[['simulazione']][['garch_q2_p1']][['simmetrico']], list('2'=list('Xt'=Xt_t_student_symmetric_garch2_q2_p1, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + b1)))

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_symmetric3)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + b1))
Xt_t_student_symmetric_garch3_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_symmetric3, q, p)

modello[['simulazione']][['garch_q2_p1']][['simmetrico']] <- append(modello[['simulazione']][['garch_q2_p1']][['simmetrico']], list('3'=list('Xt'=Xt_t_student_symmetric_garch3_q2_p1, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + b1)))

# creo il primo plot del modello Garch(2,1) con la distribuzione t-student simmetrica
plot_t_student_symmetric <- ggplot(data.frame(value = Xt_t_student_symmetric_garch1_q2_p1, index = seq_along(Xt_t_student_symmetric_garch1_q2_p1)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Garch(2,1) con la distribuzione t-student simmetrica
plot_t_student_symmetric1 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch2_q2_p1, index = seq_along(Xt_t_student_symmetric_garch2_q2_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Garch(2,1) con la distribuzione t-student simmetrica
plot_t_student_symmetric2 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch3_q2_p1, index = seq_along(Xt_t_student_symmetric_garch3_q2_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# Grid plot
# creo la figura utilizzando una griglia con i tre plot generati
type_dist = "Symmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))
subtitle_content <- (paste("path length ", (samples), " sample points"))
plots_dist_t_student_symmetric1 <- plot_grid(plot_t_student_symmetric, plot_t_student_symmetric1, plot_t_student_symmetric2, nrow = 3)
plots <- plots_dist_t_student_symmetric1 +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=author_content) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, size = 10),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(plots)

##########  DISTRUBUZIONE T-STUDENT  ASIMMETRICA
modello[['simulazione']][['garch_q2_p1']] <- append(modello[['simulazione']][['garch_q2_p1']], list('asimmetrico'=list()))

# Prima traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric1)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + b1))
Xt_t_student_asymmetric_garch1_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)

modello[['simulazione']][['garch_q2_p1']][['asimmetrico']] <- append(modello[['simulazione']][['garch_q2_p1']][['asimmetrico']], list('1'=list('Xt'=Xt_t_student_asymmetric_garch1_q2_p1, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + b1)))

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric2)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + b1))
Xt_t_student_asymmetric_garch2_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_asymmetric2, q, p)

modello[['simulazione']][['garch_q2_p1']][['asimmetrico']] <- append(modello[['simulazione']][['garch_q2_p1']][['asimmetrico']], list('2'=list('Xt'=Xt_t_student_asymmetric_garch2_q2_p1, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + b1)))

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric3)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + b1))
Xt_t_student_asymmetric_garch3_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_asymmetric3, q, p)

modello[['simulazione']][['garch_q2_p1']][['asimmetrico']] <- append(modello[['simulazione']][['garch_q2_p1']][['asimmetrico']], list('3'=list('Xt'=Xt_t_student_asymmetric_garch3_q2_p1, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + b1)))

# creo il primo plot del modello Garch(2,1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch1_q2_p1, index = seq_along(Xt_t_student_asymmetric_garch1_q2_p1)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Garch(2,1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric1 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch2_q2_p1, index = seq_along(Xt_t_student_asymmetric_garch2_q2_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Garch(2,1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric2 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch3_q2_p1, index = seq_along(Xt_t_student_asymmetric_garch3_q2_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# Grid plot
# creo la figura utilizzando una griglia con i tre plot generati
type_dist = "Asymmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))
subtitle_content <- (paste("path length ", (samples), " sample points"))
plots_dist_t_student_asymmetric1 <- plot_grid(plot_t_student_asymmetric, plot_t_student_asymmetric1, plot_t_student_asymmetric2, nrow = 3)
plots <- plots_dist_t_student_asymmetric1 +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=author_content) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, size = 10),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(plots)

###########################################################################################################
"Costruisco un processo GARCH per ogni distribuzione considerando q=2 e p=2 (solo per il modello GARCH)"

q <- 2
p <- 2 # utilizzato solo per Garch

############################################## " MODELLO GARCH "
modello[['simulazione']] <- append(modello[['simulazione']], list('garch_q2_p2'=list('a0'=a0, 'a1'=aq[1], 'a2'=aq[2], 'b1'=bp[1], 'b2'=bp[2], 'q'=q, 'p'=p)))
type_model = substitute(paste0("Model GARCH(", q, ",", p, ")"))

##########  DISTRUBUZIONE 
modello[['simulazione']][['garch_q2_p2']] <- append(modello[['simulazione']][['garch_q2_p2']], list('normale'=list()))

# Prima traiettoria
sigmasquaredW <- var(dist_normal1)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2]))
Xt_normal_garch1_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_normal1, q, p)

modello[['simulazione']][['garch_q2_p2']][['normale']] <- append(modello[['simulazione']][['garch_q2_p2']][['normale']], list('1'=list('Xt'=Xt_normal_garch1_q2_p2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])))

# Seconda traiettoria
sigmasquaredW <- var(dist_normal2)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2]))
Xt_normal_garch2_q2_p2<- model_garch(a0, aq, bp, X0, sigmasquared0, dist_normal2, q, p)

modello[['simulazione']][['garch_q2_p2']][['normale']] <- append(modello[['simulazione']][['garch_q2_p2']][['normale']], list('2'=list('Xt'=Xt_normal_garch2_q2_p2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])))

# Terza traiettoria
sigmasquaredW <- var(dist_normal3)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2]))
Xt_normal_garch3_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_normal3, q, p)

modello[['simulazione']][['garch_q2_p2']][['normale']] <- append(modello[['simulazione']][['garch_q2_p2']][['normale']], list('3'=list('Xt'=Xt_normal_garch3_q2_p2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])))

# creo il primo plot del modello Garch(2,2) con la distribuzione normale
plot_dist_normal1 <- ggplot(data.frame(value = Xt_normal_garch1_q2_p2, index = seq_along(Xt_normal_garch1_q2_p2)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Garch(2,2) con la distribuzione normale
plot_dist_normal2 <- ggplot(data.frame(value = Xt_normal_garch2_q2_p2, index = seq_along(Xt_normal_garch2_q2_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Garch(2,2) con la distribuzione normale
plot_dist_normal3 <- ggplot(data.frame(value = Xt_normal_garch3_q2_p2, index = seq_along(Xt_normal_garch3_q2_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# Grid plot
# creo la figura utilizzando una griglia con i tre plot generati
type_dist = "Normal Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)), " of a ", .(type_dist))))
subtitle_content <- (paste("path length ", (samples), " sample points"))
plots_dist_norm <- plot_grid(plot_dist_normal1, plot_dist_normal2, plot_dist_normal3, nrow = 3)
plots <- plots_dist_norm +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=author_content) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, size = 10),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(plots)

##########  DISTRUBUZIONE T-STUDENT  SIMMETRICA
modello[['simulazione']][['garch_q2_p2']] <- append(modello[['simulazione']][['garch_q2_p2']], list('simmetrico'=list()))

# Prima traiettoria
sigmasquaredW <- var(dist_t_student_symmetric1)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2]))
Xt_t_student_symmetric_garch1_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_symmetric1, q, p)

modello[['simulazione']][['garch_q2_p2']][['simmetrico']] <- append(modello[['simulazione']][['garch_q2_p2']][['simmetrico']], list('1'=list('Xt'=Xt_t_student_symmetric_garch1_q2_p2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])))

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_symmetric2)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2]))
Xt_t_student_symmetric_garch2_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_symmetric2, q, p)

modello[['simulazione']][['garch_q2_p2']][['simmetrico']] <- append(modello[['simulazione']][['garch_q2_p2']][['simmetrico']], list('2'=list('Xt'=Xt_t_student_symmetric_garch2_q2_p2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])))

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_symmetric3)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2]))
Xt_t_student_symmetric_garch3_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_symmetric3, q, p)
Xt_t_student_symmetric_garch3_q2_p2

modello[['simulazione']][['garch_q2_p2']][['simmetrico']] <- append(modello[['simulazione']][['garch_q2_p2']][['simmetrico']], list('3'=list('Xt'=Xt_t_student_symmetric_garch3_q2_p2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])))

type_dist = "Symmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric <- ggplot(data.frame(value = Xt_t_student_symmetric_garch1_q2_p2, index = seq_along(Xt_t_student_symmetric_garch1_q2_p2)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric1 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch2_q2_p2, index = seq_along(Xt_t_student_symmetric_garch2_q2_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric2 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch3_q2_p2, index = seq_along(Xt_t_student_symmetric_garch3_q2_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_t_student_symmetric1 <- plot_grid(plot_t_student_symmetric, plot_t_student_symmetric1, plot_t_student_symmetric2, nrow = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
garch_plot_dist_t_student_symmetric1 <- grid.arrange(title_content_gtable, plots_dist_t_student_symmetric1, ncol = 1, heights = c(0.2, 1))

##########  DISTRUBUZIONE T-STUDENT  ASIMMETRICA
modello[['simulazione']][['garch_q2_p2']] <- append(modello[['simulazione']][['garch_q2_p2']], list('asimmetrico'=list()))

# Prima traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric1)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2]))
Xt_t_student_asymmetric_garch1_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)

modello[['simulazione']][['garch_q2_p2']][['asimmetrico']] <- append(modello[['simulazione']][['garch_q2_p2']][['asimmetrico']], list('1'=list('Xt'=Xt_t_student_asymmetric_garch1_q2_p2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])))

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric2)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2]))
Xt_t_student_asymmetric_garch2_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_asymmetric2, q, p)

modello[['simulazione']][['garch_q2_p2']][['asimmetrico']] <- append(modello[['simulazione']][['garch_q2_p2']][['asimmetrico']], list('2'=list('Xt'=Xt_t_student_asymmetric_garch2_q2_p2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])))

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric3)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2]))
Xt_t_student_asymmetric_garch3_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_asymmetric3, q, p)

modello[['simulazione']][['garch_q2_p2']][['asimmetrico']] <- append(modello[['simulazione']][['garch_q2_p2']][['asimmetrico']], list('3'=list('Xt'=Xt_t_student_asymmetric_garch3_q2_p2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])))

# creo il primo plot del modello Garch(2,2) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch1_q2_p2, index = seq_along(Xt_t_student_asymmetric_garch1_q2_p2)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Garch(2,2) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric1 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch2_q2_p2, index = seq_along(Xt_t_student_asymmetric_garch2_q2_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Garch(2,2) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric2 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch3_q2_p2, index = seq_along(Xt_t_student_asymmetric_garch3_q2_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# Grid plot
# creo la figura utilizzando una griglia con i tre plot generati
type_dist = "Asymmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))
subtitle_content <- (paste("path length ", (samples), " sample points"))
plots_dist_t_student_asymmetric1 <- plot_grid(plot_t_student_asymmetric, plot_t_student_asymmetric1, plot_t_student_asymmetric2, nrow = 3)
plots <- plots_dist_t_student_asymmetric1 +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=author_content) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5, size = 10),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(plots)

#############################################################################################################
#############################################################################################################

########## Test Statistici & Stima dei parametri
"Per verificare la correttezza del modello si studiano:
- Omoschedasticità:  significa che i residui sono equamente distribuiti lungo la linea di regressione;
- Assenza di autocorrelazione: si verifica quando i residui non sono indipendenti l'uno dall'altro;
- Stazionarietà**: un modello di predizione con residui stazionari garantisce che le previsioni future
    siano affidabili e non influenzate da fluttuazioni casuali o tendenze temporali;
    
Non studiamo i casi di:
- Gaussianità: se i residui seguono una distribuzione normale;

I test computazionali che vengono solitamente applicati per rilevare la stazionarietà dei residui, 
sono il test di Augmented Dickey-Fuller (ADF) e il test di Kwiatowski-Phillips-Schmidt-Shin (KPSS). 
Il test ADF assume l'ipotesi nulla di non stazionarietà; assume che la serie temporale sia generata 
da un processo stocastico con un componente di random walk.
Al contrario, il test KPSS assume l'ipotesi nulla di stazionarietà; assume che la serie temporale 
sia generata da un processo autoregressivo. 
- Quando il test ADF respinge l'ipotesi nulla e KPSS no, abbiamo prove di stazionarietà 
nella serie temporale. 
- Quando il test ADF non respinge l'ipotesi nulla e KPSS sì, abbiamo prove di non stazionarietà. 

Il test computazionale che viene utilizzato per rilevare che la seria sia normalmente
distribuito è il test di Shapiro-Wilk (SW).
Se il p-value del test è maggiore di a=0.05 allora la serie è normalmente distribuito.

I test computazionali che vengono solitamente applicati per rilevare l'eteroschedasticità nelle serie 
temporali sono i test di Breusch-Pagan (BP) e White (W). 
Si ha: 
- ipotesi nulla: l'omoschedasticità è presente (i residui sono distributii con una uguale varianza);
- ipotesi alternativa: l'eteroschedasticità è presente ( i residui non sono distribuiti con una varianze uguali).
Se il p-value del test è minore di 0.05 allora possiamo rigettare l'ipotesi nulla e 
concludere che l'eteroschedasticità è presente.

I test computazionali che vengono solitamente applicati per rilevare assenza di autocorrelazione 
nelle serie temporali sono i test di Ljiung-box. 
Si ha: 
- ipotesi nulla: assenza di autocorrelazione (i residui del modello sono indipendentemente distribuiti);
- ipotesi alternativa: presenza di autocorrelazione (i residui del modello non sono indipendentemente distribuiti).
Se il p-value del test è minore di 0.05 allora possiamo rigettare l'ipotesi nulla e 
concludere che che ci sia presenza di autocorrelazione nei residui del modello.

Se la serie non presenta eteroschedasticità e non ha assenza di autocorrelazione, stimiamo i parametri che si adattano meglio
al modello tenendo in considerazione il valore AIC( Aikaike Information Criterion) e il valore del p-value del test di Ljung-Box
per garantire assenza di autocorrelazione."

##########################################

########## MODELLO ARCH(1)
modello[['stimati']] <- append(modello[['stimati']], list('arch_q1'=list()))

##### DISTRIBUZIONE NORMALE
# Consideriamo una traiettoia con distribuzione normale di un modello ARCH(1)
Xt <- modello[['simulazione']][['arch_q1']][['normale']][['1']][['Xt']]
df_Xt_normal_arch1_q1 <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df<- df_Xt_normal_arch1_q1
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\"  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(1) with a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_arch1_q1_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_arch1_q1_sp)
# La regression line è orizzontale, quindi, questo indica che non si ha la presenza di un trend all'interno della serie;
# mentre, la LOESS oscilla leggermente intorno alla linea orizzontale, ma potremmo dire di avere stazionarietà nella serie.

# Consideriamo un modello lineare
Xt_normal_arch1_q1_lm <- lm(Xt~t, data=df_Xt_normal_arch1_q1)
summary(Xt_normal_arch1_q1_lm)
summary(Xt_normal_arch1_q1_lm$fitted.values)

Xt_normal_arch1_q1_res <- Xt_normal_arch1_q1_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- DescTools::Skew(Xt_normal_arch1_q1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew
#       skew      lwr.ci      upr.ci 
# -0.08678682 -0.24846259  0.04388842 
set.seed(123)
kurt  <- DescTools::Kurt(Xt_normal_arch1_q1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
kurt
# Intervallo di confidenza 80%
#      kurt     lwr.ci     upr.ci 
# -0.1544153 -0.3911550  0.2348151 
# Intervallo di confidenza 99%
#      kurt     lwr.ci     upr.ci 
# -0.1544153 -0.5565872  0.7696437 

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_normal_arch1_q1_cf <- descdist(Xt, discrete=FALSE, boot=500)
# Dal grafico notiamo che le osservazioni seguono una distribuzine normale.

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare.
y <- Xt_normal_arch1_q1_res
num_lags <- 3                  # Setting the lag parameter for the test.
Xt_normal_arch1_q1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_normal_arch1_q1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_normal_arch1_q1_res   
Xt_normal_arch1_q1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_normal_arch1_q1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_arch1_q1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(1) with a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_normal_arch1_q1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_arch1_q1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_arch1_q1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals with the model Arch(1) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_arch1_q1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_arch1_q1_sp)

# Test Shamiro-Wilk 
Xt_normal_arch1_q1_sw <- shapiro.test(Xt)
show(Xt_normal_arch1_q1_sw)
# Si ha un p-value maggiore di 0.05, quindi, non possiamo rigettare lìipotesi nulla che i dati seguano una distribuzione normale. 
# I dati sembrano seguire una distribuzione normale.

plot(Xt_normal_arch1_q1_lm,1) # Residuals vs Fitted
plot(Xt_normal_arch1_q1_lm,2) # Q-Q Residuals
plot(Xt_normal_arch1_q1_lm,3) # Scale-location

# Omoschedasticità
# Test BREUSCH-PAGAN sui residui del modello lineare
t <-1:length(Xt)
Xt_normal_arch1_q1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = FALSE, data=df_Xt_normal_arch1_q1)
show(Xt_normal_arch1_q1_bp)
# Si ha un p-value di 0.008681 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_arch1_q1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = FALSE, data=df_Xt_normal_arch1_q1)
show(Xt_normal_arch1_q1_w)
# Si ha un p-value di 0.03002 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_normal_arch1_q1_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model ARCH(1)")))
subtitle_content <- bquote(paste("Normal distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano sempre entro 
# una banda ristretta, quindi, possiamo dire che che la serie non è significativamente 
# correlata con le serie ritardate.

# Test Ljiung-box
y <- Xt_normal_arch1_q1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 3.7866, df = 1, p-value = 0.05166
# Si ha un p-value > 0.05, quindi non è possibile rifiutare l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 3  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_normal_arch1_q1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_normal_arch1_q1_lb)
# I ritardi da 1 a 4 hanno il p-value leggermente superiore a 0.05, ma vicino a questo valore. 
# A partire dal ritardo 5, il p-value sembra superare 0.05, suggerendo che non c'è autocorrelazione significativa 
# nei residui a partire da quel punto.

modello[['simulazione']][['arch_q1']][['normale']][['1']] <- append(modello[['simulazione']][['arch_q1']][['normale']][['1']], 
                                                                    list('lm'=Xt_normal_arch1_q1_lm, 'skew'=skew, 'kurt'=kurt, 'Cullen-Frey'=Xt_normal_arch1_q1_cf, 
                                                                         'Breusch-Pagan'=Xt_normal_arch1_q1_bp, 'White'=Xt_normal_arch1_q1_w, 
                                                                         'Ljiung-Box'=Xt_normal_arch1_q1_lb, 'Dickey-Fuller'=Xt_normal_arch1_q1_adf, 
                                                                         'Kwiatowski-Phillips-Schmidt-Shin'=Xt_normal_arch1_q1_kpss, 'Shapiro-Wilk'=Xt_normal_arch1_q1_sw))

# Possiamo concludere che la prima traiettoria della distribuzione normale di un modello ARCH(1) ha evidenza di eteroschedasticità
# e assenza di autocorrelazione nei residui del modello.

# Proviamo a stimare i parametri del modello:
q <- 1
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,0)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']] 
sigmasquaredW <- var(dist_normal1)
stazionaietà <- a1*sigmasquaredW
print(paste("Verifico condizione di stazionerietà: ", stazionaietà))
Xt_normal_arch1_q1_new <- model_arch(a0, a1, X0, dist_normal1, q)

Xt <- Xt_normal_arch1_q1_new
df_Xt_normal_arch1_q1 <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df<- df_Xt_normal_arch1_q1
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\"  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(1) with a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_arch1_q1_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_arch1_q1_sp)

# Consideriamo un modello lineare
Xt_normal_arch1_q1_lm <- lm(Xt~t, data=df_Xt_normal_arch1_q1)
summary(Xt_normal_arch1_q1_lm)
summary(Xt_normal_arch1_q1_lm$fitted.values)

Xt_normal_arch1_q1_res <- Xt_normal_arch1_q1_lm$residuals

skew <- DescTools::Skew(Xt_normal_arch1_q1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew          lwr.ci      upr.ci 
# -0.04892516 -0.15903750  0.05665529 
kurt  <- DescTools::Kurt(Xt_normal_arch1_q1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
show(kurt)
# Livello di significatività 5%
#      kurt      lwr.ci      upr.ci 
# -0.32278623 -0.55014759  0.03314538 
# Livello di significatività1%
#      kurt      lwr.ci      upr.ci 
# -0.3227862 -0.6138619   0.2428574 

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_normal_arch1_q1_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_normal_arch1_q1_res
num_lags <- 3                  # Setting the lag parameter for the test.
Xt_normal_arch1_q1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_normal_arch1_q1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_normal_arch1_q1_res   
Xt_normal_arch1_q1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_normal_arch1_q1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_arch1_q1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(1) with a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_normal_arch1_q1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_arch1_q1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_arch1_q1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals with the model Arch(1) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_arch1_q1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_arch1_q1_sp)

# Test Shapiro-Wilk
Xt_normal_arch1_q1_sw <- shapiro.test(Xt)
show(Xt_normal_arch1_q1_sw)
# Si ha un p-value maggiore di 0.05, quindi, non possiamo rigettare l'ipotesi nulla che i dati seguano una distribuzione normale.
# I dati sembrano seguire una distribuzione normale.

plot(Xt_normal_arch1_q1_lm,1) # Residuals vs Fitted
plot(Xt_normal_arch1_q1_lm,2) # Q-Q Residuals
plot(Xt_normal_arch1_q1_lm,3) # Scale-location

# Omoschedasticità
# Test BREUSCH-PAGAN sui residui del modello lineare
t <-1:length(Xt)
Xt_normal_arch1_q1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = FALSE, data=df_Xt_normal_arch1_q1)
show(Xt_normal_arch1_q1_bp)
# Si ha un p-value di 0.02537 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_arch1_q1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = FALSE, data=df_Xt_normal_arch1_q1)
show(Xt_normal_arch1_q1_w)
# Si ha un p-value di 0.07878 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_normal_arch1_q1_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model ARCH(1)")))
subtitle_content <- bquote(paste("Normal distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano sempre entro 
# una banda ristretta, quindi, possiamo dire che che la serie non è significativamente 
# correlata con le serie ritardate.

# Test Ljiung-box
y <- Xt_normal_arch1_q1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 3.7866, df = 1, p-value = 0.05166
# Si ha un p-value > 0.05, quindi non è possibile rifiutare l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 3  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10,T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_normal_arch1_q1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_normal_arch1_q1_lb)
#  I primi ritardi non mostrano autocorrelazione significativa nei residui, ma a partire dal ritardo 5, 
# potrebbe esserci una certa autocorrelazione. 

modello[['stimati']][['arch_q1']] <- append(modello[['stimati']][['arch_q1']], 
                                            list('normale'=list('Xt'=Xt_normal_arch1_q1_new, 'a0'=a0, 'a1'=a1, 'q'=q, 'stazionarietà'=stazionaietà, 
                                                               'lm'=Xt_normal_arch1_q1_lm, 'skew'=skew, 'kurt'=kurt, 'Cullen-Frey'=Xt_normal_arch1_q1_cf, 
                                                               'Breusch-Pagan'=Xt_normal_arch1_q1_bp, 'White'=Xt_normal_arch1_q1_w, 
                                                               'Ljiung-Box'=Xt_normal_arch1_q1_lb, 'Dickey-Fuller'=Xt_normal_arch1_q1_adf, 
                                                               'Kwiatowski-Phillips-Schmidt-Shin'=Xt_normal_arch1_q1_kpss, 'Shapiro-Wilk'=Xt_normal_arch1_q1_sw)))

# Possiamo concludere che la prima traiettoria della distribuzione normale di un modello ARCH(1) ha evidenza di eteroschedasticità
# nel test di Breusch-Pagan e presenza di omochedasticità nel test di White
# e assenza di autocorrelazione nei residui del modello.
##########################################

##### DISTRIBUZIONE T-STUDENT SIMMETRICA
# Consideriamo la prima traiettoia con distribuzione t-student simmetrica di un modello ARCH(1)
Xt <- Xt_t_student_symmetric_arch1_q1
df_Xt_t_student_symmetric_arch1_q1 <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df<- df_Xt_t_student_symmetric_arch1_q1
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1],Data_df$t[1])
Last_Day <- paste(Data_df$t[length],Data_df$t[length])
title_content <- bquote(atop(.(content), paste("Line Plot of Model Arch(1)")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_arch1_q1_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_arch1_q1_sp)
# La regression line è orizzontale, quindi, questo indica che non si ha la presenza di un trend all'interno della serie;
# mentre, la LOESS corrisponde quasi esattamente alla regression line, potremmo dire di avere stazionarietà nella serie.

# Consideriamo un modello lineare
Xt_t_student_symmetric_arch1_q1_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_arch1_q1)
summary(Xt_t_student_symmetric_arch1_q1_lm)
summary(Xt_t_student_symmetric_arch1_q1_lm$fitted.values)

Xt_t_student_symmetric_arch1_q1_res <- Xt_t_student_symmetric_arch1_q1_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- skew <- DescTools::Skew(Xt_t_student_symmetric_arch1_q1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew          lwr.ci      upr.ci 
# 0.19494181 -0.08884189  0.52093593
set.seed(123)
kurt <- kurt <- DescTools::Kurt(Xt_t_student_symmetric_arch1_q1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
show(kurt)
#      kurt      lwr.ci      upr.ci 
# 1.8612384 0.6592552 3.6767636 
# All'1% di significatività l'eccesso di kurtosi non è zero. 

Xt_t_student_symmetric_arch1_q1_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=500)
Xt_t_student_symmetric_arch1_q1_cf <- append(Xt_t_student_symmetric_arch1_q1_cf, list(cf))
show(cf)
# summary statistics
------
#  min:  -3.262066   max:  3.37251 
# median:  0.001791954 
# mean:  -0.002615934 
# estimated sd:  0.763665 
# estimated skewness:  0.1949418 
# estimated kurtosis:  4.861238 
# Conferma visiva che l'ipotesi nulla di normalità bisogna scartarla all'1% di significatività 
# quindi possiamo dire che è una distribuzione simmetrica con una forte kurtosi.

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_symmetric_arch1_q1_res
show(c(mean(y),var(y)))
# [1] -8.542537e-18  5.829699e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] -1.086989e-17  1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, graph=TRUE, boot=500)
Xt_t_student_symmetric_arch1_q1_cf <- append(Xt_t_student_symmetric_arch1_q1_cf, list(cf)) 
show(cf)
# summary statistics
------
# min:  -4.266144   max:  4.59034 
# median:  0.004906486 
# mean:  7.984715e-18 
# estimated sd:  1 
# estimated skewness:  0.2004542 
# estimated kurtosis:  4.955997 
# Da Cullen-Frey, abbiamo una possibile distribuzione logistica o di student per i residui.
  
# Esploriamo queste possibilità in più dettagli.
z_st_qemp <- EnvStats::qemp(stats::ppoints(z_st), z_st) # Empirical quantiles of the residuals.
z_st_demp <- EnvStats::demp(z_st_qemp, z_st)     # Empirical probability density of the residuals.
z_st_pemp <- EnvStats::pemp(z_st_qemp, z_st)     # Empirical distribution function of the residuals.  
x <- z_st_qemp
y_d <- z_st_demp
y_p <- z_st_pemp
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=3, e la funzione di densità logistica generalizzata con il parametri location=0,
# scale=sqrt(3)/pi e shape=1.
loc <- 0
shp <- 1
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the ARCH(1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dt(x, df=3), lwd=2, col= "red")
lines(x, dglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", "Student Density Function, df=3", Gen_Log_Dens_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
loc <- 0
shp <- 1
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the ARCH(1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pt(x, df=3), lwd=2, col= "red")
lines(x, pglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=3", Gen_Log_Distr_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

fitdist_test <- list()
# Distribuzione logistica generalizzata
# Come prima cosa, fittiamo la distribuzione logistica generalizzata utilizzando la funzione fitdistrplus::fitdist().
fitdist_glogis <- fitdistrplus::fitdist(z_st, "glogis", start=list(location=0, scale=sqrt(3)/pi, shape=1), method= "mle")
fitdist_test[["glogis"]][["glogis"]] <- fitdist_glogis
summary(fitdist_glogis)
# Fitting of the distribution ' glogis ' by maximum likelihood 
# Parameters : 
#           estimate Std. Error
# location -0.07964129 0.14169334
# scale     0.55665568 0.03669895
# shape     1.08914075 0.17787739
# Loglikelihood:  -695.0136   AIC:  1396.027   BIC:  1408.671 
# Correlation matrix:
#            location      scale      shape
# location  1.0000000 -0.7926341 -0.9554749
# scale    -0.7926341  1.0000000  0.8243698
# shape    -0.9554749  0.8243698  1.0000000

# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_glogis_bd <- fitdistrplus::bootdist(fitdist_glogis, niter=1000)
fitdist_test[["glogis"]][["uncertainty"]] <- fitdist_glogis_bd
summary(fitdist_glogis_bd)
# Parametric bootstrap medians and 95% percentile CI 
# Median       2.5%     97.5%
#   location -0.0889993 -0.4299843 0.1810415
# scale     0.5549285  0.4836019 0.6360499
# shape     1.0993132  0.7864985 1.6373604
# We fix the initial points of the constrained maximization procedure
location <- fitdist_glogis_bd[["fitpart"]][["estimate"]][1]
scale <- fitdist_glogis_bd[["fitpart"]][["estimate"]][2]
shape <- fitdist_glogis_bd[["fitpart"]][["estimate"]][3]
minus_logLik <- function(x) -sum(log(dglogis(z_st, location=x[1], scale=x[2], shape=x[3]))) # the log-likelihood of the generalized logistic
# distribution.
fminunc_result <- fminunc(x0=c(location, scale, shape), fn=minus_logLik)   # the minimization procedure where (location,scale,shape) is the 
# starting point.
show(c(fminunc_result$par[1],fminunc_result$par[2],fminunc_result$par[3])) # the estimated parameters.
#    location       scale       shape 
# -0.08271232  0.55777995  1.09281598

# Setting
location <- fitdist_glogis[["estimate"]][["location"]]
scale <- fitdist_glogis[["estimate"]][["scale"]]
shape <- fitdist_glogis[["estimate"]][["shape"]]
# We plot the histogram and the empirical density function of the standardized residuals together with the density function of the estimated 
# generalized logistic.
loc <- round(location,4)
scl <- round(scale,4)
shp <- round(shape,4)
show(c(loc,scl,shp))
# -0.0796  0.5567  1.0891
Est_Gen_Log_Dens_Func <- bquote(paste("Estimated Generalized Logistic Density Function, location = ", .(loc), ", scale = ", .(scl) , ", shape = ", .(shp)))
hist(z_st, breaks= "Scott", col= "green", border= "black", xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), 
     freq=FALSE, main= "Density Histogram of the Standardized Residuals of the GARCH(1,1) model + Empirical Density Function + Estimated Generalized Logistic Density", xlab= "Standardized Residuals", ylab= "Density")
lines(density(z_st), lwd=2, col= "darkgreen")
lines(x, dglogis(x, location=location, scale=scale, shape=shape), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", Est_Gen_Log_Dens_Func), 
       col=c("darkgreen", "magenta"),
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

# Dato che la Generalized Student distribution ha il parametro location che coincide con la media, possiamo provare 
# a fittare i dati con una generalized Student distribution con zero location. 
# Questo viene fatto cambiando il parametro m nella funzione *fitdistrplus::fitdist*.
fitdist_t_ls <- fitdistrplus::fitdist(z_st, dt_ls, start=list(s=sqrt(1/3), df=3), fix.arg=list(m=0), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' t_ls ' by maximum likelihood 
# Parameters : 
#   estimate Std. Error
# s  0.811707   0.04317391
# df 5.7769418  1.42531533
# Fixed parameters:
#     value
# m     0
# Loglikelihood:  -694.9214   AIC:  1393.843   BIC:  1402.272 
#Correlation matrix:
#       s       df
# s  1.000000 0.680783
# df 0.680783 1.000000

# Alla fine, consideriamo il test Goodness of fit.
# The Kolmogorov-Smirnov test in the library *stats*
loc <- round(fitdist_glogis[["estimate"]][["location"]],4)
scl <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shp <- round(fitdist_glogis[["estimate"]][["shape"]],4)
KS_z_st_glogis <- ks.test(z_st, y="pglogis", location=loc, scale=scl, shape=shp, alternative= "two.sided")
fitdist_test[["glogis"]][["Kolmogorov-Smirnov"]] <- KS_z_st_glogis
show(KS_z_st_glogis)
# 	Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.017578, p-value = 0.9978
# alternative hypothesis: two-sided
# Con un p-value cosi alto, non possiamo rigettare l'ipotesi nulla. Di conseguenza, abbiamo una prova sufficiente
# che la serie è una distribuzione logistica generalizzata.
m <- 0
s <- round(fminunc_result$par[1],4)
df <- round(fminunc_result$par[2],4)
KS_z_st_t_ls <- stats::ks.test(z_st, y="pt_ls", m=0, s=s, df=df, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.96561, p-value < 2.2e-16
# alternative hypothesis: two-sided
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# The Cramer-Von Mises test in the library *goftest*.
# This function performs the Cramer-Von Mises test of goodness-of-fit to the distribution specified by the argument null. It is assumed that the 
# values in x are independent and identically distributed random values, with some cumulative distribution function F. The null hypothesis is 
# that F is the function specified by the argument null, while the alternative hypothesis is that F is some other function.
loc <- round(fitdist_glogis[["estimate"]][["location"]],4)
scl <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shp <- round(fitdist_glogis[["estimate"]][["shape"]],4)
CVM_z_st_glogis <- goftest::cvm.test(z_st, null= "pglogis", location=loc, scale=scl, shape=shp, estimated=FALSE)
fitdist_test[["glogis"]][["Cramer-Von Mises"]] <- CVM_z_st_glogis 
show(CVM_z_st_glogis)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘pglogis’
# with parameters location = -0.0796, scale = 0.5567, shape = 1.0891
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.018845, p-value = 0.998
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
s <- round(fminunc_result$par[1],4)
df <- round(fminunc_result$par[2],4)
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "pt_ls", m=0, s=s, df=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘pt_ls’
# with parameters m = 0, s = -0.0827, df = 0.5578
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 9.9693, p-value < 2.2e-16
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.
loc <- round(fitdist_glogis[["estimate"]][["location"]],4)
scl <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shp <- round(fitdist_glogis[["estimate"]][["shape"]],4)
AD_z_st_glogis <- goftest::ad.test(z_st, null= "pglogis", location=location, scale=scale, shape=shape, estimated=FALSE)
fitdist_test[["glogis"]][["Anderson-Darling"]] <- AD_z_st_glogis
show(AD_z_st_glogis)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘pglogis’
# with parameters location = -0.0796412857701351, scale = 0.556655675177367, shape = 1.0891407461961
# Parameters assumed to be fixed
# data:  z_st
# An = 0.17549, p-value = 0.9957
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
s <- round(fminunc_result$par[1],4)
df <- round(fminunc_result$par[2],4)
AD_z_st_t_ls <- goftest::ad.test(z_st, "pt_ls", m=0, s=s, df=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_symmetric_arch1_q1_res
num_lags <- 3                   # Setting the lag parameter for the test.
Xt_t_student_symmetric_arch1_q1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_symmetric_arch1_q1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_symmetric_arch1_q1_res   
Xt_t_student_symmetric_arch1_q1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_symmetric_arch1_q1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X =Xt_t_student_symmetric_arch1_q1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(1) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_symmetric_arch1_q1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_arch1_q1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_arch1_q1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Arch(1) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_arch1_q1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_arch1_q1_sp)

# Test Shapiro-Wilk
Xt_t_student_symmetric_arch1_q1_sw <- shapiro.test(Xt)
show(Xt_t_student_symmetric_arch1_q1_sw)
# Si ha un p-value minore di 0.05, quindi possiamo rigettare l'ipotesi nulla e
# dire che la serie non è normalmente distribuito.

plot(Xt_t_student_symmetric_arch1_q1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_arch1_q1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_arch1_q1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_arch1_q1)
show(Xt_t_student_symmetric_arch1_q1_bp)
# Si ha un p-value di 0.001644 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_arch1_q1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_arch1_q1)
show(Xt_t_student_symmetric_arch1_q1_w)
# Si ha un p-value di 0.007024 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_arch1_q1_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model ARCH(1)")))
subtitle_content <- bquote(paste("t-student symmetric distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano sempre entro 
# una banda ristretta, quindi, possiamo dire che che la serie non è significativamente 
# correlata con le serie ritardate.

# Test Ljiung-box
y <- Xt_t_student_symmetric_arch1_q1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 0.47606, df = 1, p-value = 0.4902
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 3  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_symmetric_arch1_q1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_symmetric_arch1_q1_lb)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag considerati.

modello[['simulazione']][['arch_q1']][['simmetrico']][['1']] <- append(modello[['simulazione']][['arch_q1']][['simmetrico']][['1']], 
                                                                       list('lm'=Xt_t_student_symmetric_arch1_q1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                            'Cullen-Frey'=Xt_t_student_symmetric_arch1_q1_cf, 
                                                                            'Breusch-Pagan'=Xt_t_student_symmetric_arch1_q1_bp,'White'=Xt_t_student_symmetric_arch1_q1_w, 
                                                                            'Ljiung-Box'=Xt_t_student_symmetric_arch1_q1_lb, 'Dickey-Fuller'=Xt_t_student_symmetric_arch1_q1_adf, 
                                                                            'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_arch1_q1_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_arch1_q1_sw,
                                                                            'Generalized Distribution'=fitdist_test))

# In questo modello Arch(1) con una distribuzione t-student simmetrica si ha evidenza di eteroschedasticità nella serie
# e assenza di autocorrelazione nei residui.

# Stimiamo i parametri del modello:
q <- 1
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,0)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']] 
sigmasquaredW <- var(dist_t_student_symmetric1)
stazionarietà <- a1*sigmasquaredW
print(paste("Verifico condizione di stazionerietà: ", stazionarietà))
Xt_t_student_symmetric_arch1_q1_new <- model_arch(a0, a1, X0, dist_t_student_symmetric1, q)

Xt <- Xt_t_student_symmetric_arch1_q1_new
df_Xt_t_student_symmetric_arch1_q1 <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df<- df_Xt_t_student_symmetric_arch1_q1
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1],Data_df$t[1])
Last_Day <- paste(Data_df$t[length],Data_df$t[length])
title_content <- bquote(atop(.(content), paste("Line Plot of Model Arch(1)")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_arch1_q1_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_arch1_q1_sp)
# La regression line è orizzontale, quindi, questo indica che non si ha la presenza di un trend all'interno della serie;
# mentre, la LOESS corrisponde quasi esattamente alla regression line, potremmo dire di avere stazionarietà nella serie.

# Consideriamo un modello lineare
Xt_t_student_symmetric_arch1_q1_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_arch1_q1)
summary(Xt_t_student_symmetric_arch1_q1_lm)
summary(Xt_t_student_symmetric_arch1_q1_lm$fitted.values)

Xt_t_student_symmetric_arch1_q1_res <- Xt_t_student_symmetric_arch1_q1_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- skew <- DescTools::Skew(Xt_t_student_symmetric_arch1_q1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# 0.1949418 -0.1103381  0.4706468 
set.seed(123)
kurt <- kurt <- DescTools::Kurt(Xt_t_student_symmetric_arch1_q1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt      lwr.ci   upr.ci 
# 1.861238 1.196812 2.765069 

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_symmetric_arch1_q1_cf <- descdist(Xt, discrete=FALSE, boot=500)

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_symmetric_arch1_q1_res
show(c(mean(y),var(y)))
# [1] -8.444634e-18  2.704741e+00
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] -4.984077e-18  1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, graph=TRUE, boot=500)
Xt_t_student_symmetric_arch1_q1_cf <- append(Xt_t_student_symmetric_arch1_q1_cf, list(cf)) 
show(cf)
# summary statistics
------
#  min:  -4.462193   max:  3.578521 
# median:  -0.01590323 
# mean:  -4.984077e-18 
# estimated sd:  1 
# estimated skewness:  0.155173 
# estimated kurtosis:  4.596664 
  
# Esploriamo queste possibilità in più dettagli.
z_st_qemp <- EnvStats::qemp(stats::ppoints(z_st), z_st) # Empirical quantiles of the residuals.
z_st_demp <- EnvStats::demp(z_st_qemp, z_st)     # Empirical probability density of the residuals.
z_st_pemp <- EnvStats::pemp(z_st_qemp, z_st)     # Empirical distribution function of the residuals.  
x <- z_st_qemp
y_d <- z_st_demp
y_p <- z_st_pemp
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=3, e la funzione di densità logistica generalizzata con il parametri location=0,
# scale=sqrt(3)/pi e shape=1.
loc <- 0
shp <- 1
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the ARCH(1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dt(x, df=3), lwd=2, col= "red")
lines(x, dglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", "Student Density Function, df=3", Gen_Log_Dens_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
loc <- 0
shp <- 1
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the ARCH(1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pt(x, df=3), lwd=2, col= "red")
lines(x, pglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=3", Gen_Log_Distr_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

fitdist_test <- list()
# Distribuzione logistica generalizzata
# Come prima cosa, fittiamo la distribuzione logistica generalizzata utilizzando la funzione fitdistrplus::fitdist().
fitdist_glogis <- fitdistrplus::fitdist(z_st, "glogis", start=list(location=0, scale=sqrt(3)/pi, shape=1), method= "mle")
fitdist_test[["glogis"]][["glogis"]] <- fitdist_glogis
summary(fitdist_glogis)
# Fitting of the distribution ' glogis ' by maximum likelihood 
# Parameters : 
#           estimate Std. Error
# location -0.1060596 0.14770666
# scale     0.5659501 0.03745644
# shape     1.1215516 0.18804060
# Loglikelihood:  -697.4753   AIC:  1400.951   BIC:  1413.594 
# Correlation matrix:
#           location      scale      shape
# location  1.0000000 -0.7988681 -0.9583583
# scale    -0.7988681  1.0000000  0.8269470
# shape    -0.9583583  0.8269470  1.0000000

# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_glogis_bd <- fitdistrplus::bootdist(fitdist_glogis, niter=1000)
fitdist_test[["glogis"]][["uncertainty"]] <- fitdist_glogis_bd
summary(fitdist_glogis_bd)
# Parametric bootstrap medians and 95% percentile CI 
#             Median       2.5%     97.5%
# location -0.1175512 -0.4685861 0.1617938
# scale     0.5640942  0.4920352 0.6460453
# shape     1.1331028  0.8074557 1.6928413
# We fix the initial points of the constrained maximization procedure
location <- fitdist_glogis_bd[["fitpart"]][["estimate"]][1]
scale <- fitdist_glogis_bd[["fitpart"]][["estimate"]][2]
shape <- fitdist_glogis_bd[["fitpart"]][["estimate"]][3]
minus_logLik <- function(x) -sum(log(dglogis(z_st, location=x[1], scale=x[2], shape=x[3]))) # the log-likelihood of the generalized logistic
# distribution.
fminunc_result <- fminunc(x0=c(location, scale, shape), fn=minus_logLik)   # the minimization procedure where (location,scale,shape) is the 
# starting point.
show(c(fminunc_result$par[1],fminunc_result$par[2],fminunc_result$par[3])) # the estimated parameters.
#    location       scale       shape 
#   -0.1062089  0.5659415  1.1217658 

# Setting
location <- fitdist_glogis[["estimate"]][["location"]]
scale <- fitdist_glogis[["estimate"]][["scale"]]
shape <- fitdist_glogis[["estimate"]][["shape"]]
# We plot the histogram and the empirical density function of the standardized residuals together with the density function of the estimated 
# generalized logistic.
loc <- round(location,4)
scl <- round(scale,4)
shp <- round(shape,4)
show(c(loc,scl,shp))
# -0.1061  0.5660  1.1216
Est_Gen_Log_Dens_Func <- bquote(paste("Estimated Generalized Logistic Density Function, location = ", .(loc), ", scale = ", .(scl) , ", shape = ", .(shp)))
hist(z_st, breaks= "Scott", col= "green", border= "black", xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), 
     freq=FALSE, main= "Density Histogram of the Standardized Residuals of the GARCH(1,1) model + Empirical Density Function + Estimated Generalized Logistic Density", xlab= "Standardized Residuals", ylab= "Density")
lines(density(z_st), lwd=2, col= "darkgreen")
lines(x, dglogis(x, location=location, scale=scale, shape=shape), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", Est_Gen_Log_Dens_Func), 
       col=c("darkgreen", "magenta"),
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

# Dato che la Generalized Student distribution ha il parametro location che coincide con la media, possiamo provare 
# a fittare i dati con una generalized Student distribution con zero location. 
# Questo viene fatto cambiando il parametro m nella funzione *fitdistrplus::fitdist*.
fitdist_t_ls <- fitdistrplus::fitdist(z_st, dt_ls, start=list(s=sqrt(1/3), df=3), fix.arg=list(m=0), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' t_ls ' by maximum likelihood 
# Parameters : 
#   estimate Std. Error
# s  0.8279158 0.04380522
# df 6.2749723 1.66554550
# Fixed parameters:
#     value
# m     0
# Loglikelihood:  -697.4625   AIC:  1398.925   BIC:  1407.354 
#Correlation matrix:
#       s       df
# s  1.000000 0.687201
# df 0.687201 1.000000

# Alla fine, consideriamo il test Goodness of fit.
# The Kolmogorov-Smirnov test in the library *stats*
loc <- round(fitdist_glogis[["estimate"]][["location"]],4)
scl <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shp <- round(fitdist_glogis[["estimate"]][["shape"]],4)
KS_z_st_glogis <- ks.test(z_st, y="pglogis", location=loc, scale=scl, shape=shp, alternative= "two.sided")
fitdist_test[["glogis"]][["Kolmogorov-Smirnov"]] <- KS_z_st_glogis
show(KS_z_st_glogis)
# 	Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.018322, p-value = 0.9961
# alternative hypothesis: two-sided
# Con un p-value cosi alto, non possiamo rigettare l'ipotesi nulla. Di conseguenza, abbiamo una prova sufficiente
# che la serie è una distribuzione logistica generalizzata.
m <- 0
s <- round(fminunc_result$par[1],4)
df <- round(fminunc_result$par[2],4)
KS_z_st_t_ls <- stats::ks.test(z_st, y="pt_ls", m=0, s=s, df=df, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.96561, p-value < 2.2e-16
# alternative hypothesis: two-sided
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# The Cramer-Von Mises test in the library *goftest*.
# This function performs the Cramer-Von Mises test of goodness-of-fit to the distribution specified by the argument null. It is assumed that the 
# values in x are independent and identically distributed random values, with some cumulative distribution function F. The null hypothesis is 
# that F is the function specified by the argument null, while the alternative hypothesis is that F is some other function.
loc <- round(fitdist_glogis[["estimate"]][["location"]],4)
scl <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shp <- round(fitdist_glogis[["estimate"]][["shape"]],4)
CVM_z_st_glogis <- goftest::cvm.test(z_st, null= "pglogis", location=loc, scale=scl, shape=shp, estimated=FALSE)
fitdist_test[["glogis"]][["Cramer-Von Mises"]] <- CVM_z_st_glogis 
show(CVM_z_st_glogis)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘pglogis’
# with parameters location = -0.1061, scale = 0.566, shape = 1.1216
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.018152, p-value = 0.9984
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
s <- round(fminunc_result$par[1],4)
df <- round(fminunc_result$par[2],4)
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "pt_ls", m=0, s=s, df=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘pt_ls’
# with parameters m = 0, s = -0.1062, df = 0.5659
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 9.9693, p-value < 2.2e-16
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.
loc <- round(fitdist_glogis[["estimate"]][["location"]],4)
scl <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shp <- round(fitdist_glogis[["estimate"]][["shape"]],4)
AD_z_st_glogis <- goftest::ad.test(z_st, null= "pglogis", location=location, scale=scale, shape=shape, estimated=FALSE)
fitdist_test[["glogis"]][["Anderson-Darling"]] <- AD_z_st_glogis
show(AD_z_st_glogis)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘pglogis’
# with parameters location = -0.106059627135577, scale = 0.565950101781953, shape = 1.12155158016255
# Parameters assumed to be fixed
# data:  z_st
# An = An = 0.1698, p-value = 0.9965
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
s <- round(fminunc_result$par[1],4)
df <- round(fminunc_result$par[2],4)
AD_z_st_t_ls <- goftest::ad.test(z_st, "pt_ls", m=0, s=s, df=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘pt_ls’
# with parameters m = 0, s = -0.1062, df = 0.5659
# Parameters assumed to be fixed
# data:  z_st
# An = 50.215, p-value = 1.2e-06
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_symmetric_arch1_q1_res
num_lags <- 3                   # Setting the lag parameter for the test.
Xt_t_student_symmetric_arch1_q1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_symmetric_arch1_q1_adf)
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_symmetric_arch1_q1_res   
Xt_t_student_symmetric_arch1_q1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_symmetric_arch1_q1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X =Xt_t_student_symmetric_arch1_q1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(1) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_symmetric_arch1_q1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_arch1_q1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_arch1_q1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Arch(1) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_arch1_q1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_arch1_q1_sp)

#Test Shapiro-Wilk
Xt_t_student_symmetric_arch1_q1 <- shapiro.test(Xt)
show(Xt_t_student_symmetric_arch1_q1_sw)
# Si ha un p-value minore di 0.05, quindi possiamo rigettare l'ipotesi nulla e
# dire che la serie non è normalmente distribuito.

plot(Xt_t_student_symmetric_arch1_q1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_arch1_q1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_arch1_q1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_arch1_q1)
show(Xt_t_student_symmetric_arch1_q1_bp)
# Si ha un p-value di 0.002928 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_arch1_q1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_arch1_q1)
show(Xt_t_student_symmetric_arch1_q1_w)
# Si ha un p-value di 0.01187 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_arch1_q1_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model ARCH(1)")))
subtitle_content <- bquote(paste("t-student symmetric distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano sempre entro 
# una banda ristretta, quindi, possiamo dire che che la serie non è significativamente 
# correlata con le serie ritardate.

# Test Ljiung-box
y <- Xt_t_student_symmetric_arch1_q1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 0.47606, df = 1, p-value = 0.4902
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 3  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_symmetric_arch1_q1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_symmetric_arch1_q1_lb)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag considerati.

modello[['stimati']][['arch_q1']] <- append(modello[['stimati']][['arch_q1']], list('simmetrico'=list('Xt'=Xt_t_student_symmetric_arch1_q1_new, 'a0'=a0, 'a1'=a1, 'q'=q, 
                                                                                           'stazionarietà'=stazionaietà, 'lm'=Xt_t_student_symmetric_arch1_q1_lm, 
                                                                                           'skew'=skew, 'kurt'=kurt, 'Cullen-Frey'=Xt_t_student_symmetric_arch1_q1_cf, 
                                                                                           'Breusch-Pagan'=Xt_t_student_symmetric_arch1_q1_bp,'White'=Xt_t_student_symmetric_arch1_q1_w, 
                                                                                           'Ljiung-Box'=Xt_t_student_symmetric_arch1_q1_lb, 'Dickey-Fuller'=Xt_t_student_symmetric_arch1_q1_adf, 
                                                                                           'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_arch1_q1_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_arch1_q1_sw,                                                                             
                                                                                           'Generalized Distribution'=fitdist_test))

# In questo modello Arch(1) con una distribuzione t-student simmetrica si ha evidenza di eteroschedasticità nella serie
# e assenza di autocorrelazione nei residui.
##########################################

##### DSITRIBUZIONE T-STUDENT ASIMMETRICA

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello ARCH(1)
Xt <- Xt_t_student_asymmetric_arch1_q1
df_Xt_t_student_asymmetric_arch1_q1 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_asymmetric_arch1_q1_sw <- shapiro.test(Xt)
show(Xt_t_student_asymmetric_arch1_q1_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_asymmetric_arch1_q1
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Master in Data Science - Essentials of Time Series", paste("Line Plot of Model Arch(1)")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_arch1_q1_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_arch1_q1_sp)
# La regression line tende ad essere leggermente inclinata rispetto alla linea orizzontale, e 
# la LOESS corrisponde quasi esattamente alla regression line.

# Consideriamo un modello lineare
Xt_t_student_asymmetric_arch1_q1_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_arch1_q1)
summary(Xt_t_student_asymmetric_arch1_q1_lm)
summary(Xt_t_student_asymmetric_arch1_q1_lm$fitted.values)

Xt_t_student_asymmetric_arch1_q1_res <- Xt_t_student_asymmetric_arch1_q1_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- DescTools::Skew(Xt_t_student_asymmetric_arch1_q1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# -1.734758 -2.012437 -1.515962 
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_asymmetric_arch1_q1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt   lwr.ci   upr.ci 
#4.210131 3.024145 5.978218 

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_asymmetric_arch1_q1_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_arch1_q1_res
num_lags <- 3                 # Setting the lag parameter for the test.
Xt_t_student_asymmetric_arch1_q1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_arch1_q1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_asymmetric_arch1_q1_res   
Xt_t_student_asymmetric_arch1_q1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_arch1_q1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_arch1_q1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(1) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_asymmetric_arch1_q1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_arch1_q1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_arch1_q1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Arch(1) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Square root of absolute residuals")
Xt_t_student_asymmetric_arch1_q1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_arch1_q1_sp)

plot(Xt_t_student_asymmetric_arch1_q1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_arch1_q1_lm,3) # Scale-location

#Determiniamo se la serie è eteroschedastico:
# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_arch1_q1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_arch1_q1)
show(Xt_t_student_asymmetric_arch1_q1_bp)
# Si ha un p-value di 0.01372 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_arch1_q1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_arch1_q1)
show(Xt_t_student_asymmetric_arch1_q1_w)
# Si ha un p-value di 0.04648 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_asymmetric_arch1_q1_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model ARCH(1)")))
subtitle_content <- bquote(paste("t-student asymmetric distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano entro 
# una banda ristretta eccetto nel primo lag.

# Test Ljiung-box
y <- Xt_t_student_asymmetric_arch1_q1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 12.319, df = 1, p-value = 0.0004484
# I risultati mostrano un p-value < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 3  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_asymmetric_arch1_q1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_asymmetric_arch1_q1_lb)
# La forma estesa del test di Ljung-Box conferma che c'è presenza di correlazione nei lag 1 al 9
# e nel lag 13.

modello[['simulazione']][['arch_q1']][['asimmetrico']][['1']] <- append(modello[['simulazione']][['arch_q1']][['asimmetrico']][['1']], 
                                                                        list('lm'=Xt_t_student_asymmetric_arch1_q1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                             'Cullen-Frey'=Xt_t_student_asymmetric_arch1_q1_cf, 
                                                                             'Breusch-Pagan'=Xt_t_student_asymmetric_arch1_q1_bp, 'White'=Xt_t_student_asymmetric_arch1_q1_w, 
                                                                             'Ljiung-Box'=Xt_t_student_asymmetric_arch1_q1_lb, 'Dickey-Fuller'=Xt_t_student_asymmetric_arch1_q1_adf, 
                                                                             'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_arch1_q1_kpss, 'Shapiro-Wilk'=Xt_t_student_asymmetric_arch1_q1_sw,                                              
                                                                             'Generalized Distribution'=fitdist_test))

# In questo modello Arch(1) si ha presenza di eteroschedasticità e presenza di autocorrelazione.
# Quindi, proviamo a stimare i migliori parametri per il modello:
q <- 1
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,0)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
sigmasquaredW <- var(dist_t_student_asymmetric1)
stazionarietà <- a1*sigmasquaredW
print(paste("Verifico condizione di stazionerietà: ", stazionarietà))
Xt_t_student_asymmetric_arch1_q1_new <- model_arch(a0, a1, X0, dist_t_student_asymmetric1, q)

Xt <- Xt_t_student_asymmetric_arch1_q1_new
df_Xt_t_student_asymmetric_arch1_q1 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_asymmetric_arch1_q1_sw <- shapiro.test(Xt)
show(Xt_t_student_asymmetric_arch1_q1_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_asymmetric_arch1_q1
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(1) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_arch1_q1_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_arch1_q1_sp)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_arch1_q1_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_arch1_q1)
summary(Xt_t_student_asymmetric_arch1_q1_lm)
summary(Xt_t_student_asymmetric_arch1_q1_lm$fitted.values)

Xt_t_student_asymmetric_arch1_q1_res <- Xt_t_student_asymmetric_arch1_q1_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- DescTools::Skew(Xt_t_student_asymmetric_arch1_q1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# -1.734758 -2.012437 -1.515962 
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_asymmetric_arch1_q1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt      lwr.ci   upr.ci 
# 4.210131 3.024145 5.978218 

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_asymmetric_arch1_q1_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_arch1_q1_res
num_lags <- 3                  # Setting the lag parameter for the test.
Xt_t_student_asymmetric_arch1_q1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_arch1_q1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_asymmetric_arch1_q1_res   
Xt_t_student_asymmetric_arch1_q1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_arch1_q1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_arch1_q1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(1) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
Xt_t_student_asymmetric_arch1_q1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_arch1_q1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_arch1_q1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Arch(1) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
Xt_t_student_asymmetric_arch1_q1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_arch1_q1_sp)

plot(Xt_t_student_asymmetric_arch1_q1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_arch1_q1_lm,3) # Scale-location

#Determiniamo se la serie è eteroschedastico:
# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_arch1_q1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_arch1_q1)
show(Xt_t_student_asymmetric_arch1_q1_bp)
# Si ha un p-value di 0.02001 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_arch1_q1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_arch1_q1)
show(Xt_t_student_asymmetric_arch1_q1_w)
# Si ha un p-value di 0.06186 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Test Ljiung-box
y <- Xt_t_student_asymmetric_arch1_q1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 2.8229, df = 1, p-value = 0.09293
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 3  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_asymmetric_arch1_q1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag.

modello[['stimati']][['arch_q1']] <- append(modello[['stimati']][['arch_q1']], 
                                            list('asimmetrico'=list('Xt'=Xt_t_student_asymmetric_arch1_q1_new, 'a0'=a0, 'a1'=a1, 'q'=q,
                                                                    'stazionarietà'=stazionaietà, 'lm'=Xt_t_student_asymmetric_arch1_q1_lm, 
                                                                    'skew'=skew, 'kurt'=kurt, 'Cullen-Frey'=Xt_t_student_asymmetric_arch1_q1_cf, 
                                                                    'Breusch-Pagan'=Xt_t_student_asymmetric_arch1_q1_bp, 'White'=Xt_t_student_asymmetric_arch1_q1_w, 
                                                                    'Ljiung-Box'=Xt_t_student_asymmetric_arch1_q1_lb, 'Dickey-Fuller'=Xt_t_student_asymmetric_arch1_q1_adf, 
                                                                    'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_arch1_q1_kpss)))

# In questo modello ARCH(1) con una distribuzione t-student asimmetrica, con i parametri stimati,
# si ha un'assenza di correlazione e presenza di eteroschedasticità nel test di Breusch-Pagan, a 
# differenza del test di White in cui si ha presenza di omoschedasticità.
 
##########################################
##########################################

########## MODEL GARCH(1,1)

##### DISTRIBUZIONE NORMALE
modello[['stimati']] <- append(modello[['stimati']], list('garch_q1_p1'=list()))

# Consideriamo la seconda traiettoia con distribuzione normale di un modello GARCH(1,1)
Xt <- Xt_normal_garch2_q1_p1 
df_Xt_normal_garch2_q1_p1  <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_normal_garch2_q1_p1_sw <- shapiro.test(Xt)
show(Xt_normal_garch2_q1_p1_sw)
# Si ha un p-value maggiore di 0.05 quindi possiamo dire che la serie è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_normal_garch2_q1_p1 
lenh <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) with a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_garch2_q1_p1_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch2_q1_p1_sp)
# La regression line risulta essere orizzontale, quindi possiamo dedurre assenza di un trend e quindi il
# modello è stazionario. La LOESS oscilla leggermente intorno alla regression line.

# Consideriamo un modello lineare
Xt_normal_garch2_q1_p1_lm <- lm(Xt~t, data=df_Xt_normal_garch2_q1_p1 )
summary(Xt_normal_garch2_q1_p1_lm)
summary(Xt_normal_garch2_q1_p1_lm$fitted.values)

Xt_normal_garch2_q1_p1_res <- Xt_normal_garch2_q1_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- DescTools::Skew(Xt_normal_garch2_q1_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# -0.06223087 -0.20788399  0.06658206 
set.seed(123)
kurt <- DescTools::Kurt(Xt_normal_garch2_q1_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt      lwr.ci   upr.ci 
# -0.1692899 -0.3993812  0.1749151    

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_normal_garch2_q1_p1_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare.
# ADF Test
y <- Xt_normal_garch2_q1_p1_res
num_lags <- 5                   # Setting the lag parameter for the test.
Xt_normal_garch2_q1_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_normal_garch2_q1_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_normal_garch2_q1_p1_res   
Xt_normal_garch2_q1_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_normal_garch2_q1_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch2_q1_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_normal_garch2_q1_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch2_q1_p1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch2_q1_p1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(1,1) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Square root of absolute residuals")
Xt_normal_garch2_q1_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch2_q1_p1_sp)

plot(Xt_normal_garch2_q1_p1_lm,1) # Residuals vs Fitted
plot(Xt_normal_garch2_q1_p1_lm,2) # Q-Q Residuals
plot(Xt_normal_garch2_q1_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch2_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch2_q1_p1 )
show(Xt_normal_garch2_q1_p1_bp)
# Si ha un p-value di 0.7758 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch2_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch2_q1_p1 )
show(Xt_normal_garch2_q1_p1_w)
# Si ha un p-value di 0.9314 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_normal_garch2_q1_p1_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(1,1)")))
subtitle_content <- bquote(paste("Normal distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano sempre entro 
# l'inervallo di confidenza.

# Test Ljiung-box
y <- Xt_normal_garch2_q1_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 0.20218, df = 1, p-value = 0.653
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 4  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_normal_garch2_q1_p1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_normal_garch2_q1_p1_lb)
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

modello[['simulazione']][['garch_q1_p1']][['normale']][['1']] <- append(modello[['simulazione']][['garch_q1_p1']][['normale']][['1']], 
                                                                        list('lm'=Xt_normal_garch2_q1_p1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                             'Cullen-Frey'=Xt_normal_garch2_q1_p1_cf, 
                                                                             'Breusch-Pagan'=Xt_normal_garch2_q1_p1_bp, 'White'=Xt_normal_garch2_q1_p1_w, 
                                                                             'Ljiung-Box'=Xt_normal_garch2_q1_p1_lb, 'Dickey-Fuller'=Xt_normal_garch2_q1_p1_adf, 
                                                                             'Kwiatowski-Phillips-Schmidt-Shin'=Xt_normal_garch2_q1_p1_kpss, 'Shapiro-Wilk'=Xt_normal_garch2_q1_p1_sw))

# Questo modello Garch(1,1) con una distribuzione normale ha assenza di autocorrelazione
# ma presenza di omoschedasticità. 
# Proviamo a stimare i parametri che si adattano meglio al modello.
q <- 1
p <- 1
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal2)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_normal2)
stazionarietà <- a1*sigmasquaredW + b1
print(paste("Verifico condizione di stazionerietà: ", stazionarietà))
Xt_normal_garch2_q1_p1_new <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_normal2, q, p)

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(1,1)
Xt <- Xt_normal_garch2_q1_p1_new
df_Xt_normal_garch2_q1_p1  <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_normal_garch2_q1_p1_sw <- shapiro.test(Xt)
show(Xt_normal_garch2_q1_p1_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_normal_garch2_q1_p1 
lenh <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) with a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_garch2_q1_p1_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch2_q1_p1_sp)
# La regression line risulta essere orizzontale, quindi possiamo dedurre assenza di un trend e quindi il
# modello è stazionario. La LOESS oscilla intorno alla regression line.

# Consideriamo un modello lineare
Xt_normal_garch2_q1_p1_lm <- lm(Xt~t, data=df_Xt_normal_garch2_q1_p1 )
summary(Xt_normal_garch2_q1_p1_lm)
summary(Xt_normal_garch2_q1_p1_lm$fitted.values)

Xt_normal_garch2_q1_p1_res <- Xt_normal_garch2_q1_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- skew <- DescTools::Skew(Xt_normal_garch2_q1_p1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew          lwr.ci      upr.ci 
# 0.1735643069 0.0005266142 0.3567590946
set.seed(123)
kurt <- kurt <- DescTools::Kurt(Xt_normal_garch2_q1_p1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#      kurt      lwr.ci      upr.ci 
# 0.7551241 0.4687479 1.1888544

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_normal_garch2_q1_p1_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_normal_garch2_q1_p1_res
num_lags <- 5                   # Setting the lag parameter for the test.
Xt_normal_garch2_q1_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_normal_garch2_q1_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_normal_garch2_q1_p1_res   
Xt_normal_garch2_q1_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_normal_garch2_q1_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch2_q1_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_normal_garch2_q1_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch2_q1_p1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch2_q1_p1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(1,1) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Square root of absolute residuals")
Xt_normal_garch2_q1_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch2_q1_p1_sp)

plot(Xt_normal_garch2_q1_p1_lm,1) # Residuals vs Fitted
plot(Xt_normal_garch2_q1_p1_lm,2) # Q-Q Residuals
plot(Xt_normal_garch2_q1_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch2_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch2_q1_p1 )
show(Xt_normal_garch2_q1_p1_bp)
# Si ha un p-value di 0.006036 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch2_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch2_q1_p1 )
show(Xt_normal_garch2_q1_p1_w)
# Si ha un p-value di 0.023 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Plot of the autocorrelogram.
y <- Xt_normal_garch2_q1_p1_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(1,1)")))
subtitle_content <- bquote(paste("Normal distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano sempre entro 
# l'intervallo di confidenza.

# Test Ljiung-box
y <- Xt_normal_garch2_q1_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 2.6795, df = 1, p-value = 0.1016
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 5  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_normal_garch2_q1_p1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_normal_garch2_q1_p1_lb)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag.

modello[['stimati']][['garch_q1_p1']] <- append(modello[['stimati']][['garch_q1_p1']], 
                                                list('normale'=list('Xt'=Xt_normal_garch2_q1_p1_new, 'a0'=a0, 'a1'=a1, 'b1'=b1, 'q'=q, 'p'=p,
                                                                   'stazionarietà'=stazionaietà, 'lm'=Xt_normal_garch2_q1_p1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                   'Cullen-Frey'=Xt_normal_garch2_q1_p1_cf, 
                                                                   'Breusch-Pagan'=Xt_normal_garch2_q1_p1_bp, 'White'=Xt_normal_garch2_q1_p1_w, 
                                                                   'Ljiung-Box'=Xt_normal_garch2_q1_p1_lb, 'Dickey-Fuller'=Xt_normal_garch2_q1_p1_adf, 
                                                                   'Kwiatowski-Phillips-Schmidt-Shin'=Xt_normal_garch2_q1_p1_kpss, 'Shapiro-Wilk'=Xt_normal_garch2_q1_p1_sw)))

# In questo modello Garch(1,1) con distribuzione normale risulta essere eteroschedastico e con assenza
# di autocorrelazione con i parametri stimati.

##########################################

##### DISTRIBUZIONE T-STUDENT SIMMETRICA
# Consideriamo la terza traiettoia con distribuzione t-student simmetrica di un modello GARCH(1,1)
Xt <- Xt_t_student_symmetric_garch3_q1_p1
df_Xt_t_student_symmetric_garch3_q1_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_symmetric_garch3_q1_p1_sw <- shapiro.test(Xt)
show(Xt_t_student_symmetric_garch3_q1_p1_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_symmetric_garch3_q1_p1
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\"  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) with a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_garch3_q1_p1_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch3_q1_p1_sp)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch3_q1_p1_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch3_q1_p1)
summary(Xt_t_student_symmetric_garch3_q1_p1_lm)
summary(Xt_t_student_symmetric_garch3_q1_p1_lm$fitted.values)

Xt_t_student_symmetric_garch3_q1_p1_res <- Xt_t_student_symmetric_garch3_q1_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- skew <- DescTools::Skew(Xt_t_student_symmetric_garch3_q1_p1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew          lwr.ci      upr.ci 
# 0.2002904 -0.1743216  0.5399429
set.seed(123)
kurt <- kurt <- DescTools::Kurt(Xt_t_student_symmetric_garch3_q1_p1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#      kurt      lwr.ci      upr.ci 
#    2.285842 1.510861 3.472128

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_symmetric_garch3_q1_p1_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_symmetric_garch3_q1_p1_res
num_lags <- 5                   # Setting the lag parameter for the test.
Xt_t_student_symmetric_garch3_q1_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_symmetric_garch3_q1_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_symmetric_garch3_q1_p1_res   
Xt_t_student_symmetric_garch3_q1_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_symmetric_garch3_q1_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch3_q1_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_symmetric_garch3_q1_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch3_q1_p1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch3_q1_p1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(1,1) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Square root of absolute residuals")
Xt_t_student_symmetric_garch3_q1_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch3_q1_p1_sp)

plot(Xt_t_student_symmetric_garch3_q1_p1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_garch3_q1_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch3_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch3_q1_p1)
show(Xt_t_student_symmetric_garch3_q1_p1_bp)
# Si ha un p-value di 0.1184 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch3_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch3_q1_p1)
show(Xt_t_student_symmetric_garch3_q1_p1_w)
# Si ha un p-value di 0.1875 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_garch3_q1_p1_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(1,1)")))
subtitle_content <- bquote(paste("t-student symmetric distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano entro 
# una banda ristretta, anche se in alcuni lag questo valore esce dall'intervallo; 
# quindi, possiamo dire che che la serie non è significativamente 
# correlata con le serie ritardate, ovvero che il passato non "spiega" il presente e che le 
# variazioni da un istante o periodo ad un altro sono sostanzialmente casuali

# Test Ljiung-box
y <- Xt_t_student_symmetric_garch3_q1_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 2.932, df = 1, p-value = 0.08684
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 5  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_symmetric_garch3_q1_p1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_symmetric_garch3_q1_p1_lb)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione.
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

modello[['simulazione']][['garch_q1_p1']][['simmetrico']][['3']] <- append(modello[['simulazione']][['garch_q1_p1']][['simmetrico']][['3']], 
                                                                           list('lm'=Xt_t_student_symmetric_garch3_q1_p1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                                'Cullen-Frey'=Xt_t_student_symmetric_garch3_q1_p1_cf, 
                                                                                'Breusch-Pagan'=Xt_t_student_symmetric_garch3_q1_p1_bp, 'White'=Xt_t_student_symmetric_garch3_q1_p1_w, 
                                                                                'Ljiung-Box'=Xt_t_student_symmetric_garch3_q1_p1_lb, 'Dickey-Fuller'=Xt_t_student_symmetric_garch3_q1_p1_adf, 
                                                                                'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_garch3_q1_p1_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_garch3_q1_p1_sw,                                                                             'Generalized Distribution'=fitdist_test)),                                                                             'Generalized Distribution'=fitdist_test))

# Il modello Garch(1,1) con distribuzione t-student simmetrica ha presenza di omoschedasticità.
# Dato che la serie è omoschedastico, proviamo a stimare i migliori parametri per il modello.
q <- 1
p <- 1
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
b1 <- coef(fit)[['beta1']] 
sigmasquaredW <- var(dist_t_student_symmetric1)
stazionarietà <- a1*sigmasquaredW
print(paste("Verifico condizione di stazionerietà: ", stazionarietà))   
Xt_t_student_symmetric_garch3_q1_p1_new <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_symmetric3, q, p)

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello GARCH(1,1)
Xt <- Xt_t_student_symmetric_garch3_q1_p1_new
df_Xt_t_student_symmetric_garch3_q1_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_symmetric_garch3_q1_p1_sw <- shapiro.test(Xt)
show(Xt_t_student_symmetric_garch3_q1_p1_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_symmetric_garch3_q1_p1
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) with a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_garch3_q1_p1_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch3_q1_p1_sp)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch3_q1_p1_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch3_q1_p1)
summary(Xt_t_student_symmetric_garch3_q1_p1_lm)
summary(Xt_t_student_symmetric_garch3_q1_p1_lm$fitted.values)

Xt_t_student_symmetric_garch3_q1_p1_res <- Xt_t_student_symmetric_garch3_q1_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- skew <- DescTools::Skew(Xt_t_student_symmetric_garch3_q1_p1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# 0.2002904 -0.1743216  0.5399429 
set.seed(123)
kurt <- kurt <- DescTools::Kurt(Xt_t_student_symmetric_garch3_q1_p1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt      lwr.ci   upr.ci 
# 2.285842 1.510861 3.472128

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_symmetric_garch3_q1_p1_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_symmetric_garch3_q1_p1_res
num_lags <- 5                   # Setting the lag parameter for the test.
Xt_t_student_symmetric_garch3_q1_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_symmetric_garch3_q1_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_symmetric_garch3_q1_p1_res   
Xt_t_student_symmetric_garch3_q1_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_symmetric_garch3_q1_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch3_q1_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_symmetric_garch3_q1_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch3_q1_p1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch3_q1_p1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(1,1) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_garch3_q1_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch3_q1_p1_sp)

plot(Xt_t_student_symmetric_garch3_q1_p1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_garch3_q1_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch3_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch3_q1_p1)
show(Xt_t_student_symmetric_garch3_q1_p1_bp)
# Si ha un p-value di 2.2e-16 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch3_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch3_q1_p1)
show(Xt_t_student_symmetric_garch3_q1_p1_w)
# Si ha un p-value di 2.2e-16 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_garch3_q1_p1_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(1,1)")))
subtitle_content <- bquote(paste("t-student symmetric distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")

# Test Ljiung-box
y <- Xt_t_student_symmetric_garch3_q1_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 2.353, df = 1, p-value = 0.125
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 5  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_symmetric_garch3_q1_p1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_symmetric_garch3_q1_p1_lb)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag.

modello[['stimati']][['garch_q1_p1']] <- append(modello[['stimati']][['garch_q1_p1']], 
                                                list('simmetrico'=list('Xt'=Xt_t_student_symmetric_garch3_q1_p1_new, 'a0'=a0, 'a1'=a1, 'b1'=b1, 'q'=q, 'p'=p, 
                                                                       'stazionarietà'=stazionaietà, 'lm'=Xt_t_student_symmetric_garch3_q1_p1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                       'Cullen-Frey'=Xt_t_student_symmetric_garch3_q1_p1_cf,
                                                                       'Breusch-Pagan'=Xt_t_student_symmetric_garch3_q1_p1_bp, 'White'=Xt_t_student_symmetric_garch3_q1_p1_w, 
                                                                       'Ljiung-Box'=Xt_t_student_symmetric_garch3_q1_p1_lb, 'Dickey-Fuller'=Xt_t_student_symmetric_garch3_q1_p1_adf, 
                                                                       'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_garch3_q1_p1_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_garch3_q1_p1_sw,                                                                             'Generalized Distribution'=fitdist_test)),                                                                             'Generalized Distribution'=fitdist_test)))

# In questo modello Garch(1,1) con una distribuzione t-student simmetrica, con i nuovi parametri stimati,
# si ha evidenza di eteroschedasticità e assenza di autocorrelazione.

##########################################

# Consideriamo la seconda traiettoia con distribuzione t-student asimmetrica di un modello GARCH(1,1)
Xt <- Xt_t_student_asymmetric_garch2_q1_p1
df_Xt_t_student_asymmetric_garch2_q1_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_asymmetric_garch2_q1_p1_sw <- shapiro.test(Xt)
show(Xt_t_student_asymmetric_garch2_q1_p1_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_asymmetric_garch2_q1_p1
lenh <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Line plot of the model Garch(1,1) with a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_garch2_q1_p1_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch2_q1_p1_sp)
# La regression line risulta leggermente inclinata, mentre la LOESS oscilla leggermente intorno alla regression line.

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch2_q1_p1_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch2_q1_p1)
summary(Xt_t_student_asymmetric_garch2_q1_p1_lm)
summary(Xt_t_student_asymmetric_garch2_q1_p1_lm$fitted.values)

Xt_t_student_asymmetric_garch2_q1_p1_res <- Xt_t_student_asymmetric_garch2_q1_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- DescTools::Skew(Xt_t_student_asymmetric_garch2_q1_p1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# -1.765622 -2.065372 -1.553775
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_asymmetric_garch2_q1_p1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt   lwr.ci   upr.ci 
#4.371883 3.127511 6.223429  

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_asymmetric_garch2_q1_p1_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_garch2_q1_p1_res
num_lags <- 5                   # Setting the lag parameter for the test.
Xt_t_student_asymmetric_garch2_q1_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_garch2_q1_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_asymmetric_garch2_q1_p1_res   
Xt_t_student_asymmetric_garch2_q1_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_garch2_q1_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch2_q1_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_asymmetric_garch2_q1_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch2_q1_p1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch2_q1_p1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(1,1) of asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_garch2_q1_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch2_q1_p1_sp)

plot(Xt_t_student_asymmetric_garch2_q1_p1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_garch2_q1_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch2_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch2_q1_p1)
show(Xt_t_student_asymmetric_garch2_q1_p1_bp)
# Si ha un p-value di 0.02387 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch2_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch2_q1_p1)
show(Xt_t_student_asymmetric_garch2_q1_p1_w)
# Si ha un p-value di 0.03169 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_asymmetric_garch2_q1_p1_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(1,1)")))
subtitle_content <- bquote(paste("t-student asymmetric distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano entro 
# una banda ristretta, anche se nel primo lag questo valore esce anche 
# dall'intervallo di confidenza del 99%.

# Test Ljiung-box
y <- Xt_t_student_asymmetric_garch2_q1_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 12.535, df = 1, p-value = 0.0003994
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 5  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_asymmetric_garch2_q1_p1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_asymmetric_garch2_q1_p1_lb)
# I risultati hanno un p-value < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

modello[['simulazione']][['garch_q1_p1']][['asimmetrico']][['2']] <- append(modello[['simulazione']][['garch_q1_p1']][['asimmetrico']][['2']], 
                                                                            list('lm'=Xt_t_student_asymmetric_garch2_q1_p1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                                 'Cullen-Frey'=Xt_t_student_asymmetric_garch2_q1_p1_cf,
                                                                                 'Breusch-Pagan'=Xt_t_student_asymmetric_garch2_q1_p1_bp, 'White'=Xt_t_student_asymmetric_garch2_q1_p1_w, 
                                                                                 'Ljiung-Box'=Xt_t_student_asymmetric_garch2_q1_p1_lb, 'Dickey-Fuller'=Xt_t_student_asymmetric_garch2_q1_p1_adf, 
                                                                                 'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_garch2_q1_p1_kpss, 'Shapiro-Wilk'=Xt_t_student_asymmetric_garch2_q1_p1_sw,                                                                             'Generalized Distribution'=fitdist_test)),                                                                             'Generalized Distribution'=fitdist_test))

# Questo modello Garch(1,1) con una distribuzione normale ha presenza di autocorrelazione
# e presenza di eteroschedasticità. 
# Proviamo a stimare i parametri che si adattano meglio al modello.
q <- 1
p <- 1
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_t_student_asymmetric1)
stazionarietà <- a1*sigmasquaredW
print(paste("Verifico condizione di stazionerietà: ", stazionarietà))
Xt_t_student_asymmetric_garch2_q1_p1_new <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_asymmetric2, q, p)

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello GARCH(1,1)
Xt <- Xt_t_student_asymmetric_garch2_q1_p1_new
df_Xt_t_student_asymmetric_garch2_q1_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_asymmetric_garch2_q1_p1_sw <- shapiro.test(Xt)
show(Xt_t_student_asymmetric_garch2_q1_p1_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_asymmetric_garch2_q1_p1
lenh <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Line plot of the model Garch(1,1) with a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_garch2_q1_p1_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch2_q1_p1_sp)
# La regression line risulta leggermente inclinata, mentre la LOESS oscilla leggermente intorno alla regression line.

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch2_q1_p1_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch2_q1_p1)
summary(Xt_t_student_asymmetric_garch2_q1_p1_lm)
summary(Xt_t_student_asymmetric_garch2_q1_p1_lm$fitted.values)

Xt_t_student_asymmetric_garch2_q1_p1_res <- Xt_t_student_asymmetric_garch2_q1_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- DescTools::Skew(Xt_t_student_asymmetric_garch2_q1_p1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# -1.765622 -2.065372 -1.553775 
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_asymmetric_garch2_q1_p1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt      lwr.ci   upr.ci 
# 4.371883 3.127511 6.223429

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_asymmetric_garch2_q1_p1_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_garch2_q1_p1_res
num_lags <- 5                   # Setting the lag parameter for the test.
Xt_t_student_asymmetric_garch2_q1_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_garch2_q1_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_asymmetric_garch2_q1_p1_res   
Xt_t_student_asymmetric_garch2_q1_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_garch2_q1_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch2_q1_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_asymmetric_garch2_q1_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch2_q1_p1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch2_q1_p1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(1,1) of asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_garch2_q1_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch2_q1_p1_sp)

plot(Xt_t_student_asymmetric_garch2_q1_p1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_garch2_q1_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch2_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch2_q1_p1)
show(Xt_t_student_asymmetric_garch2_q1_p1_bp)
# Si ha un p-value di 3.044e-05 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch2_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch2_q1_p1)
show(Xt_t_student_asymmetric_garch2_q1_p1_w)
# Si ha un p-value di 0.0001603 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Plot of the autocorrelogram.
y <- Xt_t_student_asymmetric_garch2_q1_p1_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(1,1)")))
subtitle_content <- bquote(paste("t-student asymmetric distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano entro 
# una banda ristretta

# Test Ljiung-box
y <- Xt_t_student_asymmetric_garch2_q1_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 1.4426, df = 1, p-value = 0.2297
# I risultati mostrano un p-value > 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 5  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_asymmetric_garch2_q1_p1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_asymmetric_garch2_q1_p1_lb)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione.

modello[['stimati']][['garch_q1_p1']] <- append(modello[['stimati']][['garch_q1_p1']], 
                                                list('asimmetrico'=list('Xt'=Xt_t_student_asymmetric_garch2_q1_p1_new, 'a0'=a0, 'a1'=a1, 'b1'=b1, 'q'=q, 'p'=p,
                                                                        'stazionarietà'=stazionaietà, 'lm'=Xt_t_student_asymmetric_garch2_q1_p1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                        'Cullen-Frey'=Xt_t_student_asymmetric_garch2_q1_p1_cf,
                                                                        'Breusch-Pagan'=Xt_t_student_asymmetric_garch2_q1_p1_bp, 'White'=Xt_t_student_asymmetric_garch2_q1_p1_w, 
                                                                        'Ljiung-Box'=Xt_t_student_asymmetric_garch2_q1_p1_lb, 'Dickey-Fuller'=Xt_t_student_asymmetric_garch2_q1_p1_adf, 
                                                                        'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_garch2_q1_p1_kpss, 'Shapiro-Wilk'=Xt_t_student_asymmetric_garch2_q1_p1_sw,                                                                             'Generalized Distribution'=fitdist_test)),                                                                             'Generalized Distribution'=fitdist_test)))

# In questo modello Garch(1,1) con una distribuzione t-student asimmetrica, con i
# i miglior parametri stimati la serie ha presenza di eteroschedasticità e
# assenza di autocorrelazione.

##########################################
##########################################

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(1,2)
Xt <- Xt_normal_garch1_q1_p2
df_Xt_normal_garch1_q1_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_normal_garch1_q1_p2_sw <- shapiro.test(Xt)
show(Xt_normal_garch1_q1_p2_sw)
# Si ha un p-value maggiore di 0.05 quindi possiamo dire che la serie è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_normal_garch1_q1_p2
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,2) with a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_garch1_q1_p2_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch1_q1_p2_sp)
# La regression line tende ad essere leggermente inclinata e la LOESS oscilla intorno
# a questa linea.

# Consideriamo un modello lineare
Xt_normal_garch1_q1_p2_lm <- lm(Xt~t, data=df_Xt_normal_garch1_q1_p2)
summary(Xt_normal_garch1_q1_p2_lm)
summary(Xt_normal_garch1_q1_p2_lm$fitted.values)

Xt_normal_garch1_q1_p2_res <- Xt_normal_garch1_q1_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- DescTools::Skew(Xt_normal_garch1_q1_p2_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# -0.05205686 -0.19468377  0.07767588
set.seed(123)
kurt <- DescTools::Kurt(Xt_normal_garch1_q1_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt      lwr.ci   upr.ci 
# -0.1701762 -0.3900903  0.1612521  

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_normal_garch1_q1_p2_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_normal_garch1_q1_p2_res
num_lags <- 6                   # Setting the lag parameter for the test.
Xt_normal_garch1_q1_p2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_normal_garch1_q1_p2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_normal_garch1_q1_p2_res   
Xt_normal_garch1_q1_p2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_normal_garch1_q1_p2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch1_q1_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,2) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_normal_garch1_q1_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch1_q1_p2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch1_q1_p2_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(1,2) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_garch1_q1_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch1_q1_p2_sp)

plot(Xt_normal_garch1_q1_p2_lm,1) # Residuals vs Fitted
plot(Xt_normal_garch1_q1_p2_lm,2) # Q-Q Residuals
plot(Xt_normal_garch1_q1_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch1_q1_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch1_q1_p2)
show(Xt_normal_garch1_q1_p2_bp)
# Si ha un p-value di 0.004426 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch1_q1_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch1_q1_p2)
show(Xt_normal_garch1_q1_p2_w)
# Si ha un p-value di 0.0166 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_normal_garch1_q1_p2_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(1,1)")))
subtitle_content <- bquote(paste("Normal distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano sempre entro 
# una banda ristretta, quindi, possiamo dire che che la serie non è significativamente 
# correlata con le serie ritardate.

# Test Ljiung-box
y <- Xt_normal_garch1_q1_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 3.783, df = 1, p-value = 0.05177
# Si ha un p-value > 0.05, quindi non è possibile rigettare l'ipotesi nulla di 
# assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 6  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_normal_garch1_q1_p2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_normal_garch1_q1_p2_lb)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag.
# I risultati indicano che c'è assenza di correlazione nella serie.

modello[['simulazione']][['garch_q1_p2']][['normale']][['1']] <- append(modello[['simulazione']][['garch_q1_p2']][['normale']][['1']], 
                                                                        list('lm'=Xt_normal_garch1_q1_p2_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                             'Cullen-Frey'=Xt_normal_garch1_q1_p2_cf, 'Breusch-Pagan'=Xt_normal_garch1_q1_p2_bp, 'White'=Xt_normal_garch1_q1_p2_w, 
                                                                             'Ljiung-Box'=Xt_normal_garch1_q1_p2_lb, 'Dickey-Fuller'=Xt_normal_garch1_q1_p2_adf, 
                                                                             'Kwiatowski-Phillips-Schmidt-Shin'=Xt_normal_garch1_q1_p2_kpss, 'Shapiro-Wilk'=Xt_normal_garch1_q1_p2_sw))

# In questo modello Garch(1,2) con una distribuzione normale possiamo affermare che la serie 
# ha evidenza di eteroschedasticità e assenza di autocorrelazione.
# Proviamo a stimare i parametri che si adattano meglio al modello.
q <- 1
p <- 2
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
bp <- c(coef(fit)[['beta1']], coef(fit)[['beta2']])
sigmasquaredW <- var(dist_normal2)
stazionarietà <- a1*sigmasquaredW + bp[1] + bp[2]
print(paste("Verifico condizione di stazionerietà: ", stazionarietà))
Xt_normal_garch1_q1_p2_new <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_normal1, q, p)

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(1,2)
Xt <- Xt_normal_garch1_q1_p2_new
df_Xt_normal_garch1_q1_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_normal_garch1_q1_p2_sw <- shapiro.test(Xt)
show(Xt_normal_garch1_q1_p2_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_normal_garch1_q1_p2
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,2) with a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_garch1_q1_p2_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch1_q1_p2_sp)
# La regression line tende ad essere leggermente inclinata e la LOESS oscilla intorno
# a questa linea.

# Consideriamo un modello lineare
Xt_normal_garch1_q1_p2_lm <- lm(Xt~t, data=df_Xt_normal_garch1_q1_p2)
summary(Xt_normal_garch1_q1_p2_lm)
summary(Xt_normal_garch1_q1_p2_lm$fitted.values)

Xt_normal_garch1_q1_p2_res <- Xt_normal_garch1_q1_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- DescTools::Skew(Xt , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# 0.12299752 -0.05726906  0.30523411
set.seed(123)
kurt <- DescTools::Kurt(Xt , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt      lwr.ci   upr.ci 
# 0.7790380 0.4872084 1.2193013 

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_normal_garch1_q1_p2_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_normal_garch1_q1_p2_res
num_lags <- 6                   # Setting the lag parameter for the test.
Xt_normal_garch1_q1_p2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_normal_garch1_q1_p2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_normal_garch1_q1_p2_res   
Xt_normal_garch1_q1_p2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_normal_garch1_q1_p2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch1_q1_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,2) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_normal_garch1_q1_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch1_q1_p2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch1_q1_p2_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(1,2) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_garch1_q1_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch1_q1_p2_sp)

plot(Xt_normal_garch1_q1_p2_lm,1) # Residuals vs Fitted
plot(Xt_normal_garch1_q1_p2_lm,2) # Q-Q Residuals
plot(Xt_normal_garch1_q1_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch1_q1_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch1_q1_p2)
show(Xt_normal_garch1_q1_p2_bp)
# Si ha un p-value di 5.046e-13 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch1_q1_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch1_q1_p2)
show(Xt_normal_garch1_q1_p2_w)
# Si ha un p-value di 4.633e-12 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_normal_garch1_q1_p2_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(1,1)")))
subtitle_content <- bquote(paste("Normal distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano sempre entro 
# una banda ristretta, quindi, possiamo dire che che la serie non è significativamente 
# correlata con le serie ritardate.

# Test Ljiung-box
y <- Xt_normal_garch1_q1_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 0.18437, df = 1, p-value = 0.6676
# Si ha un p-value > 0.05, quindi non è possibile rigettare l'ipotesi nulla di 
# assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 6  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_normal_garch1_q1_p2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_normal_garch1_q1_p2_lb)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag.
# I risultati indicano che c'è assenza di correlazione nella serie.

modello[['stimati']][['garch_q1_p2']] <- append(modello[['stimati']][['garch_q1_p2']], 
                                                list('normale'=list('Xt'=Xt_normal_garch1_q1_p2_new, 'a0'=a0, 'a1'=a1, 'b1'=bp[1], 'b2'=bp[2], 'q'=q, 'p'=p,
                                                                   'stazionarietà'=stazionaietà, 'lm'=Xt_normal_garch1_q1_p2_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                   'Cullen-Frey'=Xt_normal_garch1_q1_p2_cf, 'Breusch-Pagan'=Xt_normal_garch1_q1_p2_bp, 'White'=Xt_normal_garch1_q1_p2_w, 
                                                                   'Ljiung-Box'=Xt_normal_garch1_q1_p2_lb, 'Dickey-Fuller'=Xt_normal_garch1_q1_p2_adf, 
                                                                   'Kwiatowski-Phillips-Schmidt-Shin'=Xt_normal_garch1_q1_p2_kpss,'Shapiro-Wilk'=Xt_normal_garch1_q1_p2_sw)))

# In questo modello Garch(1,1) con distribuzione normale risulta essere eteroschedastico e con assenza
# di autocorrelazione con i parametri stimati.

##########################################

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello GARCH(1,2)
Xt <- Xt_t_student_symmetric_garch1_q1_p2
df_Xt_t_student_symmetric_garch1_q1_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_symmetric_garch1_q1_p2_sw <- shapiro.test(Xt)
show(Xt_t_student_symmetric_garch1_q1_p2_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_symmetric_garch1_q1_p2
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\"  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) with a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_garch1_q1_p2_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch1_q1_p2_sp)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch1_q1_p2_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch1_q1_p2)
summary(Xt_t_student_symmetric_garch1_q1_p2_lm)
summary(Xt_t_student_symmetric_garch1_q1_p2_lm$fitted.values)

Xt_t_student_symmetric_garch1_q1_p2_res <- Xt_t_student_symmetric_garch1_q1_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- skew <- DescTools::Skew(Xt_t_student_symmetric_garch1_q1_p2, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew          lwr.ci      upr.ci 
# 0.1529583 -0.2693668  0.5254305
set.seed(123)
kurt <- kurt <- DescTools::Kurt(Xt_t_student_symmetric_garch1_q1_p2, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#      kurt      lwr.ci      upr.ci 
#    2.531558 1.586253 3.976148

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_symmetric_garch1_q1_p2_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_symmetric_garch1_q1_p2_res
num_lags <- 6                   # Setting the lag parameter for the test.
Xt_t_student_symmetric_garch1_q1_p2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_symmetric_garch1_q1_p2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_symmetric_garch1_q1_p2_res   
Xt_t_student_symmetric_garch1_q1_p2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_symmetric_garch1_q1_p2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch1_q1_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,2) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_symmetric_garch1_q1_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch1_q1_p2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch1_q1_p2_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(1,2) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_garch1_q1_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch1_q1_p2_sp)

plot(Xt_t_student_symmetric_garch1_q1_p2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_garch1_q1_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch1_q1_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch1_q1_p2)
show(Xt_t_student_symmetric_garch1_q1_p2_bp)
# Si ha un p-value di 0.002272 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch1_q1_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch1_q1_p2)
show(Xt_t_student_symmetric_garch1_q1_p2_w)
# Si ha un p-value di 0.009257 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_garch1_q1_p2_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(1,2)")))
subtitle_content <- bquote(paste("t-student symmetric distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano entro 
# una banda ristretta

# Test Ljiung-box
y <- Xt_t_student_symmetric_garch1_q1_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 0.40705, df = 1, p-value = 0.5235
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 6  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_symmetric_garch1_q1_p2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_symmetric_garch1_q1_p2_lb)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag.
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

modello[['simulazione']][['garch_q1_p2']][['simmetrico']][['1']] <- append(modello[['simulazione']][['garch_q1_p2']][['simmetrico']][['1']], 
                                                                           list('lm'=Xt_t_student_symmetric_garch1_q1_p2_lm, 'skew'=skew, 'kurt'=kurt,
                                                                                'Cullen-Frey'=Xt_t_student_symmetric_garch1_q1_p2_cf, 'Breusch-Pagan'=Xt_t_student_symmetric_garch1_q1_p2_bp, 'White'=Xt_t_student_symmetric_garch1_q1_p2_w, 
                                                                                'Ljiung-Box'=Xt_t_student_symmetric_garch1_q1_p2_lb, 'Dickey-Fuller'=Xt_t_student_symmetric_garch1_q1_p2_adf, 
                                                                                'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_garch1_q1_p2_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_garch1_q1_p2_sw,                                                                             'Generalized Distribution'=fitdist_test)),                                                                             'Generalized Distribution'=fitdist_test))

# Questo modello Garch(1,2) con distribuzione t-student simmetrica ha un'evidenza di 
# eteroschedasticità nella serie e assenza di autocorrelazione nei residui.
# Proviamo a stimare i migliori parametri per il modello.
q <- 1
p <- 2
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
bp <- c(coef(fit)[['beta1']], coef(fit)[['beta2']])
sigmasquaredW <- var(dist_t_student_symmetric1)
stazionarietà <- (a1*sigmasquaredW) + bp[1] + bp[2]
print(paste("Verifico condizione di stazionerietà: ", stazionarietà))   
Xt_t_student_symmetric_garch1_q1_p2_new <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_symmetric1, q, p)

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello GARCH(1,2)
Xt <- Xt_t_student_symmetric_garch1_q1_p2_new
df_Xt_t_student_symmetric_garch1_q1_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_symmetric_garch1_q1_p2_sw <- shapiro.test(Xt)
show(Xt_t_student_symmetric_garch1_q1_p2_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_symmetric_garch1_q1_p2
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) with a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_garch1_q1_p2_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch1_q1_p2_sp)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch1_q1_p2_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch1_q1_p2)
summary(Xt_t_student_symmetric_garch1_q1_p2_lm)
summary(Xt_t_student_symmetric_garch1_q1_p2_lm$fitted.values)

Xt_t_student_symmetric_garch1_q1_p2_res <- Xt_t_student_symmetric_garch1_q1_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- skew <- DescTools::Skew(Xt, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew          lwr.ci      upr.ci 
# 0.49900252 0.05631944 1.07944638
set.seed(123)
kurt <- kurt <- DescTools::Kurt(Xt, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#      kurt      lwr.ci      upr.ci 
#    5.202500 3.906953 7.276112

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_symmetric_garch1_q1_p2_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_symmetric_garch1_q1_p2_res
num_lags <- 6                   # Setting the lag parameter for the test.
Xt_t_student_symmetric_garch1_q1_p2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_symmetric_garch1_q1_p2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_symmetric_garch1_q1_p2_res   
Xt_t_student_symmetric_garch1_q1_p2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_symmetric_garch1_q1_p2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch1_q1_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,2) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_symmetric_garch1_q1_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch1_q1_p2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch1_q1_p2_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(1,2) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_garch1_q1_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch1_q1_p2_sp)

plot(Xt_t_student_symmetric_garch1_q1_p2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_garch1_q1_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch1_q1_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch1_q1_p2)
show(Xt_t_student_symmetric_garch1_q1_p2_bp)
# Si ha un p-value di 2.2e-16 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch1_q1_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch1_q1_p2)
show(Xt_t_student_symmetric_garch1_q1_p2_w)
# Si ha un p-value di 2.2e-16 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_garch1_q1_p2_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(1,2)")))
subtitle_content <- bquote(paste("t-student symmetric distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano entro 
# una banda ristretta

# Test Ljiung-box
y <- Xt_t_student_symmetric_garch1_q1_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 0.91573, df = 1, p-value = 0.3386
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 6  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_symmetric_garch1_q1_p2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_symmetric_garch1_q1_p2_lb)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag.
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

modello[['stimati']][['garch_q1_p2']] <- append(modello[['stimati']][['garch_q1_p2']], 
                                                list('simmetrico'=list('Xt'=Xt_t_student_symmetric_garch1_q1_p2_new, 'a0'=a0, 'a1'=a1, 'b1'=bp[1], 'b2'=bp[2], 'q'=q, 'p'=p,
                                                                       'stazionarietà'=stazionaietà, 'lm'=Xt_t_student_symmetric_garch1_q1_p2_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                       'Cullen-Frey'=Xt_t_student_symmetric_garch1_q1_p2_cf, 'Breusch-Pagan'=Xt_t_student_symmetric_garch1_q1_p2_bp, 'White'=Xt_t_student_symmetric_garch1_q1_p2_w, 
                                                                       'Ljiung-Box'=Xt_t_student_symmetric_garch1_q1_p2_lb, 'Dickey-Fuller'=Xt_t_student_symmetric_garch1_q1_p2_adf, 
                                                                       'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_garch1_q1_p2_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_garch1_q1_p2_sw,                                                                             'Generalized Distribution'=fitdist_test)),                                                                             'Generalized Distribution'=fitdist_test)))

# In questo modello Garch(1,1) con una distribuzione t-student simmetrica, con i nuovi parametri stimati,
# si ha evidenza di eteroschedasticità e assenza di autocorrelazione.

##########################################

# Consideriamo la prima traiettoia con distribuzione t-student asimmetrica di un modello GARCH(1,1)
Xt <- Xt_t_student_asymmetric_garch1_q1_p2
df_Xt_t_student_asymmetric_garch1_q1_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_asymmetric_garch1_q1_p2_sw <- shapiro.test(Xt)
show(Xt_t_student_asymmetric_garch1_q1_p2_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_asymmetric_garch1_q1_p2
lenh <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Line plot of the model Garch(1,1) with a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_garch1_q1_p2_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch1_q1_p2_sp)
# La regression line risulta leggermente inclinata, e la LOESS corrisponde alla linea orizzontale.
 
# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch1_q1_p2_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch1_q1_p2)
summary(Xt_t_student_asymmetric_garch1_q1_p2_lm)
summary(Xt_t_student_asymmetric_garch1_q1_p2_lm$fitted.values)

Xt_t_student_asymmetric_garch1_q1_p2_res <- Xt_t_student_asymmetric_garch1_q1_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- DescTools::Skew(Xt_t_student_asymmetric_garch1_q1_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# -1.787824 -2.120223 -1.557365
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_asymmetric_garch1_q1_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt   lwr.ci   upr.ci 
#4.600950 3.186499 6.708246 

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_asymmetric_garch1_q1_p2_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_garch1_q1_p2_res
num_lags <- 6                   # Setting the lag parameter for the test.
Xt_t_student_asymmetric_garch1_q1_p2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_garch1_q1_p2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_asymmetric_garch1_q1_p2_res   
Xt_t_student_asymmetric_garch1_q1_p2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_garch1_q1_p2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch1_q1_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,2) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_asymmetric_garch1_q1_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch1_q1_p2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch1_q1_p2_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(1,2) of a asymmetricc t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_garch1_q1_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch1_q1_p2_sp)

plot(Xt_t_student_asymmetric_garch1_q1_p2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_garch1_q1_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q1_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q1_p2)
show(Xt_t_student_asymmetric_garch1_q1_p2_bp)
# Si ha un p-value di 0.0178 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q1_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q1_p2)
show(Xt_t_student_asymmetric_garch1_q1_p2_w)
# Si ha un p-value di 0.0594 > 0.05, quindi, possiamo non rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_asymmetric_garch1_q1_p2_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(1,2)")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano entro 
# una banda ristretta, eccetto nel primo il cui valore esce dall'intervallo di confidenza del 99%.

# Test Ljiung-box
y <- Xt_t_student_asymmetric_garch1_q1_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 13.422, df = 1, p-value = 0.0002487
T <- length(y)
n_pars <- 6  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_asymmetric_garch1_q1_p2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_asymmetric_garch1_q1_p2_lb)
# I risultati hanno un p-value < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# I risultati mostrano un p-value < 0.05, ciò significa che ci sono prove
# di autocorrelazione nei residui del modello.

modello[['simulazione']][['garch_q1_p2']][['asimmetrico']][['1']] <- append(modello[['simulazione']][['garch_q1_p2']][['asimmetrico']][['1']], 
                                                                            list('lm'=Xt_t_student_asymmetric_garch1_q1_p2_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                                 'Cullen-Frey'=Xt_t_student_asymmetric_garch1_q1_p2_cf, 
                                                                                 'Breusch-Pagan'=Xt_t_student_asymmetric_garch1_q1_p2_bp, 'White'=Xt_t_student_asymmetric_garch1_q1_p2_w, 
                                                                                 'Ljiung-Box'=Xt_t_student_asymmetric_garch1_q1_p2_lb, 'Dickey-Fuller'=Xt_t_student_asymmetric_garch1_q1_p2_adf, 
                                                                                 'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_garch1_q1_p2_kpss, 'Shapiro-Wilk'=Xt_t_student_asymmetric_garch1_q1_p2_sw,                                                                             'Generalized Distribution'=fitdist_test)),                                                                             'Generalized Distribution'=fitdist_test))

# Questo modello Garch(1,2) con una distribuzione t-student asimmetrica ha presenza di autocorrelazione
# e presenza di omoschedasticità nel test di White. 
# Proviamo a stimare i parametri che si adattano meglio al modello.
q <- 1
p <- 2
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
bp <- c(coef(fit)[['beta1']], coef(fit)[['beta2']])
sigmasquaredW <- var(dist_t_student_asymmetric1)
stazionarietà <- (a1*sigmasquaredW) + bp[1] + bp[2]
print(paste("Verifico condizione di stazionerietà: ", stazionarietà))
Xt_t_student_asymmetric_garch1_q1_p2_new <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)

# Consideriamo la prima traiettoia con distribuzione t-student asimmetrica di un modello GARCH(1,1)
Xt <- Xt_t_student_asymmetric_garch1_q1_p2
df_Xt_t_student_asymmetric_garch1_q1_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_asymmetric_garch1_q1_p2_sw <- shapiro.test(Xt)
show(Xt_t_student_asymmetric_garch1_q1_p2_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_asymmetric_garch1_q1_p2
lenh <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Line plot of the model Garch(1,1) with a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_garch1_q1_p2_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch1_q1_p2_sp)
# La regression line risulta leggermente inclinata, e la LOESS corrisponde alla linea orizzontale.

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch1_q1_p2_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch1_q1_p2)
summary(Xt_t_student_asymmetric_garch1_q1_p2_lm)
summary(Xt_t_student_asymmetric_garch1_q1_p2_lm$fitted.values)

Xt_t_student_asymmetric_garch1_q1_p2_res <- Xt_t_student_asymmetric_garch1_q1_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- DescTools::Skew(Xt, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# -1.787824 -2.120223 -1.557365 
set.seed(123)
kurt <- DescTools::Kurt(Xt, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt   lwr.ci   upr.ci 
#4.600950 3.186499 6.708246 

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_asymmetric_garch1_q1_p2_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_garch1_q1_p2_res
num_lags <- 6                   # Setting the lag parameter for the test.
Xt_t_student_asymmetric_garch1_q1_p2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_garch1_q1_p2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_asymmetric_garch1_q1_p2_res   
Xt_t_student_asymmetric_garch1_q1_p2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_garch1_q1_p2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch1_q1_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,2) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_asymmetric_garch1_q1_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch1_q1_p2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch1_q1_p2_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(1,2) of a asymmetricc t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_garch1_q1_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch1_q1_p2_sp)

plot(Xt_t_student_asymmetric_garch1_q1_p2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_garch1_q1_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q1_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q1_p2)
show(Xt_t_student_asymmetric_garch1_q1_p2_bp)
# Si ha un p-value di 0.0178 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q1_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q1_p2)
show(Xt_t_student_asymmetric_garch1_q1_p2_w)
# Si ha un p-value di 0.0594 > 0.05, quindi, possiamo non rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_asymmetric_garch1_q1_p2_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(1,2)")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano entro 
# una banda ristretta, eccetto nel primo il cui valore esce dall'intervallo di confidenza del 99%.

# Test Ljiung-box
y <- Xt_t_student_asymmetric_garch1_q1_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 13.422, df = 1, p-value = 0.0002487
T <- length(y)
n_pars <- 6  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_asymmetric_garch1_q1_p2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_asymmetric_garch1_q1_p2_lb)
# I risultati hanno un p-value < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# I risultati mostrano un p-value < 0.05, ciò significa che ci sono prove
# di autocorrelazione nei residui del modello.

modello[['stimati']][['garch_q1_p2']] <- append(modello[['stimati']][['garch_q1_p2']], 
                                                list('asimmetrico'=list('Xt'=Xt_t_student_asymmetric_garch1_q1_p2_new, 'a0'=a0, 'a1'=a1, 'b1'=bp[1], 'b2'=bp[2], 'q'=q, 'p'=p,
                                                                        'stazionarietà'=stazionaietà, 'lm'=Xt_t_student_asymmetric_garch1_q1_p2_lm,'skew'=skew, 'kurt'=kurt, 
                                                                        'Cullen-Frey'=Xt_t_student_asymmetric_garch1_q1_p2_cf, 
                                                                        'Breusch-Pagan'=Xt_t_student_asymmetric_garch1_q1_p2_bp, 'White'=Xt_t_student_asymmetric_garch1_q1_p2_w, 
                                                                        'Ljiung-Box'=Xt_t_student_asymmetric_garch1_q1_p2_lb, 'Dickey-Fuller'=Xt_t_student_asymmetric_garch1_q1_p2_adf, 
                                                                        'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_garch1_q1_p2_kpss, 'Shapiro-Wilk'=Xt_t_student_asymmetric_garch1_q1_p2_sw,                                                                             'Generalized Distribution'=fitdist_test)),                                                                             'Generalized Distribution'=fitdist_test)))

# In questo modello Garch(1,2) con una distribuzione t-student asimmetrica, con i nuovi parametri stimati,
# presenta eteroschedasticità nella serie e assenza di autocorrelazione.

##########################################
##########################################

# Consideriamo la prima traiettoia con distribuzione normale di un modello ARCH(2)
Xt <- Xt_normal_arch1_q2
df_Xt_normal_arch1_q2 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_normal_arch1_q2_sw <- shapiro.test(Xt)
show(Xt_normal_arch1_q2_sw)
# Si ha un p-value maggiore di 0.05 quindi possiamo dire che la serie è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_normal_arch1_q2
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Line plot of the model Arch(2) with a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_arch1_q2_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_arch1_q2_sp)
# La regression line tende ad essere leggermente inclinata e la LOESS oscilla intorno
# a questa linea.

# Consideriamo un modello lineare
Xt_normal_arch1_q2_lm <- lm(Xt~t, data=df_Xt_normal_arch1_q2)
summary(Xt_normal_arch1_q2_lm)
summary(Xt_normal_arch1_q2_lm$fitted.values)

Xt_normal_arch1_q2_res <- Xt_normal_arch1_q2_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- skew <- DescTools::Skew(Xt_normal_arch1_q2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew
#       skew      lwr.ci      upr.ci 
# -0.1544153 -0.3911550  0.2348151 
set.seed(123)
kurt <- kurt <- DescTools::Kurt(Xt_normal_arch1_q2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
kurt
#      kurt     lwr.ci     upr.ci 
# -0.1544153 -0.3911550  0.2348151

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_normal_arch1_q2_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_normal_arch1_q2_res
num_lags <- 4                   # Setting the lag parameter for the test.
Xt_normal_arch1_q2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_normal_arch1_q2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_normal_arch1_q2_res   
Xt_normal_arch1_q2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_normal_arch1_q2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_arch1_q2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(2) with a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_normal_arch1_q2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_arch1_q2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_arch1_q2_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Arch(2) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_arch1_q2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_arch1_q2_sp)

plot(Xt_normal_arch1_q2_lm,1) # Residuals vs Fitted
plot(Xt_normal_arch1_q2_lm,2) # Q-Q Residuals
plot(Xt_normal_arch1_q2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_arch1_q2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_arch1_q2)
show(Xt_normal_arch1_q2_bp)
# Si ha un p-value di 0.004084 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_arch1_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_arch1_q2)
show(Xt_normal_arch1_q2_w)
# Si ha un p-value di 0.01576 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_normal_arch1_q2_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model ARCH(2)")))
subtitle_content <- bquote(paste("Normal distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano entro 
# una banda ristretta eccetto in due lag.

# Test Ljiung-box
y <- Xt_normal_arch1_q2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 4.4703, df = 1, p-value = 0.03449
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 4  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_normal_arch1_q2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_normal_arch1_q2_lb)
# La forma estesa del test di Ljung-Box conferma che c'è presenza di correlazione.
# Il risultato indica che c'è presenza di autocorrelazione nei residui del modello.

modello[['simulazione']][['arch_q2']][['normale']][['1']] <- append(modello[['simulazione']][['arch_q2']][['normale']][['1']], 
                                                                    list('lm'=Xt_normal_arch1_q2_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                         'Cullen-Frey'=Xt_normal_arch1_q2_cf, 
                                                                         'Breusch-Pagan'=Xt_normal_arch1_q2_bp, 'White'=Xt_normal_arch1_q2_w, 
                                                                         'Ljiung-Box'=Xt_normal_arch1_q2_lb, 'Dickey-Fuller'=Xt_normal_arch1_q2_adf, 
                                                                         'Kwiatowski-Phillips-Schmidt-Shin'=Xt_normal_arch1_q2_kpss, 'Shapiro-Wilk'=Xt_normal_arch1_q2_sw))

# Questo modello Arch(2) con una distribuzione normale ha presenza di autocorrelazione
# e presenza di eteroschedasticità. 
# Proviamo a stimare i parametri che si adattano meglio al modello.
q <- 2
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,0)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
sigmasquaredW <- var(dist_normal1)
stazionaietà <- (aq[1]+aq[2])*sigmasquaredW
print(paste("Verifico condizione di stazionerietà: ", stazionaietà))
Xt_normal_arch1_q2_new <- model_arch(a0, aq, X0, dist_normal1, q)

# Consideriamo una traiettoia con distribuzione normale di un modello ARCH(2)
Xt <- Xt_normal_arch1_q2_new
df_Xt_normal_arch1_q2 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_normal_arch1_q2_sw <- shapiro.test(Xt)
show(Xt_normal_arch1_q2_sw)
# Si ha un p-value maggiore di 0.05 quindi possiamo dire che la serie è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_normal_arch1_q2
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Line plot of the model Arch(2) with a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_arch1_q2_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_arch1_q2_sp)
# La regression line tende ad essere leggermente inclinata e la LOESS oscilla intorno
# a questa linea.

# Consideriamo un modello lineare
Xt_normal_arch1_q2_lm <- lm(Xt~t, data=df_Xt_normal_arch1_q2)
summary(Xt_normal_arch1_q2_lm)
summary(Xt_normal_arch1_q2_lm$fitted.values)

Xt_normal_arch1_q2_res <- Xt_normal_arch1_q2_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- skew <- DescTools::Skew(Xt_normal_arch1_q2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew
#       skew      lwr.ci      upr.ci 
# -0.05299184 -0.17462423  0.05864336 
set.seed(123)
kurt <- kurt <- DescTools::Kurt(Xt_normal_arch1_q2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
kurt
#      kurt     lwr.ci     upr.ci 
# -0.1544153 -0.3911550  0.2348151

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_normal_arch1_q2_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_normal_arch1_q2_res
num_lags <- 4                   # Setting the lag parameter for the test.
Xt_normal_arch1_q2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_normal_arch1_q2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_normal_arch1_q2_res   
Xt_normal_arch1_q2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_normal_arch1_q2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_arch1_q2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(2) with a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_normal_arch1_q2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_arch1_q2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_arch1_q2_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Arch(2) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_arch1_q2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_arch1_q2_sp)

plot(Xt_normal_arch1_q2_lm,1) # Residuals vs Fitted
plot(Xt_normal_arch1_q2_lm,2) # Q-Q Residuals
plot(Xt_normal_arch1_q2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_arch1_q2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_arch1_q2)
show(Xt_normal_arch1_q2_bp)
# Si ha un p-value di 0.01246 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_arch1_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_arch1_q2)
show(Xt_normal_arch1_q2_w)
# Si ha un p-value di 0.04176 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_normal_arch1_q2_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model ARCH(2)")))
subtitle_content <- bquote(paste("Normal distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano entro 
# una banda ristretta eccetto in due lag.

# Test Ljiung-box
y <- Xt_normal_arch1_q2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 3.0555, df = 1, p-value = 0.08046
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 4  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_normal_arch1_q2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_normal_arch1_q2_lb)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione.
# Il risultato indica che c'è assenza di autocorrelazione nei residui del modello.

modello[['stimati']][['arch_q2']] <- append(modello[['stimati']][['arch_q2']], 
                                            list('normale'=list('Xt'=Xt_normal_arch1_q2_new, 'a0'=a0, 'a1'=aq[1], 'a2'=aq[2], 'q'=q, 
                                                               'stazionarietà'=stazionaietà, 'lm'=Xt_normal_arch1_q2_lm, 'skew'=skew, 'kurt'=kurt, 
                                                               'Cullen-Frey'=Xt_normal_arch1_q2_cf, 
                                                               'Breusch-Pagan'=Xt_normal_arch1_q2_bp, 'White'=Xt_normal_arch1_q2_w, 
                                                               'Ljiung-Box'=Xt_normal_arch1_q2_lb, 'Dickey-Fuller'=Xt_normal_arch1_q2_adf, 
                                                               'Kwiatowski-Phillips-Schmidt-Shin'=Xt_normal_arch1_q2_kpss, 'Shapiro-Wilk'=Xt_normal_arch1_q2_sw)))

# In questo modello Arch(2) con distribuzione normale, risulta essere eteroschedastico e con assenza
# di autocorrelazione con i parametri stimati.

##########################################

# Consideriamo la seconda traiettoia con distribuzione t-student simmetrica di un modello ARCH(2)
Xt <- Xt_t_student_symmetric_arch2_q2
df_Xt_t_student_symmetric_arch2_q2 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_symmetric_arch2_q2_sw <- shapiro.test(Xt)
show(Xt_t_student_symmetric_arch2_q2_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_symmetric_arch2_q2
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\"  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(2) with a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_arch2_q2_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_arch2_q2_sp)
# Regression line e LOESS sembrano coincidere.

# Consideriamo un modello lineare
Xt_t_student_symmetric_arch2_q2_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_arch2_q2)
summary(Xt_t_student_symmetric_arch2_q2_lm)
summary(Xt_t_student_symmetric_arch2_q2_lm$fitted.values)

Xt_t_student_symmetric_arch2_q2_res <- Xt_t_student_symmetric_arch2_q2_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- skew <- DescTools::Skew(Xt_t_student_symmetric_arch2_q2, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#     skew      lwr.ci    upr.ci 
#  0.2881626 -0.4626866  1.1913669 
set.seed(123)
kurt <- kurt <- DescTools::Kurt(Xt_t_student_symmetric_arch2_q2, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#      kurt      lwr.ci      upr.ci 
#     6.405644  3.595817 10.055224

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_symmetric_arch2_q2_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_symmetric_arch2_q2_res
num_lags <- 4                   # Setting the lag parameter for the test.
Xt_t_student_symmetric_arch2_q2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_symmetric_arch2_q2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_symmetric_arch2_q2_res   
Xt_t_student_symmetric_arch2_q2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_symmetric_arch2_q2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_arch2_q2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(2) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_symmetric_arch2_q2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_arch2_q2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_arch2_q2_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Arch(2) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_arch2_q2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_arch2_q2_sp)

plot(Xt_t_student_symmetric_arch2_q2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_arch2_q2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_arch2_q2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_arch2_q2)
show(Xt_t_student_symmetric_arch2_q2_bp)
# Si ha un p-value di 0.05495 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_arch2_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_arch2_q2)
show(Xt_t_student_symmetric_arch2_q2_w)
# Si ha un p-value di 0.1056 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_arch2_q2_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model ARCH(2)")))
subtitle_content <- bquote(paste("t-student symmetric distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano entro 
# una banda ristretta, eccetto per il lag 15.

# Test Ljiung-box
y <- Xt_t_student_symmetric_arch2_q2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 1.1745, df = 1, p-value = 0.2785
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 4  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_symmetric_arch2_q2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_symmetric_arch2_q2_lb)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione.
# I risultati dei test hanno un p-value > 0.05; questo, significa che c'è assenza di autocorrelazione,
# poichè non possiamo rigettare l'ipotesi nulla.

modello[['simulazione']][['arch_q2']][['simmetrico']][['2']] <- append(modello[['simulazione']][['arch_q2']][['simmetrico']][['2']], 
                                                                       list('lm'=Xt_t_student_symmetric_arch2_q2_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                            'Cullen-Frey'=Xt_t_student_symmetric_arch2_q2_cf, 
                                                                            'Breusch-Pagan'=Xt_t_student_symmetric_arch2_q2_bp, 'White'=Xt_t_student_symmetric_arch2_q2_w, 
                                                                            'Ljiung-Box'=Xt_t_student_symmetric_arch2_q2_lb, 'Dickey-Fuller'=Xt_t_student_symmetric_arch2_q2_adf, 
                                                                            'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_arch2_q2_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_arch2_q2_sw,                                                                             'Generalized Distribution'=fitdist_test)),                                                                             'Generalized Distribution'=fitdist_test))

# Il modello Arch(2) con distribuzione t-student simmetrica ha presenza di omoschedasticità
# nella serie e assenza di autocorrelazione.
q <- 2
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,0)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric2)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
sigmasquaredW <- var(dist_t_student_symmetric1)
stazionarietà <- (aq[1]+aq[2])*sigmasquaredW
print(paste("Verifico condizione di stazionerietà: ", stazionarietà))
Xt_t_student_symmetric_arch2_q2_new <- model_arch(a0, aq, X0, dist_t_student_symmetric2, q)

Xt <- Xt_t_student_symmetric_arch2_q2_new
df_Xt_t_student_symmetric_arch2_q2 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_symmetric_arch2_q2_sw <- shapiro.test(Xt)
show(Xt_t_student_symmetric_arch2_q2_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_symmetric_arch2_q2
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\"  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(2) with a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_arch2_q2_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_arch2_q2_sp)
# Regression line e LOESS sembrano coincidere.

# Consideriamo un modello lineare
Xt_t_student_symmetric_arch2_q2_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_arch2_q2)
summary(Xt_t_student_symmetric_arch2_q2_lm)
summary(Xt_t_student_symmetric_arch2_q2_lm$fitted.values)

Xt_t_student_symmetric_arch2_q2_res <- Xt_t_student_symmetric_arch2_q2_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- skew <- DescTools::Skew(Xt_t_student_symmetric_arch2_q2, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#     skew      lwr.ci    upr.ci 
#  0.2881626 -0.4626866  1.1913669 
set.seed(123)
kurt <- kurt <- DescTools::Kurt(Xt_t_student_symmetric_arch2_q2, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#      kurt      lwr.ci      upr.ci 
#     6.405644  3.595817 10.055224

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_symmetric_arch2_q2_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_symmetric_arch2_q2_res
num_lags <- 4                   # Setting the lag parameter for the test.
Xt_t_student_symmetric_arch2_q2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_symmetric_arch2_q2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_symmetric_arch2_q2_res   
Xt_t_student_symmetric_arch2_q2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_symmetric_arch2_q2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_arch2_q2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(2) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_symmetric_arch2_q2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_arch2_q2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_arch2_q2_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Arch(2) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_arch2_q2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_arch2_q2_sp)

plot(Xt_t_student_symmetric_arch2_q2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_arch2_q2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_arch2_q2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_arch2_q2)
show(Xt_t_student_symmetric_arch2_q2_bp)
# Si ha un p-value di 0.08707 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_arch2_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_arch2_q2)
show(Xt_t_student_symmetric_arch2_q2_w)
# Si ha un p-value di 0.1964 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_arch2_q2_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model ARCH(2)")))
subtitle_content <- bquote(paste("t-student symmetric distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano entro 
# una banda ristretta, eccetto per il lag 15.

# Test Ljiung-box
y <- Xt_t_student_symmetric_arch2_q2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 0.29774, df = 1, p-value = 0.5853
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 4  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_symmetric_arch2_q2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_symmetric_arch2_q2_lb)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione.
# I risultati dei test hanno un p-value > 0.05; questo, significa che c'è assenza di autocorrelazione,
# poichè non possiamo rigettare l'ipotesi nulla.

modello[['stimati']][['arch_q2']] <- append(modello[['stimati']][['arch_q2']], list('simmetrico'=list('Xt'=Xt_t_student_symmetric_arch2_q2_new, 'a0'=a0, 'a1'=aq[1], 'a2'=aq[2], 'q'=q, 
                                                                                                      'stazionarietà'=stazionaietà, 'lm'=Xt_t_student_symmetric_arch2_q2_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                                                      'Cullen-Frey'=Xt_t_student_symmetric_arch2_q2_cf, 
                                                                                                      'Breusch-Pagan'=Xt_t_student_symmetric_arch2_q2_bp, 'White'=Xt_t_student_symmetric_arch2_q2_w, 
                                                                                                      'Ljiung-Box'=Xt_t_student_symmetric_arch2_q2_lb, 'Dickey-Fuller'=Xt_t_student_symmetric_arch2_q2_adf, 
                                                                                                      'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_arch2_q2_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_arch2_q2_sw,                                                                             'Generalized Distribution'=fitdist_test)),                                                                             'Generalized Distribution'=fitdist_test)))

# In questo modello Arch(2) con una distribuzione t-student simmetrica si ha evidenza di omoschedasticità nella serie
# e assenza di autocorrelazione nei residui.
##########################################

# Consideriamo la prima traiettoia con distribuzione t-student asimmetrica di un modello ARCH(2)
Xt <- Xt_t_student_asymmetric_arch1_q2
df_Xt_t_student_asymmetric_arch1_q2 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_asymmetric_arch1_q2_sw <- shapiro.test(Xt)
show(Xt_t_student_asymmetric_arch1_q2_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_asymmetric_arch1_q2
lenh <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Line plot of the model Arch(2) with a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_arch1_q2_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_arch1_q2_sp)
# La regression line risulta leggermente inclinata, e la LOESS coincide con la regression line.

# Consideriamo un modello lineare
Xt_t_student_asymmetric_arch1_q2_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_arch1_q2)
summary(Xt_t_student_asymmetric_arch1_q2_lm)
summary(Xt_t_student_asymmetric_arch1_q2_lm$fitted.values)

Xt_t_student_asymmetric_arch1_q2_res <- Xt_t_student_asymmetric_arch1_q2_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- DescTools::Skew(Xt_t_student_asymmetric_arch1_q2, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# -1.953076 -2.282325 -1.697657 
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_asymmetric_arch1_q2, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt   lwr.ci   upr.ci 
#5.677547 4.243872 8.020919 

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_asymmetric_arch1_q2_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_arch1_q2_res
num_lags <- 4                   # Setting the lag parameter for the test.
Xt_t_student_asymmetric_arch1_q2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_arch1_q2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_asymmetric_arch1_q2_res   
Xt_t_student_asymmetric_arch1_q2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_arch1_q2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_arch1_q2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(2) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_asymmetric_arch1_q2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_arch1_q2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_arch1_q2_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Arch(21) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_arch1_q2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_arch1_q2_sp)

plot(Xt_t_student_asymmetric_arch1_q2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_arch1_q2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_arch1_q2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_arch1_q2)
show(Xt_t_student_asymmetric_arch1_q2_bp)
# Si ha un p-value di  0.008644 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_arch1_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_arch1_q2)
show(Xt_t_student_asymmetric_arch1_q2_w)
# Si ha un p-value di 0.0311 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_asymmetric_arch1_q2_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model ARCH(2)")))
subtitle_content <- bquote(paste("t-student asymmetric distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori nei primi due lag
# non rientrano nell'intervallo di confidenza.

# Test Ljiung-box
y <- Xt_t_student_asymmetric_arch1_q2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 18.342, df = 1, p-value = 1.846e-05
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 4  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_asymmetric_arch1_q2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_asymmetric_arch1_q2_lb)
# Il risultato del test haun p-value < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

modello[['simulazione']][['arch_q2']][['asimmetrico']][['1']] <- append(modello[['simulazione']][['arch_q2']][['asimmetrico']][['1']], 
                                                                        list('lm'=Xt_t_student_asymmetric_arch1_q2_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                             'Cullen-Frey'=Xt_t_student_asymmetric_arch1_q2_cf, 
                                                                             'Breusch-Pagan'=Xt_t_student_asymmetric_arch1_q2_bp, 'White'=Xt_t_student_asymmetric_arch1_q2_w, 
                                                                             'Ljiung-Box'=Xt_t_student_asymmetric_arch1_q2_lb, 'Dickey-Fuller'=Xt_t_student_asymmetric_arch1_q2_adf, 
                                                                             'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_arch1_q2_kpss, 'Shapiro-Wilk'=Xt_t_student_asymmetric_arch1_q2_sw,                                                                             'Generalized Distribution'=fitdist_test)),                                                                             'Generalized Distribution'=fitdist_test))

# In questo modello Arch(2) con una distribuzione t-student asimmetrica si ha presenza di eteroschedasticità
# ma con presenza di autocorrelazione. 
# Proviamo a stimare i parametri che si adattano meglio al modello.
q <- 2
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,0)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
sigmasquaredW <- var(dist_t_student_asymmetric1)
stazionarietà <- (aq[1]+aq[2])*sigmasquaredW
print(paste("Verifico condizione di stazionerietà: ", stazionarietà))
Xt_t_student_asymmetric_arch1_q2_new <- model_arch(a0, aq, X0, dist_t_student_asymmetric1, q)

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello ARCH(2)
Xt <- Xt_t_student_asymmetric_arch1_q2_new
df_Xt_t_student_asymmetric_arch1_q2 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_asymmetric_arch1_q2_sw <- shapiro.test(Xt)
show(Xt_t_student_asymmetric_arch1_q2_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_asymmetric_arch1_q2
lenh <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Line plot of the model Arch(2) with a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_arch1_q2_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_arch1_q2_sp)
# La regression line risulta leggermente inclinata, e la LOESS coincide con la regression line.

# Consideriamo un modello lineare
Xt_t_student_asymmetric_arch1_q2_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_arch1_q2)
summary(Xt_t_student_asymmetric_arch1_q2_lm)
summary(Xt_t_student_asymmetric_arch1_q2_lm$fitted.values)

Xt_t_student_asymmetric_arch1_q2_res <- Xt_t_student_asymmetric_arch1_q2_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- DescTools::Skew(Xt_t_student_asymmetric_arch1_q2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# -1.719841 -2.060248 -1.481361 
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_asymmetric_arch1_q2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt   lwr.ci   upr.ci 
#4.487039 3.086632 6.572315

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_asymmetric_arch1_q2_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_arch1_q2_res
num_lags <- 4                   # Setting the lag parameter for the test.
Xt_t_student_asymmetric_arch1_q2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_arch1_q2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_asymmetric_arch1_q2_res   
Xt_t_student_asymmetric_arch1_q2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_arch1_q2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_arch1_q2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(2) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_asymmetric_arch1_q2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_arch1_q2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_arch1_q2_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Arch(21) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_arch1_q2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_arch1_q2_sp)

plot(Xt_t_student_asymmetric_arch1_q2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_arch1_q2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_arch1_q2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_arch1_q2)
show(Xt_t_student_asymmetric_arch1_q2_bp)
# Si ha un p-value di  0.01755 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_arch1_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_arch1_q2)
show(Xt_t_student_asymmetric_arch1_q2_w)
# Si ha un p-value di 0.05526 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_asymmetric_arch1_q2_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model ARCH(2)")))
subtitle_content <- bquote(paste("t-student asymmetric distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori nei primi due lag
# non rientrano nell'intervallo di confidenza.

# Test Ljiung-box
y <- Xt_t_student_asymmetric_arch1_q2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 2.968, df = 1, p-value = 0.08493
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 4  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_asymmetric_arch1_q2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_asymmetric_arch1_q2_lb)
# Il risultato del test haun p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

modello[['stimati']][['arch_q2']] <- append(modello[['stimati']][['arch_q2']], 
                                            list('asimmetrico'=list('Xt'=Xt_t_student_asymmetric_arch1_q2_new, 'a0'=a0, 'a1'=aq[1], 'a2'=aq[2], 'q'=q, 
                                                                    'stazionarietà'=stazionaietà, 'lm'=Xt_t_student_asymmetric_arch1_q2_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                    'Cullen-Frey'=Xt_t_student_asymmetric_arch1_q2_cf, 
                                                                    'Breusch-Pagan'=Xt_t_student_asymmetric_arch1_q2_bp, 'White'=Xt_t_student_asymmetric_arch1_q2_w, 
                                                                    'Ljiung-Box'=Xt_t_student_asymmetric_arch1_q2_lb, 'Dickey-Fuller'=Xt_t_student_asymmetric_arch1_q2_adf, 
                                                                    'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_arch1_q2_kpss, 'Shapiro-Wilk'=Xt_t_student_asymmetric_arch1_q2_sw,                                                                             'Generalized Distribution'=fitdist_test)),                                                                             'Generalized Distribution'=fitdist_test)))

# In questo modello Arch(2) con una distribuzione t-student asimmetrica si ha presenza di eteroschedasticità
# nel test di Breusch-Pagan ma presenza di omoschedasticità con il test di White; con i nuovi
# parametri stimati si ha assenza di autocorrelazione. 

##########################################
##########################################

# Consideriamo la prima traiettoia con distribuzione normale di un modello GARCH(2,1)
Xt <- Xt_normal_garch1_q2_p1
df_Xt_normal_garch1_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_normal_garch1_q2_p1_sw <- shapiro.test(Xt)
show(Xt_normal_garch1_q2_p1_sw)
# Si ha un p-value maggiore di 0.05 quindi possiamo dire che la serie è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_normal_garch1_q1_p2
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,2) with a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_garch1_q1_p2_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch1_q1_p2_sp)
# La regression line tende ad essere leggermente inclinata e la LOESS oscilla intorno
# a questa linea.

# Consideriamo un modello lineare
Xt_normal_garch1_q2_p1_lm <- lm(Xt~t, data=df_Xt_normal_garch1_q2_p1)
summary(Xt_normal_garch1_q2_p1_lm)
summary(Xt_normal_garch1_q2_p1_lm$fitted.values)

Xt_normal_garch1_q2_p1_res <- Xt_normal_garch1_q2_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- DescTools::Skew(Xt_normal_garch1_q2_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# -0.06382440 -0.21414944  0.08838435 
set.seed(123)
kurt <- DescTools::Kurt(Xt_normal_garch1_q2_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt      lwr.ci   upr.ci 
# -0.04413334 -0.32341129  0.30455569   

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_normal_garch1_q2_p1_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_normal_garch1_q2_p1_res
num_lags <- 6                   # Setting the lag parameter for the test.
Xt_normal_garch1_q2_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_normal_garch1_q2_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_normal_garch1_q2_p1_res   
Xt_normal_garch1_q2_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_normal_garch1_q2_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch1_q2_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,1) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_normal_garch1_q2_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch1_q2_p1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch1_q2_p1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(2,1) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_garch1_q2_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch1_q2_p1_sp)

plot(Xt_normal_garch1_q2_p1_lm,1) # Residuals vs Fitted
plot(Xt_normal_garch1_q2_p1_lm,2) # Q-Q Residuals
plot(Xt_normal_garch1_q2_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch1_q2_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch1_q2_p1)
show(Xt_normal_garch1_q2_p1_bp)
# Si ha un p-value di 0.0008743 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch1_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch1_q2_p1)
show(Xt_normal_garch1_q2_p1_w)
# Si ha un p-value di 0.003932 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_normal_garch1_q2_p1_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(2,1)")))
subtitle_content <- bquote(paste("Normal distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano sempre entro 
# una banda ristretta.

# Test Ljiung-box
y <- Xt_normal_garch1_q2_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 4.6602, df = 1, p-value = 0.03087
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 4  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_normal_garch1_q2_p1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_normal_garch1_q2_p1_lb)
# I risultati mostrano un p-value < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Il risultati indica una presenza di autocorrelazione nei residui del modello con i test Box-Ljung,
# poichè rifiutiamo l'ipotesi nulla di assenza di autocorrelazione in favore dell'alternatiava.

modello[['simulazione']][['garch_q2_p1']][['normale']][['1']] <- append(modello[['simulazione']][['garch_q2_p1']][['normale']][['1']], 
                                                                        list('lm'=Xt_normal_garch1_q2_p1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                             'Cullen-Frey'=Xt_normal_garch1_q2_p1_cf, 
                                                                             'Breusch-Pagan'=Xt_normal_garch1_q2_p1_bp, 'White'=Xt_normal_garch1_q2_p1_w, 
                                                                             'Ljiung-Box'=Xt_normal_garch1_q2_p1_lb, 'Dickey-Fuller'=Xt_normal_garch1_q2_p1_adf, 
                                                                             'Kwiatowski-Phillips-Schmidt-Shin'=Xt_normal_garch1_q2_p1_kpss, 'Shapiro-Wilk'=Xt_normal_garch1_q2_p1_sw))

# Proviamo a stimare i parametri che si adattano meglio al modello.
q <- 2
p <- 1
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_normal1)
stazionarietà <- a1*sigmasquaredW + b1
print(paste("Verifico condizione di stazionerietà: ", stazionarietà))
Xt_normal_garch1_q2_p1_new <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_normal1, q, p)

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(2,1)
Xt <- Xt_normal_garch1_q2_p1_new
df_Xt_normal_garch1_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_normal_garch1_q2_p1_sw <- shapiro.test(Xt)
show(Xt_normal_garch1_q2_p1_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_normal_garch1_q1_p2
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,2) with a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_garch1_q1_p2_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch1_q1_p2_sp)
# La regression line tende ad essere leggermente inclinata e la LOESS oscilla intorno
# a questa linea.

# Consideriamo un modello lineare
Xt_normal_garch1_q2_p1_lm <- lm(Xt~t, data=df_Xt_normal_garch1_q2_p1)
summary(Xt_normal_garch1_q2_p1_lm)
summary(Xt_normal_garch1_q2_p1_lm$fitted.values)

Xt_normal_garch1_q2_p1_res <- Xt_normal_garch1_q2_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- DescTools::Skew(Xt_normal_garch1_q2_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# -0.0349105 -0.2026294  0.1713391
set.seed(123)
kurt <- DescTools::Kurt(Xt_normal_garch1_q2_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt      lwr.ci   upr.ci 
# 0.8079337 0.4901407 1.2738097   

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_normal_garch1_q2_p1_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_normal_garch1_q2_p1_res
num_lags <- 5                   # Setting the lag parameter for the test.
Xt_normal_garch1_q2_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_normal_garch1_q2_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_normal_garch1_q2_p1_res   
Xt_normal_garch1_q2_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_normal_garch1_q2_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch1_q2_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,1) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_normal_garch1_q2_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch1_q2_p1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch1_q2_p1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(2,1) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_garch1_q2_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch1_q2_p1_sp)

plot(Xt_normal_garch1_q2_p1_lm,1) # Residuals vs Fitted
plot(Xt_normal_garch1_q2_p1_lm,2) # Q-Q Residuals
plot(Xt_normal_garch1_q2_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch1_q2_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch1_q2_p1)
show(Xt_normal_garch1_q2_p1_bp)
# Si ha un p-value di 2.2e-16 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch1_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch1_q2_p1)
show(Xt_normal_garch1_q2_p1_w)
# Si ha un p-value di 2.2e-16 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_normal_garch1_q2_p1_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(2,1)")))
subtitle_content <- bquote(paste("Normal distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano sempre entro 
# una banda ristretta.

# Test Ljiung-box
y <- Xt_normal_garch1_q2_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 1.6778, df = 1, p-value = 0.1952
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 4  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_normal_garch1_q2_p1_bt <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_normal_garch1_q2_p1_bt)
# I risultati mostrano un p-value < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Il risultati indica una presenza di autocorrelazione nei residui del modello con i test Box-Ljung,
# poichè rifiutiamo l'ipotesi nulla di assenza di autocorrelazione in favore dell'alternatiava.

modello[['stimati']][['garch_q2_p1']] <- append(modello[['stimati']][['garch_q2_p1']], 
                                                list('normale'=list('Xt'=Xt_normal_garch1_q2_p1_new, 'a0'=a0, 'a1'=aq[1], 'a2'=aq[2], 'b1'=b1, 'q'=q, 'p'=p,
                                                                   'stazionarietà'=stazionaietà, 'lm'=Xt_normal_garch1_q2_p1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                   'Cullen-Frey'=Xt_normal_garch1_q2_p1_cf, 
                                                                   'Breusch-Pagan'=Xt_normal_garch1_q2_p1_bp, 'White'=Xt_normal_garch1_q2_p1_w, 
                                                                   'Ljiung-Box'=Xt_normal_garch1_q2_p1_lb, 'Dickey-Fuller'=Xt_normal_garch1_q2_p1_adf, 
                                                                   'Kwiatowski-Phillips-Schmidt-Shin'=Xt_normal_garch1_q2_p1_kpss, 'Shapiro-Wilk'=Xt_normal_garch1_q2_p1_sw)))

# In questo modello Garch(2,1) con una distribuzione normale e con i nuovi parametri stimati
# ha evidenza di eteroschedasticità e assenza di autocorrelazione.

##########################################

# Consideriamo la seconda traiettoia con distribuzione t-student simmetrica di un modello GARCH(2,1)
Xt <- Xt_t_student_symmetric_garch2_q2_p1
df_Xt_t_student_symmetric_garch2_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_symmetric_garch2_q2_p1_sw <- shapiro.test(Xt)
show(Xt_t_student_symmetric_garch2_q2_p1_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_symmetric_garch2_q2_p1
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,1) with a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_garch2_q2_p1_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch2_q2_p1_sp)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch2_q2_p1_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch2_q2_p1)
summary(Xt_t_student_symmetric_garch2_q2_p1_lm)
summary(Xt_t_student_symmetric_garch2_q2_p1_lm$fitted.values)

Xt_t_student_symmetric_garch2_q2_p1_res <- Xt_t_student_symmetric_garch2_q2_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- skew <- DescTools::Skew(Xt_t_student_symmetric_garch2_q2_p1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew          lwr.ci      upr.ci 
# 0.4054487 -0.5069528  1.5384136 
set.seed(123)
kurt <- kurt <- DescTools::Kurt(Xt_t_student_symmetric_garch2_q2_p1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#      kurt      lwr.ci      upr.ci 
#    8.718254  4.774529 13.232193

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_symmetric_garch2_q2_p1_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_symmetric_garch2_q2_p1_res
num_lags <- 5                   # Setting the lag parameter for the test.
Xt_t_student_symmetric_garch2_q2_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_symmetric_garch2_q2_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_symmetric_garch2_q2_p1_res   
Xt_t_student_symmetric_garch2_q2_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_symmetric_garch2_q2_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch2_q2_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,1) of a symmetic t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Linear Model Residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_garch2_q2_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch2_q2_p1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch2_q2_p1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(2,1) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_garch2_q2_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch2_q2_p1_sp)

plot(Xt_t_student_symmetric_garch2_q2_p1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_garch2_q2_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch2_q2_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch2_q2_p1)
show(Xt_t_student_symmetric_garch2_q2_p1_bp)
# Si ha un p-value di 0.06841 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch2_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch2_q2_p1)
show(Xt_t_student_symmetric_garch2_q2_p1_w)
# Si ha un p-value di 0.07795 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_garch2_q2_p1_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(2,1)")))
subtitle_content <- bquote(paste("t-student symmetric distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano entro 
# una banda ristretta

# Test Ljiung-box
y <- Xt_t_student_symmetric_garch2_q2_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 1.5367, df = 1, p-value = 0.2151
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 4  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_symmetric_garch2_q2_p1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_symmetric_garch2_q2_p1_lb)
# La forma estesa del test di Ljung-Box conferma che c'è presenza di correlazione in tutti i lag
# eccetto nel lag 17 e 19.
# Il risultati indica una presenza di autocorrelazione nei residui del modello.

modello[['simulazione']][['garch_q2_p1']][['simmetrico']][['2']] <- append(modello[['simulazione']][['garch_q2_p1']][['simmetrico']][['2']], 
                                                                           list('lm'=Xt_t_student_symmetric_garch2_q2_p1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                                'Cullen-Frey'=Xt_t_student_symmetric_garch2_q2_p1_cf, 
                                                                                'Breusch-Pagan'=Xt_t_student_symmetric_garch2_q2_p1_bp, 'White'=Xt_t_student_symmetric_garch2_q2_p1_w, 
                                                                                'Ljiung-Box'=Xt_t_student_symmetric_garch2_q2_p1_lb, 'Dickey-Fuller'=Xt_t_student_symmetric_garch2_q2_p1_adf, 
                                                                                'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_garch2_q2_p1_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_garch2_q2_p1_sw,                                                                             'Generalized Distribution'=fitdist_test)),                                                                             'Generalized Distribution'=fitdist_test))

# In questo modello Garch(2,1) con distribuzione simmetrica si ha presenza di 
# omoschedasticità e assenza di autocorrelazione.
# Proviamo a stimare i miglior parametri per il modello:
q <- 2
p <- 1
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric2)
print(fit)
intconf = confint(fit)
show(intconf)

distribution <- dist_t_student_symmetric2
a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(distribution)
print((aq[1]+aq[2])*sigmasquaredW + b1)
Xt_t_student_symmetric_garch2_q2_p1_new <- model_garch(a0, aq, b1, X0, sigmasquared0, distribution, q, p)

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello GARCH(2,1)
Xt <- Xt_t_student_symmetric_garch2_q2_p1_new
df_Xt_t_student_symmetric_garch1_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_symmetric_garch2_q2_p1_sw <- shapiro.test(Xt)
show(Xt_t_student_symmetric_garch2_q2_p1_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_symmetric_garch2_q2_p1
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,1) with a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_garch2_q2_p1_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch2_q2_p1_sp)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch2_q2_p1_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch2_q2_p1)
summary(Xt_t_student_symmetric_garch2_q2_p1_lm)
summary(Xt_t_student_symmetric_garch2_q2_p1_lm$fitted.values)

Xt_t_student_symmetric_garch2_q2_p1_res <- Xt_t_student_symmetric_garch2_q2_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- skew <- DescTools::Skew(Xt_t_student_symmetric_garch2_q2_p1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew          lwr.ci      upr.ci 
# -0.255279475 -0.587126502 -0.004490806  
set.seed(123)
kurt <- kurt <- DescTools::Kurt(Xt_t_student_symmetric_garch2_q2_p1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#      kurt      lwr.ci      upr.ci 
#    2.013370 1.344181 3.064797

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_symmetric_garch2_q2_p1_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_symmetric_garch2_q2_p1_res
num_lags <- 5                   # Setting the lag parameter for the test.
Xt_t_student_symmetric_garch2_q2_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_symmetric_garch2_q2_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_symmetric_garch2_q2_p1_res   
Xt_t_student_symmetric_garch2_q2_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_symmetric_garch2_q2_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch2_q2_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,1) of a symmetic t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Linear Model Residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_garch2_q2_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch2_q2_p1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch2_q2_p1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(2,1) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_garch2_q2_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch2_q2_p1_sp)

plot(Xt_t_student_symmetric_garch2_q2_p1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_garch2_q2_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch2_q2_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch2_q2_p1)
show(Xt_t_student_symmetric_garch2_q2_p1_bp)
# Si ha un p-value di 8.935e-07 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch2_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch2_q2_p1)
show(Xt_t_student_symmetric_garch2_q2_p1_w)
# Si ha un p-value di 1.682e-06 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_garch2_q2_p1_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(2,1)")))
subtitle_content <- bquote(paste("t-student symmetric distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano entro 
# una banda ristretta

# Test Ljiung-box
y <- Xt_t_student_symmetric_garch2_q2_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 1.5367, df = 1, p-value = 0.2151
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 4  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_symmetric_garch2_q2_p1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_symmetric_garch2_q2_p1_lb)

modello[['stimati']][['garch_q2_p1']] <- append(modello[['stimati']][['garch_q2_p1']], 
                                                list('simmetrico'=list('Xt'=Xt_t_student_symmetric_garch2_q2_p1_new, 'a0'=a0, 'a1'=aq[1], 'a2'=aq[2], 'b1'=b1, 'q'=q, 'p'=p, 
                                                                        'stazionarietà'=stazionaietà, 'lm'=Xt_t_student_symmetric_garch2_q2_p1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                       'Cullen-Frey'=Xt_t_student_symmetric_garch2_q2_p1_cf, 
                                                                       'Breusch-Pagan'=Xt_t_student_symmetric_garch2_q2_p1_bp, 'White'=Xt_t_student_symmetric_garch2_q2_p1_w, 
                                                                       'Ljiung-Box'=Xt_t_student_symmetric_garch2_q2_p1_lb, 'Dickey-Fuller'=Xt_t_student_symmetric_garch2_q2_p1_adf, 
                                                                       'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_garch2_q2_p1_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_garch2_q2_p1_sw,                                                                             'Generalized Distribution'=fitdist_test)),                                                                             'Generalized Distribution'=fitdist_test)))

# Con la stima dei nuovi parametri, il modello Garch(2,1) con distribuzione t-student
# simmetrica ha presenza di eteroschedasticità nel modello e assenza di autocorrelazione
# eccetto per il lag 15

##########################################

# Consideriamo la prima traiettoia con distribuzione t-student asimmetrica di un modello GARCH(2,1)
Xt <- Xt_t_student_asymmetric_garch1_q2_p1
df_Xt_t_student_asymmetric_garch1_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_asymmetric_garch1_q2_p1_sw <- shapiro.test(Xt)
show(Xt_t_student_asymmetric_garch1_q2_p1_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_asymmetric_garch1_q2_p1
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,1) with a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_garch1_q2_p1_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch1_q2_p1_sp)
# La regression line tende ad essere leggermente inclinata e la LOESS coincide con
# questa linea.

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch1_q2_p1_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch1_q2_p1)
summary(Xt_t_student_asymmetric_garch1_q2_p1_lm)
summary(Xt_t_student_asymmetric_garch1_q2_p1_lm$fitted.values)

Xt_t_student_asymmetric_garch1_q2_p1_res <- Xt_t_student_asymmetric_garch1_q2_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- DescTools::Skew(Xt_t_student_asymmetric_garch1_q2_p1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# -2.061462 -2.458634 -1.771173
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_asymmetric_garch1_q2_p1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt   lwr.ci   upr.ci 
#6.671224 4.830661 9.393460

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_asymmetric_garch1_q2_p1_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_garch1_q2_p1_res
num_lags <- 5                   # Setting the lag parameter for the test.
Xt_t_student_asymmetric_garch1_q2_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_garch1_q2_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_asymmetric_garch1_q2_p1_res   
Xt_t_student_asymmetric_garch1_q2_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_garch1_q2_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch1_q2_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,1) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_asymmetric_garch1_q2_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch1_q2_p1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch1_q2_p1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(2,1) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_garch1_q2_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch1_q2_p1_sp)

plot(Xt_t_student_asymmetric_garch1_q2_p1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_garch1_q2_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q2_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q2_p1)
show(Xt_t_student_asymmetric_garch1_q2_p1_bp)
# Si ha un p-value di 0.01844 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q2_p1)
show(Xt_t_student_asymmetric_garch1_q2_p1_w)
# Si ha un p-value di 0.06222 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Nel test di Breusch-Pagan il p-value < 0.05 ed indica una presenza di eteroschedasticità nei residui del modello; ma,
# nel test di White il p-value > 0.05 ed indica che la presenza di eteroschedasticità è meno chiara nel momento in cui si considerano
# due gradi di libertà poichè non possiamo rifiutare l'ipotesi nulla di omoschedasticità.

# Plot of the autocorrelogram.
y <- Xt_t_student_asymmetric_garch1_q2_p1_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(2,1)")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori nei primi lag tendono a seguire
# un trend; mentre, dal lag 8 i valori oscillano all'interno dell'intervallo.

# Test Ljiung-box
y <- Xt_t_student_asymmetric_garch1_q2_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 24.414, df = 1, p-value = 7.771e-07
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 6  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_asymmetric_garch1_q2_p1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_asymmetric_garch1_q2_p1_lb)
# I risultati hanno un p-value < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

modello[['simulazione']][['garch_q2_p1']][['asimmetrico']][['1']] <- append(modello[['simulazione']][['garch_q2_p1']][['asimmetrico']][['1']], 
                                                                            list('lm'=Xt_t_student_asymmetric_garch1_q2_p1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                                 'Cullen-Frey'=Xt_t_student_asymmetric_garch1_q2_p1_cf, 
                                                                                 'Breusch-Pagan'=Xt_t_student_asymmetric_garch1_q2_p1_bp, 'White'=Xt_t_student_asymmetric_garch1_q2_p1_w, 
                                                                                 'Ljiung-Box'=Xt_t_student_asymmetric_garch1_q2_p1_lb, 'Dickey-Fuller'=Xt_t_student_asymmetric_garch1_q2_p1_adf, 
                                                                                 'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_garch1_q2_p1_kpss, 'Shapiro-Wilk'=Xt_t_student_asymmetric_garch1_q2_p1_sw,                                                                             'Generalized Distribution'=fitdist_test)),                                                                             'Generalized Distribution'=fitdist_test))

# In questo modello Garch(2,1) con una distribuzione t-student asimmetrica si ha presenza
# di omoschedasticità nel test di White e presenza di autocorrelazione.
# Quindi, proviamo a stimare i parametri che si adattano meglio al modello.
q <- 2
p <- 1
distribution <- dist_t_student_asymmetric1
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = distribution)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(distribution)
stazionarietà <- (aq[1]+aq[2])*sigmasquaredW + b1
print(paste("Verifico condizione di stazionerietà: ", stazionarietà))
Xt_t_student_asymmetric_garch1_q2_p1_new <- model_garch(a0, aq, b1, X0, sigmasquared0, distribution, q, p)

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello GARCH(2,1)
Xt <- Xt_t_student_asymmetric_garch1_q2_p1_new
df_Xt_t_student_asymmetric_garch1_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_asymmetric_garch1_q2_p1_sw <- shapiro.test(Xt)
show(Xt_t_student_asymmetric_garch1_q2_p1_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_asymmetric_garch1_q2_p1
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,1) with a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_garch1_q2_p1_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch1_q2_p1_sp)
# La regression line tende ad essere leggermente inclinata e la LOESS coincide con
# questa linea.

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch1_q2_p1_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch1_q2_p1)
summary(Xt_t_student_asymmetric_garch1_q2_p1_lm)
summary(Xt_t_student_asymmetric_garch1_q2_p1_lm$fitted.values)

Xt_t_student_asymmetric_garch1_q2_p1_res <- Xt_t_student_asymmetric_garch1_q2_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- DescTools::Skew(Xt_t_student_asymmetric_garch1_q2_p1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# -1.817504 -2.161507 -1.575212
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_asymmetric_garch1_q2_p1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt   lwr.ci   upr.ci 
#5.308088 3.831570 7.501694

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_asymmetric_garch1_q2_p1_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_garch1_q2_p1_res
num_lags <- 5                   # Setting the lag parameter for the test.
Xt_t_student_asymmetric_garch1_q2_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_garch1_q2_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_asymmetric_garch1_q2_p1_res   
Xt_t_student_asymmetric_garch1_q2_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_garch1_q2_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch1_q2_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,1) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_asymmetric_garch1_q2_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch1_q2_p1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch1_q2_p1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(2,1) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_garch1_q2_p1_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch1_q2_p1_sp)

plot(Xt_t_student_asymmetric_garch1_q2_p1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_garch1_q2_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q2_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q2_p1)
show(Xt_t_student_asymmetric_garch1_q2_p1_bp)
# Si ha un p-value di 0.000104 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q2_p1)
show(Xt_t_student_asymmetric_garch1_q2_p1_w)
# Si ha un p-value di 0.000536 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_asymmetric_garch1_q2_p1_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(2,1)")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori nei primi lag tendono a seguire
# un trend; mentre, dal lag 8 i valori oscillano all'interno dell'intervallo.

# Test Ljiung-box
y <- Xt_t_student_asymmetric_garch1_q2_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 4.0259, df = 1, p-value = 0.04481
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 6  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_asymmetric_garch1_q2_p1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_asymmetric_garch1_q2_p1_lb)
# I risultati hanno un p-value < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

modello[['stimati']][['garch_q2_p1']] <- append(modello[['stimati']][['garch_q2_p1']], 
                                                list('asimmetrico'=list('Xt'=Xt_t_student_asymmetric_garch1_q2_p1_new, 'a0'=a0, 'a1'=aq[1], 'a2'=aq[2], 'b1'=b1, 'q'=q, 'p'=p,
                                                                        'stazionarietà'=stazionaietà, 'lm'=Xt_t_student_asymmetric_garch1_q2_p1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                        'Cullen-Frey'=Xt_t_student_asymmetric_garch1_q2_p1_cf, 
                                                                        'Breusch-Pagan'=Xt_t_student_asymmetric_garch1_q2_p1_bp, 'White'=Xt_t_student_asymmetric_garch1_q2_p1_w, 
                                                                        'Ljiung-Box'=Xt_t_student_asymmetric_garch1_q2_p1_lb, 'Dickey-Fuller'=Xt_t_student_asymmetric_garch1_q2_p1_adf, 
                                                                        'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_garch1_q2_p1_kpss, 'Shapiro-Wilk'=Xt_t_student_asymmetric_garch1_q2_p1_sw,                                                                             'Generalized Distribution'=fitdist_test)),                                                                             'Generalized Distribution'=fitdist_test)))

# Con i parametri stimati, il modello Garch(2,1) con distribuzione t-student asimmetrica
# ha presenza di eteroschedasticità nei residui del modello e assenza di autocorrelazione
# eccetto nel lag 1 in cui il p-value < 0.05.

##########################################
##########################################

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(2,2)
Xt <- Xt_normal_garch1_q2_p2
df_Xt_normal_garch1_q2_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_normal_garch1_q2_p2_sw <- shapiro.test(Xt)
show(Xt_normal_garch1_q2_p2_sw)
# Si ha un p-value maggiore di 0.05 quindi possiamo dire che la serie è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_normal_garch1_q2_p2
lenh <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Line plot of the model Garch(2,2) with a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
df_Xt_normal_garch1_q2_p2_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(df_Xt_normal_garch1_q2_p2_sp)

# Consideriamo un modello lineare
Xt_normal_garch1_q2_p2_lm <- lm(Xt~t, data=df_Xt_normal_garch1_q2_p2)
summary(Xt_normal_garch1_q2_p2_lm)
summary(Xt_normal_garch1_q2_p2_lm$fitted.values)

Xt_normal_garch1_q2_p2_res <- Xt_normal_garch1_q2_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- DescTools::Skew(Xt_normal_garch1_q2_p2_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# -0.08014246 -0.23155545  0.06907189 
set.seed(123)
kurt <- DescTools::Kurt(Xt_normal_garch1_q2_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt      lwr.ci   upr.ci 
# -0.06302992 -0.33610769  0.32050405

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_normal_garch1_q2_p2_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_normal_garch1_q2_p2_res
num_lags <- 7                   # Setting the lag parameter for the test.
Xt_normal_garch1_q2_p2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_normal_garch1_q2_p2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_normal_garch1_q2_p2_res   
Xt_normal_garch1_q2_p2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_normal_garch1_q2_p2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch1_q2_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,2) with a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Linear Model Residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_garch1_q2_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch1_q2_p2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch1_q2_p2_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(2,2) with a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_garch1_q2_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch1_q2_p2_sp)

plot(Xt_normal_garch1_q2_p2_lm,1) # Residuals vs Fitted
plot(Xt_normal_garch1_q2_p2_lm,2) # Q-Q Residuals
plot(Xt_normal_garch1_q2_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch1_q2_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch1_q2_p2)
show(Xt_normal_garch1_q2_p2_bp)
# Si ha un p-value di 0.0005444 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch1_q2_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch1_q2_p2)
show(Xt_normal_garch1_q2_p2_w)
# Si ha un p-value di 0.00252 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_normal_garch1_q2_p2_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(2,2)")))
subtitle_content <- bquote(paste("Normal distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")

# Test Ljiung-box
y <- Xt_normal_garch1_q2_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 4.4317, df = 1, p-value = 0.03528
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 7  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_normal_garch1_q2_p2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_normal_garch1_q2_p2_lb)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag
# eccetto nel lag 1.

modello[['simulazione']][['garch_q2_p2']][['normale']][['1']] <- append(modello[['simulazione']][['garch_q2_p2']][['normale']][['1']], 
                                                                        list('lm'=Xt_normal_garch1_q2_p2_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                             'Cullen-Frey'=Xt_normal_garch1_q2_p2_cf, 
                                                                             'Breusch-Pagan'=Xt_normal_garch1_q2_p2_bp, 'White'=Xt_normal_garch1_q2_p2_w, 
                                                                             'Ljiung-Box'=Xt_normal_garch1_q2_p2_lb, 'Dickey-Fuller'=Xt_normal_garch1_q2_p2_adf, 
                                                                             'Kwiatowski-Phillips-Schmidt-Shin'=Xt_normal_garch1_q2_p2_kpss, 'Shapiro-Wilk'=Xt_normal_garch1_q2_p2_sw))

# Il modello Garch(2,2) con una distribuzione normale ha evidenza di eteroschedasticità
# nei residui del modello e assenza di autocorrelazione eccetto per il lag 1.
# Proviamo a stimare i parametri che si adattano meglio al modello.
q <- 2
p <- 2
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
bp <- c(coef(fit)[['beta1']], coef(fit)[['beta2']])
sigmasquaredW <- var(dist_normal1)
stazionarietà <- (a1[1]+aq[2])*sigmasquaredW + bp[1] + bp[2]
print(paste("Verifico condizione di stazionerietà: ", stazionarietà))
Xt_normal_garch1_q2_p2_new <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_normal1, q, p)

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(2,2)
Xt <- Xt_normal_garch1_q2_p2_new
df_Xt_normal_garch1_q2_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_normal_garch1_q2_p2_sw <- shapiro.test(Xt)
show(Xt_normal_garch1_q2_p2_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_normal_garch1_q2_p2
lenh <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Line plot of the model Garch(2,2) with a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
df_Xt_normal_garch1_q2_p2_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(df_Xt_normal_garch1_q2_p2_sp)

# Consideriamo un modello lineare
Xt_normal_garch1_q2_p2_lm <- lm(Xt~t, data=df_Xt_normal_garch1_q2_p2)
summary(Xt_normal_garch1_q2_p2_lm)
summary(Xt_normal_garch1_q2_p2_lm$fitted.values)

Xt_normal_garch1_q2_p2_res <- Xt_normal_garch1_q2_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- DescTools::Skew(Xt_normal_garch1_q2_p2_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# -0.04782991 -0.23058973  0.17510086
set.seed(123)
kurt <- DescTools::Kurt(Xt_normal_garch1_q2_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt      lwr.ci   upr.ci 
# 0.9834121 0.6180878 1.5050297

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_normal_garch1_q2_p2_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_normal_garch1_q2_p2_res
num_lags <- 7                   # Setting the lag parameter for the test.
Xt_normal_garch1_q2_p2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_normal_garch1_q2_p2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_normal_garch1_q2_p2_res   
Xt_normal_garch1_q2_p2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_normal_garch1_q2_p2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch1_q2_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,2) with a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Linear Model Residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_garch1_q2_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch1_q2_p2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch1_q2_p2_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(2,2) with a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_garch1_q2_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_normal_garch1_q2_p2_sp)

plot(Xt_normal_garch1_q2_p2_lm,1) # Residuals vs Fitted
plot(Xt_normal_garch1_q2_p2_lm,2) # Q-Q Residuals
plot(Xt_normal_garch1_q2_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch1_q2_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch1_q2_p2)
show(Xt_normal_garch1_q2_p2_bp)
# Si ha un p-value di 2.2e-16 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch1_q2_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch1_q2_p2)
show(Xt_normal_garch1_q2_p2_w)
# Si ha un p-value di 2.2e-16 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_normal_garch1_q2_p2_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(2,2)")))
subtitle_content <- bquote(paste("Normal distribution, path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")

# Test Ljiung-box
y <- Xt_normal_garch1_q2_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 1.4881, df = 1, p-value = 0.2225
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 7  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_normal_garch1_q2_p2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_normal_garch1_q2_p2_lb)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione

modello[['stimati']][['garch_q2_p2']] <- append(modello[['stimati']][['garch_q2_p2']], 
                                                list('normale'=list('Xt'=Xt_normal_garch1_q2_p2_new, 'a0'=a0, 'a1'=aq[1], 'a2'=aq[2], 'b1'=bp[1], 'b2'=bp[2], 'q'=q, 'p'=p,
                                                                    'stazionarietà'=stazionaietà, 'lm'=Xt_normal_garch1_q2_p2_lm,'skew'=skew, 'kurt'=kurt, 
                                                                   'Cullen-Frey'=Xt_normal_garch1_q2_p2_cf, 
                                                                   'Breusch-Pagan'=Xt_normal_garch1_q2_p2_bp, 'White'=Xt_normal_garch1_q2_p2_w, 
                                                                   'Ljiung-Box'=Xt_normal_garch1_q2_p2_lb, 'Dickey-Fuller'=Xt_normal_garch1_q2_p2_adf, 
                                                                   'Kwiatowski-Phillips-Schmidt-Shin'=Xt_normal_garch1_q2_p2_kpss, 'Shapiro-Wilk'=Xt_normal_garch1_q2_p2_sw)))

# In questo modello Garch(1,1) con distribuzione normale risulta essere eteroschedastico e con assenza
# di autocorrelazione con i parametri stimati.

##########################################

# Consideriamo la seconda traiettoia con distribuzione t-student simmetrica di un modello GARCH(2,2)
Xt <- Xt_t_student_symmetric_garch2_q2_p2
df_Xt_t_student_symmetric_garch2_q2_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_symmetric_garch2_q2_p2_sw <- shapiro.test(Xt)
show(Xt_t_student_symmetric_garch2_q2_p2_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_symmetric_garch2_q2_p2
lenh <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Line plot of the model Garch(2,2) with a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
df_Xt_t_student_symmetric_garch2_q2_p2_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(df_Xt_t_student_symmetric_garch2_q2_p2_sp)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch2_q2_p2_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch2_q2_p2)
summary(Xt_t_student_symmetric_garch2_q2_p2_lm)
summary(Xt_t_student_symmetric_garch2_q2_p2_lm$fitted.values)

Xt_t_student_symmetric_garch2_q2_p2_res <- Xt_t_student_symmetric_garch2_q2_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- skew <- DescTools::Skew(Xt_t_student_symmetric_garch2_q2_p2, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew          lwr.ci      upr.ci 
# 0.3374604 -0.4776944  1.3811634
set.seed(123)
kurt <- kurt <- DescTools::Kurt(Xt_t_student_symmetric_garch2_q2_p2, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#      kurt      lwr.ci      upr.ci 
#     7.592470  4.044877 11.725884

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_symmetric_garch2_q2_p2_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_symmetric_garch2_q2_p2_res
num_lags <- 7                   # Setting the lag parameter for the test.
Xt_t_student_symmetric_garch2_q2_p2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_symmetric_garch2_q2_p2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_symmetric_garch2_q2_p2_res   
Xt_t_student_symmetric_garch2_q2_p2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_symmetric_garch2_q2_p2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch2_q2_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,2) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Linear Model Residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_garch2_q2_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch2_q2_p2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch2_q2_p2_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(2,2) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_garch2_q2_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch2_q2_p2_sp)

plot(Xt_t_student_symmetric_garch2_q2_p2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_garch2_q2_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch2_q2_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch2_q2_p2)
show(Xt_t_student_symmetric_garch2_q2_p2_bp)
# Si ha un p-value di 0.08959 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch2_q2_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch2_q2_p2)
show(Xt_t_student_symmetric_garch2_q2_p2_w)
# Si ha un p-value di 0.08332 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_garch2_q2_p2_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(2,2)")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano all'interno 
# dell'intervallo eccetto nel lag 15

# Test Ljiung-box
y <- Xt_t_student_symmetric_garch2_q2_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 1.0338, df = 1, p-value = 0.3093
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 7  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_symmetric_garch2_q2_p2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_symmetric_garch2_q2_p2_lb)
# Nella forma estesa del test di Ljung-Box si hanno alcuni lag con un p-value < 0.05
# questo indica una possibile presenza di autocorrelazione

modello[['simulazione']][['garch_q2_p2']][['simmetrico']][['2']] <- append(modello[['simulazione']][['garch_q2_p2']][['simmetrico']][['2']], 
                                                                           list('lm'=Xt_t_student_symmetric_garch2_q2_p2_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                                'Cullen-Frey'=Xt_t_student_symmetric_garch2_q2_p2_cf, 
                                                                                'Breusch-Pagan'=Xt_t_student_symmetric_garch2_q2_p2_bp, 'White'=Xt_t_student_symmetric_garch2_q2_p2_w, 
                                                                                'Ljiung-Box'=Xt_t_student_symmetric_garch2_q2_p2_lb, 'Dickey-Fuller'=Xt_t_student_symmetric_garch2_q2_p2_adf, 
                                                                                'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_garch2_q2_p2_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_garch2_q2_p2_sw,                                                                             'Generalized Distribution'=fitdist_test)),                                                                             'Generalized Distribution'=fitdist_test))

# In questo modello Garch(2,2) con una distribuzione t-student simmetrica
# si ha presenza di omoschedasticità nei residui del modello e presenza di autocorrelazione.
# Proviamo a stimare i migliori parametri che si adattano meglio al modello
distribution <- dist_t_student_symmetric2
q <- 2
p <- 2
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = distribution)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
bp <- c(coef(fit)[['beta1']], coef(fit)[['beta2']])
sigmasquaredW <- var(distribution)
stazionarietà <- (aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2]
print(paste("Verifico condizione di stazionerietà: ", stazionarietà))  
Xt_t_student_symmetric_garch2_q2_p2_new <- model_garch(a0, aq, bp, X0, sigmasquared0, distribution, q, p)

# Consideriamo la seconda traiettoia con distribuzione t-student simmetrica di un modello GARCH(2,2)
Xt <- Xt_t_student_symmetric_garch2_q2_p2_new
df_Xt_t_student_symmetric_garch2_q2_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_symmetric_garch2_q2_p2_sw <- shapiro.test(Xt)
show(Xt_t_student_symmetric_garch2_q2_p2_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_symmetric_garch2_q2_p2
lenh <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Line plot of the model Garch(2,2) with a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
df_Xt_t_student_symmetric_garch2_q2_p2_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(df_Xt_t_student_symmetric_garch2_q2_p2_sp)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch2_q2_p2_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch2_q2_p2)
summary(Xt_t_student_symmetric_garch2_q2_p2_lm)
summary(Xt_t_student_symmetric_garch2_q2_p2_lm$fitted.values)

Xt_t_student_symmetric_garch2_q2_p2_res <- Xt_t_student_symmetric_garch2_q2_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- skew <- DescTools::Skew(Xt_t_student_symmetric_garch2_q2_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew          lwr.ci      upr.ci 
# -0.255462013 -0.602606510 -0.001996112 
set.seed(123)
kurt <- kurt <- DescTools::Kurt(Xt_t_student_symmetric_garch2_q2_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#      kurt      lwr.ci      upr.ci 
#     2.083156 1.391013 3.136304 

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_symmetric_garch2_q2_p2_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_symmetric_garch2_q2_p2_res
num_lags <- 7                   # Setting the lag parameter for the test.
Xt_t_student_symmetric_garch2_q2_p2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_symmetric_garch2_q2_p2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_symmetric_garch2_q2_p2_res   
Xt_t_student_symmetric_garch2_q2_p2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_symmetric_garch2_q2_p2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch2_q2_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,2) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Linear Model Residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_garch2_q2_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch2_q2_p2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch2_q2_p2_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(2,2) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_garch2_q2_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_symmetric_garch2_q2_p2_sp)

plot(Xt_t_student_symmetric_garch2_q2_p2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_garch2_q2_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch2_q2_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch2_q2_p2)
show(Xt_t_student_symmetric_garch2_q2_p2_bp)
# Si ha un p-value di 3.22e-07 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch2_q2_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch2_q2_p2)
show(Xt_t_student_symmetric_garch2_q2_p2_w)
# Si ha un p-value di 8.154e-07 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_garch2_q2_p2_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(2,2)")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano all'interno 
# dell'intervallo eccetto nel lag 15

# Test Ljiung-box
y <- Xt_t_student_symmetric_garch2_q2_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 1.0338, df = 1, p-value = 0.3093
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 7  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_symmetric_garch2_q2_p2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_symmetric_garch2_q2_p2_lb)
# Nella forma estesa del test di Ljung-Box si hanno alcuni lag con un p-value < 0.05
# questo indica una possibile presenza di autocorrelazione

modello[['stimati']][['garch_q2_p2']] <- append(modello[['stimati']][['garch_q2_p2']], 
                                                list('simmetrico'=list('Xt'=Xt_t_student_symmetric_garch2_q2_p2_new, 'a0'=a0, 'a1'=aq[1], 'a2'=aq[2], 'b1'=bp[1], 'b2'=bp[2], 'q'=q, 'p'=p,
                                                                       'stazionarietà'=stazionaietà, 'lm'=Xt_t_student_symmetric_garch2_q2_p2_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                       'Cullen-Frey'=Xt_t_student_symmetric_garch2_q2_p2_cf, 
                                                                       'Breusch-Pagan'=Xt_t_student_symmetric_garch2_q2_p2_bp, 'White'=Xt_t_student_symmetric_garch2_q2_p2_w, 
                                                                       'Ljiung-Box'=Xt_t_student_symmetric_garch2_q2_p2_lb, 'Dickey-Fuller'=Xt_t_student_symmetric_garch2_q2_p2_adf, 
                                                                       'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_garch2_q2_p2_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_garch2_q2_p2_sw,                                                                             'Generalized Distribution'=fitdist_test)),                                                                             'Generalized Distribution'=fitdist_test)))

# Con i nuovi parametri stimati, nel modello Garch(2,2) con una distribuzione t-student simmetrica
# si ha presenza di eteroschedasticità nei residui del modello e assenza di autocorrelazione.

##########################################

# Consideriamo la prima traiettoia con distribuzione t-student asimmetrica di un modello GARCH(2,2)
Xt <- Xt_t_student_asymmetric_garch1_q2_p2
df_Xt_t_student_asymmetric_garch1_q2_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_asymmetric_garch1_q2_p2_sw <- shapiro.test(Xt)
show(Xt_t_student_asymmetric_garch1_q2_p2_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_asymmetric_garch1_q2_p2
lenh <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Line plot of the model Garch(2,2) with a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
df_Xt_t_student_asymmetric_garch1_q2_p2_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(df_Xt_t_student_asymmetric_garch1_q2_p2_sp)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch1_q2_p2_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch1_q2_p2)
summary(Xt_t_student_asymmetric_garch1_q2_p2_lm)
summary(Xt_t_student_asymmetric_garch1_q2_p2_lm$fitted.values)

Xt_t_student_asymmetric_garch1_q2_p2_res <- Xt_t_student_asymmetric_garch1_q2_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- DescTools::Skew(Xt_t_student_asymmetric_garch1_q2_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# -1.965005 -2.317760 -1.697547 
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_asymmetric_garch1_q2_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt   lwr.ci   upr.ci 
#5.958485 4.327387 8.254191 

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_asymmetric_garch1_q2_p2_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_garch1_q2_p2_res
num_lags <- 7                   # Setting the lag parameter for the test.
Xt_t_student_asymmetric_garch1_q2_p2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_garch1_q2_p2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_asymmetric_garch1_q2_p2_res   
Xt_t_student_asymmetric_garch1_q2_p2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_garch1_q2_p2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch1_q2_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\"  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,2) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_asymmetric_garch1_q2_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch1_q2_p2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch1_q2_p2_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\"  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(2,2) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_garch1_q2_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch1_q2_p2_sp)

plot(Xt_t_student_asymmetric_garch1_q2_p2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_garch1_q2_p2_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q2_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q2_p2)
show(Xt_t_student_asymmetric_garch1_q2_p2_bp)
# Si ha un p-value di 0.01213 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q2_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q2_p2)
show(Xt_t_student_asymmetric_garch1_q2_p2_w)
# Si ha un p-value di 0.04304 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_asymmetric_garch1_q2_p2_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(2,2)")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori tendono a seguire un trend iniziale

# Test Ljiung-box
y <- Xt_t_student_asymmetric_garch1_q2_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 22.226, df = 1, p-value = 2.423e-06
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 7  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_asymmetric_garch1_q2_p2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_asymmetric_garch1_q2_p2_lb)
# Nella forma estesa del test di Ljung-Box si ha presenza di autocorrelazione

modello[['simulazione']][['garch_q2_p2']][['asimmetrico']][['1']] <- append(modello[['simulazione']][['garch_q2_p2']][['asimmetrico']][['1']], 
                                                                            list('lm'=Xt_t_student_asymmetric_garch1_q2_p2_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                                 'Cullen-Frey'=Xt_t_student_asymmetric_garch1_q2_p2_cf, 
                                                                                 'Breusch-Pagan'=Xt_t_student_asymmetric_garch1_q2_p2_bp, 'White'=Xt_t_student_asymmetric_garch1_q2_p2_w, 
                                                                                 'Ljiung-Box'=Xt_t_student_asymmetric_garch1_q2_p2_lb, 'Dickey-Fuller'=Xt_t_student_asymmetric_garch1_q2_p2_adf, 
                                                                                 'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_garch1_q2_p2_kpss, 'Shapiro-Wilk'=Xt_t_student_asymmetric_garch1_q2_p2_sw,                                                                             'Generalized Distribution'=fitdist_test)),                                                                             'Generalized Distribution'=fitdist_test))

# In questo modello Garch(2,2) con distribuzione t-student asimmetrica si ha presenza di 
# eteroschedasticità nel modello e presenza di autocorrelazione.
# Proviamo a stimare i migliori parametri che si adattano meglio al modello.
distribution <- dist_t_student_asymmetric1
q <- 2
p <- 2
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = distribution)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
bp <- c(coef(fit)[['beta1']], coef(fit)[['beta2']])
sigmasquaredW <- var(dist_t_student_asymmetric1)
stazionarietà <- (aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2]
print(paste("Verifico condizione di stazionerietà: ", stazionarietà))
Xt_t_student_asymmetric_garch1_q2_p2_new <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello GARCH(2,2)
Xt <- Xt_t_student_asymmetric_garch1_q2_p2_new
df_Xt_t_student_asymmetric_garch1_q2_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_t_student_asymmetric_garch1_q2_p2_sw <- shapiro.test(Xt)
show(Xt_t_student_asymmetric_garch1_q2_p2_sw)
# Si ha un p-value minore di 0.05 quindi possiamo dire che la serie non è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_t_student_asymmetric_garch1_q2_p2
lenh <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Line plot of the model Garch(2,2) with a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Samples")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
df_Xt_t_student_asymmetric_garch1_q2_p2_sp <- ggplot(Data_df) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(df_Xt_t_student_asymmetric_garch1_q2_p2_sp)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch1_q2_p2_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch1_q2_p2)
summary(Xt_t_student_asymmetric_garch1_q2_p2_lm)
summary(Xt_t_student_asymmetric_garch1_q2_p2_lm$fitted.values)

Xt_t_student_asymmetric_garch1_q2_p2_res <- Xt_t_student_asymmetric_garch1_q2_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
set.seed(123)
skew <- DescTools::Skew(Xt_t_student_asymmetric_garch1_q2_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# -1.817514 -2.161602 -1.575137  
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_asymmetric_garch1_q2_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt   lwr.ci   upr.ci 
#5.308298 3.831205 7.502219 

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_t_student_asymmetric_garch1_q2_p2_cf <- descdist(Xt, discrete=FALSE, boot=500)

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_garch1_q2_p2_res
num_lags <- 7                   # Setting the lag parameter for the test.
Xt_t_student_asymmetric_garch1_q2_p2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_garch1_q2_p2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_t_student_asymmetric_garch1_q2_p2_res   
Xt_t_student_asymmetric_garch1_q2_p2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_garch1_q2_p2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di non stazionarietà e concludere che la serie temporale è stazionaria 
# rispetto al livello di significatività specificato.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch1_q2_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\"  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,2) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_asymmetric_garch1_q2_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch1_q2_p2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch1_q2_p2_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\"  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(2,2) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
x_breaks_num <- 30
x_breaks_low <- Data_df$t[1]
x_breaks_up <- Data_df$t[length]
x_binwidth <- floor((x_breaks_up-x_breaks_low)/x_breaks_num)
x_breaks <- seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth)
if((max(x_breaks)-x_breaks_up)>x_binwidth/2){x_breaks <- c(x_breaks,x_breaks_up)}
x_labs <- paste(Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Square root of absolute residuals")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Samples")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_garch1_q2_p2_sp <- ggplot(Data_df) +
  geom_point(alpha=1, size=0.5, shape=19, aes(x=t, y=X, color="y1_col")) +
  geom_line(alpha=0.7, size=0.01, linetype="solid", aes(x=t, y=X, color="y1_col", group=1)) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=t, y=X, color="y3_col"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=TRUE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=t, y=X, color="y2_col"),
              method = "loess", formula = y ~ x, se=FALSE) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), 
                                                           linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(Xt_t_student_asymmetric_garch1_q2_p2_sp)

plot(Xt_t_student_asymmetric_garch1_q2_p2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_garch1_q2_p2_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q2_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q2_p2)
show(Xt_t_student_asymmetric_garch1_q2_p2_bp)
# Si ha un p-value di 0.0001036 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q2_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q2_p2)
show(Xt_t_student_asymmetric_garch1_q2_p2_w)
# Si ha un p-value di 0.000534 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_asymmetric_garch1_q2_p2_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model GARCH(2,2)")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width = 0.1, col="black", inherit.aes = TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend = TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend = TRUE, lty=4) + 
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend = TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=waiver(), label=waiver()) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis = sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="red", CI_95="blue", CI_99="green")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust = 0.5), 
        plot.subtitle=element_text(hjust =  0.5),
        plot.caption = element_text(hjust = 1.0),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
# nel grafico dell'autocorrelogramma possiamo notare che i valori tendono a seguire un trend iniziale

# Test Ljiung-box
y <- Xt_t_student_asymmetric_garch1_q2_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 22.226, df = 1, p-value = 2.423e-06
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 7  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_asymmetric_garch1_q2_p2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_asymmetric_garch1_q2_p2_lb)
# Nella forma estesa del test di Ljung-Box si ha presenza di autocorrelazione con u p-valie nel primo lag
# < 0.05

modello[['stimati']][['garch_q2_p2']] <- append(modello[['stimati']][['garch_q2_p2']], 
                                                list('asimmetrico'=list('Xt'=Xt_t_student_asymmetric_garch1_q2_p2_new, 'a0'=a0, 'a1'=aq[1], 'a2'=aq[2], 'b1'=bp[1], 'b2'=bp[2], 'q'=q, 'p'=p,
                                                                        'stazionarietà'=stazionaietà, 'lm'=Xt_t_student_asymmetric_garch1_q2_p2_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                        'Cullen-Frey'=Xt_t_student_asymmetric_garch1_q2_p2_cf, 
                                                                        'Breusch-Pagan'=Xt_t_student_asymmetric_garch1_q2_p2_bp, 'White'=Xt_t_student_asymmetric_garch1_q2_p2_w, 
                                                                        'Ljiung-Box'=Xt_t_student_asymmetric_garch1_q2_p2_lb, 'Dickey-Fuller'=Xt_t_student_asymmetric_garch1_q2_p2_adf, 
                                                                        'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_garch1_q2_p2_kpss, 'Shapiro-Wilk'=Xt_t_student_asymmetric_garch1_q2_p2_sw,                                                                             'Generalized Distribution'=fitdist_test)),                                                                             'Generalized Distribution'=fitdist_test)))

# Con i nuovi parametri stimati, rispetto al modello precedente, si ha un modello Garch(2,2)
# con una distribuzione t-student asimmetrica che ha evidenza di eteroschedasticità nel modello
# e assenza di autocorrelazione

#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################
