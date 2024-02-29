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
library(glogis) # generalized logistic distribution
library(pracma)
library(fGarch)

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
# mean: location parameter.
# sd: scale parameter.
# nu: shape parameter (degrees of freedom).
set.seed(10)
dist_t_student_symmetric1 <- rstd(n, mean = 0, sd = 1, nu=5)
set.seed(20)
dist_t_student_symmetric2 <- rstd(n, mean = 0, sd = 1, nu=5)
set.seed(30)
dist_t_student_symmetric3 <- rstd(n, mean = 0, sd = 1, nu=5)

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
# mean: location parameter.
# sd: scale parameter.
# nu: shape parameter (degrees of freedom).
# xi: skewness parameter.
set.seed(10)
dist_t_student_asymmetric1 <- rsstd(n, mean = 0, sd = 1, nu = 5, xi = -1.5)
set.seed(20)
dist_t_student_asymmetric2 <- rsstd(n, mean = 0, sd = 1, nu = 5, xi = -1.5)
set.seed(30)
dist_t_student_asymmetric3 <- rsstd(n, mean = 0, sd = 1, nu = 5, xi = -1.5)

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

Xt_normal_arch1_q1_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_normal_arch1_q1_cf <- append(Xt_normal_arch1_q1_cf, list(cf))
show(cf)
# min:  -2.063044   max:  1.635871 
# median:  -0.01028502 
# mean:  -0.007676261 
# estimated sd:  0.5890253 
# estimated skewness:  -0.05850557 
# estimated kurtosis:  2.832156 
# Dal grafico notiamo che le osservazioni seguono una distribuzine normale.

# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_normal_arch1_q1_res
show(c(mean(y),var(y)))
# [1]  1.512592e-17 3.458052e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] 9.887056e-18 1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_normal_arch1_q1_cf <- append(Xt_normal_arch1_q1_cf, list(cf))
show(cf)
# min:  -2.063044   max:  1.635871 
# median:  -0.01028502 
# mean:  -0.007676261 
# estimated sd:  0.5890253 
# estimated skewness:  -0.05850557 
# estimated kurtosis:  2.832156

# Esploriamo queste possibilità in più dettagli.
z_st_qemp <- EnvStats::qemp(stats::ppoints(z_st), z_st) # Empirical quantiles of the residuals.
z_st_demp <- EnvStats::demp(z_st_qemp, z_st)     # Empirical probability density of the residuals.
z_st_pemp <- EnvStats::pemp(z_st_qemp, z_st)     # Empirical distribution function of the residuals.  
x <- z_st_qemp
y_d <- z_st_demp
y_p <- z_st_pemp
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica
mu <- 0
alpha <- 1
beta <-2
Gen_Norm_Dens_Func <- bquote(paste("Gener. Normal Density Function, mu = ", .(mu),", alpha = ", .(alpha), ", beta = ", .(beta)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the GARCH(2,1) model with a normal distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, mean=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dgnorm(x, mu=0, alpha=1, beta=2), lwd=2, col= "red")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", Gen_Norm_Dens_Func), 
       col=c("darkblue", "darkgreen", "red"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
Gen_Norm_Distr_Func <-bquote(paste("Gener. Normal Distribution Function, mu = ", .(mu),", alpha = ", alpha, ", beta = ", .(beta)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(2,1) model with a normal distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, mean=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pgnorm(x, mu=0, alpha=1, beta=2), lwd=2, col= "red")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", Gen_Norm_Distr_Func), 
       col=c("darkblue", "darkgreen", "red"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

fitdist_test <- list()
# Come prima cosa, fittiamo la distribuzione normale generalizzata utilizzando la funzione fitdistrplus::fitdist().
fitdist_gnorm <- fitdistrplus::fitdist(z_st, "gnorm", start=list(mu=0, alpha=1, beta=1), method= "mle")
fitdist_test[["gnorm"]][["gnorm"]] <- fitdist_gnorm
summary(fitdist_gnorm)
# Fitting of the distribution ' gnorm ' by maximum likelihood 
# Parameters : 
#   estimate Std. Error
# mu    -0.002722894 0.04423086
# alpha  1.504666783 0.07544991
# beta   2.296981324 0.21957757
# Loglikelihood:  -707.9277   AIC:  1421.855   BIC:  1434.499 
# Correlation matrix:
#   mu       alpha        beta
# mu     1.00000000 -0.04686897 -0.05807108
# alpha -0.04686897  1.00000000  0.80847846
# beta  -0.05807108  0.80847846  1.00000000
#
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_gnorm_bd <- fitdistrplus::bootdist(fitdist_gnorm, niter=1000)
fitdist_test[["gnorm"]][["bootdist"]] <- fitdist_gnorm_bd
summary(fitdist_gnorm_bd)
# Parametric bootstrap medians and 95% percentile CI 
#             Median     2.5%     97.5%
# mu    -0.001464568 -0.08968968 0.08087496
# alpha  1.508524049  1.33825165 1.67126435
# beta   2.312148388  1.89663996 2.90909157
#
# We fix the initial points of the constrained maximization procedure
mu <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["mu"]][1]
alpha <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["alpha"]][1]
beta <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["beta"]][1]
show(c(mu,alpha,beta)) # the estimated parameters.
# 0.04423086 0.07544991 0.21957757

# Setting
mu <- fitdist_gnorm[["estimate"]][["mu"]]
alpha <- fitdist_gnorm[["estimate"]][["alpha"]]
beta <- fitdist_gnorm[["estimate"]][["beta"]]
# We plot the histogram and the empirical density function of the standardized residuals together with the density function of the estimated 
# generalized normal.
Gen_Norm_Dens_Func <- bquote(paste("Gener. Normal Density Function, mu = ", .(mu),", alpha = ", alpha, ", beta = ", .(beta)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the GARCH(2,1) model with a normal distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, mean=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dgnorm(x, mu=mu, alpha=alpha, beta=beta), lwd=2, col= "red")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", Gen_Norm_Dens_Func), 
       col=c("darkblue", "darkgreen", "red"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
Gen_Norm_Distr_Func <-bquote(paste("Gener. Normal Distribution Function, mu = ", .(mu),", alpha = ", alpha, ", beta = ", .(beta)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(2,1) model with a normal distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, mean=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pgnorm(x, mu=mu, alpha=alpha, beta=beta), lwd=2, col= "red")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", Gen_Norm_Distr_Func), 
       col=c("darkblue", "darkgreen", "red"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

# The Kolmogorov-Smirnov test in the library *stats*
mu <- fitdist_gnorm[["estimate"]][["mu"]]
alpha <- fitdist_gnorm[["estimate"]][["alpha"]]
beta <- fitdist_gnorm[["estimate"]][["beta"]]
KS_z_st_t_ls <- stats::ks.test(z_st, y="pgnorm", mu=mu, alpha=alpha, beta=beta, alternative= "two.sided")
fitdist_test[["gnorm"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  Xt_normal_arch1_q1
# D = 0.029899, p-value = 0.7627
# alternative hypothesis: two-sided
# Non possiamo rigettare l'ipotesi nulla, quindi sembrerebbe che la serie segua una distribuzione normale
# confermando il grafico di Cullen-Frey.

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
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
                                                                         'Kwiatowski-Phillips-Schmidt-Shin'=Xt_normal_arch1_q1_kpss, 'Shapiro-Wilk'=Xt_normal_arch1_q1_sw,
                                                                         'Distribution test'=fitdist_test))

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
# Calcoliamo skew e kurtosi
set.seed(123)
skew <- DescTools::Skew(Xt_normal_arch1_q1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew          lwr.ci      upr.ci 
#-0.04892516 -0.16710693  0.06090632
kurt <- list()
set.seed(123)
k  <- DescTools::Kurt(Xt_normal_arch1_q1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
kurt[['0.99']] <- k
show(k)
#      kurt      lwr.ci      upr.ci 
# -0.3227862 -0.6341239  0.1911353 
set.seed(123)
k  <- DescTools::Kurt(Xt_normal_arch1_q1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
kurt[['0.80']] <- k
show(k)
#      kurt     lwr.ci      upr.ci 
# -0.3227862 -0.4773869 -0.1035970 

Xt_normal_arch1_q1_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_normal_arch1_q1_cf <- append(Xt_normal_arch1_q1_cf, list(cf))
show(cf)
# min:  -3.087656   max:  2.750387 
# median:  -0.01841802 
# mean:  -0.009551976 
# estimated sd:  1.029645 
# estimated skewness:  -0.02289063 
# estimated kurtosis:  2.678415 
# Dal grafico notiamo che le osservazioni seguono una distribuzine normale.

# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_normal_arch1_q1_res
show(c(mean(y),var(y)))
# [1]  3.989864e-18 1.055981e+00
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] 7.265239e-19 1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_normal_arch1_q1_cf <- append(Xt_normal_arch1_q1_cf, list(cf))
show(cf)
# min:  -3.087656   max:  2.750387 
# median:  -0.01841802 
# mean:  -0.009551976 
# estimated sd:  1.029645 
# estimated skewness:  -0.02289063 
# estimated kurtosis:  2.678415 

# Esploriamo queste possibilità in più dettagli.
z_st_qemp <- EnvStats::qemp(stats::ppoints(z_st), z_st) # Empirical quantiles of the residuals.
z_st_demp <- EnvStats::demp(z_st_qemp, z_st)     # Empirical probability density of the residuals.
z_st_pemp <- EnvStats::pemp(z_st_qemp, z_st)     # Empirical distribution function of the residuals.  
x <- z_st_qemp
y_d <- z_st_demp
y_p <- z_st_pemp
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica
mu <- 0
alpha <-sqrt(2)
beta <-1
Gen_Norm_Dens_Func <- bquote(paste("Gener. Normal Density Function, mu = ", .(mu),", alpha = ", alpha, ", beta = ", .(beta)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the GARCH(2,1) model with a normal distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, mean=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dgnorm(x, mu=0, alpha=1, beta=2), lwd=2, col= "red")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", Gen_Norm_Dens_Func), 
       col=c("darkblue", "darkgreen", "red"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
Gen_Norm_Distr_Func <-bquote(paste("Gener. Normal Distribution Function, mu = ", .(mu),", alpha = ", alpha, ", beta = ", .(beta)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(2,1) model with a normal distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, mean=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pgnorm(x, mu=0, alpha=1, beta=2), lwd=2, col= "red")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", Gen_Norm_Distr_Func), 
       col=c("darkblue", "darkgreen", "red"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

fitdist_test <- list()
# Come prima cosa, fittiamo la distribuzione normale generalizzata utilizzando la funzione fitdistrplus::fitdist().
fitdist_gnorm <- fitdistrplus::fitdist(z_st, "gnorm", start=list(mu=0, alpha=1, beta=1), method= "mle")
fitdist_test[["gnorm"]][["gnorm"]] <- fitdist_gnorm
summary(fitdist_gnorm)
# Fitting of the distribution ' gnorm ' by maximum likelihood 
# Parameters : 
#   estimate Std. Error
# mu    -0.002953504 0.04378516
# alpha  1.544000973 0.07473609
# beta   2.461769281 0.25089416
# Loglikelihood:  -706.8794   AIC:  1419.759   BIC:  1432.403 
# Correlation matrix:
#   mu       alpha        beta
# mu     1.00000000 -0.04057593 -0.05015944
# alpha -0.04057593  1.00000000  0.80823742
# beta  -0.05015944  0.80823742  1.00000000
#
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_gnorm_bd <- fitdistrplus::bootdist(fitdist_gnorm, niter=1000)
fitdist_test[["gnorm"]][["bootdist"]] <- fitdist_gnorm_bd
summary(fitdist_gnorm_bd)
# Parametric bootstrap medians and 95% percentile CI 
#             Median     2.5%     97.5%
# mu    -0.001556471 -0.08991733 0.08030943
# alpha  1.547889241  1.38326952 1.70431714
# beta   2.480690091  2.02556193 3.13902092
#
# We fix the initial points of the constrained maximization procedure
mu <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["mu"]][1]
alpha <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["alpha"]][1]
beta <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["beta"]][1]
show(c(mu,alpha,beta)) # the estimated parameters.
# 0.04378516 0.07473609 0.25089416

# Setting
mu <- fitdist_gnorm[["estimate"]][["mu"]]
alpha <- fitdist_gnorm[["estimate"]][["alpha"]]
beta <- fitdist_gnorm[["estimate"]][["beta"]]
# We plot the histogram and the empirical density function of the standardized residuals together with the density function of the estimated 
# generalized normal.
Gen_Norm_Dens_Func <- bquote(paste("Gener. Normal Density Function, mu = ", .(mu),", alpha = ", alpha, ", beta = ", .(beta)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the GARCH(2,1) model with a normal distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, mean=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dgnorm(x, mu=mu, alpha=alpha, beta=beta), lwd=2, col= "red")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", Gen_Norm_Dens_Func), 
       col=c("darkblue", "darkgreen", "red"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
Gen_Norm_Distr_Func <-bquote(paste("Gener. Normal Distribution Function, mu = ", .(mu),", alpha = ", alpha, ", beta = ", .(beta)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(2,1) model with a normal distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, mean=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pgnorm(x, mu=mu, alpha=alpha, beta=beta), lwd=2, col= "red")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", Gen_Norm_Distr_Func), 
       col=c("darkblue", "darkgreen", "red"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

# The Kolmogorov-Smirnov test in the library *stats*
mu <- fitdist_gnorm[["estimate"]][["mu"]]
alpha <- fitdist_gnorm[["estimate"]][["alpha"]]
beta <- fitdist_gnorm[["estimate"]][["beta"]]
KS_z_st_t_ls <- stats::ks.test(z_st, y="pgnorm", mu=mu, alpha=alpha, beta=beta, alternative= "two.sided")
fitdist_test[["gnorm"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  Xt_normal_arch1_q1
# D = 0.027329, p-value = 0.8493
# alternative hypothesis: two-sided
# Non possiamo rigettare l'ipotesi nulla, quindi sembrerebbe che la serie segua una distribuzione normale
# confermando il grafico di Cullen-Frey.

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
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
                                                               'Kwiatowski-Phillips-Schmidt-Shin'=Xt_normal_arch1_q1_kpss, 'Shapiro-Wilk'=Xt_normal_arch1_q1_sw,
                                                               'Distribution test'=fitdist_test)))

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
# 0.1799107 -0.1237301  0.4423559
set.seed(123)
kurt <- kurt <- DescTools::Kurt(Xt_t_student_symmetric_arch1_q1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
show(kurt)
#      kurt      lwr.ci      upr.ci 
# 1.6321276 0.5812969 3.4699081  
# All'1% di significatività l'eccesso di kurtosi non è zero. 

Xt_t_student_symmetric_arch1_q1_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_t_student_symmetric_arch1_q1_cf <- append(Xt_t_student_symmetric_arch1_q1_cf, list(cf))
show(cf)
# summary statistics
# ------
#  min:  -2.510932   max:  2.157534 
# median:  0.001382529 
# mean:  -0.0006129684 
# estimated sd:  0.5686051 
# estimated skewness:  0.1799107 
# estimated kurtosis:  4.632128 
# Conferma visiva che l'ipotesi nulla di normalità bisogna scartarla all'1% di significatività 
# quindi possiamo dire che è una distribuzione simmetrica con una forte kurtosi.

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_symmetric_arch1_q1_res
show(c(mean(y),var(y)))
# [1] -1.362322e-17  3.231065e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] -2.379694e-17  1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, graph=TRUE, boot=5000)
Xt_t_student_symmetric_arch1_q1_cf <- append(Xt_t_student_symmetric_arch1_q1_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -4.419501   max:  3.793315 
# median:  -0.01652876 
# mean:  -2.379694e-17 
# estimated sd:  1 
# estimated skewness:  0.1606417 
# estimated kurtosis:  4.634281 
# Da Cullen-Frey, abbiamo una possibile distribuzione logistica o di student per i residui.
  
# Esploriamo queste possibilità in più dettagli.
z_st_qemp <- EnvStats::qemp(stats::ppoints(z_st), z_st) # Empirical quantiles of the residuals.
z_st_demp <- EnvStats::demp(z_st_qemp, z_st)     # Empirical probability density of the residuals.
z_st_pemp <- EnvStats::pemp(z_st_qemp, z_st)     # Empirical distribution function of the residuals.  
x <- z_st_qemp
y_d <- z_st_demp
y_p <- z_st_pemp
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata con il parametri location=0,
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
lines(x, dstd(x, mean=0, sd=1, nu=5), lwd=2, col= "red")
lines(x, dglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", "Student Density Function, df=5", Gen_Log_Dens_Func), 
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
lines(x, pstd(x, mean = 0, sd = 1, nu = 5), lwd=2, col= "red")
lines(x, pglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=5", Gen_Log_Distr_Func), 
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
# location -0.1014806 0.14664491
# scale     0.5643380 0.03731272
# shape     1.1159704 0.18625469
#  Loglikelihood:  -697.0939   AIC:  1400.188   BIC:  1412.832 
# Correlation matrix:
#   location      scale      shape
# location  1.0000000 -0.7977072 -0.9578733
# scale    -0.7977072  1.0000000  0.8264219
# shape    -0.9578733  0.8264219  1.0000000
#
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_glogis_bd <- fitdistrplus::bootdist(fitdist_glogis, niter=1000)
fitdist_test[["glogis"]][["bootdist"]] <- fitdist_glogis_bd
summary(fitdist_glogis_bd)
# Parametric bootstrap medians and 95% percentile CI 
# Median       2.5%     97.5%
# location -0.1128609 -0.4616263 0.1651183
# scale     0.5625120  0.4907607 0.6442987
# shape     1.1275542  0.8038518 1.6826028
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
# -0.1017126  0.5643816  1.1161846 

# Dato che la Generalized Student distribution ha il parametro location che coincide con la media, possiamo provare 
# a fittare i dati con una generalized Student distribution con zero location. 
# Questo viene fatto cambiando il parametro m nella funzione *fitdistrplus::fitdist*.
# fitdist_t_ls <- fitdistrplus::fitdist(z_st, dstd , start=list(s=1, df=5), fix.arg=list(m=0), method= "mle")
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "std", start=list(nu=3.2), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' std ' by maximum likelihood 
# Parameters : 
#   estimate Std. Error
# nu 6.466067   1.474759
# Fixed parameters:
#   value
# mean     0
# sd       1
# Loglikelihood:  -697.9264   AIC:  1397.853   BIC:  1402.067 
abs(5-5.841794) # 0.841794

# Fitting of the distribution ' std  ' by maximum likelihood 
# Parameters : 
#   estimate   Std. Error
# s  0.8116467 0.04316767
# df 5.7749758 1.42428524
# Fixed parameters:
#     value
# m     0
# Loglikelihood:  -694.9214   AIC:  1393.843   BIC:  1402.272 
#Correlation matrix:
#       s       df
# s  1.0000000 0.6807098
# df 0.6807098 1.000000

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, graph=TRUE, boot=5000)
Xt_t_student_symmetric_arch1_q1_cf <- append(Xt_t_student_symmetric_arch1_q1_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -4.514675   max:  3.617464 
# median:  -0.01505772 
# mean:  6.457617e-18 
# estimated sd:  1 
# estimated skewness:  0.1488536 
# estimated kurtosis:  4.569972 

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
# -0.1015  0.5643  1.1160
#
m <- fitdist_t_ls[["fix.arg"]][["mean"]]
sd <- fitdist_t_ls[["fix.arg"]][["sd"]]
df <- round(fitdist_t_ls[["estimate"]][["nu"]], 4)
#
Gen_Log_Dens_Func <- bquote(paste("Estim. Logistic Density Function, location = ", .(loc),", scale = ", scl, ", shape = ", .(shp)))
Gen_Stud_Dens_Func <- bquote(paste("Estim. Student Density Function, m = ", .(m),", sd = ", sd, ", df = ", .(df)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the ARCH(1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dstd(x, mean=m, sd = sd, nu=df), lwd=2, col= "red")
lines(x, dglogis(x, location=loc, scale=scl, shape=shp), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", Gen_Stud_Dens_Func, Gen_Log_Dens_Func), 
       col=c("darkblue", "darkgreen", "red","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
Gen_Log_Distr_Func <-bquote(paste("Estim. Logistic Distribution Function, location = ", .(loc),", scale = ", scl, ", shape = ", .(shp)))
Gen_Stud_Distr_Func <-bquote(paste("Estim. Student Distribution Function, location = ", .(m),", scale = ", s, ", df = ", .(df)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the ARCH(1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pstd(x, mean=m, sd = sd, nu=df), lwd=2, col= "red")
lines(x, pglogis(x, location=loc, scale=scl, shape=shp), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", Gen_Stud_Distr_Func, Gen_Log_Distr_Func), 
       col=c("darkblue", "darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

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
# D = 0.019612, p-value = 0.9906
# alternative hypothesis: two-sided
# Con un p-value cosi alto, non possiamo rigettare l'ipotesi nulla. Di conseguenza, abbiamo una prova sufficiente
# che la serie è una distribuzione logistica generalizzata.
m <- fitdist_t_ls[["fix.arg"]][["mean"]]
sd <- fitdist_t_ls[["fix.arg"]][["sd"]]
df <- round(fitdist_t_ls[["estimate"]][["nu"]], 4)
show(c(m, sd, df))
KS_z_st_t_ls <- stats::ks.test(z_st, y="pstd", mean=m, sd=sd, nu=df, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.02121, p-value = 0.9781
# alternative hypothesis: two-sided
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.

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
# with parameters location = -0.1115, scale = 0.5678, shape = 1.1285
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.018408, p-value = 0.9983
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- fitdist_t_ls[["fix.arg"]][["mean"]]
sd <- fitdist_t_ls[["fix.arg"]][["sd"]]
df <- round(fitdist_t_ls[["estimate"]][["nu"]], 4)
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "pstd", mean=m, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 6.4661
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.026699, p-value = 0.9858
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.
loc <- round(fitdist_glogis[["estimate"]][["location"]],4)
scl <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shp <- round(fitdist_glogis[["estimate"]][["shape"]],4)
AD_z_st_glogis <- goftest::ad.test(z_st, null= "pglogis", location=location, scale=scale, shape=shape, estimated=FALSE)
fitdist_test[["glogis"]][["Anderson-Darling"]] <- AD_z_st_glogis
show(AD_z_st_glogis)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘pglogis’
# with parameters location = -0.111500335995556, scale = 0.567771399622821, shape = 1.12850503676796
# Parameters assumed to be fixed
# data:  z_st
# An = 0.17159, p-value = 0.9963
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- fitdist_t_ls[["fix.arg"]][["mean"]]
sd <- fitdist_t_ls[["fix.arg"]][["sd"]]
df <- round(fitdist_t_ls[["estimate"]][["nu"]], 4)
AD_z_st_t_ls <- goftest::ad.test(z_st, null="pstd", mean=m, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 6.4661
# Parameters assumed to be fixed
# data:  z_st
# An = 0.2002, p-value = 0.9904
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata

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
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
# Si ha un p-value di 0.003575 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_arch1_q1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_arch1_q1)
show(Xt_t_student_symmetric_arch1_q1_w)
# Si ha un p-value di 0.01421 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 0.16224, df = 1, p-value = 0.6871
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 3  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
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
skew <- DescTools::Skew(Xt_t_student_symmetric_arch1_q1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(skew)
#  skew       lwr.ci      upr.ci 
# 0.1799107 -0.1237301  0.4423559 
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_symmetric_arch1_q1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt      lwr.ci   upr.ci 
# 1.632128 1.029260 2.417568 

Xt_t_student_symmetric_arch1_q1_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_t_student_symmetric_arch1_q1_cf <- append(Xt_t_student_symmetric_arch1_q1_cf, list(cf))
show(cf)
# summary statistics
# ------
#  min:  -4.358866   max:  3.530716 
# median: 0.002402829 
# mean: 0.0001143497 
# estimated sd:  0.966715 
# estimated skewness:  0.1700341 
# estimated kurtosis:  4.57074 
# Conferma visiva che l'ipotesi nulla di normalità bisogna scartarla all'1% di significatività 
# quindi possiamo dire che è una distribuzione simmetrica con una forte kurtosi.

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_symmetric_arch1_q1_res
show(c(mean(y),var(y)))
# [1] 8.495375e-18 9.337522e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] 6.457617e-18 1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, graph=TRUE, boot=5000)
Xt_t_student_symmetric_arch1_q1_cf <- append(Xt_t_student_symmetric_arch1_q1_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -4.514675   max:  3.617464 
# median:  -0.01505772 
# mean:  6.457617e-18 
# estimated sd:  1 
# estimated skewness:  0.1488536 
# estimated kurtosis:  4.569972
# Da Cullen-Frey, abbiamo una possibile distribuzione logistica o di student per i residui.

# Esploriamo queste possibilità in più dettagli.
z_st_qemp <- EnvStats::qemp(stats::ppoints(z_st), z_st) # Empirical quantiles of the residuals.
z_st_demp <- EnvStats::demp(z_st_qemp, z_st)     # Empirical probability density of the residuals.
z_st_pemp <- EnvStats::pemp(z_st_qemp, z_st)     # Empirical distribution function of the residuals.  
x <- z_st_qemp
y_d <- z_st_demp
y_p <- z_st_pemp
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata con il parametri location=0,
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
lines(x, dstd(x, mean=0, sd=1, nu=5), lwd=2, col= "red")
lines(x, dglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", "Student Density Function, df=5", Gen_Log_Dens_Func), 
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
lines(x, pstd(x, mean = 0, sd = 1, nu = 5), lwd=2, col= "red")
lines(x, pglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=5", Gen_Log_Distr_Func), 
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
# location -0.1115003 0.14918042
# scale     0.5677714 0.03765475
# shape     1.1285050 0.19054318
# Loglikelihood:  -697.8684   AIC:  1401.737   BIC:  1414.381 
# Correlation matrix:
#   location      scale      shape
# location  1.0000000 -0.8006856 -0.9590605
# scale    -0.8006856  1.0000000  0.8279624
# shape    -0.9590605  0.8279624  1.0000000
#
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_glogis_bd <- fitdistrplus::bootdist(fitdist_glogis, niter=1000)
fitdist_test[["glogis"]][["bootdist"]] <- fitdist_glogis_bd
summary(fitdist_glogis_bd)
# Parametric bootstrap medians and 95% percentile CI 
#             Median       2.5%     97.5%
# location -0.1229266 -0.4762804 0.1581700
# scale     0.5660481  0.4937498 0.6479042
# shape     1.1400779  0.8119753 1.7076289
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
#   -0.1109670  0.5676041  1.1277079 

# Dato che la Generalized Student distribution ha il parametro location che coincide con la media, possiamo provare 
# a fittare i dati con una generalized Student distribution con zero location. 
# Questo viene fatto cambiando il parametro m nella funzione *fitdistrplus::fitdist*.
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "std", start=list(nu=3.2), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' t_ls ' by maximum likelihood 
# Parameters : 
#      estimate Std. Error
# nu   6.466067   1.474759
# Fixed parameters:
#        value
# mean     0
# sd       1
# Loglikelihood:  -697.9264   AIC:  1397.853   BIC:  1402.067 
#
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_t_ls_bd <- fitdistrplus::bootdist(fitdist_t_ls, niter=1000)
fitdist_test[["gstudent"]][["bootdist"]] <- fitdist_t_ls_bd
summary(fitdist_t_ls_bd)
# Parametric bootstrap medians and 95% percentile CI 
#  Median       2.5%     97.5%
# 6.522191  4.601135 12.757373
#
# We fix the initial points of the constrained maximization procedure
m <- 0
sd <- 1
df <- fitdist_t_ls_bd[["fitpart"]][["estimate"]][1]
minus_logLik <- function(x) -sum(log(dstd(z_st, mean=x[1], sd=x[2], nu=x[3]))) # the log-likelihood of the generalized logistic
# distribution.
fminunc_result <- fminunc(x0=c(m, sd, df), fn=minus_logLik)   # the minimization procedure where (location,scale,shape) is the 
# starting point.
show(c(fminunc_result$par[1],fminunc_result$par[2],fminunc_result$par[3])) # the estimated parameters.
#     m             s          df 
# -0.01282994  1.00251199  6.39827413  

loc <- round(location,4)
scl <- round(scale,4)
shp <- round(shape,4)
show(c(loc,scl,shp))
# -0.1115   0.5678   1.1285
#
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']],4)
#
Gen_Log_Dens_Func <- bquote(paste("Estim. Logistic Density Function, location = ", .(loc),", scale = ", scl, ", shape = ", .(shp)))
Gen_Stud_Dens_Func <- bquote(paste("Estim. Student Density Function, mean = ", .(m),", sd = ", sd, ", df = ", .(df)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the ARCH(1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dstd(x, mean=m, sd=sd, nu=df), lwd=2, col= "red")
lines(x, dglogis(x, location=loc, scale=scl, shape=shp), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", Gen_Stud_Dens_Func, Gen_Log_Dens_Func), 
       col=c("darkblue", "darkgreen", "red","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
Gen_Log_Distr_Func <-bquote(paste("Estim. Logistic Distribution Function, location = ", .(loc),", scale = ", scl, ", shape = ", .(shp)))
Gen_Stud_Distr_Func <-bquote(paste("Estim. Student Distribution Function, location = ", .(m),", scale = ", s, ", df = ", .(df)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the ARCH(1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pstd(x, mean=m, sd=sd, nu=df), lwd=2, col= "red")
lines(x, pglogis(x, location=loc, scale=scl, shape=shp), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", Gen_Stud_Distr_Func, Gen_Log_Distr_Func), 
       col=c("darkblue", "darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

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
# D = 0.019612, p-value = 0.9906
# alternative hypothesis: two-sided
# Con un p-value cosi alto, non possiamo rigettare l'ipotesi nulla. Di conseguenza, abbiamo una prova sufficiente
# che la serie è una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']],4)
KS_z_st_t_ls <- stats::ks.test(z_st, y="pstd", m=m, sd=sd, nu=df, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.02121, p-value = 0.9781
# alternative hypothesis: two-sided
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.

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
# with parameters location = -0.1115, scale = 0.5678, shape = 1.1285
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.018408, p-value = 0.9983
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "pstd", mean=m, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘std’
# with parameters mean = 0, sd = 1, nu = 6
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.031914, p-value = 0.9696
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.

# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘pglogis’
# with parameters location = -0.111500335995556, scale = 0.567771399622821, shape = 1.12850503676796
# Parameters assumed to be fixed
# data:  z_st
# An = 0.17159, p-value = 0.9963
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "pstd", mean=m, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 6
# Parameters assumed to be fixed
# data:  z_st
# An = 0.22038, p-value = 0.9838
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata

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
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
# Si ha un p-value di 0.003575 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_arch1_q1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_arch1_q1)
show(Xt_t_student_symmetric_arch1_q1_w)
# Si ha un p-value di 0.01421 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 0.16224, df = 1, p-value = 0.6871
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 3  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_symmetric_arch1_q1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_symmetric_arch1_q1_lb)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag considerati.

modello[['stimati']][['arch_q1']] <- append(modello[['stimati']][['arch_q1']], list('simmetrico'=list('Xt'=Xt_t_student_symmetric_arch1_q1_new, 'a0'=a0, 'a1'=a1, 'q'=q, 
                                                                                           'stazionarietà'=stazionaietà, 'lm'=Xt_t_student_symmetric_arch1_q1_lm, 
                                                                                           'skew'=skew, 'kurt'=kurt, 'Cullen-Frey'=Xt_t_student_symmetric_arch1_q1_cf, 
                                                                                           'Breusch-Pagan'=Xt_t_student_symmetric_arch1_q1_bp,'White'=Xt_t_student_symmetric_arch1_q1_w, 
                                                                                           'Ljiung-Box'=Xt_t_student_symmetric_arch1_q1_lb, 'Dickey-Fuller'=Xt_t_student_symmetric_arch1_q1_adf, 
                                                                                           'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_arch1_q1_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_arch1_q1_sw,                                                                             
                                                                                           'Generalized Distribution'=fitdist_test)))

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
# -1.260001 -1.550350 -1.035570  
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_asymmetric_arch1_q1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt   lwr.ci   upr.ci 
# 3.182414 2.339964 4.608794 

Xt_t_student_asymmetric_arch1_q1_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_t_student_asymmetric_arch1_q1_cf <- append(Xt_t_student_asymmetric_arch1_q1_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -2.966366   max:  1.479536 
# median:  0.05766362 
# mean:  -0.006708226 
# estimated sd:  0.6129858 
# estimated skewness:  -1.260001 
# estimated kurtosis:  6.182414 

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_asymmetric_arch1_q1_res
show(c(mean(y),var(y)))
# [1] 1.142858e-17 3.755527e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] 1.513655e-17 1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, graph=TRUE, boot=5000)
Xt_t_student_asymmetric_arch1_q1_cf <- append(Xt_t_student_asymmetric_arch1_q1_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -4.840613   max:  2.439973 
# median:  0.110772 
# mean:  1.513655e-17 
# estimated sd:  1 
# estimated skewness:  -1.242898 
# estimated kurtosis:  6.155451  

fitdist_test <- list()
# Libreria fGarch è stata riadattata per i modelli Garch.
# Dato che la Generalized Student distribution ha il parametro location che coincide con la media, possiamo provare 
# a fittare i dati con una generalized Student distribution con zero location. 
# Questo viene fatto cambiando il parametro m nella funzione *fitdistrplus::fitdist*.
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "sstd", start=list(nu = 5, xi = 1.5), fix.arg=list(mean = 0, sd = 1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' t_ls ' by maximum likelihood 
# Parameters : 
#    estimate  Std. Error
# nu 5.1450898 0.81059110
# xi 0.6988761 0.04279706
# Fixed parameters:
#   value
# mean     0
# sd       1
# Loglikelihood:  -664.1924   AIC:  1332.385   BIC:  1340.814 
# Correlation matrix:
#      nu         xi
# nu  1.0000000 -0.2922686
# xi -0.2922686  1.0000000

# Alla fine, consideriamo il test Goodness of fit.
# The Kolmogorov-Smirnov test in the library *stats*
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="psstd", mean=m, sd=sd, nu=df, xi=xi, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.068601, p-value = 0.01808
# alternative hypothesis: two-sided
# Possiamo rigettare l'ipotesi nulla al 5% di significatività che i residui standardizzati hanno una 
# distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.
# Non possiamo rigettare l'ipotesi nulla al 1% di livello di significatività.

# Cramer-von Mises test of goodness-of-fit
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "psstd", mean=0, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 5, xi = 1m = 0, s = -0.1062, df = 0.5659
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.81055, p-value = 0.006876
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "psstd", mean=m, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 5, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# An = 4.396, p-value = 0.005602
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_arch1_q1_res
num_lags <- 3                 # Setting the lag parameter for the test.
Xt_t_student_asymmetric_arch1_q1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_arch1_q1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_t_student_asymmetric_arch1_q1_res   
Xt_t_student_asymmetric_arch1_q1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_arch1_q1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
# Si ha un p-value di 0.01012 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_arch1_q1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_arch1_q1)
show(Xt_t_student_asymmetric_arch1_q1_w)
# Si ha un p-value di 0.03662 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 1.3117, df = 1, p-value = 0.2521
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 3  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_asymmetric_arch1_q1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_asymmetric_arch1_q1_lb)
# La forma estesa del test di Ljung-Box conferma che c'è presenza di correlazione.

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
# -1.2319788 -1.5860635 -0.9768156 
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_asymmetric_arch1_q1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt      lwr.ci   upr.ci 
# 3.322231 2.173290 4.986746

Xt_t_student_asymmetric_arch1_q1_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_t_student_asymmetric_arch1_q1_cf <- append(Xt_t_student_asymmetric_arch1_q1_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -5.824868   max:  2.88577 
# median:  0.11235 
# mean:  -0.003192019 
# estimated sd:  1.136465 
# estimated skewness:  -1.247804 
# estimated kurtosis:  6.354071 

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_asymmetric_arch1_q1_res
show(c(mean(y),var(y)))
# [1] -2.685178e-17  1.290966e+00
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] -1.888203e-17  1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, graph=TRUE, boot=5000)
Xt_t_student_asymmetric_arch1_q1_cf <- append(Xt_t_student_asymmetric_arch1_q1_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -5.134045   max:  2.556295 
# median:  0.1067869 
# mean:  -1.888203e-17 
# estimated sd:  1 
# estimated skewness:  -1.231979 
# estimated kurtosis:  6.322231

fitdist_test <- list()
# Dato che la Generalized Student distribution ha il parametro location che coincide con la media, possiamo provare 
# a fittare i dati con una generalized Student distribution con zero location. 
# Questo viene fatto cambiando il parametro m nella funzione *fitdistrplus::fitdist*.
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "sstd", start=list(nu=3.2, xi=1.5), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' t_ls ' by maximum likelihood 
# Parameters : 
#     estimate Std. Error
# nu 5.3989484 0.92178503
# xi 0.7007254 0.04435856
# Fixed parameters:
#        value
# mean     0
# sd       1
# Loglikelihood:  -666.4104   AIC:  1336.821   BIC:  1345.25 
# Correlation matrix:
#      nu         xi
# nu  1.0000000 -0.2993225
# xi -0.2993225  1.0000000

# Alla fine, consideriamo il test Goodness of fit.
# The Kolmogorov-Smirnov test in the library *stats*
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="psstd", mean=m, sd=sd, nu=df, xi=xi, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.06536, p-value = 0.02791
# alternative hypothesis: two-sided
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# Cramer-von Mises test of goodness-of-fit
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "psstd", mean=0, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 5, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.73758, p-value = 0.0103
# Ma possiamo rigettare l'ipotesi nulla al 5% di livello di significatività 
# che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.
# Non possiamo rifiutare l'ipotesi nulla al 1% livello di significatività.

# The Anderson-Darling test in the library *goftest*.
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "psstd", mean=m, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 5, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# An = 4.0196, p-value = 0.008537
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata

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
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
# Si ha un p-value di 0.01191 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_arch1_q1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_arch1_q1)
show(Xt_t_student_asymmetric_arch1_q1_w)
# Si ha un p-value di 0.04111 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Test Ljiung-box
y <- Xt_t_student_asymmetric_arch1_q1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 0.52221, df = 1, p-value = 0.4699
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 3  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_asymmetric_arch1_q1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_asymmetric_arch1_q1_lb)
# La forma estesa del test di Ljung-Box conferma che c'è presenza di correlazione.

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
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Line Plot of the model Garch(1,1) with a normal distribution")))
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
skew <- list()
set.seed(123)
s <- DescTools::Skew(Xt_normal_garch2_q1_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew[['0.80']] <- s
print('conf.level=0.80')
show(s)
#     skew       lwr.ci      upr.ci 
# -0.03835342 -0.17413800  0.07881778 
set.seed(123)
s <- DescTools::Skew(Xt_normal_garch2_q1_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
skew[['0.99']] <- s
print('conf.level=0.99')
show(s)
#     skew       lwr.ci      upr.ci 
# -0.03835342 -0.32631653  0.22298066 
kurt <- list()
set.seed(123)
k <- DescTools::Kurt(Xt_normal_garch2_q1_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
kurt[['0.80']] <- k
print('conf.level=0.80')
show(k)
#     kurt      lwr.ci   upr.ci 
# -0.04608956 -0.23081440  0.18900943   
set.seed(123)
k <- DescTools::Kurt(Xt_normal_garch2_q1_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
kurt[['0.99']] <- k
print('conf.level=0.99')
show(k)
#     kurt      lwr.ci   upr.ci 
# -0.04608956 -0.40012112  0.41884544   

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_normal_garch2_q1_p1_cf <- descdist(Xt, discrete=FALSE, boot=5000)
show(Xt_normal_garch2_q1_p1_cf)
# Conferma che la serie è una distribuzione normale.

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_normal_garch2_q1_p1
show(c(mean(y),var(y)))
# [1] -0.008155592  0.499408493
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] 9.205093e-18 1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, graph=TRUE, boot=5000)
Xt_normal_garch2_q1_p1_cf <- append(Xt_normal_garch2_q1_p1_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -3.323782   max:  3.519228 
# median:  0.01740523 
# mean:  -1.407338e-17 
# estimated sd:  1 
# estimated skewness:  -0.01480967 
# estimated kurtosis:  3.793243  
# Da Cullen-Frey, si ha una evidenza di una distribuzione logistica.

# Alla fine, consideriamo il test Goodness of fit.
# The Kolmogorov-Smirnov test in the library *stats*
KS_z_st_t_ls <- stats::ks.test(z_st, y="pnorm", mean=0, sd=1, alternative= "two.sided")
fitdist_test[["normal"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.019065, p-value = 0.9934
# alternative hypothesis: two-sided
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch2_q1_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) of a normal distribution")))
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

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare.
# ADF Test
y <- Xt_normal_garch2_q1_p1_res
num_lags <- 5                   # Setting the lag parameter for the test.
Xt_normal_garch2_q1_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_normal_garch2_q1_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_normal_garch2_q1_p1_res   
Xt_normal_garch2_q1_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_normal_garch2_q1_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
# rispetto al livello di significatività specificato.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch2_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch2_q1_p1 )
show(Xt_normal_garch2_q1_p1_bp)
# Si ha un p-value di 0.7758 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità
#
# Test WHITE sui residui del modello lineare
Xt_normal_garch2_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch2_q1_p1 )
show(Xt_normal_garch2_q1_p1_w)
# Si ha un p-value di 0.9314 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_normal_garch2_q1_p1_lm$residuals
length <- length(y)
maxlag <- ceiling(min(10, T/4))
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

# The partial autocorrelogram of the remainders.
y <-  Xt_normal_garch2_q1_p1_lm$residuals
T <- length(y)
# maxlag <- ceiling(10*log10(T))    # Default
# maxlag <- ceiling(sqrt(n)+45)     # Box-Jenkins
maxlag <- ceiling(min(10, T/4))     # Hyndman (for data without seasonality)
# maxlag <- ceiling(min(2*12, T/5)) # Hyndman (for data with seasonality)
# https://robjhyndman.com/hyndsight/ljung-box-test/
Aut_Fun_y <- pacf(y, lag.max=maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(T)
ci_95 <- qnorm((1+0.95)/2)/sqrt(T)
ci_99 <- qnorm((1+0.99)/2)/sqrt(T)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop("Partial Autocorrelogram of the Residuals in the Linear Model for the model GARCH(1,1)"))
subtitle_content <- bquote(paste("Path length ", .(T), " sample points. Lags ", .(maxlag)))
caption_content <- author_content
x_name <- bquote("lags")
x_breaks_num <- maxlag
x_binwidth <- 1
x_breaks <- Aut_Fun_y$lag
x_labs <- format(x_breaks, scientific=FALSE)
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf))+
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), linewidth=1, col="black") +
  # geom_col(mapping=NULL, data=NULL, position="dodge", width=0.1, col="black", inherit.aes=TRUE)+
  geom_hline(aes(yintercept=-ci_90, color="CI_90"), show.legend=TRUE, lty=3) +
  geom_hline(aes(yintercept=ci_90, color="CI_90"), lty=3) +
  geom_hline(aes(yintercept=ci_95, color="CI_95"), show.legend=TRUE, lty=4)+
  geom_hline(aes(yintercept=-ci_95, color="CI_95"), lty=4) +
  geom_hline(aes(yintercept=-ci_99, color="CI_99"), show.legend=TRUE, lty=4) +
  geom_hline(aes(yintercept=ci_99, color="CI_99"), lty=4) +
  scale_x_continuous(name="lag", breaks=x_breaks, label=x_labs) +
  scale_y_continuous(name="acf value", breaks=waiver(), labels=NULL,
                     sec.axis=sec_axis(~., breaks=waiver(), labels=waiver())) +
  scale_color_manual(name="Conf. Inter.", labels=c("90%","95%","99%"),
                     values=c(CI_90="green", CI_95="blue", CI_99="red")) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  theme(plot.title=element_text(hjust=0.5, size=9), 
        plot.subtitle=element_text(hjust= 0.5, size=8.5),
        plot.caption=element_text(hjust=1.0),
        legend.key.width=unit(0.8,"cm"), legend.position="bottom")

# Test Ljiung-box
y <- Xt_normal_garch2_q1_p1_res
max_lag <- ceiling(min(10, T/4)) 
Box.test(y, lag = max_lag, type = "Ljung-Box", fitdf = 0)
# X-squared = 0.20218, df = 1, p-value = 0.653
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 4 # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
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
skew <- list()
set.seed(123)
s <- DescTools::Skew(Xt_normal_garch2_q1_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew[['0.80']] <- s
print('conf.level=0.80')
show(s)
#     skew       lwr.ci      upr.ci 
# -0.03835342 -0.17413800  0.07881778 
set.seed(123)
s <- DescTools::Skew(Xt_normal_garch2_q1_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
skew[['0.99']] <- s
print('conf.level=0.99')
show(s)
#     skew       lwr.ci      upr.ci 
# 0.1735643 -0.1593904  0.5740895
kurt <- list()
set.seed(123)
k <- DescTools::Kurt(Xt_normal_garch2_q1_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
kurt[['0.80']] <- k
print('conf.level=0.80')
show(k)
#     kurt      lwr.ci   upr.ci 
# 0.1735643069 0.0005266142 0.3567590946   
set.seed(123)
k <- DescTools::Kurt(Xt_normal_garch2_q1_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
kurt[['0.99']] <- k
print('conf.level=0.99')
show(k)
#     kurt      lwr.ci   upr.ci 
# 0.7551241 0.2266635 1.8394832  

Xt_normal_garch2_q1_p1_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_normal_garch2_q1_p1_cf <- descdist(Xt, discrete=FALSE, boot=5000)
show(Xt_normal_garch2_q1_p1_cf)

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_normal_garch2_q1_p1
show(c(mean(y),var(y)))
# [1] -0.008155592  0.499408493
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] 9.205093e-18 1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, graph=TRUE, boot=5000)
Xt_normal_garch2_q1_p1_cf <- append(Xt_normal_garch2_q1_p1_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -3.323782   max:  3.519228 
# median:  0.01740523 
# mean:  -1.407338e-17 
# estimated sd:  1 
# estimated skewness:  -0.01480967 
# estimated kurtosis:  3.793243  
# Da Cullen-Frey, si ha una evidenza di una distribuzione logistica.

# Esploriamo queste possibilità in più dettagli.
z_st_qemp <- EnvStats::qemp(stats::ppoints(z_st), z_st) # Empirical quantiles of the residuals.
z_st_demp <- EnvStats::demp(z_st_qemp, z_st)     # Empirical probability density of the residuals.
z_st_pemp <- EnvStats::pemp(z_st_qemp, z_st)     # Empirical distribution function of the residuals.  
x <- z_st_qemp
y_d <- z_st_demp
y_p <- z_st_pemp
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata con il parametri location=0,
# scale=sqrt(3)/pi e shape=1.
loc <- 0
shp <- 1
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the GARCH(1,1) model with a normal distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", Gen_Log_Dens_Func), 
       col=c("darkblue", "darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
loc <- 0
shp <- 1
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(1,1) model with a normal distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", Gen_Log_Distr_Func), 
       col=c("darkblue","darkgreen","magenta"), 
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
# location -0.1232003  0.1850044
# scale     0.5993065  0.0435735
# shape     1.1466406  0.2255960
# Loglikelihood:  -714.2822   AIC:  1434.564   BIC:  1447.208 
# Correlation matrix:
#            location      scale      shape
# location  1.0000000 -0.8426598 -0.9701573
# scale    -0.8426598  1.0000000  0.8629687
# shape    -0.9701573  0.8629687  1.0000000
#
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_glogis_bd <- fitdistrplus::bootdist(fitdist_glogis, niter=1000)
fitdist_test[["glogis"]][["bootdist"]] <- fitdist_glogis_bd
summary(fitdist_glogis_bd)
# Parametric bootstrap medians and 95% percentile CI 
#            Median       2.5%     97.5%
# location -0.1349432 -0.5115477 0.1627783
# scale     0.5974578  0.5213120 0.6835657
# shape     1.1586496  0.8234587 1.7423547
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
#   -0.1230653  0.5992392  1.1466324  
#
logLik <- -fminunc_result$value # the minimized negative log-likelihood
n <- length(z_st)
k <- length(fminunc_result[["par"]])
AIC <- 2*k-2*logLik
BIC <- k*log(n)-2*logLik
AICc <- AIC + 2*k*((k+1)/(n-k-1))
show(c(logLik, AIC, BIC, AICc))
#    logLik    AIC       BIC      AICc
# -714.2822 1434.5644 1447.2082 1434.6128
#
fitdist_glogis_location <- as.numeric(fitdist_glogis$estimate[1])
fitdist_glogis_scale <- as.numeric(fitdist_glogis$estimate[2])
fitdist_glogis_shape <- as.numeric(fitdist_glogis$estimate[3])
# Notiamo:
round(c(fitdist_glogis_location, fitdist_glogis_scale, fitdist_glogis_shape),4)
# location    scale    shape
#  -0.1232  0.5993  1.1466
round(c(fminunc_result$par[1],fminunc_result$par[2],fminunc_result$par[3]),4)
# location    scale    shape
#  -0.1231   0.5992   1.1466 
#

# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata
loc <- fitdist_glogis_location
shp <- fitdist_glogis_shape
scl <- fitdist_glogis_scale
#
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", .(scl), ", shape = ", .(shp)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the GARCH(1,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dglogis(x, location=loc, scale=scl, shape=shp), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", Gen_Log_Dens_Func), 
       col=c("darkblue", "darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", .(scl), ", shape = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(1,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pstd(x, mean = m, sd = sd, nu = df), lwd=2, col= "red")
lines(x, pglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=5", Gen_Log_Distr_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

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
# D = 0.02777, p-value = 0.8354
# alternative hypothesis: two-sided
# Con un p-value cosi alto, non possiamo rigettare l'ipotesi nulla. Di conseguenza, abbiamo una prova sufficiente
# che la serie è una distribuzione logistica generalizzata.
KS_z_st_t_ls <- stats::ks.test(z_st, y="pnorm", mean=0, sd=1, alternative= "two.sided")
fitdist_test[["normal"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.019065, p-value = 0.9934
# alternative hypothesis: two-sided
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.

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
# with parameters location = location = -0.1232, scale = 0.5993, shape = 1.1466
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.085386, p-value = 0.6615
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "pnorm", mean=0, sd=1, estimated=FALSE)
fitdist_test[["normal"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.02501, p-value = 0.9896
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.
loc <- round(fitdist_glogis[["estimate"]][["location"]],4)
scl <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shp <- round(fitdist_glogis[["estimate"]][["shape"]],4)
AD_z_st_glogis <- goftest::ad.test(z_st, null= "pglogis", location=loc, scale=scl, shape=shp, estimated=FALSE)
fitdist_test[["glogis"]][["Anderson-Darling"]] <- AD_z_st_glogis
show(AD_z_st_glogis)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘pglogis’
# with parameters location = -0.123200335183057, scale = 0.599306483325485, shape = 1.14664062505984
# Parameters assumed to be fixed
# data:  z_st
# An = 0.59424, p-value = 0.6534
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "pnorm", mean=0, sd=1, estimated=FALSE)
fitdist_test[["normal"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 9
# Parameters assumed to be fixed
# data:  z_st
# An = 0.16568, p-value = 0.9971
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata

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

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_normal_garch2_q1_p1_res
num_lags <- 5                   # Setting the lag parameter for the test.
Xt_normal_garch2_q1_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_normal_garch2_q1_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_normal_garch2_q1_p1_res   
Xt_normal_garch2_q1_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_normal_garch2_q1_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
# rispetto al livello di significatività specificato.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch2_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch2_q1_p1 )
show(Xt_normal_garch2_q1_p1_bp)
# Si ha un p-value di 2.015e-12 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità
#
# Test WHITE sui residui del modello lineare
Xt_normal_garch2_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch2_q1_p1 )
show(Xt_normal_garch2_q1_p1_w)
# Si ha un p-value di 1.722e-11 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
max_lag <- ceiling(min(10, T/4))
Box.test(y, lag = max_lag, type = "Ljung-Box", fitdf = 0)
# X-squared = 0.18709, df = 1, p-value = 0.6654
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 5  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_normal_garch2_q1_p1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_normal_garch2_q1_p1_lb)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag.

modello[['stimati']][['garch_q1_p1']] <- append(modello[['stimati']][['garch_q1_p1']], 
                                                list('normale'=list('Xt'=Xt_normal_garch2_q1_p1_new, 'a0'=a0, 'a1'=a1, 'b1'=b1, 'q'=q, 'p'=p,
                                                                   'stazionarietà'=stazionarietà, 'lm'=Xt_normal_garch2_q1_p1_lm, 'skew'=skew, 'kurt'=kurt, 
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
title_content <- bquote(atop("University of Roma \"Tor Vergata\"  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Line plot of the model Garch(1,1) with a symmetric t-student distribution")))
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
skew <- list()
# Calcoliamo la skew e la kurtosi
set.seed(123)
s <- DescTools::Skew(Xt_t_student_symmetric_garch3_q1_p1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew[['0.80']] <-  s
show(s)
#  skew          lwr.ci      upr.ci 
# -0.01480967 -0.22399428  0.19431789
set.seed(123)
s <- DescTools::Skew(Xt_t_student_symmetric_garch3_q1_p1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
skew[['0.99']] <-  s
show(s)
#  skew          lwr.ci      upr.ci 
# -0.01480967 -0.37926462  0.39911609 
kurt <- list()
set.seed(123)
k <- DescTools::Kurt(Xt_t_student_symmetric_garch3_q1_p1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
kurt[['0.80']] <- k
show(k)
#  kurt      lwr.ci      upr.ci 
# 0.7932429 0.4462187 1.2150872 
set.seed(123)
k <- DescTools::Kurt(Xt_t_student_symmetric_garch3_q1_p1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
kurt[['0.99']] <- k
show(k)
#  kurt      lwr.ci      upr.ci 
# 0.7932429 0.1536269 1.8019091 

Xt_t_student_symmetric_garch3_q1_p1_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_t_student_symmetric_garch3_q1_p1_cf <- append(Xt_t_student_symmetric_garch3_q1_p1_cf, cf)
show(cf)
# summary statistics
# ------
# min:  -2.444267   max:  2.490699 
# median:  -0.03470675 
# mean:  -0.04725887 
# estimated sd:  0.721169 
# estimated skewness:  -0.01480967 
# estimated kurtosis:  3.793243   

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_symmetric_garch3_q1_p1
show(c(mean(y),var(y)))
# [1] -0.04725887  0.52008470
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] -1.407338e-17  1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, graph=TRUE, boot=5000)
Xt_t_student_symmetric_garch3_q1_p1_cf <- append(Xt_t_student_symmetric_garch3_q1_p1_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -3.323782   max:  3.519228 
# median:  0.01740523 
# mean:  -1.407338e-17 
# estimated sd:  1 
# estimated skewness:  -0.01480967 
# estimated kurtosis:  3.793243  
# Da Cullen-Frey, si ha una evidenza di una distribuzione logistica.
  
# Esploriamo queste possibilità in più dettagli.
z_st_qemp <- EnvStats::qemp(stats::ppoints(z_st), z_st) # Empirical quantiles of the residuals.
z_st_demp <- EnvStats::demp(z_st_qemp, z_st)     # Empirical probability density of the residuals.
z_st_pemp <- EnvStats::pemp(z_st_qemp, z_st)     # Empirical distribution function of the residuals.  
x <- z_st_qemp
y_d <- z_st_demp
y_p <- z_st_pemp
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata con il parametri location=0,
# scale=sqrt(3)/pi e shape=1.
loc <- 0
shp <- 1
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the GARCH(1,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dstd(x, mean=0, sd=1, nu=5), lwd=2, col= "red")
lines(x, dglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", "Student Density Function, df=5", Gen_Log_Dens_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
loc <- 0
shp <- 1
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(1,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pstd(x, mean = 0, sd = 1, nu = 5), lwd=2, col= "red")
lines(x, pglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=5", Gen_Log_Distr_Func), 
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
# location 0.06359204  0.1394851
# scale    0.54003749  0.0381732
# shape    0.93271464  0.1522018
# Loglikelihood:  -703.9183   AIC:  1413.837   BIC:  1426.481 
# Correlation matrix:
#   location      scale      shape
# location  1.0000000 -0.8035767 -0.9516047
# scale    -0.8035767  1.0000000  0.8474619
# shape    -0.9516047  0.8474619  1.0000000
#
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_glogis_bd <- fitdistrplus::bootdist(fitdist_glogis, niter=1000)
fitdist_test[["glogis"]][["bootdist"]] <- fitdist_glogis_bd
summary(fitdist_glogis_bd)
# Parametric bootstrap medians and 95% percentile CI 
#            Median       2.5%     97.5%
# location 0.05610663 -0.2542059 0.3091325
# scale    0.53871967  0.4680651 0.6196193
# shape    0.93888338  0.6868144 1.3611969
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
#   0.06362149 0.54001383 0.93270721  
#
logLik <- -fminunc_result$value # the minimized negative log-likelihood
n <- length(z_st)
k <- length(fminunc_result[["par"]])
AIC <- 2*k-2*logLik
BIC <- k*log(n)-2*logLik
AICc <- AIC + 2*k*((k+1)/(n-k-1))
show(c(logLik, AIC, BIC, AICc))
#    logLik    AIC       BIC      AICc
# -703.9183 1413.8367 1426.4805 1413.8851
#
fitdist_glogis_location <- as.numeric(fitdist_glogis$estimate[1])
fitdist_glogis_scale <- as.numeric(fitdist_glogis$estimate[2])
fitdist_glogis_shape <- as.numeric(fitdist_glogis$estimate[3])
# Notiamo:
round(c(fitdist_glogis_location, fitdist_glogis_scale, fitdist_glogis_shape),4)
# location    scale    shape
#  0.0636 0.5400 0.9327
round(c(fminunc_result$par[1],fminunc_result$par[2],fminunc_result$par[3]),4)
# location    scale    shape
#  0.0636   0.5400   0.9327 
#

# Dato che la Generalized Student distribution ha il parametro location che coincide con la media, possiamo provare 
# a fittare i dati con una generalized Student distribution con zero location. 
# Questo viene fatto cambiando il parametro m nella funzione *fitdistrplus::fitdist*.
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "std", start=list(nu=5), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' t_ls ' by maximum likelihood 
# Parameters : 
#   estimate Std. Error
# nu  8.77186   3.032077
# Fixed parameters:
#      value
# mean     0
# sd       1
# Loglikelihood:  -704.2682   AIC:  1410.536   BIC:  1414.751 

# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata con il parametri location=0,
# scale=sqrt(3)/pi e shape=1.
loc <- fitdist_glogis_location
shp <- fitdist_glogis_shape
scl <- fitdist_glogis_scale
#
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
#
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", .(scl), ", shape = ", .(shp)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the GARCH(1,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dstd(x, mean=m, sd=sd, nu=df), lwd=2, col= "red")
lines(x, dglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", "Student Density Function, df=5", Gen_Log_Dens_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", .(scl), ", shape = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(1,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pstd(x, mean = m, sd = sd, nu = df), lwd=2, col= "red")
lines(x, pglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=5", Gen_Log_Distr_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

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
# D = 0.014717, p-value = 0.9999
# alternative hypothesis: two-sided
# Con un p-value cosi alto, non possiamo rigettare l'ipotesi nulla. Di conseguenza, abbiamo una prova sufficiente
# che la serie è una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="pstd", mean=m, sd=sd, nu=df, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.01966, p-value = 0.9904
# alternative hypothesis: two-sided
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.

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
# with parameters location = 0.0636, scale = 0.54, shape = 0.9327
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.015323, p-value = 0.9996
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "pstd", mean=m, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 9
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.025218, p-value = 0.9892
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.
loc <- round(fitdist_glogis[["estimate"]][["location"]],4)
scl <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shp <- round(fitdist_glogis[["estimate"]][["shape"]],4)
AD_z_st_glogis <- goftest::ad.test(z_st, null= "pglogis", location=loc, scale=scl, shape=shp, estimated=FALSE)
fitdist_test[["glogis"]][["Anderson-Darling"]] <- AD_z_st_glogis
show(AD_z_st_glogis)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘pglogis’
# with parameters location = 0.0635920364688568, scale = 0.54003748800123, shape = 0.932714643747957
# Parameters assumed to be fixed
# data:  z_st
# An = 0.11188, p-value = 0.9999
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "pstd", mean=m, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 9
# Parameters assumed to be fixed
# data:  z_st
# An = 0.16568, p-value = 0.9971
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch3_q1_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) with a symmetric t-student distribution")))
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
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(1,1) with a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Square root of absolute residuals")
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

plot(Xt_t_student_symmetric_garch3_q1_p1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_garch3_q1_p1_lm,3) # Scale-location

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_symmetric_garch3_q1_p1_res
num_lags <- 5                   # Setting the lag parameter for the test.
Xt_t_student_symmetric_garch3_q1_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_symmetric_garch3_q1_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_t_student_symmetric_garch3_q1_p1_res   
Xt_t_student_symmetric_garch3_q1_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_symmetric_garch3_q1_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
# rispetto al livello di significatività specificato.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch3_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch3_q1_p1)
show(Xt_t_student_symmetric_garch3_q1_p1_bp)
# Si ha un p-value di 0.157 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch3_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch3_q1_p1)
show(Xt_t_student_symmetric_garch3_q1_p1_w)
# Si ha un p-value di 0.2072 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
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
max_lag <- ceiling(min(10, T/4))
Box.test(y, lag = max_lag, type = "Ljung-Box", fitdf = 0)
# X-squared = 2.2579, df = 1, p-value = 0.1329
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 5  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_symmetric_garch3_q1_p1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_symmetric_garch3_q1_p1_lb)
# La forma estesa del test di Ljung-Box conferma che c'è presenza di correlazione.
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

modello[['simulazione']][['garch_q1_p1']][['simmetrico']][['3']] <- append(modello[['simulazione']][['garch_q1_p1']][['simmetrico']][['3']], 
                                                                           list('lm'=Xt_t_student_symmetric_garch3_q1_p1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                                'Cullen-Frey'=Xt_t_student_symmetric_garch3_q1_p1_cf, 
                                                                                'Breusch-Pagan'=Xt_t_student_symmetric_garch3_q1_p1_bp, 'White'=Xt_t_student_symmetric_garch3_q1_p1_w, 
                                                                                'Ljiung-Box'=Xt_t_student_symmetric_garch3_q1_p1_lb, 'Dickey-Fuller'=Xt_t_student_symmetric_garch3_q1_p1_adf, 
                                                                                'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_garch3_q1_p1_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_garch3_q1_p1_sw,
                                                                                'Generalized Distribution'=fitdist_test))

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
skew <- list()
set.seed(123)
s <- DescTools::Skew(Xt_t_student_symmetric_garch3_q1_p1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew[['0.80']] <- s
show(s)
#     skew       lwr.ci      upr.ci 
# -0.01480967 -0.22399428  0.19431789  
set.seed(123)
s <- DescTools::Skew(Xt_t_student_symmetric_garch3_q1_p1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
skew[['0.99']] <- s
show(s)
#     skew       lwr.ci      upr.ci 
# -0.01480967 -0.37926462  0.39911609 
kurt <- list()
set.seed(123)
k <- DescTools::Kurt(Xt_t_student_symmetric_garch3_q1_p1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
kurt[['0.80']] <- k
show(k)
#    kurt      lwr.ci   upr.ci 
# 0.7932429 0.4462187 1.2150872 
set.seed(123)
k <- DescTools::Kurt(Xt_t_student_symmetric_garch3_q1_p1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
kurt[['0.99']] <- k
show(k)
#    kurt      lwr.ci   upr.ci 
# 0.7932429 0.1536269 1.8019091 

Xt_t_student_symmetric_garch3_q1_p1_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_t_student_symmetric_garch3_q1_p1_cf <- append(Xt_t_student_symmetric_garch3_q1_p1_cf, cf)
show(cf)
# summary statistics
# ------
# min:  -2.374945   max:  2.012716 
# median:  -0.02175566 
# mean:  -0.03846084 
# estimated sd:  0.6626274 
# estimated skewness:  -0.1212236 
# estimated kurtosis:  3.950504 

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_symmetric_garch3_q1_p1_res
show(c(mean(y),var(y)))
# [1] -6.989201e-18  4.376492e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] -1.005641e-17  1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, graph=TRUE, boot=5000)
Xt_t_student_symmetric_garch3_q1_p1_cf <- append(Xt_t_student_symmetric_garch3_q1_p1_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -3.446296   max:  3.082768 
# median:  0.01171033 
# mean:  -1.005641e-17 
# estimated sd:  1 
# estimated skewness:  -0.05688258 
# estimated kurtosis:  3.913263 
# Da Cullen-Frey, potremmo avere una distribuzione logistica o t-student.

# Esploriamo queste possibilità in più dettagli.
z_st_qemp <- EnvStats::qemp(stats::ppoints(z_st), z_st) # Empirical quantiles of the residuals.
z_st_demp <- EnvStats::demp(z_st_qemp, z_st)     # Empirical probability density of the residuals.
z_st_pemp <- EnvStats::pemp(z_st_qemp, z_st)     # Empirical distribution function of the residuals.  
x <- z_st_qemp
y_d <- z_st_demp
y_p <- z_st_pemp
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata con il parametri location=0,
# scale=sqrt(3)/pi e shape=1.
loc <- 0
shp <- 1
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the GARCH(1,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dstd(x, mean=0, sd=1, nu=5), lwd=2, col= "red")
lines(x, dglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", "Student Density Function, df=5", Gen_Log_Dens_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
loc <- 0
shp <- 1
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(1,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pstd(x, mean = 0, sd = 1, nu = 5), lwd=2, col= "red")
lines(x, pglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=5", Gen_Log_Distr_Func), 
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
# location 0.07370581 0.12608847
# scale    0.52941558 0.03573147
# shape    0.91762303 0.13870842
# Loglikelihood:  -700.3245   AIC:  1406.649   BIC:  1419.293 
# Correlation matrix:
#           location      scale      shape
# location  1.0000000 -0.7757777 -0.9428012
# scale    -0.7757777  1.0000000  0.8283347
# shape    -0.9428012  0.8283347  1.0000000
#
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_glogis_bd <- fitdistrplus::bootdist(fitdist_glogis, niter=1000)
fitdist_test[["glogis"]][["bootdist"]] <- fitdist_glogis_bd
summary(fitdist_glogis_bd)
# Parametric bootstrap medians and 95% percentile CI 
#            Median       2.5%     97.5%
# location 0.06616558 -0.2342071 0.3154116
# scale    0.52831741  0.4590044 0.6073470
# shape    0.92327848  0.6780818 1.3374339
# We fix the initial points of the constrained maximization procedure
location <- fitdist_glogis_bd[["fitpart"]][["estimate"]][1]
scale <- fitdist_glogis_bd[["fitpart"]][["estimate"]][2]
shape <- fitdist_glogis_bd[["fitpart"]][["estimate"]][3]
minus_logLik <- function(x) -sum(log(dglogis(z_st, location=x[1], scale=x[2], shape=x[3]))) # the log-likelihood of the generalized logistic
# distribution.
fminunc_result <- fminunc(x0=c(location, scale, shape), fn=minus_logLik)   # the minimization procedure where (location,scale,shape) is the 
# starting point.
show(c(fminunc_result$par[1],fminunc_result$par[2],fminunc_result$par[3])) # the estimated parameters.
#   location   scale     shape 
# 0.0736076 0.5295083 0.9177434  
#
logLik <- -fminunc_result$value # the minimized negative log-likelihood
n <- length(z_st)
k <- length(fminunc_result[["par"]])
AIC <- 2*k-2*logLik
BIC <- k*log(n)-2*logLik
AICc <- AIC + 2*k*((k+1)/(n-k-1))
show(c(logLik, AIC, BIC, AICc))
#    logLik    AIC       BIC      AICc
# -700.3245 1406.6490 1419.2928 1406.6974
#
fitdist_glogis_location <- as.numeric(fitdist_glogis$estimate[1])
fitdist_glogis_scale <- as.numeric(fitdist_glogis$estimate[2])
fitdist_glogis_shape <- as.numeric(fitdist_glogis$estimate[3])
# Notiamo:
round(c(fitdist_glogis_location, fitdist_glogis_scale, fitdist_glogis_shape),4)
# location    scale    shape
#   0.0737 0.5294 0.9176
round(c(fminunc_result$par[1],fminunc_result$par[2],fminunc_result$par[3]),4)
# location    scale    shape
#  0.0736   0.5295   0.9177 

# Dato che la Generalized Student distribution ha il parametro location che coincide con la media, possiamo provare 
# a fittare i dati con una generalized Student distribution con zero location. 
# Questo viene fatto cambiando il parametro m nella funzione *fitdistrplus::fitdist*.
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "std", start=list(nu=3.2), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' t_ls ' by maximum likelihood 
# Parameters : 
#   estimate Std. Error
# nu 6.291429   1.553826
# Fixed parameters:
#       value
# mean     0
# sd       1
# Loglikelihood:  -701.2011   AIC:  1404.402   BIC:  1408.617 

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
# 0.0737 0.5294 0.9176
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
# 
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the GARCH(1,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dstd(x, mean=m, sd=sd, nu=df), lwd=2, col= "red")
lines(x, dglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", "Student Density Function, df=5", Gen_Log_Dens_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(1,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pstd(x, mean = m, sd = sd, nu = df), lwd=2, col= "red")
lines(x, pglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=5", Gen_Log_Distr_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

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
# D = 0.027988, p-value = 0.8284
# alternative hypothesis: two-sided
# Con un p-value cosi alto, non possiamo rigettare l'ipotesi nulla. Di conseguenza, abbiamo una prova sufficiente
# che la serie è una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="pstd", mean=m, sd=sd, nu=df, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.026937, p-value = 0.8612
# alternative hypothesis: two-sided
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.

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
# with parameters location = 0.0737, scale = 0.5294, shape = 0.9176
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.055914, p-value = 0.8399
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "pstd", mean=0, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 6
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.047865, p-value = 0.8893
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
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
# with parameters location = 0.0737058145351111, scale = 0.529415583364253, shape = 0.91762303404264
# Parameters assumed to be fixed
# data:  z_st
# An = 0.36962, p-value = 0.8782
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "pstd", mean=m, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 6
# Parameters assumed to be fixed
# data:  z_st
# An = 0.35197, p-value = 0.8946
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch3_q1_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) with a t-student symmetric distribution")))
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
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(1,1) with a symmetric t-student distribution")))
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

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_symmetric_garch3_q1_p1_res
num_lags <- 5                   # Setting the lag parameter for the test.
Xt_t_student_symmetric_garch3_q1_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_symmetric_garch3_q1_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_t_student_symmetric_garch3_q1_p1_res   
Xt_t_student_symmetric_garch3_q1_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_symmetric_garch3_q1_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
# rispetto al livello di significatività specificato.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch3_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch3_q1_p1)
show(Xt_t_student_symmetric_garch3_q1_p1_bp)
# Si ha un p-value di 7.749e-07 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità
#
# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch3_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch3_q1_p1)
show(Xt_t_student_symmetric_garch3_q1_p1_w)
# Si ha un p-value di 1.171e-06 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
max_lag <- ceiling(min(10, T/4)) 
Box.test(y, lag = max_lag, type = "Ljung-Box", fitdf = 0)
# X-squared = 0.34583, df = 1, p-value = 0.5565
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 5  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_symmetric_garch3_q1_p1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_symmetric_garch3_q1_p1_lb)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag.

modello[['stimati']][['garch_q1_p1']] <- append(modello[['stimati']][['garch_q1_p1']], 
                                                list('simmetrico'=list('Xt'=Xt_t_student_symmetric_garch3_q1_p1_new, 'a0'=a0, 'a1'=a1, 'b1'=b1, 'q'=q, 'p'=p, 
                                                                       'stazionarietà'=stazionarietà, 'lm'=Xt_t_student_symmetric_garch3_q1_p1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                       'Cullen-Frey'=Xt_t_student_symmetric_garch3_q1_p1_cf,
                                                                       'Breusch-Pagan'=Xt_t_student_symmetric_garch3_q1_p1_bp, 'White'=Xt_t_student_symmetric_garch3_q1_p1_w, 
                                                                       'Ljiung-Box'=Xt_t_student_symmetric_garch3_q1_p1_lb, 'Dickey-Fuller'=Xt_t_student_symmetric_garch3_q1_p1_adf, 
                                                                       'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_garch3_q1_p1_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_garch3_q1_p1_sw,                                                                             
                                                                       'Generalized Distribution'=fitdist_test)))

# In questo modello Garch(1,1) con una distribuzione t-student simmetrica, con i nuovi parametri stimati,
# si ha evidenza di eteroschedasticità e assenza di autocorrelazione.

##########################################

#### DISTRIBUZIONE ASIMMETRICA
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
skew <- list()
set.seed(123)
s <- DescTools::Skew(Xt_t_student_asymmetric_garch2_q1_p1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew[['0.80']] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -1.491705 -1.880368 -1.191514
set.seed(123)
s <- DescTools::Skew(Xt_t_student_asymmetric_garch2_q1_p1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
skew[['0.99']] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -1.4917048 -2.3299279 -0.8590101 
kurt <- list()
set.seed(123)
k <- DescTools::Kurt(Xt_t_student_asymmetric_garch2_q1_p1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
kurt[['0.80']] <- k
show(k)
#  kurt   lwr.ci   upr.ci 
# 4.532716 2.907448 6.749892   
set.seed(123)
k <- DescTools::Kurt(Xt_t_student_asymmetric_garch2_q1_p1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
kurt[['0.99']] <- k
show(k)
#  kurt   lwr.ci   upr.ci 
# 4.532716 1.153100 9.407195   

Xt_t_student_asymmetric_garch2_q1_p1_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, method= "sample", boot=2000)
Xt_t_student_asymmetric_garch2_q1_p1_cf <- append(Xt_t_student_asymmetric_garch2_q1_p1_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -3.851051   max:  1.446431 
# median:  0.1617075 
# mean:  0.03789853 
# sample sd:  0.6783321 
# sample skewness:  -1.521593 
# sample kurtosis:  7.678408   
# Dal grafico di Cullen-Frey, la serie potrebbe seguire una distribuzione t-student.

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_asymmetric_garch2_q1_p1_res
show(c(mean(y),var(y)))
# [1] -7.086996e-18  4.599380e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] -7.489994e-18  1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, method= "sample", graph=TRUE, boot=2000)
Xt_t_student_asymmetric_garch2_q1_p1_cf <- append(Xt_t_student_asymmetric_garch2_q1_p1_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -5.65463   max:  2.087659 
# median:  0.1820182 
# mean:  -7.489994e-18 
# sample sd:  0.9989995 
# sample skewness:  -1.487226 
# sample kurtosis:  7.47554 

fitdist_test <- list()
#
location <- 0
scale <- sqrt(3)/pi
shape <- 1
mean <- location+(digamma(shape)-digamma(1))*scale
show(mean)
# mean 0
sd <- sqrt((psigamma(shape, deriv=1)+psigamma(1, deriv=1))*scale^2)
show(sd)
# sd 1
skewness <- (psigamma(shape, deriv=2)-psigamma(1, deriv=2))/((psigamma(shape, deriv=1)+psigamma(1, deriv=1))^(3/2))
show(skewness)
# skew 0
# Dato che la Generalized Student distribution ha il parametro location che coincide con la media, possiamo provare 
# a fittare i dati con una generalized Student distribution con zero location. 
# Questo viene fatto cambiando il parametro m nella funzione *fitdistrplus::fitdist*.
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "sstd", start=list(nu=3.2, xi=1.5), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' std  ' by maximum likelihood 
# Parameters : 
#     estimate Std. Error
# nu 4.7064719  0.6255521
# xi 0.6536622  0.0411537
# Fixed parameters:
#      value
# mean     0
# sd       1
# Loglikelihood:  -650.2719   AIC:  1304.544   BIC:  1312.973 
# Correlation matrix:
#       nu         xi
# nu  1.0000000 -0.2984671
# xi -0.2984671  1.0000000

# Alla fine, consideriamo il test Goodness of fit.
# The Kolmogorov-Smirnov test in the library *stats*
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="psstd", mean=m, sd=sd, nu=df, xi=xi, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.10085, p-value = 7.658e-05
# alternative hypothesis: two-sided
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# Cramer-von Mises test of goodness-of-fit
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "psstd", mean=0, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 5, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 1.4069, p-value = 0.0002779
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "psstd", mean=m, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 5, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# An = 7.1406, p-value = 0.0002863
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata

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

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_garch2_q1_p1_res
num_lags <- 5                   # Setting the lag parameter for the test.
Xt_t_student_asymmetric_garch2_q1_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_garch2_q1_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_t_student_asymmetric_garch2_q1_p1_res   
Xt_t_student_asymmetric_garch2_q1_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_garch2_q1_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
# rispetto al livello di significatività specificato.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch2_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch2_q1_p1)
show(Xt_t_student_asymmetric_garch2_q1_p1_bp)
# Si ha un p-value di 0.01823 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità
#
# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch2_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch2_q1_p1)
show(Xt_t_student_asymmetric_garch2_q1_p1_w)
# Si ha un p-value di 0.04229 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
max_lag <- ceiling(min(10, T/4)) 
Box.test(y, lag = max_lag, type = "Ljung-Box", fitdf = 0)
# X-squared = 7.1729, df = 1, p-value = 0.007401
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 5  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_asymmetric_garch2_q1_p1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_asymmetric_garch2_q1_p1_lb)
# I risultati hanno un p-value < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

modello[['simulazione']][['garch_q1_p1']][['asimmetrico']][['2']] <- append(modello[['simulazione']][['garch_q1_p1']][['asimmetrico']][['2']], 
                                                                            list('lm'=Xt_t_student_asymmetric_garch2_q1_p1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                                 'Cullen-Frey'=Xt_t_student_asymmetric_garch2_q1_p1_cf,
                                                                                 'Breusch-Pagan'=Xt_t_student_asymmetric_garch2_q1_p1_bp, 'White'=Xt_t_student_asymmetric_garch2_q1_p1_w, 
                                                                                 'Ljiung-Box'=Xt_t_student_asymmetric_garch2_q1_p1_lb, 'Dickey-Fuller'=Xt_t_student_asymmetric_garch2_q1_p1_adf, 
                                                                                 'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_garch2_q1_p1_kpss, 'Shapiro-Wilk'=Xt_t_student_asymmetric_garch2_q1_p1_sw,         
                                                                                 'Generalized Distribution'=fitdist_test))

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
skew <- list()
set.seed(123)
s <- DescTools::Skew(Xt_t_student_asymmetric_garch2_q1_p1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew[['0.80']] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -1.526175 -1.919925 -1.213420  
set.seed(123)
s <- DescTools::Skew(Xt_t_student_asymmetric_garch2_q1_p1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
skew[['0.99']] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -1.5261751 -2.3890455 -0.8644226 
kurt <- list()
set.seed(123)
k <- DescTools::Kurt(Xt_t_student_asymmetric_garch2_q1_p1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
kurt[["0.80"]] <- k
show(k)
#  kurt      lwr.ci   upr.ci 
# 4.737628 3.032273 6.980299 
set.seed(123)
k <- DescTools::Kurt(Xt_t_student_asymmetric_garch2_q1_p1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
kurt[["0.99"]] <- k
show(k)
#  kurt      lwr.ci   upr.ci 
# 4.737628 1.192398 9.830379

Xt_t_student_asymmetric_arch1_q1_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, method= "sample", boot=2000)
Xt_t_student_asymmetric_arch1_q1_cf <- append(Xt_t_student_asymmetric_arch1_q1_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -3.190775   max:  1.802073 
# median:  0.1162308 
# mean:  0.0524619 
# sample sd:  0.6614715 
# sample skewness:  -1.071379 
# sample kurtosis:  5.714497 
# Dal grafico di Cullen-Frey, la serie potrebbe seguire una distribuzione t-student.

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_asymmetric_garch2_q1_p1_res
show(c(mean(y),var(y)))
# [1] 3.440824e-18 4.374096e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] 4.532832e-18 1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, method= "sample", graph=TRUE, boot=2000)
Xt_t_student_asymmetric_arch1_q1_cf <- append(Xt_t_student_asymmetric_arch1_q1_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -4.887009   max:  2.566044 
# median:  0.1293638 
# mean:  4.532832e-18 
# sample sd:  0.9989995 
# sample skewness:  -1.134164 
# sample kurtosis:  5.837144

fitdist_test <- list()
#
location <- 0
scale <- sqrt(3)/pi
shape <- 1
mean <- location+(digamma(shape)-digamma(1))*scale
show(mean)
# mean 0
sd <- sqrt((psigamma(shape, deriv=1)+psigamma(1, deriv=1))*scale^2)
show(sd)
# sd 1
skewness <- (psigamma(shape, deriv=2)-psigamma(1, deriv=2))/((psigamma(shape, deriv=1)+psigamma(1, deriv=1))^(3/2))
show(skewness)
# skew 0
# Dato che la Generalized Student distribution ha il parametro location che coincide con la media, possiamo provare 
# a fittare i dati con una generalized Student distribution con zero location. 
# Questo viene fatto cambiando il parametro m nella funzione *fitdistrplus::fitdist*.
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "sstd", start=list(nu=3.2, xi=1.5), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' std  ' by maximum likelihood 
# Parameters : 
#     estimate Std. Error
# nu 4.8994564 0.75789977
# xi 0.7269478 0.04240755
# Fixed parameters:
#        value
# mean     0
# sd       1
# Loglikelihood:  -668.2579   AIC:  1340.516   BIC:  1348.945 
# Correlation matrix:
#      nu         xi
# nu  1.0000000 -0.2706387
# xi -0.2706387  1.0000000

# Alla fine, consideriamo il test Goodness of fit.
# The Kolmogorov-Smirnov test in the library *stats*
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="psstd", mean=m, sd=sd, nu=df, xi=xi, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.094481, p-value = 0.0002656
# alternative hypothesis: two-sided
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# Cramer-von Mises test of goodness-of-fit
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "psstd", mean=m, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 5, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.95588, p-value = 0.003106
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "psstd", mean=m, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters  mean = 0, sd = 1, nu = 5, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# An = 4.8908, p-value = 0.003237
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata

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


# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_garch2_q1_p1_res
num_lags <- 5                   # Setting the lag parameter for the test.
Xt_t_student_asymmetric_garch2_q1_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_garch2_q1_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_t_student_asymmetric_garch2_q1_p1_res   
Xt_t_student_asymmetric_garch2_q1_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_garch2_q1_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
# rispetto al livello di significatività specificato.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch2_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch2_q1_p1)
show(Xt_t_student_asymmetric_garch2_q1_p1_bp)
# Si ha un p-value di 3.15e-05 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità
#
# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch2_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch2_q1_p1)
show(Xt_t_student_asymmetric_garch2_q1_p1_w)
# Si ha un p-value di 6.905e-05 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
max_lag <- ceiling(min(10, T/4))
Box.test(y, lag = max_lag, type = "Ljung-Box", fitdf = 0)
# X-squared = 2.9465, df = 1, p-value = 0.08607
# I risultati mostrano un p-value > 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 5  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_asymmetric_garch2_q1_p1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_asymmetric_garch2_q1_p1_lb)
# La forma estesa del test di Ljung-Box conferma che c'è presenza di correlazione.

modello[['stimati']][['garch_q1_p1']] <- append(modello[['stimati']][['garch_q1_p1']], 
                                                list('asimmetrico'=list('Xt'=Xt_t_student_asymmetric_garch2_q1_p1_new, 'a0'=a0, 'a1'=a1, 'b1'=b1, 'q'=q, 'p'=p,
                                                                        'stazionarietà'=stazionarietà, 'lm'=Xt_t_student_asymmetric_garch2_q1_p1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                        'Cullen-Frey'=Xt_t_student_asymmetric_garch2_q1_p1_cf,
                                                                        'Breusch-Pagan'=Xt_t_student_asymmetric_garch2_q1_p1_bp, 'White'=Xt_t_student_asymmetric_garch2_q1_p1_w, 
                                                                        'Ljiung-Box'=Xt_t_student_asymmetric_garch2_q1_p1_lb, 'Dickey-Fuller'=Xt_t_student_asymmetric_garch2_q1_p1_adf, 
                                                                        'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_garch2_q1_p1_kpss, 'Shapiro-Wilk'=Xt_t_student_asymmetric_garch2_q1_p1_sw,  
                                                                        'Generalized Distribution'=fitdist_test)))

# In questo modello Garch(1,1) con una distribuzione t-student asimmetrica, con i
# i miglior parametri stimati la serie ha presenza di eteroschedasticità e
# assenza di autocorrelazione.

##########################################
##########################################

#### DISTRIBUZIONE NORMALE
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
# Call:
# lm(formula = Xt ~ t, data = df_Xt_normal_garch1_q1_p2)
# Residuals:
#  Min       1Q   Median       3Q      Max 
# -2.53594 -0.49959 -0.01361  0.54157  2.01056 
# Coefficients:
#              Estimate   Std. Error  t value Pr(>|t|)
# (Intercept) -0.0801921  0.0646316  -1.241    0.215
# t            0.0002850  0.0002236   1.275    0.203
# Residual standard error: 0.7215 on 498 degrees of freedom
# Multiple R-squared:  0.003252,	Adjusted R-squared:  0.00125 
# F-statistic: 1.625 on 1 and 498 DF,  p-value: 0.203
summary(Xt_normal_garch1_q1_p2_lm$fitted.values)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -0.079907 -0.044359 -0.008812 -0.008812  0.026736  0.062284 

Xt_normal_garch1_q1_p2_res <- Xt_normal_garch1_q1_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
skew <- list()
set.seed(123)
s <- DescTools::Skew(Xt_normal_garch1_q1_p2_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew[['0.80']] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -0.08117254 -0.22670022  0.05093809 
set.seed(123)
s <- DescTools::Skew(Xt_normal_garch1_q1_p2_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
skew[['0.99']] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -0.08117254 -0.36068527  0.15186825 
kurt <- list()
set.seed(123)
k <- DescTools::Kurt(Xt_normal_garch1_q1_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
kurt[['0.80']] <- k
show(k)
#  kurt      lwr.ci   upr.ci 
# -0.1573010 -0.3865318  0.1979878  
set.seed(123)
k <- DescTools::Kurt(Xt_normal_garch1_q1_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
kurt[['0.99']] <- k
show(k)
#  kurt      lwr.ci   upr.ci 
# -0.1573010 -0.5581969  0.7102910  

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_normal_garch1_q1_p2_cf <- descdist(Xt, discrete=FALSE, method= "sample", boot=2000)
show(Xt_normal_garch1_q1_p2_cf)
# summary statistics
# 
# min:  -2.497308   max:  2.021551 
# median:  -0.01276216 
# mean:  -0.00881181 
# sample sd:  0.721249 
# sample skewness:  -0.05190056 
# sample kurtosis:  2.819545 
# Da Cullen-Frey, possiam notare che la serie segue una distribuzione normale.

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_normal_garch1_q1_p2_res
num_lags <- 6                   # Setting the lag parameter for the test.
Xt_normal_garch1_q1_p2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_normal_garch1_q1_p2_adf) 
# ...
# Value of test-statistic is: -7.9723 
# Critical values for test statistics: 
#       1pct  5pct 10pct
# tau1 -2.58 -1.95 -1.62
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.

# KPSS Test
y <- Xt_normal_garch1_q1_p2_res   
Xt_normal_garch1_q1_p2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_normal_garch1_q1_p2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_normal_garch1_q1_p2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_normal_garch1_q1_p2_lb)
# La forma estesa del test di Ljung-Box conferma che c'è presenza di correlazione in tutti i lag.

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
#              2.5 %     97.5 %
# mu     -0.10833541 0.08436193
# ar1    -0.98431625 1.02746154
# ma1    -0.93838286 1.06533506
# omega  -0.01146436 0.01522447
# alpha1 -0.01227059 0.02515966
# beta1          NaN        NaN
# beta2   0.98986127 0.99524965

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
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -0.015548 -0.004108  0.007333  0.007333  0.018773  0.030213 

Xt_normal_garch1_q1_p2_res <- Xt_normal_garch1_q1_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
skew <- list()
set.seed(123)
s <- DescTools::Skew(Xt , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew[["0.80"]] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# 0.006359738 -0.177782338  0.228236993 
set.seed(123)
s <- DescTools::Skew(Xt , weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
skew[["0.99"]] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# 0.006359738 -0.391642551  0.370653586 
kurt <- list()
set.seed(123)
k <- DescTools::Kurt(Xt , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
kurt[["0.80"]] <- k
show(k)
#  kurt      lwr.ci   upr.ci 
# 0.9657684 0.5952081 1.4529301  
set.seed(123)
k <- DescTools::Kurt(Xt , weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
kurt[["0.99"]] <- k
show(k)
#  kurt      lwr.ci   upr.ci 
# 0.9657684 0.3157029 2.0276446 

# Cullen-Frey
Xt_normal_garch1_q1_p2_cf <- list()
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, method= "sample", boot=2000)
Xt_normal_garch1_q1_p2_cf <- append(Xt_normal_garch1_q1_p2_cf, list(cf))
show(cf)
# summary statistics
# 
# min:  -1.842703   max:  1.746654 
# median:  -0.006976736 
# mean:  0.007332511 
# sample sd:  0.492373 
# sample skewness:  0.006340643 
# sample kurtosis:  3.944162
# Da Cullen-Frey, notiamo che la serie non segue una distribuzione normale. Proviamo ad eseguire dei test
# per verifica se sia una distribuzione normale o logistica.

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_normal_garch1_q1_p2_res
show(c(mean(y),var(y)))
# [1] -8.818684e-18  2.427415e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] -1.247852e-17  1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, method= "sample", boot=2000)
Xt_normal_garch1_q1_p2_cf <- append(Xt_normal_garch1_q1_p2_cf, list(cf))
show(cf)
# summary statistics
# ------
# min:  -1.842703   max:  1.746654 
# median:  -0.006976736 
# mean:  0.007332511 
# sample sd:  0.492373 
# sample skewness:  0.006340643 
# sample kurtosis:  3.944162 
# Da Cullen-Frey, abbiamo una possibile distribuzione logistica o di student per i residui.
  
# Esploriamo queste possibilità in più dettagli.
z_st_qemp <- EnvStats::qemp(stats::ppoints(z_st), z_st) # Empirical quantiles of the residuals.
z_st_demp <- EnvStats::demp(z_st_qemp, z_st)     # Empirical probability density of the residuals.
z_st_pemp <- EnvStats::pemp(z_st_qemp, z_st)     # Empirical distribution function of the residuals.  
x <- z_st_qemp
y_d <- z_st_demp
y_p <- z_st_pemp
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata con il parametri location=0,
# scale=sqrt(3)/pi e shape=1.
loc <- 0
shp <- 1
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the GARCH(1,1) model with a normal distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dstd(x, mean=0, sd=1, nu=5), lwd=2, col= "red")
lines(x, dglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", "Student Density Function, df=5", Gen_Log_Dens_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
loc <- 0
shp <- 1
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(1,1) model with a normal distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pstd(x, mean = 0, sd = 1, nu = 5), lwd=2, col= "red")
lines(x, pglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=5", Gen_Log_Distr_Func), 
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
# location 0.05767796 0.12045187
# scale    0.52390486 0.03432382
# shape    0.93099643 0.13631579
# Loglikelihood:  -693.1906   AIC:  1392.381   BIC:  1405.019 
# Correlation matrix:
#   location      scale      shape
# location  1.0000000 -0.7611068 -0.9393534
# scale    -0.7611068  1.0000000  0.8157231
# shape    -0.9393534  0.8157231  1.0000000
#
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_glogis_bd <- fitdistrplus::bootdist(fitdist_glogis, niter=1000)
fitdist_test[["glogis"]][["bootdist"]] <- fitdist_glogis_bd
summary(fitdist_glogis_bd)
# Parametric bootstrap medians and 95% percentile CI 
#             Median     2.5%     97.5%
# location -0.0145382 -0.3401486 0.2426516
# scale     0.5473206  0.4758294 0.6288735
# shape     1.0110592  0.7324895 1.4867812
# We fix the initial points of the constrained maximization procedure
location <- fitdist_glogis_bd[["fitpart"]][["estimate"]][1]
scale <- fitdist_glogis_bd[["fitpart"]][["estimate"]][2]
shape <- fitdist_glogis_bd[["fitpart"]][["estimate"]][3]
show(c(location,scale,shape)) # the estimated parameters.
# -0.006418736  0.548695880  1.004792941
minus_logLik <- function(x) -sum(log(dglogis(z_st, location=x[1], scale=x[2], shape=x[3]))) # the log-likelihood of the generalized logistic
# distribution.
fminunc_result <- fminunc(x0=c(location, scale, shape), fn=minus_logLik)   # the minimization procedure where (location,scale,shape) is the 
# starting point.
show(c(fminunc_result$par[1],fminunc_result$par[2],fminunc_result$par[3])) # the estimated parameters.
#    location       scale       shape 
# -0.006338434  0.548692452  1.004828309 

# Distribuzione normale generalizzata
fitdist_gnorm <- fitdistrplus::fitdist(z_st, "gnorm", start=list(mu=0, alpha=1, beta=1), method= "mle")
fitdist_test[["gnorm"]][["gnorm"]] <- fitdist_gnorm
summary(fitdist_gnorm)
# Fitting of the distribution ' t_ls ' by maximum likelihood 
# Parameters : 
#         estimate Std. Error
# mu    -0.008176826 0.04369701
# alpha  1.089159374 0.08845032
# beta   1.402292205 0.11599273
# Loglikelihood:  -699.3841   AIC:  1404.768   BIC:  1417.412 
# Correlation matrix:
#            mu      alpha       beta
# mu    1.00000000 0.02893693 0.03269519
# alpha 0.02893693 1.00000000 0.88532136
# beta  0.03269519 0.88532136 1.00000000
#
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_gnorm_bd <- fitdistrplus::bootdist(fitdist_gnorm, niter=1000)
fitdist_test[["gnorm"]][["bootdist"]] <- fitdist_gnorm_bd
summary(fitdist_gnorm_bd)
# Parametric bootstrap medians and 95% percentile CI 
#             Median     2.5%     97.5%
# mu    -0.006456984 -0.08638249 0.07189488
# alpha  1.092726065  0.89359291 1.29690118
# beta   1.409213148  1.17452750 1.71367706
mu <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["mu"]][1]
alpha <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["alpha"]][1]
beta <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["beta"]][1]
show(c(mu,alpha,beta)) # the estimated parameters.
# 0.04369701 0.08845032 0.11599273

# Setting
#
location <- fitdist_glogis[["estimate"]][["location"]]
scale <- fitdist_glogis[["estimate"]][["scale"]]
shape <- fitdist_glogis[["estimate"]][["shape"]]
# We plot the histogram and the empirical density function of the standardized residuals together with the density function of the estimated 
# generalized logistic and generalized normal.
Est_Gen_Log_Dens_Func <- bquote(paste("Estimated Generalized Logistic Density Function, location = ", .(loc), ", scale = ", .(scl) , ", shape = ", .(shp)))
hist(z_st, breaks= "Scott", col= "green", border= "black", xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), 
     freq=FALSE, main= "Density Histogram of the Standardized Residuals of the GARCH(1,1) model + Empirical Density Function + Estimated Generalized Logistic Density", 
     xlab= "Standardized Residuals", ylab= "Density")
lines(density(z_st), lwd=2, col= "darkgreen")
lines(x, dglogis(x, location=location, scale=scale, shape=shape), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", Est_Gen_Log_Dens_Func), 
       col=c("darkgreen", "magenta"),
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

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
mu <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["mu"]][1]
alpha <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["alpha"]][1]
beta <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["beta"]][1]
KS_z_st_t_ls <- stats::ks.test(z_st, y="pgnorm", mu=mu, alpha=alpha, beta=beta, alternative= "two.sided")
fitdist_test[["gnorm"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.49997, p-value < 2.2e-16
# alternative hypothesis: two-sided
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione normale generalizzata, quindi,
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
mu <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["mu"]][1]
alpha <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["alpha"]][1]
beta <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["beta"]][1]
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "pgnorm", mu=mu, alpha=alpha, beta=beta, estimated=FALSE)
fitdist_test[["gnorm"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘pgnorm’
# with parameters mu = 0.0436970062948844, alpha = 0.0884503208625058, beta = 0.115992731562846
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 41.664, p-value < 2.2e-16
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione normale generalizzata, quindi,
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
# An = 0.1698, p-value = 0.9965
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
mu <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["mu"]][1]
alpha <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["alpha"]][1]
beta <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["beta"]][1]
AD_z_st_t_ls <- goftest::ad.test(z_st, "pgnorm", mu=mu, alpha=alpha, beta=beta, estimated=FALSE)
fitdist_test[["gnorm"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘pgnorm’
# with parameters mu = 0.0436970062948844, alpha = 0.0884503208625058, beta = 0.115992731562846
# Parameters assumed to be fixed
# data:  z_st
# An = 193.14, p-value = 1.2e-06
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione normale generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata

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
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
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

#### DISTRIBUZIONE SIMMETRICA
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
# 0.1405681 -0.2265767  0.4301141 
kurt <- list()
set.seed(123)
k <- DescTools::Kurt(Xt_t_student_symmetric_garch1_q1_p2, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
kurt[['0.80']] <- k
show(k)
#      kurt     lwr.ci   upr.ci 
#    2.531558 1.586253 3.976148
set.seed(123)
k <- DescTools::Kurt(Xt_t_student_symmetric_garch1_q1_p2, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
kurt[['0.99']] <- k
show(k)
#      kurt     lwr.ci   upr.ci 
#    1.9298989 0.6348638 4.1983769 

Xt_t_student_symmetric_garch1_q1_p2_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_t_student_symmetric_garch1_q1_p2_cf <- append(Xt_t_student_symmetric_garch1_q1_p2_cf, list(cf))
show(cf)
# summary statistics
# ------
# min:  -3.336787   max:  2.808864 
# median:  0.001713917 
# mean:  -0.001669761 
# estimated sd:  0.70018 
# estimated skewness:  0.1405681 
# estimated kurtosis:  4.929899 
# Conferma visiva che l'ipotesi nulla di normalità bisogna scartarla all'1% di significatività 
# quindi possiamo dire che è una distribuzione simmetrica con una forte kurtosi.

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_symmetric_garch1_q1_p2_res
show(c(mean(y),var(y)))
# [1] 2.872810e-18 4.899532e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] 2.876172e-18 1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, graph=TRUE, boot=5000)
Xt_t_student_symmetric_garch1_q1_p2_cf <- append(Xt_t_student_symmetric_garch1_q1_p2_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -4.767843   max:  4.011907 
# median:  -0.01478639 
# mean:  2.876172e-18 
# estimated sd:  1 
# estimated skewness:  0.1210375 
# estimated kurtosis:  4.93264 
# Da Cullen-Frey, abbiamo una possibile distribuzione logistica o di student per i residui.
  
# Esploriamo queste possibilità in più dettagli.
z_st_qemp <- EnvStats::qemp(stats::ppoints(z_st), z_st) # Empirical quantiles of the residuals.
z_st_demp <- EnvStats::demp(z_st_qemp, z_st)     # Empirical probability density of the residuals.
z_st_pemp <- EnvStats::pemp(z_st_qemp, z_st)     # Empirical distribution function of the residuals.  
x <- z_st_qemp
y_d <- z_st_demp
y_p <- z_st_pemp
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata con il parametri location=0,
# scale=sqrt(3)/pi e shape=1.
loc <- 0
shp <- 1
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the GARCH(1,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dstd(x, mean=0, sd=1, nu=5), lwd=2, col= "red")
lines(x, dglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", "Student Density Function, df=5", Gen_Log_Dens_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
loc <- 0
shp <- 1
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(1,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pstd(x, mean = 0, sd = 1, nu = 5), lwd=2, col= "red")
lines(x, pglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=5", Gen_Log_Distr_Func), 
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
# location -0.07986713 0.14232289
# scale     0.55789615 0.03678236
# shape     1.08960103 0.17827817
# Loglikelihood:  -695.6324   AIC:  1397.265   BIC:  1409.909 
# Correlation matrix:
#           ocation      scale      shape
# location  1.0000000 -0.7928637 -0.9556487
# scale    -0.7928637  1.0000000  0.8245319
# shape    -0.9556487  0.8245319  1.0000000
#
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_glogis_bd <- fitdistrplus::bootdist(fitdist_glogis, niter=1000)
fitdist_test[["glogis"]][["bootdist"]] <- fitdist_glogis_bd
summary(fitdist_glogis_bd)
# Parametric bootstrap medians and 95% percentile CI 
#              Median       2.5%     97.5%
# location -0.08910874 -0.4312154 0.1812276
# scale     0.55609717  0.4848656 0.6374959
# shape     1.09984795  0.7867167 1.6383893
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
# -0.07911316  0.55761372  1.08873794 

# Dato che la Generalized Student distribution ha il parametro location che coincide con la media, possiamo provare 
# a fittare i dati con una generalized Student distribution con zero location. 
# Questo viene fatto cambiando il parametro m nella funzione *fitdistrplus::fitdist*.
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "std", start=list(nu=3.2), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' std ' by maximum likelihood 
# Parameters : 
#      estimate Std. Error
# nu 5.954581   1.219213
# Fixed parameters:
#       value
# mean     0
# sd       1
# Loglikelihood:  -695.1662   AIC:  1392.332   BIC:  1396.547 
#
# Fitting of the distribution ' std  ' by maximum likelihood 
# Parameters : 
#   estimate Std. Error
# s  0.7938275 0.04234867
# df 5.3709506 1.24061821
# Fixed parameters:
#     value
# m     0
# Loglikelihood:  -690.8996   AIC:  1385.799   BIC:  1394.228 
# Correlation matrix:
#        s        df
# s  1.0000000 0.6735108
# df 0.6735108 1.0000000

# Setting
location <- fitdist_glogis[["estimate"]][["location"]]
scale <- fitdist_glogis[["estimate"]][["scale"]]
shape <- fitdist_glogis[["estimate"]][["shape"]]
#
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata con il parametri location=0,
# scale=sqrt(3)/pi e shape=1.
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the GARCH(1,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dstd(x, mean=m, sd=sd, nu=df), lwd=2, col= "red")
lines(x, dglogis(x, location=location, scale=scale, shape=shape), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", "Student Density Function, df=5", Gen_Log_Dens_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(1,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pstd(x, mean=m, sd=sd, nu=df), lwd=2, col= "red")
lines(x, pglogis(x, location=location, scale=scale, shape=shape), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=5", Gen_Log_Distr_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

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
# D = 0.019284, p-value = 0.9924
# alternative hypothesis: two-sided
# Con un p-value cosi alto, non possiamo rigettare l'ipotesi nulla. Di conseguenza, abbiamo una prova sufficiente
# che la serie è una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="pstd", mean=m, sd=sd, nu=df, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.021027, p-value = 0.9799
# alternative hypothesis: two-sided
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.

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
# with parameters location = -0.0799, scale = 0.5579, shape = 1.0896
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.018809, p-value = 0.998
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "psstd", mean=m, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 6
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.69421, p-value = 0.01312
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
# with parameters location = -0.079867128219223, scale = 0.557896147664348, shape = 1.08960102791872
# Parameters assumed to be fixed
# data:  z_st
# An = 0.17649, p-value = 0.9955
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "pstd", mean=m, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parametersmean = 0, sd = 1, nu = 6
# Parameters assumed to be fixed
# data:  z_st
# An = 0.17715, p-value = 0.9954
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_symmetric_garch1_q1_p2_res
num_lags <- 6                   # Setting the lag parameter for the test.
Xt_t_student_symmetric_garch1_q1_p2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_symmetric_garch1_q1_p2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_t_student_symmetric_garch1_q1_p2_res   
Xt_t_student_symmetric_garch1_q1_p2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_symmetric_garch1_q1_p2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
# Si ha un p-value di 0.002732 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità
#
# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch1_q1_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch1_q1_p2)
show(Xt_t_student_symmetric_garch1_q1_p2_w)
# Si ha un p-value di 0.01121 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 0.23211, df = 1, p-value = 0.63
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 6  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_symmetric_garch1_q1_p2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_symmetric_garch1_q1_p2_lb)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag.
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

modello[['simulazione']][['garch_q1_p2']][['simmetrico']][['1']] <- append(modello[['simulazione']][['garch_q1_p2']][['simmetrico']][['1']], 
                                                                           list('lm'=Xt_t_student_symmetric_garch1_q1_p2_lm, 'skew'=skew, 'kurt'=kurt,
                                                                                'Cullen-Frey'=Xt_t_student_symmetric_garch1_q1_p2_cf, 'Breusch-Pagan'=Xt_t_student_symmetric_garch1_q1_p2_bp, 'White'=Xt_t_student_symmetric_garch1_q1_p2_w, 
                                                                                'Ljiung-Box'=Xt_t_student_symmetric_garch1_q1_p2_lb, 'Dickey-Fuller'=Xt_t_student_symmetric_garch1_q1_p2_adf, 
                                                                                'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_garch1_q1_p2_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_garch1_q1_p2_sw,  
                                                                                'Generalized Distribution'=fitdist_test))

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
skew <- list()
set.seed(123)
s <- DescTools::Skew(Xt, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew[["0.80"]] <- s
show(s)
#  skew        lwr.ci      upr.ci 
#  0.28508979 -0.06846262  0.74951363 
set.seed(123)
s <- DescTools::Skew(Xt, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
skew[["0.99"]] <- s
show(s)
#  skew        lwr.ci      upr.ci 
# 0.2850898 -0.3726066  1.0311312 
kurt <- list()
set.seed(123)
k <- DescTools::Kurt(Xt, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
kurt[["0.80"]] <- k
show(k)
#      kurt      lwr.ci    upr.ci 
#    3.399481 2.503734 4.530410
set.seed(123)
k <- DescTools::Kurt(Xt, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
kurt[["0.99"]] <- k
show(k)
#      kurt      lwr.ci   upr.ci 
#    3.399481 1.603617 6.157511 

Xt_t_student_symmetric_garch1_q1_p2_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_t_student_symmetric_garch1_q1_p2_cf <- append(Xt_t_student_symmetric_garch1_q1_p2_cf, list(cf))
show(cf)
# summary statistics
# ------
# min:  -2.623291   max:  2.816715 
# median:  0.0004635091 
# mean:  0.007670484 
# estimated sd:  0.600683 
# estimated skewness:  0.2850898 
# estimated kurtosis:  6.399481 
# Conferma visiva che l'ipotesi nulla di normalità bisogna scartarla all'1% di significatività 
# quindi possiamo dire che è una distribuzione simmetrica con una forte kurtosi.

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_symmetric_garch1_q1_p2_res
show(c(mean(y),var(y)))
# [1] -4.975404e-19  3.605476e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] 2.708229e-18 1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, graph=TRUE, boot=5000)
Xt_t_student_symmetric_garch1_q1_p2_cf <- append(Xt_t_student_symmetric_garch1_q1_p2_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -4.385124   max:  4.633192 
# median:  0.00393389 
# mean:  2.708229e-18 
# estimated sd:  1 
# estimated skewness:  0.2225245 
# estimated kurtosis:  6.358811  
# Da Cullen-Frey, abbiamo una possibile distribuzione logistica o di student per i residui.

# Esploriamo queste possibilità in più dettagli.
z_st_qemp <- EnvStats::qemp(stats::ppoints(z_st), z_st) # Empirical quantiles of the residuals.
z_st_demp <- EnvStats::demp(z_st_qemp, z_st)     # Empirical probability density of the residuals.
z_st_pemp <- EnvStats::pemp(z_st_qemp, z_st)     # Empirical distribution function of the residuals.  
x <- z_st_qemp
y_d <- z_st_demp
y_p <- z_st_pemp
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata con il parametri location=0,
# scale=sqrt(3)/pi e shape=1.
loc <- 0
shp <- 1
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the GARCH(1,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dstd(x, mean=0, sd=1, nu=5), lwd=2, col= "red")
lines(x, dglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", "Student Density Function, df=5", Gen_Log_Dens_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
loc <- 0
shp <- 1
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(1,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pstd(x, mean = 0, sd = 1, nu = 5), lwd=2, col= "red")
lines(x, pglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=5", Gen_Log_Distr_Func), 
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
# location 0.03583481 0.10292542
# scale    0.50138542 0.03072564
# shape    0.94328082 0.12478043
# Loglikelihood:  -678.3146   AIC:  1362.629   BIC:  1375.273 
# Correlation matrix:
#            location      scale      shape
# location  1.0000000 -0.7170000 -0.9260051
# scale    -0.7170000  1.0000000  0.7810496
# shape    -0.9260051  0.7810496  1.0000000
#
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_glogis_bd <- fitdistrplus::bootdist(fitdist_glogis, niter=1000)
fitdist_test[["glogis"]][["bootdist"]] <- fitdist_glogis_bd
summary(fitdist_glogis_bd)
# Parametric bootstrap medians and 95% percentile CI 
#              Median       2.5%     97.5%
# location 0.02927614 -0.2613023 0.2629313
# scale    0.49999265  0.4345270 0.5750452
# shape    0.94966481  0.6932612 1.3803121
# We fix the initial points of the constrained maximization procedure
location <- fitdist_glogis_bd[["fitpart"]][["estimate"]][1]
scale <- fitdist_glogis_bd[["fitpart"]][["estimate"]][2]
shape <- fitdist_glogis_bd[["fitpart"]][["estimate"]][3]
minus_logLik <- function(x) -sum(log(dglogis(z_st, location=x[1], scale=x[2], shape=x[3]))) # the log-likelihood of the generalized logistic
# distribution.
fminunc_result <- fminunc(x0=c(location, scale, shape), fn=minus_logLik)   # the minimization procedure where (location,scale,shape) is the 
# starting point.
show(c(fminunc_result$par[1],fminunc_result$par[2],fminunc_result$par[3])) # the estimated parameters.
#  location      scale      shape 
#  0.03597035 0.50147809 0.94326607  

# Dato che la Generalized Student distribution ha il parametro location che coincide con la media, possiamo provare 
# a fittare i dati con una generalized Student distribution con zero location. 
# Questo viene fatto cambiando il parametro m nella funzione *fitdistrplus::fitdist*.
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "std", start=list(nu=3.2), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' std  ' by maximum likelihood 
# Parameters : 
#   estimate Std. Error
# nu 3.496084  0.3018963
# Fixed parameters:
#        value
# mean     0
# sd       1
# Loglikelihood:  -669.0401   AIC:  1340.08   BIC:  1344.295 

# Setting
location <- round(fitdist_glogis[["estimate"]][["location"]],4)
sscale <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shape <- round(fitdist_glogis[["estimate"]][["shape"]],4)
#
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata con il parametri location=0,
# scale=sqrt(3)/pi e shape=1.
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the GARCH(1,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dstd(x, mean=m, sd=sd, nu=df), lwd=2, col= "red")
lines(x, dglogis(x, location=location, scale=scale, shape=shape), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", "Student Density Function, df=5", Gen_Log_Dens_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(1,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pstd(x, mean=m, sd=sd, nu=df), lwd=2, col= "red")
lines(x, pglogis(x, location=location, scale=scale, shape=shape), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=5", Gen_Log_Distr_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

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
# D = 0.047802, p-value = 0.2033
# alternative hypothesis: two-sided
# Non possiamo rifiutare l'ipotesi nulla che la serie è una distribuzione logistica generalizzata al livello di significativitò
# del 5% ma non del 1%. 
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="pstd", mean=m, sd=sd, nu=df, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.043184, p-value = 0.3087
# alternative hypothesis: two-sided
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.

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
# with parameters location = 0.0358, scale = 0.5014, shape = 0.9433
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.27289, p-value = 0.1615
# Non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "psstd", mean=m, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 3
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 1.7862, p-value = 3.786e-05
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.
loc <- round(fitdist_glogis[["estimate"]][["location"]],4)
scl <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shp <- round(fitdist_glogis[["estimate"]][["shape"]],4)
AD_z_st_glogis <- goftest::ad.test(z_st, null= "pglogis", location=loc, scale=scl, shape=shp, estimated=FALSE)
fitdist_test[["glogis"]][["Anderson-Darling"]] <- AD_z_st_glogis
show(AD_z_st_glogis)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘pglogis’
# with parameters location = 0.0358, scale = 0.5014, shape = 0.9433
# Parameters assumed to be fixed
# data:  z_st
# An = 1.671, p-value = 0.1405
# Non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "pstd", mean=m, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters   mean = 0, sd = 1, nu = 3
# Parameters assumed to be fixed
# data:  z_st
# An =  1.2705, p-value = 0.2422
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_symmetric_garch1_q1_p2_res
num_lags <- 6                   # Setting the lag parameter for the test.
Xt_t_student_symmetric_garch1_q1_p2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_symmetric_garch1_q1_p2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_t_student_symmetric_garch1_q1_p2_res   
Xt_t_student_symmetric_garch1_q1_p2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_symmetric_garch1_q1_p2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
# Si ha un p-value di 1.759e-13 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità
#
# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch1_q1_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch1_q1_p2)
show(Xt_t_student_symmetric_garch1_q1_p2_w)
# Si ha un p-value di 3.639e-13 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 0.80544, df = 1, p-value = 0.3695
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 6  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
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
                                                                       'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_garch1_q1_p2_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_garch1_q1_p2_sw,    
                                                                       'Generalized Distribution'=fitdist_test)))

# In questo modello Garch(1,1) con una distribuzione t-student simmetrica, con i nuovi parametri stimati,
# si ha evidenza di eteroschedasticità e assenza di autocorrelazione.

##########################################

#### DSITRIBUZIONE ASIMMETRICA
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
skew <- list()
set.seed(123)
s <- DescTools::Skew(Xt_t_student_asymmetric_garch1_q1_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew[["0.80"]] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -1.319395 -1.668872 -1.050046
set.seed(123)
s <- DescTools::Skew(Xt_t_student_asymmetric_garch1_q1_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
skew[["0.99"]] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -1.3193946 -2.1228276 -0.8336835 
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_asymmetric_garch1_q1_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt   lwr.ci   upr.ci 
# 3.659129 2.538755 5.530543 

Xt_t_student_asymmetric_garch1_q1_p2_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_t_student_asymmetric_garch1_q1_p2_cf <- append(Xt_t_student_asymmetric_garch1_q1_p2_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:   -4.020476   max:  1.788635 
# median:  0.06985381 
# mean:  -0.01048066 
# estimated sd:  0.7604086 
# estimated skewness:  -1.335765 
# estimated kurtosis:  6.680125 

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_asymmetric_garch1_q1_p2_res
show(c(mean(y),var(y)))
# [1] -1.295513e-17  5.779383e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] -1.438129e-17  1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, graph=TRUE, boot=5000)
Xt_t_student_asymmetric_garch1_q1_p2_cf <- append(Xt_t_student_asymmetric_garch1_q1_p2_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -5.285409   max:  2.380731 
# median:  0.112653 
# mean:  -1.438129e-17 
# estimated sd:  1 
# estimated skewness:  -1.319395 
# estimated kurtosis:  6.659129 

fitdist_test <- list()
# Dato che la Generalized Student distribution ha il parametro location che coincide con la media, possiamo provare 
# a fittare i dati con una generalized Student distribution con zero location. 
# Questo viene fatto cambiando il parametro m nella funzione *fitdistrplus::fitdist*.
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "sstd", start=list(nu=3.2, xi=1.5), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' t_ls ' by maximum likelihood 
# Parameters : 
#     estimate Std. Error
# nu  4.9358155 0.72907552
# xi 0.6995796 0.04252452
# Fixed parameters:
#   value
# mean     0
# sd       1
# Loglikelihood:  -660.9355   AIC:  1325.871   BIC:  1334.3 
# Correlation matrix:
#        nu        xi
# nu  1.000000 -0.295368
# xi -0.295368  1.000000

# Alla fine, consideriamo il test Goodness of fit.
# The Kolmogorov-Smirnov test in the library *stats*
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="psstd", mean=m, sd=sd, nu=df, xi=xi, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.070426, p-value = 0.01403
# alternative hypothesis: two-sided
# Possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.
# Ma al 1% del livello di significatività non possiamo rigettare l'ipotesi nulla.

# Cramer-von Mises test of goodness-of-fit
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "psstd", mean=0, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters  mean = 0, sd = 1, nu = 5, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.85377, p-value = 0.005422
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "psstd", mean=m, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 5, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# An = 4.6005, p-value = 0.004462
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_garch1_q1_p2_res
num_lags <- 6                   # Setting the lag parameter for the test.
Xt_t_student_asymmetric_garch1_q1_p2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_garch1_q1_p2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_t_student_asymmetric_garch1_q1_p2_res   
Xt_t_student_asymmetric_garch1_q1_p2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_garch1_q1_p2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
# Si ha un p-value di 0.01438 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità
#
# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q1_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q1_p2)
show(Xt_t_student_asymmetric_garch1_q1_p2_w)
# Si ha un p-value di 0.04991 > 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 1.6547, df = 1, p-value = 0.1983
T <- length(y)
n_pars <- 6  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
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
                                                                                 'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_garch1_q1_p2_kpss, 'Shapiro-Wilk'=Xt_t_student_asymmetric_garch1_q1_p2_sw,                
                                                                                 'Generalized Distribution'=fitdist_test))

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
skew <- list()
set.seed(123)
s <- DescTools::Skew(Xt_t_student_asymmetric_garch1_q1_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew[["0.80"]] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -1.319395 -1.668872 -1.050046 
set.seed(123)
s <- DescTools::Skew(Xt_t_student_asymmetric_garch1_q1_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
skew[["0.99"]] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -1.3193946 -2.1228276 -0.8336835 
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_asymmetric_garch1_q1_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt   lwr.ci   upr.ci 
# 3.659129 2.538755 5.530543

Xt_t_student_asymmetric_garch1_q1_p2_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_t_student_asymmetric_garch1_q1_p2_cf <- append(Xt_t_student_asymmetric_garch1_q1_p2_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -4.020476   max:  1.788635 
# median:  0.06985381 
# mean:  -0.01048066 
# estimated sd:  0.7604086 
# estimated skewness:  -1.335765 
# estimated kurtosis:  6.680125 

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_asymmetric_garch1_q1_p2_res
show(c(mean(y),var(y)))
# [1] -1.295513e-17  5.779383e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] -1.438129e-17  1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, graph=TRUE, boot=5000)
Xt_t_student_asymmetric_garch1_q1_p2_cf <- append(Xt_t_student_asymmetric_garch1_q1_p2_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -5.285409   max:  2.380731 
# median:  0.112653 
# mean:  -1.438129e-17 
# estimated sd:  1 
# estimated skewness:  -1.319395 
# estimated kurtosis:  6.659129  

fitdist_test <- list()
# Dato che la Generalized Student distribution ha il parametro location che coincide con la media, possiamo provare 
# a fittare i dati con una generalized Student distribution con zero location. 
# Questo viene fatto cambiando il parametro m nella funzione *fitdistrplus::fitdist*.
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "sstd", start=list(nu=3.2, xi=1.5), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' t_ls ' by maximum likelihood 
# Parameters : 
#     estimate Std. Error
# nu 4.9358155 0.72907552
# xi 0.6995796 0.04252452
# Fixed parameters:
#   value
# mean     0
# sd       1
# Loglikelihood:  -660.9355   AIC:  1325.871   BIC:  1334.3 
# Correlation matrix:
#        nu        xi
# nu  1.000000 -0.295368
# xi -0.295368  1.000000

# Alla fine, consideriamo il test Goodness of fit.
# The Kolmogorov-Smirnov test in the library *stats*
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="psstd", mean=m, sd=sd, nu=df, xi=xi, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.070426, p-value = 0.01403
# alternative hypothesis: two-sided
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# Cramer-von Mises test of goodness-of-fit
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "psstd", mean=m, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 5, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.85377, p-value = 0.005422
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "psstd", mean=m, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 5, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# An = 4.6005, p-value = 0.004462
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_garch1_q1_p2_res
num_lags <- 6                   # Setting the lag parameter for the test.
Xt_t_student_asymmetric_garch1_q1_p2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_garch1_q1_p2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_t_student_asymmetric_garch1_q1_p2_res   
Xt_t_student_asymmetric_garch1_q1_p2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_garch1_q1_p2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
# Si ha un p-value di 0.01438 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità
#
# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q1_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q1_p2)
show(Xt_t_student_asymmetric_garch1_q1_p2_w)
# Si ha un p-value di 0.04991 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 1.6547, df = 1, p-value = 0.1983
T <- length(y)
n_pars <- 6  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
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
                                                                        'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_garch1_q1_p2_kpss, 'Shapiro-Wilk'=Xt_t_student_asymmetric_garch1_q1_p2_sw,
                                                                        'Generalized Distribution'=fitdist_test)))

# In questo modello Garch(1,2) con una distribuzione t-student asimmetrica, con i nuovi parametri stimati,
# presenta eteroschedasticità nella serie e assenza di autocorrelazione.

##########################################
##########################################

#### DISTRIBUZIONE NORMALE
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
skew <- list()
set.seed(123)
s <- DescTools::Skew(Xt_normal_arch1_q2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew[['0.80']] <- s
s
#     skew      lwr.ci      upr.ci 
# -0.12103715 -0.27716327  0.01932358
set.seed(123)
s <- DescTools::Skew(Xt_normal_arch1_q2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
skew[['0.99']] <- s
s
#     skew      lwr.ci      upr.ci 
# -0.1210372 -0.3951437  0.1461877
kurt <- list()
set.seed(123)
k <- DescTools::Kurt(Xt_normal_arch1_q2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
kurt[['0.80']] <- k
k
#      kurt     lwr.ci     upr.ci 
# -0.09836765 -0.38301354  0.30008145 
set.seed(123)
k <- DescTools::Kurt(Xt_normal_arch1_q2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
kurt[['0.99']] <- k
k
#      kurt     lwr.ci     upr.ci 
# -0.09836765 -0.59905511  0.79380037 

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_normal_arch1_q2_cf <- descdist(Xt, discrete=FALSE, boot=5000)
# Da Cullen-Frey si ha una distribuzione normale.

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
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
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
skew <- list()
set.seed(123)
s <- DescTools::Skew(Xt_normal_arch1_q2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew[['0.80']] <- s
s
#     skew      lwr.ci      upr.ci 
# -0.05299184 -0.17462423  0.05864336 
set.seed(123)
s <- DescTools::Skew(Xt_normal_arch1_q2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
skew[['0.99']] <- s
s
#     skew      lwr.ci      upr.ci 
# -0.05299184 -0.28981124  0.15265276 
kurt <- list()
set.seed(123)
k <- DescTools::Kurt(Xt_normal_arch1_q2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
kurt[['0.80']] <- k
k
#      kurt     lwr.ci     upr.ci 
# -0.30593670 -0.47404313 -0.07316509  
set.seed(123)
k <- DescTools::Kurt(Xt_normal_arch1_q2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
kurt[['0.99']] <- k
k
#      kurt     lwr.ci     upr.ci 
# -0.3059367 -0.6259383  0.2754610  

Xt_normal_arch1_q2_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_normal_arch1_q2_cf <- append(Xt_normal_arch1_q2_cf, cf)
show(cf)
# summary statistics
# ------
# min:  -3.184772   max:  2.784073 
# median:  -0.0184342 
# mean:  -0.01015675 
# estimated sd:  1.033983 
# estimated skewness:  -0.02657078 
# estimated kurtosis:  2.693313 

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
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
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

#### DISTRIBUZIONE T-STUDENT SIMMETRICA
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
#  0.0477896 -0.4597433  0.4788980 
set.seed(123)
kurt <- kurt <- DescTools::Kurt(Xt_t_student_symmetric_arch2_q2, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#      kurt      lwr.ci      upr.ci 
#     6.405644  3.595817 10.055224

Xt_t_student_symmetric_arch2_q2_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_t_student_symmetric_arch2_q2_cf <- append(Xt_t_student_symmetric_arch2_q2_cf, cf)
show(cf)
# summary statistics
# ------
# min:  -3.284355   max:  3.178185 
# median:  -0.0329561 
# mean:  -0.02391484 
# estimated sd:  0.6538484 
# estimated skewness:  0.0477896 
# estimated kurtosis:  6.234913 

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_symmetric_arch2_q2_res
show(c(mean(y),var(y)))
# [1] 3.815633e-18 4.272211e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] 6.784124e-18 1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, graph=TRUE, boot=5000)
Xt_t_student_symmetric_arch2_q2_cf <- append(Xt_t_student_symmetric_arch2_q2_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -5.016809   max:  4.870653 
# median:  -0.01403835 
# mean:  6.784124e-18 
# estimated sd:  1 
# estimated skewness:  0.0330962 
# estimated kurtosis:  6.223267 

# Esploriamo queste possibilità in più dettagli.
z_st_qemp <- EnvStats::qemp(stats::ppoints(z_st), z_st) # Empirical quantiles of the residuals.
z_st_demp <- EnvStats::demp(z_st_qemp, z_st)     # Empirical probability density of the residuals.
z_st_pemp <- EnvStats::pemp(z_st_qemp, z_st)     # Empirical distribution function of the residuals.  
x <- z_st_qemp
y_d <- z_st_demp
y_p <- z_st_pemp
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata con il parametri location=0,
# scale=sqrt(3)/pi e shape=1.
loc <- 0
shp <- 1
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the ARCH(2) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dstd(x, mean=0, sd=1, nu=5), lwd=2, col= "red")
lines(x, dglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", "Student Density Function, df=5", Gen_Log_Dens_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
loc <- 0
shp <- 1
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the ARCH(2) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pstd(x, mean = 0, sd = 1, nu = 5), lwd=2, col= "red")
lines(x, pglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=5", Gen_Log_Distr_Func), 
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
# location 0.01318935 0.11641283
# scale    0.52018156 0.03299072
# shape    0.97590034 0.13999702
#Loglikelihood:  -685.1963   AIC:  1376.393   BIC:  1389.036 
# Correlation matrix:
#        location      scale     shape
# location  1.0000000 -0.7505517 -0.938574
# scale    -0.7505517  1.0000000  0.802224
# shape    -0.9385740  0.8022240  1.000000
#
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_glogis_bd <- fitdistrplus::bootdist(fitdist_glogis, niter=1000)
fitdist_test[["glogis"]][["bootdist"]] <- fitdist_glogis_bd
summary(fitdist_glogis_bd)
# Parametric bootstrap medians and 95% percentile CI 
#            Median       2.5%     97.5%
# location 0.005994848 -0.3002384 0.2472756
# scale    0.518903793  0.4507412 0.5964519
# shape    0.981520199  0.7132867 1.4386685
# We fix the initial points of the constrained maximization procedure
location <- fitdist_glogis_bd[["fitpart"]][["estimate"]][1]
scale <- fitdist_glogis_bd[["fitpart"]][["estimate"]][2]
shape <- fitdist_glogis_bd[["fitpart"]][["estimate"]][3]
minus_logLik <- function(x) -sum(log(dglogis(z_st, location=x[1], scale=x[2], shape=x[3]))) # the log-likelihood of the generalized logistic
# distribution.
fminunc_result <- fminunc(x0=c(location, scale, shape), fn=minus_logLik)   # the minimization procedure where (location,scale,shape) is the 
# starting point.
show(c(fminunc_result$par[1],fminunc_result$par[2],fminunc_result$par[3])) # the estimated parameters.
#   location    scale       shape 
# 0.01355793 0.52011496 0.97538212  
#
logLik <- -fminunc_result$value # the minimized negative log-likelihood
n <- length(z_st)
k <- length(fminunc_result[["par"]])
AIC <- 2*k-2*logLik
BIC <- k*log(n)-2*logLik
AICc <- AIC + 2*k*((k+1)/(n-k-1))
show(c(logLik, AIC, BIC, AICc))
#    logLik    AIC       BIC      AICc
# -685.1963 1376.3926 1389.0364 1376.4410
#
fitdist_glogis_location <- as.numeric(fitdist_glogis$estimate[1])
fitdist_glogis_scale <- as.numeric(fitdist_glogis$estimate[2])
fitdist_glogis_shape <- as.numeric(fitdist_glogis$estimate[3])
# Notiamo:
round(c(fitdist_glogis_location, fitdist_glogis_scale, fitdist_glogis_shape),4)
# location   scale    shape
#  0.0132 0.5202 0.9759
round(c(fminunc_result$par[1],fminunc_result$par[2],fminunc_result$par[3]),4)
# location   scale    shape
# 0.0136   0.5201   0.9754  

# Dato che la Generalized Student distribution ha il parametro location che coincide con la media, possiamo provare 
# a fittare i dati con una generalized Student distribution con zero location. 
# Questo viene fatto cambiando il parametro m nella funzione *fitdistrplus::fitdist*.
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "std", start=list(nu=3.2), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' t_ls ' by maximum likelihood 
# Parameters : 
#   estimate Std. Error
# nu 4.256257  0.5440075
# Fixed parameters:
#       value
# mean     0
# sd       1
#Loglikelihood:  -681.2307   AIC:  1364.461   BIC:  1368.676 

# Setting
loc <- fitdist_glogis[["estimate"]][["location"]]
scl <- fitdist_glogis[["estimate"]][["scale"]]
shp <- fitdist_glogis[["estimate"]][["shape"]]
#
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata con il parametri location=0,
# scale=sqrt(3)/pi e shape=1.
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", scl, ", shape = ", .(shp)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the ARCH(2) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dstd(x, mean=m, sd=sd, nu=df), lwd=2, col= "red")
lines(x, dglogis(x, location=loc, scale=scl, shape=shp), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", "Student Density Function, df=5", Gen_Log_Dens_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the ARCH(2) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pstd(x, mean=m, sd=sd, nu=df), lwd=2, col= "red")
lines(x, pglogis(x, location=loc, scale=scl, shape=shp), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=5", Gen_Log_Distr_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

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
# D = 0.033191, p-value = 0.6403
# alternative hypothesis: two-sided
# Con un p-value cosi alto, non possiamo rigettare l'ipotesi nulla. Di conseguenza, abbiamo una prova sufficiente
# che la serie è una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="pstd", mean=m, sd=sd, nu=df, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.021119, p-value = 0.979
# alternative hypothesis: two-sided
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.
#

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
# with parameters location = 0.0132, scale = 0.5202, shape = 0.9759
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.094272, p-value = 0.6139
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "pstd", mean=m, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 4
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.029751, p-value = 0.9771
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.
loc <- round(fitdist_glogis[["estimate"]][["location"]],4)
scl <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shp <- round(fitdist_glogis[["estimate"]][["shape"]],4)
AD_z_st_glogis <- goftest::ad.test(z_st, null= "pglogis", location=location, scale=scale, shape=shape, estimated=FALSE)
fitdist_test[["glogis"]][["Anderson-Darling"]] <- AD_z_st_glogis
show(AD_z_st_glogis)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘pglogis’
# with parameters location = 0.0131893459664435, scale = 0.520181556933482, shape = 0.975900340456598
# Parameters assumed to be fixed
# data:  z_st
# An = 0.59025, p-value = 0.6572
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "pstd", mean=m, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 4
# Parameters assumed to be fixed
# data:  z_st
# An = 0.23432, p-value = 0.9781
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_symmetric_arch2_q2_res
num_lags <- 4                   # Setting the lag parameter for the test.
Xt_t_student_symmetric_arch2_q2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_symmetric_arch2_q2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_t_student_symmetric_arch2_q2_res   
Xt_t_student_symmetric_arch2_q2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_symmetric_arch2_q2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
# Si ha un p-value di 0.06779 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità
#
# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_arch2_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_arch2_q2)
show(Xt_t_student_symmetric_arch2_q2_w)
# Si ha un p-value di 0.143 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 0.51582, df = 1, p-value = 0.4726
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 4  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
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
                                                                            'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_arch2_q2_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_arch2_q2_sw,
                                                                            'Generalized Distribution'=fitdist_test))

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
#  0.0477896 -0.4597433  0.4788980
set.seed(123)
kurt <- kurt <- DescTools::Kurt(Xt_t_student_symmetric_arch2_q2, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#      kurt      lwr.ci      upr.ci 
#     3.234913 2.024320 4.740100

Xt_t_student_symmetric_arch2_q2_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_t_student_symmetric_arch2_q2_cf <- append(Xt_t_student_symmetric_arch2_q2_cf, cf)
show(cf)
# summary statistics
# ------
# min:  -4.302721   max:  3.548004 
# median:  -0.05402259 
# mean:  -0.04114046 
# estimated sd:  0.9674306 
# estimated skewness:  -0.05144538 
# estimated kurtosis:  4.900747 

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_symmetric_arch2_q2_res
show(c(mean(y),var(y)))
# [1] 1.19838e-17 9.35246e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] 1.066692e-17 1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, graph=TRUE, boot=5000)
Xt_t_student_symmetric_arch2_q2_cf <- append(Xt_t_student_symmetric_arch2_q2_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -4.435761   max:  3.665265 
# median:  -0.0110929 
# mean:  1.066692e-17 
# estimated sd:  1 
# estimated skewness:  -0.0628364 
# estimated kurtosis:  4.88613 

# Esploriamo queste possibilità in più dettagli.
z_st_qemp <- EnvStats::qemp(stats::ppoints(z_st), z_st) # Empirical quantiles of the residuals.
z_st_demp <- EnvStats::demp(z_st_qemp, z_st)     # Empirical probability density of the residuals.
z_st_pemp <- EnvStats::pemp(z_st_qemp, z_st)     # Empirical distribution function of the residuals.  
x <- z_st_qemp
y_d <- z_st_demp
y_p <- z_st_pemp
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata con il parametri location=0,
# scale=sqrt(3)/pi e shape=1.
loc <- 0
shp <- 1
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the ARCH(2) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dstd(x, mean=0, sd=1, nu=5), lwd=2, col= "red")
lines(x, dglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", "Student Density Function, df=5", Gen_Log_Dens_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
loc <- 0
shp <- 1
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the ARCH(2) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pstd(x, mean = 0, sd = 1, nu = 5), lwd=2, col= "red")
lines(x, pglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=5", Gen_Log_Distr_Func), 
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
# location 0.02428212 0.12255514
# scale    0.53013874 0.03434561
# shape    0.96704017 0.14276981
# Loglikelihood:  -693.5562   AIC:  1393.112   BIC:  1405.756 
# Correlation matrix:
#  location      scale      shape
# location  1.0000000 -0.7621369 -0.9417586
# scale    -0.7621369  1.0000000  0.8123080
# shape    -0.9417586  0.8123080  1.0000000
#
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_glogis_bd <- fitdistrplus::bootdist(fitdist_glogis, niter=1000)
fitdist_test[["glogis"]][["bootdist"]] <- fitdist_glogis_bd
summary(fitdist_glogis_bd)
# Parametric bootstrap medians and 95% percentile CI 
#            Median       2.5%     97.5%
# location 0.01712643 -0.2941393 0.2634145
# scale    0.52870543  0.4592990 0.6079342
# shape    0.97269156  0.7075249 1.4238595
# We fix the initial points of the constrained maximization procedure
location <- fitdist_glogis_bd[["fitpart"]][["estimate"]][1]
scale <- fitdist_glogis_bd[["fitpart"]][["estimate"]][2]
shape <- fitdist_glogis_bd[["fitpart"]][["estimate"]][3]
minus_logLik <- function(x) -sum(log(dglogis(z_st, location=x[1], scale=x[2], shape=x[3]))) # the log-likelihood of the generalized logistic
# distribution.
fminunc_result <- fminunc(x0=c(location, scale, shape), fn=minus_logLik)   # the minimization procedure where (location,scale,shape) is the 
# starting point.
show(c(fminunc_result$par[1],fminunc_result$par[2],fminunc_result$par[3])) # the estimated parameters.
#   location    scale       shape 
# 0.0239515 0.5301768 0.9674039 
#
logLik <- -fminunc_result$value # the minimized negative log-likelihood
n <- length(z_st)
k <- length(fminunc_result[["par"]])
AIC <- 2*k-2*logLik
BIC <- k*log(n)-2*logLik
AICc <- AIC + 2*k*((k+1)/(n-k-1))
show(c(logLik, AIC, BIC, AICc))
#    logLik    AIC       BIC      AICc
#  -693.5562 1393.1124 1405.7562 1393.1608
#
fitdist_glogis_location <- as.numeric(fitdist_glogis$estimate[1])
fitdist_glogis_scale <- as.numeric(fitdist_glogis$estimate[2])
fitdist_glogis_shape <- as.numeric(fitdist_glogis$estimate[3])
# Notiamo:
round(c(fitdist_glogis_location, fitdist_glogis_scale, fitdist_glogis_shape),4)
# location   scale    shape
# 0.0243 0.5301 0.9670
round(c(fminunc_result$par[1],fminunc_result$par[2],fminunc_result$par[3]),4)
# location   scale    shape
#  0.0240   0.5302   0.9674 

# Dato che la Generalized Student distribution ha il parametro location che coincide con la media, possiamo provare 
# a fittare i dati con una generalized Student distribution con zero location. 
# Questo viene fatto cambiando il parametro m nella funzione *fitdistrplus::fitdist*.
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "std", start=list(nu=3.2), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' t_ls ' by maximum likelihood 
# Parameters : 
#   estimate Std. Error
# nu 5.026982  0.8611653
# Fixed parameters:
#   value
# mean     0
# sd       1
# Loglikelihood:  -692.5503   AIC:  1387.101   BIC:  1391.315 

# Setting
loc <- fitdist_glogis[["estimate"]][["location"]]
scl <- fitdist_glogis[["estimate"]][["scale"]]
shp <- fitdist_glogis[["estimate"]][["shape"]]
#
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata con il parametri location=0,
# scale=sqrt(3)/pi e shape=1.
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", scl, ", shape = ", .(shp)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the ARCH(2) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dstd(x, mean=m, sd=sd, nu=df), lwd=2, col= "red")
lines(x, dglogis(x, location=loc, scale=scl, shape=shp), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", "Student Density Function, df=5", Gen_Log_Dens_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the ARCH(2) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pstd(x, mean=m, sd=sd, nu=df), lwd=2, col= "red")
lines(x, pglogis(x, location=loc, scale=scl, shape=shp), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=5", Gen_Log_Distr_Func), 
       col=c("darkblue", "red","darkgreen","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

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
# D = 0.025986, p-value = 0.8883
# alternative hypothesis: two-sided
# Con un p-value cosi alto, non possiamo rigettare l'ipotesi nulla. Di conseguenza, abbiamo una prova sufficiente
# che la serie è una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="pstd", mean=m, sd=sd, nu=df, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.020747, p-value = 0.9825
# alternative hypothesis: two-sided
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.

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
# with parameters location = 0.0243, scale = 0.5301, shape = 0.967
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.057778, p-value = 0.8282
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "pstd", mean=m, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘pstd’
# with parameters mean = 0, sd = 1, nu = 5
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.024975, p-value = 0.9897
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
# with parameters location = 0.0242821159700998, scale = 0.530138740070849, shape = 0.967040165803978
# Parameters assumed to be fixed
# data:  z_st
# An = 0.3979, p-value = 0.8508
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "pstd", mean=m, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 5
# Parameters assumed to be fixed
# data:  z_st
# An = 0.19936, p-value = 0.9906
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata

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
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
# Si ha un p-value di 0.111 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità
#
# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_arch2_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_arch2_q2)
show(Xt_t_student_symmetric_arch2_q2_w)
# Si ha un p-value di 0.2541 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 0.19128, df = 1, p-value = 0.6619
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 4  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
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
                                                                                                      'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_arch2_q2_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_arch2_q2_sw,    
                                                                                                      'Generalized Distribution'=fitdist_test)))

# In questo modello Arch(2) con una distribuzione t-student simmetrica si ha evidenza di omoschedasticità nella serie
# e assenza di autocorrelazione nei residui.

##########################################

#### DISTRIBUZIONE T-STUDENT ASIMMETRICA
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
skew <- list()
set.seed(123)
s <- DescTools::Skew(Xt_t_student_asymmetric_arch1_q2, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew[['0.80']] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -1.054419 -1.367669 -0.698684  
set.seed(123)
s <- DescTools::Skew(Xt_t_student_asymmetric_arch1_q2, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
skew[['0.99']] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -1.0544191 -1.6279523 -0.1983669  
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_asymmetric_arch1_q2, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#   kurt   lwr.ci   upr.ci 
# 3.445536 2.659977 4.614481  

Xt_t_student_asymmetric_arch1_q2_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, method= "sample", boot=2000)
Xt_t_student_asymmetric_arch1_q2_cf <- append(Xt_t_student_asymmetric_arch1_q2_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -3.232504   max:  2.863358 
# median:  0.0714099 
# mean:  -0.004656135 
# sample sd:  0.7045951 
# sample skewness:  -1.051253 
# sample kurtosis:  6.399201
# Dal grafico di Cullen-Frey, la serie potrebbe seguire una distribuzione t-student.

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_asymmetric_arch1_q2_res
show(c(mean(y),var(y)))
# [1] 3.137573e-18 4.973176e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] 2.909565e-18 1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, method= "sample", graph=TRUE, boot=2000)
Xt_t_student_asymmetric_arch1_q2_cf <- append(Xt_t_student_asymmetric_arch1_q2_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -4.584982   max:  4.091947 
# median:  0.1096481 
# mean:  2.909565e-18 
# sample sd:  0.9989995 
# sample skewness:  -1.035504 
# sample kurtosis:  6.388775  

fitdist_test <- list()
# Distribuzione student asimmetrica generalizzata
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "sstd", start=list(nu=3.2, xi=1.5), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' std  ' by maximum likelihood 
# Parameters : 
#     estimate Std. Error
# nu 4.2184862  0.4762162
# xi 0.7451216  0.0438584
# Fixed parameters:
#   value
# mean     0
# sd       1
# Loglikelihood:  -661.0156   AIC:  1326.031   BIC:  1334.46 
# Correlation matrix:
#     nu         xi
# nu  1.0000000 -0.2357207
# xi -0.2357207  1.0000000

# Alla fine, consideriamo il test Goodness of fit.
# The Kolmogorov-Smirnov test in the library *stats*
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="psstd", m=m, sd=sd, nu=df, xi=xi, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.068265, p-value = 0.01893
# alternative hypothesis: two-sided
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student asimmetrica generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# Cramer-von Mises test of goodness-of-fit
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "psstd", mean=m, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 4, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.77988, p-value = 0.008145
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "psstd", mean=m, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 4, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# An = 4.0598, p-value = 0.00816
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_arch1_q2_res
num_lags <- 4                   # Setting the lag parameter for the test.
Xt_t_student_asymmetric_arch1_q2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_arch1_q2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_t_student_asymmetric_arch1_q2_res   
Xt_t_student_asymmetric_arch1_q2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_arch1_q2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
# Si ha un p-value di  0.001449 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità
#
# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_arch1_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_arch1_q2)
show(Xt_t_student_asymmetric_arch1_q2_w)
# Si ha un p-value di 0.00608 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 1.2676, df = 1, p-value = 0.2602
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 4  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_asymmetric_arch1_q2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_asymmetric_arch1_q2_lb)
# Il risultato del test haun p-value < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

modello[['simulazione']][['arch_q2']][['asimmetrico']][['1']] <- append(modello[['simulazione']][['arch_q2']][['asimmetrico']][['1']], 
                                                                        list('lm'=Xt_t_student_asymmetric_arch1_q2_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                             'Cullen-Frey'=Xt_t_student_asymmetric_arch1_q2_cf, 
                                                                             'Breusch-Pagan'=Xt_t_student_asymmetric_arch1_q2_bp, 'White'=Xt_t_student_asymmetric_arch1_q2_w, 
                                                                             'Ljiung-Box'=Xt_t_student_asymmetric_arch1_q2_lb, 'Dickey-Fuller'=Xt_t_student_asymmetric_arch1_q2_adf, 
                                                                             'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_arch1_q2_kpss, 'Shapiro-Wilk'=Xt_t_student_asymmetric_arch1_q2_sw,         
                                                                             'Generalized Distribution'=fitdist_test))

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
skew <- list()
set.seed(123)
s<- DescTools::Skew(Xt_t_student_asymmetric_arch1_q2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew[['0.80']] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -1.1576415 -1.5178810 -0.9058469
set.seed(123)
s<- DescTools::Skew(Xt_t_student_asymmetric_arch1_q2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
skew[['0.99']] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -1.1576415 -1.7498957 -0.6540517  
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_asymmetric_arch1_q2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt   lwr.ci   upr.ci 
# 3.249043 2.265292 4.778715

Xt_t_student_asymmetric_arch1_q2_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, method= "sample", boot=2000)
Xt_t_student_asymmetric_arch1_q2_cf <- append(Xt_t_student_asymmetric_arch1_q2_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -5.642719   max:  3.234806 
# median:  0.1091652 
# mean:  0.0005123739 
# sample sd:  1.131737 
# sample skewness:  -1.168482 
# sample kurtosis:  6.227255  
# Dal grafico di Cullen-Frey, la serie potrebbe seguire una distribuzione t-student.

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- residuals(fit)
show(c(mean(y),var(y)))
# [1] 0.001536828 1.112368888
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] 1.877908e-17 1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, method= "sample", graph=TRUE, boot=2000)
Xt_t_student_asymmetric_arch1_q2_cf <- append(Xt_t_student_asymmetric_arch1_q2_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -5.098489   max:  2.558423 
# median:  0.1341632 
# mean:  1.877908e-17 
# sample sd:  0.9989975 
# sample skewness:  -1.245177 
# sample kurtosis:  6.296855 

fitdist_test <- list()
# Distribuzione student asimmetrica generalizzata
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "sstd", start=list(nu=3.2, xi=1.5), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' std  ' by maximum likelihood 
# Parameters : 
#     estimate Std. Error
# nu 5.4891534 0.94582629
# xi 0.6899426 0.04274545
# Fixed parameters:
#        value
# mean     0
# sd       1
# Loglikelihood:  -663.9724   AIC:  1331.945   BIC:  1340.37 
# Correlation matrix:
#         nu         xi
# nu  1.0000000 -0.2875501
# xi -0.2875501  1.0000000

# Alla fine, consideriamo il test Goodness of fit.
# The Kolmogorov-Smirnov test in the library *stats*
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="psstd", mean=m, sd=sd, nu=df, xi=xi, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.06636, p-value = 0.02468
# alternative hypothesis: two-sided
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student asimmetrica generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.
#

# Cramer-von Mises test of goodness-of-fit
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "psstd", mean=m, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 5, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.76964, p-value = 0.008621
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "psstd", mean=m, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 5, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# An = 4.1824, p-value = 0.007112
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_arch1_q2_res
num_lags <- 4                   # Setting the lag parameter for the test.
Xt_t_student_asymmetric_arch1_q2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_arch1_q2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_t_student_asymmetric_arch1_q2_res   
Xt_t_student_asymmetric_arch1_q2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_arch1_q2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
# Si ha un p-value di  0.005627 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità
#
# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_arch1_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_arch1_q2)
show(Xt_t_student_asymmetric_arch1_q2_w)
# Si ha un p-value di 0.02061 < 0.05, quindi,  possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 0.59029, df = 1, p-value = 0.4423
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 4  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_asymmetric_arch1_q2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_asymmetric_arch1_q2_lb)
# Il risultato del test haun p-value < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

modello[['stimati']][['arch_q2']] <- append(modello[['stimati']][['arch_q2']], 
                                            list('asimmetrico'=list('Xt'=Xt_t_student_asymmetric_arch1_q2_new, 'a0'=a0, 'a1'=aq[1], 'a2'=aq[2], 'q'=q, 
                                                                    'stazionarietà'=stazionaietà, 'lm'=Xt_t_student_asymmetric_arch1_q2_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                    'Cullen-Frey'=Xt_t_student_asymmetric_arch1_q2_cf, 
                                                                    'Breusch-Pagan'=Xt_t_student_asymmetric_arch1_q2_bp, 'White'=Xt_t_student_asymmetric_arch1_q2_w, 
                                                                    'Ljiung-Box'=Xt_t_student_asymmetric_arch1_q2_lb, 'Dickey-Fuller'=Xt_t_student_asymmetric_arch1_q2_adf, 
                                                                    'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_arch1_q2_kpss, 'Shapiro-Wilk'=Xt_t_student_asymmetric_arch1_q2_sw,    
                                                                    'Generalized Distribution'=fitdist_test)))

# In questo modello Arch(2) con una distribuzione t-student asimmetrica si ha presenza di eteroschedasticità
# nel test di Breusch-Pagan ma presenza di omoschedasticità con il test di White; con i nuovi
# parametri stimati si ha assenza di autocorrelazione. 

##########################################
##########################################

# DISTRIBUZIONE NORMALE
# Consideriamo la prima traiettoia con distribuzione normale di un modello GARCH(2,1)
Xt <- Xt_normal_garch1_q2_p1
df_Xt_normal_garch1_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Test Shamiro-Wilk
Xt_normal_garch1_q2_p1_sw <- shapiro.test(Xt)
show(Xt_normal_garch1_q2_p1_sw)
# Si ha un p-value maggiore di 0.05 quindi possiamo dire che la serie è
# normalmente distribuito.

# Line plot
Data_df<- df_Xt_normal_garch1_q2_p1
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
skew <- list()
set.seed(123)
s <- DescTools::Skew(Xt_normal_garch1_q2_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew[['0.80']] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -0.06382440 -0.21414944  0.08838435 
set.seed(123)
s <- DescTools::Skew(Xt_normal_garch1_q2_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
skew[['0.99']] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -0.0638244 -0.3353090  0.2458714 
kurt <- list()
set.seed(123)
k <- DescTools::Kurt(Xt_normal_garch1_q2_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
kurt[['0.80']] <- k
show(k)
#  kurt      lwr.ci   upr.ci 
# -0.04413334 -0.32341129  0.30455569   
set.seed(123)
k <- DescTools::Kurt(Xt_normal_garch1_q2_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.89, ci.type= "bca", R=1000)
kurt[['0.99']] <- k
show(k)
#  kurt      lwr.ci   upr.ci 
# -0.04413334 -0.39062796  0.39411442   

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_normal_garch1_q2_p1_cf <- descdist(Xt, discrete=FALSE, boot=5000)
# Da Cullen-Frey, si ha una distribuzione normale.

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
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_normal_garch1_q2_p1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_normal_garch1_q2_p1_lb)
# I risultati mostrano non sempre un p-value < 0.05, ciò significa che possiamo rigettare
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
Xt_normal_garch1_q2_p1_fit = ugarchfit(spec = uspec, data = dist_normal1)
print(Xt_normal_garch1_q2_p1_fit)
intconf = confint(Xt_normal_garch1_q2_p1_fit)
show(intconf)

fit <- Xt_normal_garch1_q2_p1_fit
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
Data_df<- df_Xt_normal_garch1_q2_p1
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
skew <- list()
set.seed(123)
s <- DescTools::Skew(Xt_normal_garch1_q2_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew[['0.80']] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -0.0349105 -0.2026294  0.1713391 
set.seed(123)
s <- DescTools::Skew(Xt_normal_garch1_q2_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
skew[['0.99']] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -0.0349105 -0.3936895  0.3060584 
kurt <- list()
set.seed(123)
k <- DescTools::Kurt(Xt_normal_garch1_q2_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
kurt[['0.80']] <- k
show(k)
#  kurt      lwr.ci   upr.ci 
# 0.8079337 0.4901407 1.2738097  
set.seed(123)
k <- DescTools::Kurt(Xt_normal_garch1_q2_p1_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.89, ci.type= "bca", R=1000)
kurt[['0.99']] <- k
show(k)
#  kurt      lwr.ci   upr.ci 
# 0.8079337 0.4246159 1.3773373     

# Cullen-Frey
Xt_normal_garch1_q2_p1_cf <- list()
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, method= "sample", boot=2000)
Xt_normal_garch1_q2_p1_cf <- append(Xt_normal_garch1_q2_p1_cf, list(cf))
show(cf)
# summary statistics
# 
# min:  -2.035312   max:  1.922305 
# median:  -0.00830492 
# mean:  0.008739157 
# sample sd:  0.5605122 
# sample skewness:  0.01947295 
# sample kurtosis:  3.772552 
# Da Cullen-Frey, notiamo che la serie non segue una distribuzione normale. Proviamo ad eseguire dei test
# per verifica se sia una distribuzione normale o logistica.

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- residuals(Xt_normal_garch1_q2_p1_fit)
show(c(mean(y),var(y)))
# [1] 0.002322742 1.027094597
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] -1.247852e-17  1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, method= "sample", boot=2000)
Xt_normal_garch1_q2_p1_cf <- append(Xt_normal_garch1_q2_p1_cf, list(cf))
show(cf)
# summary statistics
# ------
# min:  -2.035312   max:  1.922305 
# median:  -0.00830492 
# mean:  0.008739157 
# sample sd:  0.5605122 
# sample skewness:  0.01947295 
# sample kurtosis:  3.772552 
# Da Cullen-Frey, abbiamo una possibile distribuzione logistica o di student per i residui.

# Esploriamo queste possibilità in più dettagli.
z_st_qemp <- EnvStats::qemp(stats::ppoints(z_st), z_st) # Empirical quantiles of the residuals.
z_st_demp <- EnvStats::demp(z_st_qemp, z_st)     # Empirical probability density of the residuals.
z_st_pemp <- EnvStats::pemp(z_st_qemp, z_st)     # Empirical distribution function of the residuals.  
x <- z_st_qemp
y_d <- z_st_demp
y_p <- z_st_pemp
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata con il parametri location=0,
# scale=sqrt(3)/pi e shape=1.
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the GARCH(2,1) model with a normal distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dgnorm(x, mu=0, alpha=sqrt(2), beta=2), lwd=2, col= "red")
lines(x, dglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", Gen_Log_Dens_Func), 
       col=c("darkblue", "red","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(2,1) model with a normal distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pgnorm(x, mu=0, alpha=sqrt(2), beta=2), lwd=2, col= "red")
lines(x, pglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", Gen_Log_Distr_Func), 
       col=c("darkblue", "red", "magenta"), 
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
# location -0.1393353 0.21462947
# scale     0.6106307 0.04864602
# shape     1.1679590 0.26096328
# Loglikelihood:  -716.5919   AIC:  1439.184   BIC:  1451.822 
# Correlation matrix:
#   location      scale      shape
# location  1.0000000 -0.8736368 -0.9770636
# scale    -0.8736368  1.0000000  0.8887035
# shape    -0.9770636  0.8887035  1.0000000
#
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_glogis_bd <- fitdistrplus::bootdist(fitdist_glogis, niter=1000)
fitdist_test[["glogis"]][["bootdist"]] <- fitdist_glogis_bd
summary(fitdist_glogis_bd)
# Parametric bootstrap medians and 95% percentile CI 
#             Median     2.5%     97.5%
# location -0.1464190 -0.5745180 0.1630198
# scale     0.6112263  0.5328851 0.6966038
# shape     1.1798520  0.8441464 1.8078186
# We fix the initial points of the constrained maximization procedure
location <- fitdist_glogis_bd[["fitpart"]][["estimate"]][1]
scale <- fitdist_glogis_bd[["fitpart"]][["estimate"]][2]
shape <- fitdist_glogis_bd[["fitpart"]][["estimate"]][3]
show(c(location,scale,shape)) # the estimated parameters.
# -0.1393353  0.6106307  1.1679590 
minus_logLik <- function(x) -sum(log(dglogis(z_st, location=x[1], scale=x[2], shape=x[3]))) # the log-likelihood of the generalized logistic
# distribution.
fminunc_result <- fminunc(x0=c(location, scale, shape), fn=minus_logLik)   # the minimization procedure where (location,scale,shape) is the 
# starting point.
show(c(fminunc_result$par[1],fminunc_result$par[2],fminunc_result$par[3])) # the estimated parameters.
#    location       scale       shape 
# -0.1391401  0.6106133  1.1676185 

# Distribuzione normale generalizzata
fitdist_gnorm <- fitdistrplus::fitdist(z_st, "gnorm", start=list(mu=0, alpha=1, beta=1), method= "mle")
fitdist_test[["gnorm"]][["gnorm"]] <- fitdist_gnorm
summary(fitdist_gnorm)
# Fitting of the distribution ' t_ls ' by maximum likelihood 
# Parameters : 
#         estimate Std. Error
# mu    -0.001154817 0.04395314
# alpha  1.538915761 0.07619485
# beta   2.437874280 0.25280176
# Loglikelihood:  -705.7195   AIC:  1417.439   BIC:  1430.077 
# Correlation matrix:
#   mu       alpha        beta
# mu     1.00000000 -0.01894327 -0.02318005
# alpha -0.01894327  1.00000000  0.81530084
# beta  -0.02318005  0.81530084  1.00000000
#
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_gnorm_bd <- fitdistrplus::bootdist(fitdist_gnorm, niter=1000)
fitdist_test[["gnorm"]][["bootdist"]] <- fitdist_gnorm_bd
summary(fitdist_gnorm_bd)
# Parametric bootstrap medians and 95% percentile CI 
#             Median     2.5%     97.5%
# mu    -0.002189349 -0.08497988 0.0799868
# alpha  1.542353270  1.36290668 1.7007911
# beta   2.459879806  1.99858442 3.1089104
# We fix the initial points of the constrained maximization procedure
mu <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["mu"]][1]
alpha <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["alpha"]][1]
beta <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["beta"]][1]
show(c(mu,alpha,beta)) # the estimated parameters.
# 0.04395314 0.07619485 0.25280176 

# Setting
location <- fitdist_glogis[["estimate"]][["location"]]
scale <- fitdist_glogis[["estimate"]][["scale"]]
shape <- fitdist_glogis[["estimate"]][["shape"]]
#
mu <- fitdist_gnorm[["estimate"]][["mu"]]
alpha <- fitdist_gnorm[["estimate"]][["alpha"]]
beta <- fitdist_gnorm[["estimate"]][["beta"]]
# We plot the histogram and the empirical density function of the standardized residuals together with the density function of the estimated 
# generalized logistic and generalized normal.
Est_Gen_Log_Dens_Func <- bquote(paste("Estimated Generalized Logistic Density Function, location = ", .(loc), ", scale = ", .(scl) , ", shape = ", .(shp)))
Est_Gen_Norm_Dens_Func <- bquote(paste("Estimated Generalized Normal Density Function, mu = ", .(mu), ", alpha = ", .(alpha) , ", beta = ", .(beta)))
hist(z_st, breaks= "Scott", col= "green", border= "black", xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), 
     freq=FALSE, main= "Density Histogram of the Standardized Residuals of the GARCH(2,1) model + Empirical Density Function + Estimated Generalized Logistic Density + Estimated Generalized Normal Density", 
     xlab= "Standardized Residuals", ylab= "Density")
lines(density(z_st), lwd=2, col= "darkgreen")
lines(x, dgnorm(x, mu=mu, alpha=alpha, beta=beta), lwd=2, col= "red")
lines(x, dglogis(x, location=location, scale=scale, shape=shape), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", Est_Gen_Norm_Dens_Func, Est_Gen_Log_Dens_Func), 
       col=c("darkgreen", "red", "magenta"),
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

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
# D = 0.040939, p-value = 0.373
# alternative hypothesis: two-sided
# Con un p-value cosi alto, non possiamo rigettare l'ipotesi nulla. Di conseguenza, abbiamo una prova sufficiente
# che la serie è una distribuzione logistica generalizzata.
mu <- fitdist_gnorm[["estimate"]][["mu"]]
alpha <- fitdist_gnorm[["estimate"]][["alpha"]]
beta <- fitdist_gnorm[["estimate"]][["beta"]]
KS_z_st_t_ls <- stats::ks.test(z_st, y="pgnorm", mu=mu, alpha=alpha, beta=beta, alternative= "two.sided")
fitdist_test[["gnorm"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.026188, p-value = 0.8835
# alternative hypothesis: two-sided
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione normale generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.

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
# with parameters location = -0.1393, scale = 0.6106, shape = 1.168
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.18548, p-value = 0.2975
# Non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
mu <- fitdist_gnorm[["estimate"]][["mu"]]
alpha <- fitdist_gnorm[["estimate"]][["alpha"]]
beta <- fitdist_gnorm[["estimate"]][["beta"]]
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "pgnorm", mu=mu, alpha=alpha, beta=beta, estimated=FALSE)
fitdist_test[["gnorm"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘pgnorm’
# with parameters mu = -0.00115481709970017, alpha = 1.53891576061928, beta = 2.4378742798516
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.039417, p-value = 0.9363
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione normale generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.
loc <- round(fitdist_glogis[["estimate"]][["location"]],4)
scl <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shp <- round(fitdist_glogis[["estimate"]][["shape"]],4)
AD_z_st_glogis <- goftest::ad.test(z_st, null= "pglogis", location=location, scale=scale, shape=shape, estimated=FALSE)
fitdist_test[["glogis"]][["Anderson-Darling"]] <- AD_z_st_glogis
show(AD_z_st_glogis)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘pglogis’
# with parameters location = -0.139335312142869, scale = 0.610630671526202, shape = 1.16795902161195
# Parameters assumed to be fixed
# data:  z_st
# An = 1.3142, p-value = 0.2278
# Non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
loc <- round(fitdist_glogis[["estimate"]][["location"]],4)
scl <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shp <- round(fitdist_glogis[["estimate"]][["shape"]],4)
AD_z_st_t_ls <- goftest::ad.test(z_st, "pgnorm", mu=mu, alpha=alpha, beta=beta, estimated=FALSE)
fitdist_test[["gnorm"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘pgnorm’
# with parameters mu = -0.00115481709970017, alpha = 1.53891576061928, beta = 2.4378742798516
# Parameters assumed to be fixed
# data:  z_st
# An = 0.31785, p-value = 0.9241
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione normale generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata

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
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_normal_garch1_q2_p1_bt <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_normal_garch1_q2_p1_bt)
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Il risultati indica una assenza di autocorrelazione nei residui del modello con i test Box-Ljung.

modello[['stimati']][['garch_q2_p1']] <- append(modello[['stimati']][['garch_q2_p1']], 
                                                list('normale'=list('Xt'=Xt_normal_garch1_q2_p1_new, 'a0'=a0, 'a1'=aq[1], 'a2'=aq[2], 'b1'=b1, 'q'=q, 'p'=p,
                                                                   'stazionarietà'=stazionaietà, 'lm'=Xt_normal_garch1_q2_p1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                   'Cullen-Frey'=Xt_normal_garch1_q2_p1_cf, 
                                                                   'Breusch-Pagan'=Xt_normal_garch1_q2_p1_bp, 'White'=Xt_normal_garch1_q2_p1_w, 
                                                                   'Ljiung-Box'=Xt_normal_garch1_q2_p1_lb, 'Dickey-Fuller'=Xt_normal_garch1_q2_p1_adf, 
                                                                   'Kwiatowski-Phillips-Schmidt-Shin'=Xt_normal_garch1_q2_p1_kpss, 'Shapiro-Wilk'=Xt_normal_garch1_q2_p1_sw,
                                                                   'Generalized Distribution'=fitdist_test)))

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
# 0.05922268 -0.52223043  0.62583718
set.seed(123)
kurt <- kurt <- DescTools::Kurt(Xt_t_student_symmetric_garch2_q2_p1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#      kurt      lwr.ci      upr.ci 
#    4.147305   2.536975   6.261847

Xt_t_student_symmetric_garch2_q2_p1_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_t_student_symmetric_garch2_q2_p1_cf <- append(Xt_t_student_symmetric_garch2_q2_p1_cf, list(cf))
show(cf)
# summary statistics
# ------
# min:  -4.685117   max:  4.594745 
# median:  -0.04164059 
# mean:  -0.02805814 
# estimated sd:  0.8584542 
# estimated skewness:  0.05922268 
# estimated kurtosis:  7.147305  
# Conferma visiva che l'ipotesi nulla di normalità bisogna scartarla all'1% di significatività 
# quindi possiamo dire che è una distribuzione simmetrica con una forte kurtosi.

# Verifichiamo la distribuzione della serie attraverso Goodness of fit
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_symmetric_garch2_q2_p1_res
show(c(mean(y),var(y)))
# [1] 7.070028e-18 7.363905e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] 6.652339e-18 1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, graph=TRUE, boot=5000)
Xt_t_student_symmetric_garch2_q2_p1_cf <- append(Xt_t_student_symmetric_garch2_q2_p1_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -5.456657   
# max:  5.357559 
# median:  -0.01879541 
# mean:  6.652339e-18 
# estimated sd:  1 
# estimated skewness:  0.04280734 
# estimated kurtosis:  7.140661  
# Da Cullen-Frey, abbiamo una possibile distribuzione logistica o di student per i residui.

# Esploriamo queste possibilità in più dettagli.
z_st_qemp <- EnvStats::qemp(stats::ppoints(z_st), z_st) # Empirical quantiles of the residuals.
z_st_demp <- EnvStats::demp(z_st_qemp, z_st)     # Empirical probability density of the residuals.
z_st_pemp <- EnvStats::pemp(z_st_qemp, z_st)     # Empirical distribution function of the residuals.  
x <- z_st_qemp
y_d <- z_st_demp
y_p <- z_st_pemp
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata con il parametri location=0,
# scale=sqrt(3)/pi e shape=1.
loc <- 0
shp <- 1
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the GARCH(2,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dstd(x, mean=0, sd=1, nu=5), lwd=2, col= "red")
lines(x, dglogis(x, location=loc, scale=sqrt(3)/pi, shape=shp), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", "Student Density Function, df=5", Gen_Log_Dens_Func), 
       col=c("darkblue", "darkgreen", "red","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(2,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pstd(x, mean = 0, sd = 1, nu = 5), lwd=2, col= "red")
lines(x, pglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function","Student Distribution Function, df=5", Gen_Log_Distr_Func), 
       col=c("darkblue", "darkgreen", "red","magenta"), 
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
# location 0.0123738  0.1129075
# scale    0.5142024  0.0321967
# shape    0.9750459  0.1373638
# Loglikelihood:  -680.7066   AIC:  1367.413   BIC:  1380.057 
# Correlation matrix:
#       location      scale      shape
# location  1.0000000 -0.7425956 -0.9362592
# scale    -0.7425956  1.0000000  0.7960526
# shape    -0.9362592  0.7960526  1.0000000
#
# Setting
location <- fitdist_glogis[["estimate"]][["location"]]
scale <- fitdist_glogis[["estimate"]][["scale"]]
shape <- fitdist_glogis[["estimate"]][["shape"]]
# We obtain
mean <- location+(digamma(shape)-digamma(1))*scale
fitdist_test[["glogis"]][["mean"]] <- mean
show(mean)
# -0.00912685
sd <- sqrt((psigamma(shape, deriv=1)+psigamma(1, deriv=1))*scale^2)
fitdist_test[["glogis"]][["sd"]] <- sd
show(sd)
# 0.9414187
skewness <- (psigamma(shape, deriv=2)-psigamma(1, deriv=2))/((psigamma(shape, deriv=1)+psigamma(1, deriv=1))^(3/2))
fitdist_test[["glogis"]][["skew"]] <- skewness
show(skewness)
# -0.02772218
#
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_glogis_bd <- fitdistrplus::bootdist(fitdist_glogis, niter=1000)
fitdist_test[["glogis"]][["bootdist"]] <- fitdist_glogis_bd
summary(fitdist_glogis_bd)
# Parametric bootstrap medians and 95% percentile CI 
#              Median       2.5%     97.5%
# location 0.005612547 -0.2974726 0.2439670
# scale    0.512933373  0.4455774 0.5896815
# shape    0.980986547  0.7129541 1.4365004
#
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
#    0.01254582 0.51419525 0.97499737  
logLik <- -fminunc_result$value # the minimized negative log-likelihood
n <- length(z_st)
k <- length(fminunc_result[["par"]])
AIC <- 2*k-2*logLik
BIC <- k*log(n)-2*logLik
AICc <- AIC + 2*k*((k+1)/(n-k-1))
show(c(logLik, AIC, BIC, AICc))
# logLik       AIC       BIC      AICc
# -680.7066 1367.4131 1380.0569 1367.4615

# Dato che la Generalized Student distribution ha il parametro location che coincide con la media, possiamo provare 
# a fittare i dati con una generalized Student distribution con zero location. 
# Questo viene fatto cambiando il parametro m nella funzione *fitdistrplus::fitdist*.
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "std", start=list(nu=3.2), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' std  ' by maximum likelihood 
# Parameters : 
#   estimate Std. Error
# nu 4.014713  0.4561426
# Fixed parameters:
#   value
# mean     0
# sd       1
# Loglikelihood:  -675.0405   AIC:  1352.081   BIC:  1356.296 
logLik <- fitdist_t_ls$loglik # the minimized negative log-likelihood
n <- length(z_st)
k <- length(fitdist_t_ls[["estimate"]])
AIC <- 2*k-2*logLik
BIC <- k*log(n)-2*logLik
AICc <- AIC + 2*k*((k+1)/(n-k-1))
show(c(logLik, AIC, BIC, AICc))
# logLik       AIC       BIC       AICc
# -675.0405 1352.0810 1356.2956 1352.0890
#
# Confrontando la distribuzione Student generalizzata con la distribuzione logistica generalizzata
# Fitting of the distribution ' glogis ' by maximum likelihood 
# Parameters : 
#           estimate Std. Error
# location 0.01752954 0.09815676
# scale    0.48487522 0.02907360
# shape    0.96132629 0.12554329
# Loglikelihood:  -659.8476   AIC:  1325.695   BIC:  1338.339 
# Notiamo che la distribuzione logistica è un modello migliore per i residui standardizzati del modello Garch(2,1)
# rispetto alla distribuzione student generalizzata.
#
# Also in this case we evaluate the uncertainty in estimated parameters of the fitted distribution by means of the fitdistrplus::bootdist()
# function.
set.seed(12345)
fitdist_t_ls_bd <- bootdist(fitdist_t_ls, niter=1000)
fitdist_test[["gstudent"]][["bootdist"]] <- fitdist_t_ls_bd
summary(fitdist_t_ls_bd)
#Parametric bootstrap medians and 95% percentile CI 
#      Median      2.5%     97.5%
# nu  4.051102 3.347787 5.465607
#

# We plot the histogram and the empirical density function of the standardized residuals together with the density function of the estimated 
# generalized student and generalized logistic.
# Setting
loc <- round(fitdist_glogis[["estimate"]][["location"]],4)
scl <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shp <- round(fitdist_glogis[["estimate"]][["shape"]],4)
#
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
Est_Gen_Stud_Dens_Func <- bquote(paste("Estimated Student Density Function, location = ", .(m), ", scale = ", .(s) , ", degrees of freedom = ", .(df)))
Est_Gen_Logis_Dens_Func <- bquote(paste("Estimated Generalized Logistic Density Function, location = ", .(loc), ", scale = ", .(scl) , ", degrees of freedom = ", .(shp)))
hist(z_st, breaks= "Scott", col= "green", border= "black", xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+1.1), 
     freq=FALSE, main= "Density Histogram of the Standardized Residuals of the GARCH(2,1) model + Empirical 
     Density Function + Estimated Generalized Student Density", xlab= "Standardized Residuals", ylab= "Density")
lines(density(z_st), lwd=2, col= "darkgreen")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "red")
lines(x, dstd (x, mean=0, sd=sd, nu=df), lwd=2, col= "blue")
lines(x, dglogis(x, location=location, scale=scale, shape=shape), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density", "Standard Gaussian Density", Est_Gen_Stud_Dens_Func, Est_Gen_Logis_Dens_Func), 
       col=c("darkgreen","red", "blue", "magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# We also plot the empirical distribution function of the standardized residuals together with the distribution function of the standard 
# Gaussian, generalized Student and generalized logistic.
Est_Gen_Stud_Distr_Func <- bquote(paste("Estimated Student Distribution Function, location = ", .(m), ", scale = ", .(s) , ", degrees of freedom = ", .(df)))
Est_Gen_Logis_Distr_Func <- bquote(paste("Estimated Generalized Logistic Distribution Function, location = ", .(loc), ", scale = ", .(scl) , ", degrees of freedom = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(2,1) model", 
     xlab= "Standardized Residuals", ylab= "Probability Distribution")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "red")
lines(x, pstd(x, mean=0, sd=sd, nu=df), lwd=2, col= "blue")
lines(x, pglogis(x, location=location, scale=scale, shape=shape), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", Est_Gen_Stud_Distr_Func, Est_Gen_Logis_Distr_Func ),
       col=c("darkgreen", "red", "blue", "magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

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
# D = 0.03435, p-value = 0.5968
# alternative hypothesis: two-sided
# Non possiamo rigettare l'ipotesi nulla. Di conseguenza, abbiamo una prova sufficiente
# che la serie è una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="pstd", mean=m, sd=sd, nu=df, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.024188, p-value = 0.9317
# alternative hypothesis: two-sided
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata.

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
# with parameters location = 0.0124, scale = 0.5142, shape = 0.975
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.098893, p-value = 0.5905
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "pstd", mean=m, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 4
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.022887, p-value = 0.9935
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata.

# The Anderson-Darling test in the library *goftest*.
loc <- round(fitdist_glogis[["estimate"]][["location"]],4)
scl <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shp <- round(fitdist_glogis[["estimate"]][["shape"]],4)
AD_z_st_glogis <- goftest::ad.test(z_st, null= "pglogis", location=location, scale=scale, shape=shape, estimated=FALSE)
fitdist_test[["glogis"]][["Anderson-Darling"]] <- AD_z_st_glogis
show(AD_z_st_glogis)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘pglogis’
# with parameters location = 0.0123738015306441, scale = 0.51420243656747, shape = 0.975045908876096
# Parameters assumed to be fixed
# data:  z_st
# An = 0.64163, p-value = 0.6095
# Non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "pstd", mean=m, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 4
# Parameters assumed to be fixed
# data:  z_st
# An = 0.16003, p-value = 0.9977
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata.

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_symmetric_garch2_q2_p1_res
num_lags <- 5                   # Setting the lag parameter for the test.
Xt_t_student_symmetric_garch2_q2_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_symmetric_garch2_q2_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_t_student_symmetric_garch2_q2_p1_res   
Xt_t_student_symmetric_garch2_q2_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_symmetric_garch2_q2_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
# Si ha un p-value di 0.07009 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità
#
# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch2_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch2_q2_p1)
show(Xt_t_student_symmetric_garch2_q2_p1_w)
# Si ha un p-value di 0.107 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 0.93002, df = 1, p-value = 0.3349
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 4  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_symmetric_garch2_q2_p1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_symmetric_garch2_q2_p1_lb)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione
# Il risultati indica una presenza di autocorrelazione nei residui del modello.

modello[['simulazione']][['garch_q2_p1']][['simmetrico']][['2']] <- append(modello[['simulazione']][['garch_q2_p1']][['simmetrico']][['2']], 
                                                                           list('lm'=Xt_t_student_symmetric_garch2_q2_p1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                                'Cullen-Frey'=Xt_t_student_symmetric_garch2_q2_p1_cf, 
                                                                                'Breusch-Pagan'=Xt_t_student_symmetric_garch2_q2_p1_bp, 'White'=Xt_t_student_symmetric_garch2_q2_p1_w, 
                                                                                'Ljiung-Box'=Xt_t_student_symmetric_garch2_q2_p1_lb, 'Dickey-Fuller'=Xt_t_student_symmetric_garch2_q2_p1_adf, 
                                                                                'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_garch2_q2_p1_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_garch2_q2_p1_sw,     
                                                                                'Generalized Distribution'=fitdist_test))

# In questo modello Garch(2,1) con distribuzione simmetrica si ha presenza di 
# omoschedasticità e assenza di autocorrelazione.
# Proviamo a stimare i miglior parametri per il modello:
q <- 2
p <- 1
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)), distribution.model= "std")
Xt_t_student_symmetric_garch2_q2_p1_fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric2)
print(Xt_t_student_symmetric_garch2_q2_p1_fit)
intconf = confint(Xt_t_student_symmetric_garch2_q2_p1_fit)
show(intconf)

distribution <- dist_t_student_symmetric2
fit <- Xt_t_student_symmetric_garch2_q2_p1_fit
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
# -0.255279459 -0.587126491 -0.004490806  
set.seed(123)
kurt <- kurt <- DescTools::Kurt(Xt_t_student_symmetric_garch2_q2_p1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#      kurt      lwr.ci      upr.ci 
#    2.013369  1.344181   3.064795 

Xt_t_student_symmetric_garch2_q2_p1_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_t_student_symmetric_garch2_q2_p1_cf <- append(Xt_t_student_symmetric_garch2_q2_p1_cf, list(cf))
show(cf)
# summary statistics
# ------
# min:  -1.837079   max:  1.403279 
# median:  -0.01707776 
# mean:  -0.02261815 
# estimated sd:  0.4062156 
# estimated skewness:  -0.3024933 
# estimated kurtosis:  5.023175 
# Conferma visiva che l'ipotesi nulla di normalità bisogna scartarla all'1% di significatività 
# quindi possiamo dire che è una distribuzione simmetrica con una forte kurtosi.

# Verifichiamo la distribuzione della serie attraverso Goodness of fit
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- residuals(Xt_t_student_symmetric_garch2_q2_p1_fit)
show(c(mean(y),var(y)))
# [1] -0.0001554686  0.9538404745
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] -4.481079e-18  1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, graph=TRUE, boot=5000)
Xt_t_student_symmetric_garch2_q2_p1_cf <- append(Xt_t_student_symmetric_garch2_q2_p1_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -4.064726   max:  3.901932 
# median:  -0.01109141 
# mean:  -4.481079e-18 
# estimated sd:  1 
# estimated skewness:  -0.09056084 
# estimated kurtosis:  4.631105 
# Da Cullen-Frey, abbiamo una possibile distribuzione logistica o di student per i residui.

# Esploriamo queste possibilità in più dettagli.
z_st_qemp <- EnvStats::qemp(stats::ppoints(z_st), z_st) # Empirical quantiles of the residuals.
z_st_demp <- EnvStats::demp(z_st_qemp, z_st)     # Empirical probability density of the residuals.
z_st_pemp <- EnvStats::pemp(z_st_qemp, z_st)     # Empirical distribution function of the residuals.  
x <- z_st_qemp
y_d <- z_st_demp
y_p <- z_st_pemp
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata con il parametri location=0,
# scale=sqrt(3)/pi e shape=1.
loc <- 0
shp <- 1
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the GARCH(2,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dstd(x, mean=0, sd=1, nu=5), lwd=2, col= "red")
lines(x, dglogis(x, location=loc, scale=sqrt(3)/pi, shape=shp), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", "Student Density Function, df=5", Gen_Log_Dens_Func), 
       col=c("darkblue", "darkgreen",  "red","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(2,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pstd(x, mean = 0, sd = 1, nu = 5), lwd=2, col= "red")
lines(x, pglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=5", Gen_Log_Distr_Func), 
       col=c("darkblue", "darkgreen", "red","magenta"), 
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
# location 0.05767796 0.12045186
# scale    0.52390486 0.03432382
# shape    0.93099643 0.13631579
# Loglikelihood:  -693.1906   AIC:  1392.381   BIC:  1405.019 
# Correlation matrix:
#            location      scale      shape
# location  1.0000000 -0.7611068 -0.9393534
# scale    -0.7611068  1.0000000  0.8157231
# shape    -0.9393534  0.8157231  1.0000000
#
# Setting
location <- fitdist_glogis[["estimate"]][["location"]]
scale <- fitdist_glogis[["estimate"]][["scale"]]
shape <- fitdist_glogis[["estimate"]][["shape"]]
# We obtain
mean <- location+(digamma(shape)-digamma(1))*scale
fitdist_test[["glogis"]][["mean"]] <- mean
show(mean)
# -0.004986678
sd <- sqrt((psigamma(shape, deriv=1)+psigamma(1, deriv=1))*scale^2)
fitdist_test[["glogis"]][["sd"]] <- sd
show(sd)
# 0.9763075
skewness <- (psigamma(shape, deriv=2)-psigamma(1, deriv=2))/((psigamma(shape, deriv=1)+psigamma(1, deriv=1))^(3/2))
fitdist_test[["glogis"]][["skew"]] <- skewness
show(skewness)
# -0.07954867
#
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_glogis_bd <- fitdistrplus::bootdist(fitdist_glogis, niter=1000)
fitdist_test[["glogis"]][["bootdist"]] <- fitdist_glogis_bd
summary(fitdist_glogis_bd)
# Parametric bootstrap medians and 95% percentile CI 
#              Median       2.5%     97.5%
# location 0.05150566 -0.2721809 0.3039783
# scale    0.52421945  0.4529294 0.5994984
# shape    0.94008533  0.6885779 1.3748832
#
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
#   0.05752617 0.52403767 0.93121722  
logLik <- -fminunc_result$value # the minimized negative log-likelihood
n <- length(z_st)
k <- length(fminunc_result[["par"]])
AIC <- 2*k-2*logLik
BIC <- k*log(n)-2*logLik
AICc <- AIC + 2*k*((k+1)/(n-k-1))
show(c(logLik, AIC, BIC, AICc))
# logLik       AIC       BIC      AICc
# -693.1906 1392.3811 1405.0189 1392.4296

# Dato che la Generalized Student distribution ha il parametro location che coincide con la media, possiamo provare 
# a fittare i dati con una generalized Student distribution con zero location. 
# Questo viene fatto cambiando il parametro m nella funzione *fitdistrplus::fitdist*.
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "std", start=list(nu=3.2), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' std  ' by maximum likelihood 
# Parameters : 
#   estimate Std. Error
# nu 5.080309  0.8908301
# Fixed parameters:
#         value
# mean      0
# sd        1
# Loglikelihood:  -692.5881   AIC:  1387.176   BIC:  1391.389 
#
# Confrontando la distribuzione Student generalizzata con la distribuzione logistica generalizzata
# Fitting of the distribution ' glogis ' by maximum likelihood 
# Parameters : 
#           estimate Std. Error
# location 0.05767796 0.12045186
# scale    0.52390486 0.03432382
# shape    0.93099643 0.13631579
# Loglikelihood:  -693.1906   AIC:  1392.381   BIC:  1405.019 
# Notiamo che la distribuzione logistica è un modello migliore per i residui standardizzati del modello Garch(2,1)
# rispetto alla distribuzione student generalizzata.
#
# Also in this case we evaluate the uncertainty in estimated parameters of the fitted distribution by means of the fitdistrplus::bootdist()
# function.
set.seed(12345)
fitdist_t_ls_bd <- bootdist(fitdist_t_ls, niter=1000)
fitdist_test[["gstudent"]][["bootdist"]] <- fitdist_t_ls_bd
summary(fitdist_t_ls_bd)
#Parametric bootstrap medians and 95% percentile CI 
#      Median      2.5%     97.5%
# nu  5.147667 3.958483 8.224833 

# We plot the histogram and the empirical density function of the standardized residuals together with the density function of the estimated 
# generalized student and generalized logistic.
# Setting
loc <- round(fitdist_glogis[["estimate"]][["location"]],4)
scl <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shp <- round(fitdist_glogis[["estimate"]][["shape"]],4)
#
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
Est_Gen_Stud_Dens_Func <- bquote(paste("Estimated Student Density Function, mean = ", .(m), ", sd = ", .(sd) , ", degrees of freedom = ", .(df)))
Est_Gen_Logis_Dens_Func <- bquote(paste("Estimated Generalized Logistic Density Function, location = ", .(loc), ", scale = ", .(scl) , ", degrees of freedom = ", .(shp)))
hist(z_st, breaks= "Scott", col= "green", border= "black", xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+1.1), 
     freq=FALSE, main= "Density Histogram of the Standardized Residuals of the GARCH(2,1) model + Empirical 
     Density Function + Estimated Generalized Student Density", xlab= "Standardized Residuals", ylab= "Density")
lines(density(z_st), lwd=2, col= "darkgreen")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "red")
lines(x, dstd (x, mean=m, sd=sd, nu=df), lwd=2, col= "blue")
lines(x, dglogis(x, location=location, scale=scale, shape=shape), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density", "Standard Gaussian Density", Est_Gen_Stud_Dens_Func, Est_Gen_Logis_Dens_Func), 
       col=c("darkgreen","red", "blue", "magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# We also plot the empirical distribution function of the standardized residuals together with the distribution function of the standard 
# Gaussian, generalized Student and generalized logistic.
Est_Gen_Stud_Distr_Func <- bquote(paste("Estimated Student Distribution Function, mean = ", .(m), ", sd = ", .(sd) , ", degrees of freedom = ", .(df)))
Est_Gen_Logis_Distr_Func <- bquote(paste("Estimated Generalized Logistic Distribution Function, location = ", .(loc), ", scale = ", .(scl) , ", degrees of freedom = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(2,1) model", 
     xlab= "Standardized Residuals", ylab= "Probability Distribution")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "red")
lines(x, pstd(x, mean=m, sd=sd, nu=df), lwd=2, col= "blue")
lines(x, pglogis(x, location=location, scale=scale, shape=shape), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", Est_Gen_Stud_Distr_Func, Est_Gen_Logis_Distr_Func ),
       col=c("darkgreen", "red", "blue", "magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

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
# D = 0.029084, p-value = 0.7925
# alternative hypothesis: two-sided
# Non possiamo rigettare l'ipotesi nulla. Di conseguenza, abbiamo una prova sufficiente
# che la serie è una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="pstd", mean=m, sd=sd, nu=df, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.023555, p-value = 0.9447
# alternative hypothesis: two-sided
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata.

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
# with parameters location = 0.0577, scale = 0.5239, shape = 0.931
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.061978, p-value = 0.8018
# Poiché il p-value è molto alto, non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "psstd", mean=m, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters  mean = 0, sd = 1, nu = 5
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 1.1422, p-value = 0.001137
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata.

# The Anderson-Darling test in the library *goftest*.
loc <- round(fitdist_glogis[["estimate"]][["location"]],4)
scl <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shp <- round(fitdist_glogis[["estimate"]][["shape"]],4)
AD_z_st_glogis <- goftest::ad.test(z_st, null= "pglogis", location=location, scale=scale, shape=shape, estimated=FALSE)
fitdist_test[["glogis"]][["Anderson-Darling"]] <- AD_z_st_glogis
show(AD_z_st_glogis)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘pglogis’
# with parameters location = 0.0576779638506883, scale = 0.523904859172222, shape = 0.930996432523
# Parameters assumed to be fixed
# data:  z_st
# An = 0.45271, p-value = 0.7952
# Non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "pstd", mean=m, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters  mean = 0, sd = 1, nu = 5
# Parameters assumed to be fixed
# data:  z_st
# An = 0.25784, p-value = 0.9661
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata.

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_symmetric_garch2_q2_p1_res
num_lags <- 5                   # Setting the lag parameter for the test.
Xt_t_student_symmetric_garch2_q2_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_symmetric_garch2_q2_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_t_student_symmetric_garch2_q2_p1_res   
Xt_t_student_symmetric_garch2_q2_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_symmetric_garch2_q2_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
# Si ha un p-value di 8.936e-07 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità
#
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
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_symmetric_garch2_q2_p1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_symmetric_garch2_q2_p1_lb)

modello[['stimati']][['garch_q2_p1']] <- append(modello[['stimati']][['garch_q2_p1']], 
                                                list('simmetrico'=list('Xt'=Xt_t_student_symmetric_garch2_q2_p1_new, 'a0'=a0, 'a1'=aq[1], 'a2'=aq[2], 'b1'=b1, 'q'=q, 'p'=p, 
                                                                        'stazionarietà'=stazionaietà, 'lm'=Xt_t_student_symmetric_garch2_q2_p1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                       'Cullen-Frey'=Xt_t_student_symmetric_garch2_q2_p1_cf, 
                                                                       'Breusch-Pagan'=Xt_t_student_symmetric_garch2_q2_p1_bp, 'White'=Xt_t_student_symmetric_garch2_q2_p1_w, 
                                                                       'Ljiung-Box'=Xt_t_student_symmetric_garch2_q2_p1_lb, 'Dickey-Fuller'=Xt_t_student_symmetric_garch2_q2_p1_adf, 
                                                                       'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_garch2_q2_p1_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_garch2_q2_p1_sw,                                                 
                                                                       'Generalized Distribution'=fitdist_test)))

# Con la stima dei nuovi parametri, il modello Garch(2,1) con distribuzione t-student
# simmetrica ha assenza di autocorrelazione.

##########################################

#### DISTRIBUZIONE T-STUDENT ASIMMETRICA
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
# -1.3020583 -1.7272608 -0.8887039
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_asymmetric_garch1_q2_p1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt   lwr.ci   upr.ci 
# 4.979284 3.659133 7.390155 

Xt_t_student_asymmetric_garch1_q2_p1_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_t_student_asymmetric_garch1_q2_p1_cf <- append(Xt_t_student_asymmetric_garch1_q2_p1_cf, list(cf)) 
show(cf)
# summary statistics
# -------5.515358   max:  3.96331 
# median:  0.09973168 
# mean:  -0.01288055 
# estimated sd:  0.9682439 
# estimated skewness:  -1.31112 
# estimated kurtosis:  7.979817  

# Goodness of fit
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_asymmetric_garch1_q2_p1_res
show(c(mean(y),var(y)))
# [1] -1.239460e-18  9.374041e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] -1.17354e-18  1.00000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, graph=TRUE, boot=5000)
Xt_t_student_asymmetric_garch1_q2_p1_cf <- append(Xt_t_student_asymmetric_garch1_q2_p1_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -5.687993   max:  4.122067 
# median:  0.1072694 
# mean:  -1.17354e-18 
# estimated sd:  1 
# estimated skewness:  -1.302058 
# estimated kurtosis:  7.979284 

fitdist_test <- list()
# Distribuzione student asimmetrica generalizzata
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "sstd", start=list(nu=3.2, xi=1.5), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' std  ' by maximum likelihood 
# Parameters : 
#     estimate Std. Error
# nu 3.7864147 0.34804860
# xi 0.7530256 0.04258794
# Fixed parameters:
#   value
# mean     0
# sd       1
# Loglikelihood:  -647.6883   AIC:  1299.377   BIC:  1307.806 
# Correlation matrix:
#      nu         xi
# nu  1.0000000 -0.2567307
# xi -0.2567307  1.0000000
#

# Alla fine, consideriamo il test Goodness of fit.
# The Kolmogorov-Smirnov test in the library *stats*
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="psstd", mean=m, sd=sd, nu=df, xi=xi, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.070608, p-value = 0.01367
# alternative hypothesis: two-sided
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# Cramer-von Mises test of goodness-of-fit
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "psstd", mean=m, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 4, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.87823, p-value = 0.004742
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "psstd", mean=m, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 4, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# An = 4.4973, p-value = 0.005004
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_garch1_q2_p1_res
num_lags <- 5                   # Setting the lag parameter for the test.
Xt_t_student_asymmetric_garch1_q2_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_garch1_q2_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_t_student_asymmetric_garch1_q2_p1_res   
Xt_t_student_asymmetric_garch1_q2_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_garch1_q2_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
# Si ha un p-value di 0.008424 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità
#
# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q2_p1)
show(Xt_t_student_asymmetric_garch1_q2_p1_w)
# Si ha un p-value di 0.03038 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 2.2677, df = 1, p-value = 0.1321
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 6  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_asymmetric_garch1_q2_p1_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_asymmetric_garch1_q2_p1_lb)
# I risultati hanno un p-value < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

modello[['simulazione']][['garch_q2_p1']][['asimmetrico']][['1']] <- append(modello[['simulazione']][['garch_q2_p1']][['asimmetrico']][['1']], 
                                                                            list('lm'=Xt_t_student_asymmetric_garch1_q2_p1_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                                 'Cullen-Frey'=Xt_t_student_asymmetric_garch1_q2_p1_cf, 
                                                                                 'Breusch-Pagan'=Xt_t_student_asymmetric_garch1_q2_p1_bp, 'White'=Xt_t_student_asymmetric_garch1_q2_p1_w, 
                                                                                 'Ljiung-Box'=Xt_t_student_asymmetric_garch1_q2_p1_lb, 'Dickey-Fuller'=Xt_t_student_asymmetric_garch1_q2_p1_adf, 
                                                                                 'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_garch1_q2_p1_kpss, 'Shapiro-Wilk'=Xt_t_student_asymmetric_garch1_q2_p1_sw,    
                                                                                 'Generalized Distribution'=fitdist_test))

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
# -1.366555 -1.888880 -1.006714 
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_asymmetric_garch1_q2_p1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt   lwr.ci   upr.ci 
# 5.412643 3.447495 8.338754

Xt_t_student_asymmetric_garch1_q2_p1_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_t_student_asymmetric_garch1_q2_p1_cf <- append(Xt_t_student_asymmetric_garch1_q2_p1_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -5.132047   max:  2.300351 
# median:  0.06576357 
# mean:  -0.002418708 
# estimated sd:  0.8262552 
# estimated skewness:  -1.445498 
# estimated kurtosis:  8.60122 

# Goodness of fit
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_asymmetric_garch1_q2_p1_res
show(c(mean(y),var(y)))
# [1] 7.234664e-18 6.816501e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] 7.049483e-18 1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, graph=TRUE, boot=5000)
Xt_t_student_asymmetric_garch1_q2_p1_cf <- append(Xt_t_student_asymmetric_garch1_q2_p1_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -6.153227   max:  2.814234 
# median:  0.05739205 
# mean:  7.049483e-18 
# estimated sd:  1 
# estimated skewness:  -1.366555 
# estimated kurtosis:  8.412643 

fitdist_test <- list()
# Distribuzione student asimmetrica generalizzata
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "sstd", start=list(nu=3.2, xi=1.5), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' std  ' by maximum likelihood 
# Parameters : 
#          estimate Std. Error
# nu 3.4607151  0.2845284
# xi 0.8539229  0.0423700
# Fixed parameters:
#   value
# mean     0
# sd       1
# Loglikelihood:  -650.3156   AIC:  1304.631   BIC:  1313.061 
# Correlation matrix:
#        nu         xi
# nu  1.0000000 -0.2537074
# xi -0.2537074  1.0000000
#

# Alla fine, consideriamo il test Goodness of fit.
# The Kolmogorov-Smirnov test in the library *stats*
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="psstd", mean=m, sd=sd, nu=df, xi=xi, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.0589, p-value = 0.06228
# alternative hypothesis: two-sided
# Non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.

# Cramer-von Mises test of goodness-of-fit
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "psstd", mean=m, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters  mean = 0, sd = 1, nu = 3, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.63226, p-value = 0.0186
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "psstd", mean=m, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters  mean = 0, sd = 1, nu = 3, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# An = 3.6983, p-value = 0.01227
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_garch1_q2_p1_res
num_lags <- 5                   # Setting the lag parameter for the test.
Xt_t_student_asymmetric_garch1_q2_p1_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_garch1_q2_p1_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_t_student_asymmetric_garch1_q2_p1_res   
Xt_t_student_asymmetric_garch1_q2_p1_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_garch1_q2_p1_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
# Si ha un p-value di 9.843e-09 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità
#
# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q2_p1)
show(Xt_t_student_asymmetric_garch1_q2_p1_w)
# Si ha un p-value di 3.869e-08 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 0.90869, df = 1, p-value = 0.3405
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 6  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
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
                                                                        'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_garch1_q2_p1_kpss, 'Shapiro-Wilk'=Xt_t_student_asymmetric_garch1_q2_p1_sw, 
                                                                        'Generalized Distribution'=fitdist_test)))

# Con i parametri stimati, il modello Garch(2,1) con distribuzione t-student asimmetrica
# ha presenza di eteroschedasticità nei residui del modello e assenza di autocorrelazione
# eccetto nel lag 1 in cui il p-value < 0.05.

##########################################
##########################################

#### DISTRIBUZIONE NORMALE
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
skew <- list()
set.seed(123)
s <- DescTools::Skew(Xt_normal_garch1_q2_p2_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew[['0.80']] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -0.08014246 -0.23155545  0.06907189 
set.seed(123)
s <- DescTools::Skew(Xt_normal_garch1_q2_p2_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
skew[['0.99']] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -0.08014246 -0.35778686  0.21597514 
kurt <- list()
set.seed(123)
k <- DescTools::Kurt(Xt_normal_garch1_q2_p2_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
kurt[['0.80']] <- k
show(k)
#  kurt      lwr.ci   upr.ci 
# -0.06302992 -0.33610769  0.32050405  
set.seed(123)
k <- DescTools::Kurt(Xt_normal_garch1_q2_p2_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.89, ci.type= "bca", R=1000)
kurt[['0.99']] <- k
show(k)
#  kurt      lwr.ci   upr.ci 
# -0.06302992 -0.39943829  0.42263731 

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
Xt_normal_garch1_q2_p2_cf <- descdist(Xt, discrete=FALSE, boot=5000)
# Da Cullen-Frey, notiamo che le osservazioni seguono una distribuzione normale.

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
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
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
Xt_normal_garch1_q2_p2_fit= ugarchfit(spec = uspec, data = dist_normal1)
print(Xt_normal_garch1_q2_p2_fit)
intconf = confint(Xt_normal_garch1_q2_p2_fit)
show(intconf)

fit <- Xt_normal_garch1_q2_p2_fit
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
skew <- list()
set.seed(123)
s <- DescTools::Skew(Xt_normal_garch1_q2_p2_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew[['0.80']] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -0.04782991 -0.23058973  0.17510086  
set.seed(123)
s <- DescTools::Skew(Xt_normal_garch1_q2_p2_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
skew[['0.99']] <- s
show(s)
#  skew       lwr.ci      upr.ci 
# -0.04782991 -0.44221019  0.31886224  
kurt <- list()
set.seed(123)
k <- DescTools::Kurt(Xt_normal_garch1_q2_p2_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
kurt[['0.80']] <- k
show(k)
#  kurt      lwr.ci   upr.ci 
# 0.9834121 0.6180878 1.5050297   
set.seed(123)
k <- DescTools::Kurt(Xt_normal_garch1_q2_p2_res , weights=NULL, na.rm=TRUE, method=2, conf.level=0.89, ci.type= "bca", R=1000)
kurt[['0.99']] <- k
show(k)
#  kurt      lwr.ci   upr.ci 
# 0.9834121 0.5556488 1.6089440 

# Cullen-Frey
Xt_normal_garch1_q2_p2_cf <- list()
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, method= "sample", boot=2000)
Xt_normal_garch1_q2_p2_cf <- append(Xt_normal_garch1_q2_p2_cf, list(cf))
show(cf)
# summary statistics
# 
# min:  -1.84271   max:  1.74666 
# median:  -0.006976761 
# mean:  0.007332518 
# sample sd:  0.4923746 
# sample skewness:  0.006340228 
# sample kurtosis:  3.944163 
# Da Cullen-Frey, notiamo che la serie non segue una distribuzione normale. Proviamo ad eseguire dei test
# per verifica se sia una distribuzione normale o logistica.

# Test pe verificare che sia una distribuzione t-student -> Goodness of fit: prendere i dati e fare una distribuzione parametrica della distribuzione per verificare che sia una t-student
# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- residuals(Xt_normal_garch1_q2_p2_fit)
show(c(mean(y),var(y)))
# [1] 0.002464275 1.027130747
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] -6.112163e-18  1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, method= "sample", boot=2000)
Xt_normal_garch1_q2_p2_cf <- append(Xt_normal_garch1_q2_p2_cf, list(cf))
show(cf)
# summary statistics
# ------
# min:  -1.84271   max:  1.74666 
# median:  -0.006976761 
# mean:  0.007332518 
# sample sd:  0.4923746 
# sample skewness:  0.006340228 
# sample kurtosis:  3.944163 
# Da Cullen-Frey, abbiamo una possibile distribuzione logistica o di student per i residui.

# Esploriamo queste possibilità in più dettagli.
z_st_qemp <- EnvStats::qemp(stats::ppoints(z_st), z_st) # Empirical quantiles of the residuals.
z_st_demp <- EnvStats::demp(z_st_qemp, z_st)     # Empirical probability density of the residuals.
z_st_pemp <- EnvStats::pemp(z_st_qemp, z_st)     # Empirical distribution function of the residuals.  
x <- z_st_qemp
y_d <- z_st_demp
y_p <- z_st_pemp
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata con il parametri location=0,
# scale=sqrt(3)/pi e shape=1.
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the GARCH(2,1) model with a normal distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dgnorm(x, mu=0, alpha=sqrt(2), beta=2), lwd=2, col= "red")
lines(x, dglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", Gen_Log_Dens_Func), 
       col=c("darkblue", "red","magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(2,1) model with a normal distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pgnorm(x, mu=0, alpha=sqrt(2), beta=2), lwd=2, col= "red")
lines(x, pglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", Gen_Log_Distr_Func), 
       col=c("darkblue", "red", "magenta"), 
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
# location -0.1398082 0.21474402
# scale     0.6109650 0.04867046
# shape     1.1689192 0.26125278
# Loglikelihood:  -716.5702   AIC:  1439.14   BIC:  1451.778 
# Correlation matrix:
#   location      scale      shape
# location  1.0000000 -0.8735474 -0.9770817
# scale    -0.8735474  1.0000000  0.8886017
# shape    -0.9770817  0.8886017  1.0000000
#
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_glogis_bd <- fitdistrplus::bootdist(fitdist_glogis, niter=1000)
fitdist_test[["glogis"]][["bootdist"]] <- fitdist_glogis_bd
summary(fitdist_glogis_bd)
# Parametric bootstrap medians and 95% percentile CI 
#             Median     2.5%     97.5%
# location -0.1468732 -0.5763657 0.1621430
# scale     0.6114775  0.5331466 0.6970307
# shape     1.1807657  0.8449669 1.8077666
# We fix the initial points of the constrained maximization procedure
location <- fitdist_glogis_bd[["fitpart"]][["estimate"]][1]
scale <- fitdist_glogis_bd[["fitpart"]][["estimate"]][2]
shape <- fitdist_glogis_bd[["fitpart"]][["estimate"]][3]
show(c(location,scale,shape)) # the estimated parameters.
# -0.1398082  0.6109650  1.1689192  
minus_logLik <- function(x) -sum(log(dglogis(z_st, location=x[1], scale=x[2], shape=x[3]))) # the log-likelihood of the generalized logistic
# distribution.
fminunc_result <- fminunc(x0=c(location, scale, shape), fn=minus_logLik)   # the minimization procedure where (location,scale,shape) is the 
# starting point.
show(c(fminunc_result$par[1],fminunc_result$par[2],fminunc_result$par[3])) # the estimated parameters.
#    location       scale       shape 
#  -0.1390338  0.6105466  1.1674834 

# Distribuzione normale generalizzata
fitdist_gnorm <- fitdistrplus::fitdist(z_st, "gnorm", start=list(mu=0, alpha=1, beta=1), method= "mle")
fitdist_test[["gnorm"]][["gnorm"]] <- fitdist_gnorm
summary(fitdist_gnorm)
# Fitting of the distribution ' t_ls ' by maximum likelihood 
# Parameters : 
#         estimate Std. Error
# mu    -0.001154565 0.04397541
# alpha  1.538574948 0.07622422
# beta   2.435337774 0.25232697
# Loglikelihood:  -705.7415   AIC:  1417.483   BIC:  1430.121 
# Correlation matrix:
#   mu       alpha        beta
# mu     1.00000000 -0.01884259 -0.02302986
# alpha -0.01884259  1.00000000  0.81523750
# beta  -0.02302986  0.81523750  1.00000000
#
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_gnorm_bd <- fitdistrplus::bootdist(fitdist_gnorm, niter=1000)
fitdist_test[["gnorm"]][["bootdist"]] <- fitdist_gnorm_bd
summary(fitdist_gnorm_bd)
# Parametric bootstrap medians and 95% percentile CI 
#             Median     2.5%     97.5%
# mu    -0.002137185 -0.0852385 0.08022336
# alpha  1.541962330  1.3619640 1.70014081
# beta   2.457314768  1.9963907 3.10513150
# We fix the initial points of the constrained maximization procedure
mu <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["mu"]][1]
alpha <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["alpha"]][1]
beta <- fitdist_gnorm_bd[["fitpart"]][["sd"]][["beta"]][1]
show(c(mu,alpha,beta)) # the estimated parameters.
# 0.04397541 0.07622422 0.25232697 

# Setting
location <- fitdist_glogis[["estimate"]][["location"]]
scale <- fitdist_glogis[["estimate"]][["scale"]]
shape <- fitdist_glogis[["estimate"]][["shape"]]
#
mu <- fitdist_gnorm[["estimate"]][["mu"]]
alpha <- fitdist_gnorm[["estimate"]][["alpha"]]
beta <- fitdist_gnorm[["estimate"]][["beta"]]
# We plot the histogram and the empirical density function of the standardized residuals together with the density function of the estimated 
# generalized logistic and generalized normal.
Est_Gen_Log_Dens_Func <- bquote(paste("Estimated Generalized Logistic Density Function, location = ", .(loc), ", scale = ", .(scl) , ", shape = ", .(shp)))
Est_Gen_Norm_Dens_Func <- bquote(paste("Estimated Generalized Normal Density Function, mu = ", .(mu), ", alpha = ", .(alpha) , ", beta = ", .(beta)))
hist(z_st, breaks= "Scott", col= "green", border= "black", xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), 
     freq=FALSE, main= "Density Histogram of the Standardized Residuals of the GARCH(2,1) model + Empirical Density Function + Estimated Generalized Logistic Density + Estimated Generalized Normal Density", 
     xlab= "Standardized Residuals", ylab= "Density")
lines(density(z_st), lwd=2, col= "darkgreen")
lines(x, dgnorm(x, mu=mu, alpha=alpha, beta=beta), lwd=2, col= "red")
lines(x, dglogis(x, location=location, scale=scale, shape=shape), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density Function", Est_Gen_Norm_Dens_Func, Est_Gen_Log_Dens_Func), 
       col=c("darkgreen", "red", "magenta"),
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

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
# D = 0.040918, p-value = 0.3736
# alternative hypothesis: two-sided
# Con un p-value cosi alto, non possiamo rigettare l'ipotesi nulla. Di conseguenza, abbiamo una prova sufficiente
# che la serie è una distribuzione logistica generalizzata.
mu <- fitdist_gnorm[["estimate"]][["mu"]]
alpha <- fitdist_gnorm[["estimate"]][["alpha"]]
beta <- fitdist_gnorm[["estimate"]][["beta"]]
KS_z_st_t_ls <- stats::ks.test(z_st, y="pgnorm", mu=mu, alpha=alpha, beta=beta, alternative= "two.sided")
fitdist_test[["gnorm"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.026855, p-value = 0.8644
# alternative hypothesis: two-sided
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione normale generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.

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
# with parameters location = -0.1398, scale = 0.611, shape = 1.1689
# Parameters assumed to be fixed
# data:  z_st
# omega2 =  0.18343, p-value = 0.302
# Non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
mu <- fitdist_gnorm[["estimate"]][["mu"]]
alpha <- fitdist_gnorm[["estimate"]][["alpha"]]
beta <- fitdist_gnorm[["estimate"]][["beta"]]
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "pgnorm", mu=mu, alpha=alpha, beta=beta, estimated=FALSE)
fitdist_test[["gnorm"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘pgnorm’
# with parameters mu = -0.00115456536466935, alpha = 1.53857494838738, beta = 2.43533777374854
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.039517, p-value = 0.9358
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione normale generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.
loc <- round(fitdist_glogis[["estimate"]][["location"]],4)
scl <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shp <- round(fitdist_glogis[["estimate"]][["shape"]],4)
AD_z_st_glogis <- goftest::ad.test(z_st, null= "pglogis", location=location, scale=scale, shape=shape, estimated=FALSE)
fitdist_test[["glogis"]][["Anderson-Darling"]] <- AD_z_st_glogis
show(AD_z_st_glogis)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘pglogis’
# with parameters location = -0.139808156226173, scale = 0.610964990486153, shape = 1.16891922981522
# Parameters assumed to be fixed
# data:  z_st
# An = 1.3063, p-value = 0.2303
# Non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
loc <- round(fitdist_glogis[["estimate"]][["location"]],4)
scl <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shp <- round(fitdist_glogis[["estimate"]][["shape"]],4)
AD_z_st_t_ls <- goftest::ad.test(z_st, "pgnorm", mu=mu, alpha=alpha, beta=beta, estimated=FALSE)
fitdist_test[["gnorm"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘pgnorm’
# with parameters mu = -0.00115456536466935, alpha = 1.53857494838738, beta = 2.43533777374854
# Parameters assumed to be fixed
# data:  z_st
# An = 0.31769, p-value = 0.9243
# E non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione normale generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata

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
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_normal_garch1_q2_p2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_normal_garch1_q2_p2_lb)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione

modello[['stimati']][['garch_q2_p2']] <- append(modello[['stimati']][['garch_q2_p2']], 
                                                list('normale'=list('Xt'=Xt_normal_garch1_q2_p2_new, 'a0'=a0, 'a1'=aq[1], 'a2'=aq[2], 'b1'=bp[1], 'b2'=bp[2], 'q'=q, 'p'=p,
                                                                    'stazionarietà'=stazionaietà, 'lm'=Xt_normal_garch1_q2_p2_lm,'skew'=skew, 'kurt'=kurt, 
                                                                   'Cullen-Frey'=Xt_normal_garch1_q2_p2_cf, 
                                                                   'Breusch-Pagan'=Xt_normal_garch1_q2_p2_bp, 'White'=Xt_normal_garch1_q2_p2_w, 
                                                                   'Ljiung-Box'=Xt_normal_garch1_q2_p2_lb, 'Dickey-Fuller'=Xt_normal_garch1_q2_p2_adf, 
                                                                   'Kwiatowski-Phillips-Schmidt-Shin'=Xt_normal_garch1_q2_p2_kpss, 'Shapiro-Wilk'=Xt_normal_garch1_q2_p2_sw, 
                                                                   'Generalized Distribution'=fitdist_test)))

# In questo modello Garch(1,1) con distribuzione normale risulta essere eteroschedastico e con assenza
# di autocorrelazione con i parametri stimati.

##########################################

#### DISTRIBUZIONE T-STUDENT SIMMETRICA
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
length <- nrow(Data_df)
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
skew <- list()
set.seed(123)
s <- DescTools::Skew(Xt_t_student_symmetric_garch2_q2_p2, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew[['0.80']] <-s
show(s)
#  skew          lwr.ci      upr.ci 
# 0.04680226 -0.48451031  0.56101369
set.seed(123)
s <- DescTools::Skew(Xt_t_student_symmetric_garch2_q2_p2, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
skew[['0.99']] <-s
show(s)
#  skew          lwr.ci      upr.ci 
# 0.04680226 -0.89569276  0.97717169 
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_symmetric_garch2_q2_p2, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#      kurt      lwr.ci      upr.ci 
#    3.600370 2.213924 5.432474 

Xt_t_student_symmetric_garch2_q2_p2_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_t_student_symmetric_garch2_q2_p2_cf <- append(Xt_t_student_symmetric_garch2_q2_p2_cf, list(cf))
show(cf)
# summary statistics
# ------
# min:  -4.439292   max:  4.388261 
# median:  -0.04177032 
# mean:  -0.02942944 
# estimated sd:  0.8498939 
# estimated skewness:  0.04680226 
# estimated kurtosis:  6.60037 

# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_symmetric_garch2_q2_p2_res
show(c(mean(y),var(y)))
# [1] 9.589009e-18 7.218655e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] 8.410427e-18 1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, boot=5000)
Xt_t_student_symmetric_garch2_q2_p2_cf <- append(Xt_t_student_symmetric_garch2_q2_p2_cf, list(cf))
show(cf)
# summary statistics
# ------
# min:  -5.217521   max:  5.172572 
# median:  -0.01366885 
# mean:  8.410427e-18 
# estimated sd:  1 
# estimated skewness:  0.03342274 
# estimated kurtosis:  6.594698  

# Esploriamo queste possibilità in più dettagli.
z_st_qemp <- EnvStats::qemp(stats::ppoints(z_st), z_st) # Empirical quantiles of the residuals.
z_st_demp <- EnvStats::demp(z_st_qemp, z_st)     # Empirical probability density of the residuals.
z_st_pemp <- EnvStats::pemp(z_st_qemp, z_st)     # Empirical distribution function of the residuals.  
x <- z_st_qemp
y_d <- z_st_demp
y_p <- z_st_pemp
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata con il parametri location=0,
# scale=sqrt(3)/pi e shape=1.
m <- 0
s <- sqrt(1/3)
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the GARCH(2,2) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dstd(x, mean=0, sd=1, nu=5), lwd=2, col= "red")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", "Student Density Function, df=5"), 
       col=c("darkblue", "darkgreen", "red"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
loc <- 0
shp <- 1
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(1,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pstd(x, mean = 0, sd = 1, nu = 5), lwd=2, col= "red")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=5"), 
       col=c("darkblue", "darkgreen", "red"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

fitdist_test <- list()
# Dato che la Generalized Student distribution ha il parametro location che coincide con la media, possiamo provare 
# a fittare i dati con una generalized Student distribution con zero location. 
# Questo viene fatto cambiando il parametro m nella funzione *fitdistrplus::fitdist*.
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "std", start=list(nu=3.2), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' std  ' by maximum likelihood 
# Parameters : 
#    estimate Std. Error
# nu 4.190374  0.5172012
# Fixed parameters:
#        value
# mean     0
# sd       1
# Loglikelihood:  -679.1697   AIC:  1360.339   BIC:  1364.554
# 
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_t_ls_bd <- fitdistrplus::bootdist(fitdist_t_ls, niter=1000)
fitdist_test[["gstudent"]][["bootdist"]] <- fitdist_t_ls_bd
summary(fitdist_t_ls_bd)
# Parametric bootstrap medians and 95% percentile CI 
#      Median     2.5%     97.5%
# nu  4.240719 3.481995 5.754641

# We plot the histogram and the empirical density function of the standardized residuals together with the density function of the estimated 
# generalized student.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
Est_Gen_Stud_Dens_Func <- bquote(paste("Estimated Student Density Function, location = ", .(m), ", scale = ", .(s) , ", degrees of freedom = ", .(df)))
hist(z_st, breaks= "Scott", col= "green", border= "black", xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+1.1), 
     freq=FALSE, main= "Density Histogram of the Standardized Residuals of the GARCH(1,1) model + Empirical 
     Density Function + Estimated Generalized Student Density", xlab= "Standardized Residuals", ylab= "Density")
lines(density(z_st), lwd=2, col= "darkgreen")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "red")
lines(x, dstd (x, m=m, sd=sd, nu=df), lwd=2, col= "blue")
legend("topleft", legend=c("Empirical Density", "Standard Gaussian Density", Est_Gen_Stud_Dens_Func), 
       col=c("darkgreen","red", "blue"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# We also plot the empirical distribution function of the standardized residuals together with the distribution function of the standard 
# Gaussian and generalized Student.
Est_Gen_Stud_Distr_Func <- bquote(paste("Estimated Student Distribution Function, location = ", .(m), ", scale = ", .(s) , ", degrees of freedom = ", .(df)))
plot(z_st, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the ARCH(2) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkgreen")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "red")
lines(x, pstd(x, dstd (x, m=m, sd=sd, nu=df)), lwd=2, col= "blue")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=5", Est_Gen_Stud_Distr_Func), 
       col=c("darkgreen","red", "blue"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

# The Kolmogorov-Smirnov test in the library *stats*
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="pstd", mean=m, sd=sd, nu=df, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.021112, p-value = 0.9791
# alternative hypothesis: two-sided
# Non possiamo rigettare l'ipotesi nulla, quindi, possiamo affermare che la serie segue una distribuzione t-student simmetrica.

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_symmetric_garch2_q2_p2_res
num_lags <- 7                   # Setting the lag parameter for the test.
Xt_t_student_symmetric_garch2_q2_p2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_symmetric_garch2_q2_p2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_t_student_symmetric_garch2_q2_p2_res   
Xt_t_student_symmetric_garch2_q2_p2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_symmetric_garch2_q2_p2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
# Si ha un p-value di 0.09135 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità
#
# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch2_q2_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch2_q2_p2)
show(Xt_t_student_symmetric_garch2_q2_p2_w)
# Si ha un p-value di 0.121 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 0.69404, df = 1, p-value = 0.4048
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 7  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_symmetric_garch2_q2_p2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_symmetric_garch2_q2_p2_lb)
# Nella forma estesa del test di Ljung-Box si hanno alcuni lag con un p-value < 0.05
# questo indica una possibile presenza di autocorrelazione

modello[['simulazione']][['garch_q2_p2']][['simmetrico']][['2']] <- append(modello[['simulazione']][['garch_q2_p2']][['simmetrico']][['2']], 
                                                                           list('lm'=Xt_t_student_symmetric_garch2_q2_p2_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                                'Cullen-Frey'=Xt_t_student_symmetric_garch2_q2_p2_cf, 
                                                                                'Breusch-Pagan'=Xt_t_student_symmetric_garch2_q2_p2_bp, 'White'=Xt_t_student_symmetric_garch2_q2_p2_w, 
                                                                                'Ljiung-Box'=Xt_t_student_symmetric_garch2_q2_p2_lb, 'Dickey-Fuller'=Xt_t_student_symmetric_garch2_q2_p2_adf, 
                                                                                'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_garch2_q2_p2_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_garch2_q2_p2_sw, 
                                                                                'Generalized Distribution'=fitdist_test))

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
skew <- list()
set.seed(123)
s <- DescTools::Skew(Xt_t_student_symmetric_garch2_q2_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew[['0.80']] <- s
show(s)
#  skew          lwr.ci      upr.ci 
# -0.255469421 -0.602616033 -0.001999383
s <- DescTools::Skew(Xt_t_student_symmetric_garch2_q2_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
skew[['0.99']] <- s
show(s)
#  skew          lwr.ci      upr.ci 
# -0.2554694 -0.8663158  0.2793340 
kurt <- list()
set.seed(123)
k <- DescTools::Kurt(Xt_t_student_symmetric_garch2_q2_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
kurt[['0.80']] <- k
show(k)
#      kurt      lwr.ci      upr.ci 
#     2.083194 1.391056 3.136408  
set.seed(123)
k <- DescTools::Kurt(Xt_t_student_symmetric_garch2_q2_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.99, ci.type= "bca", R=1000)
kurt[['0.99']] <- k
show(k)
#      kurt      lwr.ci      upr.ci 
#    2.0831942 0.8683994 3.8755929 

Xt_t_student_symmetric_garch2_q2_p2_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, boot=5000)
Xt_t_student_symmetric_garch2_q2_p2_cf <- append(Xt_t_student_symmetric_garch2_q2_p2_cf, list(cf))
show(cf)
# summary statistics
# ------
# min:  -1.263695   max:  0.9833368 
# median:  -0.01174033 
# mean:  -0.01565248 
# estimated sd:  0.280533 
# estimated skewness:  -0.305897 
# estimated kurtosis:  5.093433

# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_symmetric_garch2_q2_p2_res
show(c(mean(y),var(y)))
# [1] -7.005898e-18  7.859170e-02
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] -6.644208e-18  1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(z_st, discrete=FALSE, boot=5000)
Xt_t_student_symmetric_garch2_q2_p2_cf <- append(Xt_t_student_symmetric_garch2_q2_p2_cf, list(cf))
show(cf)
# summary statistics
# ------
# min:  -4.44483   max:  3.613402 
# median:  -0.007762063 
# mean:  -6.644208e-18 
# estimated sd:  1 
# estimated skewness:  -0.2554694 
# estimated kurtosis:  5.083194 

# Esploriamo queste possibilità in più dettagli.
z_st_qemp <- EnvStats::qemp(stats::ppoints(z_st), z_st) # Empirical quantiles of the residuals.
z_st_demp <- EnvStats::demp(z_st_qemp, z_st)     # Empirical probability density of the residuals.
z_st_pemp <- EnvStats::pemp(z_st_qemp, z_st)     # Empirical distribution function of the residuals.  
x <- z_st_qemp
y_d <- z_st_demp
y_p <- z_st_pemp
# Creiamo un istogramma dei residui standardizzati insieme alla funzione di densità empirica, la funzione di densità gaussiana standard.
# la funzione di densità Student con gradi di libertà df=5, e la funzione di densità logistica generalizzata con il parametri location=0,
# scale=sqrt(3)/pi e shape=1.
m <- 0
s <- sqrt(1/3)
loc <- 0
shp <- 1
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
Gen_Stud_Dens_Func <- bquote(paste("Gener. Student Density Function, m = ", .(m),", s = ", .(s)))
plot(x, y_d, xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+0.75), type= "n")
hist(z_st, breaks= "Scott", col= "cyan", border= "black", xlim=c(x[1]-1.0, x[length(x)]+1.0), ylim=c(0, y_d[length(y)]+0.75), 
     freq=FALSE, main= "Density Histogram and Empirical Density Function of the Standardized Residuals of the GARCH(2,2) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Histogram Values+Density Function")
lines(x, y_d, lwd=2, col= "darkblue")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, dstd(x, mean=0, sd=1, nu=5), lwd=2, col= "red")
lines(x, dglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "orange")
legend("topleft", legend=c("Empirical Density Function", "Standard Gaussian Density Function", "Student Density Function, df=5", Gen_Log_Dens_Func), 
       col=c("darkblue", "darkgreen", "red", "orange"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# Plot della funzione di distribuzione empirica dei residui standardizzati
Gen_Log_Distr_Func <-bquote(paste("Gener. Logistic Distribution Function, location = ", .(loc),", scale = ", sqrt(3)/pi, ", shape = ", .(shp)))
Gen_Stud_Distr_Func <-bquote(paste("Gener. Student Distribution Function, location = ", .(m),", scale = ", .(s)))
plot(x, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the GARCH(1,1) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "darkblue")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pstd(x, mean = 0, sd = 1, nu = 5), lwd=2, col= "red")
lines(x, pglogis(x, location=0, scale=sqrt(3)/pi, shape=1), lwd=2, col= "orange")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=5",  Gen_Log_Distr_Func), 
       col=c("darkblue", "darkgreen", "red", "orange"), 
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
# location 0.09911437 0.10627568
# scale    0.50547711 0.03170329
# shape    0.88294117 0.11833221
# Loglikelihood:  -689.4708   AIC:  1384.942   BIC:  1397.586 
# Correlation matrix:
#   location      scale      shape
# location  1.0000000 -0.7212606 -0.9249803
# scale    -0.7212606  1.0000000  0.7930994
# shape    -0.9249803  0.7930994  1.0000000
#
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_glogis_bd <- fitdistrplus::bootdist(fitdist_glogis, niter=1000)
fitdist_test[["glogis"]][["bootdist"]] <- fitdist_glogis_bd
summary(fitdist_glogis_bd)
# Parametric bootstrap medians and 95% percentile CI 
#            Median       2.5%     97.5%
# location 0.09217931 -0.1904434 0.3290632
# scale    0.50482887  0.4377730 0.5804352
# shape    0.88769556  0.6532831 1.2751974
# We fix the initial points of the constrained maximization procedure
location <- fitdist_glogis_bd[["fitpart"]][["estimate"]][1]
scale <- fitdist_glogis_bd[["fitpart"]][["estimate"]][2]
shape <- fitdist_glogis_bd[["fitpart"]][["estimate"]][3]
minus_logLik <- function(x) -sum(log(dglogis(z_st, location=x[1], scale=x[2], shape=x[3]))) # the log-likelihood of the generalized logistic
# distribution.
fminunc_result <- fminunc(x0=c(location, scale, shape), fn=minus_logLik)   # the minimization procedure where (location,scale,shape) is the 
# starting point.
show(c(fminunc_result$par[1],fminunc_result$par[2],fminunc_result$par[3])) # the estimated parameters.
#  location    scale       shape 
# 0.09905461 0.50553941 0.88296832 
#
logLik <- -fminunc_result$value # the minimized negative log-likelihood
n <- length(z_st)
k <- length(fminunc_result[["par"]])
AIC <- 2*k-2*logLik
BIC <- k*log(n)-2*logLik
AICc <- AIC + 2*k*((k+1)/(n-k-1))
show(c(logLik, AIC, BIC, AICc))
#    logLik    AIC       BIC      AICc
#  -689.4708 1384.9417 1397.5855 1384.9901

# Dato che la Generalized Student distribution ha il parametro location che coincide con la media, possiamo provare 
# a fittare i dati con una generalized Student distribution con zero location. 
# Questo viene fatto cambiando il parametro m nella funzione *fitdistrplus::fitdist*.
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "std", start=list(nu=3.2), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' std  ' by maximum likelihood 
# Parameters : 
#   estimate Std. Error
# nu 4.217238  0.5599402
# Fixed parameters:
#   value
# mean     0
# sd       1
# Loglikelihood:  -687.0721   AIC:  1376.144   BIC:  1380.359  
# 
# La funzione fitdistrplus::bootdist() permette di determinare l'incertezza nei parametri stimati della distribuzione fittata.
set.seed(12345)
fitdist_t_ls_bd <- fitdistrplus::bootdist(fitdist_t_ls, niter=1000)
fitdist_test[["gstudent"]][["bootdist"]] <- fitdist_t_ls_bd
summary(fitdist_t_ls_bd)
# Parametric bootstrap medians and 95% percentile CI 
#      Median     2.5%     97.5%
# nu  4.269423 3.465110 5.756986 

# We plot the histogram and the empirical density function of the standardized residuals together with the density function of the estimated 
# generalized student.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
#
location <- fitdist_glogis[["estimate"]][["location"]]
scale <- fitdist_glogis[["estimate"]][["scale"]]
shape <- fitdist_glogis[["estimate"]][["shape"]]
#
Gen_Log_Dens_Func <- bquote(paste("Gener. Logistic Density Function, location = ", .(loc),", scale = ", scale, ", shape = ", .(shp)))
Est_Gen_Stud_Dens_Func <- bquote(paste("Estimated Student Density Function, mean = ", .(m), ", sd = ", .(sd) , ", degrees of freedom = ", .(df)))
hist(z_st, breaks= "Scott", col= "green", border= "black", xlim=c(x[1]-2.0, x[length(x)]+2.0), ylim=c(0, y_d[length(y_d)]+1.1), 
     freq=FALSE, main= "Density Histogram of the Standardized Residuals of the GARCH(1,1) model + Empirical 
     Density Function + Estimated Generalized Student Density", xlab= "Standardized Residuals", ylab= "Density")
lines(density(z_st), lwd=2, col= "darkgreen")
lines(x, dnorm(x, m=0, sd=1), lwd=2, col= "red")
lines(x, dstd (x, mean=m, s=sd, nu=df), lwd=2, col= "blue")
lines(x, dglogis(x, location=location, scale=scale, shape=shape), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Density", "Standard Gaussian Density", Est_Gen_Stud_Dens_Func, Gen_Log_Dens_Func), 
       col=c("darkgreen","red", "blue", "magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")
# We also plot the empirical distribution function of the standardized residuals together with the distribution function of the standard 
# Gaussian and generalized Student.
Est_Gen_Stud_Distr_Func <- bquote(paste("Estimated Student Distribution Function, mean = ", .(m), ", sd = ", .(sd) , ", degrees of freedom = ", .(df)))
Est_Gen_Log_Distr_Func <- bquote(paste("Estimated Generalized Logistic Distribution Function, location = ", .(location), ", scale = ", .(scale) , ", shape = ", .(shp)))
plot(z_st, y_p, pch=16, col= "cyan", xlim=c(x[1]-1.0, x[length(x)]+1.0), 
     main= "Empirical Distribution Function of the Standardized Residuals of the ARCH(2) model with a symmetric t-student distribution", 
     xlab= "Standardized Residuals", ylab= "Empirical Probability Distribution")
lines(x, y_p, lwd=2, col= "orange")
lines(x, pnorm(x, m=0, sd=1), lwd=2, col= "darkgreen")
lines(x, pstd(x, mean=m, s=sd, nu=df), lwd=2, col= "red")
lines(x, pglogis(x, location=location, scale=scale, shape=shape), lwd=2, col= "magenta")
legend("topleft", legend=c("Empirical Distribution Function", "Standard Gaussian Distribution Function", "Student Distribution Function, df=5", Est_Gen_Stud_Distr_Func, Est_Gen_Log_Distr_Func), 
       col=c("orange", "darkgreen","red", "magenta"), 
       lty=1, lwd=0.1, cex=0.8, x.intersp=0.50, y.intersp=0.70, text.width=2, seg.len=1, text.font=4, box.lty=0,
       inset=-0.01, bty= "n")

# The Kolmogorov-Smirnov test in the library *stats*
loc <- round(fitdist_glogis[["estimate"]][["location"]],4)
scl <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shp <- round(fitdist_glogis[["estimate"]][["shape"]],4)
KS_z_st_glogis <- ks.test(z_st, y="pglogis", location=loc, scale=scl, shape=shp, alternative= "two.sided")
fitdist_test[["glogis"]][["Kolmogorov-Smirnov"]] <- KS_z_st_glogis
show(KS_z_st_glogis)
# 	Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.045957, p-value = 0.2416
# alternative hypothesis: two-sided
# Non possiamo rigettare l'ipotesi nulla, quindi, abbiamo una prova sufficiente
# che la serie è una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="pstd", mean=m, sd=sd, nu=df, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.033459, p-value = 0.6303
# alternative hypothesis: two-sided
# Non possiamo rigettare l'ipotesi nulla, quindi, possiamo affermare che la serie segue una distribuzione t-student simmetrica.

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
# with parameters location = 0.0991, scale = 0.5055, shape = 0.8829
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.20705, p-value = 0.2543
# Non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "pstd", mean=m, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘pstd’
# with parameters mean = 0, sd = 1, nu = 4
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.071665, p-value = 0.7416
# Non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.
loc <- round(fitdist_glogis[["estimate"]][["location"]],4)
scl <- round(fitdist_glogis[["estimate"]][["scale"]],4)
shp <- round(fitdist_glogis[["estimate"]][["shape"]],4)
AD_z_st_glogis <- goftest::ad.test(z_st, null= "pglogis", location=location, scale=scale, shape=shape, estimated=FALSE)
fitdist_test[["glogis"]][["Anderson-Darling"]] <- AD_z_st_glogis
show(AD_z_st_glogis)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘pglogis’
# with parameters location = 0.0991143710034504, scale = 0.505477108686048, shape = 0.88294117436035
# Parameters assumed to be fixed
# data:  z_st
# An = 1.2002, p-value = 0.2675
# Non ci sono evidenze sufficienti per respingere l'ipotesi nulla. 
# Questo suggerisce che la serie sembra seguire una distribuzione logistica generalizzata.
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "pstd", mean=m, sd=sd, nu=df, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 4
# Parameters assumed to be fixed
# data:  z_st
# An =  0.59536, p-value = 0.6523
# Non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_symmetric_garch2_q2_p2_res
num_lags <- 7                   # Setting the lag parameter for the test.
Xt_t_student_symmetric_garch2_q2_p2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_symmetric_garch2_q2_p2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_t_student_symmetric_garch2_q2_p2_res   
Xt_t_student_symmetric_garch2_q2_p2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_symmetric_garch2_q2_p2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
#
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
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
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
                                                                       'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_symmetric_garch2_q2_p2_kpss, 'Shapiro-Wilk'=Xt_t_student_symmetric_garch2_q2_p2_sw, 
                                                                       'Generalized Distribution'=fitdist_test)))

# Con i nuovi parametri stimati, nel modello Garch(2,2) con una distribuzione t-student simmetrica
# si ha presenza di eteroschedasticità nei residui del modello e assenza di autocorrelazione.

##########################################

#### DISTRIBUZIONE T-STUDENT ASIMMETRICA
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
# -1.1984789 -1.5480696 -0.8240799
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_asymmetric_garch1_q2_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt   lwr.ci   upr.ci 
# 4.132030 3.156296 5.807462 

Xt_t_student_asymmetric_garch1_q2_p2_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, method= "sample", boot=2000)
Xt_t_student_asymmetric_garch1_q2_p2_cf <- append(Xt_t_student_asymmetric_garch1_q2_p2_cf, list(cf)) 
show(cf)
#summary statistics
# ------
# min:  -4.957061   max:  3.775335 
# median:  0.09695226 
# mean:  -0.01081236 
# sample sd:  0.9550634 
# sample skewness:  -1.205601 
# sample kurtosis:  7.083448 
# Da Cullen-Frey, la serie potrebbe seguire una distribuzione esponenziale, gamma o beta.

# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_asymmetric_garch1_q2_p2_res
show(c(mean(y),var(y)))
# [1] -2.970844e-17  9.138646e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] -1.028084e-17  1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, method= "sample", boot=2000)
Xt_t_student_asymmetric_garch1_q2_p2_cf <- append(Xt_t_student_asymmetric_garch1_q2_p2_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:  -4.957061   max:  3.775335 
# median:  0.09695226 
# mean:  -0.01081236 
# sample sd:  0.9550634 
# sample skewness:  -1.205601 
# sample kurtosis:  7.083448  
# Da Cullen-Frey, la serie potrebbe seguire una distribuzione student.

fitdist_test <- list()
# Distribuzione student asimmetrica generalizzata
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "sstd", start=list(nu=3.2, xi=1.5), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' std  ' by maximum likelihood 
# Parameters : 
#     estimate Std. Error
# nu 3.9442458 0.39225713
# xi 0.7493448 0.04266034
# Fixed parameters:
#        value
# mean     0
# sd       1
# Loglikelihood:  -653.573   AIC:  1311.146   BIC:  1319.575 
# Correlation matrix:
#   nu         xi
# nu  1.0000000 -0.2475059
# xi -0.2475059  1.0000000

# Alla fine, consideriamo il test Goodness of fit.
# The Kolmogorov-Smirnov test in the library *stats*
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="psstd", mean=m, sd=sd, nu=df, xi=xi, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.069458, p-value = 0.01606
# alternative hypothesis: two-sided
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student asimmetrica generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# Cramer-von Mises test of goodness-of-fit
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "psstd", mean=m, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 4, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# omega2 = 0.84238, p-value = 0.005772
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "psstd", mean=m, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 4, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# An = 4.3451, p-value = 0.005929
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_garch1_q2_p2_res
num_lags <- 7                   # Setting the lag parameter for the test.
Xt_t_student_asymmetric_garch1_q2_p2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_garch1_q2_p2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_t_student_asymmetric_garch1_q2_p2_res   
Xt_t_student_asymmetric_garch1_q2_p2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_garch1_q2_p2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
# Si ha un p-value di 0.002564 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità
#
# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q2_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q2_p2)
show(Xt_t_student_asymmetric_garch1_q2_p2_w)
# Si ha un p-value di 0.01056 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_asymmetric_garch1_q2_p2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_asymmetric_garch1_q2_p2_lb)
# Nella forma estesa del test di Ljung-Box si ha presenza di autocorrelazione

modello[['simulazione']][['garch_q2_p2']][['asimmetrico']][['1']] <- append(modello[['simulazione']][['garch_q2_p2']][['asimmetrico']][['1']], 
                                                                            list('lm'=Xt_t_student_asymmetric_garch1_q2_p2_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                                 'Cullen-Frey'=Xt_t_student_asymmetric_garch1_q2_p2_cf, 
                                                                                 'Breusch-Pagan'=Xt_t_student_asymmetric_garch1_q2_p2_bp, 'White'=Xt_t_student_asymmetric_garch1_q2_p2_w, 
                                                                                 'Ljiung-Box'=Xt_t_student_asymmetric_garch1_q2_p2_lb, 'Dickey-Fuller'=Xt_t_student_asymmetric_garch1_q2_p2_adf, 
                                                                                 'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_garch1_q2_p2_kpss, 'Shapiro-Wilk'=Xt_t_student_asymmetric_garch1_q2_p2_sw,   
                                                                                 'Generalized Distribution'=fitdist_test))

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
# -1.381282 -1.918875 -1.007438  
set.seed(123)
kurt <- DescTools::Kurt(Xt_t_student_asymmetric_garch1_q2_p2_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#  kurt   lwr.ci   upr.ci 
# 5.605969 3.522017 8.679813

Xt_t_student_asymmetric_garch1_q2_p2_cf <- list()
# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, method= "sample", boot=2000)
Xt_t_student_asymmetric_garch1_q2_p2_cf <- append(Xt_t_student_asymmetric_garch1_q2_p2_cf, list(cf)) 
show(cf)
#summary statistics
# ------
# min:  -4.644343   max:  2.05256 
# median:  0.05762716 
# mean:  -0.002640901 
# sample sd:  0.7373117 
# sample skewness:  -1.459215 
# sample kurtosis:  8.735853 
# Da Cullen-Frey, la serie potrebbe seguire una distribuzione student.

# Applichiamo la standardizzazione
# Calcolo dei residui standardizzati
y <- Xt_t_student_asymmetric_garch1_q2_p2_res
show(c(mean(y),var(y)))
# [1] -3.253691e-18  5.438591e-01
z_st <- as.numeric((1/sd(y))*(y-mean(y))) # We standardize the residuals of the GARCH model.
show(c(mean(z_st),var(z_st)))
# [1] -1.412065e-18  1.000000e+00

# Cullen-Frey
options(repr.plot.width = 10, repr.plot.height = 6)
cf <- descdist(Xt, discrete=FALSE, method= "sample", boot=2000)
Xt_t_student_asymmetric_garch1_q2_p2_cf <- append(Xt_t_student_asymmetric_garch1_q2_p2_cf, list(cf)) 
show(cf)
# summary statistics
# ------
# min:   -4.644343   max:  2.05256 
# median:  0.05762716 
# mean:  -0.002640901 
# sample sd:  0.7373117 
# sample skewness:  -1.459215 
# sample kurtosis:  8.735853 
# Da Cullen-Frey, la serie potrebbe seguire una distribuzione esponenziale, gamma o beta.

fitdist_test <- list()
# Distribuzione student asimmetrica generalizzata
fitdist_t_ls <- fitdistrplus::fitdist(z_st, "sstd", start=list(nu=3.2, xi=1.5), fix.arg=list(mean=0, sd=1), method= "mle")
fitdist_test[["gstudent"]][["gstudent"]] <- fitdist_t_ls
summary(fitdist_t_ls)
# Fitting of the distribution ' std  ' by maximum likelihood 
# Parameters : 
#     estimate Std. Error
# nu 3.4035510 0.26918210
# xi 0.8583075 0.04212473
# Fixed parameters:
#        value
# mean     0
# sd       1
# Loglikelihood:  -648.5926   AIC:  1301.185   BIC:  1309.614 
# Correlation matrix:
#        nu         xi
# nu  1.0000000 -0.2503397
# xi -0.2503397  1.0000000

# Alla fine, consideriamo il test Goodness of fit.
# The Kolmogorov-Smirnov test in the library *stats*
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
KS_z_st_t_ls <- stats::ks.test(z_st, y="psstd", mean=m, sd=sd, nu=df, xi=xi, alternative= "two.sided")
fitdist_test[["gstudent"]][["Kolmogorov-Smirnov"]] <- KS_z_st_t_ls
show(KS_z_st_t_ls)
# Asymptotic one-sample Kolmogorov-Smirnov test
# data:  z_st
# D = 0.057974, p-value = 0.0694
# alternative hypothesis: two-sided
# Ma non possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student asimmetrica generalizzata, quindi,
# possiamo affermare che i residui seguono la distribuzione specificata.
#

# Cramer-von Mises test of goodness-of-fit
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
CVM_z_st_t_ls <- goftest::cvm.test(z_st, null= "psstd", mean=m, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Cramer-Von Mises"]] <- CVM_z_st_t_ls
show(CVM_z_st_t_ls)
# Cramer-von Mises test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 3, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# omega2 =0.61931, p-value = 0.02002
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata.

# The Anderson-Darling test in the library *goftest*.
# Setting
m <- 0
sd <- 1
df <- round(fitdist_t_ls[['estimate']][['nu']])
xi <- round(fitdist_t_ls[['estimate']][['xi']])
AD_z_st_t_ls <- goftest::ad.test(z_st, null = "psstd", mean=m, sd=sd, nu=df, xi=xi, estimated=FALSE)
fitdist_test[["gstudent"]][["Anderson-Darling"]] <- AD_z_st_t_ls
show(AD_z_st_t_ls)
# Anderson-Darling test of goodness-of-fit
# Null hypothesis: distribution ‘psstd’
# with parameters mean = 0, sd = 1, nu = 3, xi = 1
# Parameters assumed to be fixed
# data:  z_st
# An =3.5981, p-value = 0.01375
# Ma possiamo rigettare l'ipotesi nulla che i residui standardizzati hanno una distribuzione Student generalizzata, quindi,
# possiamo affermare che i residui non seguono la distribuzione specificata

# La serie è stata costruita per essere stazionaria, ma eseguiamo i test ADF e KPSS per verificare. 
# ADF Test
y <- Xt_t_student_asymmetric_garch1_q2_p2_res
num_lags <- 7                   # Setting the lag parameter for the test.
Xt_t_student_asymmetric_garch1_q2_p2_adf <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Xt_t_student_asymmetric_garch1_q2_p2_adf) 
# Il valore della statistica del test è significativamente inferiore al valore critico 
# al livello di significatività del 1%. Di conseguenza, possiamo respingere l'ipotesi 
# nulla e concludere che la serie temporale è stazionaria.
#
# KPSS Test
y <- Xt_t_student_asymmetric_garch1_q2_p2_res   
Xt_t_student_asymmetric_garch1_q2_p2_kpss <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Xt_t_student_asymmetric_garch1_q2_p2_kpss) 
# Il valore del test statistico è minore per ogni livello di significatività, pertanto possiamo
# respingere l'ipotesi di stazionarietà e concludere che la serie temporale è non stazionaria 
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
# Si ha un p-value di 5.793e-09 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità
#
# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q2_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q2_p2)
show(Xt_t_student_asymmetric_garch1_q2_p2_w)
# Si ha un p-value di 1.928e-08 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# X-squared =0.88367, df = 1, p-value = 0.3472
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 7  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(10, T/4)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_t_student_asymmetric_garch1_q2_p2_lb <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_t_student_asymmetric_garch1_q2_p2_lb)
# Nella forma estesa del test di Ljung-Box si ha presenza di autocorrelazione

modello[['stimati']][['garch_q2_p2']] <- append(modello[['stimati']][['garch_q2_p2']], 
                                                list('asimmetrico'=list('Xt'=Xt_t_student_asymmetric_garch1_q2_p2_new, 'a0'=a0, 'a1'=aq[1], 'a2'=aq[2], 'b1'=bp[1], 'b2'=bp[2], 'q'=q, 'p'=p,
                                                                        'stazionarietà'=stazionaietà, 'lm'=Xt_t_student_asymmetric_garch1_q2_p2_lm, 'skew'=skew, 'kurt'=kurt, 
                                                                        'Cullen-Frey'=Xt_t_student_asymmetric_garch1_q2_p2_cf, 
                                                                        'Breusch-Pagan'=Xt_t_student_asymmetric_garch1_q2_p2_bp, 'White'=Xt_t_student_asymmetric_garch1_q2_p2_w, 
                                                                        'Ljiung-Box'=Xt_t_student_asymmetric_garch1_q2_p2_lb, 'Dickey-Fuller'=Xt_t_student_asymmetric_garch1_q2_p2_adf, 
                                                                        'Kwiatowski-Phillips-Schmidt-Shin'=Xt_t_student_asymmetric_garch1_q2_p2_kpss, 'Shapiro-Wilk'=Xt_t_student_asymmetric_garch1_q2_p2_sw,         
                                                                        'Generalized Distribution'=fitdist_test)))

# Con i nuovi parametri stimati, rispetto al modello precedente, si ha un modello Garch(2,2)
# con una distribuzione t-student asimmetrica che ha evidenza di eteroschedasticità nel modello
# e assenza di autocorrelazione

#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################
