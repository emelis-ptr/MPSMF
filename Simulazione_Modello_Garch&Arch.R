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
hist_dist_symmetric2 <- ggplot(data.frame(value = dist_t_student_symmetric13), aes(x = value)) + 
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
dist_t_student_asymmetric <- rast(n = n, mu = 0, s = 1, alpha = 0.9, nu1 = 5, nu2 = Inf, pars = NULL)
set.seed(20)
dist_t_student_asymmetric1 <- rast(n = n, mu = 0, s = 1, alpha = 0.9, nu1 = 5, nu2 = Inf, pars = NULL)
set.seed(30)
dist_t_student_asymmetric2 <- rast(n = n, mu = 0, s = 1, alpha = 0.9, nu1 = 5, nu2 = Inf, pars = NULL)

type_dist = "Asymmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Histogram of ", .(n) ," samples generated from the ", .(type_dist))))

subtitle_content <- bquote(paste("First sample of the ", .(type_dist)))
# creo il primo plot dei campioni generati dalla distribuzione t-student asimmetrica
hist_dist_asymmetric <- ggplot(data.frame(value = dist_t_student_asymmetric), aes(x = value)) + 
  geom_histogram(binwidth = 0.2, alpha = 0.7) +
  labs(subtitle=subtitle_content, caption=" ") +
  xlab("Samples") + ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

subtitle_content <- bquote(paste("Second sample of the ", .(type_dist)))
# creo il secondo plot dei campioni generati dalla distribuzione t-student asimmetrica
hist_dist_asymmetric1 <- ggplot(data.frame(value = dist_t_student_asymmetric1), aes(x = value)) + 
  geom_histogram(binwidth = 0.2, alpha = 0.7) +
  labs(subtitle=subtitle_content, caption=" ") +
  xlab("Samples") + ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

subtitle_content <- bquote(paste("Third sample of the ", .(type_dist)))
# creo il terzo plot dei campioni generati dalla distribuzione t-student asimmetrica
hist_dist_asymmetric2 <- ggplot(data.frame(value = dist_t_student_asymmetric2), aes(x = value)) + 
  geom_histogram(binwidth = 0.2, alpha = 0.7) +
  #ggtitle("\n\n") +
  labs(subtitle=subtitle_content, caption=author_content) +
  xlab("Samples") + ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_asymmetric <- plot_grid(hist_dist_asymmetric, hist_dist_asymmetric1, hist_dist_asymmetric2, ncol = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
final_plot_dist_t_student_asymmetric <- grid.arrange(title_content_gtable, plots_dist_asymmetric, ncol = 1, heights = c(0.2, 1))

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
sigmasquaredW <- var(dist_t_student_symmetric13)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_symmetric_arch3_q1 <- model_arch(a0, a1, X0, dist_t_student_symmetric13, q)

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
sigmasquaredW <- var(dist_t_student_asymmetric)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_asymmetric_arch_q1 <- model_arch(a0, a1, X0, dist_t_student_asymmetric, q)

modello[['simulazione']][['arch_q1']][['asimmetrico']] <- append(modello[['simulazione']][['arch_q1']][['simmetrico']], list('1'=list('Xt'=Xt_t_student_asymmetric_arch1_q1, 'stazionarietà'=a1*sigmasquaredW)))

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric1)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_asymmetric_arch2_q1 <- model_arch(a0, a1, X0, dist_t_student_asymmetric1, q)

modello[['simulazione']][['arch_q1']][['asimmetrico']] <- append(modello[['simulazione']][['arch_q1']][['asimmetrico']], list('2'=list('Xt'=Xt_t_student_asymmetric_arch2_q1, 'stazionarietà'=a1*sigmasquaredW)))

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric2)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_asymmetric_arch3_q1 <- model_arch(a0, a1, X0, dist_t_student_asymmetric2, q)

modello[['simulazione']][['arch_q1']][['asimmetrico']] <- append(modello[['simulazione']][['arch_q1']][['asimmetrico']], list('3'=list('Xt'=Xt_t_student_asymmetric_arch3_q1, 'stazionarietà'=a1*sigmasquaredW)))

# creo il primo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric <- ggplot(data.frame(value = Xt_t_student_asymmetric_arch_q1, index = seq_along(Xt_t_student_asymmetric_arch_q1)), aes(x = index, y = value)) + 
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
plots_dist_t_student_asymmetric <- plot_grid(plot_t_student_asymmetric, plot_t_student_asymmetric1, plot_t_student_asymmetric2, nrow = 3)
plots <- plots_dist_t_student_asymmetric +
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

modello[['simulazione']][['garch_q1_p1']][['simmetrico']] <- append(modello[['simulazione']][['simmetrico']], list('1'=list('Xt'=Xt_t_student_symmetric_garch1_q1_p1, 'stazionarietà'=a1*sigmasquaredW)))

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_symmetric2)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_symmetric_garch2_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_symmetric2, q, p)

modello[['simulazione']][['garch_q1_p1']][['simmetrico']] <- append(modello[['simulazione']][['simmetrico']], list('2'=list('Xt'=Xt_t_student_symmetric_garch2_q1_p1, 'stazionarietà'=a1*sigmasquaredW)))

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_symmetric13)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_symmetric_garch3_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_symmetric13, q, p)

modello[['simulazione']][['garch_q1_p1']][['simmetrico']] <- append(modello[['simulazione']][['simmetrico']], list('3'=list('Xt'=Xt_t_student_symmetric_garch3_q1_p1, 'stazionarietà'=a1*sigmasquaredW)))

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
sigmasquaredW <- var(dist_t_student_asymmetric)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_asymmetric_garch1_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_asymmetric, q, p)

modello[['simulazione']][['garch_q1_p1']][['asimmetrico']] <- append(modello[['simulazione']][['asimmetrico']], list('1'=list('Xt'=Xt_t_student_asymmetric_garch1_q1_p1, 'stazionarietà'=a1*sigmasquaredW)))

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric1)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_asymmetric_garch2_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)

modello[['simulazione']][['garch_q1_p1']][['asimmetrico']] <- append(modello[['simulazione']][['asimmetrico']], list('2'=list('Xt'=Xt_t_student_asymmetric_garch2_q1_p1, 'stazionarietà'=a1*sigmasquaredW)))
# Terza traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric2)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_asymmetric_garch3_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_asymmetric2, q, p)

modello[['simulazione']][['garch_q1_p1']][['asimmetrico']] <- append(modello[['simulazione']][['asimmetrico']], list('3'=list('Xt'=Xt_t_student_asymmetric_garch3_q1_p1, 'stazionarietà'=a1*sigmasquaredW)))

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
plots_dist_t_student_asymmetric <- plot_grid(plot_t_student_asymmetric, plot_t_student_asymmetric1, plot_t_student_asymmetric2, nrow = 3)
plots <- plots_dist_t_student_asymmetric +
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
sigmasquaredW <- var(dist_t_student_symmetric13)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW + bp[1] + bp[2]))
Xt_t_student_symmetric_garch3_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_symmetric13, q, p)

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
sigmasquaredW <- var(dist_t_student_asymmetric)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW + bp[1] + bp[2]))
Xt_t_student_asymmetric_garch1_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_asymmetric, q, p)

modello[['simulazione']][['garch_q1_p2']][['asimmetrico']] <- append(modello[['simulazione']][['garch_q1_p2']][['asimmetrico']], list('1'=list('Xt'=Xt_t_student_asymmetric_garch1_q1_p2, 'stazionarietà'=a1*sigmasquaredW + bp[1] + bp[2])))

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric1)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW + bp[1] + bp[2]))
Xt_t_student_asymmetric_garch2_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)

modello[['simulazione']][['garch_q1_p2']][['asimmetrico']] <- append(modello[['simulazione']][['garch_q1_p2']][['asimmetrico']], list('2'=list('Xt'=Xt_t_student_asymmetric_garch2_q1_p2, 'stazionarietà'=a1*sigmasquaredW + bp[1] + bp[2])))

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric2)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW + bp[1] + bp[2]))
Xt_t_student_asymmetric_garch3_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_asymmetric2, q, p)

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
plots_dist_t_student_asymmetric <- plot_grid(plot_t_student_asymmetric, plot_t_student_asymmetric1, plot_t_student_asymmetric2, nrow = 3)
plots <- plots_dist_t_student_asymmetric +
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
sigmasquaredW <- var(dist_t_student_symmetric13)
print(paste("Verifico condizione di stazionerietà: ",(aq[1]+aq[2])*sigmasquaredW))
Xt_t_student_symmetric_arch3_q2 <- model_arch(a0, aq, X0, dist_t_student_symmetric13, q)

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
sigmasquaredW <- var(dist_t_student_asymmetric)
print(paste("Verifico condizione di stazionerietà: ",(aq[1]+aq[2])*sigmasquaredW))
Xt_t_student_asymmetric_arch1_q2 <- model_arch(a0, aq, X0, dist_t_student_asymmetric, q)

modello[['simulazione']][['arch_q2']][['asimmetrico']] <- append(modello[['simulazione']][['arch_q2']][['asimmetrico']], list('1'=list('Xt'=Xt_t_student_asymmetric_arch1_q2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW)))

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric1)
print(paste("Verifico condizione di stazionerietà: ",(aq[1]+aq[2])*sigmasquaredW))
Xt_t_student_asymmetric_arch2_q2 <- model_arch(a0, aq, X0, dist_t_student_asymmetric1, q)

modello[['simulazione']][['arch_q2']][['asimmetrico']] <- append(modello[['simulazione']][['arch_q2']][['asimmetrico']], list('2'=list('Xt'=Xt_t_student_asymmetric_arch2_q2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW)))

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric2)
print(paste("Verifico condizione di stazionerietà: ",(aq[1]+aq[2])*sigmasquaredW))
Xt_t_student_asymmetric_arch3_q2 <- model_arch(a0, aq, X0, dist_t_student_asymmetric2, q)

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
plots_dist_t_student_asymmetric <- plot_grid(plot_t_student_asymmetric, plot_t_student_asymmetric1, plot_t_student_asymmetric2, nrow = 3)
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
sigmasquaredW <- var(dist_t_student_asymmetric)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + b1))
Xt_t_student_asymmetric_garch1_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_asymmetric, q, p)

modello[['simulazione']][['garch_q2_p1']][['asimmetrico']] <- append(modello[['simulazione']][['garch_q2_p1']][['asimmetrico']], list('1'=list('Xt'=Xt_t_student_asymmetric_garch1_q2_p1, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + b1)))

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric1)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + b1))
Xt_t_student_asymmetric_garch2_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)

modello[['simulazione']][['garch_q2_p1']][['asimmetrico']] <- append(modello[['simulazione']][['garch_q2_p1']][['asimmetrico']], list('2'=list('Xt'=Xt_t_student_asymmetric_garch2_q2_p1, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + b1)))

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric2)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + b1))
Xt_t_student_asymmetric_garch3_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_asymmetric2, q, p)

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
plots_dist_t_student_asymmetric <- plot_grid(plot_t_student_asymmetric, plot_t_student_asymmetric1, plot_t_student_asymmetric2, nrow = 3)
plots <- plots_dist_t_student_asymmetric +
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
sigmasquaredW <- var(dist_t_student_symmetric13)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2]))
Xt_t_student_symmetric_garch3_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_symmetric13, q, p)
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
sigmasquaredW <- var(dist_t_student_asymmetric)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2]))
Xt_t_student_asymmetric_garch1_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_asymmetric, q, p)

modello[['simulazione']][['garch_q2_p2']][['asimmetrico']] <- append(modello[['simulazione']][['garch_q2_p2']][['asimmetrico']], list('1'=list('Xt'=Xt_t_student_asymmetric_garch1_q2_p2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])))

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric1)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2]))
Xt_t_student_asymmetric_garch2_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)

modello[['simulazione']][['garch_q2_p2']][['asimmetrico']] <- append(modello[['simulazione']][['garch_q2_p2']][['asimmetrico']], list('2'=list('Xt'=Xt_t_student_asymmetric_garch2_q2_p2, 'stazionarietà'=(aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])))

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric2)
print(paste("Verifico condizione di stazionerietà: ", (aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2]))
Xt_t_student_asymmetric_garch3_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_asymmetric2, q, p)

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
plots_dist_t_student_asymmetric <- plot_grid(plot_t_student_asymmetric, plot_t_student_asymmetric1, plot_t_student_asymmetric2, nrow = 3)
plots <- plots_dist_t_student_asymmetric +
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

Non studiamo i casi di:
- Stazionarietà**: un modello di predizione con residui stazionari garantisce che le previsioni future
    siano affidabili e non influenzate da fluttuazioni casuali o tendenze temporal;
- Gaussianità: se i residui seguono una distribuzione normale;

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
y1_col <- bquote("Monthly Deaths")
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
skew <- DescTools::Skew(Xt_normal_arch1_q1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
skew
#       skew      lwr.ci      upr.ci 
# -0.08678682 -0.24846259  0.04388842 
kurt <- DescTools::Kurt(Xt_normal_arch1_q1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
kurt
#      kurt     lwr.ci     upr.ci 
# -0.1544153 -0.3911550  0.2348151 

# Cullen-Frey
cf <- descdist(Xt, discrete=FALSE, boot=500)

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

plot(Xt_normal_arch1_q1_lm,1) # Residuals vs Fitted
plot(Xt_normal_arch1_q1_lm,2) # Q-Q Residuals
plot(Xt_normal_arch1_q1_lm,3) # Scale-location

# Omoschedasticità
# Test BREUSCH-PAGAN sui residui del modello lineare
t <-1:length(Xt)
Xt_normal_arch1_q1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = FALSE, data=df_Xt_normal_arch1_q1)
show(Xt_normal_arch1_q1_bp)
# Si ha un p-value di 0.00615 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_arch1_q1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = FALSE, data=df_Xt_normal_arch1_q1)
show(Xt_normal_arch1_q1_w)
# Si ha un p-value di 0.02191 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
n_pars <- 4  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
Xt_normal_arch1_q1_bt <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
show(Xt_normal_arch1_q1_bt)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag considerati.
# I risultati indicano assenza di autocorrelazione nei resisui.

modello[['simulazione']][['arch_q1']][['normale']][['1']] <- append(modello[['simulazione']][['arch_q1']][['normale']][['1']], list('lm'=Xt_normal_arch_q1_lm, 'skew'=skew, 'kurt'=kurt, 'cullen-frey'=cf, 'test-breusch-pagan'=Xt_normal_arch1_q1_bp, 'test-white'=Xt_normal_arch1_q1_w, 'test-ljiung-box'=Xt_normal_arch1_q1_bt))

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
y1_col <- bquote("Monthly Deaths")
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
kurt <- DescTools::Kurt(Xt_normal_arch1_q1_res, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
show(kurt)
#      kurt      lwr.ci      upr.ci 
# -0.32278623 -0.47849072 -0.09394918

# Cullen-Frey
cf <- descdist(Xt, discrete=FALSE, boot=500)

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
n_pars <- 4  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
lbt <- LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag considerati.

# I risultati indicano assenza di autocorrelazione nei resisui.

modello[['stimati']] <- append(modello[['stimati']], list('arch_q1'=list('normal'=list('Xt'=Xt_normal_arch_q1_new, 'a0'=a0, 'a1'=a1, 'q'=q, 'stazionarietà'=stazionaietà, 'lm'=Xt_normal_arch_q1_lm, 'skew'=skew, 'kurt'=kurt, 'cullen-frey'=cf, 'test-breusch-pagan'=Xt_normal_arch1_q1_bp, 'test-white'=Xt_normal_arch1_q1_w, 'test-ljiung-box'=Xt_normal_arch1_q1_bt))))

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
y_name <- bquote("Monthly Deaths")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Monthly Deaths")
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
DescTools::Skew(Xt_t_student_symmetric_arch1_q1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
#  skew          lwr.ci      upr.ci 
# 0.19494181 -0.08884189  0.52093593 
DescTools::Kurt(Xt_t_student_symmetric_arch1_q1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
#      kurt      lwr.ci      upr.ci 
# 1.861238 1.276939 2.867688

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
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag considerati.

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
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
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
y_name <- bquote("Monthly Deaths")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Monthly Deaths")
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
DescTools::Skew(Xt_t_student_symmetric_arch1_q1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
#  skew       lwr.ci      upr.ci 
# 0.1949418 -0.1103381  0.4706468 
DescTools::Kurt(Xt_t_student_symmetric_arch1_q1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
#  kurt      lwr.ci   upr.ci 
# 1.861238 1.196812 2.765069 

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
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag considerati.

# In questo modello Arch(1) con una distribuzione t-student simmetrica si ha evidenza di eteroschedasticità nella serie
# e assenza di autocorrelazione nei residui.
##########################################

##### DSITRIBUZIONE T-STUDENT ASIMMETRICA

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello ARCH(1)
Xt <- Xt_t_student_asymmetric_arch_q1
df_Xt_t_student_asymmetric_arch_q1 <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df<- df_Xt_t_student_asymmetric_arch_q1
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
y_name <- bquote("Monthly Deaths")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Monthly Deaths")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_arch_q1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_arch_q1_sp)
# La regression line tende ad essere leggermente inclinata rispetto alla linea orizzontale, e 
# la LOESS corrisponde quasi esattamente alla regression line.

# Consideriamo un modello lineare
Xt_t_student_asymmetric_arch_q1_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_arch_q1)
summary(Xt_t_student_asymmetric_arch_q1_lm)
summary(Xt_t_student_asymmetric_arch_q1_lm$fitted.values)

Xt_t_student_asymmetric_arch_q1_res <- Xt_t_student_asymmetric_arch_q1_lm$residuals
# Calcoliamo la skew e la kurtosi
DescTools::Skew(Xt_t_student_asymmetric_arch_q1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
#  skew       lwr.ci      upr.ci 
# -1.734758 -2.031297 -1.516443 
DescTools::Kurt(Xt_t_student_asymmetric_arch_q1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
#  kurt      lwr.ci   upr.ci 
# 4.210131 3.348380 6.058005 

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_arch_q1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(1) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_asymmetric_arch_q1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_arch_q1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_arch_q1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Arch(1) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Square root of absolute residuals")
Xt_t_student_asymmetric_arch_q1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_arch_q1_sp)

plot(Xt_t_student_asymmetric_arch_q1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_arch_q1_lm,3) # Scale-location

#Determiniamo se la serie è eteroschedastico:
# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_arch_q1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_arch_q1)
show(Xt_t_student_asymmetric_arch_q1_bp)
# Si ha un p-value di 0.01372 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_arch_q1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_arch_q1)
show(Xt_t_student_asymmetric_arch_q1_w)
# Si ha un p-value di 0.04648 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_asymmetric_arch_q1_lm$residuals
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
y <- Xt_t_student_asymmetric_arch_q1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 12.319, df = 1, p-value = 0.0004484
# I risultati mostrano un p-value < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è presenza di correlazione nei lag 1 al 9
# e nel lag 13.

# In questo modello Arch(1) si ha presenza di eteroschedasticità e presenza di autocorrelazione.
# Quindi, proviamo a stimare i migliori parametri per il modello:
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,0)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
sigmasquaredW <- var(dist_t_student_asymmetric)
print(paste("Verifico condizione di stazionerietà: ", a1*sigmasquaredW))
Xt_t_student_asymmetric_arch_q1_new <- model_arch(a0, a1, X0, dist_t_student_asymmetric, q)

Xt <- Xt_t_student_asymmetric_arch_q1_new
df_Xt_t_student_asymmetric_arch_q1 <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df<- df_Xt_t_student_asymmetric_arch_q1
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
y_name <- bquote("Monthly Deaths")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Monthly Deaths")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_arch_q1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_arch_q1_sp)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_arch_q1_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_arch_q1)
summary(Xt_t_student_asymmetric_arch_q1_lm)
summary(Xt_t_student_asymmetric_arch_q1_lm$fitted.values)

Xt_t_student_asymmetric_arch_q1_res <- Xt_t_student_asymmetric_arch_q1_lm$residuals
# Calcoliamo la skew e la kurtosi
DescTools::Skew(Xt_t_student_asymmetric_arch_q1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
#  skew       lwr.ci      upr.ci 
# -1.734758 -2.010997 -1.530683
DescTools::Kurt(Xt_t_student_asymmetric_arch_q1, weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
#  kurt      lwr.ci   upr.ci 
# 4.210131 3.072269 5.932324 

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_arch_q1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(1) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
Xt_t_student_asymmetric_arch_q1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_arch_q1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_arch_q1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Arch(1) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
Xt_t_student_asymmetric_arch_q1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_arch_q1_sp)

#Determiniamo se la serie è eteroschedastico:
# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_arch_q1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_arch_q1)
show(Xt_t_student_asymmetric_arch_q1_bp)
# Si ha un p-value di 0.02001 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_arch_q1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_arch_q1)
show(Xt_t_student_asymmetric_arch_q1_w)
# Si ha un p-value di 0.06186 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Test Ljiung-box
y <- Xt_t_student_asymmetric_arch_q1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 2.8229, df = 1, p-value = 0.09293
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag.

# In questo modello ARCH(1) con una distribuzione t-student asimmetrica, con i parametri stimati,
# si ha un'assenza di correlazione e presenza di eteroschedasticità nel test di Breusch-Pagan, a 
# differenza del test di White in cui si ha presenza di omoschedasticità.
 
##########################################
##########################################

########## MODEL GARCH(1,1)

##### DISTRIBUZIONE NORMALE
# Consideriamo la seconda traiettoia con distribuzione normale di un modello GARCH(1,1)
Xt <- Xt_normal_garch2_q1_p1 
df_Xt_normal_garch1_q1_p1  <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df<- df_Xt_normal_garch1_q1_p1 
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
y_name <- bquote("Monthly Deaths")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Monthly Deaths")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_garch2_q1_p1 _sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch2_q1_p1 _sp)
# La regression line risulta essere orizzontale, quindi possiamo dedurre assenza di un trend e quindi il
# modello è stazionario. La LOESS oscilla leggermente intorno alla regression line.

# Consideriamo un modello lineare
Xt_normal_garch1_q1_p1 _lm <- lm(Xt~t, data=df_Xt_normal_garch1_q1_p1 )
summary(Xt_normal_garch1_q1_p1 _lm)
summary(Xt_normal_garch1_q1_p1 _lm$fitted.values)

Xt_normal_garch1_q1_p1 _res <- Xt_normal_garch1_q1_p1 _lm$residuals
# Calcoliamo la skew e la kurtosi
DescTools::Skew(Xt_normal_garch1_q1_p1 , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
#  skew       lwr.ci      upr.ci 
# -0.06223087 -0.21328905  0.06138098
DescTools::Kurt(Xt_normal_garch1_q1_p1 , weights=NULL, na.rm=TRUE, method=2, conf.level=0.80, ci.type= "bca", R=1000)
#  kurt      lwr.ci   upr.ci 
# -0.1692899 -0.3828177  0.1962899   

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch1_q1_p1 _res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_normal_garch1_q1_p1 _sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch1_q1_p1 _sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch1_q1_p1 _res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(1,1) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Square root of absolute residuals")
Xt_normal_garch1_q1_p1 _sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch1_q1_p1 _sp)

plot(Xt_normal_garch1_q1_p1 _lm,1) # Residuals vs Fitted
plot(Xt_normal_garch1_q1_p1 _lm,2) # Q-Q Residuals
plot(Xt_normal_garch1_q1_p1 _lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch1_q1_p1 _bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch1_q1_p1 )
show(Xt_normal_garch1_q1_p1 _bp)
# Si ha un p-value di 0.7758 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch1_q1_p1 _w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch1_q1_p1 )
show(Xt_normal_garch1_q1_p1 _w)
# Si ha un p-value di 0.9314 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_normal_garch1_q1_p1 _lm$residuals
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
y <- Xt_normal_garch1_q1_p1 _res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 0.20218, df = 1, p-value = 0.653

# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

# Questo modello Garch(1,1) con una distribuzione normale ha assenza di autocorrelazione
# ma presenza di omoschedasticità. 
# Proviamo a stimare i parametri che si adattano meglio al modello.
q <- 1
p <- 1
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal2)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_normal2)
print(paste("Verifico condizione di stazionerietà: ",a1*sigmasquaredW + b1))
Xt_normal_garch2_q1_p1 _new <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_normal2, q, p)

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(1,1)
Xt <- Xt_normal_garch1_q1_p1 _new
df_Xt_normal_garch1_q1_p1  <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df<- df_Xt_normal_garch1_q1_p1 
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
y_name <- bquote("Monthly Deaths")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Monthly Deaths")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_normal_garch2_q1_p1 _sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch2_q1_p1 _sp)
# La regression line risulta essere orizzontale, quindi possiamo dedurre assenza di un trend e quindi il
# modello è stazionario. La LOESS oscilla intorno alla regression line.

# Consideriamo un modello lineare
Xt_normal_garch1_q1_p1 _lm <- lm(Xt~t, data=df_Xt_normal_garch1_q1_p1 )
summary(Xt_normal_garch1_q1_p1 _lm)
summary(Xt_normal_garch1_q1_p1 _lm$fitted.values)

Xt_normal_garch1_q1_p1 _res <- Xt_normal_garch1_q1_p1 _lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_garch1_q1_p1 _res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_garch1_q1_p1 _res)                  # theoretical value 3.
# La skew è pari a -0.04261056; il suo valore è prossimo a zero, quindi la sua distribuzione 
# viene considerata approssimativamente simmetrica.
# La kurtosi è pari a 2.689033; questo indica che la distribuzione è leggermenete platykurtic.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch1_q1_p1 _res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_normal_garch1_q1_p1 _sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch1_q1_p1 _sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch1_q1_p1 _res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(1,1) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Square root of absolute residuals")
Xt_normal_garch1_q1_p1 _sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch1_q1_p1 _sp)

plot(Xt_normal_garch1_q1_p1 _lm,1) # Residuals vs Fitted
plot(Xt_normal_garch1_q1_p1 _lm,2) # Q-Q Residuals
plot(Xt_normal_garch1_q1_p1 _lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch1_q1_p1 _bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch1_q1_p1 )
show(Xt_normal_garch1_q1_p1 _bp)
# Si ha un p-value di 0.006036 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch1_q1_p1 _w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch1_q1_p1 )
show(Xt_normal_garch1_q1_p1 _w)
# Si ha un p-value di 0.023 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Plot of the autocorrelogram.
y <- Xt_normal_garch1_q1_p1 _lm$residuals
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
y <- Xt_normal_garch1_q1_p1 _res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 2.6795, df = 1, p-value = 0.1016
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag.

# In questo modello Garch(1,1) con distribuzione normale risulta essere eteroschedastico e con assenza
# di autocorrelazione con i parametri stimati.

##########################################

##### DISTRIBUZIONE T-STUDENT SIMMETRICA
# Consideriamo la terza traiettoia con distribuzione t-student simmetrica di un modello GARCH(1,1)
Xt <- Xt_t_student_symmetric_garch3_q1_p1
df_Xt_t_student_symmetric_garch1_q1_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df<- df_Xt_t_student_symmetric_garch1_q1_p1
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
y_name <- bquote("Monthly Deaths")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Monthly Deaths")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_garch1_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch1_q1_p1_sp)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch1_q1_p1_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch1_q1_p1)
summary(Xt_t_student_symmetric_garch1_q1_p1_lm)
summary(Xt_t_student_symmetric_garch1_q1_p1_lm$fitted.values)

Xt_t_student_symmetric_garch1_q1_p1_res <- Xt_t_student_symmetric_garch1_q1_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_garch1_q1_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_garch1_q1_p1_res)                  # theoretical value 3.
# La skew è pari a -0.01744386; il suo valore è negativo e la sua distribuzione è approssimativamente simmetrica.
# La kurtosi è pari a 4.204011; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch1_q1_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_symmetric_garch1_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch1_q1_p1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch1_q1_p1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(1,1) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Square root of absolute residuals")
Xt_t_student_symmetric_garch1_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch1_q1_p1_sp)

plot(Xt_t_student_symmetric_garch1_q1_p1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_garch1_q1_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch1_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch1_q1_p1)
show(Xt_t_student_symmetric_garch1_q1_p1_bp)
# Si ha un p-value di 0.1184 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch1_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch1_q1_p1)
show(Xt_t_student_symmetric_garch1_q1_p1_w)
# Si ha un p-value di 0.1875 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_garch1_q1_p1_lm$residuals
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
y <- Xt_t_student_symmetric_garch1_q1_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 2.932, df = 1, p-value = 0.08684
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione.

# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

# Il modello Garch(1,1) con distribuzione t-student simmetrica ha presenza di omoschedasticità.
# Dato che la serie è omoschedastico, proviamo a stimare i migliori parametri per il modello.
distribution <- dist_t_student_symmetric13
y <- c(0,distribution)
class(y)
head(y)
T <- length(y)
# 146
max_lag <- ceiling(min(10, T/4))   # Hyndman (for data without seasonality)
# We introduce some lists where to store the results of our procedure.
Xt_t_student_symmetric_garch1_q1_p1_ls <- list()
Xt_t_student_symmetric_garch1_q1_p1_model_ls <- list()
Xt_t_student_symmetric_garch1_q1_p1_LB_test_ls <- list()
Xt_t_student_symmetric_garch1_q1_p1_Ext_LB_test_ls <- list()
Xt_t_student_symmetric_garch1_q1_p1_HLB_test_ls <- list()
Xt_t_student_symmetric_garch1_q1_p1_err_war_mess <- list()
q_order_components <- vector(mode="list", length=1)
names(q_order_components) <- "q parameter" # Order of components
p_order_res_delay <- vector(mode="list", length=1)
names(p_order_res_delay) <- "p parameter" # Order of residuals delay
omega <- vector(mode="list", length=1)
names(omega) <- "omega parameter(a0)"
alpha1 <- vector(mode="list", length=1)
names(alpha1) <- "alpha1 parameter(a1)"
beta1 <- vector(mode="list", length=1)
names(beta1) <- "beta1 parameter(b1)"
loglik_val <- vector(mode="list", length=1)
names(loglik_val) <- "loglik value"
AIC_val <- vector(mode="list", length=1)
names(AIC_val) <- "AIC value"
AICc_val <- vector(mode="list", length=1)
names(AICc_val) <- "AICc value"
BIC_val <- vector(mode="list", length=1)
names(BIC_val) <- "BIC value"
stationarity <- vector(mode="list", length=1)
names(stationarity) <- "stationarity"
closeAllConnections()
cn <- 1 # Setting a counter
con <- file("Garch(1,1) model of a symmetric t-student distribution")
sink(con, append=TRUE, type=c("output", "message"), split=TRUE)
q = 1
p = 1
# Looping over parameter omega
for(a0 in seq(0.01, 0.1, by = 0.01)){
  # Looping over parameter alpha1
  for(a1 in seq(0.0001, 0.001, by = 0.0001)){
    # Looping over parameter alpha1
    for(b1 in seq(0.8, 1.0, by = 0.01)){
      # ERROR and WARNINGS HANDLING
      tryCatch({
        q_order_components[[1]] <- q
        p_order_res_delay[[1]] <- p
        omega[[1]] <- a0
        alpha1[[1]] <- a1
        beta1[[1]] <- b1
        sigmasquaredW <- var(distribution)
        stationarity[[1]] <- a1*sigmasquaredW + b1
        uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)), distribution.model= "std", fixed.pars = list(omega=a0, alpha1=a1, beta1=b1))
        Xt_t_student_symmetric_garch1_q1_p1_model_ls[[cn]] <- ugarchfit(spec = uspec, data = c(0,distribution))
        show(Xt_t_student_symmetric_garch1_q1_p1_model_ls[[cn]])
        
        k <- length(coef(Xt_t_student_symmetric_garch1_q1_p1_model_ls[[cn]])) + 1
        loglik_val[[1]] <- likelihood(Xt_t_student_symmetric_garch1_q1_p1_model_ls[[cn]])
        AIC_val[[1]] <- infocriteria(Xt_t_student_symmetric_garch1_q1_p1_model_ls[[cn]])
        AICc_val[[1]] <- AIC_val[[1]] +2*k*(k+1)/(n-k-1)
        BIC_val[[1]] <- AIC_val[[1]]+k*(log(n)-2)
        
        cat("\n")
        cat(blue("Garch parameter p = ",p,","), blue("Garch parameter q = ",q,","))
        cat("\n")
        cat(blue("AIC = ",round(AIC_val[[1]],3),","), blue("AICc = ",round(AICc_val[[1]],3),","), blue("BIC = ",round(BIC_val[[1]],3),","))
        cat("\n")
        
        LB_fitdf <- min(min(max_lag, k), max_lag-1)
        Xt_t_student_symmetric_garch1_q1_p1_LB_test_ls[[cn]] <- Box.test(residuals(Xt_t_student_symmetric_garch1_q1_p1_model_ls[[cn]]), 
                                                           lag=max_lag, fitdf=LB_fitdf, type = "Ljung-Box")
        show(Xt_t_student_symmetric_garch1_q1_p1_LB_test_ls[[cn]])
        Xt_t_student_symmetric_garch1_q1_p1_Ext_LB_test_ls[[cn]] <- LjungBoxTest(residuals(Xt_t_student_symmetric_garch1_q1_p1_model_ls[[cn]]),
                                                                   lag.max=max_lag,  k=k, StartLag=1, SquaredQ=FALSE)
        show(Xt_t_student_symmetric_garch1_q1_p1_Ext_LB_test_ls[[cn]])
        H_max_lag <-max(max_lag, k+3)
        Xt_t_student_symmetric_garch1_q1_p1_HLB_test_ls[[cn]] <- Box.test(residuals(Xt_t_student_symmetric_garch1_q1_p1_model_ls[[cn]]), 
                                                            lag=H_max_lag, fitdf=k, type = "Ljung-Box")
        show(Xt_t_student_symmetric_garch1_q1_p1_HLB_test_ls[[cn]])
        cat("  \n","  \n")
        Xt_t_student_symmetric_garch1_q1_p1_ls[[cn]]  <- list(c(q_order_components, p_order_res_delay, omega, alpha1, beta1, loglik_val, AIC_val, AICc_val, BIC_val, stationarity),
                                                Xt_t_student_symmetric_garch1_q1_p1_model_ls[[cn]],
                                                Xt_t_student_symmetric_garch1_q1_p1_LB_test_ls[[cn]],
                                                Xt_t_student_symmetric_garch1_q1_p1_HLB_test_ls[[cn]])
        cn <- cn+1
        cat("  \n","  \n")
      }, error = function(e){
        cat(red(sprintf("caught error: %s", e)))
        cat(red("Garch parameter p = ", p,","), red("Garch parameter q = ", q,","))
        cat("\n")
        traceback(1, max.lines = 1)
        cat("  \n","  \n")
        Xt_t_student_symmetric_garch1_q1_p1_err_war_mess <<- c(Xt_t_student_symmetric_garch1_q1_p1_err_war_mess, list(c(e, paste("ERROR!... for counter cn = ",cn,",... Garch parameters p = ",p,", q = ",q,"."))))
        Xt_t_student_symmetric_garch1_q1_p1_ls[[cn]] <<- list(c(e, paste("ERROR!... for counter cn = ",cn,",... Garch parameters p = ",p,", q = ",q,".")))
        cn <<- cn+1
      }, warning = function(w){
        cat(yellow(sprintf("caught warning: %s", w)))
        cat(yellow("Garch parameter p = ",p,","), yellow("Garch parameter q = ",q,","))
        cat("\n")
        traceback(1, max.lines = 1)
        cat("  \n","  \n")
        Xt_t_student_symmetric_garch1_q1_p1_err_war_mess <<- c(Xt_t_student_symmetric_garch1_q1_p1_err_war_mess, list(c(w, paste("WARNING!... for counter cn = ",cn,",... Garch parameters p = ",p,", q = ",q,"."))))
        Xt_t_student_symmetric_garch1_q1_p1_ls[[cn]] <<- list(c(w, paste("WARNING!... for counter cn = ",cn,",... Arch parameters p = ",p,", q = ",q,".")))
        cn <<- cn+1
      }
      )
    } 
  }
}
sink()
closeAllConnections()

num_estrazioni <- 72
# num_estrazioni <- length(Xt_t_student_symmetric_garch1_q1_p1_ls)
# Estraiamo alcune informazioni dalla lista creata precedentemente
Xt_t_student_symmetric_garch1_q1_p1_loglik_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_symmetric_garch1_q1_p1_ls[[k]])==4) 
{Xt_t_student_symmetric_garch1_q1_p1_loglik_values[k]<- Xt_t_student_symmetric_garch1_q1_p1_ls[[k]][[1]][["loglik value"]]}}
show(Xt_t_student_symmetric_garch1_q1_p1_loglik_values)
#
Xt_t_student_symmetric_garch1_q1_p1_AICc_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_symmetric_garch1_q1_p1_ls[[k]])==4) 
{Xt_t_student_symmetric_garch1_q1_p1_AICc_values[k]<- Xt_t_student_symmetric_garch1_q1_p1_ls[[k]][[1]][["AICc value"]]}}
show(Xt_t_student_symmetric_garch1_q1_p1_AICc_values)
#
Xt_t_student_symmetric_garch1_q1_p1_BIC_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_symmetric_garch1_q1_p1_ls[[k]])==4) 
{Xt_t_student_symmetric_garch1_q1_p1_BIC_values[k]<- Xt_t_student_symmetric_garch1_q1_p1_ls[[k]][[1]][["BIC value"]]}}
show(Xt_t_student_symmetric_garch1_q1_p1_BIC_values)
#
Xt_t_student_symmetric_garch1_q1_p1_AIC_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_symmetric_garch1_q1_p1_ls[[k]])==4) 
{Xt_t_student_symmetric_garch1_q1_p1_AIC_values[k]<- Xt_t_student_symmetric_garch1_q1_p1_ls[[k]][[1]][["AIC value"]]}}
show(Xt_t_student_symmetric_garch1_q1_p1_AIC_values)
#
Xt_t_student_symmetric_garch1_q1_p1_omega_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_symmetric_garch1_q1_p1_ls[[k]])==4) 
{Xt_t_student_symmetric_garch1_q1_p1_omega_params[k]<- Xt_t_student_symmetric_garch1_q1_p1_ls[[k]][[1]][["omega parameter(a0)"]]}}
show(Xt_t_student_symmetric_garch1_q1_p1_omega_params)
#
Xt_t_student_symmetric_garch1_q1_p1_alpha1_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_symmetric_garch1_q1_p1_ls[[k]])==4) 
{Xt_t_student_symmetric_garch1_q1_p1_alpha1_params[k]<- Xt_t_student_symmetric_garch1_q1_p1_ls[[k]][[1]][["alpha1 parameter(a1)"]]} }
show(Xt_t_student_symmetric_garch1_q1_p1_alpha1_params)
#
Xt_t_student_symmetric_garch1_q1_p1_beta1_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_symmetric_garch1_q1_p1_ls[[k]])==4) 
{Xt_t_student_symmetric_garch1_q1_p1_beta1_params[k]<- Xt_t_student_symmetric_garch1_q1_p1_ls[[k]][[1]][["beta1 parameter(b1)"]]} }
show(Xt_t_student_symmetric_garch1_q1_p1_beta1_params)
#
Xt_t_student_symmetric_garch1_q1_p1_LB_p_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_symmetric_garch1_q1_p1_ls[[k]])==4) 
{Xt_t_student_symmetric_garch1_q1_p1_LB_p_values[k]<- Xt_t_student_symmetric_garch1_q1_p1_ls[[k]][[3]][["p.value"]]}}
show(Xt_t_student_symmetric_garch1_q1_p1_LB_p_values)
#
Xt_t_student_symmetric_garch1_q1_p1_HLB_p_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_symmetric_garch1_q1_p1_ls[[k]])==4) 
{Xt_t_student_symmetric_garch1_q1_p1_HLB_p_values[k]<- Xt_t_student_symmetric_garch1_q1_p1_ls[[k]][[4]][["p.value"]]}}
show(Xt_t_student_symmetric_garch1_q1_p1_HLB_p_values)
#
Xt_t_student_symmetric_garch1_q1_p1_stationarity <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_symmetric_garch1_q1_p1_ls[[k]])==4) 
{Xt_t_student_symmetric_garch1_q1_p1_stationarity[k]<- Xt_t_student_symmetric_garch1_q1_p1_ls[[k]][[1]][["stationarity"]]}}
show(Xt_t_student_symmetric_garch1_q1_p1_stationarity)
# We store the extracted information in a data frame
Xt_t_student_symmetric_garch1_q1_p1_df <- data.frame(Index=1:num_estrazioni,
                                       loglik_value=Xt_t_student_symmetric_garch1_q1_p1_loglik_values,
                                       AICc_value=Xt_t_student_symmetric_garch1_q1_p1_AICc_values, 
                                       BIC_value=Xt_t_student_symmetric_garch1_q1_p1_BIC_values,
                                       AIC_value=Xt_t_student_symmetric_garch1_q1_p1_AIC_values,
                                       a0_param=Xt_t_student_symmetric_garch1_q1_p1_omega_params,
                                       a1_param=Xt_t_student_symmetric_garch1_q1_p1_alpha1_params,
                                       b1_param=Xt_t_student_symmetric_garch1_q1_p1_beta1_params,
                                       LB_p_value=Xt_t_student_symmetric_garch1_q1_p1_LB_p_values,
                                       HLB_p_value=Xt_t_student_symmetric_garch1_q1_p1_HLB_p_values,
                                       stationarity=Xt_t_student_symmetric_garch1_q1_p1_stationarity)
# rimuoviamo i parametri che non rispettano la condizione di stazionarietà
Xt_t_student_symmetric_garch1_q1_p1_df <- Xt_t_student_symmetric_garch1_q1_p1_df[Xt_t_student_symmetric_garch1_q1_p1_df$stationarity <= 1, ]
head(Xt_t_student_symmetric_garch1_q1_p1_df, 20)
tail(Xt_t_student_symmetric_garch1_q1_p1_df,10)

# We sort the models according to increasing AICc values.
Xt_t_student_symmetric_garch1_q1_p1_AICc_sort_df <- Xt_t_student_symmetric_garch1_q1_p1_df[order(Xt_t_student_symmetric_garch1_q1_p1_df$AICc_value),]
rownames(Xt_t_student_symmetric_garch1_q1_p1_AICc_sort_df) <- NULL
head(Xt_t_student_symmetric_garch1_q1_p1_AICc_sort_df,20)
# We sort the models according to increasing BIC values.
Xt_t_student_symmetric_garch1_q1_p1_BIC_sort_df <- Xt_t_student_symmetric_garch1_q1_p1_df[order(Xt_t_student_symmetric_garch1_q1_p1_df$BIC_value),]
rownames(Xt_t_student_symmetric_garch1_q1_p1_BIC_sort_df) <- NULL
head(Xt_t_student_symmetric_garch1_q1_p1_BIC_sort_df,20)
# We sort the models according to increasing AIC values.
Xt_t_student_symmetric_garch1_q1_p1_AIC_sort_df <- Xt_t_student_symmetric_garch1_q1_p1_df[order(Xt_t_student_symmetric_garch1_q1_p1_df$AIC_value),]
rownames(Xt_t_student_symmetric_garch1_q1_p1_AIC_sort_df) <- NULL
head(Xt_t_student_symmetric_garch1_q1_p1_AIC_sort_df,20)

# Costruiamo il nuovo modello con i parametri stimati
a0 <- 0.01
a1 <- 4e-04
b1 <- 0.99
Xt_t_student_symmetric_garch1_q1_p1_new <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_symmetric13, q, p)

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello GARCH(1,1)
Xt <- Xt_t_student_symmetric_garch1_q1_p1_new
df_Xt_t_student_symmetric_garch1_q1_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df<- df_Xt_t_student_symmetric_garch1_q1_p1
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
y_name <- bquote("Monthly Deaths")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Monthly Deaths")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_garch1_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch1_q1_p1_sp)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch1_q1_p1_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch1_q1_p1)
summary(Xt_t_student_symmetric_garch1_q1_p1_lm)
summary(Xt_t_student_symmetric_garch1_q1_p1_lm$fitted.values)

Xt_t_student_symmetric_garch1_q1_p1_res <- Xt_t_student_symmetric_garch1_q1_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_garch1_q1_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_garch1_q1_p1_res)                  # theoretical value 3.
# La skew è pari a -0.002286079; la sua distribuzione è approssimativamente simmetrica.
# La kurtosi è pari a 3.648979; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch1_q1_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_symmetric_garch1_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch1_q1_p1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch1_q1_p1_res)
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
Xt_t_student_symmetric_garch1_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch1_q1_p1_sp)

plot(Xt_t_student_symmetric_garch1_q1_p1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_garch1_q1_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch1_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch1_q1_p1)
show(Xt_t_student_symmetric_garch1_q1_p1_bp)
# Si ha un p-value di 0.04765 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch1_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch1_q1_p1)
show(Xt_t_student_symmetric_garch1_q1_p1_w)
# Si ha un p-value di 0.006325 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_garch1_q1_p1_lm$residuals
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
y <- Xt_t_student_symmetric_garch1_q1_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 1.221, df = 1, p-value = 0.2692
# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag.

# In questo modello Garch(1,1) con una distribuzione t-student simmetrica, con i nuovi parametri stimati,
# si ha evidenza di eteroschedasticità e assenza di autocorrelazione.

##########################################

# Consideriamo la seconda traiettoia con distribuzione t-student asimmetrica di un modello GARCH(1,1)
Xt <- Xt_t_student_asymmetric_garch2_q1_p1
df_Xt_t_student_asymmetric_garch1_q1_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df<- df_Xt_t_student_asymmetric_garch1_q1_p1
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
y_name <- bquote("Monthly Deaths")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Monthly Deaths")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_garch1_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch1_q1_p1_sp)
# La regression line risulta leggermente inclinata, mentre la LOESS oscilla leggermente intorno alla regression line.

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch1_q1_p1_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch1_q1_p1)
summary(Xt_t_student_asymmetric_garch1_q1_p1_lm)
summary(Xt_t_student_asymmetric_garch1_q1_p1_lm$fitted.values)

Xt_t_student_asymmetric_garch1_q1_p1_res <- Xt_t_student_asymmetric_garch1_q1_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_garch1_q1_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_garch1_q1_p1_res)                  # theoretical value 3.
# La skew è pari a -1.732549; il suo valore è negativo e la sua distribuzione è
# asimmetrica verso sinistra con una coda lunga negativa.
# La kurtosi è pari a 7.407139; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch1_q1_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_asymmetric_garch1_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch1_q1_p1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch1_q1_p1_res)
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
Xt_t_student_asymmetric_garch1_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch1_q1_p1_sp)

plot(Xt_t_student_asymmetric_garch1_q1_p1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_garch1_q1_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q1_p1)
show(Xt_t_student_asymmetric_garch1_q1_p1_bp)
# Si ha un p-value di 0.02387 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q1_p1)
show(Xt_t_student_asymmetric_garch1_q1_p1_w)
# Si ha un p-value di 0.03169 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_asymmetric_garch1_q1_p1_lm$residuals
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
y <- Xt_t_student_asymmetric_garch1_q1_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 12.535, df = 1, p-value = 0.0003994

# I risultati hanno un p-value < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

# Questo modello Garch(1,1) con una distribuzione normale ha presenza di autocorrelazione
# e presenza di eteroschedasticità. 
# Quindi, proviamo a stimare i parametri che si adattano meglio al modello.
distribution <- dist_t_student_asymmetric1 
y <- c(0,distribution)
class(y)
head(y)
T <- length(y)
# 146
max_lag <- ceiling(min(10, T/4))   # Hyndman (for data without seasonality)
# We introduce some lists where to store the results of our procedure.
Xt_t_student_asymmetric_garch1_q1_p1_ls <- list()
Xt_t_student_asymmetric_garch1_q1_p1_model_ls <- list()
Xt_t_student_asymmetric_garch1_q1_p1_LB_test_ls <- list()
Xt_t_student_asymmetric_garch1_q1_p1_Ext_LB_test_ls <- list()
Xt_t_student_asymmetric_garch1_q1_p1_HLB_test_ls <- list()
Xt_t_student_asymmetric_garch1_q1_p1_err_war_mess <- list()
q_order_components <- vector(mode="list", length=1)
names(q_order_components) <- "q parameter" # Order of components
p_order_res_delay <- vector(mode="list", length=1)
names(p_order_res_delay) <- "p parameter" # Order of residuals delay
omega <- vector(mode="list", length=1)
names(omega) <- "omega parameter(a0)"
alpha1 <- vector(mode="list", length=1)
names(alpha1) <- "alpha1 parameter(a1)"
beta1 <- vector(mode="list", length=1)
names(beta1) <- "beta1 parameter(b1)"
loglik_val <- vector(mode="list", length=1)
names(loglik_val) <- "loglik value"
AIC_val <- vector(mode="list", length=1)
names(AIC_val) <- "AIC value"
AICc_val <- vector(mode="list", length=1)
names(AICc_val) <- "AICc value"
BIC_val <- vector(mode="list", length=1)
names(BIC_val) <- "BIC value"
stationarity <- vector(mode="list", length=1)
names(stationarity) <- "stationarity"
closeAllConnections()
cn <- 1 # Setting a counter
con <- file("Garch(1,1) model of a normal distribution")
sink(con, append=TRUE, type=c("output", "message"), split=TRUE)
q = 1
p = 1
# Looping over parameter omega
for(a0 in seq(0.01, 0.1, by = 0.01)){
  # Looping over parameter alpha1
  for(a1 in seq(0.001, 0.01, by = 0.001)){
    # Looping over parameter alpha1
    for(b1 in seq(0.8, 0.99, by = 0.1)){
      # ERROR and WARNINGS HANDLING
      tryCatch({
        q_order_components[[1]] <- q
        p_order_res_delay[[1]] <- p
        omega[[1]] <- a0
        alpha1[[1]] <- a1
        beta1[[1]] <- b1
        sigmasquaredW <- var(distribution)
        stationarity[[1]] <- a1*sigmasquaredW + b1
        uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)), distribution.model= "sstd", fixed.pars = list(omega=a0, alpha1=a1, beta1=b1))
        Xt_t_student_asymmetric_garch1_q1_p1_model_ls[[cn]] <- ugarchfit(spec = uspec, data = c(0,distribution))
        show(Xt_t_student_asymmetric_garch1_q1_p1_model_ls[[cn]])
        
        k <- length(coef(Xt_t_student_asymmetric_garch1_q1_p1_model_ls[[cn]])) + 1
        loglik_val[[1]] <- likelihood(Xt_t_student_asymmetric_garch1_q1_p1_model_ls[[cn]])
        AIC_val[[1]] <- infocriteria(Xt_t_student_asymmetric_garch1_q1_p1_model_ls[[cn]])
        AICc_val[[1]] <- AIC_val[[1]] +2*k*(k+1)/(n-k-1)
        BIC_val[[1]] <- AIC_val[[1]]+k*(log(n)-2)
        
        cat("\n")
        cat(blue("Garch parameter p = ",p,","), blue("Garch parameter q = ",q,","))
        cat("\n")
        cat(blue("AIC = ",round(AIC_val[[1]],3),","), blue("AICc = ",round(AICc_val[[1]],3),","), blue("BIC = ",round(BIC_val[[1]],3),","))
        cat("\n")
        
        LB_fitdf <- min(min(max_lag, k), max_lag-1)
        Xt_t_student_asymmetric_garch1_q1_p1_LB_test_ls[[cn]] <- Box.test(residuals(Xt_t_student_asymmetric_garch1_q1_p1_model_ls[[cn]]), 
                                                           lag=max_lag, fitdf=LB_fitdf, type = "Ljung-Box")
        show(Xt_t_student_asymmetric_garch1_q1_p1_LB_test_ls[[cn]])
        Xt_t_student_asymmetric_garch1_q1_p1_Ext_LB_test_ls[[cn]] <- LjungBoxTest(residuals(Xt_t_student_asymmetric_garch1_q1_p1_model_ls[[cn]]),
                                                                   lag.max=max_lag,  k=k, StartLag=1, SquaredQ=FALSE)
        show(Xt_t_student_asymmetric_garch1_q1_p1_Ext_LB_test_ls[[cn]])
        H_max_lag <-max(max_lag, k+3)
        Xt_t_student_asymmetric_garch1_q1_p1_HLB_test_ls[[cn]] <- Box.test(residuals(Xt_t_student_asymmetric_garch1_q1_p1_model_ls[[cn]]), 
                                                            lag=H_max_lag, fitdf=k, type = "Ljung-Box")
        show(Xt_t_student_asymmetric_garch1_q1_p1_HLB_test_ls[[cn]])
        cat("  \n","  \n")
        Xt_t_student_asymmetric_garch1_q1_p1_ls[[cn]]  <- list(c(q_order_components, p_order_res_delay, omega, alpha1, beta1, loglik_val, AIC_val, AICc_val, BIC_val, stationarity),
                                                Xt_t_student_asymmetric_garch1_q1_p1_model_ls[[cn]],
                                                Xt_t_student_asymmetric_garch1_q1_p1_LB_test_ls[[cn]],
                                                Xt_t_student_asymmetric_garch1_q1_p1_HLB_test_ls[[cn]])
        cn <- cn+1
        cat("  \n","  \n")
      }, error = function(e){
        cat(red(sprintf("caught error: %s", e)))
        cat(red("Garch parameter p = ", p,","), red("Garch parameter q = ", q,","))
        cat("\n")
        traceback(1, max.lines = 1)
        cat("  \n","  \n")
        Xt_t_student_asymmetric_garch1_q1_p1_err_war_mess <<- c(Xt_t_student_asymmetric_garch1_q1_p1_err_war_mess, list(c(e, paste("ERROR!... for counter cn = ",cn,",... Garch parameters p = ",p,", q = ",q,"."))))
        Xt_t_student_asymmetric_garch1_q1_p1_ls[[cn]] <<- list(c(e, paste("ERROR!... for counter cn = ",cn,",... Garch parameters p = ",p,", q = ",q,".")))
        cn <<- cn+1
      }, warning = function(w){
        cat(yellow(sprintf("caught warning: %s", w)))
        cat(yellow("Garch parameter p = ",p,","), yellow("Garch parameter q = ",q,","))
        cat("\n")
        traceback(1, max.lines = 1)
        cat("  \n","  \n")
        Xt_t_student_asymmetric_garch1_q1_p1_err_war_mess <<- c(Xt_t_student_asymmetric_garch1_q1_p1_err_war_mess, list(c(w, paste("WARNING!... for counter cn = ",cn,",... Garch parameters p = ",p,", q = ",q,"."))))
        Xt_t_student_asymmetric_garch1_q1_p1_ls[[cn]] <<- list(c(w, paste("WARNING!... for counter cn = ",cn,",... Arch parameters p = ",p,", q = ",q,".")))
        cn <<- cn+1
      }
      )
    } 
  }
}
sink()
closeAllConnections()

num_estrazioni <- 72
# num_estrazioni <- length(Xt_t_student_asymmetric_garch1_q1_p1_ls)
# Estraiamo alcune informazioni dalla lista creata precedentemente
Xt_t_student_asymmetric_garch1_q1_p1_loglik_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_garch1_q1_p1_ls[[k]])==4) 
{Xt_t_student_asymmetric_garch1_q1_p1_loglik_values[k]<- Xt_t_student_asymmetric_garch1_q1_p1_ls[[k]][[1]][["loglik value"]]}}
show(Xt_t_student_asymmetric_garch1_q1_p1_loglik_values)
#
Xt_t_student_asymmetric_garch1_q1_p1_AICc_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_garch1_q1_p1_ls[[k]])==4) 
{Xt_t_student_asymmetric_garch1_q1_p1_AICc_values[k]<- Xt_t_student_asymmetric_garch1_q1_p1_ls[[k]][[1]][["AICc value"]]}}
show(Xt_t_student_asymmetric_garch1_q1_p1_AICc_values)
#
Xt_t_student_asymmetric_garch1_q1_p1_BIC_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_garch1_q1_p1_ls[[k]])==4) 
{Xt_t_student_asymmetric_garch1_q1_p1_BIC_values[k]<- Xt_t_student_asymmetric_garch1_q1_p1_ls[[k]][[1]][["BIC value"]]}}
show(Xt_t_student_asymmetric_garch1_q1_p1_BIC_values)
#
Xt_t_student_asymmetric_garch1_q1_p1_AIC_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_garch1_q1_p1_ls[[k]])==4) 
{Xt_t_student_asymmetric_garch1_q1_p1_AIC_values[k]<- Xt_t_student_asymmetric_garch1_q1_p1_ls[[k]][[1]][["AIC value"]]}}
show(Xt_t_student_asymmetric_garch1_q1_p1_AIC_values)
#
Xt_t_student_asymmetric_garch1_q1_p1_omega_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_garch1_q1_p1_ls[[k]])==4) 
{Xt_t_student_asymmetric_garch1_q1_p1_omega_params[k]<- Xt_t_student_asymmetric_garch1_q1_p1_ls[[k]][[1]][["omega parameter(a0)"]]}}
show(Xt_t_student_asymmetric_garch1_q1_p1_omega_params)
#
Xt_t_student_asymmetric_garch1_q1_p1_alpha1_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_garch1_q1_p1_ls[[k]])==4) 
{Xt_t_student_asymmetric_garch1_q1_p1_alpha1_params[k]<- Xt_t_student_asymmetric_garch1_q1_p1_ls[[k]][[1]][["alpha1 parameter(a1)"]]} }
show(Xt_t_student_asymmetric_garch1_q1_p1_alpha1_params)
#
Xt_t_student_asymmetric_garch1_q1_p1_beta1_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_garch1_q1_p1_ls[[k]])==4) 
{Xt_t_student_asymmetric_garch1_q1_p1_beta1_params[k]<- Xt_t_student_asymmetric_garch1_q1_p1_ls[[k]][[1]][["beta1 parameter(b1)"]]} }
show(Xt_t_student_asymmetric_garch1_q1_p1_beta1_params)
#
Xt_t_student_asymmetric_garch1_q1_p1_LB_p_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_garch1_q1_p1_ls[[k]])==4) 
{Xt_t_student_asymmetric_garch1_q1_p1_LB_p_values[k]<- Xt_t_student_asymmetric_garch1_q1_p1_ls[[k]][[3]][["p.value"]]}}
show(Xt_t_student_asymmetric_garch1_q1_p1_LB_p_values)
#
Xt_t_student_asymmetric_garch1_q1_p1_HLB_p_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_garch1_q1_p1_ls[[k]])==4) 
{Xt_t_student_asymmetric_garch1_q1_p1_HLB_p_values[k]<- Xt_t_student_asymmetric_garch1_q1_p1_ls[[k]][[4]][["p.value"]]}}
show(Xt_t_student_asymmetric_garch1_q1_p1_HLB_p_values)
#
Xt_t_student_asymmetric_garch1_q1_p1_stationarity <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_garch1_q1_p1_ls[[k]])==4) 
{Xt_t_student_asymmetric_garch1_q1_p1_stationarity[k]<- Xt_t_student_asymmetric_garch1_q1_p1_ls[[k]][[1]][["stationarity"]]}}
show(Xt_t_student_asymmetric_garch1_q1_p1_stationarity)
# We store the extracted information in a data frame
Xt_t_student_asymmetric_garch1_q1_p1_df <- data.frame(Index=1:num_estrazioni,
                                       loglik_value=Xt_t_student_asymmetric_garch1_q1_p1_loglik_values,
                                       AICc_value=Xt_t_student_asymmetric_garch1_q1_p1_AICc_values, 
                                       BIC_value=Xt_t_student_asymmetric_garch1_q1_p1_BIC_values,
                                       AIC_value=Xt_t_student_asymmetric_garch1_q1_p1_AIC_values,
                                       a0_param=Xt_t_student_asymmetric_garch1_q1_p1_omega_params,
                                       a1_param=Xt_t_student_asymmetric_garch1_q1_p1_alpha1_params,
                                       b1_param=Xt_t_student_asymmetric_garch1_q1_p1_beta1_params,
                                       LB_p_value=Xt_t_student_asymmetric_garch1_q1_p1_LB_p_values,
                                       HLB_p_value=Xt_t_student_asymmetric_garch1_q1_p1_HLB_p_values,
                                       stationarity=Xt_t_student_asymmetric_garch1_q1_p1_stationarity)
# rimuoviamo i parametri che non rispettano la condizione di stazionarietà
Xt_t_student_asymmetric_garch1_q1_p1_df <- Xt_t_student_asymmetric_garch1_q1_p1_df[Xt_t_student_asymmetric_garch1_q1_p1_df$stationarity <= 1, ]
head(Xt_t_student_asymmetric_garch1_q1_p1_df, 20)
tail(Xt_t_student_asymmetric_garch1_q1_p1_df,10)

# We sort the models according to increasing AICc values.
Xt_t_student_asymmetric_garch1_q1_p1_AICc_sort_df <- Xt_t_student_asymmetric_garch1_q1_p1_df[order(Xt_t_student_asymmetric_garch1_q1_p1_df$AICc_value),]
rownames(Xt_t_student_asymmetric_garch1_q1_p1_AICc_sort_df) <- NULL
head(Xt_t_student_asymmetric_garch1_q1_p1_AICc_sort_df,20)
# We sort the models according to increasing BIC values.
Xt_t_student_asymmetric_garch1_q1_p1_BIC_sort_df <- Xt_t_student_asymmetric_garch1_q1_p1_df[order(Xt_t_student_asymmetric_garch1_q1_p1_df$BIC_value),]
rownames(Xt_t_student_asymmetric_garch1_q1_p1_BIC_sort_df) <- NULL
head(Xt_t_student_asymmetric_garch1_q1_p1_BIC_sort_df,20)
# We sort the models according to increasing AIC values.
Xt_t_student_asymmetric_garch1_q1_p1_AIC_sort_df <- Xt_t_student_asymmetric_garch1_q1_p1_df[order(Xt_t_student_asymmetric_garch1_q1_p1_df$AIC_value),]
rownames(Xt_t_student_asymmetric_garch1_q1_p1_AIC_sort_df) <- NULL
head(Xt_t_student_asymmetric_garch1_q1_p1_AIC_sort_df,20)

# Costruiamo il nuovo modello con i parametri stimati
a0 <- 0.03
a1 <- 0.002
b1 <- 0.9
Xt_t_student_asymmetric_garch1_q1_p1_new <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello GARCH(1,1)
Xt <- Xt_t_student_asymmetric_garch1_q1_p1_new
df_Xt_t_student_asymmetric_garch1_q1_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df<- df_Xt_t_student_asymmetric_garch1_q1_p1_new
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
y_name <- bquote("Monthly Deaths")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Monthly Deaths")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_asymmetric_garch1_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch1_q1_p1_sp)
# La regression line risulta leggermente inclinata, mentre la LOESS oscilla leggermente intorno alla regression line.

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch1_q1_p1_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch1_q1_p1)
summary(Xt_t_student_asymmetric_garch1_q1_p1_lm)
summary(Xt_t_student_asymmetric_garch1_q1_p1_lm$fitted.values)

Xt_t_student_asymmetric_garch1_q1_p1_res <- Xt_t_student_asymmetric_garch1_q1_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_garch1_q1_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_garch1_q1_p1_res)                  # theoretical value 3.
# La skew è pari a -1.527619; il suo valore è negativo e la sua distribuzione è
# asimmetrica verso sinistra con una coda lunga negativa.
# La kurtosi è pari a 6.05205; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch1_q1_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) of a asymmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_asymmetric_garch1_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch1_q1_p1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch1_q1_p1_res)
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
Xt_t_student_asymmetric_garch1_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch1_q1_p1_sp)

plot(Xt_t_student_asymmetric_garch1_q1_p1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_garch1_q1_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q1_p1)
show(Xt_t_student_asymmetric_garch1_q1_p1_bp)
# Si ha un p-value di 0.154 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q1_p1)
show(Xt_t_student_asymmetric_garch1_q1_p1_w)
# Si ha un p-value di 0.3151 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Plot of the autocorrelogram.
y <- Xt_t_student_asymmetric_garch1_q1_p1_lm$residuals
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
y <- Xt_t_student_asymmetric_garch1_q1_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 3.2542, df = 1, p-value = 0.07124
# I risultati mostrano un p-value > 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione.

# In questo modello Garch(1,1) con una distribuzione t-student asimmetrica, nonostante abbiamo cercato
# di stimare i miglior parametri la serie continua ad avere evidenza di omoschedasticità nella serie 
# con assenza di autocorrelazione.
# Evidentemente utilizzare un modello Garch(1,1) non si adatta bene ai dati della 
# ditribuzione t-student asimmetrica.

##########################################
##########################################

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(1,2)
Xt <- Xt_normal_garch1_q1_p2
df_Xt_normal_garch1_q1_p2 <- data.frame(t = 1:length(Xt), X = Xt)

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
y_name <- bquote("Monthly Deaths")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Monthly Deaths")
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
moments::skewness(Xt_normal_garch1_q1_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_garch1_q1_p2_res)                  # theoretical value 3.
# La skew è pari a -0.08092882; il suo valore è prossimo a zero, quindi la sua distribuzione 
# viene considerata simmetrica con una coda negativa.
# La kurtosi è pari a 2.832292; questo indica che la distribuzione è leggermenete platykurtic.

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
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag.

# I risultati indicano che c'è assenza di correlazione nella serie.

# In questo modello Garch(1,2) con una distribuzione normale possiamo affermare che la serie 
# ha evidenza di eteroschedasticità e assenza di autocorrelazione.

##########################################

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello GARCH(1,2)
Xt <- Xt_t_student_symmetric_garch1_q1_p2
df_Xt_t_student_symmetric_garch1_q1_p2 <- data.frame(t = 1:length(Xt), X = Xt)

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
y_name <- bquote("Monthly Deaths")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Monthly Deaths")
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
moments::skewness(Xt_t_student_symmetric_garch1_q1_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_garch1_q1_p2_res)                  # theoretical value 3.
# La skew è pari a 0.1368888; il suo valore è positivo e vicino allo zero; quindi, la sua distribuzione
# è vicina alla simmetria.
# La kurtosi è pari a 5.499922; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

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
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag.

# I risultati mostrano un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

# Questo modello Garch(1,2) con distribuzione t-student simmetrica ha un'evidenza di 
# eteroschedasticità nella serie e assenza di autocorrelazione nei residui.

##########################################

# Consideriamo la prima traiettoia con distribuzione t-student asimmetrica di un modello GARCH(1,1)
Xt <- Xt_t_student_asymmetric_garch1_q1_p2
df_Xt_t_student_asymmetric_garch1_q1_p2 <- data.frame(t = 1:length(Xt), X = Xt)

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
moments::skewness(Xt_t_student_asymmetric_garch1_q1_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_garch1_q1_p2_res)                  # theoretical value 3.
# La skew è pari a -1.728404; il suo valore è negativo e la sua distribuzione è
# asimmetrica verso sinistra con una coda lunga negativa.
# La kurtosi è pari a 7.426788; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

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

# I risultati mostrano un p-value < 0.05, ciò significa che ci sono prove
# di autocorrelazione nei residui del modello.

# Questo modello Garch(1,2) con una distribuzione t-student asimmetrica ha presenza di autocorrelazione
# e presenza di omoschedasticità nel test di White. 
# Quindi, proviamo a stimare i parametri che si adattano meglio al modello.
distribution <- dist_t_student_asymmetric
y <- c(0,distribution)
class(y)
head(y)
T <- length(y)
# 146
max_lag <- ceiling(min(10, T/4))   # Hyndman (for data without seasonality)
# We introduce some lists where to store the results of our procedure.
Xt_t_student_asymmetric_garch1_q1_p2_ls <- list()
Xt_t_student_asymmetric_garch1_q1_p2_model_ls <- list()
Xt_t_student_asymmetric_garch1_q1_p2_LB_test_ls <- list()
Xt_t_student_asymmetric_garch1_q1_p2_Ext_LB_test_ls <- list()
Xt_t_student_asymmetric_garch1_q1_p2_HLB_test_ls <- list()
Xt_t_student_asymmetric_garch1_q1_p2_err_war_mess <- list()
q_order_components <- vector(mode="list", length=1)
names(q_order_components) <- "q parameter" # Order of components
p_order_res_delay <- vector(mode="list", length=1)
names(p_order_res_delay) <- "p parameter" # Order of residuals delay
omega <- vector(mode="list", length=1)
names(omega) <- "omega parameter(a0)"
alpha1 <- vector(mode="list", length=1)
names(alpha1) <- "alpha1 parameter(a1)"
beta1 <- vector(mode="list", length=1)
names(beta1) <- "beta1 parameter(b1)"
beta2 <- vector(mode="list", length=1)
names(beta2) <- "beta2 parameter(b2)"
loglik_val <- vector(mode="list", length=1)
names(loglik_val) <- "loglik value"
AIC_val <- vector(mode="list", length=1)
names(AIC_val) <- "AIC value"
AICc_val <- vector(mode="list", length=1)
names(AICc_val) <- "AICc value"
BIC_val <- vector(mode="list", length=1)
names(BIC_val) <- "BIC value"
stationarity <- vector(mode="list", length=1)
names(stationarity) <- "stationarity"
closeAllConnections()
cn <- 1 # Setting a counter
con <- file("Garch(1,1) model of a asymmetric t-student distribution")
sink(con, append=TRUE, type=c("output", "message"), split=TRUE)
q = 1
p = 2
# Looping over parameter omega
for(a0 in seq(0.001, 0.002, by = 0.0002)){
  # Looping over parameter alpha1
  for(a1 in seq(0.001, 0.002, by = 0.0002)){
    # Looping over parameter beta1
    for(b1 in seq(0.79, 0.99, by = 0.1)){
      # Looping over parameter beta1
      for(b2 in seq(0.01, 0.02, by = 0.002)){
      # ERROR and WARNINGS HANDLING
      tryCatch({
        q_order_components[[1]] <- q
        p_order_res_delay[[1]] <- p
        omega[[1]] <- a0
        alpha1[[1]] <- a1
        beta1[[1]] <- b1
        beta2[[1]] <- b2
        sigmasquaredW <- var(distribution)
        stationarity[[1]] <- a1*sigmasquaredW + b1 + b2
        uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)), distribution.model= "sstd", fixed.pars = list(omega=a0, alpha1=a1, beta1=b1, beta2=b2))
        Xt_t_student_asymmetric_garch1_q1_p2_model_ls[[cn]] <- ugarchfit(spec = uspec, data = c(0,distribution))
        show(Xt_t_student_asymmetric_garch1_q1_p2_model_ls[[cn]])
        
        k <- length(coef(Xt_t_student_asymmetric_garch1_q1_p2_model_ls[[cn]])) + 1
        loglik_val[[1]] <- likelihood(Xt_t_student_asymmetric_garch1_q1_p2_model_ls[[cn]])
        AIC_val[[1]] <- infocriteria(Xt_t_student_asymmetric_garch1_q1_p2_model_ls[[cn]])
        AICc_val[[1]] <- AIC_val[[1]] +2*k*(k+1)/(n-k-1)
        BIC_val[[1]] <- AIC_val[[1]]+k*(log(n)-2)
        
        cat("\n")
        cat(blue("Garch parameter p = ",p,","), blue("Garch parameter q = ",q,","), blue("a0 = ",a0,","), blue("a1 = ",a1,","), blue("b1 = ",b1,","), blue("b2 = ",b2,","))
        cat("\n")
        cat(blue("AIC = ",round(AIC_val[[1]],3),","), blue("AICc = ",round(AICc_val[[1]],3),","), blue("BIC = ",round(BIC_val[[1]],3),","))
        cat("\n")
        
        LB_fitdf <- min(min(max_lag, k), max_lag-1)
        Xt_t_student_asymmetric_garch1_q1_p2_LB_test_ls[[cn]] <- Box.test(residuals(Xt_t_student_asymmetric_garch1_q1_p2_model_ls[[cn]]), 
                                                                         lag=max_lag, fitdf=LB_fitdf, type = "Ljung-Box")
        show(Xt_t_student_asymmetric_garch1_q1_p2_LB_test_ls[[cn]])
        Xt_t_student_asymmetric_garch1_q1_p2_Ext_LB_test_ls[[cn]] <- LjungBoxTest(residuals(Xt_t_student_asymmetric_garch1_q1_p2_model_ls[[cn]]),
                                                                                 lag.max=max_lag,  k=k, StartLag=1, SquaredQ=FALSE)
        show(Xt_t_student_asymmetric_garch1_q1_p2_Ext_LB_test_ls[[cn]])
        H_max_lag <-max(max_lag, k+3)
        Xt_t_student_asymmetric_garch1_q1_p2_HLB_test_ls[[cn]] <- Box.test(residuals(Xt_t_student_asymmetric_garch1_q1_p2_model_ls[[cn]]), 
                                                                          lag=H_max_lag, fitdf=k, type = "Ljung-Box")
        show(Xt_t_student_asymmetric_garch1_q1_p2_HLB_test_ls[[cn]])
        cat("  \n","  \n")
        Xt_t_student_asymmetric_garch1_q1_p2_ls[[cn]]  <- list(c(q_order_components, p_order_res_delay, omega, alpha1, beta1, beta2, loglik_val, AIC_val, AICc_val, BIC_val, stationarity),
                                                              Xt_t_student_asymmetric_garch1_q1_p2_model_ls[[cn]],
                                                              Xt_t_student_asymmetric_garch1_q1_p2_LB_test_ls[[cn]],
                                                              Xt_t_student_asymmetric_garch1_q1_p2_HLB_test_ls[[cn]])
        cn <- cn+1
        cat("  \n","  \n")
      }, error = function(e){
        cat(red(sprintf("caught error: %s", e)))
        cat(red("Garch parameter p = ", p,","), red("Garch parameter q = ", q,","), red("a0 = ",a0,","),red("a1 = ",a1,","), red("b1 = ",b1,","), red("b2 = ",b2,","))
        cat("\n")
        traceback(1, max.lines = 1)
        cat("  \n","  \n")
        Xt_t_student_asymmetric_garch1_q1_p2_err_war_mess <<- c(Xt_t_student_asymmetric_garch1_q1_p2_err_war_mess, list(c(e, paste("ERROR!... for counter cn = ",cn,",... Garch parameters p = ",p,", q = ",q,"."))))
        Xt_t_student_asymmetric_garch1_q1_p2_ls[[cn]] <<- list(c(e, paste("ERROR!... for counter cn = ",cn,",... Garch parameters p = ",p,", q = ",q,".")))
        cn <<- cn+1
      }, warning = function(w){
        cat(yellow(sprintf("caught warning: %s", w)))
        cat(yellow("Garch parameter p = ",p,","), yellow("Garch parameter q = ",q,","), yellow("a0 = ",a0,","), yellow("a1 = ",a1,","), yellow("b1 = ",b1,","), yellow("b2 = ",b2,","))
        cat("\n")
        traceback(1, max.lines = 1)
        cat("  \n","  \n")
        Xt_t_student_asymmetric_garch1_q1_p2_err_war_mess <<- c(Xt_t_student_asymmetric_garch1_q1_p2_err_war_mess, list(c(w, paste("WARNING!... for counter cn = ",cn,",... Garch parameters p = ",p,", q = ",q,"."))))
        Xt_t_student_asymmetric_garch1_q1_p2_ls[[cn]] <<- list(c(w, paste("WARNING!... for counter cn = ",cn,",... Arch parameters p = ",p,", q = ",q,".")))
        cn <<- cn+1
      }
      )
      } 
    }
  }
}
sink()
closeAllConnections()

num_estrazioni <- 72
num_estrazioni <- length(Xt_t_student_asymmetric_garch1_q1_p2_ls)
# Estraiamo alcune informazioni dalla lista creata precedentemente
Xt_t_student_asymmetric_garch1_q1_p2_loglik_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]])==4) 
{Xt_t_student_asymmetric_garch1_q1_p2_loglik_values[k]<- Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]][[1]][["loglik value"]]}}
show(Xt_t_student_asymmetric_garch1_q1_p2_loglik_values)
#
Xt_t_student_asymmetric_garch1_q1_p2_AICc_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]])==4) 
{Xt_t_student_asymmetric_garch1_q1_p2_AICc_values[k]<- Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]][[1]][["AICc value"]]}}
show(Xt_t_student_asymmetric_garch1_q1_p2_AICc_values)
#
Xt_t_student_asymmetric_garch1_q1_p2_BIC_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]])==4) 
{Xt_t_student_asymmetric_garch1_q1_p2_BIC_values[k]<- Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]][[1]][["BIC value"]]}}
show(Xt_t_student_asymmetric_garch1_q1_p2_BIC_values)
#
Xt_t_student_asymmetric_garch1_q1_p2_AIC_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]])==4) 
{Xt_t_student_asymmetric_garch1_q1_p2_AIC_values[k]<- Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]][[1]][["AIC value"]]}}
show(Xt_t_student_asymmetric_garch1_q1_p2_AIC_values)
#
Xt_t_student_asymmetric_garch1_q1_p2_omega_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]])==4) 
{Xt_t_student_asymmetric_garch1_q1_p2_omega_params[k]<- Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]][[1]][["omega parameter(a0)"]]}}
show(Xt_t_student_asymmetric_garch1_q1_p2_omega_params)
#
Xt_t_student_asymmetric_garch1_q1_p2_alpha1_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]])==4) 
{Xt_t_student_asymmetric_garch1_q1_p2_alpha1_params[k]<- Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]][[1]][["alpha1 parameter(a1)"]]} }
show(Xt_t_student_asymmetric_garch1_q1_p2_alpha1_params)
#
Xt_t_student_asymmetric_garch1_q1_p2_beta1_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]])==4) 
{Xt_t_student_asymmetric_garch1_q1_p2_beta1_params[k]<- Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]][[1]][["beta1 parameter(b1)"]]} }
show(Xt_t_student_asymmetric_garch1_q1_p2_beta1_params)
#
Xt_t_student_asymmetric_garch1_q1_p2_beta2_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]])==4) 
{Xt_t_student_asymmetric_garch1_q1_p2_beta2_params[k]<- Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]][[1]][["beta2 parameter(b2)"]]} }
show(Xt_t_student_asymmetric_garch1_q1_p2_beta2_params)
#
Xt_t_student_asymmetric_garch1_q1_p2_LB_p_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]])==4) 
{Xt_t_student_asymmetric_garch1_q1_p2_LB_p_values[k]<- Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]][[3]][["p.value"]]}}
show(Xt_t_student_asymmetric_garch1_q1_p2_LB_p_values)
#
Xt_t_student_asymmetric_garch1_q1_p2_HLB_p_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]])==4) 
{Xt_t_student_asymmetric_garch1_q1_p2_HLB_p_values[k]<- Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]][[4]][["p.value"]]}}
show(Xt_t_student_asymmetric_garch1_q1_p2_HLB_p_values)
#
Xt_t_student_asymmetric_garch1_q1_p2_stationarity <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]])==4) 
{Xt_t_student_asymmetric_garch1_q1_p2_stationarity[k]<- Xt_t_student_asymmetric_garch1_q1_p2_ls[[k]][[1]][["stationarity"]]}}
show(Xt_t_student_asymmetric_garch1_q1_p2_stationarity)
# We store the extracted information in a data frame
Xt_t_student_asymmetric_garch1_q1_p2_df <- data.frame(Index=1:num_estrazioni,
                                                     loglik_value=Xt_t_student_asymmetric_garch1_q1_p2_loglik_values,
                                                     AICc_value=Xt_t_student_asymmetric_garch1_q1_p2_AICc_values, 
                                                     BIC_value=Xt_t_student_asymmetric_garch1_q1_p2_BIC_values,
                                                     AIC_value=Xt_t_student_asymmetric_garch1_q1_p2_AIC_values,
                                                     a0_param=Xt_t_student_asymmetric_garch1_q1_p2_omega_params,
                                                     a1_param=Xt_t_student_asymmetric_garch1_q1_p2_alpha1_params,
                                                     b1_param=Xt_t_student_asymmetric_garch1_q1_p2_beta1_params,
                                                     b2_param=Xt_t_student_asymmetric_garch1_q1_p2_beta2_params,
                                                     LB_p_value=Xt_t_student_asymmetric_garch1_q1_p2_LB_p_values,
                                                     HLB_p_value=Xt_t_student_asymmetric_garch1_q1_p2_HLB_p_values,
                                                     stationarity=Xt_t_student_asymmetric_garch1_q1_p2_stationarity)
# rimuoviamo i parametri che non rispettano la condizione di stazionarietà
Xt_t_student_asymmetric_garch1_q1_p2_df <- Xt_t_student_asymmetric_garch1_q1_p2_df[Xt_t_student_asymmetric_garch1_q1_p2_df$stationarity < 1, ]
head(Xt_t_student_asymmetric_garch1_q1_p2_df, 20)
tail(Xt_t_student_asymmetric_garch1_q1_p2_df,10)

# We sort the models according to increasing AICc values.
Xt_t_student_asymmetric_garch1_q1_p2_AICc_sort_df <- Xt_t_student_asymmetric_garch1_q1_p2_df[order(Xt_t_student_asymmetric_garch1_q1_p2_df$AICc_value),]
rownames(Xt_t_student_asymmetric_garch1_q1_p2_AICc_sort_df) <- NULL
head(Xt_t_student_asymmetric_garch1_q1_p2_AICc_sort_df,20)
# We sort the models according to increasing BIC values.
Xt_t_student_asymmetric_garch1_q1_p2_BIC_sort_df <- Xt_t_student_asymmetric_garch1_q1_p2_df[order(Xt_t_student_asymmetric_garch1_q1_p2_df$BIC_value),]
rownames(Xt_t_student_asymmetric_garch1_q1_p2_BIC_sort_df) <- NULL
head(Xt_t_student_asymmetric_garch1_q1_p2_BIC_sort_df,20)
# We sort the models according to increasing AIC values.
Xt_t_student_asymmetric_garch1_q1_p2_AIC_sort_df <- Xt_t_student_asymmetric_garch1_q1_p2_df[order(Xt_t_student_asymmetric_garch1_q1_p2_df$AIC_value),]
rownames(Xt_t_student_asymmetric_garch1_q1_p2_AIC_sort_df) <- NULL
head(Xt_t_student_asymmetric_garch1_q1_p2_AIC_sort_df,20)

# Costruiamo il nuovo modello con i parametri stimati
a0 <- 0.002
a1 <- 0.002
bp <- c(0.89, 0.02)
Xt_t_student_asymmetric_garch1_q1_p2_new <- model_garch(a0, a1, bp, X0, sigmasquared0, distribution, q, p)

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello GARCH(1,1)
Xt <- Xt_t_student_asymmetric_garch1_q1_p2_new
df_Xt_t_student_asymmetric_garch1_q1_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df <- df_Xt_t_student_asymmetric_garch1_q1_p2
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

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch1_q1_p2_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch1_q1_p2)
summary(Xt_t_student_asymmetric_garch1_q1_p2_lm)
summary(Xt_t_student_asymmetric_garch1_q1_p2_lm$fitted.values)

Xt_t_student_asymmetric_garch1_q1_p2_res <- Xt_t_student_asymmetric_garch1_q1_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_garch1_q1_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_garch1_q1_p2_res)                  # theoretical value 3.
# La skew è pari a -1.728404; il suo valore è negativo e la sua distribuzione è
# asimmetrica verso sinistra con una coda lunga negativa.
# La kurtosi è pari a 7.426788; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

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
Xt_t_student_asymmetric_garch1_q1_p1_sp <- ggplot(Data_df) +
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
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(1,2) of asymmetric t-student distribution")))
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
# Si ha un p-value di 0.0114 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q1_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q1_p2)
show(Xt_t_student_asymmetric_garch1_q1_p2_w)
# Si ha un p-value di 0.0404 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

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
y <- Xt_t_student_asymmetric_garch1_q1_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 4.0259, df = 1, p-value = 0.04481
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione.

# In questo modello Garch(1,2) con una distribuzione t-student asimmetrica, con i nuovi parametri stimati,
# presenta eteroschedasticità nella serie e assenza di autocorrelazione.

##########################################
##########################################

# Consideriamo la prima traiettoia con distribuzione normale di un modello ARCH(2)
Xt <- Xt_normal_arch1_q2
df_Xt_normal_arch1_q2 <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df<- df_Xt_normal_arch1_q2
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Line plot of the model Arch(2) with a normal distribution")))
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
y_name <- bquote("Monthly Deaths")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Monthly Deaths")
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
moments::skewness(Xt_normal_arch1_q2_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_arch1_q2_res)                  # theoretical value 3.
# La skew è pari a -0.1206737; il suo valore è prossimo a zero, quindi la sua distribuzione potrebbe essere
# considerata approssimativamente simmetrica.
# La kurtosi è pari a 2.890637; questo indica che la distribuzione è leggermenete platykurtic.

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

# Il risultato indica che c'è presenza di autocorrelazione nei residui del modello.

# Questo modello Arch(2) con una distribuzione normale ha presenza di autocorrelazione
# e presenza di eteroschedasticità. 
# Quindi, per eliminare la correlazione nella serie proviamo a stimare i parametri che si adattano meglio al modello.
distribution <- dist_normal1
y <- c(0,distribution)
class(y)
head(y)
T <- length(y)
# 146
max_lag <- ceiling(min(10, T/4))   # Hyndman (for data without seasonality)
# We introduce some lists where to store the results of our procedure.
Xt_normal_arch1_q2_ls <- list()
Xt_normal_arch1_q2_model_ls <- list()
Xt_normal_arch1_q2_LB_test_ls <- list()
Xt_normal_arch1_q2_Ext_LB_test_ls <- list()
Xt_normal_arch1_q2_HLB_test_ls <- list()
Xt_normal_arch1_q2_err_war_mess <- list()
q_order_components <- vector(mode="list", length=1)
names(q_order_components) <- "q parameter" # Order of components
p_order_res_delay <- vector(mode="list", length=1)
names(p_order_res_delay) <- "p parameter" # Order of residuals delay
omega <- vector(mode="list", length=1)
names(omega) <- "omega parameter(a0)"
alpha1 <- vector(mode="list", length=1)
names(alpha1) <- "alpha1 parameter(a1)"
alpha2 <- vector(mode="list", length=1)
names(alpha2) <- "alpha2 parameter(a2)"
loglik_val <- vector(mode="list", length=1)
names(loglik_val) <- "loglik value"
AIC_val <- vector(mode="list", length=1)
names(AIC_val) <- "AIC value"
AICc_val <- vector(mode="list", length=1)
names(AICc_val) <- "AICc value"
BIC_val <- vector(mode="list", length=1)
names(BIC_val) <- "BIC value"
stationarity <- vector(mode="list", length=1)
names(stationarity) <- "stationarity"
closeAllConnections()
cn <- 1 # Setting a counter
con <- file("Arch(2) model of a normal distribution")
sink(con, append=TRUE, type=c("output", "message"), split=TRUE)
q = 2
p = 0
# Looping over parameter omega
for(a0 in seq(0.01, 0.02, by = 0.002)){
  # Looping over parameter alpha1
  for(a1 in seq(0.001, 0.02, by = 0.002)){
      # Looping over parameter alpha2
      for(a2 in seq(0.01, 0.2, by = 0.002)){
        # ERROR and WARNINGS HANDLING
        tryCatch({
          q_order_components[[1]] <- q
          p_order_res_delay[[1]] <- p
          omega[[1]] <- a0
          alpha1[[1]] <- a1
          alpha2[[1]] <- a2
          sigmasquaredW <- var(distribution)
          stationarity[[1]] <- (a1+a2)*sigmasquaredW
          uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)), distribution.model= "norm", fixed.pars = list(omega=a0, alpha1=a1, alpha2=a2))
          Xt_normal_arch1_q2_model_ls[[cn]] <- ugarchfit(spec = uspec, data = c(0,distribution))
          show(Xt_normal_arch1_q2_model_ls[[cn]])
          
          k <- length(coef(Xt_normal_arch1_q2_model_ls[[cn]])) + 1
          loglik_val[[1]] <- likelihood(Xt_normal_arch1_q2_model_ls[[cn]])
          AIC_val[[1]] <- infocriteria(Xt_normal_arch1_q2_model_ls[[cn]])
          AICc_val[[1]] <- AIC_val[[1]] +2*k*(k+1)/(n-k-1)
          BIC_val[[1]] <- AIC_val[[1]]+k*(log(n)-2)
          
          cat("\n")
          cat(blue("Arch parameter p = ",p,","), blue("Arch parameter q = ",q,","), blue("a0 = ",a0,","), blue("a1 = ",a1,","), blue("a2 = ",a2,","))
          cat("\n")
          cat(blue("AIC = ",round(AIC_val[[1]],3),","), blue("AICc = ",round(AICc_val[[1]],3),","), blue("BIC = ",round(BIC_val[[1]],3),","))
          cat("\n")
          
          LB_fitdf <- min(min(max_lag, k), max_lag-1)
          Xt_normal_arch1_q2_LB_test_ls[[cn]] <- Box.test(residuals(Xt_normal_arch1_q2_model_ls[[cn]]), 
                                                                           lag=max_lag, fitdf=LB_fitdf, type = "Ljung-Box")
          show(Xt_normal_arch1_q2_LB_test_ls[[cn]])
          Xt_normal_arch1_q2_Ext_LB_test_ls[[cn]] <- LjungBoxTest(residuals(Xt_normal_arch1_q2_model_ls[[cn]]),
                                                                                   lag.max=max_lag,  k=k, StartLag=1, SquaredQ=FALSE)
          show(Xt_normal_arch1_q2_Ext_LB_test_ls[[cn]])
          H_max_lag <-max(max_lag, k+3)
          Xt_normal_arch1_q2_HLB_test_ls[[cn]] <- Box.test(residuals(Xt_normal_arch1_q2_model_ls[[cn]]), 
                                                                            lag=H_max_lag, fitdf=k, type = "Ljung-Box")
          show(Xt_normal_arch1_q2_HLB_test_ls[[cn]])
          cat("  \n","  \n")
          Xt_normal_arch1_q2_ls[[cn]]  <- list(c(q_order_components, p_order_res_delay, omega, alpha1, alpha2, loglik_val, AIC_val, AICc_val, BIC_val, stationarity),
                                              Xt_normal_arch1_q2_model_ls[[cn]],
                                              Xt_normal_arch1_q2_LB_test_ls[[cn]],
                                                                Xt_normal_arch1_q2_HLB_test_ls[[cn]])
          cn <- cn+1
          cat("  \n","  \n")
        }, error = function(e){
          cat(red(sprintf("caught error: %s", e)))
          cat(red("Arch parameter p = ", p,","), red("Arch parameter q = ", q,","), red("a0 = ",a0,","),red("a1 = ",a1,","), red("a2 = ",a2,","))
          cat("\n")
          traceback(1, max.lines = 1)
          cat("  \n","  \n")
          Xt_normal_arch1_q2_err_war_mess <<- c(Xt_normal_arch1_q2_err_war_mess, list(c(e, paste("ERROR!... for counter cn = ",cn,",... Garch parameters p = ",p,", q = ",q,"."))))
          Xt_normal_arch1_q2_ls[[cn]] <<- list(c(e, paste("ERROR!... for counter cn = ",cn,",... Arch parameters p = ",p,", q = ",q,".")))
          cn <<- cn+1
        }, warning = function(w){
          cat(yellow(sprintf("caught warning: %s", w)))
          cat(yellow("Arch parameter p = ",p,","), yellow("Arch parameter q = ",q,","), yellow("a0 = ",a0,","), yellow("a1 = ",a1,","), yellow("a2 = ",a2,","))
          cat("\n")
          traceback(1, max.lines = 1)
          cat("  \n","  \n")
          Xt_normal_arch1_q2_err_war_mess <<- c(Xt_normal_arch1_q2_err_war_mess, list(c(w, paste("WARNING!... for counter cn = ",cn,",... Garch parameters p = ",p,", q = ",q,"."))))
          Xt_normal_arch1_q2_ls[[cn]] <<- list(c(w, paste("WARNING!... for counter cn = ",cn,",... Arch parameters p = ",p,", q = ",q,".")))
          cn <<- cn+1
        }
        )
      } 
    }
}
sink()
closeAllConnections()

num_estrazioni <- 72
#num_estrazioni <- length(Xt_t_student_asymmetric_arch_q1_ls)
# Estraiamo alcune informazioni dalla lista creata precedentemente
Xt_normal_arch1_q2_loglik_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_normal_arch1_q2_ls[[k]])==4) 
{Xt_normal_arch1_q2_loglik_values[k]<- Xt_normal_arch1_q2_ls[[k]][[1]][["loglik value"]]}}
show(Xt_normal_arch1_q2_loglik_values)
#
Xt_normal_arch1_q2_AICc_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_normal_arch1_q2_ls[[k]])==4) 
{Xt_normal_arch1_q2_AICc_values[k]<- Xt_normal_arch1_q2_ls[[k]][[1]][["AICc value"]]}}
show(Xt_normal_arch1_q2_AICc_values)
#
Xt_normal_arch1_q2_BIC_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_normal_arch1_q2_ls[[k]])==4) 
{Xt_normal_arch1_q2_BIC_values[k]<- Xt_normal_arch1_q2_ls[[k]][[1]][["BIC value"]]}}
show(Xt_normal_arch1_q2_BIC_values)
#
Xt_normal_arch1_q2_AIC_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_normal_arch1_q2_ls[[k]])==4) 
{Xt_normal_arch1_q2_AIC_values[k]<- Xt_normal_arch1_q2_ls[[k]][[1]][["AIC value"]]}}
show(Xt_normal_arch1_q2_AIC_values)
#
Xt_normal_arch1_q2_omega_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_normal_arch1_q2_ls[[k]])==4) 
{Xt_normal_arch1_q2_omega_params[k]<- Xt_normal_arch1_q2_ls[[k]][[1]][["omega parameter(a0)"]]}}
show(Xt_normal_arch1_q2_omega_params)
#
Xt_normal_arch1_q2_alpha1_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_normal_arch1_q2_ls[[k]])==4) 
{Xt_normal_arch1_q2_alpha1_params[k]<- Xt_normal_arch1_q2_ls[[k]][[1]][["alpha1 parameter(a1)"]]} }
show(Xt_normal_arch1_q2_alpha1_params)
#
Xt_normal_arch1_q2_alpha2_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_normal_arch1_q2_ls[[k]])==4) 
{Xt_normal_arch1_q2_alpha2_params[k]<- Xt_normal_arch1_q2_ls[[k]][[1]][["alpha2 parameter(a2)"]]} }
show(Xt_normal_arch1_q2_alpha2_params)
#
Xt_normal_arch1_q2_LB_p_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_normal_arch1_q2_ls[[k]])==4) 
{Xt_normal_arch1_q2_LB_p_values[k]<- Xt_normal_arch1_q2_ls[[k]][[3]][["p.value"]]}}
show(Xt_normal_arch1_q2_LB_p_values)
#
Xt_normal_arch1_q2_HLB_p_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_normal_arch1_q2_ls[[k]])==4) 
{Xt_normal_arch1_q2_HLB_p_values[k]<- Xt_normal_arch1_q2_ls[[k]][[4]][["p.value"]]}}
show(Xt_normal_arch1_q2_HLB_p_values)
#
Xt_normal_arch1_q2_stationarity <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_normal_arch1_q2_ls[[k]])==4) 
{Xt_normal_arch1_q2_stationarity[k]<- Xt_normal_arch1_q2_ls[[k]][[1]][["stationarity"]]}}
show(Xt_normal_arch1_q2_stationarity)
# We store the extracted information in a data frame
Xt_normal_arch1_q2_df <- data.frame(Index=1:num_estrazioni,
                                       loglik_value=Xt_normal_arch1_q2_loglik_values,
                                       AICc_value=Xt_normal_arch1_q2_AICc_values, 
                                       BIC_value=Xt_normal_arch1_q2_BIC_values,
                                       AIC_value=Xt_normal_arch1_q2_AIC_values,
                                       a0_param=Xt_normal_arch1_q2_omega_params,
                                       a1_param=Xt_normal_arch1_q2_alpha1_params,
                                       a2_param=Xt_normal_arch1_q2_alpha2_params,
                                       LB_p_value=Xt_normal_arch1_q2_LB_p_values,
                                       HLB_p_value=Xt_normal_arch1_q2_HLB_p_values,
                                       stationarity=Xt_normal_arch1_q2_stationarity)
# rimuoviamo i parametri che non rispettano la condizione di stazionarietà
Xt_normal_arch1_q2_df <- Xt_normal_arch1_q2_df[Xt_normal_arch1_q2_df$stationarity <= 1, ]
head(Xt_normal_arch1_q2_df, 20)
tail(Xt_normal_arch1_q2_df,10)

# We sort the models according to increasing AICc values.
Xt_normal_arch1_q2_AICc_sort_df <- Xt_normal_arch1_q2_df[order(Xt_normal_arch1_q2_df$AICc_value),]
rownames(Xt_normal_arch1_q2_AICc_sort_df) <- NULL
head(Xt_normal_arch1_q2_AICc_sort_df,20)
# We sort the models according to increasing BIC values.
Xt_normal_arch1_q2_BIC_sort_df <- Xt_normal_arch1_q2_df[order(Xt_normal_arch1_q2_df$BIC_value),]
rownames(Xt_normal_arch1_q2_BIC_sort_df) <- NULL
head(Xt_normal_arch1_q2_BIC_sort_df,20)
# We sort the models according to increasing AIC values.
Xt_normal_arch1_q2_AIC_sort_df <- Xt_normal_arch1_q2_df[order(Xt_normal_arch1_q2_df$AIC_value),]
rownames(Xt_normal_arch1_q2_AIC_sort_df) <- NULL
head(Xt_normal_arch1_q2_AIC_sort_df,20)

# Costruiamo il nuovo modello con i parametri stimati
a0 <- 0.01
aq <- c(0.001, 0.152)
Xt_normal_arch1_q2_new <- model_arch(a0, aq, X0, distribution, q)

# Consideriamo una traiettoia con distribuzione normale di un modello ARCH(2)
Xt <- Xt_normal_arch1_q2_new
df_Xt_normal_arch1_q2 <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df<- df_Xt_normal_arch1_q2
lenh <- nrow(Data_df)
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
x_labs <- paste(Data_df$t[x_breaks],Data_df$t[x_breaks])
J <- 0
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("Monthly Deaths")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Monthly Deaths")
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

# Consideriamo un modello lineare
Xt_normal_arch1_q2_lm <- lm(Xt~t, data=df_Xt_normal_arch1_q2)
summary(Xt_normal_arch1_q2_lm)
summary(Xt_normal_arch1_q2_lm$fitted.values)

Xt_normal_arch1_q2_res <- Xt_normal_arch1_q2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_arch1_q2_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_arch1_q2_res)                  # theoretical value 3.
# La skew è pari a -0.05283273; il suo valore è prossimo a zero, quindi la sua distribuzione potrebbe essere
# considerata approssimativamente simmetrica.
# La kurtosi è pari a 2.685138; questo indica che la distribuzione è leggermenete platykurtic.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_arch1_q2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(2) of a normal distribution")))
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
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Arch(2) of a normal distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Square root of absolute residuals")
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
# Si ha un p-value di 0.01156 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_arch1_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_arch1_q2)
show(Xt_normal_arch1_q2_w)
# Si ha un p-value di 0.03951 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

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
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano sempre entro 
# l'intervallo di confidenza.

# Test Ljiung-box
y <- Xt_normal_arch1_q2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 3.0659, df = 1, p-value = 0.07995
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag.

# In questo modello Arch(2) con distribuzione normale, risulta essere eteroschedastico e con assenza
# di autocorrelazione con i parametri stimati.

##########################################

# Consideriamo la seconda traiettoia con distribuzione t-student simmetrica di un modello ARCH(2)
Xt <- Xt_t_student_symmetric_arch2_q2
df_Xt_t_student_symmetric_arch1_q2 <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df<- df_Xt_t_student_symmetric_arch1_q2
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
y_name <- bquote("Monthly Deaths")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Monthly Deaths")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_arch1_q2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_arch1_q2_sp)
# Regression line e LOESS sembrano coincidere.

# Consideriamo un modello lineare
Xt_t_student_symmetric_arch1_q2_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_arch1_q2)
summary(Xt_t_student_symmetric_arch1_q2_lm)
summary(Xt_t_student_symmetric_arch1_q2_lm$fitted.values)

Xt_t_student_symmetric_arch1_q2_res <- Xt_t_student_symmetric_arch1_q2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_arch1_q2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_arch1_q2_res)                  # theoretical value 3.
# La skew è pari a 0.2677586; il suo valore è prossimo a zero, quindi la sua distribuzione potrebbe essere
# considerata approssimativamente simmetrica.
# La kurtosi è pari a 9.305962; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_arch1_q2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(2) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_symmetric_arch1_q2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_arch1_q2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_arch1_q2_res)
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
Xt_t_student_symmetric_arch1_q2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_arch1_q2_sp)

plot(Xt_t_student_symmetric_arch1_q2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_arch1_q2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_arch1_q2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_arch1_q2)
show(Xt_t_student_symmetric_arch1_q2_bp)
# Si ha un p-value di 0.05495 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_arch1_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_arch1_q2)
show(Xt_t_student_symmetric_arch1_q2_w)
# Si ha un p-value di 0.1056 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_arch1_q2_lm$residuals
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
y <- Xt_t_student_symmetric_arch1_q2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 1.1745, df = 1, p-value = 0.2785
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione.

# I risultati dei test hanno un p-value > 0.05; questo, significa che c'è assenza di autocorrelazione,
# poichè non possiamo rigettare l'ipotesi nulla.

# Il modello Arch(2) con distribuzione t-student simmetrica ha presenza di omoschedasticità.
# Dato che la serie è omoschedastico, proviamo a stimare i migliori parametri per il modello.
distribution <- dist_t_student_symmetric13
y <- c(0,distribution)
class(y)
head(y)
T <- length(y)
# 146
max_lag <- ceiling(min(10, T/4))   # Hyndman (for data without seasonality)
# We introduce some lists where to store the results of our procedure.
Xt_t_student_symmetric_arch1_q2_ls <- list()
Xt_t_student_symmetric_arch1_q2_model_ls <- list()
Xt_t_student_symmetric_arch1_q2_LB_test_ls <- list()
Xt_t_student_symmetric_arch1_q2_Ext_LB_test_ls <- list()
Xt_t_student_symmetric_arch1_q2_HLB_test_ls <- list()
Xt_t_student_symmetric_arch1_q2_err_war_mess <- list()
q_order_components <- vector(mode="list", length=1)
names(q_order_components) <- "q parameter" # Order of components
p_order_res_delay <- vector(mode="list", length=1)
names(p_order_res_delay) <- "p parameter" # Order of residuals delay
omega <- vector(mode="list", length=1)
names(omega) <- "omega parameter(a0)"
alpha1 <- vector(mode="list", length=1)
names(alpha1) <- "alpha1 parameter(a1)"
alpha2 <- vector(mode="list", length=1)
names(alpha2) <- "alpha2 parameter(a2)"
loglik_val <- vector(mode="list", length=1)
names(loglik_val) <- "loglik value"
AIC_val <- vector(mode="list", length=1)
names(AIC_val) <- "AIC value"
AICc_val <- vector(mode="list", length=1)
names(AICc_val) <- "AICc value"
BIC_val <- vector(mode="list", length=1)
names(BIC_val) <- "BIC value"
stationarity <- vector(mode="list", length=1)
names(stationarity) <- "stationarity"
closeAllConnections()
cn <- 1 # Setting a counter
con <- file("Arch(2) model of a symmetric t-student distribution")
sink(con, append=TRUE, type=c("output", "message"), split=TRUE)
q = 2
p = 0
# Looping over parameter omega
for(a0 in seq(0.01, 0.04, by = 0.002)){
  # Looping over parameter alpha1
  for(a1 in seq(0.01, 0.04, by = 0.002)){
    # Looping over parameter alpha1
    for(a2 in seq(0.01, 0.04, by = 0.002)){
      # ERROR and WARNINGS HANDLING
      tryCatch({
        q_order_components[[1]] <- q
        p_order_res_delay[[1]] <- p
        omega[[1]] <- a0
        alpha1[[1]] <- a1
        alpha2[[1]] <- a2
        sigmasquaredW <- var(distribution)
        stationarity[[1]] <- (a1+a2)*sigmasquaredW
        uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)), distribution.model= "std", fixed.pars = list(omega=a0, alpha1=a1, alpha2=a2))
        Xt_t_student_symmetric_arch1_q2_model_ls[[cn]] <- ugarchfit(spec = uspec, data = c(0,distribution))
        show(Xt_t_student_symmetric_arch1_q2_model_ls[[cn]])
        
        k <- length(coef(Xt_t_student_symmetric_arch1_q2_model_ls[[cn]])) + 1
        loglik_val[[1]] <- likelihood(Xt_t_student_symmetric_arch1_q2_model_ls[[cn]])
        AIC_val[[1]] <- infocriteria(Xt_t_student_symmetric_arch1_q2_model_ls[[cn]])
        AICc_val[[1]] <- AIC_val[[1]] +2*k*(k+1)/(n-k-1)
        BIC_val[[1]] <- AIC_val[[1]]+k*(log(n)-2)
        
        cat("\n")
        cat(blue("Arch parameter p = ",p,","), blue("Arch parameter q = ",q,","), blue("a0 = ",a0,","), blue("a1 = ",a1,","), blue("a2 = ",a2,","))
        cat("\n")
        cat(blue("AIC = ",round(AIC_val[[1]],3),","), blue("AICc = ",round(AICc_val[[1]],3),","), blue("BIC = ",round(BIC_val[[1]],3),","))
        cat("\n")
        
        LB_fitdf <- min(min(max_lag, k), max_lag-1)
        Xt_t_student_symmetric_arch1_q2_LB_test_ls[[cn]] <- Box.test(residuals(Xt_t_student_symmetric_arch1_q2_model_ls[[cn]]), 
                                                                        lag=max_lag, fitdf=LB_fitdf, type = "Ljung-Box")
        show(Xt_t_student_symmetric_arch1_q2_LB_test_ls[[cn]])
        Xt_t_student_symmetric_arch1_q2_Ext_LB_test_ls[[cn]] <- LjungBoxTest(residuals(Xt_t_student_symmetric_arch1_q2_model_ls[[cn]]),
                                                                                lag.max=max_lag,  k=k, StartLag=1, SquaredQ=FALSE)
        show(Xt_t_student_symmetric_arch1_q2_Ext_LB_test_ls[[cn]])
        H_max_lag <-max(max_lag, k+3)
        Xt_t_student_symmetric_arch1_q2_HLB_test_ls[[cn]] <- Box.test(residuals(Xt_t_student_symmetric_arch1_q2_model_ls[[cn]]), 
                                                                         lag=H_max_lag, fitdf=k, type = "Ljung-Box")
        show(Xt_t_student_symmetric_arch1_q2_HLB_test_ls[[cn]])
        cat("  \n","  \n")
        Xt_t_student_symmetric_arch1_q2_ls[[cn]]  <- list(c(q_order_components, p_order_res_delay, omega, alpha1, alpha2, loglik_val, AIC_val, AICc_val, BIC_val, stationarity),
                                                         Xt_t_student_symmetric_arch1_q2_model_ls[[cn]],
                                                         Xt_t_student_symmetric_arch1_q2_LB_test_ls[[cn]],
                                                         Xt_t_student_symmetric_arch1_q2_HLB_test_ls[[cn]])
        cn <- cn+1
        cat("  \n","  \n")
      }, error = function(e){
        cat(red(sprintf("caught error: %s", e)))
        cat(red("Arch parameter p = ", p,","), red("Arch parameter q = ", q,","), red("a0 = ",a0,","), red("a1 = ",a1,","), red("a2 = ",a2,","))
        cat("\n")
        traceback(1, max.lines = 1)
        cat("  \n","  \n")
        Xt_t_student_symmetric_arch1_q2_err_war_mess <<- c(Xt_t_student_symmetric_arch1_q2_err_war_mess, list(c(e, paste("ERROR!... for counter cn = ",cn,",... Arch parameters p = ",p,", q = ",q,"."))))
        Xt_t_student_symmetric_arch1_q2_ls[[cn]] <<- list(c(e, paste("ERROR!... for counter cn = ",cn,",... Arch parameters p = ",p,", q = ",q,".")))
        cn <<- cn+1
      }, warning = function(w){
        cat(yellow(sprintf("caught warning: %s", w)))
        cat(yellow("Arch parameter p = ",p,","), yellow("Arch parameter q = ",q,","), yellow("a0 = ",a0,","), yellow("a1 = ",a1,","), yellow("a2 = ",a2,","))
        cat("\n")
        traceback(1, max.lines = 1)
        cat("  \n","  \n")
        Xt_t_student_symmetric_arch1_q2_err_war_mess <<- c(Xt_t_student_symmetric_arch1_q2_err_war_mess, list(c(w, paste("WARNING!... for counter cn = ",cn,",... Arch parameters p = ",p,", q = ",q,"."))))
        Xt_t_student_symmetric_arch1_q2_ls[[cn]] <<- list(c(w, paste("WARNING!... for counter cn = ",cn,",... Arch parameters p = ",p,", q = ",q,".")))
        cn <<- cn+1
      }
      )
    } 
  }
}
sink()
closeAllConnections()

num_estrazioni <- 72
num_estrazioni <- length(Xt_t_student_symmetric_arch1_q2_ls)
# Estraiamo alcune informazioni dalla lista creata precedentemente
Xt_t_student_symmetric_arch1_q2_loglik_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_symmetric_arch1_q2_ls[[k]])==4) 
{Xt_t_student_symmetric_arch1_q2_loglik_values[k]<- Xt_t_student_symmetric_arch1_q2_ls[[k]][[1]][["loglik value"]]}}
show(Xt_t_student_symmetric_arch1_q2_loglik_values)
#
Xt_t_student_symmetric_arch1_q2_AICc_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_symmetric_arch1_q2_ls[[k]])==4) 
{Xt_t_student_symmetric_arch1_q2_AICc_values[k]<- Xt_t_student_symmetric_arch1_q2_ls[[k]][[1]][["AICc value"]]}}
show(Xt_t_student_symmetric_arch1_q2_AICc_values)
#
Xt_t_student_symmetric_arch1_q2_BIC_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_symmetric_arch1_q2_ls[[k]])==4) 
{Xt_t_student_symmetric_arch1_q2_BIC_values[k]<- Xt_t_student_symmetric_arch1_q2_ls[[k]][[1]][["BIC value"]]}}
show(Xt_t_student_symmetric_arch1_q2_BIC_values)
#
Xt_t_student_symmetric_arch1_q2_AIC_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_symmetric_arch1_q2_ls[[k]])==4) 
{Xt_t_student_symmetric_arch1_q2_AIC_values[k]<- Xt_t_student_symmetric_arch1_q2_ls[[k]][[1]][["AIC value"]]}}
show(Xt_t_student_symmetric_arch1_q2_AIC_values)
#
Xt_t_student_symmetric_arch1_q2_omega_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_symmetric_arch1_q2_ls[[k]])==4) 
{Xt_t_student_symmetric_arch1_q2_omega_params[k]<- Xt_t_student_symmetric_arch1_q2_ls[[k]][[1]][["omega parameter(a0)"]]}}
show(Xt_t_student_symmetric_arch1_q2_omega_params)
#
Xt_t_student_symmetric_arch1_q2_alpha1_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_symmetric_arch1_q2_ls[[k]])==4) 
{Xt_t_student_symmetric_arch1_q2_alpha1_params[k]<- Xt_t_student_symmetric_arch1_q2_ls[[k]][[1]][["alpha1 parameter(a1)"]]} }
show(Xt_t_student_symmetric_arch1_q2_alpha1_params)
#
Xt_t_student_symmetric_arch1_q2_alpha2_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_symmetric_arch1_q2_ls[[k]])==4) 
{Xt_t_student_symmetric_arch1_q2_alpha2_params[k]<- Xt_t_student_symmetric_arch1_q2_ls[[k]][[1]][["alpha2 parameter(a2)"]]} }
show(Xt_t_student_symmetric_arch1_q2_alpha2_params)
#
Xt_t_student_symmetric_arch1_q2_LB_p_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_symmetric_arch1_q2_ls[[k]])==4) 
{Xt_t_student_symmetric_arch1_q2_LB_p_values[k]<- Xt_t_student_symmetric_arch1_q2_ls[[k]][[3]][["p.value"]]}}
show(Xt_t_student_symmetric_arch1_q2_LB_p_values)
#
Xt_t_student_symmetric_arch1_q2_HLB_p_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_symmetric_arch1_q2_ls[[k]])==4) 
{Xt_t_student_symmetric_arch1_q2_HLB_p_values[k]<- Xt_t_student_symmetric_arch1_q2_ls[[k]][[4]][["p.value"]]}}
show(Xt_t_student_symmetric_arch1_q2_HLB_p_values)
#
Xt_t_student_symmetric_arch1_q2_stationarity <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_symmetric_arch1_q2_ls[[k]])==4) 
{Xt_t_student_symmetric_arch1_q2_stationarity[k]<- Xt_t_student_symmetric_arch1_q2_ls[[k]][[1]][["stationarity"]]}}
show(Xt_t_student_symmetric_arch1_q2_stationarity)
# We store the extracted information in a data frame
Xt_t_student_symmetric_arch1_q2_df <- data.frame(Index=1:num_estrazioni,
                                                    loglik_value=Xt_t_student_symmetric_arch1_q2_loglik_values,
                                                    AICc_value=Xt_t_student_symmetric_arch1_q2_AICc_values, 
                                                    BIC_value=Xt_t_student_symmetric_arch1_q2_BIC_values,
                                                    AIC_value=Xt_t_student_symmetric_arch1_q2_AIC_values,
                                                    a0_param=Xt_t_student_symmetric_arch1_q2_omega_params,
                                                    a1_param=Xt_t_student_symmetric_arch1_q2_alpha1_params,
                                                    a2_param=Xt_t_student_symmetric_arch1_q2_alpha2_params,
                                                    LB_p_value=Xt_t_student_symmetric_arch1_q2_LB_p_values,
                                                    HLB_p_value=Xt_t_student_symmetric_arch1_q2_HLB_p_values,
                                                    stationarity=Xt_t_student_symmetric_arch1_q2_stationarity)
# rimuoviamo i parametri che non rispettano la condizione di stazionarietà
Xt_t_student_symmetric_arch1_q2_df <- Xt_t_student_symmetric_arch1_q2_df[Xt_t_student_symmetric_arch1_q2_df$stationarity <= 1, ]
head(Xt_t_student_symmetric_arch1_q2_df, 20)
tail(Xt_t_student_symmetric_arch1_q2_df,10)

# We sort the models according to increasing AICc values.
Xt_t_student_symmetric_arch1_q2_AICc_sort_df <- Xt_t_student_symmetric_arch1_q2_df[order(Xt_t_student_symmetric_arch1_q2_df$AICc_value),]
rownames(Xt_t_student_symmetric_arch1_q2_AICc_sort_df) <- NULL
head(Xt_t_student_symmetric_arch1_q2_AICc_sort_df,20)
# We sort the models according to increasing BIC values.
Xt_t_student_symmetric_arch1_q2_BIC_sort_df <- Xt_t_student_symmetric_arch1_q2_df[order(Xt_t_student_symmetric_arch1_q2_df$BIC_value),]
rownames(Xt_t_student_symmetric_arch1_q2_BIC_sort_df) <- NULL
head(Xt_t_student_symmetric_arch1_q2_BIC_sort_df,20)
# We sort the models according to increasing AIC values.
Xt_t_student_symmetric_arch1_q2_AIC_sort_df <- Xt_t_student_symmetric_arch1_q2_df[order(Xt_t_student_symmetric_arch1_q2_df$AIC_value),]
rownames(Xt_t_student_symmetric_arch1_q2_AIC_sort_df) <- NULL
head(Xt_t_student_symmetric_arch1_q2_AIC_sort_df,20)

# Costruiamo il nuovo modello con i parametri stimati
a0 <- 0.04
aq <- c(0.04, 0.04)
Xt_t_student_symmetric_arch1_q2_new <- model_arch(a0, aq, X0, distribution, q)

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello ARCH(2)
Xt <- Xt_t_student_symmetric_arch1_q2_new
df_Xt_t_student_symmetric_arch1_q2 <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df<- df_Xt_t_student_symmetric_arch1_q2
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
y_name <- bquote("Monthly Deaths")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Monthly Deaths")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_arch1_q2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_arch1_q2_sp)

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello ARCH(2)
Xt <- Xt_t_student_symmetric_arch1_q2_new
df_Xt_t_student_symmetric_arch1_q2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_symmetric_arch1_q2_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_arch1_q2)
summary(Xt_t_student_symmetric_arch1_q2_lm)
summary(Xt_t_student_symmetric_arch1_q2_lm$fitted.values)

Xt_t_student_symmetric_arch1_q2_res <- Xt_t_student_symmetric_arch1_q2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_arch1_q2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_arch1_q2_res)                  # theoretical value 3.
# La skew è pari a -0.02348946; il suo valore è prossimo a zero, quindi la sua distribuzione potrebbe essere
# considerata approssimativamente simmetrica.
# La kurtosi è pari a 3.745636; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_arch1_q2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(2) of a symmetric t-student distribution")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("t")
y_name <- bquote("Linear Model Residuals")
Xt_t_student_symmetric_arch1_q2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_arch1_q2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_arch1_q2_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Arch(2) of a symmetric t-student distribution")))
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
Xt_t_student_symmetric_arch1_q2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_arch1_q2_sp)

plot(Xt_t_student_symmetric_arch1_q2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_arch1_q2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_arch1_q2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_arch1_q2)
show(Xt_t_student_symmetric_arch1_q2_bp)
# Si ha un p-value di 0.1586 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_arch1_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_arch1_q2)
show(Xt_t_student_symmetric_arch1_q2_w)
# Si ha un p-value di 0.2108 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_arch1_q2_lm$residuals
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
# una banda ristretta.

# Test Ljiung-box
y <- Xt_t_student_symmetric_arch1_q2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 2.0506, df = 1, p-value = 0.1521
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag.

# Il modello Arch(2) con distribuzione t-student simmetrica continua ad avere presenza di omoschedasticità
# nella serie anche con i nuovi parametri stimati con assenza di autocorrelazione.

##########################################

# Consideriamo la prima traiettoia con distribuzione t-student asimmetrica di un modello ARCH(2)
Xt <- Xt_t_student_asymmetric_arch1_q2
df_Xt_t_student_asymmetric_arch1_q2 <- data.frame(t = 1:length(Xt), X = Xt)

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
moments::skewness(Xt_t_student_asymmetric_arch1_q2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_arch1_q2_res)                  # theoretical value 3.
# La skew è pari a -1.876595; questo indica una forte asimmetria verso sinistra
# con una coda lunga negativa.
# La kurtosi è pari a 8.390935; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

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

# Il risultato del test haun p-value < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

# In questo modello Arch(2) con una distribuzione t-student asimmetrica si ha presenza di eteroschedasticità
# ma con presenza di autocorrelazione. 
# Quindi, proviamo a stimare i parametri che si adattano meglio al modello.
distribution <- dist_t_student_asymmetric
y <- c(0,distribution)
class(y)
head(y)
T <- length(y)
# 146
max_lag <- ceiling(min(10, T/4))   # Hyndman (for data without seasonality)
# We introduce some lists where to store the results of our procedure.
Xt_t_student_asymmetric_arch1_q2_ls <- list()
Xt_t_student_asymmetric_arch1_q2_model_ls <- list()
Xt_t_student_asymmetric_arch1_q2_LB_test_ls <- list()
Xt_t_student_asymmetric_arch1_q2_Ext_LB_test_ls <- list()
Xt_t_student_asymmetric_arch1_q2_HLB_test_ls <- list()
Xt_t_student_asymmetric_arch1_q2_err_war_mess <- list()
q_order_components <- vector(mode="list", length=1)
names(q_order_components) <- "q parameter" # Order of components
p_order_res_delay <- vector(mode="list", length=1)
names(p_order_res_delay) <- "p parameter" # Order of residuals delay
omega <- vector(mode="list", length=1)
names(omega) <- "omega parameter(a0)"
alpha1 <- vector(mode="list", length=1)
names(alpha1) <- "alpha1 parameter(a1)"
alpha2 <- vector(mode="list", length=1)
names(alpha2) <- "alpha2 parameter(a2)"
loglik_val <- vector(mode="list", length=1)
names(loglik_val) <- "loglik value"
AIC_val <- vector(mode="list", length=1)
names(AIC_val) <- "AIC value"
AICc_val <- vector(mode="list", length=1)
names(AICc_val) <- "AICc value"
BIC_val <- vector(mode="list", length=1)
names(BIC_val) <- "BIC value"
stationarity <- vector(mode="list", length=1)
names(stationarity) <- "stationarity"
closeAllConnections()
cn <- 1 # Setting a counter
con <- file("Arch(2) model of a asymmetric t-student distribution")
sink(con, append=TRUE, type=c("output", "message"), split=TRUE)
q = 2
p = 0
# Looping over parameter omega
for(a0 in seq(0.001, 0.01, by = 0.001)){
  # Looping over parameter alpha1
  for(a1 in seq(0.01, 0.02, by = 0.001)){
    # Looping over parameter alpha1
    for(a2 in seq(0.019, 0.025, by = 0.001)){
      # ERROR and WARNINGS HANDLING
      tryCatch({
        q_order_components[[1]] <- q
        p_order_res_delay[[1]] <- p
        omega[[1]] <- a0
        alpha1[[1]] <- a1
        alpha2[[1]] <- a2
        sigmasquaredW <- var(distribution)
        stationarity[[1]] <- (a1 + a2)*sigmasquaredW
        uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)), distribution.model= "sstd", fixed.pars = list(omega=a0, alpha1=a1, alpha2=a2))
        Xt_t_student_asymmetric_arch1_q2_model_ls[[cn]] <- ugarchfit(spec = uspec, data = c(0,distribution))
        show(Xt_t_student_asymmetric_arch1_q2_model_ls[[cn]])
        
        k <- length(coef(Xt_t_student_asymmetric_arch1_q2_model_ls[[cn]])) + 1
        loglik_val[[1]] <- likelihood(Xt_t_student_asymmetric_arch1_q2_model_ls[[cn]])
        AIC_val[[1]] <- infocriteria(Xt_t_student_asymmetric_arch1_q2_model_ls[[cn]])
        AICc_val[[1]] <- AIC_val[[1]] +2*k*(k+1)/(n-k-1)
        BIC_val[[1]] <- AIC_val[[1]]+k*(log(n)-2)
        
        cat("\n")
        cat(blue("Arch parameter p = ",p,","), blue("Arch parameter q = ",q,","), blue("a0 = ",a0,","), blue("a1 = ",a1,","), blue("a2 = ",a2, ","))
        cat("\n")
        cat(blue("AIC = ",round(AIC_val[[1]],3),","), blue("AICc = ",round(AICc_val[[1]],3),","), blue("BIC = ",round(BIC_val[[1]],3),","))
        cat("\n")
        
        LB_fitdf <- min(min(max_lag, k), max_lag-1)
        Xt_t_student_asymmetric_arch1_q2_LB_test_ls[[cn]] <- Box.test(residuals(Xt_t_student_asymmetric_arch1_q2_model_ls[[cn]]), 
                                                                         lag=max_lag, fitdf=LB_fitdf, type = "Ljung-Box")
        show(Xt_t_student_asymmetric_arch1_q2_LB_test_ls[[cn]])
        Xt_t_student_asymmetric_arch1_q2_Ext_LB_test_ls[[cn]] <- LjungBoxTest(residuals(Xt_t_student_asymmetric_arch1_q2_model_ls[[cn]]),
                                                                                 lag.max=max_lag,  k=k, StartLag=1, SquaredQ=FALSE)
        show(Xt_t_student_asymmetric_arch1_q2_Ext_LB_test_ls[[cn]])
        H_max_lag <-max(max_lag, k+3)
        Xt_t_student_asymmetric_arch1_q2_HLB_test_ls[[cn]] <- Box.test(residuals(Xt_t_student_asymmetric_arch1_q2_model_ls[[cn]]), 
                                                                          lag=H_max_lag, fitdf=k, type = "Ljung-Box")
        show(Xt_t_student_asymmetric_arch1_q2_HLB_test_ls[[cn]])
        cat("  \n","  \n")
        Xt_t_student_asymmetric_arch1_q2_ls[[cn]]  <- list(c(q_order_components, p_order_res_delay, omega, alpha1, alpha2, loglik_val, AIC_val, AICc_val, BIC_val, stationarity),
                                                          Xt_t_student_asymmetric_arch1_q2_model_ls[[cn]],
                                                          Xt_t_student_asymmetric_arch1_q2_LB_test_ls[[cn]],
                                                          Xt_t_student_asymmetric_arch1_q2_HLB_test_ls[[cn]])
        cn <- cn+1
        cat("  \n","  \n")
      }, error = function(e){
        cat(red(sprintf("caught error: %s", e)))
        cat(red("Arch parameter p = ", p,","), red("Arch parameter q = ", q,","), red("a0 = ",a0,","), red("a1 = ",a1,","), red("a2 = ",a2, ","))
        cat("\n")
        traceback(1, max.lines = 1)
        cat("  \n","  \n")
        Xt_t_student_asymmetric_arch1_q2_err_war_mess <<- c(Xt_t_student_asymmetric_arch1_q2_err_war_mess, list(c(e, paste("ERROR!... for counter cn = ",cn,",... Arch parameters p = ",p,", q = ",q,"."))))
        Xt_t_student_asymmetric_arch1_q2_ls[[cn]] <<- list(c(e, paste("ERROR!... for counter cn = ",cn,",... Arch parameters p = ",p,", q = ",q,".")))
        cn <<- cn+1
      }, warning = function(w){
        cat(yellow(sprintf("caught warning: %s", w)))
        cat(yellow("Arch parameter p = ",p,","), yellow("Arch parameter q = ",q,","), yellow("a0 = ",a0,","), yellow("a1 = ",a1,","), yellow("a2 = ",a2, ","))
        cat("\n")
        traceback(1, max.lines = 1)
        cat("  \n","  \n")
        Xt_t_student_asymmetric_arch1_q2_err_war_mess <<- c(Xt_t_student_asymmetric_arch1_q2_err_war_mess, list(c(w, paste("WARNING!... for counter cn = ",cn,",... Arch parameters p = ",p,", q = ",q,"."))))
        Xt_t_student_asymmetric_arch1_q2_ls[[cn]] <<- list(c(w, paste("WARNING!... for counter cn = ",cn,",... Arch parameters p = ",p,", q = ",q,".")))
        cn <<- cn+1
      }
      )
    } 
  }
}
sink()
closeAllConnections()

# num_estrazioni <- 72
num_estrazioni <- length(Xt_t_student_asymmetric_arch1_q2_ls)
# Estraiamo alcune informazioni dalla lista creata precedentemente
Xt_t_student_asymmetric_arch1_q2_loglik_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_arch1_q2_ls[[k]])==4) 
{Xt_t_student_asymmetric_arch1_q2_loglik_values[k]<- Xt_t_student_asymmetric_arch1_q2_ls[[k]][[1]][["loglik value"]]}}
show(Xt_t_student_asymmetric_arch1_q2_loglik_values)
#
Xt_t_student_asymmetric_arch1_q2_AICc_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_arch1_q2_ls[[k]])==4) 
{Xt_t_student_asymmetric_arch1_q2_AICc_values[k]<- Xt_t_student_asymmetric_arch1_q2_ls[[k]][[1]][["AICc value"]]}}
show(Xt_t_student_asymmetric_arch1_q2_AICc_values)
#
Xt_t_student_asymmetric_arch1_q2_BIC_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_arch1_q2_ls[[k]])==4) 
{Xt_t_student_asymmetric_arch1_q2_BIC_values[k]<- Xt_t_student_asymmetric_arch1_q2_ls[[k]][[1]][["BIC value"]]}}
show(Xt_t_student_asymmetric_arch1_q2_BIC_values)
#
Xt_t_student_asymmetric_arch1_q2_AIC_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_arch1_q2_ls[[k]])==4) 
{Xt_t_student_asymmetric_arch1_q2_AIC_values[k]<- Xt_t_student_asymmetric_arch1_q2_ls[[k]][[1]][["AIC value"]]}}
show(Xt_t_student_asymmetric_arch1_q2_AIC_values)
#
Xt_t_student_asymmetric_arch1_q2_omega_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_arch1_q2_ls[[k]])==4) 
{Xt_t_student_asymmetric_arch1_q2_omega_params[k]<- Xt_t_student_asymmetric_arch1_q2_ls[[k]][[1]][["omega parameter(a0)"]]}}
show(Xt_t_student_asymmetric_arch1_q2_omega_params)
#
Xt_t_student_asymmetric_arch1_q2_alpha1_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_arch1_q2_ls[[k]])==4) 
{Xt_t_student_asymmetric_arch1_q2_alpha1_params[k]<- Xt_t_student_asymmetric_arch1_q2_ls[[k]][[1]][["alpha1 parameter(a1)"]]} }
show(Xt_t_student_asymmetric_arch1_q2_alpha1_params)
#
Xt_t_student_asymmetric_arch1_q2_alpha2_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_arch1_q2_ls[[k]])==4) 
{Xt_t_student_asymmetric_arch1_q2_alpha2_params[k]<- Xt_t_student_asymmetric_arch1_q2_ls[[k]][[1]][["alpha2 parameter(a2)"]]} }
show(Xt_t_student_asymmetric_arch1_q2_alpha2_params)
#
Xt_t_student_asymmetric_arch1_q2_LB_p_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_arch1_q2_ls[[k]])==4) 
{Xt_t_student_asymmetric_arch1_q2_LB_p_values[k]<- Xt_t_student_asymmetric_arch1_q2_ls[[k]][[3]][["p.value"]]}}
show(Xt_t_student_asymmetric_arch1_q2_LB_p_values)
#
Xt_t_student_asymmetric_arch1_q2_HLB_p_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_arch1_q2_ls[[k]])==4) 
{Xt_t_student_asymmetric_arch1_q2_HLB_p_values[k]<- Xt_t_student_asymmetric_arch1_q2_ls[[k]][[4]][["p.value"]]}}
show(Xt_t_student_asymmetric_arch1_q2_HLB_p_values)
#
Xt_t_student_asymmetric_arch1_q2_stationarity <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_t_student_asymmetric_arch1_q2_ls[[k]])==4) 
{Xt_t_student_asymmetric_arch1_q2_stationarity[k]<- Xt_t_student_asymmetric_arch1_q2_ls[[k]][[1]][["stationarity"]]}}
show(Xt_t_student_asymmetric_arch1_q2_stationarity)
# We store the extracted information in a data frame
Xt_t_student_asymmetric_arch1_q2_df <- data.frame(Index=1:num_estrazioni,
                                                     loglik_value=Xt_t_student_asymmetric_arch1_q2_loglik_values,
                                                     AICc_value=Xt_t_student_asymmetric_arch1_q2_AICc_values, 
                                                     BIC_value=Xt_t_student_asymmetric_arch1_q2_BIC_values,
                                                     AIC_value=Xt_t_student_asymmetric_arch1_q2_AIC_values,
                                                     a0_param=Xt_t_student_asymmetric_arch1_q2_omega_params,
                                                     a1_param=Xt_t_student_asymmetric_arch1_q2_alpha1_params,
                                                     a2_param=Xt_t_student_asymmetric_arch1_q2_alpha2_params,
                                                     LB_p_value=Xt_t_student_asymmetric_arch1_q2_LB_p_values,
                                                     HLB_p_value=Xt_t_student_asymmetric_arch1_q2_HLB_p_values,
                                                     stationarity=Xt_t_student_asymmetric_arch1_q2_stationarity)
# rimuoviamo i parametri che non rispettano la condizione di stazionarietà
Xt_t_student_asymmetric_arch1_q2_df <- Xt_t_student_asymmetric_arch1_q2_df[Xt_t_student_asymmetric_arch1_q2_df$stationarity <= 1, ]
head(Xt_t_student_asymmetric_arch1_q2_df, 20)
tail(Xt_t_student_asymmetric_arch1_q2_df,10)

# We sort the models according to increasing AICc values.
Xt_t_student_asymmetric_arch1_q2_AICc_sort_df <- Xt_t_student_asymmetric_arch1_q2_df[order(Xt_t_student_asymmetric_arch1_q2_df$AICc_value),]
rownames(Xt_t_student_asymmetric_arch1_q2_AICc_sort_df) <- NULL
head(Xt_t_student_asymmetric_arch1_q2_AICc_sort_df,20)
# We sort the models according to increasing BIC values.
Xt_t_student_asymmetric_arch1_q2_BIC_sort_df <- Xt_t_student_asymmetric_arch1_q2_df[order(Xt_t_student_asymmetric_arch1_q2_df$BIC_value),]
rownames(Xt_t_student_asymmetric_arch1_q2_BIC_sort_df) <- NULL
head(Xt_t_student_asymmetric_arch1_q2_BIC_sort_df,20)
# We sort the models according to increasing AIC values.
Xt_t_student_asymmetric_arch1_q2_AIC_sort_df <- Xt_t_student_asymmetric_arch1_q2_df[order(Xt_t_student_asymmetric_arch1_q2_df$AIC_value),]
rownames(Xt_t_student_asymmetric_arch1_q2_AIC_sort_df) <- NULL
head(Xt_t_student_asymmetric_arch1_q2_AIC_sort_df,num_estrazioni)
# We sort the models according to increasing LB values.
Xt_t_student_asymmetric_arch1_q2_LB_sort_df <- Xt_t_student_asymmetric_arch1_q2_df[order(Xt_t_student_asymmetric_arch1_q2_df$LB_p_value, decreasing = TRUE),]
rownames(Xt_t_student_asymmetric_arch1_q2_LB_sort_df) <- NULL
head(Xt_t_student_asymmetric_arch1_q2_LB_sort_df,num_estrazioni)

# Costruiamo il nuovo modello con i parametri stimati
a0 <- 0.01
aq <- c(0.01, 0.019)
Xt_t_student_asymmetric_arch1_q2_new <- model_arch(a0, aq, X0, distribution, q)

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello ARCH(2)
Xt <- Xt_t_student_asymmetric_arch1_q2_new
df_Xt_t_student_asymmetric_arch1_q2 <- data.frame(t = 1:length(Xt), X = Xt)

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

# Consideriamo un modello lineare
Xt_t_student_asymmetric_arch1_q2_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_arch1_q2)
summary(Xt_t_student_asymmetric_arch1_q2_lm)
summary(Xt_t_student_asymmetric_arch1_q2_lm$fitted.values)

Xt_t_student_asymmetric_arch1_q2_res <- Xt_t_student_asymmetric_arch1_q2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_arch1_q2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_arch1_q2_res)                  # theoretical value 3.
# La skew è pari a -1.706085; questo indica una forte asimmetria verso sinistra
# con una coda lunga negativa.
# La kurtosi è pari a 7.356684; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

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
# Si ha un p-value di   0.01675 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_arch1_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_arch1_q2)
show(Xt_t_student_asymmetric_arch1_q2_w)
# Si ha un p-value di  0.0533 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

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
# nel grafico dell'autocorrelogramma possiamo notare che i valori rientrano nell'intervallo di confidenza.

# Test Ljiung-box
y <- Xt_t_student_asymmetric_arch1_q2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 3.7748, df = 1, p-value = 0.05203
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag.

# In questo modello Arch(2) con una distribuzione t-student asimmetrica si ha presenza di eteroschedasticità
# nel test di Breusch-Pagan ma presenza di omoschedasticità con il test di White; con i nuovi
# parametri stimati si ha assenza di autocorrelazione. 

##########################################
##########################################

# Consideriamo la prima traiettoia con distribuzione normale di un modello GARCH(2,1)
Xt <- Xt_normal_garch1_q2_p1
df_Xt_normal_garch1_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

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
Xt_normal_garch1_q2_p1_lm <- lm(Xt~t, data=df_Xt_normal_garch1_q2_p1)
summary(Xt_normal_garch1_q2_p1_lm)
summary(Xt_normal_garch1_q2_p1_lm$fitted.values)

Xt_normal_garch1_q2_p1_res <- Xt_normal_garch1_q2_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_garch1_q2_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_garch1_q2_p1_res)                  # theoretical value 3.
# La skew è pari a -0.06363277; il suo valore è prossimo a zero, quindi la sua distribuzione 
# viene considerata simmetrica con una coda negativa.
# La kurtosi è pari a 2.944331; questo indica che la distribuzione è leggermenete platykurtic.

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

# Il risultati indica una presenza di autocorrelazione nei residui del modello con i test Box-Ljung,
# poichè rifiutiamo l'ipotesi nulla di assenza di autocorrelazione in favore dell'alternatiava.

# Quindi, proviamo a stimare i parametri che si adattano meglio al modello.
distribution <- dist_normal1
y <- c(0,distribution)
class(y)
head(y)
T <- length(y)
# 146
max_lag <- ceiling(min(10, T/4))   # Hyndman (for data without seasonality)
# We introduce some lists where to store the results of our procedure.
Xt_normal_garch1_q2_p1_ls <- list()
Xt_normal_garch1_q2_p1_model_ls <- list()
Xt_normal_garch1_q2_p1_LB_test_ls <- list()
Xt_normal_garch1_q2_p1_Ext_LB_test_ls <- list()
Xt_normal_garch1_q2_p1_HLB_test_ls <- list()
Xt_normal_garch1_q2_p1_err_war_mess <- list()
q_order_components <- vector(mode="list", length=1)
names(q_order_components) <- "q parameter" # Order of components
p_order_res_delay <- vector(mode="list", length=1)
names(p_order_res_delay) <- "p parameter" # Order of residuals delay
omega <- vector(mode="list", length=1)
names(omega) <- "omega parameter(a0)"
alpha1 <- vector(mode="list", length=1)
names(alpha1) <- "alpha1 parameter(a1)"
alpha2 <- vector(mode="list", length=1)
names(alpha2) <- "alpha2 parameter(a2)"
beta1 <- vector(mode="list", length=1)
names(beta1) <- "beta1 parameter(b1)"
loglik_val <- vector(mode="list", length=1)
names(loglik_val) <- "loglik value"
AIC_val <- vector(mode="list", length=1)
names(AIC_val) <- "AIC value"
AICc_val <- vector(mode="list", length=1)
names(AICc_val) <- "AICc value"
BIC_val <- vector(mode="list", length=1)
names(BIC_val) <- "BIC value"
stationarity <- vector(mode="list", length=1)
names(stationarity) <- "stationarity"
closeAllConnections()
cn <- 1 # Setting a counter
con <- file("Garch(2,1) model with a normal distribution")
sink(con, append=TRUE, type=c("output", "message"), split=TRUE)
q = 2
p = 1
# Looping over parameter omega
for(a0 in seq(0.01, 0.02, by = 0.005)){
  # Looping over parameter alpha1
  for(a1 in seq(0.01, 0.02, by = 0.002)){
    # Looping over parameter beta1
    for(a2 in seq(0.01, 0.02, by = 0.002)){
      # Looping over parameter beta1
      for(b1 in seq(0.79, 0.99, by = 0.01)){
        # ERROR and WARNINGS HANDLING
        tryCatch({
          q_order_components[[1]] <- q
          p_order_res_delay[[1]] <- p
          omega[[1]] <- a0
          alpha1[[1]] <- a1
          alpha2[[1]] <- a2
          beta1[[1]] <- b1
          sigmasquaredW <- var(distribution)
          stationarity[[1]] <- (a1+a2)*sigmasquaredW + b1
          uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)), distribution.model= "norm", fixed.pars = list(omega=a0, alpha1=a1, alpha2=a2, beta1=b1))
          Xt_normal_garch1_q2_p1_model_ls[[cn]] <- ugarchfit(spec = uspec, data = c(0,distribution))
          show(Xt_normal_garch1_q2_p1_model_ls[[cn]])
          
          k <- length(coef(Xt_normal_garch1_q2_p1_model_ls[[cn]])) + 1
          loglik_val[[1]] <- likelihood(Xt_normal_garch1_q2_p1_model_ls[[cn]])
          AIC_val[[1]] <- infocriteria(Xt_normal_garch1_q2_p1_model_ls[[cn]])
          AICc_val[[1]] <- AIC_val[[1]] +2*k*(k+1)/(n-k-1)
          BIC_val[[1]] <- AIC_val[[1]]+k*(log(n)-2)
          
          cat("\n")
          cat(blue("Garch parameter p = ",p,","), blue("Garch parameter q = ",q,","), blue("a0 = ",a0,","), blue("a1 = ",a1,","), blue("a2= ",a2,","), blue("b1 = ",b1,","))
          cat("\n")
          cat(blue("AIC = ",round(AIC_val[[1]],3),","), blue("AICc = ",round(AICc_val[[1]],3),","), blue("BIC = ",round(BIC_val[[1]],3),","))
          cat("\n")
          
          LB_fitdf <- min(min(max_lag, k), max_lag-1)
          Xt_normal_garch1_q2_p1_LB_test_ls[[cn]] <- Box.test(residuals(Xt_normal_garch1_q2_p1_model_ls[[cn]]), 
                                                                           lag=max_lag, fitdf=LB_fitdf, type = "Ljung-Box")
          show(Xt_normal_garch1_q2_p1_LB_test_ls[[cn]])
          Xt_normal_garch1_q2_p1_Ext_LB_test_ls[[cn]] <- LjungBoxTest(residuals(Xt_normal_garch1_q2_p1_model_ls[[cn]]),
                                                                                   lag.max=max_lag,  k=k, StartLag=1, SquaredQ=FALSE)
          show(Xt_normal_garch1_q2_p1_Ext_LB_test_ls[[cn]])
          H_max_lag <-max(max_lag, k+3)
          Xt_normal_garch1_q2_p1_HLB_test_ls[[cn]] <- Box.test(residuals(Xt_normal_garch1_q2_p1_model_ls[[cn]]), 
                                                                            lag=H_max_lag, fitdf=k, type = "Ljung-Box")
          show(Xt_normal_garch1_q2_p1_HLB_test_ls[[cn]])
          cat("  \n","  \n")
          Xt_normal_garch1_q2_p1_ls[[cn]]  <- list(c(q_order_components, p_order_res_delay, omega, alpha1, alpha2, beta1, loglik_val, AIC_val, AICc_val, BIC_val, stationarity),
                                                                Xt_normal_garch1_q2_p1_model_ls[[cn]],
                                                                Xt_normal_garch1_q2_p1_LB_test_ls[[cn]],
                                                                Xt_normal_garch1_q2_p1_HLB_test_ls[[cn]])
          cn <- cn+1
          cat("  \n","  \n")
        }, error = function(e){
          cat(red(sprintf("caught error: %s", e)))
          cat(red("Garch parameter p = ", p,","), red("Garch parameter q = ", q,","), red("a0 = ",a0,","),red("a1 = ",a1,","), red("a2 = ",a2,","), red("b1 = ",b1,","))
          cat("\n")
          traceback(1, max.lines = 1)
          cat("  \n","  \n")
          Xt_normal_garch1_q2_p1_err_war_mess <<- c(Xt_normal_garch1_q2_p1_err_war_mess, list(c(e, paste("ERROR!... for counter cn = ",cn,",... Garch parameters p = ",p,", q = ",q,"."))))
          Xt_normal_garch1_q2_p1_ls[[cn]] <<- list(c(e, paste("ERROR!... for counter cn = ",cn,",... Garch parameters p = ",p,", q = ",q,".")))
          cn <<- cn+1
        }, warning = function(w){
          cat(yellow(sprintf("caught warning: %s", w)))
          cat(yellow("Garch parameter p = ",p,","), yellow("Garch parameter q = ",q,","), yellow("a0 = ",a0,","), yellow("a1 = ",a1,","), yellow("a2 = ",a2,","), yellow("b1 = ",b1,","))
          cat("\n")
          traceback(1, max.lines = 1)
          cat("  \n","  \n")
          Xt_normal_garch1_q2_p1_err_war_mess <<- c(Xt_normal_garch1_q2_p1_err_war_mess, list(c(w, paste("WARNING!... for counter cn = ",cn,",... Garch parameters p = ",p,", q = ",q,"."))))
          Xt_normal_garch1_q2_p1_ls[[cn]] <<- list(c(w, paste("WARNING!... for counter cn = ",cn,",... Arch parameters p = ",p,", q = ",q,".")))
          cn <<- cn+1
        }
        )
      } 
    }
  }
}
sink()
closeAllConnections()

num_estrazioni <- 72
num_estrazioni <- length(Xt_normal_garch1_q2_p1_ls)
# Estraiamo alcune informazioni dalla lista creata precedentemente
Xt_normal_garch1_q2_p1_loglik_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_normal_garch1_q2_p1_ls[[k]])==4) 
{Xt_normal_garch1_q2_p1_loglik_values[k]<- Xt_normal_garch1_q2_p1_ls[[k]][[1]][["loglik value"]]}}
show(Xt_normal_garch1_q2_p1_loglik_values)
#
Xt_normal_garch1_q2_p1_AICc_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_normal_garch1_q2_p1_ls[[k]])==4) 
{Xt_normal_garch1_q2_p1_AICc_values[k]<- Xt_normal_garch1_q2_p1_ls[[k]][[1]][["AICc value"]]}}
show(Xt_normal_garch1_q2_p1_AICc_values)
#
Xt_normal_garch1_q2_p1_BIC_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_normal_garch1_q2_p1_ls[[k]])==4) 
{Xt_normal_garch1_q2_p1_BIC_values[k]<- Xt_normal_garch1_q2_p1_ls[[k]][[1]][["BIC value"]]}}
show(Xt_normal_garch1_q2_p1_BIC_values)
#
Xt_normal_garch1_q2_p1_AIC_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_normal_garch1_q2_p1_ls[[k]])==4) 
{Xt_normal_garch1_q2_p1_AIC_values[k]<- Xt_normal_garch1_q2_p1_ls[[k]][[1]][["AIC value"]]}}
show(Xt_normal_garch1_q2_p1_AIC_values)
#
Xt_normal_garch1_q2_p1_omega_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_normal_garch1_q2_p1_ls[[k]])==4) 
{Xt_normal_garch1_q2_p1_omega_params[k]<- Xt_normal_garch1_q2_p1_ls[[k]][[1]][["omega parameter(a0)"]]}}
show(Xt_normal_garch1_q2_p1_omega_params)
#
Xt_normal_garch1_q2_p1_alpha1_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_normal_garch1_q2_p1_ls[[k]])==4) 
{Xt_normal_garch1_q2_p1_alpha1_params[k]<- Xt_normal_garch1_q2_p1_ls[[k]][[1]][["alpha1 parameter(a1)"]]} }
show(Xt_normal_garch1_q2_p1_alpha1_params)
#
Xt_normal_garch1_q2_p1_alpha2_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_normal_garch1_q2_p1_ls[[k]])==4) 
{Xt_normal_garch1_q2_p1_alpha2_params[k]<- Xt_normal_garch1_q2_p1_ls[[k]][[1]][["alpha2 parameter(a2)"]]} }
show(Xt_normal_garch1_q2_p1_alpha2_params)
#
Xt_normal_garch1_q2_p1_beta_params <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_normal_garch1_q2_p1_ls[[k]])==4) 
{Xt_normal_garch1_q2_p1_beta_params[k]<- Xt_normal_garch1_q2_p1_ls[[k]][[1]][["beta1 parameter(b1)"]]} }
show(Xt_normal_garch1_q2_p1_beta_params)
#
Xt_normal_garch1_q2_p1_LB_p_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_normal_garch1_q2_p1_ls[[k]])==4) 
{Xt_normal_garch1_q2_p1_LB_p_values[k]<- Xt_normal_garch1_q2_p1_ls[[k]][[3]][["p.value"]]}}
show(Xt_normal_garch1_q2_p1_LB_p_values)
#
Xt_normal_garch1_q2_p1_HLB_p_values <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_normal_garch1_q2_p1_ls[[k]])==4) 
{Xt_normal_garch1_q2_p1_HLB_p_values[k]<- Xt_normal_garch1_q2_p1_ls[[k]][[4]][["p.value"]]}}
show(Xt_normal_garch1_q2_p1_HLB_p_values)
#
Xt_normal_garch1_q2_p1_stationarity <- rep(NA,num_estrazioni)
for(k in 1:num_estrazioni){if(length(Xt_normal_garch1_q2_p1_ls[[k]])==4) 
{Xt_normal_garch1_q2_p1_stationarity[k]<- Xt_normal_garch1_q2_p1_ls[[k]][[1]][["stationarity"]]}}
show(Xt_normal_garch1_q2_p1_stationarity)
# We store the extracted information in a data frame
Xt_normal_garch1_q2_p1_df <- data.frame(Index=1:num_estrazioni,
                                                     loglik_value=Xt_normal_garch1_q2_p1_loglik_values,
                                                     AICc_value=Xt_normal_garch1_q2_p1_AICc_values, 
                                                     BIC_value=Xt_normal_garch1_q2_p1_BIC_values,
                                                     AIC_value=Xt_normal_garch1_q2_p1_AIC_values,
                                                     a0_param=Xt_normal_garch1_q2_p1_omega_params,
                                                     a1_param=Xt_normal_garch1_q2_p1_alpha1_params,
                                                     a2_param=Xt_normal_garch1_q2_p1_alpha2_params,
                                                     a1_param=Xt_normal_garch1_q2_p1_beta1_params,
                                                     LB_p_value=Xt_normal_garch1_q2_p1_LB_p_values,
                                                     HLB_p_value=Xt_normal_garch1_q2_p1_HLB_p_values,
                                                     stationarity=Xt_normal_garch1_q2_p1_stationarity)
# rimuoviamo i parametri che non rispettano la condizione di stazionarietà
Xt_normal_garch1_q2_p1_df <- Xt_normal_garch1_q2_p1_df[Xt_normal_garch1_q2_p1_df$stationarity < 1, ]
head(Xt_normal_garch1_q2_p1_df, 20)
tail(Xt_normal_garch1_q2_p1_df,10)

# We sort the models according to increasing AICc values.
Xt_normal_garch1_q2_p1_AICc_sort_df <- Xt_normal_garch1_q2_p1_df[order(Xt_normal_garch1_q2_p1_df$AICc_value),]
rownames(Xt_normal_garch1_q2_p1_AICc_sort_df) <- NULL
head(Xt_normal_garch1_q2_p1_AICc_sort_df,20)
# We sort the models according to increasing BIC values.
Xt_normal_garch1_q2_p1_BIC_sort_df <- Xt_normal_garch1_q2_p1_df[order(Xt_normal_garch1_q2_p1_df$BIC_value),]
rownames(Xt_normal_garch1_q2_p1_BIC_sort_df) <- NULL
head(Xt_normal_garch1_q2_p1_BIC_sort_df,20)
# We sort the models according to increasing AIC values.
Xt_normal_garch1_q2_p1_AIC_sort_df <- Xt_normal_garch1_q2_p1_df[order(Xt_normal_garch1_q2_p1_df$AIC_value),]
rownames(Xt_normal_garch1_q2_p1_AIC_sort_df) <- NULL
head(Xt_normal_garch1_q2_p1_AIC_sort_df,20)

# Costruiamo il nuovo modello con i parametri stimati
a0 <- 0.02
aq <- c(0.01, 0.012)
b1 <- 0.96
Xt_normal_garch1_q2_p1_new <- model_garch(a0, aq, b1, X0, sigmasquared0, distribution, q, p)

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(2,1)
Xt <- Xt_normal_garch1_q2_p1_new
df_Xt_normal_garch1_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df <- df_Xt_normal_garch1_q2_p1
lenh <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Line plot of the model Garch(2,1) with a normal distribution")))
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
Xt_normal_garch1_q2_p1_sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch1_q2_p1_sp)

# Consideriamo un modello lineare
Xt_normal_garch1_q2_p1_lm <- lm(Xt~t, data=df_Xt_normal_garch1_q2_p1)
summary(Xt_normal_garch1_q2_p1_lm)
summary(Xt_normal_garch1_q2_p1_lm$fitted.values)

Xt_normal_garch1_q2_p1_res <- Xt_normal_garch1_q2_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_garch1_q2_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_garch1_q2_p1_res)                  # theoretical value 3.
# La skew è pari a -0.05641253; il suo valore è prossimo a zero, quindi la sua distribuzione 
# viene considerata simmetrica con una coda negativa.
# La kurtosi è pari a 3.008423; questo indica che la distribuzione è leggermenete leptocurtica.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch1_q2_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,1) of a normal distribution")))
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
# Si ha un p-value di 2.103e-09 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch1_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch1_q2_p1)
show(Xt_normal_garch1_q2_p1_w)
# Si ha un p-value di 1.613e-08 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

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

# Test Ljiung-box
y <- Xt_normal_garch1_q2_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 1.9039, df = 1, p-value = 0.1676
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione.

# In questo modello Garch(2,1) con una distribuzione normale e con i nuovi parametri stimati
# ha evidenza di eteroschedasticità e assenza di autocorrelazione.

##########################################

# Consideriamo la seconda traiettoia con distribuzione t-student simmetrica di un modello GARCH(2,1)
Xt <- Xt_t_student_symmetric_garch2_q2_p1
df_Xt_t_student_symmetric_garch1_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df<- df_Xt_t_student_symmetric_garch1_q2_p1
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
y_name <- bquote("Monthly Deaths")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Monthly Deaths")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_garch1_q2_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch1_q2_p1_sp)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch1_q2_p1_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch1_q2_p1)
summary(Xt_t_student_symmetric_garch1_q2_p1_lm)
summary(Xt_t_student_symmetric_garch1_q2_p1_lm$fitted.values)

Xt_t_student_symmetric_garch1_q2_p1_res <- Xt_t_student_symmetric_garch1_q2_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_garch1_q2_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_garch1_q2_p1_res)                  # theoretical value 3.
# La skew è pari a 0.3829596; il suo valore è vicino allo zero; quindi, la sua distribuzione
# è vicina alla simmetria.
# La kurtosi è pari a 11.59399; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch1_q2_p1_res)
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
Xt_t_student_symmetric_garch1_q2_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch1_q2_p1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch1_q2_p1_res)
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
Xt_t_student_symmetric_garch1_q2_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch1_q2_p1_sp)

plot(Xt_t_student_symmetric_garch1_q2_p1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_garch1_q2_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch1_q2_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch1_q2_p1)
show(Xt_t_student_symmetric_garch1_q2_p1_bp)
# Si ha un p-value di 0.06841 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch1_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch1_q2_p1)
show(Xt_t_student_symmetric_garch1_q2_p1_w)
# Si ha un p-value di 0.07795 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_garch1_q2_p1_lm$residuals
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
y <- Xt_t_student_symmetric_garch1_q2_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 1.5367, df = 1, p-value = 0.2151
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag
# eccetto nel lag 17 e 19.

# Il risultati indica una assenza di autocorrelazione nei residui del modello.

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
Xt_t_student_symmetric_garch1_q2_p1_new <- model_garch(a0, aq, b1, X0, sigmasquared0, distribution, q, p)

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello GARCH(2,1)
Xt <- Xt_t_student_symmetric_garch1_q2_p1_new
df_Xt_t_student_symmetric_garch1_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df<- df_Xt_t_student_symmetric_garch1_q2_p1
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
y_name <- bquote("Monthly Deaths")
y_breaks_num <- 10
y_binwidth <- round((max(Data_df$X)-min(Data_df$X))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$X)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$X)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
y1_col <- bquote("Monthly Deaths")
y2_col <- bquote("LOESS curve")
y3_col <- bquote("regression line")
leg_labs   <- c(y1_col, y2_col, y3_col)
leg_cols   <- c("y1_col"="blue", "y2_col"="red", "y3_col"="green")
leg_breaks <- c("y1_col", "y2_col", "y3_col")
Xt_t_student_symmetric_garch1_q2_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch1_q2_p1_sp)
# La retta della regression line è leggermente inclinata e la retta della LOESS
# tende coincide quasi con la regression line.

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch1_q2_p1_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch1_q2_p1)
summary(Xt_t_student_symmetric_garch1_q2_p1_lm)
summary(Xt_t_student_symmetric_garch1_q2_p1_lm$fitted.values)

Xt_t_student_symmetric_garch1_q2_p1_res <- Xt_t_student_symmetric_garch1_q2_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_garch1_q2_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_garch1_q2_p1_res)                  # theoretical value 3.
# La skew è pari a -0.254513; il suo valore è vicino allo zero; quindi, la sua distribuzione
# è vicina alla simmetria.
# La kurtosi è pari a 4.981316; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch1_q2_p1_res)
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
Xt_t_student_symmetric_garch1_q2_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch1_q2_p1_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch1_q2_p1_res)
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
Xt_t_student_symmetric_garch1_q2_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch1_q2_p1_sp)
# Dal grafico è possibile notare che c'è evidenza di eteroschedasticità nei residui
# del modello

plot(Xt_t_student_symmetric_garch1_q2_p1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_garch1_q2_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch1_q2_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch1_q2_p1)
show(Xt_t_student_symmetric_garch1_q2_p1_bp)
# Si ha un p-value di 8.935e-07 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch1_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch1_q2_p1)
show(Xt_t_student_symmetric_garch1_q2_p1_w)
# Si ha un p-value di 1.682e-06 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram
y <- Xt_t_student_symmetric_garch1_q2_p1_lm$residuals
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
# I valori nel lag 15 non rientrono nell'intervallo di confidenza con un livello
# di significatività del 99%

# Test Ljiung-box
y <- Xt_t_student_symmetric_garch1_q2_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 0.45221, df = 1, p-value = 0.5013
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag
# eccetto nel lag 15.

# Con la stima dei nuovi parametri, il modello Garch(2,1) con distribuzione t-student
# simmetrica ha presenza di eteroschedasticità nel modello e assenza di autocorrelazione
# eccetto per il lag 15

##########################################

# Consideriamo la prima traiettoia con distribuzione t-student asimmetrica di un modello GARCH(2,1)
Xt <- Xt_t_student_asymmetric_garch1_q2_p1
df_Xt_t_student_asymmetric_garch1_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

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
moments::skewness(Xt_t_student_asymmetric_garch1_q2_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_garch1_q2_p1_res)                  # theoretical value 3.
# La skew è pari a -2.055272; il suo valore è negativo e la sua distribuzione è
# asimmetrica verso sinistra con una coda lunga negativa.
# La kurtosi è pari a 9.592723; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

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

# Test Durbin-Watson
dwtest(Xt_t_student_asymmetric_garch1_q2_p1_lm, alternative="two.sided")
# DW = 1.5564, p-value = 5.321e-07

# I risultati hanno un p-value < 0.05; ciò indica
# un'evidenza di autocorrelazione nei residui del modello.

# In questo modello Garch(2,1) con una distribuzione t-student asimmetrica si ha presenza
# di omoschedasticità nel test di White e presenza di autocorrelazione.
# Quindi, proviamo a stimare i parametri che si adattano meglio al modello.
q <- 2
p <- 1
distribution <- dist_t_student_asymmetric
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(q,p)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = distribution)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(distribution)
print((aq[1]+aq[2])*sigmasquaredW + b1)
Xt_t_student_asymmetric_garch1_q2_p1_new <- model_garch(a0, aq, b1, X0, sigmasquared0, distribution, q, p)

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello GARCH(2,1)
Xt <- Xt_t_student_asymmetric_garch1_q2_p1_new
df_Xt_t_student_asymmetric_garch1_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df<- df_Xt_t_student_asymmetric_garch1_q2_p1
lenh <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Line plot of the model Garch(2,1) with a asymmetric t-student distribution")))
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
df_Xt_t_student_asymmetric_garch1_q2_p1_sp <- ggplot(Data_df) +
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
plot(df_Xt_t_student_asymmetric_garch1_q2_p1_sp)
# Dal grafico notiamo che la regression line è inclinata e la LOESS oscilla
# leggermente intorno a questa retta

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch1_q2_p1_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch1_q2_p1)
summary(Xt_t_student_asymmetric_garch1_q2_p1_lm)
summary(Xt_t_student_asymmetric_garch1_q2_p1_lm$fitted.values)

Xt_t_student_asymmetric_garch1_q2_p1_res <- Xt_t_student_asymmetric_garch1_q2_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_garch1_q2_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_garch1_q2_p1_res)                  # theoretical value 3.
# La skew è pari a -1.812047; il suo valore è negativo e la sua distribuzione è
# asimmetrica verso sinistra con una coda lunga negativa.
# La kurtosi è pari a 8.243179; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

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
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(2,1) of a asymmetric t-student distribution")))
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
# Possiamo notare una evidenza di eteroschedasticità nei residui del modello

plot(Xt_t_student_asymmetric_garch1_q2_p1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_garch1_q2_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q2_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q2_p1)
show(Xt_t_student_asymmetric_garch1_q2_p1_bp)
# Si ha un p-value di   0.000104 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch1_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch1_q2_p1)
show(Xt_t_student_asymmetric_garch1_q2_p1_w)
# Si ha un p-value di   0.000536 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

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

# Test Ljiung-box
y <- Xt_t_student_asymmetric_garch1_q2_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 4.0259, df = 1, p-value = 0.04481
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag
# eccetto nel lag 1.

# Con i parametri stimati, il modello Garch(2,1) con distribuzione t-student asimmetrica
# ha presenza di eteroschedasticità nei residui del modello e assenza di autocorrelazione
# eccetto nel lag 1 in cui il p-value < 0.05.

##########################################
##########################################

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(2,2)
Xt <- Xt_normal_garch1_q2_p2
df_Xt_normal_garch1_q2_p2 <- data.frame(t = 1:length(Xt), X = Xt)

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
moments::skewness(Xt_normal_garch1_q2_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_garch1_q2_p2_res)                  # theoretical value 3.
# La skew è pari a -0.07990184; il suo valore è prossimo a zero, quindi la sua distribuzione 
# viene considerata simmetrica.
# La kurtosi è pari a 2.925623; questo indica che la distribuzione è platykurtic

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
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# La forma estesa del test di Ljung-Box conferma che c'è assenza di correlazione in tutti i lag
# eccetto nel lag 1.

# Il modello Garch(2,2) con una distribuzione normale ha evidenza di eteroschedasticità
# nei residui del modello e assenza di autocorrelazione eccetto per il lag 1.

##########################################

# Consideriamo la seconda traiettoia con distribuzione t-student simmetrica di un modello GARCH(2,2)
Xt <- Xt_t_student_symmetric_garch2_q2_p2
df_Xt_t_student_symmetric_garch1_q2_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df<- df_Xt_t_student_symmetric_garch1_q2_p2
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
df_Xt_t_student_symmetric_garch1_q2_p2_sp <- ggplot(Data_df) +
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
plot(df_Xt_t_student_symmetric_garch1_q2_p2_sp)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch1_q2_p2_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch1_q2_p2)
summary(Xt_t_student_symmetric_garch1_q2_p2_lm)
summary(Xt_t_student_symmetric_garch1_q2_p2_lm$fitted.values)

Xt_t_student_symmetric_garch1_q2_p2_res <- Xt_t_student_symmetric_garch1_q2_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_garch1_q2_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_garch1_q2_p2_res)                  # theoretical value 3.
# La skew è pari a 0.3197632; il suo valore è prossima allo zero, quindi possiamo dire 
# che la sua distribuzione è simmetrica.
# La kurtosi è pari a 10.48433; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch1_q2_p2_res)
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
Xt_t_student_symmetric_garch1_q2_p2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch1_q2_p2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch1_q2_p2_res)
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
Xt_t_student_symmetric_garch1_q2_p2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch1_q2_p2_sp)

plot(Xt_t_student_symmetric_garch1_q2_p2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_garch1_q2_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch1_q2_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch1_q2_p2)
show(Xt_t_student_symmetric_garch1_q2_p2_bp)
# Si ha un p-value di 0.08959 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch1_q2_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch1_q2_p2)
show(Xt_t_student_symmetric_garch1_q2_p2_w)
# Si ha un p-value di 0.08332 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_garch1_q2_p2_lm$residuals
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
y <- Xt_t_student_symmetric_garch1_q2_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 1.0338, df = 1, p-value = 0.3093
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# Nella forma estesa del test di Ljung-Box si hanno alcuni lag con un p-value < 0.05
# questo indica una possibile presenza di autocorrelazione

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
aq <- c(coef(fit)[['alpha1']] - tasso_di_errore, coef(fit)[['alpha2']])
bp <- c(coef(fit)[['beta1']], coef(fit)[['beta2']])
sigmasquaredW <- var(distribution)
print((aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_symmetric_garch1_q2_p2_new <- model_garch(a0, aq, bp, X0, sigmasquared0, distribution, q, p)

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello GARCH(2,2)
Xt <- Xt_t_student_symmetric_garch1_q2_p2_new
df_Xt_t_student_symmetric_garch1_q2_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Line plot
Data_df<- df_Xt_t_student_symmetric_garch1_q2_p2
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
df_Xt_t_student_symmetric_garch1_q2_p2_sp <- ggplot(Data_df) +
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
plot(df_Xt_t_student_symmetric_garch1_q2_p2_sp)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch1_q2_p2_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch1_q2_p2)
summary(Xt_t_student_symmetric_garch1_q2_p2_lm)
summary(Xt_t_student_symmetric_garch1_q2_p2_lm$fitted.values)

Xt_t_student_symmetric_garch1_q2_p2_res <- Xt_t_student_symmetric_garch1_q2_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_garch1_q2_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_garch1_q2_p2_res)                  # theoretical value 3.
# La skew è pari a -0.2358238; il suo valore è prossima allo zero, quindi possiamo dire 
# che la sua distribuzione è simmetrica.
# La kurtosi è pari a 4.609185; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch1_q2_p2_res)
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
Xt_t_student_symmetric_garch1_q2_p2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch1_q2_p2_sp)

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch1_q2_p2_res)
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
Xt_t_student_symmetric_garch1_q2_p2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch1_q2_p2_sp)
# Si nota una evidenza di eteroschedasticità nei residui del modello

plot(Xt_t_student_symmetric_garch1_q2_p2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_garch1_q2_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch1_q2_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch1_q2_p2)
show(Xt_t_student_symmetric_garch1_q2_p2_bp)
# Si ha un p-value di 0.0001872 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch1_q2_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch1_q2_p2)
show(Xt_t_student_symmetric_garch1_q2_p2_w)
# Si ha un p-value di 9.667e-05 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_garch1_q2_p2_lm$residuals
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
y <- Xt_t_student_symmetric_garch1_q2_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 0.39836, df = 1, p-value = 0.5279
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# Nella forma estesa del test di Ljung-Box si ha assenza di autocorrelazione

# Con i nuovi parametri stimati, nel modello Garch(2,2) con una distribuzione t-student simmetrica
# si ha presenza di eteroschedasticità nei residui del modello e assenza di autocorrelazione.

##########################################

# Consideriamo la prima traiettoia con distribuzione t-student asimmetrica di un modello GARCH(2,2)
Xt <- Xt_t_student_asymmetric_garch1_q2_p2
df_Xt_t_student_asymmetric_garch1_q2_p2 <- data.frame(t = 1:length(Xt), X = Xt)

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
moments::skewness(Xt_t_student_asymmetric_garch1_q2_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_garch1_q2_p2_res)                  # theoretical value 3.
# La skew è pari a -1.959105; il suo valore è negativo e la sua distribuzione è
# asimmetrica verso sinistra con una coda lunga negativa.
# La kurtosi è pari a 8.887091; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

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
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# Nella forma estesa del test di Ljung-Box si ha presenza di autocorrelazione

# In questo modello Garch(2,2) con distribuzione t-student asimmetrica si ha presenza di 
# eteroschedasticità nel modello e presenza di autocorrelazione.
# Proviamo a stimare i migliori parametri che si adattano meglio al modello.
distribution <- dist_t_student_asymmetric
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
sigmasquaredW <- var(dist_t_student_asymmetric)
print((aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_asymmetric_garch1_q2_p2_new <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_asymmetric, q, p)

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello GARCH(2,2)
Xt <- Xt_t_student_asymmetric_garch1_q2_p2_new
df_Xt_t_student_asymmetric_garch1_q2_p2 <- data.frame(t = 1:length(Xt), X = Xt)

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
moments::skewness(Xt_t_student_asymmetric_garch1_q2_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_garch1_q2_p2_res)                  # theoretical value 3.
# La skew è pari a -1.812057; il suo valore è negativo e la sua distribuzione è
# asimmetrica verso sinistra con una coda lunga negativa.
# La kurtosi è pari a 8.243387; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

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
# Dal rafico è possibile notare evidenza di eteroschedasticità nei residui de modello

plot(Xt_t_student_asymmetric_garch1_q2_p2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_garch1_q2_p2_lm,3) # Scale-location

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
# Adesso, possiamo notare che i valori rientrano dentro l'intervallo di confidenza

# Test Ljiung-box
y <- Xt_t_student_asymmetric_garch1_q2_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 4.0227, df = 1, p-value = 0.04489
# Consideriamo la forma estesa:
T <- length(y)
n_pars <- 0  # numbers of parameters/ or degrees of freedom estimated in the model (Hyndman)
max_lag <- ceiling(min(2*12,T/5)) # Hyndman https://robjhyndman.com/hyndsight/ljung-box-test/
LjungBoxTest(y, lag.max=max_lag, k=n_pars, StartLag=1, SquaredQ=FALSE)
# Nella forma estesa del test di Ljung-Box si ha presenza di autocorrelazione eccetto nel lag 1

# Con i nuovi parametri stimati, rispetto al modello precedente, si ha un modello Garch(2,2)
# con una distribuzione t-student asimmetrica che ha evidenza di eteroschedasticità nel modello
# e assenza di autocorrelazione

#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################
