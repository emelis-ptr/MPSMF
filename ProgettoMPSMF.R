"Progetto di Metodi Probabilistici e Statistici per i Mercati Finanziari

Il modello ARCH è un modello 'Autoregressive Conditional Heteroskedasticity' assume 
che la varianza del termine corrente sia una funzione non lineare della varianza dei rumori precedenti. 
Un aspetto rilevante dei processi ARCH è la capacità di modellare il clustering della volatilità; 
questo significa che le serie storiche finanziarie possono mostrare periodi in cui la volatilità è
elevata e seguita da periodi di volatilità relativamente bassa. 

Il modello GARCH è modello 'Generalized Autoregressive Conditional Heteroskedasticity' ed 
è un'estensione del modello ARCH che consentedi modellare la varianza del processo in base non solo 
ai valori precedenti del processo, ma anche ai valori precedenti della varianza.
"

author_content <- "Author: Melissa Petrolo"
content <- "University of Roma \"Tor Vergata\" - \u0040 MPSMF 2022-2023"

# verifica se il pacchetto è già stato installato
if (!requireNamespace("skewtDist", quietly = TRUE)) {
  devtools::install_github("dan9401/skewtDist")
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

##########################################################################################################################
##########################################################################################################################
"Genero tre diverse traiettorie per tre distribuzioni differenti: 
t-student simmetrica, t-student asimmetrica, distribuzione normale"
samples <- 10000 # numero di campioni
n <- samples -1  # numero di campioni meno X0
df <- 5 # gradi di libertà

########## DISTRIBUZIONE NORMALE
set.seed(10)
dist_normal <- rnorm(n = n, mean = 0, sd = 1)
set.seed(20)
dist_normal1 <- rnorm(n = n, mean = 0, sd = 1)
set.seed(30)
dist_normal2 <- rnorm(n = n, mean = 0, sd = 1)

type_dist = "Normal Distribution"
title_content <- bquote(atop(.(content), paste("Histogram of ", .(n) ," samples generated from the ", .(type_dist))))

subtitle_content <- bquote(paste("First sample of the ", .(type_dist)))
# creo il primo plot dei campioni generati dalla distribuzione normale
hist_dist_normal <- ggplot(data.frame(value = dist_normal), aes(x = value)) + 
  geom_histogram(binwidth = 0.2, alpha = 0.7) +
  labs(subtitle=subtitle_content, caption=" ") +
  xlab("Samples") + ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

subtitle_content <- bquote(paste("Second sample of the ", .(type_dist)))
# creo il secondo plot dei campioni generati dalla distribuzione normale
hist_dist_normal1 <- ggplot(data.frame(value = dist_normal1), aes(x = value)) + 
  geom_histogram(binwidth = 0.2, alpha = 0.7) +
  labs(subtitle=subtitle_content, caption=" ") +
  xlab("Samples") + ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

subtitle_content <- bquote(paste("Third sample of the ", .(type_dist)))
# creo il terzo plot dei campioni generati dalla distribuzione normale
hist_dist_normal2 <- ggplot(data.frame(value = dist_normal2), aes(x = value)) + 
  geom_histogram(binwidth = 0.2, alpha = 0.7) +
  labs(subtitle=subtitle_content, caption=author_content) +
  xlab("Samples") + ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_norm <- plot_grid(hist_dist_normal, hist_dist_normal1, hist_dist_normal2, ncol = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
final_plot_dist_norm <- grid.arrange(title_content_gtable, plots_dist_norm, ncol = 1, heights = c(0.2, 1))

########## DISTRIBUZIONE T-STUDENT SIMMETRICA
set.seed(10)
dist_t_student_symmetric <- rt(n = n, df = df)
set.seed(20)
dist_t_student_symmetric1 <- rt(n = n, df = df)
set.seed(30)
dist_t_student_symmetric2 <- rt(n = n, df = df)

type_dist = "Symmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Histogram of ", .(n) ," samples generated from the ", .(type_dist))))

subtitle_content <- bquote(paste("First sample of the ", .(type_dist)))
# creo il primo plot dei campioni generati dalla distribuzione t-student simmetrica
hist_dist_symmetric <- ggplot(data.frame(value = dist_t_student_symmetric), aes(x = value)) + 
  geom_histogram(binwidth = 0.2, alpha = 0.7) +
  labs(subtitle=subtitle_content, caption=" ") +
  xlab("Samples") + ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

subtitle_content <- bquote(paste("Second sample of the ", .(type_dist)))
# creo il secondo plot dei campioni generati dalla distribuzione t-student simmetrica
hist_dist_symmetric1 <- ggplot(data.frame(value = dist_t_student_symmetric1), aes(x = value)) + 
  geom_histogram(binwidth = 0.2, alpha = 0.7) +
  labs(subtitle=subtitle_content, caption=" ") +
  xlab("Samples") + ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

subtitle_content <- bquote(paste("Third sample of the ", .(type_dist)))
# creo il terzo plot dei campioni generati dalla distribuzione t-student simmetrica
hist_dist_symmetric2 <- ggplot(data.frame(value = dist_t_student_symmetric2), aes(x = value)) + 
  geom_histogram(binwidth = 0.2, alpha = 0.7) +
  #ggtitle("\n\n") +
  labs(subtitle=subtitle_content, caption=author_content) +
  xlab("Samples") + ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_symmetric <- plot_grid(hist_dist_symmetric, hist_dist_symmetric1, hist_dist_symmetric2, ncol = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
final_plot_dist_t_student_symmetric <- grid.arrange(title_content_gtable, plots_dist_symmetric, ncol = 1, heights = c(0.2, 1))

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
      stop("Numero di valori 'as' deve essere uguale a 2")
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
      stop("Numero di valori 'bs' deve essere uguale a 2")
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

##########################################################################################################################
##########################################################################################################################
# Definiamo valori iniziali 
X0 <- 0
sigmasquared0 <- 0
a0 <- 0.3

a1 <- 0.1
b1 <- 0.3
aq <- c(0.1, 0.2)
bp <- c(0.1, 0.2)

##########################################################################################################################
"Costruisco un processo ARCH e GARCH per ogni distribuzione considerando q=1 e p=1 (solo per il modello GARCH)
 Definiamo parametri in modo tale da soddisfare la condizione di stazionarietà: 
  - Modello Arch: 1 - a1*σ^2 > 0 -> 1 > a1*σ^2
  - Modello Garch: 1 - a1*σ^2 - b1 > 0 -> 1 > a1*σ^2 + b1 "

q <- 1
p <- 1 # utilizzato solo per Garch

############################################## " MODELLO ARCH "

type_model = substitute(paste0("Model ARCH(", q, ")"))

########## DISTRIBUZIONE NORMALE
# Prima traiettoria
sigmasquaredW <- var(dist_normal)
print(a1*sigmasquaredW)
Xt_normal_arch_q1 <- model_arch(a0, a1, X0, dist_normal, q)
Xt_normal_arch_q1

# Seconda traiettoria
sigmasquaredW <- var(dist_normal1)
print(a1*sigmasquaredW)
Xt_normal_arch1_q1 <- model_arch(a0, a1, X0, dist_normal1, q)
Xt_normal_arch1_q1

# Terza traiettoria
sigmasquaredW <- var(dist_normal2)
print(a1*sigmasquaredW)
Xt_normal_arch2_q1 <- model_arch(a0, a1, X0, dist_normal2, q)
Xt_normal_arch2_q1

type_dist = "Normal Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione normale
plot_dist_normal <- ggplot(data.frame(value = Xt_normal_arch_q1, index = seq_along(Xt_normal_arch_q1)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione normale
plot_dist_normal1 <- ggplot(data.frame(value = Xt_normal_arch1_q1, index = seq_along(Xt_normal_arch_q1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione normale
plot_dist_normal2 <- ggplot(data.frame(value = Xt_normal_arch2_q1, index = seq_along(Xt_normal_arch_q1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_norm <- plot_grid(plot_dist_normal, plot_dist_normal1, plot_dist_normal2, nrow = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
arch_plot_dist_norm <- grid.arrange(title_content_gtable, plots_dist_norm, ncol = 1, heights = c(0.2, 1))

##########  DISTRUBUZIONE T-STUDENT SIMMETRICA
# Prima traiettoria
sigmasquaredW <- var(dist_t_student_symmetric)
print(a1*sigmasquaredW)
Xt_t_student_symmetric_arch_q1 <- model_arch(a0, a1, X0, dist_t_student_symmetric, q)
Xt_t_student_symmetric_arch_q1

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_symmetric1)
print(a1*sigmasquaredW)
Xt_t_student_symmetric_arch1_q1 <- model_arch(a0, a1, X0, dist_t_student_symmetric1, q)
Xt_t_student_symmetric_arch1_q1

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_symmetric2)
print(a1*sigmasquaredW)
Xt_t_student_symmetric_arch2_q1 <- model_arch(a0, a1, X0, dist_t_student_symmetric2, q)
Xt_t_student_symmetric_arch2_q1

type_dist = "Symmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric <- ggplot(data.frame(value = Xt_t_student_symmetric_arch_q1, index = seq_along(Xt_t_student_symmetric_arch_q1)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric1 <- ggplot(data.frame(value = Xt_t_student_symmetric_arch1_q1, index = seq_along(Xt_t_student_symmetric_arch1_q1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric2 <- ggplot(data.frame(value = Xt_t_student_symmetric_arch2_q1, index = seq_along(Xt_t_student_symmetric_arch2_q1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_t_student_symmetric <- plot_grid(plot_t_student_symmetric, plot_t_student_symmetric1, plot_t_student_symmetric2, nrow = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
arch_plot_dist_t_student_symmetric <- grid.arrange(title_content_gtable, plots_dist_t_student_symmetric, ncol = 1, heights = c(0.2, 1))

##########  DISTRUBUZIONE T-STUDENT  ASIMMETRICA
# Prima traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric)
print(a1*sigmasquaredW)
Xt_t_student_asymmetric_arch_q1 <- model_arch(a0, a1, X0, dist_t_student_asymmetric, q)
Xt_t_student_asymmetric_arch_q1

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric1)
print(a1*sigmasquaredW)
Xt_t_student_asymmetric_arch1_q1 <- model_arch(a0, a1, X0, dist_t_student_asymmetric1, q)
Xt_t_student_asymmetric_arch1_q1

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric2)
print(a1*sigmasquaredW)
Xt_t_student_asymmetric_arch2_q1 <- model_arch(a0, a1, X0, dist_t_student_asymmetric2, q)
Xt_t_student_asymmetric_arch2_q1

type_dist = "Asymmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric <- ggplot(data.frame(value = Xt_t_student_asymmetric_arch_q1, index = seq_along(Xt_t_student_asymmetric_arch_q1)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric1 <- ggplot(data.frame(value = Xt_t_student_asymmetric_arch1_q1, index = seq_along(Xt_t_student_asymmetric_arch1_q1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric2 <- ggplot(data.frame(value = Xt_t_student_asymmetric_arch2_q1, index = seq_along(Xt_t_student_asymmetric_arch2_q1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_t_student_asymmetric <- plot_grid(plot_t_student_asymmetric, plot_t_student_asymmetric1, plot_t_student_asymmetric2, nrow = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
arch_plot_dist_t_student_asymmetric <- grid.arrange(title_content_gtable, plots_dist_t_student_asymmetric, ncol = 1, heights = c(0.2, 1))

############################################## " MODELLO GARCH "

type_model = substitute(paste0("Model GARCH(", q, ",", p, ")"))


##########  DISTRUBUZIONE NORMALE
# Prima traiettoria
sigmasquaredW <- var(dist_normal)
print(a1*sigmasquaredW + b1)
Xt_normal_garch_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_normal, q, p)
Xt_normal_garch_q1_p1

# Seconda traiettoria
sigmasquaredW <- var(dist_normal1)
print(a1*sigmasquaredW + b1)
Xt_normal_garch1_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_normal1, q, p)
Xt_normal_garch1_q1_p1

# Terza traiettoria
sigmasquaredW <- var(dist_normal)
print(a1*sigmasquaredW + b1)
Xt_normal_garch2_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_normal2, q, p)
Xt_normal_garch2_q1_p1

type_dist = "Normal Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)), " of a ", .(type_dist))))

# creo il primo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal <- ggplot(data.frame(value = Xt_normal_garch_q1_p1, index = seq_along(Xt_normal_garch_q1_p1)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal1 <- ggplot(data.frame(value = Xt_normal_garch1_q1_p1, index = seq_along(Xt_normal_garch_q1_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal2 <- ggplot(data.frame(value = Xt_normal_garch2_q1_p1, index = seq_along(Xt_normal_garch_q1_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_norm <- plot_grid(plot_dist_normal, plot_dist_normal1, plot_dist_normal2, nrow = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
garch_plot_dist_norm <- grid.arrange(title_content_gtable, plots_dist_norm, ncol = 1, heights = c(0.2, 1))

##########  DISTRUBUZIONE T-STUDENT  SIMMETRICA
# Prima traiettoria
sigmasquaredW <- var(dist_t_student_symmetric)
print(a1*sigmasquaredW + b1)
Xt_t_student_symmetric_garch_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_symmetric, q, p)
Xt_t_student_symmetric_garch_q1_p1

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_symmetric1)
print(a1*sigmasquaredW + b1)
Xt_t_student_symmetric_garch1_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_symmetric1, q, p)
Xt_t_student_symmetric_garch1_q1_p1

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_symmetric2)
print(a1*sigmasquaredW + b1)
Xt_t_student_symmetric_garch2_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_symmetric2, q, p)
Xt_t_student_symmetric_garch2_q1_p1

type_dist = "Symmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric <- ggplot(data.frame(value = Xt_t_student_symmetric_garch_q1_p1, index = seq_along(Xt_t_student_symmetric_garch_q1_p1)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric1 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch1_q1_p1, index = seq_along(Xt_t_student_symmetric_garch1_q1_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric2 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch2_q1_p1, index = seq_along(Xt_t_student_symmetric_garch2_q1_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_t_student_symmetric <- plot_grid(plot_t_student_symmetric, plot_t_student_symmetric1, plot_t_student_symmetric2, nrow = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
garch_plot_dist_t_student_symmetric <- grid.arrange(title_content_gtable, plots_dist_t_student_symmetric, ncol = 1, heights = c(0.2, 1))

##########  DISTRUBUZIONE T-STUDENT  ASIMMETRICA
# Prima traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric)
print(a1*sigmasquaredW + b1)
Xt_t_student_asymmetric_garch_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_asymmetric, q, p)
Xt_t_student_asymmetric_garch_q1_p1

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric1)
print(a1*sigmasquaredW + b1)
Xt_t_student_asymmetric_garch1_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)
Xt_t_student_asymmetric_garch1_q1_p1

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric2)
print(a1*sigmasquaredW + b1)
Xt_t_student_asymmetric_garch2_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_asymmetric2, q, p)
Xt_t_student_asymmetric_garch2_q1_p1

type_dist = "Asymmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch_q1_p1, index = seq_along(Xt_t_student_asymmetric_garch_q1_p1)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric1 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch1_q1_p1, index = seq_along(Xt_t_student_asymmetric_garch1_q1_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric2 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch2_q1_p1, index = seq_along(Xt_t_student_asymmetric_garch2_q1_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_t_student_asymmetric <- plot_grid(plot_t_student_asymmetric, plot_t_student_asymmetric1, plot_t_student_asymmetric2, nrow = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
garch_plot_dist_t_student_asymmetric <- grid.arrange(title_content_gtable, plots_dist_t_student_asymmetric, ncol = 1, heights = c(0.2, 1))

##########################################################################################################################
"Costruisco un processo GARCH per ogni distribuzione considerando q=1 e p=2 (solo per il modello GARCH)
 Definiamo parametri in modo tale da soddisfare la condizione di stazionarietà: 
  - Modello Garch: 1 - a1*σ^2 - b1 - b2 > 0 -> 1 > a1*σ^2 + b1 + b2"
q <- 1
p <- 2 # utilizzato solo per Garch

############################################## " MODELLO GARCH "

type_model = substitute(paste0("Model GARCH(", q, ",", p, ")"))

##########  DISTRUBUZIONE NORMALE
# Prima traiettoria
sigmasquaredW <- var(dist_normal)
print(a1*sigmasquaredW + bp[1] + bp[2])
Xt_normal_garch_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_normal, q, p)
Xt_normal_garch_q1_p2

# Seconda traiettoria
sigmasquaredW <- var(dist_normal1)
print(a1*sigmasquaredW + bp[1] + bp[2])
Xt_normal_garch1_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_normal1, q, p)
Xt_normal_garch1_q1_p2

# Terza traiettoria
sigmasquaredW <- var(dist_normal2)
print(a1*sigmasquaredW + bp[1] + bp[2])
Xt_normal_garch2_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_normal2, q, p)
Xt_normal_garch2_q1_p2

type_dist = "Normal Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)), " of a ", .(type_dist))))

# creo il primo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal <- ggplot(data.frame(value = Xt_normal_garch_q1_p2, index = seq_along(Xt_normal_garch_q1_p2)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal1 <- ggplot(data.frame(value = Xt_normal_garch1_q1_p2, index = seq_along(Xt_normal_garch1_q1_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal2 <- ggplot(data.frame(value = Xt_normal_garch2_q1_p2, index = seq_along(Xt_normal_garch2_q1_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_norm <- plot_grid(plot_dist_normal, plot_dist_normal1, plot_dist_normal2, nrow = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
garch_plot_dist_norm <- grid.arrange(title_content_gtable, plots_dist_norm, ncol = 1, heights = c(0.2, 1))

##########  DISTRUBUZIONE T-STUDENT  SIMMETRICA
# Prima traiettoria
sigmasquaredW <- var(dist_t_student_symmetric)
print(a1*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_symmetric_garch_q1_p2<- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_symmetric, q, p)
Xt_t_student_symmetric_garch_q1_p2

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_symmetric1)
print(a1*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_symmetric_garch1_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_symmetric1, q, p)
Xt_t_student_symmetric_garch1_q1_p2

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_symmetric2)
print(a1*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_symmetric_garch2_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_symmetric2, q, p)
Xt_t_student_symmetric_garch2_q1_p2

type_dist = "Symmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric <- ggplot(data.frame(value = Xt_t_student_symmetric_garch_q1_p2, index = seq_along(Xt_t_student_symmetric_garch_q1_p2)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric1 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch1_q1_p2, index = seq_along(Xt_t_student_symmetric_garch1_q1_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric2 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch2_q1_p2, index = seq_along(Xt_t_student_symmetric_garch2_q1_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_t_student_symmetric <- plot_grid(plot_t_student_symmetric, plot_t_student_symmetric1, plot_t_student_symmetric2, nrow = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
garch_plot_dist_t_student_symmetric <- grid.arrange(title_content_gtable, plots_dist_t_student_symmetric, ncol = 1, heights = c(0.2, 1))

##########  DISTRUBUZIONE T-STUDENT  ASIMMETRICA
# Prima traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric)
print(a1*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_asymmetric_garch_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_asymmetric, q, p)
Xt_t_student_asymmetric_garch_q1_p2

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric1)
print(a1*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_asymmetric_garch1_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)
Xt_t_student_asymmetric_garch1_q1_p2

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric2)
print(a1*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_asymmetric_garch2_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_asymmetric2, q, p)
Xt_t_student_asymmetric_garch2_q1_p2

type_dist = "Asymmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch_q1_p2, index = seq_along(Xt_t_student_asymmetric_garch_q1_p2)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric1 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch1_q1_p2, index = seq_along(Xt_t_student_asymmetric_garch1_q1_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric2 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch2_q1_p2, index = seq_along(Xt_t_student_asymmetric_garch2_q1_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_t_student_asymmetric <- plot_grid(plot_t_student_asymmetric, plot_t_student_asymmetric1, plot_t_student_asymmetric2, nrow = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
garch_plot_dist_t_student_asymmetric <- grid.arrange(title_content_gtable, plots_dist_t_student_asymmetric, ncol = 1, heights = c(0.2, 1))

##########################################################################################################################
"Costruisco un processo ARCH e GARCH per ogni distribuzione considerando q=2 e p=1 (solo per il modello GARCH)
 Definiamo parametri in modo tale da soddisfare la condizione di stazionarietà: 
  - Modello Arch: 1 - a1*σ^2 - a2*σ^2 -> 0 -> 1 > a1*σ^2 + a2*σ^2
  - Modello Garch: 1 - a1*σ^2 - a2*σ^2 - b1 > 0 -> 1 > a1*σ^2 + a2*σ^2+ b1"
q <- 2
p <- 1 # utilizzato solo per Garch

############################################## " MODELLO ARCH "

type_model = substitute(paste0("Model ARCH(", q, ")"))

##########  DISTRIBUZIONE NORMALE
# Prima traiettoria
sigmasquaredW <- var(dist_normal)
print((aq[1]+aq[2])*sigmasquaredW)
Xt_normal_arch_q2 <- model_arch(a0, aq, X0, dist_normal, q)
Xt_normal_arch_q2

# Seconda traiettoria
sigmasquaredW <- var(dist_normal1)
print((aq[1]+aq[2])*sigmasquaredW)
Xt_normal_arch1_q2 <- model_arch(a0, aq, X0, dist_normal1, q)
Xt_normal_arch1_q2

# Terza traiettoria
sigmasquaredW <- var(dist_normal2)
print((aq[1]+aq[2])*sigmasquaredW)
Xt_normal_arch2_q2 <- model_arch(a0, aq, X0, dist_normal2, q)
Xt_normal_arch2_q2

type_dist = "Normal Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione normale
plot_dist_normal <- ggplot(data.frame(value = Xt_normal_arch_q2, index = seq_along(Xt_normal_arch_q2)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione normale
plot_dist_normal1 <- ggplot(data.frame(value = Xt_normal_arch1_q2, index = seq_along(Xt_normal_arch_q2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione normale
plot_dist_normal2 <- ggplot(data.frame(value = Xt_normal_arch2_q2, index = seq_along(Xt_normal_arch_q2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_norm <- plot_grid(plot_dist_normal, plot_dist_normal1, plot_dist_normal2, nrow = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
arch_plot_dist_norm <- grid.arrange(title_content_gtable, plots_dist_norm, ncol = 1, heights = c(0.2, 1))

##########  DISTRUBUZIONE T-STUDENT SIMMETRICA
# Prima traiettoria
sigmasquaredW <- var(dist_t_student_symmetric)
print((aq[1]+aq[2])*sigmasquaredW)
Xt_t_student_symmetric_arch_q2 <- model_arch(a0, aq, X0, dist_t_student_symmetric, q)
Xt_t_student_symmetric_arch_q2

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_symmetric1)
print((aq[1]+aq[2])*sigmasquaredW)
Xt_t_student_symmetric_arch1_q2 <- model_arch(a0, aq, X0, dist_t_student_symmetric1, q)
Xt_t_student_symmetric_arch1_q2

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_symmetric2)
print((aq[1]+aq[2])*sigmasquaredW)
Xt_t_student_symmetric_arch2_q2 <- model_arch(a0, aq, X0, dist_t_student_symmetric2, q)
Xt_t_student_symmetric_arch2_q2

type_dist = "Symmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric <- ggplot(data.frame(value = Xt_t_student_symmetric_arch_q2, index = seq_along(Xt_t_student_symmetric_arch_q2)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric1 <- ggplot(data.frame(value = Xt_t_student_symmetric_arch1_q2, index = seq_along(Xt_t_student_symmetric_arch1_q2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric2 <- ggplot(data.frame(value = Xt_t_student_symmetric_arch2_q2, index = seq_along(Xt_t_student_symmetric_arch2_q2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_t_student_symmetric <- plot_grid(plot_t_student_symmetric, plot_t_student_symmetric1, plot_t_student_symmetric2, nrow = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
arch_plot_dist_t_student_symmetric <- grid.arrange(title_content_gtable, plots_dist_t_student_symmetric, ncol = 1, heights = c(0.2, 1))

##########  DISTRUBUZIONE T-STUDENT  ASIMMETRICA
# Prima traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric)
print((aq[1]+aq[2])*sigmasquaredW)
Xt_t_student_asymmetric_arch_q2 <- model_arch(a0, aq, X0, dist_t_student_asymmetric, q)
Xt_t_student_asymmetric_arch_q2

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric1)
print((aq[1]+aq[2])*sigmasquaredW)
Xt_t_student_asymmetric_arch1_q2 <- model_arch(a0, aq, X0, dist_t_student_asymmetric1, q)
Xt_t_student_asymmetric_arch1_q2

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric2)
print((aq[1]+aq[2])*sigmasquaredW)
Xt_t_student_asymmetric_arch2_q2 <- model_arch(a0, aq, X0, dist_t_student_asymmetric2, q)
Xt_t_student_asymmetric_arch2_q2

type_dist = "Asymmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric <- ggplot(data.frame(value = Xt_t_student_asymmetric_arch_q2, index = seq_along(Xt_t_student_asymmetric_arch_q2)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric1 <- ggplot(data.frame(value = Xt_t_student_asymmetric_arch1_q2, index = seq_along(Xt_t_student_asymmetric_arch1_q2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric2 <- ggplot(data.frame(value = Xt_t_student_asymmetric_arch2_q2, index = seq_along(Xt_t_student_asymmetric_arch2_q2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_t_student_asymmetric <- plot_grid(plot_t_student_asymmetric, plot_t_student_asymmetric1, plot_t_student_asymmetric2, nrow = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
arch_plot_dist_t_student_asymmetric <- grid.arrange(title_content_gtable, plots_dist_t_student_asymmetric, ncol = 1, heights = c(0.2, 1))

############################################## " MODELLO GARCH "

type_model = substitute(paste0("Model GARCH(", q, ",", p, ")"))

##########  DISTRUBUZIONE NORMALE
sigmasquaredW <- var(dist_normal)
print((aq[1]+aq[2])*sigmasquaredW + b1)
Xt_normal_garch_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_normal, q, p)
Xt_normal_garch_q2_p1

# Seconda traiettoria
sigmasquaredW <- var(dist_normal1)
print((aq[1]+aq[2])*sigmasquaredW + b1)
Xt_normal_garch1_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_normal1, q, p)
Xt_normal_garch1_q2_p1

# Terza traiettoria
sigmasquaredW <- var(dist_normal2)
print((aq[1]+aq[2])*sigmasquaredW + b1)
Xt_normal_garch2_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_normal2, q, p)
Xt_normal_garch2_q2_p1

type_dist = "Normal Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)), " of a ", .(type_dist))))

# creo il primo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal <- ggplot(data.frame(value = Xt_normal_garch_q2_p1, index = seq_along(Xt_normal_garch_q2_p1)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal1 <- ggplot(data.frame(value = Xt_normal_garch1_q2_p1, index = seq_along(Xt_normal_garch_q2_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal2 <- ggplot(data.frame(value = Xt_normal_garch2_q2_p1, index = seq_along(Xt_normal_garch_q2_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_norm <- plot_grid(plot_dist_normal, plot_dist_normal1, plot_dist_normal2, nrow = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
garch_plot_dist_norm <- grid.arrange(title_content_gtable, plots_dist_norm, ncol = 1, heights = c(0.2, 1))

##########  DISTRUBUZIONE T-STUDENT  SIMMETRICA
# Prima traiettoria
sigmasquaredW <- var(dist_t_student_symmetric)
print((aq[1]+aq[2])*sigmasquaredW + b1)
Xt_t_student_symmetric_garch_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_symmetric, q, p)
Xt_t_student_symmetric_garch_q2_p1

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_symmetric1)
print((aq[1]+aq[2])*sigmasquaredW + b1)
Xt_t_student_symmetric_garch1_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_symmetric1, q, p)
Xt_t_student_symmetric_garch1_q2_p1

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_symmetric2)
print((aq[1]+aq[2])*sigmasquaredW + b1)
Xt_t_student_symmetric_garch2_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_symmetric2, q, p)
Xt_t_student_symmetric_garch2_q2_p1

type_dist = "Symmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric <- ggplot(data.frame(value = Xt_t_student_symmetric_garch_q2_p1, index = seq_along(Xt_t_student_symmetric_garch_q2_p1)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric1 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch1_q2_p1, index = seq_along(Xt_t_student_symmetric_garch1_q2_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric2 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch2_q2_p1, index = seq_along(Xt_t_student_symmetric_garch2_q2_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_t_student_symmetric <- plot_grid(plot_t_student_symmetric, plot_t_student_symmetric1, plot_t_student_symmetric2, nrow = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
garch_plot_dist_t_student_symmetric <- grid.arrange(title_content_gtable, plots_dist_t_student_symmetric, ncol = 1, heights = c(0.2, 1))

##########  DISTRUBUZIONE T-STUDENT  ASIMMETRICA
# Prima traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric)
print((aq[1]+aq[2])*sigmasquaredW + b1)
Xt_t_student_asymmetric_garch_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_asymmetric, q, p)
Xt_t_student_asymmetric_garch_q2_p1

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric1)
print((aq[1]+aq[2])*sigmasquaredW + b1)
Xt_t_student_asymmetric_garch1_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)
Xt_t_student_asymmetric_garch1_q2_p1

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric2)
print((aq[1]+aq[2])*sigmasquaredW + b1)
Xt_t_student_asymmetric_garch2_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_asymmetric2, q, p)
Xt_t_student_asymmetric_garch2_q2_p1

type_dist = "Asymmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch_q2_p1, index = seq_along(Xt_t_student_asymmetric_garch_q2_p1)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric1 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch1_q2_p1, index = seq_along(Xt_t_student_asymmetric_garch1_q2_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric2 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch2_q2_p1, index = seq_along(Xt_t_student_asymmetric_garch2_q2_p1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_t_student_asymmetric <- plot_grid(plot_t_student_asymmetric, plot_t_student_asymmetric1, plot_t_student_asymmetric2, nrow = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
garch_plot_dist_t_student_asymmetric <- grid.arrange(title_content_gtable, plots_dist_t_student_asymmetric, ncol = 1, heights = c(0.2, 1))

###########################################################################################################
"Costruisco un processo GARCH per ogni distribuzione considerando q=2 e p=2 (solo per il modello GARCH)
 Definiamo parametri in modo tale da soddisfare la condizione di stazionarietà: 
  - Modello Arch: 1 - a1*σ^2 - a2*σ^2 > 0 -> 1 > a1*σ^2 + a2*σ^2
  - Modello Garch: 1 - a1*σ^2 - a2*σ^2 - b1 - b2 > 0 -> 1 > a1*σ^2 + a2*σ^2 + b1 + b2"
q <- 2
p <- 2 # utilizzato solo per Garch

############################################## " MODELLO GARCH "

type_model = substitute(paste0("Model GARCH(", q, ",", p, ")"))

##########  DISTRUBUZIONE NORMALE
# Prima traiettoria
sigmasquaredW <- var(dist_normal)
print((aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])
Xt_normal_garch_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_normal, q, p)
Xt_normal_garch_q2_p2

# Seconda traiettoria
sigmasquaredW <- var(dist_normal1)
print((aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])
Xt_normal_garch1_q2_p2<- model_garch(a0, aq, bp, X0, sigmasquared0, dist_normal1, q, p)
Xt_normal_garch1_q2_p2

# Terza traiettoria
sigmasquaredW <- var(dist_normal2)
print((aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])
Xt_normal_garch2_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_normal2, q, p)
Xt_normal_garch2_q2_p2

type_dist = "Normal Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)), " of a ", .(type_dist))))

# creo il primo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal <- ggplot(data.frame(value = Xt_normal_garch_q2_p2, index = seq_along(Xt_normal_garch_q2_p2)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal1 <- ggplot(data.frame(value = Xt_normal_garch1_q2_p2, index = seq_along(Xt_normal_garch1_q2_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal2 <- ggplot(data.frame(value = Xt_normal_garch2_q2_p2, index = seq_along(Xt_normal_garch2_q2_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_norm <- plot_grid(plot_dist_normal, plot_dist_normal1, plot_dist_normal2, nrow = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
garch_plot_dist_norm <- grid.arrange(title_content_gtable, plots_dist_norm, ncol = 1, heights = c(0.2, 1))

##########  DISTRUBUZIONE T-STUDENT  SIMMETRICA
# Prima traiettoria
sigmasquaredW <- var(dist_t_student_symmetric)
print((aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_symmetric_garch_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_symmetric, q, p)
Xt_t_student_symmetric_garch_q2_p2

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_symmetric1)
print((aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_symmetric_garch1_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_symmetric1, q, p)
Xt_t_student_symmetric_garch1_q2_p2

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_symmetric2)
print((aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_symmetric_garch2_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_symmetric2, q, p)
Xt_t_student_symmetric_garch2_q2_p2

type_dist = "Symmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric <- ggplot(data.frame(value = Xt_t_student_symmetric_garch_q2_p2, index = seq_along(Xt_t_student_symmetric_garch_q2_p2)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric1 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch1_q2_p2, index = seq_along(Xt_t_student_symmetric_garch1_q2_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric2 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch2_q2_p2, index = seq_along(Xt_t_student_symmetric_garch2_q2_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_t_student_symmetric <- plot_grid(plot_t_student_symmetric, plot_t_student_symmetric1, plot_t_student_symmetric2, nrow = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
garch_plot_dist_t_student_symmetric <- grid.arrange(title_content_gtable, plots_dist_t_student_symmetric, ncol = 1, heights = c(0.2, 1))

##########  DISTRUBUZIONE T-STUDENT  ASIMMETRICA
# Prima traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric)
print((aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_asymmetric_garch_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_asymmetric, q, p)
Xt_t_student_asymmetric_garch_q2_p2

# Seconda traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric1)
print((aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_asymmetric_garch1_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)
Xt_t_student_asymmetric_garch1_q2_p2

# Terza traiettoria
sigmasquaredW <- var(dist_t_student_asymmetric2)
print((aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_asymmetric_garch2_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_asymmetric2, q, p)
Xt_t_student_asymmetric_garch2_q2_p2

type_dist = "Asymmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch_q2_p2, index = seq_along(Xt_t_student_asymmetric_garch_q2_p2)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric1 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch1_q2_p2, index = seq_along(Xt_t_student_asymmetric_garch1_q2_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric2 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch2_q2_p2, index = seq_along(Xt_t_student_asymmetric_garch2_q2_p2)), aes(x = index, y = value)) +
  geom_line() +
  xlab("Time") + ylab("") +
  labs(caption=author_content) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo la figura utilizzando una griglia con i tre plot generati 
plots_dist_t_student_asymmetric <- plot_grid(plot_t_student_asymmetric, plot_t_student_asymmetric1, plot_t_student_asymmetric2, nrow = 3)
title_content_gtable <- ggdraw() + draw_label(title_content)
garch_plot_dist_t_student_asymmetric <- grid.arrange(title_content_gtable, plots_dist_t_student_asymmetric, ncol = 1, heights = c(0.2, 1))

#############################################################################################################
#############################################################################################################

########## Test Statistici
"Per verificare la correttezza del modello si studiano:

- Omoschedasticità:  significa che i residui sono equamente distribuiti lungo la linea di regressione;
- Assenza di autocorrelazione: si verifica quando i residui non sono indipendenti l'uno dall'altro;

Non studiamo i casi di:
- Stazionarietà**: un modello di predizione con residui stazionari garantisce che le previsioni future
    siano affidabili e non influenzate da fluttuazioni casuali o tendenze temporal;
- Gaussianità: se i residui seguono una distribuzione normale;

poichè nel primo caso il modello deve essere stazionario e nel secondo caso si utilizzano
distribuzioni differenti oltre dalla distribuzione normale.

I test computazionali che vengono solitamente applicati per rilevare l'eteroschedasticità nelle serie 
temporali sono i test di Breusch-Pagan (BP) e White (W). 

Abbiamo: 
- ipotesi nulla: l'omoschedasticità è presente (i residui sono distributii con una uguale varianza);
- ipotesi alternativa - l'eteroschedasticità è presente ( i residui non sono distribuiti con una varianze uguali).

BP e W sono test $χ^2$. Più alto è il valore di $χ^2$, equivalentemente più basso è il *p-value (Pro b> Chi2)*, 
meno probabile che i termini di errore siano omogenei.

Se il p-value del test è minore di 0.05 allora possiamo rigettare l'ipotesi nulla e 
concludere che l'eteroschedasticità è presente; altrimenti.

L'opzione *studentize* è importante quando si tratta di residui con distribuzione heavy tailed 
(gli eventi rari hanno una probabilità relativamente alta di verificarsi). L'opzione *varformula* consente 
l'introduzione del test White.
"
##########################################

# Consideriamo una traiettoia con distribuzione normale di un modello ARCH(1)
Xt <- Xt_normal_arch_q1
df_Xt_normal_arch_q1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_normal_arch_q1_lm <- lm(Xt~t, data=df_Xt_normal_arch_q1)
summary(Xt_normal_arch_q1_lm)

Xt_normal_arch_q1_res <- Xt_normal_arch_q1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_arch_q1_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_arch_q1_res)                  # theoretical value 3.
# La skew è pari a -0.00868007; questo indica che la sua distribuzione dei dati nei resdui
# è molto vicina alla simmetria con una coda negativa.
# La kurtosi è pari a 3.123922; questo indica che la distribuzione è leggermenete leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale ma non in modo significativo.

plot(Xt_normal_arch_q1_lm,1) # Residuals vs Fitted
plot(Xt_normal_arch_q1_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari e omoschedastici.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_arch_q1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_arch_q1)
show(Xt_normal_arch_q1_bp)
# Si ha un p-value di 0.01983 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_arch_q1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_arch_q1)
show(Xt_normal_arch_q1_w)
# Si ha un p-value di 0.03048 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_normal_arch_q1_lm$residuals
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
# correlata con le serie ritardate, ovvero che il passato non "spiega" il presente e che le 
# variazioni da un istante o periodo ad un altro sono sostanzialmente casuali

# Test Ljiung-box
y <- Xt_normal_arch_q1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 0.84216, df = 1, p-value = 0.3588

# Test Durbin-Watson
dwtest(Xt_normal_arch_q1_lm, alternative="two.sided")
# DW = 1.9816, p-value = 0.3536

# Test Breusch-Godfrey
bgtest(Xt_normal_arch_q1_lm, order=5, type="Chisq")
# LM test = 3.0326, df = 5, p-value = 0.695

# Per tutti e tre i test si ha un p-value > 0.05, ciò significa che non ci sono prove
# di autocorrelazione nei residui del modello

##########################################

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello ARCH(1)
Xt <- Xt_t_student_symmetric_arch_q1
df_Xt_t_student_symmetric_arch_q1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_symmetric_arch_q1_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_arch_q1)
summary(Xt_t_student_symmetric_arch_q1_lm)

Xt_t_student_symmetric_arch_q1_res <- Xt_t_student_symmetric_arch_q1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_arch_q1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_arch_q1_res)                  # theoretical value 3.
# La skew è pari a 0.5739274; questo indica che la distribuzione è leggermente asimmetrica, ma dato
# che il suo valore è vicino allo zero possiamo dire che la sua distribuzione è simmetrica.
# La kurtosi è pari a 12.30628; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

plot(Xt_t_student_symmetric_arch_q1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_arch_q1_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_arch_q1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_arch_q1)
show(Xt_t_student_symmetric_arch_q1_bp)
# Si ha un p-value di 0.5947 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_arch_q1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_arch_q1)
show(Xt_t_student_symmetric_arch_q1_w)
# Si ha un p-value di 0.6687 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_arch_q1_lm$residuals
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
# correlata con le serie ritardate, ovvero che il passato non "spiega" il presente e che le 
# variazioni da un istante o periodo ad un altro sono sostanzialmente casuali

# Test Ljiung-box
y <- Xt_t_student_symmetric_arch_q1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 3.4758, df = 1, p-value = 0.06227

# Test Durbin-Watson
dwtest(Xt_t_student_symmetric_arch_q1_lm, alternative="two.sided")
# DW = 1.9624, p-value = 0.05855

# Test Breusch-Godfrey
bgtest(Xt_t_student_symmetric_arch_q1_lm, order=5, type="Chisq")
# LM test = 5.8768, df = 5, p-value = 0.3184

# Per tutti e tre i test si ha un p-value > 0.05, ciò significa che non ci sono prove
# di autocorrelazione nei residui del modello

##########################################

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello ARCH(1)
Xt <- Xt_t_student_asymmetric_arch_q1
df_Xt_t_student_asymmetric_arch_q1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_arch_q1_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_arch_q1)
summary(Xt_t_student_asymmetric_arch_q1_lm)

Xt_t_student_asymmetric_arch_q1_res <- Xt_t_student_asymmetric_arch_q1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_arch_q1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_arch_q1_res)                  # theoretical value 3.
# La skew è pari a -2.398814; questo indica una forte asimmetria verso sinistra
# con una coda lunga negativa.
# La kurtosi è pari a 15.90909; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

plot(Xt_t_student_asymmetric_arch_q1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_arch_q1_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_arch_q1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_arch_q1)
show(Xt_t_student_asymmetric_arch_q1_bp)
# Si ha un p-value di 0.5958 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_arch_q1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_arch_q1)
show(Xt_t_student_asymmetric_arch_q1_w)
# Si ha un p-value di 0.8664 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
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
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano sempre entro 
# una banda ristretta, quindi, possiamo dire che che la serie non è significativamente 
# correlata con le serie ritardate, ovvero che il passato non "spiega" il presente e che le 
# variazioni da un istante o periodo ad un altro sono sostanzialmente casuali

# Test Ljiung-box
y <- Xt_t_student_asymmetric_arch_q1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 75.926, df = 1, p-value < 2.2e-16

# Test Durbin-Watson
dwtest(Xt_t_student_asymmetric_arch_q1_lm, alternative="two.sided")
# DW = 1.8255, p-value < 2.2e-16

# Test Breusch-Godfrey
bgtest(Xt_t_student_asymmetric_arch_q1_lm, order=5, type="Chisq")
# LM test = 78.512, df = 5, p-value = 1.718e-15

# Per tutti e tre i test si ha un p-value < 0.05, ciò significa che ci sono prove
# di autocorrelazione nei residui del modello

##########################################
##########################################

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(1,1)
Xt <- Xt_normal_garch1_q1_p1
df_Xt_normal_garch_q1_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_normal_garch_q1_p1_lm <- lm(Xt~t, data=df_Xt_normal_garch_q1_p1)
summary(Xt_normal_garch_q1_p1_lm)

Xt_normal_garch_q1_p1_res <- Xt_normal_garch_q1_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_garch_q1_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_garch_q1_p1_res)                  # theoretical
# La skew è pari a -0.01203641; il suo valore è prossimo a zero e negativo, quindi la sua distribuzione 
# viene considerata approssimativamente simmetrica con una coda negativa.
# La kurtosi è pari a 3.035664; questo indica che la distribuzione è leggermenete leptocurtica

plot(Xt_normal_garch_q1_p1_lm,1) # Residuals vs Fitted
plot(Xt_normal_garch_q1_p1_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch_q1_p1)
show(Xt_normal_garch_q1_p1_bp)
# Si ha un p-value di 0.7876 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch_q1_p1)
show(Xt_normal_garch_q1_p1_w)
# Si ha un p-value di 0.3665 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_normal_garch_q1_p1_lm$residuals
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
# correlata con le serie ritardate, ovvero che il passato non "spiega" il presente e che le 
# variazioni da un istante o periodo ad un altro sono sostanzialmente casuali

# Test Ljiung-box
y <- Xt_normal_garch_q1_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 4.4068, df = 1, p-value = 0.0358

# Test Durbin-Watson
dwtest(Xt_normal_garch_q1_p1_lm, alternative="two.sided")
#DW = 2.0418, p-value = 0.03757

# Test Breusch-Godfrey
bgtest(Xt_normal_garch_q1_p1_lm, order=5, type="Chisq")
# LM test = 11.792, df = 5, p-value = 0.03775

# Per tutti e tre i test si ha un p-value < 0.05, ciò significa che ci sono prove
# di autocorrelazione nei residui del modello

##########################################

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello GARCH(1,1)
Xt <- Xt_t_student_symmetric_garch2_q1_p1
df_Xt_t_student_symmetric_garch_q1_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch_q1_p1_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch_q1_p1)
summary(Xt_t_student_symmetric_garch_q1_p1_lm)

Xt_t_student_symmetric_garch_q1_p1_res <- Xt_t_student_symmetric_garch_q1_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_garch_q1_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_garch_q1_p1_res)                  # theoretical value 3.
# La skew è pari a 0.638849; il suo valore è positivo e la sua distribuzione è approssimativamente simmetrica.
# La kurtosi è pari a 15.58163; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

plot(Xt_t_student_symmetric_garch_q1_p1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_garch_q1_p1_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q1_p1)
show(Xt_t_student_symmetric_garch_q1_p1_bp)
# Si ha un p-value di 0.6434 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q1_p1)
show(Xt_t_student_symmetric_garch_q1_p1_w)
# Si ha un p-value di 0.832 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_garch_q1_p1_lm$residuals
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
y <- Xt_t_student_symmetric_garch_q1_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 0.19693, df = 1, p-value = 0.6572

# Test Durbin-Watson
dwtest(Xt_t_student_symmetric_garch_q1_p1_lm, alternative="two.sided")
# DW = 1.9911, p-value = 0.65

# Test Breusch-Godfrey
bgtest(Xt_t_student_symmetric_garch_q1_p1_lm, order=5, type="Chisq")
# LM test = 0.52794, df = 5, p-value = 0.9911

# Per tutti e tre i test si ha un p-value > 0.05, ciò significa che non ci sono prove
# di autocorrelazione nei residui del modello

##########################################

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello GARCH(1,1)
Xt <- Xt_t_student_asymmetric_garch1_q1_p1
df_Xt_t_student_asymmetric_garch_q1_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch_q1_p1_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch_q1_p1)
summary(Xt_t_student_asymmetric_garch_q1_p1_lm)

Xt_t_student_asymmetric_garch_q1_p1_res <- Xt_t_student_asymmetric_garch_q1_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_garch_q1_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_garch_q1_p1_res)                  # theoretical value 3.
# La skew è pari a -1.984191; il suo valore è negativo e la sua distribuzione è
# asimmetrica verso sinistra con una coda lunga negativa.
# La kurtosi è pari a 9.683713; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

plot(Xt_t_student_asymmetric_garch_q1_p1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_garch_q1_p1_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q1_p1)
show(Xt_t_student_asymmetric_garch_q1_p1_bp)
# Si ha un p-value di 0.6332 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q1_p1)
show(Xt_t_student_asymmetric_garch_q1_p1_w)
# Si ha un p-value di 0.8731 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_asymmetric_garch_q1_p1_lm$residuals
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
# una banda ristretta, anche se in alcuni lag questo valore esce dall'intervallo.

# Test Ljiung-box
y <- Xt_t_student_asymmetric_garch_q1_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 28.077, df = 1, p-value = 1.166e-07

# Test Durbin-Watson
dwtest(Xt_t_student_asymmetric_garch_q1_p1_lm, alternative="two.sided")
# DW = 1.894, p-value = 1.078e-07

# Test Breusch-Godfrey
bgtest(Xt_t_student_asymmetric_garch_q1_p1_lm, order=5, type="Chisq")
# LM test = 44.87, df = 5, p-value = 1.542e-08

# I risultati dei tre test hanno un p-value < 0.05 ciò significa che ci sono prove
# di autocorrelazione nei residui del modello

##########################################
##########################################

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(1,2)
Xt <- Xt_normal_garch_q1_p2
df_Xt_normal_garch_q1_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_normal_garch_q1_p2_lm <- lm(Xt~t, data=df_Xt_normal_garch_q1_p2)
summary(Xt_normal_garch_q1_p2_lm)

Xt_normal_garch_q1_p2_res <- Xt_normal_garch_q1_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_garch_q1_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_garch_q1_p2_res)                  # theoretical value 3.
# La skew è pari a -0.01017818; il suo valore è prossimo a zero, quindi la sua distribuzione 
# viene considerata simmetrica con una coda negativa.
# La kurtosi è pari a 3.132656; questo indica che la distribuzione è leggermenete leptocurtica.

plot(Xt_normal_garch_q1_p2_lm,1) # Residuals vs Fitted
plot(Xt_normal_garch_q1_p2_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch_q1_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch_q1_p2)
show(Xt_normal_garch_q1_p2_bp)
# Si ha un p-value di 0.01438 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch_q1_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch_q1_p2)
show(Xt_normal_garch_q1_p2_w)
# Si ha un p-value di 0.02121 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_normal_garch_q1_p2_lm$residuals
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
# correlata con le serie ritardate, ovvero che il passato non "spiega" il presente e che le 
# variazioni da un istante o periodo ad un altro sono sostanzialmente casuali

# Test Ljiung-box
y <- Xt_normal_garch_q1_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 0.87929, df = 1, p-value = 0.3484

# Test Durbin-Watson
dwtest(Xt_normal_garch_q1_p2_lm, alternative="two.sided")
# DW = 1.9812, p-value = 0.3433

# Test Breusch-Godfrey
bgtest(Xt_normal_garch_q1_p2_lm, order=5, type="Chisq")
# LM test = 3.1194, df = 5, p-value = 0.6816

# Per tutti e tre i test si ha un p-value > 0.05, ciò significa che non ci sono prove
# di autocorrelazione nei residui del modello

##########################################

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello GARCH(1,2)
Xt <- Xt_t_student_symmetric_garch_q1_p2
df_Xt_t_student_symmetric_garch_q1_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch_q1_p2_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch_q1_p2)
summary(Xt_t_student_symmetric_garch_q1_p2_lm)

Xt_t_student_symmetric_garch_q1_p2_res <- Xt_t_student_symmetric_garch_q1_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_garch_q1_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_garch_q1_p2_res)                  # theoretical value 3.
# La skew è pari a 0.5744184; il suo valore è positivo e vicino allo zero; quindi, la sua distribuzione
# è vicina alla simmetria.
# La kurtosi è pari a 12.42203; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

plot(Xt_t_student_symmetric_garch_q1_p2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_garch_q1_p2_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch_q1_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q1_p2)
show(Xt_t_student_symmetric_garch_q1_p2_bp)
# Si ha un p-value di 0.5635 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch_q1_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q1_p2)
show(Xt_t_student_symmetric_garch_q1_p2_w)
# Si ha un p-value di 0.6823 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_garch_q1_p2_lm$residuals
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
# una banda ristretta, anche se in alcuni lag questo valore esce dall'intervallo; 
# quindi, possiamo dire che che la serie non è significativamente 
# correlata con le serie ritardate, ovvero che il passato non "spiega" il presente e che le 
# variazioni da un istante o periodo ad un altro sono sostanzialmente casuali

# Test Ljiung-box
y <- Xt_t_student_symmetric_garch_q1_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 3.5983, df = 1, p-value = 0.05784

# Test Durbin-Watson
dwtest(Xt_t_student_symmetric_garch_q1_p2_lm, alternative="two.sided")
# DW = 1.9617, p-value = 0.05435

# Test Breusch-Godfrey
bgtest(Xt_t_student_symmetric_garch_q1_p2_lm, order=5, type="Chisq")
# LM test = 5.99, df = 5, p-value = 0.3072

# Per tutti e tre i test si ha un p-value > 0.05, ciò significa che non ci sono prove
# di autocorrelazione nei residui del modello

##########################################

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello GARCH(1,1)
Xt <- Xt_t_student_asymmetric_garch_q1_p2
df_Xt_t_student_asymmetric_garch_q1_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch_q1_p2)
summary(Xt_t_student_asymmetric_garch_lm)

Xt_t_student_asymmetric_garch_res <- Xt_t_student_asymmetric_garch_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_garch_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_garch_res)                  # theoretical value 3.
# La skew è pari a -1.996857; il suo valore è negativo e la sua distribuzione è
# asimmetrica verso sinistra con una coda lunga negativa.
# La kurtosi è pari a 10.26631; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

plot(Xt_t_student_asymmetric_garch_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_garch_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q1_p2)
show(Xt_t_student_asymmetric_garch_bp)
# Si ha un p-value di 0.0001279 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q1_p2)
show(Xt_t_student_asymmetric_garch_w)
# Si ha un p-value di 4.497e-05 > 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_asymmetric_garch_lm$residuals
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
# una banda ristretta, anche se in alcuni lag questo valore esce dall'intervallo; 
# quindi, possiamo dire che che la serie non è significativamente 
# correlata con le serie ritardate, ovvero che il passato non "spiega" il presente e che le 
# variazioni da un istante o periodo ad un altro sono sostanzialmente casuali

# Test Ljiung-box
y <- Xt_t_student_symmetric_garch_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)

# Test Durbin-Watson
dwtest(Xt_t_student_symmetric_garch_lm, alternative="two.sided")

# Test Breusch-Godfrey
bgtest(Xt_t_student_symmetric_garch_lm, order=5, type="Chisq")

# Per tutti e tre i test si ha un p-value < 0.05, ciò significa che ci sono prove
# di autocorrelazione nei residui del modello

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello GARCH(1,2)
Xt <- Xt_t_student_asymmetric_garch_q1_p2
df_Xt_t_student_asymmetric_garch_q1_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch_q1_p2_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch_q1_p2)
summary(Xt_t_student_asymmetric_garch_q1_p2_lm)

Xt_t_student_asymmetric_garch_q1_p2_res <- Xt_t_student_asymmetric_garch_q1_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_garch_q1_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_garch_q1_p2_res)                  # theoretical value 3.
# La skew è pari a -2.404047; il suo valore è negativo e la sua distribuzione è
# asimmetrica verso sinistra con una coda lunga negativa.
# La kurtosi è pari a 15.98523; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

plot(Xt_t_student_asymmetric_garch_q1_p2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_garch_q1_p2_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch_q1_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q1_p2)
show(Xt_t_student_asymmetric_garch_q1_p2_bp)
# Si ha un p-value di 0.5865 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch_q1_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q1_p2)
show(Xt_t_student_asymmetric_garch_q1_p2_w)
# Si ha un p-value di 0.8625 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_asymmetric_garch_q1_p2_lm$residuals
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
# nel grafico dell'autocorrelogramma possiamo notare che i valori nei primi lag 
# tendono a eseguire un trend; mentre dal lag 10 i valori oscillano all'interno dell'intervallo 

# Test Ljiung-box
y <- Xt_t_student_asymmetric_garch_q1_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 89.078, df = 1, p-value < 2.2e-16

# Test Durbin-Watson
dwtest(Xt_t_student_asymmetric_garch_q1_p2_lm, alternative="two.sided")
# DW = 1.811, p-value < 2.2e-16

# Test Breusch-Godfrey
bgtest(Xt_t_student_asymmetric_garch_q1_p2_lm, order=5, type="Chisq")
# LM test = 125.08, df = 5, p-value < 2.2e-16

# I risultati dei tre test indicano un p-value < 0.05; questo, indica che ci potrebbe essere evidenza 
# di autocorrelazione nei residui del tuo modello.

##########################################
##########################################

# Consideriamo una traiettoia con distribuzione normale di un modello ARCH(2)
Xt <- Xt_normal_arch_q2
df_Xt_normal_arch_q2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_normal_arch_q2_lm <- lm(Xt~t, data=df_Xt_normal_arch_q2)
summary(Xt_normal_arch_q2_lm)

Xt_normal_arch_q2_res <- Xt_normal_arch_q2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_arch_q2_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_arch_q2_res)                  # theoretical value 3.
# La skew è pari a -0.008294173; questo indica una leggera asimmetria, tuttavia,
# il suo valore è prossimo a zero, quindi la sua distribuzione potrebbe essere
# considerata approssimativamente simmetrica.
# La kurtosi è pari a 3.522397; questo indica che la distribuzione è leggermenete leptocurtica.

plot(Xt_normal_arch_q2_lm,1) # Residuals vs Fitted
plot(Xt_normal_arch_q2_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari e omoschedastici.
# Dal grafico "Scale-Location" possiamo notare omoschedasticità nei residui.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_arch_q2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_arch_q2)
show(Xt_normal_arch_q2_bp)
# Si ha un p-value di 0.02456 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_arch_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_arch_q2)
show(Xt_normal_arch_q2_w)
# Si ha un p-value di 0.0188 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_normal_arch_q2_lm$residuals
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
# una banda ristretta, quindi, possiamo dire che che la serie non è significativamente 
# correlata con le serie ritardate, ovvero che il passato non "spiega" il presente e che le 
# variazioni da un istante o periodo ad un altro sono sostanzialmente casuali

# Test Ljiung-box
y <- Xt_normal_arch_q2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 1.5659, df = 1, p-value = 0.2108

# Test Durbin-Watson
dwtest(Xt_normal_arch_q2_lm, alternative="two.sided")
# DW = 1.975, p-value = 0.2072

# Test Breusch-Godfrey
bgtest(Xt_normal_arch_q2_lm, order=5, type="Chisq")
# LM test = 3.7932, df = 5, p-value = 0.5796

# Per tutti e tre i test si ha un p-value > 0.05, ciò significa che non ci sono prove
# di autocorrelazione nei residui del modello

##########################################

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello ARCH(2)
Xt <- Xt_t_student_symmetric_arch1_q2
df_Xt_t_student_symmetric_arch_q2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_symmetric_arch_q2_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_arch_q2)
summary(Xt_t_student_symmetric_arch_q2_lm)

Xt_t_student_symmetric_arch_q2_res <- Xt_t_student_symmetric_arch_q2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_arch_q2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_arch_q2_res)                  # theoretical value 3.
# La skew è pari a -0.2455441; questo indica una leggera asimmetria verso sinistra, tuttavia,
# il suo valore è prossimo a zero, quindi la sua distribuzione potrebbe essere
# considerata approssimativamente simmetrica.
# La kurtosi è pari a 13.36341; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

plot(Xt_t_student_symmetric_arch_q2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_arch_q2_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari e omoschedastici.
# Dal grafico "Scale-Location" possiamo notare omoschedasticità nei residui.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_arch_q2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_arch_q2)
show(Xt_t_student_symmetric_arch_q2_bp)
# Si ha un p-value di 0.01557 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_arch_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_arch_q2)
show(Xt_t_student_symmetric_arch_q2_w)
# Si ha un p-value di 0.009318 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_arch_q2_lm$residuals
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
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano sempre entro 
# una banda ristretta, quindi, possiamo dire che che la serie non è significativamente 
# correlata con le serie ritardate, ovvero che il passato non "spiega" il presente e che le 
# variazioni da un istante o periodo ad un altro sono sostanzialmente casuali

# Test Ljiung-box
y <- Xt_t_student_symmetric_arch_q2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 4.8797, df = 1, p-value = 0.02717

# Test Durbin-Watson
dwtest(Xt_t_student_symmetric_arch_q2_lm, alternative="two.sided")
# DW = 2.0441, p-value = 0.02822

# Test Breusch-Godfrey
bgtest(Xt_t_student_symmetric_arch_q2_lm, order=5, type="Chisq")
# LM test = 6.5521, df = 5, p-value = 0.2561

# Per i test di Ljiung-box e Durbin-Watson si ha un p-value < 0.05; questo, potrebbe
# indicare che potrebbe esserci un'evidenza di autocorrelazione nei residui del modello.

##########################################

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello ARCH(2)
Xt <- Xt_t_student_asymmetric_arch_q2
df_Xt_t_student_asymmetric_arch_q2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_arch_q2_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_arch_q2)
summary(Xt_t_student_asymmetric_arch_q2_lm)

Xt_t_student_asymmetric_arch_q2_res <- Xt_t_student_asymmetric_arch_q2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_arch_q2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_arch_q2_res)                  # theoretical value 3.
# La skew è pari a -2.601034; questo indica una forte asimmetria verso sinistra
# con una coda lunga negativa.
# La kurtosi è pari a 18.88024; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

plot(Xt_t_student_asymmetric_arch_q2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_arch_q2_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari e omoschedastici.
# Dal grafico "Scale-Location" possiamo notare omoschedasticità nei residui.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_arch_q2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_arch_q2)
show(Xt_t_student_asymmetric_arch_q2_bp)
# Si ha un p-value di  0.4853 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_arch_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_arch_q2)
show(Xt_t_student_asymmetric_arch_q2_w)
# Si ha un p-value di 0.7058 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_asymmetric_arch_q2_lm$residuals
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
# nel grafico dell'autocorrelogramma possiamo notare che i valori nei primi lag tendono a seguire un 
# trend; mentre, dal lag 8 i valori oscillano all'interno dell'intervallo.

# Test Ljiung-box
y <- Xt_t_student_asymmetric_arch_q2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 114.71, df = 1, p-value < 2.2e-16

# Test Durbin-Watson
dwtest(Xt_t_student_asymmetric_arch_q2_lm, alternative="two.sided")
# DW = 1.7856, p-value < 2.2e-16

# Test Breusch-Godfrey
bgtest(Xt_t_student_asymmetric_arch_q2_lm, order=5, type="Chisq")
# LM test = 234.47, df = 5, p-value < 2.2e-16

# Per tutti e tre i test si ha un p-value < 0.05, ciò significa che ci sono prove
# di autocorrelazione nei residui del modello

##########################################
##########################################

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(2,1)
Xt <- Xt_normal_garch_q2_p1
df_Xt_normal_garch_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_normal_garch_q2_p1_lm <- lm(Xt~t, data=df_Xt_normal_garch_q2_p1)
summary(Xt_normal_garch_q2_p1_lm)

Xt_normal_garch_q2_p1_res <- Xt_normal_garch_q2_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_garch_q2_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_garch_q2_p1_res)                  # theoretical value 3.
# La skew è pari a -0.01889716; il suo valore è prossimo a zero, quindi la sua distribuzione 
# viene considerata simmetrica con una coda negativa.
# La kurtosi è pari a 3.752621; questo indica che la distribuzione è leggermenete leptocurtica.

plot(Xt_normal_garch_q2_p1_lm,1) # Residuals vs Fitted
plot(Xt_normal_garch_q2_p1_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch_q2_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch_q2_p1)
show(Xt_normal_garch_q2_p1_bp)
# Si ha un p-value di 0.005296 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch_q2_p1)
show(Xt_normal_garch_q2_p1_w)
# Si ha un p-value di 0.002665 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_normal_garch_q2_p1_lm$residuals
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
# una banda ristretta, quindi, possiamo dire che che la serie non è significativamente 
# correlata con le serie ritardate, ovvero che il passato non "spiega" il presente e che le 
# variazioni da un istante o periodo ad un altro sono sostanzialmente casuali

# Test Ljiung-box
y <- Xt_normal_garch_q2_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 2.0644, df = 1, p-value = 0.1508

# Test Durbin-Watson
dwtest(Xt_normal_garch_q2_p1_lm, alternative="two.sided")
# DW = 1.9713, p-value = 0.148

# Test Breusch-Godfrey
bgtest(Xt_normal_garch_q2_p1_lm, order=5, type="Chisq")
# LM test = 5.0859, df = 5, p-value = 0.4055

# Per tutti e tre i test si ha un p-value > 0.05, ciò significa che non ci sono prove
# di autocorrelazione nei residui del modello

##########################################

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello GARCH(2,1)
Xt <- Xt_t_student_symmetric_garch1_q2_p1
df_Xt_t_student_symmetric_garch_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch_q2_p1_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch_q2_p1)
summary(Xt_t_student_symmetric_garch_q2_p1_lm)

Xt_t_student_symmetric_garch_q2_p1_res <- Xt_t_student_symmetric_garch_q2_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_garch_q2_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_garch_q2_p1_res)                  # theoretical value 3.
# La skew è pari a-0.9138125; il suo valore è negativo e vicino allo zero; quindi, la sua distribuzione
# è vicina alla simmetria.
# La kurtosi è pari a 22.51892; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

plot(Xt_t_student_symmetric_garch_q2_p1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_garch_q2_p1_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch_q2_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q2_p1)
show(Xt_t_student_symmetric_garch_q2_p1_bp)
# Si ha un p-value di 0.007136 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q2_p1)
show(Xt_t_student_symmetric_garch_q2_p1_w)
# Si ha un p-value di 0.003628 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_garch_q2_p1_lm$residuals
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
# una banda ristretta, anche se in alcuni lag questo valore esce dall'intervallo; 
# quindi, possiamo dire che che la serie non è significativamente 
# correlata con le serie ritardate, ovvero che il passato non "spiega" il presente e che le 
# variazioni da un istante o periodo ad un altro sono sostanzialmente casuali

# Test Ljiung-box
y <- Xt_t_student_symmetric_garch_q2_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 6.5903, df = 1, p-value = 0.01025

# Test Durbin-Watson
dwtest(Xt_t_student_symmetric_garch_q2_p1_lm, alternative="two.sided")
# DW = 2.0512, p-value = 0.0107

# Test Breusch-Godfrey
bgtest(Xt_t_student_symmetric_garch_q2_p1_lm, order=5, type="Chisq")
# LM test = 8.9942, df = 5, p-value = 0.1093

# Per i test di Ljiung-box e Durbin-Watson si ha un p-value < 0.05; ciò potrebbe indicare
# un'evidenza di autocorrelazione nei residui del modello.

##########################################

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello GARCH(2,1)
Xt <- Xt_t_student_asymmetric_garch_q2_p1
df_Xt_t_student_asymmetric_garch_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch_q2_p1_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch_q2_p1)
summary(Xt_t_student_asymmetric_garch_q2_p1_lm)

Xt_t_student_asymmetric_garch_q2_p1_res <- Xt_t_student_asymmetric_garch_q2_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_garch_q2_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_garch_q2_p1_res)                  # theoretical value 3.
# La skew è pari a -2.655877; il suo valore è negativo e la sua distribuzione è
# asimmetrica verso sinistra con una coda lunga negativa.
# La kurtosi è pari a 19.09937; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

plot(Xt_t_student_asymmetric_garch_q2_p1_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_garch_q2_p1_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch_q2_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q2_p1)
show(Xt_t_student_asymmetric_garch_q2_p1_bp)
# Si ha un p-value di 0.5129 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q2_p1)
show(Xt_t_student_asymmetric_garch_q2_p1_w)
# Si ha un p-value di 0.5696 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_asymmetric_garch_q2_p1_lm$residuals
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
y <- Xt_t_student_symmetric_garch_q2_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 6.5903, df = 1, p-value = 0.01025

# Test Durbin-Watson
dwtest(Xt_t_student_symmetric_garch_q2_p1_lm, alternative="two.sided")
# DW = 2.0512, p-value = 0.0107

# Test Breusch-Godfrey
bgtest(Xt_t_student_symmetric_garch_q2_p1_lm, order=5, type="Chisq")
# LM test = 8.9942, df = 5, p-value = 0.1093

# Per i test di Ljiung-box e Durbin-Watson si ha un p-value < 0.05; ciò potrebbe indicare
# un'evidenza di autocorrelazione nei residui del modello.

##########################################
##########################################

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(2,2)
Xt <- Xt_normal_garch_q2_p2
df_Xt_normal_garch_q2_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_normal_garch_q2_p2_lm <- lm(Xt~t, data=df_Xt_normal_garch_q2_p2)
summary(Xt_normal_garch_q2_p2_lm)

Xt_normal_garch_q2_p2_res <- Xt_normal_garch_q2_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_garch_q2_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_garch_q2_p2_res)                  # theoretical value 3.
# La skew è pari a -0.01443772; il suo valore è prossimo a zero, quindi la sua distribuzione 
# viene considerata simmetrica.
# La kurtosi è pari a 3.659755; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

plot(Xt_normal_garch_q2_p2_lm,1) # Residuals vs Fitted
plot(Xt_normal_garch_q2_p2_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch_q2_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch_q2_p2)
show(Xt_normal_garch_q2_p2_bp)
# Si ha un p-value di 0.004081 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch_q2_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch_q2_p2)
show(Xt_normal_garch_q2_p2_w)
# Si ha un p-value di 0.002102 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_normal_garch_q2_p2_lm$residuals
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
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano anche fuori dall'intervallo

# Test Ljiung-box
y <- Xt_normal_garch_q2_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 2.0259, df = 1, p-value = 0.1546

# Test Durbin-Watson
dwtest(Xt_normal_garch_q2_p2_lm, alternative="two.sided")
# DW = 1.9715, p-value = 0.1518

# Test Breusch-Godfrey
bgtest(Xt_normal_garch_q2_p2_lm, order=5, type="Chisq")
# LM test = 4.8329, df = 5, p-value = 0.4366

# Per tutti e tre i test il valore del p-value > 0.05, ciò potrebbe indicare
# che non c'è un'evidenza di autocorrelazione nei residui del modello.

##########################################

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello GARCH(2,2)
Xt <- Xt_t_student_symmetric_garch1_q2_p2
df_Xt_t_student_symmetric_garch_q2_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch_q2_p2_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch_q2_p2)
summary(Xt_t_student_symmetric_garch_q2_p2_lm)

Xt_t_student_symmetric_garch_q2_p2_res <- Xt_t_student_symmetric_garch_q2_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_garch_q2_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_garch_q2_p2_res)                  # theoretical value 3.
# La skew è pari a -0.6458118; il suo valore è prossima allo zero, quindi possiamo dire 
# che la sua distribuzione è simmetrica con una coda negativa.
# La kurtosi è pari a 17.06515; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

plot(Xt_t_student_symmetric_garch_q2_p2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_symmetric_garch_q2_p2_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch_q2_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q2_p2)
show(Xt_t_student_symmetric_garch_q2_p2_bp)
# Si ha un p-value di 0.005632 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch_q2_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q2_p2)
show(Xt_t_student_symmetric_garch_q2_p2_w)
# Si ha un p-value di 0.002467 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_garch_q2_p2_lm$residuals
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
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano all'interno dell'intervallo

# Test Ljiung-box
y <- Xt_t_student_symmetric_garch_q2_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 5.13, df = 1, p-value = 0.02352

# Test Durbin-Watson
dwtest(Xt_t_student_symmetric_garch_q2_p2_lm, alternative="two.sided")
# DW = 2.0452, p-value = 0.02444

# Test Breusch-Godfrey
bgtest(Xt_t_student_symmetric_garch_q2_p2_lm, order=5, type="Chisq")
# LM test = 8.2508, df = 5, p-value = 0.1429

# Per i test di Ljiung-box e Burbin-Wtason si ha un p-value < 0.05, ciò significa che c'è un'evidenza di
# autocorrelazione nei residui del modello

##########################################

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello GARCH(2,2)
Xt <- Xt_t_student_asymmetric_garch_q2_p2
df_Xt_t_student_asymmetric_garch_q2_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch_q2_p2_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch_q2_p2)
summary(Xt_t_student_asymmetric_garch_q2_p2_lm)

Xt_t_student_asymmetric_garch_q2_p2_res <- Xt_t_student_asymmetric_garch_q2_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_garch_q2_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_garch_q2_p2_res)                  # theoretical value 3.
# La skew è pari a -2.604897; il suo valore è negativo e la sua distribuzione è
# asimmetrica verso sinistra con una coda lunga negativa.
# La kurtosi è pari a 18.39842; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

plot(Xt_t_student_asymmetric_garch_q2_p2_lm,1) # Residuals vs Fitted
plot(Xt_t_student_asymmetric_garch_q2_p2_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch_q2_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q2_p2)
show(Xt_t_student_asymmetric_garch_q2_p2_bp)
# Si ha un p-value di 0.5338 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch_q2_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q2_p2)
show(Xt_t_student_asymmetric_garch_q2_p2_w)
# Si ha un p-value di 0.5983 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Plot of the autocorrelogram.
y <- Xt_t_student_asymmetric_garch_q2_p2_lm$residuals
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
# nel grafico dell'autocorrelogramma possiamo notare che i valori tendono a seguire un trend 
# fino al 10 lag

# Test Ljiung-box
y <- Xt_t_student_symmetric_garch_q2_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 5.13, df = 1, p-value = 0.02352

# Test Durbin-Watson
dwtest(Xt_t_student_symmetric_garch_q2_p2_lm, alternative="two.sided")
# DW = 2.0452, p-value = 0.02444

# Test Breusch-Godfrey
bgtest(Xt_t_student_symmetric_garch_q2_p2_lm, order=5, type="Chisq")
# LM test = 8.2508, df = 5, p-value = 0.1429

# Per i test di Ljiung-box e Durbin-Watson si ha un p-value < 0.05, ciò significa che potrebbe esserci
# un'evidenza di autocorrelazione nei residui del modello

