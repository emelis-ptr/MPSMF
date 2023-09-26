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

##########################################################################################################################
##########################################################################################################################
"Genero tre diverse traiettorie per tre distribuzioni differenti: 
t-student simmetrica, t-student asimmetrica, distribuzione normale"
n <- 5000 # numero di campioni
df <- 5 # gradi di libertà

########## DISTRIBUZIONE NORMALE
set.seed(123)
dist_normal <- rnorm(n = n, mean = 0, sd = 1)
set.seed(231)
dist_normal1 <- rnorm(n = n, mean = 0, sd = 1)
set.seed(312)
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
set.seed(123)
dist_t_student_symmetric <- rt(n = n, df = df)
set.seed(231)
dist_t_student_symmetric1 <- rt(n = n, df = df)
set.seed(312)
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
set.seed(123)
# alpha: parametro skewness con 0 < alpha < 1; 
# nu1: nu1 > 0, grado di libertà per la coda sinistra 
# nu2: nu2 > 0, grado di libertà per la coda destra
dist_t_student_asymmetric <- rast(n = n, mu = 0, s = 1, alpha = 0.9, nu1 = 5, nu2 = Inf, pars = NULL)
set.seed(231)
dist_t_student_asymmetric1 <- rast(n = n, mu = 0, s = 1, alpha = 0.9, nu1 = 5, nu2 = Inf, pars = NULL)
set.seed(312)
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
    sigmasquared_t[2] <- a0 + aq[1] * (X_t^2) + aq[2] * (X0^2)
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
X0 <- 0.05
sigmasquared0 <- 0.2

##########################################################################################################################
"Costruisco un processo ARCH e GARCH per ogni distribuzione considerando q=1 e p=1 (solo per il modello GARCH)"

q <- 1
p <- 1 # utilizzato solo per Garch

# Definiamo parametri per un modello q=1 e p=1 in modo tale da soddisfare la condizione di stazionarietà: 
# Modello Arch: 1 - a1*σ^2 > 0 -> 1 > a1*σ^2
# Modello Garch: 1 - a1*σ^2 - b1 > 0 -> 1 > a1*σ^2 + b1
a0 <- 0.01
a1 <- 0.3 
b1 <- 0.05 # utilizzato solo per Garch

############################################## " MODELLO ARCH "

type_model = substitute(paste0("Model ARCH(", q, ")"))

########## DISTRIBUZIONE NORMALE
Xt_normal_arch_q1 <- model_arch(a0, a1, X0, dist_normal, q)
Xt_normal_arch_q1
Xt_normal_arch1_q1 <- model_arch(a0, a1, X0, dist_normal1, q)
Xt_normal_arch1_q1
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
Xt_t_student_symmetric_arch_q1 <- model_arch(a0, a1, X0, dist_t_student_symmetric, q)
Xt_t_student_symmetric_arch_q1
Xt_t_student_symmetric_arch1_q1 <- model_arch(a0, a1, X0, dist_t_student_symmetric1, q)
Xt_t_student_symmetric_arch1_q1
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
Xt_t_student_asymmetric_arch_q1 <- model_arch(a0, a1, X0, dist_t_student_asymmetric, q)
Xt_t_student_asymmetric_arch_q1
Xt_t_student_asymmetric_arch1_q1 <- model_arch(a0, a1, X0, dist_t_student_asymmetric1, q)
Xt_t_student_asymmetric_arch1_q1
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
Xt_normal_garch_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_normal, q, p)
Xt_normal_garch_q1_p1
Xt_normal_garch1_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_normal1, q, p)
Xt_normal_garch1_q1_p1
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
Xt_t_student_symmetric_garch_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_symmetric, q, p)
Xt_t_student_symmetric_garch_q1_p1
Xt_t_student_symmetric_garch1_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_symmetric1, q, p)
Xt_t_student_symmetric_garch1_q1_p1
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
Xt_t_student_asymmetric_garch_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_asymmetric, q, p)
Xt_t_student_asymmetric_garch_q1_p1
Xt_t_student_asymmetric_garch1_q1_p1 <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)
Xt_t_student_asymmetric_garch1_q1_p1
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
"Costruisco un processo GARCH per ogni distribuzione considerando q=1 e p=2 (solo per il modello GARCH)"

q <- 1
p <- 2 # utilizzato solo per Garch

# Definiamo parametri per un modello q=1 e p=2 in modo tale da soddisfare la condizione di stazionarietà: 
# Modello Garch: 1 - a1*σ^2 - b1 - b2 > 0 -> 1 > a1*σ^2 + b1 + b2
a0 <- 0.01
a1 <- 0.3 
bp <- c(0.05, 0.2) # utilizzato solo per Garch

############################################## " MODELLO GARCH "

type_model = substitute(paste0("Model GARCH(", q, ",", p, ")"))

##########  DISTRUBUZIONE NORMALE
Xt_normal_garch_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_normal, q, p)
Xt_normal_garch_q1_p2
Xt_normal_garch1_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_normal1, q, p)
Xt_normal_garch1_q1_p2
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
Xt_t_student_symmetric_garch_q1_p2<- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_symmetric, q, p)
Xt_t_student_symmetric_garch_q1_p2
Xt_t_student_symmetric_garch1_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_symmetric1, q, p)
Xt_t_student_symmetric_garch1_q1_p2
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
Xt_t_student_asymmetric_garch_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_asymmetric, q, p)
Xt_t_student_asymmetric_garch_q1_p2
Xt_t_student_asymmetric_garch1_q1_p2 <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)
Xt_t_student_asymmetric_garch1_q1_p2
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
"Costruisco un processo ARCH e GARCH per ogni distribuzione considerando q=2 e p=1 (solo per il modello GARCH)"

q <- 2
p <- 1 # utilizzato solo per Garch

# Definiamo parametri per un modello q=1 e p=1 in modo tale da soddisfare la condizione di stazionarietà: 
# Modello Arch: 1 - a1*σ^2 - a2*σ^2 -> 0 -> 1 > a1*σ^2 + a2*σ^2
# Modello Garch: 1 - a1*σ^2 - a2*σ^2 - b1 > 0 -> 1 > a1*σ^2 + a2*σ^2+ b1
a0 <- 0.01
aq <- c(0.3, 0.1) 
b1 <- 0.05 # utilizzato solo per Garch

############################################## " MODELLO ARCH "

type_model = substitute(paste0("Model ARCH(", q, ")"))

##########  DISTRIBUZIONE NORMALE
Xt_normal_arch_q2 <- model_arch(a0, aq, X0, dist_normal, q)
Xt_normal_arch_q2
Xt_normal_arch1_q2 <- model_arch(a0, aq, X0, dist_normal1, q)
Xt_normal_arch1_q2
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
Xt_t_student_symmetric_arch_q2 <- model_arch(a0, aq, X0, dist_t_student_symmetric, q)
Xt_t_student_symmetric_arch_q2
Xt_t_student_symmetric_arch1_q2 <- model_arch(a0, aq, X0, dist_t_student_symmetric1, q)
Xt_t_student_symmetric_arch1_q2
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
Xt_t_student_asymmetric_arch_q2 <- model_arch(a0, aq, X0, dist_t_student_asymmetric, q)
Xt_t_student_asymmetric_arch_q2
Xt_t_student_asymmetric_arch1_q2 <- model_arch(a0, aq, X0, dist_t_student_asymmetric1, q)
Xt_t_student_asymmetric_arch1_q2
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
Xt_normal_garch_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_normal, q, p)
Xt_normal_garch_q2_p1
Xt_normal_garch1_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_normal1, q, p)
Xt_normal_garch1_q2_p1
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
Xt_t_student_symmetric_garch_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_symmetric, q, p)
Xt_t_student_symmetric_garch_q2_p1
Xt_t_student_symmetric_garch1_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_symmetric1, q, p)
Xt_t_student_symmetric_garch1_q2_p1
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
Xt_t_student_asymmetric_garch_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_asymmetric, q, p)
Xt_t_student_asymmetric_garch_q2_p1
Xt_t_student_asymmetric_garch1_q2_p1 <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)
Xt_t_student_asymmetric_garch1_q2_p1
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
"Costruisco un processo GARCH per ogni distribuzione considerando q=2 e p=2 (solo per il modello GARCH)"

q <- 2
p <- 2 # utilizzato solo per Garch

# Definiamo parametri per un modello q=1 e p=1 in modo tale da soddisfare la condizione di stazionarietà: 
# Modello Arch: 1 - a1*σ^2 - a2*σ^2 > 0 -> 1 > a1*σ^2 + a2*σ^2
# Modello Garch: 1 - a1*σ^2 - a2*σ^2 - b1 - b2 > 0 -> 1 > a1*σ^2 + a2*σ^2 + b1 + b2
a0 <- 0.01
aq <- c(0.3, 0.1) 
bp <- c(0.05, 0.2) # utilizzato solo per Garch

############################################## " MODELLO GARCH "

type_model = substitute(paste0("Model GARCH(", q, ",", p, ")"))

##########  DISTRUBUZIONE NORMALE
Xt_normal_garch_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_normal, q, p)
Xt_normal_garch_q2_p2
Xt_normal_garch1_q2_p2<- model_garch(a0, aq, bp, X0, sigmasquared0, dist_normal1, q, p)
Xt_normal_garch1_q2_p2
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
Xt_t_student_symmetric_garch_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_symmetric, q, p)
Xt_t_student_symmetric_garch_q2_p2
Xt_t_student_symmetric_garch1_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_symmetric1, q, p)
Xt_t_student_symmetric_garch1_q2_p2
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
Xt_t_student_asymmetric_garch_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_asymmetric, q, p)
Xt_t_student_asymmetric_garch_q2_p2
Xt_t_student_asymmetric_garch1_q2_p2 <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)
Xt_t_student_asymmetric_garch1_q2_p2
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

# Consideriamo la seconda traiettoria del modello ARCH(1) con distribuzione normale
t <- 1:length(Xt_normal_arch2_q1)

#Consideriamo un modello lineare
Xt_normal_arch_lm <- lm(Xt_normal_arch2_q1~t, data=NULL)
summary(Xt_normal_arch_lm)

plot(Xt_normal_arch_lm,1)
plot(Xt_normal_arch_lm,3)

#BREUSCH-PAGAN test
# consideriamo un modello lineare
Xt_normal_arch_bp <- lmtest::bptest(formula = Xt_normal_arch_lm, varformula=NULL, studentize = TRUE, data=NULL)
show(Xt_normal_arch_bp)

"Si ha un p-value < 0.05, quindi, possiamo rigettare l'ipotesi nulla di omoschedasticità in favore 
dell'ipotesi alternativa di eteroschedasticità"

#WHITE test
# The studentized White test
Xt_normal_arch_w <- lmtest::bptest(formula = Xt_normal_arch_lm, varformula = ~ t+I(t^2), studentize = TRUE, data=NULL)
show(Xt_normal_arch_w)

"Si ha un p-value < 0.05, quindi, possiamo rigettare l'ipotesi nulla di omoschedasticità in favore 
dell'ipotesi alternativa"

#Plot of the autocorrelogram.
y <- Xt_normal_arch_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model ARCH(1)")))
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

# Ljiung-box test
y <- Xt_normal_arch_lm$residuals
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)

# Durbin-Watson test
dwtest(Xt_normal_arch_lm, alternative="two.sided")

# Breusch-Godfrey test
bgtest(Xt_normal_arch_lm, order=5, type="Chisq")

##########################################

# Consideriamo la prima traiettoria del modello ARCH(1) con distribuzione t-student simmetrica
t <- 1:length(Xt_t_student_symmetric_arch_q1)

#Consideriamo un modello lineare
Xt_t_student_symmetric_arch_lm <- lm(Xt_t_student_symmetric_arch_q1~t, data=NULL)
summary(Xt_t_student_symmetric_arch_lm)

plot(Xt_t_student_symmetric_arch_lm,1)
plot(Xt_t_student_symmetric_arch_lm,3)

#BREUSCH-PAGAN test
# consideriamo un modello lineare
Xt_t_student_symmetric_arch_bp <- lmtest::bptest(formula = Xt_t_student_symmetric_arch_lm, varformula=NULL,
                                                 studentize = TRUE, data=NULL)
show(Xt_t_student_symmetric_arch_bp)

"Si ha un p-value > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di omoschedasticità in favore 
dell'ipotesi alternativa"

#WHITE test
# The studentized White test
Xt_t_student_symmetric_arch_w <- lmtest::bptest(formula = Xt_t_student_symmetric_arch_lm, 
                                                varformula = ~ t+I(t^2), studentize = TRUE, data=NULL)
show(Xt_t_student_symmetric_arch_w)

"Si ha un p-value > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di omoschedasticità in favore 
dell'ipotesi alternativa"

#Plot of the autocorrelogram.
y <- Xt_t_student_symmetric_arch_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model ARCH(1)")))
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

# Ljiung-box test
y <- Xt_t_student_symmetric_arch_lm$residuals
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)

# Durbin-Watson test
dwtest(Xt_t_student_symmetric_arch_lm, alternative="two.sided")

# Breusch-Godfrey test
bgtest(Xt_t_student_symmetric_arch_lm, order=5, type="Chisq")

##########################################

# Consideriamo la prima traiettoria del modello ARCH(1) con distribuzione t-student asimmetrica
t <- 1:length(Xt_t_student_asymmetric_arch_q1)

#Consideriamo un modello lineare
Xt_t_student_asymmetric_arch_lm <- lm(Xt_t_student_asymmetric_arch_q1~t, data=NULL)
summary(Xt_t_student_asymmetric_arch_lm)

plot(Xt_t_student_asymmetric_arch_lm,1)
plot(Xt_t_student_asymmetric_arch_lm,3)

#BREUSCH-PAGAN test
# consideriamo un modello lineare
Xt_t_student_asymmetric_arch_bp <- lmtest::bptest(formula = Xt_t_student_asymmetric_arch_lm, varformula=NULL,
                                                 studentize = TRUE, data=NULL)
show(Xt_t_student_asymmetric_arch_bp)

"Si ha un p-value > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di omoschedasticità in favore 
dell'ipotesi alternativa"

#WHITE test
# The studentized White test
Xt_t_student_asymmetric_arch_w <- lmtest::bptest(formula = Xt_t_student_asymmetric_arch_lm, 
                                                varformula = ~ t+I(t^2), studentize = TRUE, data=NULL)
show(Xt_t_student_asymmetric_arch_w)

"Si ha un p-value > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di omoschedasticità in favore 
dell'ipotesi alternativa"

#Plot of the autocorrelogram.
y <- Xt_t_student_asymmetric_arch_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
title_content <- bquote(atop(.(content), paste("Plot of the Autocorrelogram of the Residuals in the Linear Model for the model ARCH(1)")))
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

# Ljiung-box test
y <- Xt_t_student_asymmetric_arch_lm$residuals
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)

# Durbin-Watson test
dwtest(Xt_t_student_asymmetric_arch_lm, alternative="two.sided")

# Breusch-Godfrey test
bgtest(Xt_t_student_asymmetric_arch_lm, order=5, type="Chisq")

##########################################
##########################################

