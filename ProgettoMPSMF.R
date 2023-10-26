"Progetto di Metodi Probabilistici e Statistici per i Mercati Finanziari

Il modello ARCH è un modello 'Autoregressive Conditional Heteroskedasticity' che assume 
che la varianza del termine corrente sia una funzione non lineare della varianza dei rumori precedenti. 
Un aspetto rilevante dei processi ARCH è la capacità di modellare il clustering della volatilità; 
questo significa che le serie storiche finanziarie possono mostrare periodi in cui la volatilità è
elevata e seguita da periodi di volatilità relativamente bassa. 

Il modello GARCH è modello 'Generalized Autoregressive Conditional Heteroskedasticity' ed 
è un'estensione del modello ARCH che consente di modellare la varianza del processo in base non solo 
ai valori precedenti del processo, ma anche ai valori precedenti della varianza.
"

author_content <- "Author: Melissa Petrolo"
content <- "University of Roma \"Tor Vergata\" - \u0040 Metodi Probabilistici e Statistici per i Mercati Finanziari 2022-2023"

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
library(tseries)

##########################################################################################################################
##########################################################################################################################
"Genero tre diverse traiettorie per tre distribuzioni differenti: 
t-student simmetrica, t-student asimmetrica, distribuzione normale"
samples <-500 # numero di campioni
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

#############################################################################################################
#############################################################################################################
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

# creo il primo plot del modello Arch(1) con la distribuzione normale
plot_dist_normal <- ggplot(data.frame(value = Xt_normal_arch_q1, index = seq_along(Xt_normal_arch_q1)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione normale
plot_dist_normal1 <- ggplot(data.frame(value = Xt_normal_arch1_q1, index = seq_along(Xt_normal_arch1_q1)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione normale
plot_dist_normal2 <- ggplot(data.frame(value = Xt_normal_arch2_q1, index = seq_along(Xt_normal_arch2_q1)), aes(x = index, y = value)) +
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
plots_dist_norm <- plot_grid(plot_dist_normal, plot_dist_normal1, plot_dist_normal2, nrow = 3)
plots <- plots_dist_norm +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=author_content) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(plots)
  
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

# Grid plot
# creo la figura utilizzando una griglia con i tre plot generati
type_dist = "Symmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)), " of a ", .(type_dist)) ))
subtitle_content <- (paste("path length ", (samples), " sample points"))
plots_dist_t_student_symmetric <- plot_grid(plot_t_student_symmetric, plot_t_student_symmetric1, plot_t_student_symmetric2, nrow = 3)
plots <- plots_dist_t_student_symmetric +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=author_content) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=-45, vjust=1),
        legend.key.width = unit(0.8,"cm"), legend.position="bottom")
plot(plots)

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

I test computazionali che vengono solitamente applicati per rilevare l'eteroschedasticità nelle serie 
temporali sono i test di Breusch-Pagan (BP) e White (W). 
Si ha: 
- ipotesi nulla: l'omoschedasticità è presente (i residui sono distributii con una uguale varianza);
- ipotesi alternativa: l'eteroschedasticità è presente ( i residui non sono distribuiti con una varianze uguali).

Se il p-value del test è minore di 0.05 allora possiamo rigettare l'ipotesi nulla e 
concludere che l'eteroschedasticità è presente.

I test computazionali che vengono solitamente applicati per rilevare assenza di autocorrelazione 
nelle serie temporali sono i test di Ljiung-box, Durbin-Watson e Breusch-Godfrey. 
Si ha: 
- ipotesi nulla: assenza di autocorrelazione (i residui del modello sono indipendentemente distribuiti);
- ipotesi alternativa: presenza di autocorrelazione (i residui del modello non sono indipendentemente distribuiti).

Se il p-value del test è minore di 0.05 allora possiamo rigettare l'ipotesi nulla e 
concludere che che ci sia presenza di autocorrelazione nei residui del modello.

"
##########################################

# Consideriamo una traiettoia con distribuzione normale di un modello ARCH(1)
Xt <- Xt_normal_arch_q1
df_Xt_normal_arch_q1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_normal_arch_q1_lm <- lm(Xt~t, data=df_Xt_normal_arch_q1)
summary(Xt_normal_arch_q1_lm)
summary(Xt_normal_arch_q1_lm$fitted.values)

Xt_normal_arch_q1_res <- Xt_normal_arch_q1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_arch_q1_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_arch_q1_res)                  # theoretical value 3.
# La skew è pari a -0.08652625; questo indica che la sua distribuzione dei dati nei resdui
# è molto vicina alla simmetria con una coda negativa.
# La kurtosi è pari a 2.835148; questo indica che la distribuzione è leggermenete  platykurtic, 
# cioè ha code più sottili rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_arch_q1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(1) of a normal distribution")))
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
Xt_normal_arch_q1_sp <- ggplot(Data_df) +
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
plot(Xt_normal_arch_q1_sp)

plot(Xt_normal_arch_q1_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari e omoschedastici.

# Stazionarietà
# Test Dickey-Fuller 
Xt_normal_arch_q1_df <- adf.test(Xt_normal_arch_q1)
show(Xt_normal_arch_q1_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_arch_q1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Arch(1) of a normal distribution")))
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
Xt_normal_arch_q1_sp <- ggplot(Data_df) +
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
plot(Xt_normal_arch_q1_sp)

plot(Xt_normal_arch_q1_lm,3) # Scale-location

# Omoschedasticità
# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_arch_q1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_arch_q1)
show(Xt_normal_arch_q1_bp)
# Si ha un p-value di 0.00615 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_arch_q1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_arch_q1)
show(Xt_normal_arch_q1_w)
# Si ha un p-value di 0.02191 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 3.7866, df = 1, p-value = 0.05166
# Si ha un p-value > 0.05, quindi non è possibile rifiutare l'ipotesi nulla di assenza di autocorrelazione.

# Test Durbin-Watson
dwtest(Xt_normal_arch_q1_lm, alternative="two.sided")
# DW = 1.8247, p-value = 0.04454
# Si ha un p-value < 0.05, quindi è possibile rifiutare l'ipotesi nulla di assenza di autocorrelazione.

# Test Breusch-Godfrey
bgtest(Xt_normal_arch_q1_lm, order=5, type="Chisq")
# LM test = 4.0142, df = 5, p-value = 0.5474
# Si ha un p-value > 0.05, quindi non è possibile rifiutare l'ipotesi nulla di assenza di autocorrelazione.

# I risultati indicano che c'è un'indicazione di autocorrelazione nei residui nel test di Durbin-Waston, mentre,
# nel test di Ljiung-box e Breusch-Godfrey non forniscono evidenze significative di autocorrelazione nei resisui.

##########################################

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello ARCH(1)
Xt <- Xt_t_student_symmetric_arch_q1
df_Xt_t_student_symmetric_arch_q1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_symmetric_arch_q1_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_arch_q1)
summary(Xt_t_student_symmetric_arch_q1_lm)
summary(Xt_t_student_symmetric_arch_q1_lm$fitted.values)

Xt_t_student_symmetric_arch_q1_res <- Xt_t_student_symmetric_arch_q1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_arch_q1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_arch_q1_res)                  # theoretical value 3.
# La skew è pari a 0.1786124; il suo valore è vicino allo zero possiamo dire che 
# la sua distribuzione è simmetrica.
# La kurtosi è pari a 4.836038; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_arch_q1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(1) of a normal distribution")))
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
Xt_t_student_symmetric_arch_q1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_arch_q1_sp)

plot(Xt_t_student_symmetric_arch_q1_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_symmetric_arch_q1_df <- adf.test(Xt_t_student_symmetric_arch_q1)
show(Xt_t_student_symmetric_arch_q1_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_arch_q1_res)
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
Xt_t_student_symmetric_arch_q1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_arch_q1_sp)

plot(Xt_t_student_symmetric_arch_q1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_arch_q1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_arch_q1)
show(Xt_t_student_symmetric_arch_q1_bp)
# Si ha un p-value di 0.001644 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_arch_q1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_arch_q1)
show(Xt_t_student_symmetric_arch_q1_w)
# Si ha un p-value di 0.007024 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 0.47606, df = 1, p-value = 0.4902

# Test Durbin-Watson
dwtest(Xt_t_student_symmetric_arch_q1_lm, alternative="two.sided")
# DW = 1.9381, p-value = 0.4599

# Test Breusch-Godfrey
bgtest(Xt_t_student_symmetric_arch_q1_lm, order=5, type="Chisq")
# LM test = 2.4665, df = 5, p-value = 0.7815

# Per tutti e tre i test si ha un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di no autocorrelazione.

##########################################

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello ARCH(1)
Xt <- Xt_t_student_asymmetric_arch_q1
df_Xt_t_student_asymmetric_arch_q1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_arch_q1_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_arch_q1)
summary(Xt_t_student_asymmetric_arch_q1_lm)
summary(Xt_t_student_asymmetric_arch_q1_lm$fitted.values)

Xt_t_student_asymmetric_arch_q1_res <- Xt_t_student_asymmetric_arch_q1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_arch_q1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_arch_q1_res)                  # theoretical value 3.
# La skew è pari a -1.675295; questo indica una forte asimmetria verso sinistra
# con una coda lunga negativa.
# La kurtosi è pari a 7.014481; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_arch_q1_res)
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

plot(Xt_t_student_asymmetric_arch_q1_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_asymmetric_arch_q1_df <- adf.test(Xt_t_student_asymmetric_arch_q1)
show(Xt_t_student_asymmetric_arch_q1_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

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

plot(Xt_t_student_asymmetric_arch_q1_lm,3) # Scale-location

#Determiniamo se la serie è eteroschedastico:
# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_arch_q1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_arch_q1)
show(Xt_t_student_asymmetric_arch_q1_bp)
# Si ha un p-value di 0.01372 > 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# una banda ristretta eccetto nel primo lag, quindi, possiamo dire che che la serie non è significativamente 
# correlata con le serie ritardate, ovvero che il passato non "spiega" il presente e che le 
# variazioni da un istante o periodo ad un altro sono sostanzialmente casuali

# Test Ljiung-box
y <- Xt_t_student_asymmetric_arch_q1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 12.319, df = 1, p-value = 0.0004484

# Test Durbin-Watson
dwtest(Xt_t_student_asymmetric_arch_q1_lm, alternative="two.sided")
# DW = 1.6837, p-value = 0.000333

# Test Breusch-Godfrey
bgtest(Xt_t_student_asymmetric_arch_q1_lm, order=5, type="Chisq")
# LM test = 12.47, df = 5, p-value = 0.02888

# Per tutti e tre i test si ha un p-value < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di no autocorrelazione.

##########################################
##########################################

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(1,1)
Xt <- Xt_normal_garch1_q1_p1
df_Xt_normal_garch_q1_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_normal_garch_q1_p1_lm <- lm(Xt~t, data=df_Xt_normal_garch_q1_p1)
summary(Xt_normal_garch_q1_p1_lm)
summary(Xt_normal_garch_q1_p1_lm$fitted.values)

Xt_normal_garch_q1_p1_res <- Xt_normal_garch_q1_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_garch_q1_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_garch_q1_p1_res)                  # theoretical
# La skew è pari a -0.03823827; il suo valore è prossimo a zero e negativo, quindi la sua distribuzione 
# viene considerata approssimativamente simmetrica con una coda negativa.
# La kurtosi è pari a 2.942394; questo indica che la distribuzione è leggermenete platykurtic

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch_q1_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) of a normal distribution")))
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
Xt_normal_garch_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch_q1_p1_sp)

plot(Xt_normal_garch_q1_p1_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Stazionarietà
# Test Dickey-Fuller 
Xt_normal_garch_q1_p1_df <- adf.test(Xt_normal_garch_q1_p1)
show(Xt_normal_garch_q1_p1_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch_q1_p1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(1,1) of a normal distribution")))
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
Xt_normal_garch_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch_q1_p1_sp)

plot(Xt_normal_garch_q1_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch_q1_p1)
show(Xt_normal_garch_q1_p1_bp)
# Si ha un p-value di 0.7758 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch_q1_p1)
show(Xt_normal_garch_q1_p1_w)
# Si ha un p-value di 0.9314 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 0.20218, df = 1, p-value = 0.653

# Test Durbin-Watson
dwtest(Xt_normal_garch_q1_p1_lm, alternative="two.sided")
# DW = 2.0391, p-value = 0.694

# Test Breusch-Godfrey
bgtest(Xt_normal_garch_q1_p1_lm, order=5, type="Chisq")
# LM test = 5.6334, df = 5, p-value = 0.3435

# Per tutti e tre i test si ha un p-value < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di no autocorrelazione.

##########################################

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello GARCH(1,1)
Xt <- Xt_t_student_symmetric_garch2_q1_p1
df_Xt_t_student_symmetric_garch_q1_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch_q1_p1_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch_q1_p1)
summary(Xt_t_student_symmetric_garch_q1_p1_lm)
summary(Xt_t_student_symmetric_garch_q1_p1_lm$fitted.values)

Xt_t_student_symmetric_garch_q1_p1_res <- Xt_t_student_symmetric_garch_q1_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_garch_q1_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_garch_q1_p1_res)                  # theoretical value 3.
# La skew è pari a -0.01744386; il suo valore è negativo e la sua distribuzione è approssimativamente simmetrica.
# La kurtosi è pari a 4.204011; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch_q1_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) of a normal distribution")))
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
Xt_t_student_symmetric_garch_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch_q1_p1_sp)

plot(Xt_t_student_symmetric_garch_q1_p1_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_symmetric_garch_q1_p1_df <- adf.test(Xt_t_student_symmetric_garch_q1_p1)
show(Xt_t_student_symmetric_garch_q1_p1_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch_q1_p1_res)
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
Xt_t_student_symmetric_garch_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch_q1_p1_sp)

plot(Xt_t_student_symmetric_garch_q1_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q1_p1)
show(Xt_t_student_symmetric_garch_q1_p1_bp)
# Si ha un p-value di 0.1184 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q1_p1)
show(Xt_t_student_symmetric_garch_q1_p1_w)
# Si ha un p-value di 0.1875 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 2.932, df = 1, p-value = 0.08684

# Test Durbin-Watson
dwtest(Xt_t_student_symmetric_garch_q1_p1_lm, alternative="two.sided")
# DW = 2.1514, p-value = 0.09866

# Test Breusch-Godfrey
bgtest(Xt_t_student_symmetric_garch_q1_p1_lm, order=5, type="Chisq")
# LM test = 7.2349, df = 5, p-value = 0.2037

# Per tutti e tre i test si ha un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di no autocorrelazione.

##########################################

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello GARCH(1,1)
Xt <- Xt_t_student_asymmetric_garch1_q1_p1
df_Xt_t_student_asymmetric_garch_q1_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch_q1_p1_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch_q1_p1)
summary(Xt_t_student_asymmetric_garch_q1_p1_lm)
summary(Xt_t_student_asymmetric_garch_q1_p1_lm$fitted.values)

Xt_t_student_asymmetric_garch_q1_p1_res <- Xt_t_student_asymmetric_garch_q1_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_garch_q1_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_garch_q1_p1_res)                  # theoretical value 3.
# La skew è pari a -1.732549; il suo valore è negativo e la sua distribuzione è
# asimmetrica verso sinistra con una coda lunga negativa.
# La kurtosi è pari a 7.407139; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch_q1_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) of a asymmetric t-student distribution")))
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
Xt_t_student_asymmetric_garch_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch_q1_p1_sp)

plot(Xt_t_student_asymmetric_garch_q1_p1_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_asymmetric_garch_q1_p1_df <- adf.test(Xt_t_student_asymmetric_garch_q1_p1)
show(Xt_t_student_asymmetric_garch_q1_p1_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch_q1_p1_res)
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
Xt_t_student_asymmetric_garch_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch_q1_p1_sp)

plot(Xt_t_student_asymmetric_garch_q1_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q1_p1)
show(Xt_t_student_asymmetric_garch_q1_p1_bp)
# Si ha un p-value di 0.02387 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q1_p1)
show(Xt_t_student_asymmetric_garch_q1_p1_w)
# Si ha un p-value di 0.03169 > 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 12.535, df = 1, p-value = 0.0003994

# Test Durbin-Watson
dwtest(Xt_t_student_asymmetric_garch_q1_p1_lm, alternative="two.sided")
# DW = 1.6801, p-value = 0.0002842

# Test Breusch-Godfrey
bgtest(Xt_t_student_asymmetric_garch_q1_p1_lm, order=5, type="Chisq")
# LM test = 17.88, df = 5, p-value = 0.0031

# I risultati dei tre test hanno un p-value < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di no autocorrelazione.

##########################################
##########################################

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(1,2)
Xt <- Xt_normal_garch_q1_p2
df_Xt_normal_garch_q1_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_normal_garch_q1_p2_lm <- lm(Xt~t, data=df_Xt_normal_garch_q1_p2)
summary(Xt_normal_garch_q1_p2_lm)
summary(Xt_normal_garch_q1_p2_lm$fitted.values)

Xt_normal_garch_q1_p2_res <- Xt_normal_garch_q1_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_garch_q1_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_garch_q1_p2_res)                  # theoretical value 3.
# La skew è pari a -0.08092882; il suo valore è prossimo a zero, quindi la sua distribuzione 
# viene considerata simmetrica con una coda negativa.
# La kurtosi è pari a 2.832292; questo indica che la distribuzione è leggermenete platykurtic.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch_q1_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,2) of a normal distribution")))
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
Xt_normal_garch_q1_p2_sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch_q1_p2_sp)

plot(Xt_normal_garch_q1_p2_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Stazionarietà
# Test Dickey-Fuller 
Xt_normal_garch_q1_p2_df <- adf.test(Xt_normal_garch_q1_p2)
show(Xt_normal_garch_q1_p2_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch_q1_p2_res)
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
Xt_normal_garch_q1_p2_sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch_q1_p2_sp)

plot(Xt_normal_garch_q1_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch_q1_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch_q1_p2)
show(Xt_normal_garch_q1_p2_bp)
# Si ha un p-value di 0.004426 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch_q1_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch_q1_p2)
show(Xt_normal_garch_q1_p2_w)
# Si ha un p-value di 0.0166 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 3.783, df = 1, p-value = 0.05177
# Si ha un p-value > 0.05, quindi non è possibile rigettare l'ipotesi nulla di 
# assenza di autocorrelazione.

# Test Durbin-Watson
dwtest(Xt_normal_garch_q1_p2_lm, alternative="two.sided")
# DW = 1.8248, p-value = 0.04465
# Si ha un p-value < 0.05, quindi è possibile rigettare l'ipotesi nulla di 
# assenza di autocorrelazione.

# Test Breusch-Godfrey
bgtest(Xt_normal_garch_q1_p2_lm, order=5, type="Chisq")
# LM test = 3.9654, df = 5, p-value = 0.5544
# Si ha un p-value > 0.05, quindi non è possibile rigettare l'ipotesi nulla di 
# assenza di autocorrelazione.

# I risultati indicano che c'è una presenza di autocorrelazione per il test di
# Durbin-Wtason; mentre, c'è un'assenza di autocorrelazione per il test di Box-Lijung 
# e Breusch-Gdfrey.

##########################################

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello GARCH(1,2)
Xt <- Xt_t_student_symmetric_garch_q1_p2
df_Xt_t_student_symmetric_garch_q1_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch_q1_p2_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch_q1_p2)
summary(Xt_t_student_symmetric_garch_q1_p2_lm)
summary(Xt_t_student_symmetric_garch_q1_p2_lm$fitted.values)

Xt_t_student_symmetric_garch_q1_p2_res <- Xt_t_student_symmetric_garch_q1_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_garch_q1_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_garch_q1_p2_res)                  # theoretical value 3.
# La skew è pari a 0.1368888; il suo valore è positivo e vicino allo zero; quindi, la sua distribuzione
# è vicina alla simmetria.
# La kurtosi è pari a 5.499922; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch_q1_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,2) of a symmetric t-student distribution")))
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
Xt_t_student_symmetric_garch_q1_p2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch_q1_p2_sp)

plot(Xt_t_student_symmetric_garch_q1_p2_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_symmetric_garch_q1_p2_df <- adf.test(Xt_t_student_symmetric_garch_q1_p2)
show(Xt_t_student_symmetric_garch_q1_p2_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch_q1_p2_res)
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
Xt_t_student_symmetric_garch_q1_p2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch_q1_p2_sp)

plot(Xt_t_student_symmetric_garch_q1_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch_q1_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q1_p2)
show(Xt_t_student_symmetric_garch_q1_p2_bp)
# Si ha un p-value di 0.002272 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch_q1_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q1_p2)
show(Xt_t_student_symmetric_garch_q1_p2_w)
# Si ha un p-value di 0.009257 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 0.40705, df = 1, p-value = 0.5235

# Test Durbin-Watson
dwtest(Xt_t_student_symmetric_garch_q1_p2_lm, alternative="two.sided")
# DW = 1.9426, p-value = 0.4916

# Test Breusch-Godfrey
bgtest(Xt_t_student_symmetric_garch_q1_p2_lm, order=5, type="Chisq")
# LM test = 2.7652, df = 5, p-value = 0.7361

# Per tutti e tre i test si ha un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di no autocorrelazione.

##########################################

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello GARCH(1,1)
Xt <- Xt_t_student_asymmetric_garch_q1_p2
df_Xt_t_student_asymmetric_garch_q1_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch_q1_p2_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch_q1_p2)
summary(Xt_t_student_asymmetric_garch_q1_p2_lm)
summary(Xt_t_student_asymmetric_garch_q1_p2_lm$fitted.values)

Xt_t_student_asymmetric_garch_q1_p2_res <- Xt_t_student_asymmetric_garch_q1_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_garch_q1_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_garch_q1_p2_res)                  # theoretical value 3.
# La skew è pari a -1.728404; il suo valore è negativo e la sua distribuzione è
# asimmetrica verso sinistra con una coda lunga negativa.
# La kurtosi è pari a 7.426788; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch_q1_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,2) of a asymmetric t-student distribution")))
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
Xt_t_student_asymmetric_garch_q1_p2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch_q1_p2_sp)

plot(Xt_t_student_asymmetric_garch_q1_p2_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_asymmetric_garch_q1_p2_df <- adf.test(Xt_t_student_asymmetric_garch_q1_p2)
show(Xt_t_student_asymmetric_garch_q1_p2_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch_q1_p2_res)
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
Xt_t_student_asymmetric_garch_q1_p2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch_q1_p2_sp)

plot(Xt_t_student_asymmetric_garch_q1_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch_q1_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q1_p2)
show(Xt_t_student_asymmetric_garch_q1_p2_bp)
# Si ha un p-value di 0.0178 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch_q1_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q1_p2)
show(Xt_t_student_asymmetric_garch_q1_p2_w)
# Si ha un p-value di 0.0594 > 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# una banda ristretta, anche se in alcuni lag questo valore esce dall'intervallo.

# Test Ljiung-box
y <- Xt_t_student_asymmetric_garch_q1_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 13.422, df = 1, p-value = 0.0002487

# Test Durbin-Watson
dwtest(Xt_t_student_asymmetric_garch_q1_p2_lm, alternative="two.sided")
# DW = 1.67, p-value = 0.000183

# Test Breusch-Godfrey
bgtest(Xt_t_student_asymmetric_garch_q1_p2_lm, order=5, type="Chisq")
# LM test = 14.04, df = 5, p-value = 0.01536

# Per tutti e tre i test si ha un p-value < 0.05, ciò significa che ci sono prove
# di autocorrelazione nei residui del modello

##########################################
##########################################

# Consideriamo una traiettoia con distribuzione normale di un modello ARCH(2)
Xt <- Xt_normal_arch_q2
df_Xt_normal_arch_q2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_normal_arch_q2_lm <- lm(Xt~t, data=df_Xt_normal_arch_q2)
summary(Xt_normal_arch_q2_lm)
summary(Xt_normal_arch_q2_lm$fitted.values)

Xt_normal_arch_q2_res <- Xt_normal_arch_q2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_arch_q2_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_arch_q2_res)                  # theoretical value 3.
# La skew è pari a -0.1206737; il suo valore è prossimo a zero, quindi la sua distribuzione potrebbe essere
# considerata approssimativamente simmetrica.
# La kurtosi è pari a 2.890637; questo indica che la distribuzione è leggermenete platykurtic.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_arch_q2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(2) of a normal distribution")))
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
Xt_normal_arch_q2_sp <- ggplot(Data_df) +
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
plot(Xt_normal_arch_q2_sp)

plot(Xt_normal_arch_q2_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari e omoschedastici.

# Stazionarietà
# Test Dickey-Fuller 
Xt_normal_arch_q2_df <- adf.test(Xt_normal_arch_q2)
show(Xt_normal_arch_q2_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_arch_q2_res)
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
Xt_normal_arch_q2_sp <- ggplot(Data_df) +
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
plot(Xt_normal_arch_q2_sp)

plot(Xt_normal_arch_q2_lm,3) # Scale-location
# Dal grafico "Scale-Location" possiamo notare omoschedasticità nei residui.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_arch_q2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_arch_q2)
show(Xt_normal_arch_q2_bp)
# Si ha un p-value di 0.004084 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_arch_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_arch_q2)
show(Xt_normal_arch_q2_w)
# Si ha un p-value di 0.01576 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano entro 
# una banda ristretta eccetto in due lag.

# Test Ljiung-box
y <- Xt_normal_arch_q2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 4.4703, df = 1, p-value = 0.03449

# Test Durbin-Watson
dwtest(Xt_normal_arch_q2_lm, alternative="two.sided")
# DW = 1.8093, p-value = 0.02916

# Test Breusch-Godfrey
bgtest(Xt_normal_arch_q2_lm, order=5, type="Chisq")
# LM test = 4.551, df = 5, p-value = 0.4731

# I risultati indicano che c'è una presenza di autocorrelazione nei residui del modello con i test Box-Ljung e Durbin-Watson, 
# ma non ci sono evidenze di correlazione seriale nel test Breusch-Godfrey.

##########################################

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello ARCH(2)
Xt <- Xt_t_student_symmetric_arch1_q2
df_Xt_t_student_symmetric_arch_q2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_symmetric_arch_q2_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_arch_q2)
summary(Xt_t_student_symmetric_arch_q2_lm)
summary(Xt_t_student_symmetric_arch_q2_lm$fitted.values)

Xt_t_student_symmetric_arch_q2_res <- Xt_t_student_symmetric_arch_q2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_arch_q2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_arch_q2_res)                  # theoretical value 3.
# La skew è pari a 0.2677586; il suo valore è prossimo a zero, quindi la sua distribuzione potrebbe essere
# considerata approssimativamente simmetrica.
# La kurtosi è pari a 9.305962; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_arch_q2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(2) of a symmetric t-student distribution")))
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
Xt_t_student_symmetric_arch_q2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_arch_q2_sp)

plot(Xt_t_student_symmetric_arch_q2_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari e omoschedastici.

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_symmetric_arch_q2_df <- adf.test(Xt_t_student_symmetric_arch_q2)
show(Xt_t_student_symmetric_arch_q2_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_arch_q2_res)
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
Xt_t_student_symmetric_arch_q2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_arch_q2_sp)

plot(Xt_t_student_symmetric_arch_q2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_arch_q2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_arch_q2)
show(Xt_t_student_symmetric_arch_q2_bp)
# Si ha un p-value di 0.05495 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_arch_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_arch_q2)
show(Xt_t_student_symmetric_arch_q2_w)
# Si ha un p-value di 0.1056 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
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
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano entro 
# una banda ristretta, eccetto per il lag 15.

# Test Ljiung-box
y <- Xt_t_student_symmetric_arch_q2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 1.1745, df = 1, p-value = 0.2785

# Test Durbin-Watson
dwtest(Xt_t_student_symmetric_arch_q2_lm, alternative="two.sided")
# DW = 2.0965, p-value = 0.3002

# Test Breusch-Godfrey
bgtest(Xt_t_student_symmetric_arch_q2_lm, order=5, type="Chisq")
# LM test = 2.7325, df = 5, p-value = 0.7411

# I risultati dei test hanno un p-value > 0.05; questo, significa che c'è assenza di autocorrelazione.

##########################################

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello ARCH(2)
Xt <- Xt_t_student_asymmetric_arch_q2
df_Xt_t_student_asymmetric_arch_q2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_arch_q2_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_arch_q2)
summary(Xt_t_student_asymmetric_arch_q2_lm)
summary(Xt_t_student_asymmetric_arch_q2_lm$fitted.values)

Xt_t_student_asymmetric_arch_q2_res <- Xt_t_student_asymmetric_arch_q2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_arch_q2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_arch_q2_res)                  # theoretical value 3.
# La skew è pari a -1.876595; questo indica una forte asimmetria verso sinistra
# con una coda lunga negativa.
# La kurtosi è pari a 8.390935; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_arch_q2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(2) of a asymmetric t-student distribution")))
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
Xt_t_student_asymmetric_arch_q2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_arch_q2_sp)

plot(Xt_t_student_asymmetric_arch_q2_lm,1) # Residuals vs Fitted

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_asymmetric_arch_q2_df <- adf.test(Xt_t_student_asymmetric_arch_q2)
show(Xt_t_student_asymmetric_arch_q2_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_arch_q2_res)
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
Xt_t_student_asymmetric_arch_q2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_arch_q2_sp)

plot(Xt_t_student_asymmetric_arch_q2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_arch_q2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_arch_q2)
show(Xt_t_student_asymmetric_arch_q2_bp)
# Si ha un p-value di  0.008644 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_arch_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_arch_q2)
show(Xt_t_student_asymmetric_arch_q2_w)
# Si ha un p-value di 0.0311 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# trend; mentre, dal lag 5 i valori oscillano all'interno dell'intervallo.

# Test Ljiung-box
y <- Xt_t_student_asymmetric_arch_q2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 18.342, df = 1, p-value = 1.846e-05

# Test Durbin-Watson
dwtest(Xt_t_student_asymmetric_arch_q2_lm, alternative="two.sided")
# DW = 1.615, p-value = 1.309e-05

# Test Breusch-Godfrey
bgtest(Xt_t_student_asymmetric_arch_q2_lm, order=5, type="Chisq")
# LM test = 26.239, df = 5, p-value = 8.019e-05

# Per tutti e tre i test si ha un p-value < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di no autocorrelazione.

##########################################
##########################################

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(2,1)
Xt <- Xt_normal_garch_q2_p1
df_Xt_normal_garch_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_normal_garch_q2_p1_lm <- lm(Xt~t, data=df_Xt_normal_garch_q2_p1)
summary(Xt_normal_garch_q2_p1_lm)
summary(Xt_normal_garch_q2_p1_lm$fitted.values)

Xt_normal_garch_q2_p1_res <- Xt_normal_garch_q2_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_garch_q2_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_garch_q2_p1_res)                  # theoretical value 3.
# La skew è pari a -0.06363277; il suo valore è prossimo a zero, quindi la sua distribuzione 
# viene considerata simmetrica con una coda negativa.
# La kurtosi è pari a 2.944331; questo indica che la distribuzione è leggermenete platykurtic.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch_q2_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,1) of a normal distribution")))
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
Xt_normal_garch_q2_p1_sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch_q2_p1_sp)

plot(Xt_normal_garch_q2_p1_lm,1) # Residuals vs Fitted

# Stazionarietà
# Test Dickey-Fuller 
Xt_normal_garch_q2_p1_df <- adf.test(Xt_normal_garch_q2_p1)
show(Xt_normal_garch_q2_p1_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch_q2_p1_res)
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
Xt_normal_garch_q2_p1_sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch_q2_p1_sp)

plot(Xt_normal_garch_q2_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch_q2_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch_q2_p1)
show(Xt_normal_garch_q2_p1_bp)
# Si ha un p-value di 0.0008743 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch_q2_p1)
show(Xt_normal_garch_q2_p1_w)
# Si ha un p-value di 0.003932 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# una banda ristretta.

# Test Ljiung-box
y <- Xt_normal_garch_q2_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 4.6602, df = 1, p-value = 0.03087

# Test Durbin-Watson
dwtest(Xt_normal_garch_q2_p1_lm, alternative="two.sided")
# DW = 1.8052, p-value = 0.02596

# Test Breusch-Godfrey
bgtest(Xt_normal_garch_q2_p1_lm, order=5, type="Chisq")
# LM test = 5.0596, df = 5, p-value = 0.4086

# I risultati indicano che ci potrebbe essere autocorrelazione nei residui del modello con i test Box-Ljung e Durbin-Watson,
# ma non ci sono evidenze di correlazione seriale con il test Breusch-Godfrey.

##########################################

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello GARCH(2,1)
Xt <- Xt_t_student_symmetric_garch1_q2_p1
df_Xt_t_student_symmetric_garch_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch_q2_p1_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch_q2_p1)
summary(Xt_t_student_symmetric_garch_q2_p1_lm)
summary(Xt_t_student_symmetric_garch_q2_p1_lm$fitted.values)

Xt_t_student_symmetric_garch_q2_p1_res <- Xt_t_student_symmetric_garch_q2_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_garch_q2_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_garch_q2_p1_res)                  # theoretical value 3.
# La skew è pari a 0.3829596; il suo valore è vicino allo zero; quindi, la sua distribuzione
# è vicina alla simmetria.
# La kurtosi è pari a 11.59399; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch_q2_p1_res)
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
Xt_t_student_symmetric_garch_q2_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch_q2_p1_sp)

plot(Xt_t_student_symmetric_garch_q2_p1_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_symmetric_garch_q2_p1_df <- adf.test(Xt_t_student_symmetric_garch_q2_p1)
show(Xt_t_student_symmetric_garch_q2_p1_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch_q2_p1_res)
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
Xt_t_student_symmetric_garch_q2_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch_q2_p1_sp)

plot(Xt_t_student_symmetric_garch_q2_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch_q2_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q2_p1)
show(Xt_t_student_symmetric_garch_q2_p1_bp)
# Si ha un p-value di 0.06841 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q2_p1)
show(Xt_t_student_symmetric_garch_q2_p1_w)
# Si ha un p-value di 0.07795 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
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
# una banda ristretta

# Test Ljiung-box
y <- Xt_t_student_symmetric_garch_q2_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 1.5367, df = 1, p-value = 0.2151

# Test Durbin-Watson
dwtest(Xt_t_student_symmetric_garch_q2_p1_lm, alternative="two.sided")
# DW = 2.1104, p-value = 0.2332

# Test Breusch-Godfrey
bgtest(Xt_t_student_symmetric_garch_q2_p1_lm, order=5, type="Chisq")
# LM test = 2.9784, df = 5, p-value = 0.7033

# I risultati indicano un p-value > 0.05; ciò indica
# un'assenza di autocorrelazione nei residui del modello.

##########################################

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello GARCH(2,1)
Xt <- Xt_t_student_asymmetric_garch_q2_p1
df_Xt_t_student_asymmetric_garch_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch_q2_p1_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch_q2_p1)
summary(Xt_t_student_asymmetric_garch_q2_p1_lm)
summary(Xt_t_student_asymmetric_garch_q2_p1_lm_lm$fitted.values)

Xt_t_student_asymmetric_garch_q2_p1_res <- Xt_t_student_asymmetric_garch_q2_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_garch_q2_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_garch_q2_p1_res)                  # theoretical value 3.
# La skew è pari a -2.055272; il suo valore è negativo e la sua distribuzione è
# asimmetrica verso sinistra con una coda lunga negativa.
# La kurtosi è pari a 9.592723; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch_q2_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,1) of a asymmetric t-student distribution")))
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
Xt_t_student_asymmetric_garch_q2_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch_q2_p1_sp)

plot(Xt_t_student_asymmetric_garch_q2_p1_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_asymmetric_garch_q2_p1_df <- adf.test(Xt_t_student_asymmetric_garch_q2_p1)
show(Xt_t_student_asymmetric_garch_q2_p1_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch_q2_p1_res)
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
Xt_t_student_asymmetric_garch_q2_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch_q2_p1_sp)

plot(Xt_t_student_asymmetric_garch_q2_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch_q2_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q2_p1)
show(Xt_t_student_asymmetric_garch_q2_p1_bp)
# Si ha un p-value di 0.01844 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q2_p1)
show(Xt_t_student_asymmetric_garch_q2_p1_w)
# Si ha un p-value di 0.06222 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Nel test di Breusch-Pagan il p-value < 0.05 ed indica una presenza di eteroschedasticità nei residui del modello; ma,
# nel test di White il p-value > 0.05 ed indica che la presenza di eteroschedasticità è meno chiara nel momento in cui si considerano
# due gradi di libertà poichè non possiamo rifiutare l'ipotesi nulla di omoschedasticità.

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
y <- Xt_t_student_asymmetric_garch_q2_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 24.414, df = 1, p-value = 7.771e-07

# Test Durbin-Watson
dwtest(Xt_t_student_asymmetric_garch_q2_p1_lm, alternative="two.sided")
# DW = 1.5564, p-value = 5.321e-07

# Test Breusch-Godfrey
bgtest(Xt_t_student_asymmetric_garch_q2_p1_lm, order=5, type="Chisq")
# LM test = 36.094, df = 5, p-value = 9.097e-07

# I risultati hanno un p-value < 0.05; ciò indica
# un'evidenza di autocorrelazione nei residui del modello.

##########################################
##########################################

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(2,2)
Xt <- Xt_normal_garch_q2_p2
df_Xt_normal_garch_q2_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_normal_garch_q2_p2_lm <- lm(Xt~t, data=df_Xt_normal_garch_q2_p2)
summary(Xt_normal_garch_q2_p2_lm)
summary(Xt_normal_garch_q2_p2_lm$fitted.values)

Xt_normal_garch_q2_p2_res <- Xt_normal_garch_q2_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_garch_q2_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_garch_q2_p2_res)                  # theoretical value 3.
# La skew è pari a -0.07990184; il suo valore è prossimo a zero, quindi la sua distribuzione 
# viene considerata simmetrica.
# La kurtosi è pari a 2.925623; questo indica che la distribuzione è platykurtic

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch_q2_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,2) of a normal distribution")))
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
Xt_normal_garch_q2_p2_sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch_q2_p2_sp)

plot(Xt_normal_garch_q2_p2_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Stazionarietà
# Test Dickey-Fuller 
Xt_normal_garch_q2_p2_df <- adf.test(Xt_normal_garch_q2_p2)
show(Xt_normal_garch_q2_p2_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch_q2_p2_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(2,2) of a normal distribution")))
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
Xt_normal_garch_q2_p2_sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch_q2_p2_sp)

plot(Xt_normal_garch_q2_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch_q2_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch_q2_p2)
show(Xt_normal_garch_q2_p2_bp)
# Si ha un p-value di 0.0005444 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch_q2_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch_q2_p2)
show(Xt_normal_garch_q2_p2_w)
# Si ha un p-value di 0.00252 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 4.4317, df = 1, p-value = 0.03528

# Test Durbin-Watson
dwtest(Xt_normal_garch_q2_p2_lm, alternative="two.sided")
# DW = 1.8098, p-value = 0.0296

# Test Breusch-Godfrey
bgtest(Xt_normal_garch_q2_p2_lm, order=5, type="Chisq")
# LM test = 4.7018, df = 5, p-value = 0.4533

# I risultati del test di Ljung-Box e Durbin-Watson hanno un p-value < 0.05 indicando una presenza di autocorrelazione
# nella serie; mentre, il p-value del test di Breusch-Godfrey indica un'assenza di correlazione seriale nella serie.

##########################################

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello GARCH(2,2)
Xt <- Xt_t_student_symmetric_garch1_q2_p2
df_Xt_t_student_symmetric_garch_q2_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch_q2_p2_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch_q2_p2)
summary(Xt_t_student_symmetric_garch_q2_p2_lm)
summary(Xt_t_student_symmetric_garch_q2_p2_lm$fitted.values)

Xt_t_student_symmetric_garch_q2_p2_res <- Xt_t_student_symmetric_garch_q2_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_garch_q2_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_garch_q2_p2_res)                  # theoretical value 3.
# La skew è pari a 0.3197632; il suo valore è prossima allo zero, quindi possiamo dire 
# che la sua distribuzione è simmetrica.
# La kurtosi è pari a 10.48433; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch_q2_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,2) of a symmetric t-student distribution")))
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
Xt_t_student_symmetric_garch_q2_p2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch_q2_p2_sp)

plot(Xt_t_student_symmetric_garch_q2_p2_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_symmetric_garch_q2_p2_df <- adf.test(Xt_t_student_symmetric_garch_q2_p2)
show(Xt_t_student_symmetric_garch_q2_p2_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch_q2_p2_res)
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
Xt_t_student_symmetric_garch_q2_p2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch_q2_p2_sp)

plot(Xt_t_student_symmetric_garch_q2_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch_q2_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q2_p2)
show(Xt_t_student_symmetric_garch_q2_p2_bp)
# Si ha un p-value di 0.08959 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch_q2_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q2_p2)
show(Xt_t_student_symmetric_garch_q2_p2_w)
# Si ha un p-value di 0.08332 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
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
# eccetto nel lag 15

# Test Ljiung-box
y <- Xt_t_student_symmetric_garch_q2_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 1.0338, df = 1, p-value = 0.3093

# Test Durbin-Watson
dwtest(Xt_t_student_symmetric_garch_q2_p2_lm, alternative="two.sided")
# DW = 2.0905, p-value = 0.3324

# Test Breusch-Godfrey
bgtest(Xt_t_student_symmetric_garch_q2_p2_lm, order=5, type="Chisq")
# LM test = 2.319, df = 5, p-value = 0.8035

# Per i test di Ljiung-box e Burbin-Wtason si ha un p-value < 0.05, ciò significa che c'è un'evidenza di
# autocorrelazione nei residui del modello

##########################################

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello GARCH(2,2)
Xt <- Xt_t_student_asymmetric_garch_q2_p2
df_Xt_t_student_asymmetric_garch_q2_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch_q2_p2_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch_q2_p2)
summary(Xt_t_student_asymmetric_garch_q2_p2_lm)
summary(Xt_t_student_asymmetric_garch_q2_p2_lm$fitted.values)

Xt_t_student_asymmetric_garch_q2_p2_res <- Xt_t_student_asymmetric_garch_q2_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_garch_q2_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_garch_q2_p2_res)                  # theoretical value 3.
# La skew è pari a -1.959105; il suo valore è negativo e la sua distribuzione è
# asimmetrica verso sinistra con una coda lunga negativa.
# La kurtosi è pari a 8.887091; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch_q2_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,2) of a asymmetric t-student distribution")))
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
Xt_t_student_asymmetric_garch_q2_p2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch_q2_p2_sp)

plot(Xt_t_student_asymmetric_garch_q2_p2_lm,1) # Residuals vs Fitted

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_asymmetric_garch_q2_p2_df <- adf.test(Xt_t_student_asymmetric_garch_q2_p2)
show(Xt_t_student_asymmetric_garch_q2_p2_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch_q2_p2_res)
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
Xt_t_student_asymmetric_garch_q2_p2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch_q2_p2_sp)

plot(Xt_t_student_asymmetric_garch_q2_p2_lm,3) # Scale-location
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch_q2_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q2_p2)
show(Xt_t_student_asymmetric_garch_q2_p2_bp)
# Si ha un p-value di 0.01213 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch_q2_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q2_p2)
show(Xt_t_student_asymmetric_garch_q2_p2_w)
# Si ha un p-value di 0.04304 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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

# Test Ljiung-box
y <- Xt_t_student_asymmetric_garch_q2_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 22.226, df = 1, p-value = 2.423e-06

# Test Durbin-Watson
dwtest(Xt_t_student_asymmetric_garch_q2_p2_lm, alternative="two.sided")
# DW = 1.5766, p-value = 1.679e-06

# Test Breusch-Godfrey
bgtest(Xt_t_student_asymmetric_garch_q2_p2_lm, order=5, type="Chisq")
# LM test = 31.244, df = 5, p-value = 8.382e-06

# I risultati dei test hanno un p-value < 0.05, ciò significa che è possibile rigettare l'ipotesi nulla
# di assenza di autocorrelazione.

#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################

########## Stima dei parametri
" Stimiamo tramite la libreria 'rugarch' i parametri che si adattano meglio ai modelli Arch(q) e Garch(q,p)
  effettuando i test statistici confrontandoli con quelli precedenti.
"
##########################################

"Costruisco un processo ARCH e GARCH per ogni distribuzione considerando q=1 e p=1 (solo per il modello GARCH)"

q <- 1
p <- 1 # utilizzato solo per Garch

############################################## " MODELLO ARCH "

type_model = substitute(paste0("Model ARCH(", q, ")"))

########## DISTRIBUZIONE NORMALE
# Troviamo il miglior parametro a1 che si adatta bene ai dati
# Prima traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,0)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']] 
sigmasquaredW <- var(dist_normal)
print(a1*sigmasquaredW)
Xt_normal_arch_q1_new <- model_arch(a0, a1, X0, dist_normal, q)
Xt_normal_arch_q1_new

# Seconda traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,0)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]  
sigmasquaredW <- var(dist_normal1)
print(a1*sigmasquaredW)
Xt_normal_arch1_q1_new <- model_arch(a0, a1, X0, dist_normal1, q)
Xt_normal_arch1_q1_new

# Terza traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,0)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal2)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']] 
sigmasquaredW <- var(dist_normal2)
print(a1*sigmasquaredW)
Xt_normal_arch2_q1_new <- model_arch(a0, a1, X0, dist_normal2, q)
Xt_normal_arch2_q1_new

type_dist = "Normal Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione normale
plot_dist_normal <- ggplot(data.frame(value = Xt_normal_arch_q1_new, index = seq_along(Xt_normal_arch_q1_new)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione normale
plot_dist_normal1 <- ggplot(data.frame(value = Xt_normal_arch1_q1_new, index = seq_along(Xt_normal_arch1_q1_new)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione normale
plot_dist_normal2 <- ggplot(data.frame(value = Xt_normal_arch2_q1_new, index = seq_along(Xt_normal_arch2_q1_new)), aes(x = index, y = value)) +
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
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,0)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']] 
sigmasquaredW <- var(dist_t_student_symmetric)
print(a1*sigmasquaredW)
Xt_t_student_symmetric_arch_q1_new <- model_arch(a0, a1, X0, dist_t_student_symmetric, q)
Xt_t_student_symmetric_arch_q1_new

# Seconda traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,0)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']] 
sigmasquaredW <- var(dist_t_student_symmetric1)
print(a1*sigmasquaredW)
Xt_t_student_symmetric_arch1_q1_new <- model_arch(a0, a1, X0, dist_t_student_symmetric1, q)
Xt_t_student_symmetric_arch1_q1_new

# Terza traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,0)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric2)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']] 
sigmasquaredW <- var(dist_t_student_symmetric2)
print(a1*sigmasquaredW)
Xt_t_student_symmetric_arch2_q1_new <- model_arch(a0, a1, X0, dist_t_student_symmetric2, q)
Xt_t_student_symmetric_arch2_q1_new

type_dist = "Symmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric <- ggplot(data.frame(value = Xt_t_student_symmetric_arch_q1_new, index = seq_along(Xt_t_student_symmetric_arch_q1_new)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric1 <- ggplot(data.frame(value = Xt_t_student_symmetric_arch1_q1_new, index = seq_along(Xt_t_student_symmetric_arch1_q1_new)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric2 <- ggplot(data.frame(value = Xt_t_student_symmetric_arch2_q1_new, index = seq_along(Xt_t_student_symmetric_arch2_q1_new)), aes(x = index, y = value)) +
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
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,0)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
sigmasquaredW <- var(dist_t_student_asymmetric)
print(a1*sigmasquaredW)
Xt_t_student_asymmetric_arch_q1_new <- model_arch(a0, a1, X0, dist_t_student_asymmetric, q)
Xt_t_student_asymmetric_arch_q1_new

# Seconda traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,0)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
sigmasquaredW <- var(dist_t_student_asymmetric1)
print(a1*sigmasquaredW)
Xt_t_student_asymmetric_arch1_q1_new <- model_arch(a0, a1, X0, dist_t_student_asymmetric1, q)
Xt_t_student_asymmetric_arch1_q1_new

# Terza traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,0)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric2)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
sigmasquaredW <- var(dist_t_student_asymmetric2)
print(a1*sigmasquaredW)
Xt_t_student_asymmetric_arch2_q1_new <- model_arch(a0, a1, X0, dist_t_student_asymmetric2, q)
Xt_t_student_asymmetric_arch2_q1_new

type_dist = "Asymmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric <- ggplot(data.frame(value = Xt_t_student_asymmetric_arch_q1_new, index = seq_along(Xt_t_student_asymmetric_arch_q1_new)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric1 <- ggplot(data.frame(value = Xt_t_student_asymmetric_arch1_q1_new, index = seq_along(Xt_t_student_asymmetric_arch1_q1_new)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric2 <- ggplot(data.frame(value = Xt_t_student_asymmetric_arch2_q1_new, index = seq_along(Xt_t_student_asymmetric_arch2_q1_new)), aes(x = index, y = value)) +
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
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_normal)
print(a1*sigmasquaredW + b1)
Xt_normal_garch_q1_p1_new <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_normal, q, p)
Xt_normal_garch_q1_p1_new

# Seconda traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_normal1)
print(a1*sigmasquaredW + b1)
Xt_normal_garch1_q1_p1_new <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_normal1, q, p)
Xt_normal_garch1_q1_p1_new

# Terza traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal2)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_normal)
print(a1*sigmasquaredW + b1)
Xt_normal_garch2_q1_p1_new <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_normal2, q, p)
Xt_normal_garch2_q1_p1_new

type_dist = "Normal Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)), " of a ", .(type_dist))))

# creo il primo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal <- ggplot(data.frame(value = Xt_normal_garch_q1_p1_new, index = seq_along(Xt_normal_garch_q1_p1_new)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal1 <- ggplot(data.frame(value = Xt_normal_garch1_q1_p1_new, index = seq_along(Xt_normal_garch_q1_p1_new)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal2 <- ggplot(data.frame(value = Xt_normal_garch2_q1_p1_new, index = seq_along(Xt_normal_garch_q1_p1_new)), aes(x = index, y = value)) +
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
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_t_student_symmetric)
print(a1*sigmasquaredW + b1)
Xt_t_student_symmetric_garch_q1_p1_new <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_symmetric, q, p)
Xt_t_student_symmetric_garch_q1_p1_new

# Seconda traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_t_student_symmetric1)
print(a1*sigmasquaredW + b1)
Xt_t_student_symmetric_garch1_q1_p1_new <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_symmetric1, q, p)
Xt_t_student_symmetric_garch1_q1_p1_new

# Terza traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric2)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_t_student_symmetric2)
print(a1*sigmasquaredW + b1)
Xt_t_student_symmetric_garch2_q1_p1_new <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_symmetric2, q, p)
Xt_t_student_symmetric_garch2_q1_p1_new

type_dist = "Symmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric <- ggplot(data.frame(value = Xt_t_student_symmetric_garch_q1_p1_new, index = seq_along(Xt_t_student_symmetric_garch_q1_p1_new)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric1 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch1_q1_p1_new, index = seq_along(Xt_t_student_symmetric_garch1_q1_p1_new)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric2 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch2_q1_p1_new, index = seq_along(Xt_t_student_symmetric_garch2_q1_p1_new)), aes(x = index, y = value)) +
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
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_t_student_asymmetric)
print(a1*sigmasquaredW + b1)
Xt_t_student_asymmetric_garch_q1_p1_new <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_asymmetric, q, p)
Xt_t_student_asymmetric_garch_q1_p1_new

# Seconda traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_t_student_asymmetric1)
print(a1*sigmasquaredW + b1)
Xt_t_student_asymmetric_garch1_q1_p1_new <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)
Xt_t_student_asymmetric_garch1_q1_p1_new

# Terza traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric2)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_t_student_asymmetric2)
print(a1*sigmasquaredW + b1)
Xt_t_student_asymmetric_garch2_q1_p1_new <- model_garch(a0, a1, b1, X0, sigmasquared0, dist_t_student_asymmetric2, q, p)
Xt_t_student_asymmetric_garch2_q1_p1_new

type_dist = "Asymmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch_q1_p1_new, index = seq_along(Xt_t_student_asymmetric_garch_q1_p1_new)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric1 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch1_q1_p1_new, index = seq_along(Xt_t_student_asymmetric_garch1_q1_p1_new)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric2 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch2_q1_p1_new, index = seq_along(Xt_t_student_asymmetric_garch2_q1_p1_new)), aes(x = index, y = value)) +
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
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
bp <- c(coef(fit)[['beta1']],coef(fit)[['beta2']])
sigmasquaredW <- var(dist_normal)
print(a1*sigmasquaredW + bp[1] + bp[2])
Xt_normal_garch_q1_p2_new <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_normal, q, p)
Xt_normal_garch_q1_p2_new

# Seconda traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
bp <- c(coef(fit)[['beta1']],coef(fit)[['beta2']])
sigmasquaredW <- var(dist_normal1)
print(a1*sigmasquaredW + bp[1] + bp[2])
Xt_normal_garch1_q1_p2_new <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_normal1, q, p)
Xt_normal_garch1_q1_p2_new

# Terza traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal2)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
bp <- c(coef(fit)[['beta1']],coef(fit)[['beta2']])
sigmasquaredW <- var(dist_normal2)
print(a1*sigmasquaredW + bp[1] + bp[2])
Xt_normal_garch2_q1_p2_new <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_normal2, q, p)
Xt_normal_garch2_q1_p2_new

type_dist = "Normal Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)), " of a ", .(type_dist))))

# creo il primo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal <- ggplot(data.frame(value = Xt_normal_garch_q1_p2_new, index = seq_along(Xt_normal_garch_q1_p2_new)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal1 <- ggplot(data.frame(value = Xt_normal_garch1_q1_p2_new, index = seq_along(Xt_normal_garch1_q1_p2_new)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal2 <- ggplot(data.frame(value = Xt_normal_garch2_q1_p2_new, index = seq_along(Xt_normal_garch2_q1_p2_new)), aes(x = index, y = value)) +
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
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
bp <- c(coef(fit)[['beta1']],coef(fit)[['beta2']])
sigmasquaredW <- var(dist_t_student_symmetric)
print(a1*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_symmetric_garch_q1_p2_new <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_symmetric, q, p)
Xt_t_student_symmetric_garch_q1_p2_new

# Seconda traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
bp <- c(coef(fit)[['beta1']],coef(fit)[['beta2']])
sigmasquaredW <- var(dist_t_student_symmetric1)
print(a1*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_symmetric_garch1_q1_p2_new <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_symmetric1, q, p)
Xt_t_student_symmetric_garch1_q1_p2_new

# Terza traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric2)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
bp <- c(coef(fit)[['beta1']],coef(fit)[['beta2']])
sigmasquaredW <- var(dist_t_student_symmetric2)
print(a1*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_symmetric_garch2_q1_p2_new <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_symmetric2, q, p)
Xt_t_student_symmetric_garch2_q1_p2_new

type_dist = "Symmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric <- ggplot(data.frame(value = Xt_t_student_symmetric_garch_q1_p2_new, index = seq_along(Xt_t_student_symmetric_garch_q1_p2_new)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric1 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch1_q1_p2_new, index = seq_along(Xt_t_student_symmetric_garch1_q1_p2_new)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric2 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch2_q1_p2_new, index = seq_along(Xt_t_student_symmetric_garch2_q1_p2_new)), aes(x = index, y = value)) +
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
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
bp <- c(coef(fit)[['beta1']],coef(fit)[['beta2']])
sigmasquaredW <- var(dist_t_student_asymmetric)
print(a1*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_asymmetric_garch_q1_p2_new <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_asymmetric, q, p)
Xt_t_student_asymmetric_garch_q1_p2_new

# Seconda traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
bp <- c(coef(fit)[['beta1']],coef(fit)[['beta2']])
sigmasquaredW <- var(dist_t_student_asymmetric1)
print(a1*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_asymmetric_garch1_q1_p2_new <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)
Xt_t_student_asymmetric_garch1_q1_p2_new

# Terza traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric2)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
a1 <- coef(fit)[['alpha1']]
bp <- c(coef(fit)[['beta1']],coef(fit)[['beta2']])
sigmasquaredW <- var(dist_t_student_asymmetric2)
print(a1*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_asymmetric_garch2_q1_p2_new <- model_garch(a0, a1, bp, X0, sigmasquared0, dist_t_student_asymmetric2, q, p)
Xt_t_student_asymmetric_garch2_q1_p2_new

type_dist = "Asymmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch_q1_p2_new, index = seq_along(Xt_t_student_asymmetric_garch_q1_p2_new)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric1 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch1_q1_p2_new, index = seq_along(Xt_t_student_asymmetric_garch1_q1_p2_new)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric2 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch2_q1_p2_new, index = seq_along(Xt_t_student_asymmetric_garch2_q1_p2_new)), aes(x = index, y = value)) +
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
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,0)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
sigmasquaredW <- var(dist_normal)
print((aq[1]+aq[2])*sigmasquaredW)
Xt_normal_arch_q2_new <- model_arch(a0, aq, X0, dist_normal, q)
Xt_normal_arch_q2_new

# Seconda traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,0)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
sigmasquaredW <- var(dist_normal1)
print((aq[1]+aq[2])*sigmasquaredW)
Xt_normal_arch1_q2_new <- model_arch(a0, aq, X0, dist_normal1, q)
Xt_normal_arch1_q2_new

# Terza traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,0)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal2)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
sigmasquaredW <- var(dist_normal2)
print((aq[1]+aq[2])*sigmasquaredW)
Xt_normal_arch2_q2_new <- model_arch(a0, aq, X0, dist_normal2, q)
Xt_normal_arch2_q2_new

type_dist = "Normal Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione normale
plot_dist_normal <- ggplot(data.frame(value = Xt_normal_arch_q2_new, index = seq_along(Xt_normal_arch_q2_new)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione normale
plot_dist_normal1 <- ggplot(data.frame(value = Xt_normal_arch1_q2_new, index = seq_along(Xt_normal_arch_q2_new)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione normale
plot_dist_normal2 <- ggplot(data.frame(value = Xt_normal_arch2_q2_new, index = seq_along(Xt_normal_arch_q2_new)), aes(x = index, y = value)) +
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
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,0)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
sigmasquaredW <- var(dist_t_student_symmetric)
print((aq[1]+aq[2])*sigmasquaredW)
Xt_t_student_symmetric_arch_q2_new <- model_arch(a0, aq, X0, dist_t_student_symmetric, q)
Xt_t_student_symmetric_arch_q2_new

# Seconda traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,0)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
sigmasquaredW <- var(dist_t_student_symmetric1)
print((aq[1]+aq[2])*sigmasquaredW)
Xt_t_student_symmetric_arch1_q2_new <- model_arch(a0, aq, X0, dist_t_student_symmetric1, q)
Xt_t_student_symmetric_arch1_q2_new

# Terza traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,0)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric2)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
sigmasquaredW <- var(dist_t_student_symmetric2)
print((aq[1]+aq[2])*sigmasquaredW)
Xt_t_student_symmetric_arch2_q2_new <- model_arch(a0, aq, X0, dist_t_student_symmetric2, q)
Xt_t_student_symmetric_arch2_q2_new

type_dist = "Symmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric <- ggplot(data.frame(value = Xt_t_student_symmetric_arch_q2_new, index = seq_along(Xt_t_student_symmetric_arch_q2_new)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric1 <- ggplot(data.frame(value = Xt_t_student_symmetric_arch1_q2_new, index = seq_along(Xt_t_student_symmetric_arch1_q2_new)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric2 <- ggplot(data.frame(value = Xt_t_student_symmetric_arch2_q2_new, index = seq_along(Xt_t_student_symmetric_arch2_q2_new)), aes(x = index, y = value)) +
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
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,0)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
sigmasquaredW <- var(dist_t_student_asymmetric)
print((aq[1]+aq[2])*sigmasquaredW)
Xt_t_student_asymmetric_arch_q2_new <- model_arch(a0, aq, X0, dist_t_student_asymmetric, q)
Xt_t_student_asymmetric_arch_q2_new

# Seconda traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,0)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
sigmasquaredW <- var(dist_t_student_asymmetric1)
print((aq[1]+aq[2])*sigmasquaredW)
Xt_t_student_asymmetric_arch1_q2_new <- model_arch(a0, aq, X0, dist_t_student_asymmetric1, q)
Xt_t_student_asymmetric_arch1_q2_new

# Terza traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,0)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric2)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
sigmasquaredW <- var(dist_t_student_asymmetric2)
print((aq[1]+aq[2])*sigmasquaredW)
Xt_t_student_asymmetric_arch2_q2_new <- model_arch(a0, aq, X0, dist_t_student_asymmetric2, q)
Xt_t_student_asymmetric_arch2_q2_new

type_dist = "Asymmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric <- ggplot(data.frame(value = Xt_t_student_asymmetric_arch_q2_new, index = seq_along(Xt_t_student_asymmetric_arch_q2_new)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric1 <- ggplot(data.frame(value = Xt_t_student_asymmetric_arch1_q2_new, index = seq_along(Xt_t_student_asymmetric_arch1_q2_new)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric2 <- ggplot(data.frame(value = Xt_t_student_asymmetric_arch2_q2_new, index = seq_along(Xt_t_student_asymmetric_arch2_q2_new)), aes(x = index, y = value)) +
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
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_normal)
print((aq[1]+aq[2])*sigmasquaredW + b1)
Xt_normal_garch_q2_p1_new <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_normal, q, p)
Xt_normal_garch_q2_p1_new

# Seconda traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_normal1)
print((aq[1]+aq[2])*sigmasquaredW + b1)
Xt_normal_garch1_q2_p1_new <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_normal1, q, p)
Xt_normal_garch1_q2_p1_new

# Terza traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal2)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_normal2)
print((aq[1]+aq[2])*sigmasquaredW + b1)
Xt_normal_garch2_q2_p1_new <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_normal2, q, p)
Xt_normal_garch2_q2_p1_new

type_dist = "Normal Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)), " of a ", .(type_dist))))

# creo il primo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal <- ggplot(data.frame(value = Xt_normal_garch_q2_p1_new, index = seq_along(Xt_normal_garch_q2_p1_new)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal1 <- ggplot(data.frame(value = Xt_normal_garch1_q2_p1_new, index = seq_along(Xt_normal_garch_q2_p1_new)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal2 <- ggplot(data.frame(value = Xt_normal_garch2_q2_p1_new, index = seq_along(Xt_normal_garch_q2_p1_new)), aes(x = index, y = value)) +
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
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_t_student_symmetric)
print((aq[1]+aq[2])*sigmasquaredW + b1)
Xt_t_student_symmetric_garch_q2_p1_new <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_symmetric, q, p)
Xt_t_student_symmetric_garch_q2_p1_new

# Seconda traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_t_student_symmetric1)
print((aq[1]+aq[2])*sigmasquaredW + b1)
Xt_t_student_symmetric_garch1_q2_p1_new <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_symmetric1, q, p)
Xt_t_student_symmetric_garch1_q2_p1_new

# Terza traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric2)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_t_student_symmetric2)
print((aq[1]+aq[2])*sigmasquaredW + b1)
Xt_t_student_symmetric_garch2_q2_p1_new <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_symmetric2, q, p)
Xt_t_student_symmetric_garch2_q2_p1_new

type_dist = "Symmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric <- ggplot(data.frame(value = Xt_t_student_symmetric_garch_q2_p1_new, index = seq_along(Xt_t_student_symmetric_garch_q2_p1_new)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric1 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch1_q2_p1_new, index = seq_along(Xt_t_student_symmetric_garch1_q2_p1_new)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric2 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch2_q2_p1_new, index = seq_along(Xt_t_student_symmetric_garch2_q2_p1_new)), aes(x = index, y = value)) +
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
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_t_student_asymmetric)
print((aq[1]+aq[2])*sigmasquaredW + b1)
Xt_t_student_asymmetric_garch_q2_p1_new <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_asymmetric, q, p)
Xt_t_student_asymmetric_garch_q2_p1_new

# Seconda traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_t_student_asymmetric1)
print((aq[1]+aq[2])*sigmasquaredW + b1)
Xt_t_student_asymmetric_garch1_q2_p1_new <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)
Xt_t_student_asymmetric_garch1_q2_p1_new

# Terza traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,1)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
b1 <- coef(fit)[['beta1']]
sigmasquaredW <- var(dist_t_student_asymmetric2)
print((aq[1]+aq[2])*sigmasquaredW + b1)
Xt_t_student_asymmetric_garch2_q2_p1_new <- model_garch(a0, aq, b1, X0, sigmasquared0, dist_t_student_asymmetric2, q, p)
Xt_t_student_asymmetric_garch2_q2_p1_new

type_dist = "Asymmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch_q2_p1_new, index = seq_along(Xt_t_student_asymmetric_garch_q2_p1_new)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric1 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch1_q2_p1_new, index = seq_along(Xt_t_student_asymmetric_garch1_q2_p1_new)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric2 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch2_q2_p1_new, index = seq_along(Xt_t_student_asymmetric_garch2_q2_p1_new)), aes(x = index, y = value)) +
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
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
bp <- c(coef(fit)[['beta1']], coef(fit)[['beta2']])
sigmasquaredW <- var(dist_normal)
print((aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])
Xt_normal_garch_q2_p2_new <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_normal, q, p)
Xt_normal_garch_q2_p2_new

# Seconda traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
bp <- c(coef(fit)[['beta1']], coef(fit)[['beta2']])
sigmasquaredW <- var(dist_normal1)
print((aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])
Xt_normal_garch1_q2_p2_new <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_normal1, q, p)
Xt_normal_garch1_q2_p2_new

# Terza traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)), distribution.model= "norm")
fit = ugarchfit(spec = uspec, data = dist_normal2)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
bp <- c(coef(fit)[['beta1']], coef(fit)[['beta2']])
sigmasquaredW <- var(dist_normal2)
print((aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])
Xt_normal_garch2_q2_p2_new <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_normal2, q, p)
Xt_normal_garch2_q2_p2_new

type_dist = "Normal Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)), " of a ", .(type_dist))))

# creo il primo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal <- ggplot(data.frame(value = Xt_normal_garch_q2_p2_new, index = seq_along(Xt_normal_garch_q2_p2_new)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal1 <- ggplot(data.frame(value = Xt_normal_garch1_q2_p2_new, index = seq_along(Xt_normal_garch1_q2_p2_new)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Garch(1,1) con la distribuzione normale
plot_dist_normal2 <- ggplot(data.frame(value = Xt_normal_garch2_q2_p2_new, index = seq_along(Xt_normal_garch2_q2_p2_new)), aes(x = index, y = value)) +
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
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
bp <- c(coef(fit)[['beta1']], coef(fit)[['beta2']])
sigmasquaredW <- var(dist_t_student_symmetric)
print((aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_symmetric_garch_q2_p2_new <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_symmetric, q, p)
Xt_t_student_symmetric_garch_q2_p2_new

# Seconda traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
bp <- c(coef(fit)[['beta1']], coef(fit)[['beta2']])
sigmasquaredW <- var(dist_t_student_symmetric1)
print((aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_symmetric_garch1_q2_p2_new <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_symmetric1, q, p)
Xt_t_student_symmetric_garch1_q2_p2_new

# Terza traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)), distribution.model= "std")
fit = ugarchfit(spec = uspec, data = dist_t_student_symmetric2)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
bp <- c(coef(fit)[['beta1']], coef(fit)[['beta2']])
sigmasquaredW <- var(dist_t_student_symmetric2)
print((aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_symmetric_garch2_q2_p2_new <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_symmetric2, q, p)
Xt_t_student_symmetric_garch2_q2_p2_new

type_dist = "Symmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric <- ggplot(data.frame(value = Xt_t_student_symmetric_garch_q2_p2_new, index = seq_along(Xt_t_student_symmetric_garch_q2_p2_new)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric1 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch1_q2_p2_new, index = seq_along(Xt_t_student_symmetric_garch1_q2_p2_new)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student simmetrica
plot_t_student_symmetric2 <- ggplot(data.frame(value = Xt_t_student_symmetric_garch2_q2_p2_new, index = seq_along(Xt_t_student_symmetric_garch2_q2_p2_new)), aes(x = index, y = value)) +
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
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
bp <- c(coef(fit)[['beta1']], coef(fit)[['beta2']])
sigmasquaredW <- var(dist_t_student_asymmetric)
print((aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_asymmetric_garch_q2_p2_new <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_asymmetric, q, p)
Xt_t_student_asymmetric_garch_q2_p2_new

# Seconda traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric1)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
bp <- c(coef(fit)[['beta1']], coef(fit)[['beta2']])
sigmasquaredW <- var(dist_t_student_asymmetric1)
print((aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_asymmetric_garch1_q2_p2_new <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_asymmetric1, q, p)
Xt_t_student_asymmetric_garch1_q2_p2_new

# Terza traiettoria
uspec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(2,2)), distribution.model= "sstd")
fit = ugarchfit(spec = uspec, data = dist_t_student_asymmetric2)
print(fit)
intconf = confint(fit)
show(intconf)

a0 <- coef(fit)[['omega']] 
aq <- c(coef(fit)[['alpha1']], coef(fit)[['alpha2']])
bp <- c(coef(fit)[['beta1']], coef(fit)[['beta2']])
sigmasquaredW <- var(dist_t_student_asymmetric2)
print((aq[1]+aq[2])*sigmasquaredW + bp[1] + bp[2])
Xt_t_student_asymmetric_garch2_q2_p2_new <- model_garch(a0, aq, bp, X0, sigmasquared0, dist_t_student_asymmetric2, q, p)
Xt_t_student_asymmetric_garch2_q2_p2_new

type_dist = "Asymmetric t-student Distribution"
title_content <- bquote(atop(.(content), paste("Plots of the ", .(eval(type_model)) , " of a ", .(type_dist))))

# creo il primo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch_q2_p2_new, index = seq_along(Xt_t_student_asymmetric_garch_q2_p2_new)), aes(x = index, y = value)) + 
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il secondo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric1 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch1_q2_p2_new, index = seq_along(Xt_t_student_asymmetric_garch1_q2_p2_new)), aes(x = index, y = value)) +
  geom_line() +
  xlab("") + ylab("") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 1))

# creo il terzo plot del modello Arch(1) con la distribuzione t-student asimmetrica
plot_t_student_asymmetric2 <- ggplot(data.frame(value = Xt_t_student_asymmetric_garch2_q2_p2_new, index = seq_along(Xt_t_student_asymmetric_garch2_q2_p2_new)), aes(x = index, y = value)) +
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

I test computazionali che vengono solitamente applicati per rilevare l'eteroschedasticità nelle serie 
temporali sono i test di Breusch-Pagan (BP) e White (W). 
Si ha: 
- ipotesi nulla: l'omoschedasticità è presente (i residui sono distributii con una uguale varianza);
- ipotesi alternativa: l'eteroschedasticità è presente ( i residui non sono distribuiti con una varianze uguali).

Se il p-value del test è minore di 0.05 allora possiamo rigettare l'ipotesi nulla e 
concludere che l'eteroschedasticità è presente.

I test computazionali che vengono solitamente applicati per rilevare assenza di autocorrelazione 
nelle serie temporali sono i test di Ljiung-box, Durbin-Watson e Breusch-Godfrey. 
Si ha: 
- ipotesi nulla: assenza di autocorrelazione (i residui del modello sono indipendentemente distribuiti);
- ipotesi alternativa: presenza di autocorrelazione (i residui del modello non sono indipendentemente distribuiti).

Se il p-value del test è minore di 0.05 allora possiamo rigettare l'ipotesi nulla e 
concludere che che ci sia presenza di autocorrelazione nei residui del modello.

"
##########################################
# Consideriamo una traiettoia con distribuzione normale di un modello ARCH(1)
Xt <- Xt_normal_arch_q1_new
df_Xt_normal_arch_q1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_normal_arch_q1_lm <- lm(Xt~t, data=df_Xt_normal_arch_q1)
summary(Xt_normal_arch_q1_lm)
summary(Xt_normal_arch_q1_lm$fitted.values)

Xt_normal_arch_q1_res <- Xt_normal_arch_q1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_arch_q1_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_arch_q1_res)                  # theoretical value 3.
# La skew è pari a -0.04877826; questo indica che la sua distribuzione dei dati nei resdui
# è molto vicina alla simmetria con una coda negativa.
# La kurtosi è pari a 2.668457; questo indica che la distribuzione è leggermenete  platykurtic, 
# cioè ha code più sottili rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_arch_q1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(1) of a normal distribution")))
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
Xt_normal_arch_q1_sp <- ggplot(Data_df) +
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
plot(Xt_normal_arch_q1_sp)

plot(Xt_normal_arch_q1_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari e omoschedastici.

# Stazionarietà
# Test Dickey-Fuller 
Xt_normal_arch_q1_df <- adf.test(Xt)
show(Xt_normal_arch_q1_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_arch_q1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Arch(1) of a normal distribution")))
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
Xt_normal_arch_q1_sp <- ggplot(Data_df) +
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
plot(Xt_normal_arch_q1_sp)

plot(Xt_normal_arch_q1_lm,3) # Scale-location

# Omoschedasticità
# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_arch_q1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_arch_q1)
show(Xt_normal_arch_q1_bp)
# Si ha un p-value di 0.01437 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_arch_q1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_arch_q1)
show(Xt_normal_arch_q1_w)
# Si ha un p-value di 0.04755 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# una banda ristretta.

# Test Ljiung-box
y <- Xt_normal_arch_q1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 2.9285, df = 1, p-value = 0.08703
# Si ha un p-value > 0.05, quindi non è possibile rifiutare l'ipotesi nulla di assenza di autocorrelazione.

# Test Durbin-Watson
dwtest(Xt_normal_arch_q1_lm, alternative="two.sided")
# DW = 1.8454, p-value = 0.07556
# Si ha un p-value > 0.05, quindi non è possibile rifiutare l'ipotesi nulla di assenza di autocorrelazione.

# Test Breusch-Godfrey
bgtest(Xt_normal_arch_q1_lm, order=5, type="Chisq")
# LM test = 3.2063, df = 5, p-value = 0.6682
# Si ha un p-value > 0.05, quindi non è possibile rifiutare l'ipotesi nulla di assenza di autocorrelazione.

# I risultati indicano che c'è un'assenza di autocorrelazione nei residui.

##########################################

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello ARCH(1)
Xt <- Xt_t_student_symmetric_arch_q1_new
df_Xt_t_student_symmetric_arch_q1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_symmetric_arch_q1_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_arch_q1)
summary(Xt_t_student_symmetric_arch_q1_lm)
summary(Xt_t_student_symmetric_arch_q1_lm$fitted.values)

Xt_t_student_symmetric_arch_q1_res <- Xt_t_student_symmetric_arch_q1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_arch_q1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_arch_q1_res)                  # theoretical value 3.
# La skew è pari a 0.1547071; il suo valore è vicino allo zero possiamo dire che 
# la sua distribuzione è simmetrica.
# La kurtosi è pari a 4.568766; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_arch_q1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(1) of a symmetric t-student distribution")))
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
Xt_t_student_symmetric_arch_q1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_arch_q1_sp)

plot(Xt_t_student_symmetric_arch_q1_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_symmetric_arch_q1_df <- adf.test(Xt)
show(Xt_t_student_symmetric_arch_q1_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_arch_q1_res)
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
Xt_t_student_symmetric_arch_q1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_arch_q1_sp)

plot(Xt_t_student_symmetric_arch_q1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_arch_q1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_arch_q1)
show(Xt_t_student_symmetric_arch_q1_bp)
# Si ha un p-value di 0.002928 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_arch_q1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_arch_q1)
show(Xt_t_student_symmetric_arch_q1_w)
# Si ha un p-value di 0.01187 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 0.20422, df = 1, p-value = 0.6513

# Test Durbin-Watson
dwtest(Xt_t_student_symmetric_arch_q1_lm, alternative="two.sided")
# DW = 1.9593, p-value = 0.6161

# Test Breusch-Godfrey
bgtest(Xt_t_student_symmetric_arch_q1_lm, order=5, type="Chisq")
# LM test = 1.5326, df = 5, p-value = 0.9093

# Per tutti e tre i test si ha un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza autocorrelazione.

##########################################

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello ARCH(1)
Xt <- Xt_t_student_asymmetric_arch_q1_new
df_Xt_t_student_asymmetric_arch_q1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_arch_q1_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_arch_q1)
summary(Xt_t_student_asymmetric_arch_q1_lm)
summary(Xt_t_student_asymmetric_arch_q1_lm$fitted.values)

Xt_t_student_asymmetric_arch_q1_res <- Xt_t_student_asymmetric_arch_q1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_arch_q1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_arch_q1_res)                  # theoretical value 3.
# La skew è pari a -1.710902; questo indica una forte asimmetria verso sinistra
# con una coda lunga negativa.
# La kurtosi è pari a 7.444902; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_arch_q1_res)
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

plot(Xt_t_student_asymmetric_arch_q1_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_asymmetric_arch_q1_df <- adf.test(Xt)
show(Xt_t_student_asymmetric_arch_q1_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

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

plot(Xt_t_student_asymmetric_arch_q1_lm,3) # Scale-location

#Determiniamo se la serie è eteroschedastico:
# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_arch_q1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_arch_q1)
show(Xt_t_student_asymmetric_arch_q1_bp)
# Si ha un p-value di 0.02001 > 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_arch_q1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_arch_q1)
show(Xt_t_student_asymmetric_arch_q1_w)
# Si ha un p-value di 0.06186 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Nel test di Breusch-Pagan è possibile rigettare l’ipotesi nulla di omoschedasticità in favore dell’ipotesi alternativa di ETEROSCHEDASTICITA’; 
# mentre, nel test di White non possiamo rigettare l’ipotesi nulla di OMOSCHEDASTICITA’.

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
# una banda ristretta

# Test Ljiung-box
y <- Xt_t_student_asymmetric_arch_q1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 2.8229, df = 1, p-value = 0.09293

# Test Durbin-Watson
dwtest(Xt_t_student_asymmetric_arch_q1_lm, alternative="two.sided")
# DW = 1.8468, p-value = 0.07825

# Test Breusch-Godfrey
bgtest(Xt_t_student_asymmetric_arch_q1_lm, order=5, type="Chisq")
# LM test = 3.2831, df = 5, p-value = 0.6564

# Per tutti e tre i test si ha un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

##########################################
##########################################

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(1,1)
Xt <- Xt_normal_garch1_q1_p1_new
df_Xt_normal_garch_q1_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_normal_garch_q1_p1_lm <- lm(Xt~t, data=df_Xt_normal_garch_q1_p1)
summary(Xt_normal_garch_q1_p1_lm)
summary(Xt_normal_garch_q1_p1_lm$fitted.values)

Xt_normal_garch_q1_p1_res <- Xt_normal_garch_q1_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_garch_q1_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_garch_q1_p1_res)                  # theoretical
# La skew è pari a 0.1730432; il suo valore è prossimo a zero, quindi la sua distribuzione 
# viene considerata approssimativamente simmetrica.
# La kurtosi è pari a 3.735618; questo indica che la distribuzione è leggermenete leptocurtica

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch_q1_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) of a normal distribution")))
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
Xt_normal_garch_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch_q1_p1_sp)

plot(Xt_normal_garch_q1_p1_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Stazionarietà
# Test Dickey-Fuller 
Xt_normal_garch_q1_p1_df <- adf.test(Xt)
show(Xt_normal_garch_q1_p1_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch_q1_p1_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(1,1) of a normal distribution")))
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
Xt_normal_garch_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch_q1_p1_sp)

plot(Xt_normal_garch_q1_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch_q1_p1)
show(Xt_normal_garch_q1_p1_bp)
# Si ha un p-value di 2.015e-12 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch_q1_p1)
show(Xt_normal_garch_q1_p1_w)
# Si ha un p-value di 1.722e-11 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# una banda ristretta

# Test Ljiung-box
y <- Xt_normal_garch_q1_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 0.18709, df = 1, p-value = 0.6654

# Test Durbin-Watson
dwtest(Xt_normal_garch_q1_p1_lm, alternative="two.sided")
# DW = 2.0367, p-value = 0.7144

# Test Breusch-Godfrey
bgtest(Xt_normal_garch_q1_p1_lm, order=5, type="Chisq")
# LM test = 2.6647, df = 5, p-value = 0.7515

# Per tutti e tre i test si ha un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza di autocorrelazione.

##########################################

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello GARCH(1,1)
Xt <- Xt_t_student_symmetric_garch2_q1_p1_new
df_Xt_t_student_symmetric_garch_q1_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch_q1_p1_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch_q1_p1)
summary(Xt_t_student_symmetric_garch_q1_p1_lm)
summary(Xt_t_student_symmetric_garch_q1_p1_lm$fitted.values)

Xt_t_student_symmetric_garch_q1_p1_res <- Xt_t_student_symmetric_garch_q1_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_garch_q1_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_garch_q1_p1_res)                  # theoretical value 3.
# La skew è pari a 0.05638369; la sua distribuzione è approssimativamente simmetrica.
# La kurtosi è pari a 3.996305; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch_q1_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) of a normal distribution")))
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
Xt_t_student_symmetric_garch_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch_q1_p1_sp)

plot(Xt_t_student_symmetric_garch_q1_p1_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_symmetric_garch_q1_p1_df <- adf.test(Xt)
show(Xt_t_student_symmetric_garch_q1_p1_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch_q1_p1_res)
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
Xt_t_student_symmetric_garch_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch_q1_p1_sp)

plot(Xt_t_student_symmetric_garch_q1_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q1_p1)
show(Xt_t_student_symmetric_garch_q1_p1_bp)
# Si ha un p-value di 0.1289 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q1_p1)
show(Xt_t_student_symmetric_garch_q1_p1_w)
# Si ha un p-value di 0.006824 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa

# Il test di Breusch-Pagan indica l'assenza di eteroschedasticità nei residui del modello, 
# mentre il test di White suggerisce la presenza di eteroschedasticità.

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
# una banda ristretta

# Test Ljiung-box
y <- Xt_t_student_symmetric_garch_q1_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 2.4481, df = 1, p-value = 0.1177

# Test Durbin-Watson
dwtest(Xt_t_student_symmetric_garch_q1_p1_lm, alternative="two.sided")
# DW = 2.1382, p-value = 0.1329

# Test Breusch-Godfrey
bgtest(Xt_t_student_symmetric_garch_q1_p1_lm, order=5, type="Chisq")
# LM test = 5.8816, df = 5, p-value = 0.3179

# Per tutti e tre i test si ha un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di no autocorrelazione.

##########################################

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello GARCH(1,1)
Xt <- Xt_t_student_asymmetric_garch1_q1_p1_new
df_Xt_t_student_asymmetric_garch_q1_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch_q1_p1_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch_q1_p1)
summary(Xt_t_student_asymmetric_garch_q1_p1_lm)
summary(Xt_t_student_asymmetric_garch_q1_p1_lm$fitted.values)

Xt_t_student_asymmetric_garch_q1_p1_res <- Xt_t_student_asymmetric_garch_q1_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_garch_q1_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_garch_q1_p1_res)                  # theoretical value 3.
# La skew è pari a -1.510915; il suo valore è negativo e la sua distribuzione è
# asimmetrica verso sinistra con una coda lunga negativa.
# La kurtosi è pari a 6.257641; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch_q1_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,1) of a asymmetric t-student distribution")))
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
Xt_t_student_asymmetric_garch_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch_q1_p1_sp)

plot(Xt_t_student_asymmetric_garch_q1_p1_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_asymmetric_garch_q1_p1_df <- adf.test(Xt)
show(Xt_t_student_asymmetric_garch_q1_p1_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch_q1_p1_res)
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
Xt_t_student_asymmetric_garch_q1_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch_q1_p1_sp)

plot(Xt_t_student_asymmetric_garch_q1_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch_q1_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q1_p1)
show(Xt_t_student_asymmetric_garch_q1_p1_bp)
# Si ha un p-value di 0.01295 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch_q1_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q1_p1)
show(Xt_t_student_asymmetric_garch_q1_p1_w)
# Si ha un p-value di 0.02366 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# una banda ristretta.

# Test Ljiung-box
y <- Xt_t_student_asymmetric_garch_q1_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 2.5459, df = 1, p-value = 0.1106

# Test Durbin-Watson
dwtest(Xt_t_student_asymmetric_garch_q1_p1_lm, alternative="two.sided")
# DW = 1.8527, p-value = 0.09001

# Test Breusch-Godfrey
bgtest(Xt_t_student_asymmetric_garch_q1_p1_lm, order=5, type="Chisq")
# LM test = 5.7675, df = 5, p-value = 0.3295

# I risultati dei tre test hanno un p-value < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla di no autocorrelazione.

##########################################
##########################################

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(1,2)
Xt <- Xt_normal_garch_q1_p2_new
df_Xt_normal_garch_q1_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_normal_garch_q1_p2_lm <- lm(Xt~t, data=df_Xt_normal_garch_q1_p2)
summary(Xt_normal_garch_q1_p2_lm)
summary(Xt_normal_garch_q1_p2_lm$fitted.values)

Xt_normal_garch_q1_p2_res <- Xt_normal_garch_q1_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_garch_q1_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_garch_q1_p2_res)                  # theoretical value 3.
# La skew è pari a -0.04768591; il suo valore è prossimo a zero, quindi la sua distribuzione 
# viene considerata simmetrica con una coda negativa.
# La kurtosi è pari a 3.961628; questo indica che la distribuzione è leggermenete platykurtic.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch_q1_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,2) of a normal distribution")))
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
Xt_normal_garch_q1_p2_sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch_q1_p2_sp)

plot(Xt_normal_garch_q1_p2_lm,1) # Residuals vs Fitted

# Stazionarietà
# Test Dickey-Fuller 
Xt_normal_garch_q1_p2_df <- adf.test(Xt)
show(Xt_normal_garch_q1_p2_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch_q1_p2_res)
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
Xt_normal_garch_q1_p2_sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch_q1_p2_sp)

plot(Xt_normal_garch_q1_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch_q1_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch_q1_p2)
show(Xt_normal_garch_q1_p2_bp)
# Si ha un p-value di 2.2e-16 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch_q1_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch_q1_p2)
show(Xt_normal_garch_q1_p2_w)
# Si ha un p-value di 2.2e-16 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# una banda ristretta, eccetto nel lag 12

# Test Ljiung-box
y <- Xt_normal_garch_q1_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 1.4881, df = 1, p-value = 0.2225

# Test Durbin-Watson
dwtest(Xt_normal_garch_q1_p2_lm, alternative="two.sided")
# DW = 1.8879, p-value = 0.1935

# Test Breusch-Godfrey
bgtest(Xt_normal_garch_q1_p2_lm, order=5, type="Chisq")
# LM test = 2.8348, df = 5, p-value = 0.7254LM test = 3.9654, df = 5, p-value = 0.5544

# I risultati indicano che c'è una assenza di autocorrelazione, poichè il p-value > 0.05.

##########################################

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello GARCH(1,2)
Xt <- Xt_t_student_symmetric_garch_q1_p2_new
df_Xt_t_student_symmetric_garch_q1_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch_q1_p2_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch_q1_p2)
summary(Xt_t_student_symmetric_garch_q1_p2_lm)
summary(Xt_t_student_symmetric_garch_q1_p2_lm$fitted.values)

Xt_t_student_symmetric_garch_q1_p2_res <- Xt_t_student_symmetric_garch_q1_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_garch_q1_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_garch_q1_p2_res)                  # theoretical value 3.
# La skew è pari a 0.3928971; il suo valore è positivo e vicino allo zero; quindi, la sua distribuzione
# è vicina alla simmetria.
# La kurtosi è pari a 8.048424; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch_q1_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,2) of a symmetric t-student distribution")))
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
Xt_t_student_symmetric_garch_q1_p2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch_q1_p2_sp)

plot(Xt_t_student_symmetric_garch_q1_p2_lm,1) # Residuals vs Fitted

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_symmetric_garch_q1_p2_df <- adf.test(Xt)
show(Xt_t_student_symmetric_garch_q1_p2_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch_q1_p2_res)
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
Xt_t_student_symmetric_garch_q1_p2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch_q1_p2_sp)

plot(Xt_t_student_symmetric_garch_q1_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch_q1_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q1_p2)
show(Xt_t_student_symmetric_garch_q1_p2_bp)
# Si ha un p-value di 2.2e-16 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch_q1_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q1_p2)
show(Xt_t_student_symmetric_garch_q1_p2_w)
# Si ha un p-value di 2.2e-16 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 0.91573, df = 1, p-value = 0.3386

# Test Durbin-Watson
dwtest(Xt_t_student_symmetric_garch_q1_p2_lm, alternative="two.sided")
# DW = 1.9131, p-value = 0.3085

# Test Breusch-Godfrey
bgtest(Xt_t_student_symmetric_garch_q1_p2_lm, order=5, type="Chisq")
# LM test = 1.9301, df = 5, p-value = 0.8587

# Per tutti e tre i test si ha un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di no autocorrelazione.

##########################################

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello GARCH(1,1)
Xt <- Xt_t_student_asymmetric_garch_q1_p2_new
df_Xt_t_student_asymmetric_garch_q1_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch_q1_p2_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch_q1_p2)
summary(Xt_t_student_asymmetric_garch_q1_p2_lm)
summary(Xt_t_student_asymmetric_garch_q1_p2_lm$fitted.values)

Xt_t_student_asymmetric_garch_q1_p2_res <- Xt_t_student_asymmetric_garch_q1_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_garch_q1_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_garch_q1_p2_res)                  # theoretical value 3.
# La skew è pari a -1.81205; il suo valore è negativo e la sua distribuzione è
# asimmetrica verso sinistra con una coda lunga negativa.
# La kurtosi è pari a 8.243205; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch_q1_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(1,2) of a asymmetric t-student distribution")))
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
Xt_t_student_asymmetric_garch_q1_p2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch_q1_p2_sp)

plot(Xt_t_student_asymmetric_garch_q1_p2_lm,1) # Residuals vs Fitted

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_asymmetric_garch_q1_p2_df <- adf.test(Xt)
show(Xt_t_student_asymmetric_garch_q1_p2_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch_q1_p2_res)
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
Xt_t_student_asymmetric_garch_q1_p2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch_q1_p2_sp)

plot(Xt_t_student_asymmetric_garch_q1_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch_q1_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q1_p2)
show(Xt_t_student_asymmetric_garch_q1_p2_bp)
# Si ha un p-value di 0.000104 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch_q1_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q1_p2)
show(Xt_t_student_asymmetric_garch_q1_p2_w)
# Si ha un p-value di 0.0005358 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# una banda ristretta.

# Test Ljiung-box
y <- Xt_t_student_asymmetric_garch_q1_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 4.0259, df = 1, p-value = 0.04481

# Test Durbin-Watson
dwtest(Xt_t_student_asymmetric_garch_q1_p2_lm, alternative="two.sided")
# DW = 1.8178, p-value = 0.03695

# Test Breusch-Godfrey
bgtest(Xt_t_student_asymmetric_garch_q1_p2_lm, order=5, type="Chisq")
# LM test = 4.5353, df = 5, p-value = 0.4752

# Per i test di Ljung-Box e Durbin-Watson si ha un p-value < 0.05, ciò significa che ci sono prove
# di autocorrelazione nei residui del modello; mentre, nel test di Breusch-Godfrey si ha un 
# p-value > 0.05, ciò significa che non c'è una correlazione seriale nella serie.

##########################################
##########################################

# Consideriamo una traiettoia con distribuzione normale di un modello ARCH(2)
Xt <- Xt_normal_arch_q2_new
df_Xt_normal_arch_q2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_normal_arch_q2_lm <- lm(Xt~t, data=df_Xt_normal_arch_q2)
summary(Xt_normal_arch_q2_lm)
summary(Xt_normal_arch_q2_lm$fitted.values)

Xt_normal_arch_q2_res <- Xt_normal_arch_q2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_arch_q2_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_arch_q2_res)                  # theoretical value 3.
# La skew è pari a -0.05283273; il suo valore è prossimo a zero, quindi la sua distribuzione potrebbe essere
# considerata approssimativamente simmetrica.
# La kurtosi è pari a 2.685138; questo indica che la distribuzione è leggermenete platykurtic.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_arch_q2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(2) of a normal distribution")))
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
Xt_normal_arch_q2_sp <- ggplot(Data_df) +
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
plot(Xt_normal_arch_q2_sp)

plot(Xt_normal_arch_q2_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari e omoschedastici.

# Stazionarietà
# Test Dickey-Fuller 
Xt_normal_arch_q2_df <- adf.test(Xt)
show(Xt_normal_arch_q2_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_arch_q2_res)
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
Xt_normal_arch_q2_sp <- ggplot(Data_df) +
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
plot(Xt_normal_arch_q2_sp)

plot(Xt_normal_arch_q2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_arch_q2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_arch_q2)
show(Xt_normal_arch_q2_bp)
# Si ha un p-value di 0.01246 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_arch_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_arch_q2)
show(Xt_normal_arch_q2_w)
# Si ha un p-value di 0.04176 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano entro 
# una banda ristretta eccetto in due lag.

# Test Ljiung-box
y <- Xt_normal_arch_q2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 3.0555, df = 1, p-value = 0.08046

# Test Durbin-Watson
dwtest(Xt_normal_arch_q2_lm, alternative="two.sided")
# DW = 1.8422, p-value = 0.06978

# Test Breusch-Godfrey
bgtest(Xt_normal_arch_q2_lm, order=5, type="Chisq")
# LM test = 3.3176, df = 5, p-value = 0.6511 

# I risultati indicano che c'è una assenza di autocorrelazione nei residui del modello 
# poichè il p-value > 0.05 e non possiamo rigettare l'ipotesi nulla.

##########################################

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello ARCH(2)
Xt <- Xt_t_student_symmetric_arch1_q2_new
df_Xt_t_student_symmetric_arch_q2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_symmetric_arch_q2_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_arch_q2)
summary(Xt_t_student_symmetric_arch_q2_lm)
summary(Xt_t_student_symmetric_arch_q2_lm$fitted.values)

Xt_t_student_symmetric_arch_q2_res <- Xt_t_student_symmetric_arch_q2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_arch_q2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_arch_q2_res)                  # theoretical value 3.
# La skew è pari a -0.0182491; il suo valore è prossimo a zero, quindi la sua distribuzione potrebbe essere
# considerata approssimativamente simmetrica.
# La kurtosi è pari a 5.294106; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_arch_q2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(2) of a symmetric t-student distribution")))
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
Xt_t_student_symmetric_arch_q2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_arch_q2_sp)

plot(Xt_t_student_symmetric_arch_q2_lm,1) # Residuals vs Fitted

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_symmetric_arch_q2_df <- adf.test(Xt)
show(Xt_t_student_symmetric_arch_q2_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_arch_q2_res)
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
Xt_t_student_symmetric_arch_q2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_arch_q2_sp)

plot(Xt_t_student_symmetric_arch_q2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_arch_q2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_arch_q2)
show(Xt_t_student_symmetric_arch_q2_bp)
# Si ha un p-value di 0.08707 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_arch_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_arch_q2)
show(Xt_t_student_symmetric_arch_q2_w)
# Si ha un p-value di 0.1964 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
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
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano entro 
# una banda ristretta, eccetto per il lag 15.

# Test Ljiung-box
y <- Xt_t_student_symmetric_arch_q2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 0.29774, df = 1, p-value = 0.5853

# Test Durbin-Watson
dwtest(Xt_t_student_symmetric_arch_q2_lm, alternative="two.sided")
# DW = 2.0484, p-value = 0.6188

# Test Breusch-Godfrey
bgtest(Xt_t_student_symmetric_arch_q2_lm, order=5, type="Chisq")
# LM test = 0.75102, df = 5, p-value = 0.9801

# I risultati dei test hanno un p-value > 0.05; questo, significa che c'è assenza di autocorrelazione.

##########################################

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello ARCH(2)
Xt <- Xt_t_student_asymmetric_arch_q2_new
df_Xt_t_student_asymmetric_arch_q2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_arch_q2_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_arch_q2)
summary(Xt_t_student_asymmetric_arch_q2_lm)
summary(Xt_t_student_asymmetric_arch_q2_lm$fitted.values)

Xt_t_student_asymmetric_arch_q2_res <- Xt_t_student_asymmetric_arch_q2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_arch_q2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_arch_q2_res)                  # theoretical value 3.
# La skew è pari a -1.714677; questo indica una forte asimmetria verso sinistra
# con una coda lunga negativa.
# La kurtosi è pari a 7.430318; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_arch_q2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Arch(2) of a asymmetric t-student distribution")))
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
Xt_t_student_asymmetric_arch_q2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_arch_q2_sp)

plot(Xt_t_student_asymmetric_arch_q2_lm,1) # Residuals vs Fitted

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_asymmetric_arch_q2_df <- adf.test(Xt)
show(Xt_t_student_asymmetric_arch_q2_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_arch_q2_res)
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
Xt_t_student_asymmetric_arch_q2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_arch_q2_sp)

plot(Xt_t_student_asymmetric_arch_q2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_arch_q2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_arch_q2)
show(Xt_t_student_asymmetric_arch_q2_bp)
# Si ha un p-value di  0.01755 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_arch_q2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_arch_q2)
show(Xt_t_student_asymmetric_arch_q2_w)
# Si ha un p-value di 0.05526 > 0.05, quindi, non possiamo rigettare l'ipotesi nulla di 
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
# nel grafico dell'autocorrelogramma possiamo notare che i valori oscillano all'interno dell'intervallo.

# Test Ljiung-box
y <- Xt_t_student_asymmetric_arch_q2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 2.968, df = 1, p-value = 0.08493

# Test Durbin-Watson
dwtest(Xt_t_student_asymmetric_arch_q2_lm, alternative="two.sided")
# DW = 1.843, p-value = 0.07132

# Test Breusch-Godfrey
bgtest(Xt_t_student_asymmetric_arch_q2_lm, order=5, type="Chisq")
# LM test = 3.7472, df = 5, p-value = 0.5864

# Per tutti e tre i test si ha un p-value > 0.05, ciò significa che non possiamo rigettare
# l'ipotesi nulla di assenza autocorrelazione.

##########################################
##########################################

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(2,1)
Xt <- Xt_normal_garch_q2_p1_new
df_Xt_normal_garch_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_normal_garch_q2_p1_lm <- lm(Xt~t, data=df_Xt_normal_garch_q2_p1)
summary(Xt_normal_garch_q2_p1_lm)
summary(Xt_normal_garch_q2_p1_lm$fitted.values)

Xt_normal_garch_q2_p1_res <- Xt_normal_garch_q2_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_garch_q2_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_garch_q2_p1_res)                  # theoretical value 3.
# La skew è pari a -0.03480568; il suo valore è prossimo a zero, quindi la sua distribuzione 
# viene considerata simmetrica con una coda negativa.
# La kurtosi è pari a 3.787901; questo indica che la distribuzione è leggermenete platykurtic.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch_q2_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,1) of a normal distribution")))
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
Xt_normal_garch_q2_p1_sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch_q2_p1_sp)

plot(Xt_normal_garch_q2_p1_lm,1) # Residuals vs Fitted

# Stazionarietà
# Test Dickey-Fuller 
Xt_normal_garch_q2_p1_df <- adf.test(Xt)
show(Xt_normal_garch_q2_p1_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch_q2_p1_res)
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
Xt_normal_garch_q2_p1_sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch_q2_p1_sp)

plot(Xt_normal_garch_q2_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch_q2_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch_q2_p1)
show(Xt_normal_garch_q2_p1_bp)
# Si ha un p-value di  2.2e-16 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch_q2_p1)
show(Xt_normal_garch_q2_p1_w)
# Si ha un p-value di  2.2e-16 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# una banda ristretta.

# Test Ljiung-box
y <- Xt_normal_garch_q2_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 1.6778, df = 1, p-value = 0.1952

# Test Durbin-Watson
dwtest(Xt_normal_garch_q2_p1_lm, alternative="two.sided")
# DW = 1.8813, p-value = 0.1693

# Test Breusch-Godfrey
bgtest(Xt_normal_garch_q2_p1_lm, order=5, type="Chisq")
# LM test = 2.8145, df = 5, p-value = 0.7286

# I risultati hanno un p-value > 0.05, quindi, non possiamo rigettare l'ipotesi
# nulla di assenza di autocorrelazione.

##########################################

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello GARCH(2,1)
Xt <- Xt_t_student_symmetric_garch1_q2_p1_new
df_Xt_t_student_symmetric_garch_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch_q2_p1_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch_q2_p1)
summary(Xt_t_student_symmetric_garch_q2_p1_lm)
summary(Xt_t_student_symmetric_garch_q2_p1_lm$fitted.values)

Xt_t_student_symmetric_garch_q2_p1_res <- Xt_t_student_symmetric_garch_q2_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_garch_q2_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_garch_q2_p1_res)                  # theoretical value 3.
# La skew è pari a -0.254513; il suo valore è vicino allo zero; quindi, la sua distribuzione
# è vicina alla simmetria.
# La kurtosi è pari a 4.981316; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch_q2_p1_res)
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
Xt_t_student_symmetric_garch_q2_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch_q2_p1_sp)

plot(Xt_t_student_symmetric_garch_q2_p1_lm,1) # Residuals vs Fitted

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_symmetric_garch_q2_p1_df <- adf.test(Xt)
show(Xt_t_student_symmetric_garch_q2_p1_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch_q2_p1_res)
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
Xt_t_student_symmetric_garch_q2_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch_q2_p1_sp)

plot(Xt_t_student_symmetric_garch_q2_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch_q2_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q2_p1)
show(Xt_t_student_symmetric_garch_q2_p1_bp)
# Si ha un p-value di 8.935e-07 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q2_p1)
show(Xt_t_student_symmetric_garch_q2_p1_w)
# Si ha un p-value di 1.682e-06 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# una banda ristretta, eccetto per il lag 15

# Test Ljiung-box
y <- Xt_t_student_symmetric_garch_q2_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 0.45221, df = 1, p-value = 0.5013

# Test Durbin-Watson
dwtest(Xt_t_student_symmetric_garch_q2_p1_lm, alternative="two.sided")
# DW = 2.0594, p-value = 0.5347

# Test Breusch-Godfrey
bgtest(Xt_t_student_symmetric_garch_q2_p1_lm, order=5, type="Chisq")
# LM test = 1.7281, df = 5, p-value = 0.8853

# I risultati indicano un p-value > 0.05; ciò indica
# un'assenza di autocorrelazione nei residui del modello.

##########################################

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello GARCH(2,1)
Xt <- Xt_t_student_asymmetric_garch_q2_p1_new
df_Xt_t_student_asymmetric_garch_q2_p1 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch_q2_p1_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch_q2_p1)
summary(Xt_t_student_asymmetric_garch_q2_p1_lm)
summary(Xt_t_student_asymmetric_garch_q2_p1_lm$fitted.values)

Xt_t_student_asymmetric_garch_q2_p1_res <- Xt_t_student_asymmetric_garch_q2_p1_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_garch_q2_p1_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_garch_q2_p1_res)                  # theoretical value 3.
# La skew è pari a -1.812047; il suo valore è negativo e la sua distribuzione è
# asimmetrica verso sinistra con una coda lunga negativa.
# La kurtosi è pari a 8.243179; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch_q2_p1_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,1) of a asymmetric t-student distribution")))
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
Xt_t_student_asymmetric_garch_q2_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch_q2_p1_sp)

plot(Xt_t_student_asymmetric_garch_q2_p1_lm,1) # Residuals vs Fitted
# Dal grafico "Residuals vs Fitted" possiamo notare che i residui del modello sono distribuiti in modo
# omogeneo intorno alla linea rossa LOESS; questo indica che i residui sono stazionari.

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_asymmetric_garch_q2_p1_df <- adf.test(Xt)
show(Xt_t_student_asymmetric_garch_q2_p1_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch_q2_p1_res)
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
Xt_t_student_asymmetric_garch_q2_p1_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch_q2_p1_sp)

plot(Xt_t_student_asymmetric_garch_q2_p1_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch_q2_p1_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q2_p1)
show(Xt_t_student_asymmetric_garch_q2_p1_bp)
# Si ha un p-value di 0.000104 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch_q2_p1_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q2_p1)
show(Xt_t_student_asymmetric_garch_q2_p1_w)
# Si ha un p-value di 0.000536 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
y <- Xt_t_student_asymmetric_garch_q2_p1_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 4.0259, df = 1, p-value = 0.04481

# Test Durbin-Watson
dwtest(Xt_t_student_asymmetric_garch_q2_p1_lm, alternative="two.sided")
# DW = 1.8178, p-value = 0.03695

# Test Breusch-Godfrey
bgtest(Xt_t_student_asymmetric_garch_q2_p1_lm, order=5, type="Chisq")
# LM test = 4.5354, df = 5, p-value = 0.4752

# I risultati dei test di Ljung-Box e Durbin-Wtason hanno un p-value < 0.05; ciò indica
# un'evidenza di autocorrelazione nei residui del modello; mentre, nel test di 
# Breusch-Godfrey si ha un p-value > 0.05, quindi, non si ha una correlazione seriale.

##########################################
##########################################

# Consideriamo una traiettoia con distribuzione normale di un modello GARCH(2,2)
Xt <- Xt_normal_garch_q2_p2_new
df_Xt_normal_garch_q2_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_normal_garch_q2_p2_lm <- lm(Xt~t, data=df_Xt_normal_garch_q2_p2)
summary(Xt_normal_garch_q2_p2_lm)
summary(Xt_normal_garch_q2_p2_lm$fitted.values)

Xt_normal_garch_q2_p2_res <- Xt_normal_garch_q2_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_normal_garch_q2_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_normal_garch_q2_p2_res)                  # theoretical value 3.
# La skew è pari a -0.0476863; il suo valore è prossimo a zero, quindi la sua distribuzione 
# viene considerata simmetrica.
# La kurtosi è pari a 3.961629; questo indica che la distribuzione è leptocurtica

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch_q2_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,2) of a normal distribution")))
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
Xt_normal_garch_q2_p2_sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch_q2_p2_sp)

plot(Xt_normal_garch_q2_p2_lm,1) # Residuals vs Fitted

# Stazionarietà
# Test Dickey-Fuller 
Xt_normal_garch_q2_p2_df <- adf.test(Xt)
show(Xt_normal_garch_q2_p2_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_normal_garch_q2_p2_res)
Data_df$X <- sqrt(abs(Data_df$X))
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Scatter Plot of the Residuals of the model Garch(2,2) of a normal distribution")))
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
Xt_normal_garch_q2_p2_sp <- ggplot(Data_df) +
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
plot(Xt_normal_garch_q2_p2_sp)

plot(Xt_normal_garch_q2_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_normal_garch_q2_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_normal_garch_q2_p2)
show(Xt_normal_garch_q2_p2_bp)
# Si ha un p-value di 2.2e-16 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_normal_garch_q2_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_normal_garch_q2_p2)
show(Xt_normal_garch_q2_p2_w)
# Si ha un p-value di 2.2e-16 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# X-squared = 1.4881, df = 1, p-value = 0.2225

# Test Durbin-Watson
dwtest(Xt_normal_garch_q2_p2_lm, alternative="two.sided")
# DW = 1.8879, p-value = 0.1935

# Test Breusch-Godfrey
bgtest(Xt_normal_garch_q2_p2_lm, order=5, type="Chisq")
# LM test = 2.8348, df = 5, p-value = 0.7254

# I risultati del test hanno un p-value < 0.05 indicando una assenza di autocorrelazione

##########################################

# Consideriamo una traiettoia con distribuzione t-student simmetrica di un modello GARCH(2,2)
Xt <- Xt_t_student_symmetric_garch1_q2_p2_new
df_Xt_t_student_symmetric_garch_q2_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_symmetric_garch_q2_p2_lm <- lm(Xt~t, data=df_Xt_t_student_symmetric_garch_q2_p2)
summary(Xt_t_student_symmetric_garch_q2_p2_lm)
summary(Xt_t_student_symmetric_garch_q2_p2_lm$fitted.values)

Xt_t_student_symmetric_garch_q2_p2_res <- Xt_t_student_symmetric_garch_q2_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_symmetric_garch_q2_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_symmetric_garch_q2_p2_res)                  # theoretical value 3.
# La skew è pari a -0.254695; il suo valore è prossima allo zero, quindi possiamo dire 
# che la sua distribuzione è simmetrica.
# La kurtosi è pari a 5.050407; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch_q2_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,2) of a symmetric t-student distribution")))
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
Xt_t_student_symmetric_garch_q2_p2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch_q2_p2_sp)

plot(Xt_t_student_symmetric_garch_q2_p2_lm,1) # Residuals vs Fitted

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_symmetric_garch_q2_p2_df <- adf.test(Xt)
show(Xt_t_student_symmetric_garch_q2_p2_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_symmetric_garch_q2_p2_res)
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
Xt_t_student_symmetric_garch_q2_p2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_symmetric_garch_q2_p2_sp)

plot(Xt_t_student_symmetric_garch_q2_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_symmetric_garch_q2_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q2_p2)
show(Xt_t_student_symmetric_garch_q2_p2_bp)
# Si ha un p-value di 3.22e-07 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_symmetric_garch_q2_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_symmetric_garch_q2_p2)
show(Xt_t_student_symmetric_garch_q2_p2_w)
# Si ha un p-value di 8.154e-07 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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
# eccetto nel lag 15

# Test Ljiung-box
y <- Xt_t_student_symmetric_garch_q2_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 0.46171, df = 1, p-value = 0.4968

# Test Durbin-Watson
dwtest(Xt_t_student_symmetric_garch_q2_p2_lm, alternative="two.sided")
# DW = 2.06, p-value = 0.5303

# Test Breusch-Godfrey
bgtest(Xt_t_student_symmetric_garch_q2_p2_lm, order=5, type="Chisq")
# LM test = 1.7896, df = 5, p-value = 0.8774

# Per i test di Ljiung-box e Burbin-Wtason si ha un p-value > 0.05, ciò significa che c'è assenza
# autocorrelazione nei residui del modello

##########################################

# Consideriamo una traiettoia con distribuzione t-student asimmetrica di un modello GARCH(2,2)
Xt <- Xt_t_student_asymmetric_garch_q2_p2_new
df_Xt_t_student_asymmetric_garch_q2_p2 <- data.frame(t = 1:length(Xt), X = Xt)

# Consideriamo un modello lineare
Xt_t_student_asymmetric_garch_q2_p2_lm <- lm(Xt~t, data=df_Xt_t_student_asymmetric_garch_q2_p2)
summary(Xt_t_student_asymmetric_garch_q2_p2_lm)
summary(Xt_t_student_asymmetric_garch_q2_p2_lm$fitted.values)

Xt_t_student_asymmetric_garch_q2_p2_res <- Xt_t_student_asymmetric_garch_q2_p2_lm$residuals
# Calcoliamo la skew e la kurtosi
moments::skewness(Xt_t_student_asymmetric_garch_q2_p2_res)                  # theoretical value 0.
moments::kurtosis(Xt_t_student_asymmetric_garch_q2_p2_res)                  # theoretical value 3.
# La skew è pari a -1.812057; il suo valore è negativo e la sua distribuzione è
# asimmetrica verso sinistra con una coda lunga negativa.
# La kurtosi è pari a 8.243387; questo indica che la distribuzione è leptocurtica, 
# cioè ha code più pesanti rispetto ad una normale.

# Scatter plot - Residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch_q2_p2_res)
length <- nrow(Data_df)
First_Day <- paste(Data_df$t[1])
Last_Day <- paste(Data_df$t[length])
title_content <- bquote(atop("University of Roma \"Tor Vergata\" -  - Metodi Probabilistici e Statistici per i Mercati Finanziari", paste("Residuals of the model Garch(2,2) of a asymmetric t-student distribution")))
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
Xt_t_student_asymmetric_garch_q2_p2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch_q2_p2_sp)

plot(Xt_t_student_asymmetric_garch_q2_p2_lm,1) # Residuals vs Fitted

# Stazionarietà
# Test Dickey-Fuller 
Xt_t_student_asymmetric_garch_q2_p2_df <- adf.test(Xt)
show(Xt_t_student_asymmetric_garch_q2_p2_df)
# Si ha un p-value < 0.05, quindi possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Scatter plot - Square root of absolute residuals
Data_df <- data.frame(t = 1:length(Xt), X = Xt_t_student_asymmetric_garch_q2_p2_res)
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
Xt_t_student_asymmetric_garch_q2_p2_sp <- ggplot(Data_df) +
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
plot(Xt_t_student_asymmetric_garch_q2_p2_sp)

plot(Xt_t_student_asymmetric_garch_q2_p2_lm,3) # Scale-location

# Test BREUSCH-PAGAN sui residui del modello lineare
Xt_t_student_asymmetric_garch_q2_p2_bp <- lmtest::bptest(formula = Xt~t, varformula=NULL, studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q2_p2)
show(Xt_t_student_asymmetric_garch_q2_p2_bp)
# Si ha un p-value di 0.0001036 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
# omoschedasticità in favore  dell'ipotesi alternativa di eteroschedasticità

# Test WHITE sui residui del modello lineare
Xt_t_student_asymmetric_garch_q2_p2_w <- lmtest::bptest(formula = Xt~t, varformula = ~ t+I(t^2), studentize = TRUE, data=df_Xt_t_student_asymmetric_garch_q2_p2)
show(Xt_t_student_asymmetric_garch_q2_p2_w)
# Si ha un p-value di 0.000534 < 0.05, quindi, possiamo rigettare l'ipotesi nulla di 
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

# Test Ljiung-box
y <- Xt_t_student_asymmetric_garch_q2_p2_res
Box.test(y, lag = 1, type = "Ljung-Box", fitdf = 0)
# X-squared = 4.0227, df = 1, p-value = 0.04489

# Test Durbin-Watson
dwtest(Xt_t_student_asymmetric_garch_q2_p2_lm, alternative="two.sided")
# DW = 1.8179, p-value = 0.03702

# Test Breusch-Godfrey
bgtest(Xt_t_student_asymmetric_garch_q2_p2_lm, order=5, type="Chisq")
# LM test = 4.5314, df = 5, p-value = 0.4757

# I risultati dei test di Ljung-Box e Durbin-Watson hanno un p-value < 0.05, ciò significa che è possibile rigettare l'ipotesi nulla
# di assenza di autocorrelazione; mentre, nel test di Breusch-Godfrey ha un p-value > 0.05, ciò indica
# assenza di correlazione seriale.

#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################


