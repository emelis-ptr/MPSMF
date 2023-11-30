"Progetto di Metodi Probabilistici e Statistici per i Mercati Finanziari"

library(quantmod)
library(zoo)
library(tibble)
library(dplyr)
library(ggplot2)
library(lmtest)
library(numbers)
library(EnvStats)
library(urca)

author_content <- "Author: Melissa Petrolo"
content <- "University of Roma \"Tor Vergata\" - \u0040 Metodi Probabilistici e Statistici per i Mercati Finanziari 2022-2023"

# Scarico i dati S&P500 da Yahoo Finance come oggetto xts e creo un plot chartSeries
getSymbols('^GSPC')
class(GSPC)
head(GSPC)
tail(GSPC)
chartSeries(GSPC, subset="2021-07-01::2023-06-30", type="auto", theme=chartTheme('white'))

# Salviamo i dati GSPC come file csv utilizzando il comando write.zoo
write.zoo(GSPC, file="GSPC.csv", index.name="Date", row.names=FALSE, col.names=TRUE, sep=",")
# Thereafter, we can read the file GSPC_bis.csv as a data.frame object
GSPC_df <- read.csv("GSPC.csv", header=TRUE)
# For sake of simplicity, I rename the columns
GSPC_df <- rename(GSPC_df, Open=GSPC.Open, High=GSPC.High, Low=GSPC.Low, Close=GSPC.Close, Volume=GSPC.Volume, Adj.Close=GSPC.Adjusted)
GSPC_df$Date <- as.Date(GSPC_df$Date)
head(GSPC_df)

# I primi dati risalgono a *Gennaio 2007*, ma si è deciso di eseguire l'analisi a partire da
# Gennaio 2022 fino a Giugno 2023, in modo tale da poter confrontare questa analisi con il periodo
# antecedente da Luglio 2021 a Dicembre 2022. 
# Aggiungiamo inoltre una colonna (**t**) di indice al *dataframe*, che replica
# i numeri di riga ed è utile per scopi di tracciamento.
# time series start = 01/2022
# time series end = 06/2023
df_GSPC_current <- GSPC_df[GSPC_df$Date >= "2022-01-01" & GSPC_df$Date <= "2023-06-30", ]
df_GSPC_current <- df_GSPC_current %>%
  mutate(Index = row_number()) %>%
  select(Index, everything())
head(df_GSPC_current)
Data_df <- df_GSPC_current

# We draw a scatter and a line plot of S&P500 Adjusted Daily Closing Prices from from Jan-01-2022 to Jun-30-2023.
# The scatter plot
length <- nrow(Data_df)
First_Day <- paste(Data_df$Date[1])
Last_Day <- paste(Data_df$Date[length])
title_content <- bquote(atop(.(content),
                             paste("Scatter Plot of the S&P500 Adjusted Daily Closing Prices from ",
                                   .(First_Day), " to ", .(Last_Day), sep="")))
subtitle_content <- bquote(paste("Yahoo Finance ^GSPC,    path length - ", .(length), " sample points"))
caption_content <- author_content
x_name <- bquote("dates")
y_name <- bquote("adj. prices (US $)")
# x_breaks_num <- 10
x_breaks_num <- 21 # (deduced from (primeFactors(length)))
# x_breaks_num <- ceiling(length^(1/2)) # Tukey & Mosteller square-root rule
# x_breaks_num <- ceiling(1+log2(length)) # Sturges rule
# x_breaks_num <- ceiling((2*length)^(1/3)) # Teller & Scott rice rule
# x_binwidth <- round((max(Data_df$X)-min(Data_df$X))/x_breaks_num, digits=1)
# x_breaks_low <- floor((min(Data_df$X)/x_binwidth))*x_binwidth
# x_breaks_up <- ceiling((max(Data_df$X)/x_binwidth))*x_binwidth
# x_breaks <- c(seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth))
# x_labs <- format(x_breaks, scientific=FALSE)
x_binwidth <- round((max(Data_df$Index)-min(Data_df$Index))/x_breaks_num, digits=0)
x_breaks_low <- ceiling((min(Data_df$Index)/x_binwidth))*x_binwidth
x_breaks_up <- floor((max(Data_df$Index)/x_binwidth))*x_binwidth
x_breaks <- c(1,round(seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth),3),length)
x_labs <- Data_df$Date[x_breaks]
J <- 1.0
x_lims <- c((x_breaks_low-J*x_binwidth), (x_breaks_up+0*x_binwidth))
y_breaks_num <- 12 # (deduced from (primeFactors(length)))
y_binwidth <- round((max(Data_df$Adj.Close)-min(Data_df$Adj.Close))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$Adj.Close)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$Adj.Close)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
k <- 1
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
point_b <- bquote("adjusted daily closing prices (US $)")
line_r  <- bquote("LOESS curve")
line_g  <- bquote("regression line")
leg_labs <- c(point_b, line_r, line_g)
leg_cols <- c("point_b"="blue", "line_r"="red", "line_g"="green")
leg_breaks <- c("point_b", "line_r", "line_g")
GSPC_adj_close_sp <- ggplot(Data_df) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=Index, y=Adj.Close, color="line_g"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=FALSE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=Index, y=Adj.Close, color="line_r"),
              method = "loess", formula = y ~ x, se=FALSE) +
  geom_point(alpha=1, size=0.6, shape=19, aes(x=Index, y=Adj.Close, color="point_b")) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,NA,NA), linetype=c("blank", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=90, vjust=1),
        legend.key.width = unit(1.0,"cm"), legend.position="bottom")
plot(GSPC_adj_close_sp)

# The line plot
line_b  <- bquote("adjusted daily closing prices (US $)")
line_r  <- bquote("LOESS curve")
line_g  <- bquote("regression line")
leg_labs <- c(line_b, line_r, line_g)
leg_cols <- c("line_b"="blue", "line_r"="red", "line_g"="green")
leg_breaks <- c("line_b", "line_r", "line_g")
title_content <- bquote(atop(.(content),
                             paste("Line Plot of the S&P500 Adjusted Daily Closing Prices from ",
                                   .(First_Day), " to ", .(Last_Day), sep="")))
GSPC_adj_close_lp <- ggplot(Data_df) +
  geom_smooth(alpha=1, size = 0.8, linetype="solid", aes(x=Index, y=Adj.Close, color="line_g"),
              method = "lm" , formula = y ~ x, se=FALSE, fullrange=FALSE) +
  geom_smooth(alpha=1, size = 0.8, linetype="dashed", aes(x=Index, y=Adj.Close, color="line_r"),
              method = "loess", formula = y ~ x, se=FALSE) +
  geom_line(alpha=1, size=0.6, linetype="solid", aes(x=Index, y=Adj.Close, color="line_b", group=1)) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "dashed", "solid")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=90, vjust=1),
        legend.key.width = unit(1.0,"cm"), legend.position="bottom")
plot(GSPC_adj_close_lp)

# Per effettuare analisi e predizione si è deciso di suddividere il *dataframe* in due parti: training set e testing set.
# La decisione presa è quella di suddividere il dataframe in modo tale da avere nel testing set
# 6 mesi e poter quindi eseguire la predizione su questi, utlizzando come Time Series i valori 
# presenti nel training set per creare il modello.
DS_length <- nrow(Data_df)
TrnS_length <- floor(DS_length*0.93)
TstS_length <- DS_length-TrnS_length
df_string <- sprintf("Data Frame lenght: %d\n", DS_length)
trn_string <- sprintf("Training Set lenght: %d\n", TrnS_length)
tst_string <- sprintf("Testing Set lenght: %d\n", TstS_length)
cat(df_string)
cat(trn_string)
cat(tst_string)

# Si visualizzano i dati tramite lo **Scatter Plot**
First_Day <- paste(Data_df$Date[1])
Last_Day <- paste(Data_df$Date[length])
title_content <- bquote(atop(.(content),
                             paste("Scatter Plot of S&P500 - Training and Test Sets - from ",
                                   .(First_Day), " to ", .(Last_Day), sep="")))
subtitle_content <- bquote(paste("Yahoo Finance ^IXIC,    path length - ", .(length), " sample points. Training set length ", .(TrnS_length), " sample points. Test set length ", .(TstS_length), " sample points."))
caption_content <- author_content
# We set the x-axis name. However, in this case, since dates will mark the x-axis ticks, we think that it is unnecessary to give the x-axis a name.
x_name <- bquote("dates")
# To obtain the sub-multiples of the length of the data set as a hint on the number of breaks to use, we factorize the length of the time series.
# primeFactors(DS_length)
x_breaks_num <- 26
x_binwidth <- round((max(Data_df$Index)-min(Data_df$Index))/x_breaks_num, digits=0)
x_breaks_low <- ceiling((min(Data_df$Index)/x_binwidth))*x_binwidth
x_breaks_up <- floor((max(Data_df$Index)/x_binwidth))*x_binwidth
x_breaks <- c(1,round(seq(from=x_breaks_low, to=x_breaks_up, by=x_binwidth),3),length)
x_labs <- Data_df$Date[x_breaks]
J <- 1
x_lims <- c(x_breaks_low-J*x_binwidth, x_breaks_up+J*x_binwidth)
y_name <- bquote("adj. prices (US $)")
y_breaks_num <- 9
y_binwidth <- round((max(Data_df$Adj.Close)-min(Data_df$Adj.Close))/y_breaks_num, digits=3)
y_breaks_low <- floor((min(Data_df$Adj.Close)/y_binwidth))*y_binwidth
y_breaks_up <- ceiling((max(Data_df$Adj.Close)/y_binwidth))*y_binwidth
y_breaks <- round(seq(from=y_breaks_low, to=y_breaks_up, by=y_binwidth),3)
y_labs <- format(y_breaks, scientific=FALSE)
K <- 0.0
y_lims <- c((y_breaks_low-k*y_binwidth), (y_breaks_up+k*y_binwidth))
col_k <- bquote("adj. daily closing prices (US $) training set")
col_b <- bquote("adj. daily closing prices (US $) validation set")
col_g <- bquote("regression line (training set)")
col_r <- bquote("LOESS curve (training set)")
leg_labs   <- c(col_k, col_b, col_g, col_r)
leg_cols   <- c("col_k"="black", "col_b"="blue", "col_r"="red", "col_g"="green")
leg_breaks <- c("col_k", "col_b", "col_g", "col_r")
GSPC_adj_close_splitted_data_sp <- ggplot(Data_df, aes(x=Index)) +
  geom_vline(xintercept=Data_df$Index[TrnS_length], linewidth=0.3, colour="black") +
  geom_smooth(data=subset(Data_df, Data_df$Index < Data_df$Index[TrnS_length]), alpha=1, linewidth=0.7, linetype="solid", 
              aes(y=Adj.Close, color="col_g"), method="lm", formula=y~x, se=FALSE, fullrange=FALSE) +
  geom_smooth(data=subset(Data_df, Data_df$Index < Data_df$Index[TrnS_length]), alpha=1, linewidth=0.7, linetype="dashed", 
              aes(y=Adj.Close, color="col_r"), method="loess", formula=y~x, se=FALSE) +
  geom_point(data=subset(Data_df, Data_df$Index < Data_df$Index[TrnS_length]), alpha=1, size=0.5, shape=19, 
             aes(y=Adj.Close, color="col_k")) +
  geom_point(data=subset(Data_df, Data_df$Index > Data_df$Index[TrnS_length]), alpha=1, size=0.5, shape=19, 
             aes(y=Adj.Close, color="col_b")) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  guides(linetype="none", shape="none") +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(shape=c(19,19,NA,NA), 
                                                           linetype=c("blank", "blank", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5),
        axis.text.x = element_text(angle=90, vjust=1),
        legend.key.width = unit(1.0,"cm"), legend.position="bottom")
plot(GSPC_adj_close_splitted_data_sp)

# The line plot
title_content <- bquote(atop(.(content),
                             paste("Line Plot of the Training and Validation Set of the S&P500 Adjusted Daily Closing Prices from ",
                                   .(First_Day), " to ", .(Last_Day), sep="")))
GSPC_adj_close_splitted_data_lp <- ggplot(Data_df, aes(x=Index)) +
  geom_vline(xintercept = Data_df$Index[TrnS_length], size=0.3, colour="black") +
  geom_smooth(data=subset(Data_df, Data_df$Index <  Data_df$Index[TrnS_length]), alpha=1, size = 0.8, linetype="solid", 
              aes(y=Adj.Close, color="col_g"), method = "lm" , formula = y ~ x, se=FALSE) +
  geom_smooth(data=subset(Data_df, Data_df$Index <  Data_df$Index[TrnS_length]), alpha=1, size = 0.8, linetype="dashed", 
              aes(y=Adj.Close, color="col_r"), method = "loess", formula = y ~ x, se=FALSE) +
  geom_line(data=subset(Data_df, Data_df$Index < Data_df$Index[TrnS_length]), alpha=1, size=0.5, linetype="solid", 
            aes(y=Adj.Close, color="col_k")) +
  geom_line(data=subset(Data_df, Data_df$Index >= Data_df$Index[TrnS_length]), alpha=1, size=0.5, linetype="solid", 
            aes(y=Adj.Close, color="col_b")) +
  scale_x_continuous(name=x_name, breaks=x_breaks, label=x_labs, limits=x_lims) +
  scale_y_continuous(name=y_name, breaks=y_breaks, labels=NULL, limits=y_lims,
                     sec.axis = sec_axis(~., breaks=y_breaks, labels=y_labs)) +
  ggtitle(title_content) +
  labs(subtitle=subtitle_content, caption=caption_content) +
  scale_colour_manual(name="Legend", labels=leg_labs, values=leg_cols, breaks=leg_breaks,
                      guide=guide_legend(override.aes=list(linetype=c("solid", "solid", "solid", "dashed")))) +
  theme(plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        axis.text.x=element_text(angle=-45, vjust=1, hjust=-0.3),
        legend.key.width=unit(0.8,"cm"), legend.position="bottom")
plot(GSPC_adj_close_splitted_data_lp)


########################################################################################################################################

# Prendiamo in considerazione solo i dati del training set
GSPC_adj_close_train_df <- subset(df_GSPC_current, df_GSPC_current$Index < df_GSPC_current$Index[TrnS_length])
head(GSPC_adj_close_train_df)

# Consideriamo un modello lineare
Data_df <- GSPC_adj_close_train_df
GSPC_adj_close_train_lm <- lm(Adj.Close~Index, data=Data_df)
summary(GSPC_adj_close_train_lm)
Data_lm <-GSPC_adj_close_train_lm

# Determiniamo anche la skewness e la kurtosi dei residui utilizzando EnvStats
y <- Data_lm$residuals
show(EnvStats::skewness(y, method = "moment"))
show(EnvStats::kurtosis(y, method = "moment", excess = TRUE))  # Gaussian value 0
# La skew è pari a -0.2313647; il suo valore è prossimo a zero, quindi la sua distribuzione potrebbe essere
# considerata approssimativamente simmetrica.
# La kurtosi è pari a -0.688044; questo indica che la distribuzione è platykurtic.

"Per verificare la correttezza del modello lineare si analizzano i suoi residui e si studiano:
- **Omoschedasticità**:  significa che i residui sono equamente distribuiti lungo la linea di 
    regressione, ovvero sopra e sotto la linea di regressione e la varianza dei residui dovrebbe 
    essere la stessa per tutti i punteggi previsti lungo la linea di regressione.
- **Assenza di autocorrelazione**: L'autocorrelazione si verifica quando i residui non sono 
    indipendenti l'uno dall'altro.
- **Stazionarietà**: un modello di predizione con residui stazionari garantisce che le previsioni 
    future siano affidabili e non influenzate da fluttuazioni casuali o tendenze temporali.
- **Gaussianità**: se i residui seguono una distribuzione normale."

#### Residuals vs Fitted Values Diagnostic Plot ####
plot(Data_lm,1)
# Il plot mostra un andamento non lineare che è la prova della non stazionarietà media dei residui,
# ciò significa che il modello lineare non è in grado di spiegare il trend.
#### Residual Q-Q Plot ####
plot(Data_lm,2)
# Notiamo una non gaussianità nei residui del modello.
#### Scale-Location Diagnostic Plot ####
plot(Data_lm,3)

# Testiamo la stazionarietà dei residui attraverso il test di Augmented Dickey-Fuller(ADF) 
# e il test di Kwiatowski-Phillips-Schmidt-Shin (KPSS)

# DF TEST
y <- Data_df$Adj.Close
max_lags <- trunc(12*(length(y)/100)^(1/4))+1
# max_lags <- ceiling((length(y)-1)^(1/3))
y_ADF_aTSA_df <- data.frame(matrix(nrow = max_lags, ncol = 9))
y_ADF_aTSA <- aTSA::adf.test(y, nlag=max_lags, output=FALSE)
y_ADF_aTSA_df <- cbind(y_ADF_aTSA$type1, y_ADF_aTSA$type2, y_ADF_aTSA$type3)
colnames(y_ADF_aTSA_df) <- c("lag", "ADF: no drift no trend", "p-value", 
                             "lag", "ADF: drift no trend", "p-value", 
                             "lag", "ADF: drift and trend", "p-value")
show(y_ADF_aTSA_df)
# Notiamo che l'ipotesi nulla non può essere rifiutata contro tutte le alternative "no drift no trend",
# and "drift and "drift and (linear) trend" ad ogni livello di significatività.
# Mentre, su "drift and no trend" in alcuni lag è possibile rigettare l'ipotesi nulla in favore
# dell'alternativa e ciò signica che potrebbe esserci presenza di un trend.

y <- Data_lm[["residuals"]]   # The data set to be tested.
num_lags <- 0                   # Setting the lag parameter for the test.
Data_lm_res_DF <- ur.df(y, type="none", lags=num_lags, selectlags="Fixed")    
summary(Data_lm_res_DF) 
# Nel test DF si ha un p-value 0.006896 < 0.05, ciò indica che è possibile rigettare
# l'ipotesi nulla di non stazionarietà in favore dell'alternativa.

# KPSS TEST
y <- Data_lm[["residuals"]]    # The data set to be tested
Data_lm_res_KPSS <- ur.kpss(y, type="mu", lags="nil", use.lag=NULL)    
summary(Data_lm_res_KPSS)    # Showing the result of the test
# Nel test KPSS il p-value 4.998 è maggiore per ogni livello di significatività, ciò
# significa che non possiamo rigettare l'ipotesi nulla di non stazionarietà.

# Testiamo la presenza di eteroschedasticità nei residui attraverso il test di Breusch-Pagan (BP) 
# e il test di White (W)

# BREUSCH-PAGAN TEST
y_res <- as.vector(y)
t <- 1:length(y_res)
Data_lm_res_BP <- lmtest::bptest(formula=y_res~t, varformula=NULL, studentize=TRUE, data=NULL)
show(Data_lm_res_BP)
# Nel test BP si ha un p-value 0.001717 < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla in favore dell'alternativa di eteroschedasticità

# WHITE TEST
y_res <- as.vector(y)
t <- 1:length(y_res)
Data_lm_res_W <- lmtest::bptest(formula=y_res~t, varformula=y_res~t+I(t^2), studentize=TRUE, data=NULL)
show(Data_lm_res_W)
# Nel test W si ha un p-value 0.004196 < 0.05, ciò significa che possiamo rigettare
# l'ipotesi nulla in favore dell'alternativa di eteroschedasticità

# In sintesi, nei residui del modello si ha presenza di eteroschedasticità.

# Consideriamo l'autocorrelogramma del training set
y <- Data_lm$residuals
length <- length(y)
maxlag <- ceiling(10*log10(length))
Aut_Fun_y <- stats::acf(y, lag.max = maxlag, type="correlation", plot=FALSE)
# Aut_Fun_y <- TSA::acf(y, lag.max = lag_max, type="correlation", plot=TRUE)
ci_90 <- qnorm((1+0.90)/2)/sqrt(length)
ci_95 <- qnorm((1+0.95)/2)/sqrt(length)
ci_99 <- qnorm((1+0.99)/2)/sqrt(length)
Plot_Aut_Fun_y <- data.frame(lag=Aut_Fun_y$lag, acf=Aut_Fun_y$acf)
First_Day <- as.character(Data_df$Date[1])
Last_Day <- as.character(Data_df$Date[nrow(Data_df)])
title_content <- bquote(atop(.(content),
                             paste("Autocorrelogram of the Training Set of the S&P500 Adjusted Daily Closing Prices ",
                                   "from ", .(First_Day), " to ", .(Last_Day), sep="")))
subtitle_content <- bquote(paste("path length ", .(length), " sample points,   ", "lags ", .(maxlag)))
caption_content <- author_content
ggplot(Plot_Aut_Fun_y, aes(x=lag, y=acf)) + 
  geom_segment(aes(x=lag, y=rep(0,length(lag)), xend=lag, yend=acf), size = 1, col="black") +
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
# Dall'autocorrelogramma possiamo notare un trend 