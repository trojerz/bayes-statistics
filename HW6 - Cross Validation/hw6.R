# libraries --------------------------------------------------------------------
library(cmdstanr)  # for interfacing Stan
library(ggplot2)   # for visualizations
library(posterior) # for extracting samples
library(bayesplot) # for some quick MCMC visualizations
library(mcmcse)    # for comparing samples and calculating MCSE
library(tidyverse) # for data manipulations
library(rstan)
library(fastDummies)
library(psych) 
library(cowplot)
library(ggdist)
library(loo) # for WAIC and LOOIC calculations
library(HDInterval)

normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

hapiness_kaggle <- read_csv("hapiness_kaggle.csv")
hapiness_kaggle <- dummy_cols(hapiness_kaggle, select_columns = "Region")
happiness <- read_csv("happiness.csv")

set.seed(16)

analysis_table <- hapiness_kaggle[c("Economy", "Health", "Freedom", "Generosity", "Trust", "Score")]

my_plots <- lapply(names(analysis_table), function(var_x){
  p <- ggplot(analysis_table) + aes_string(var_x)
  if(is.numeric(analysis_table[[var_x]])) {
    p <- p + geom_density()  + theme(text = element_text(size = 25))
    
  } else {
    p <- p + geom_bar() + theme(text = element_text(size = 25))
  } 
})


plot_grid(plotlist = my_plots)

analysis_table <- normalize(analysis_table)



# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
#  FIRST MODEL -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------

first_model <- cmdstan_model("linear_deviance.stan")

first_model_data <- hapiness_kaggle[c("Economy", "Trust", "Score")]
#first_model_data <- normalize(first_model_data)


X <- first_model_data[,-which(names(first_model_data) %in% c("Score"))]

y <- first_model_data$Score
n <- nrow(first_model_data)
m <- ncol(X)


pairs.panels(X, 
             method = "pearson", # correlation method
             hist.col = "skyblue",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

log_lik <- list()
df_aic <- data.frame(AIC=numeric(), model=factor())

# set order
stan_data_1 <- list(n = n, 
                    m = m,
                    X = X,
                    y = y)

# fit
fit <- first_model$sample(
  data = stan_data_1,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 500,
  seed = 1
)

# uncomment lines below for diagnostic purposes
# traceplot
mcmc_trace(fit$draws(c("b", "sigma")))
# summary
fit$summary(c("b", "sigma"))


log_lik[[1]] <- fit$draws(c("log_lik"))
df_ll <- as_draws_df(fit$draws(c("log_lik")))


df_ll <- data.frame(df_ll %>% select(-.chain, -.iteration, -.draw))

# average per row and store
df_aic <- rbind(df_aic, data.frame(AIC=-2*rowSums(df_ll) + 2*(m+1), Order=as.factor(1)))


# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
#  SECOND MODEL -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------

second_model <- cmdstan_model("linear_deviance_interaction_one.stan")

second_model_data <- hapiness_kaggle[c("Economy", "Trust", "Score")]
#second_model_data <- normalize(second_model_data)

X <- second_model_data[,-which(names(second_model_data) %in% c("Score"))]

y <- second_model_data$Score
n <- nrow(second_model_data)
m <- ncol(X)

# set order
stan_data_2 <- list(n = n,
                    X = X, 
                    y = y,
                    m = m)

# fit
fit <- second_model$sample(
  data = stan_data_2,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 500,
  seed = 1
)

# uncomment lines below for diagnostic purposes
# traceplot
mcmc_trace(fit$draws(c("b", "sigma")))
# summary
fit$summary(c("b", "sigma"))


# extract
log_lik[[2]] <- fit$draws(c("log_lik"))
df_ll <- as_draws_df(fit$draws(c("log_lik")))

# remove unwanted columns
# also cast to regular data frame to avoid some warnings later on
df_ll <- data.frame(df_ll %>% select(-.chain, -.iteration, -.draw))

# average per row and store
df_aic <- rbind(df_aic,
                data.frame(AIC=-2*rowSums(df_ll) + 2*(m+1), Order=as.factor(2)))

# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
#  THIRD MODEL -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------

third_model <- cmdstan_model("linear_deviance.stan")

third_model_data <- hapiness_kaggle[c("Economy", "Health", "Freedom", "Generosity", "Trust", "Score")]
#third_model_data <- normalize(third_model_data)

X <- third_model_data[,-which(names(third_model_data) %in% c("Score"))]

y <- third_model_data$Score
n <- nrow(third_model_data)
m <- ncol(X)


pairs.panels(X, 
             method = "pearson", # correlation method
             hist.col = "skyblue",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


# set order
stan_data_3 <- list(n = n, 
                    m = m,
                    X = X, 
                    y = y)

# fit
fit <- third_model$sample(
  data = stan_data_3,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 500,
  seed = 1
)

# uncomment lines below for diagnostic purposes
# traceplot
mcmc_trace(fit$draws(c("b", "sigma")))
# summary
fit$summary(c("b", "sigma"))


log_lik[[3]] <- fit$draws(c("log_lik"))
df_ll <- as_draws_df(fit$draws(c("log_lik")))


df_ll <- data.frame(df_ll %>% select(-.chain, -.iteration, -.draw))

# average per row and store
df_aic <- rbind(df_aic, data.frame(AIC=-2*rowSums(df_ll) + 2*(m+1), Order=as.factor(3)))

# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
#  FOURTH MODEL -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------

fourth_model <- cmdstan_model("linear_deviance_all.stan")

fourth_model_data <- hapiness_kaggle[c("Economy", "Health", "Freedom", "Generosity", "Trust", "Score")]
# <- normalize(fourth_model_data)

X <- fourth_model_data[,-which(names(fourth_model_data) %in% c("Score"))]

y <- fourth_model_data$Score
n <- nrow(fourth_model_data)
m <- ncol(X)


pairs.panels(X, 
             method = "pearson", # correlation method
             hist.col = "skyblue",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


# set order
stan_data_4 <- list(n = n, 
                    m = m,
                    X = X, 
                    y = y)

# fit
fit <- fourth_model$sample(
  data = stan_data_4,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 500,
  seed = 1
)

# uncomment lines below for diagnostic purposes
# traceplot
mcmc_trace(fit$draws(c("b", "sigma")))
# summary
fit$summary(c("b", "sigma"))


log_lik[[4]] <- fit$draws(c("log_lik"))
df_ll <- as_draws_df(fit$draws(c("log_lik")))


df_ll <- data.frame(df_ll %>% select(-.chain, -.iteration, -.draw))

# average per row and store
df_aic <- rbind(df_aic, data.frame(AIC=-2*rowSums(df_ll) + 2*(m+1), Order=as.factor(4)))

# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
#  FIFTH MODEL -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------

fifth_model <- cmdstan_model("linear_deviance.stan")

fifth_model_data <- hapiness_kaggle[c("Economy", 
                                      "Health",
                                      "Freedom", 
                                      "Generosity",
                                      "Trust",
                                      "Score",
                                      "Region_Australia and New Zealand", 
                                      "Region_Central and Eastern Europe", 
                                      "Region_Eastern Asia",
                                      "Region_Latin America and Caribbean", 
                                      "Region_Middle East and Northern Africa",
                                      "Region_North America",
                                      "Region_Southeastern Asia",
                                      "Region_Southern Asia", 
                                      "Region_Sub-Saharan Africa")]

#fifth_model_data <- normalize(fifth_model_data)

X <- fifth_model_data[,-which(names(fifth_model_data) %in% c("Score"))]

y <- fifth_model_data$Score
n <- nrow(fifth_model_data)
m <- ncol(X)


pairs.panels(X, 
             method = "pearson", # correlation method
             hist.col = "skyblue",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


# set order
stan_data_5 <- list(n = n, 
                    m = m,
                    X = X, 
                    y = y)

# fit
fit <- fifth_model$sample(
  data = stan_data_5,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 500,
  seed = 1
)

# uncomment lines below for diagnostic purposes
# traceplot
mcmc_trace(fit$draws(c("b", "sigma")))
# summary
fit$summary(c("b", "sigma"))


log_lik[[5]] <- fit$draws(c("log_lik"))
df_ll <- as_draws_df(fit$draws(c("log_lik")))


df_ll <- data.frame(df_ll %>% select(-.chain, -.iteration, -.draw))

# average per row and store
df_aic <- rbind(df_aic, data.frame(AIC=-2*rowSums(df_ll) + 2*(m+1), Order=as.factor(5)))


# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
#  SIXTH MODEL -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------
# -----------------------------------------------

sixth_model <- cmdstan_model("linear_deviance_all.stan")

sixth_model_data <- hapiness_kaggle[c("Economy", 
                                      "Health",
                                      "Freedom", 
                                      "Generosity",
                                      "Trust",
                                      "Score",
                                      "Region_Australia and New Zealand", 
                                      "Region_Central and Eastern Europe", 
                                      "Region_Eastern Asia",
                                      "Region_Latin America and Caribbean", 
                                      "Region_Middle East and Northern Africa",
                                      "Region_North America",
                                      "Region_Southeastern Asia",
                                      "Region_Southern Asia", 
                                      "Region_Sub-Saharan Africa")]
#sixth_model_data <- normalize(sixth_model_data)

X <- sixth_model_data[,-which(names(sixth_model_data) %in% c("Score"))]

y <- sixth_model_data$Score
n <- nrow(sixth_model_data)
m <- ncol(X)


pairs.panels(X, 
             method = "pearson", # correlation method
             hist.col = "skyblue",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


# set order
stan_data_6 <- list(n = n, 
                    m = m,
                    X = X, 
                    y = y)

# fit
fit <- sixth_model$sample(
  data = stan_data_6,
  parallel_chains = 4,
  iter_warmup = 500,
  iter_sampling = 500,
  seed = 1
)

# uncomment lines below for diagnostic purposes
# traceplot
mcmc_trace(fit$draws(c("b", "sigma")))
# summary
fit$summary(c("b", "sigma"))


log_lik[[6]] <- fit$draws(c("log_lik"))
df_ll <- as_draws_df(fit$draws(c("log_lik")))


df_ll <- data.frame(df_ll %>% select(-.chain, -.iteration, -.draw))

# average per row and store
df_aic <- rbind(df_aic, data.frame(AIC=-2*rowSums(df_ll) + 2*(m+1), Order=as.factor(6)))



     

# COMPARING -----------------------------


df_aic_summary <- df_aic %>% group_by(Order) %>% 
  summarize(mean_AIC=mean(AIC),
            hdi5=hdi(AIC, credMass=0.9)[1],
            hdi95=hdi(AIC, credMass=0.9)[2])

ggplot(data=df_aic_summary, aes(x=c("SN", "SNI", "NF", "NFI", "NFR", "NFRI"), y=mean_AIC)) +
  geom_point(shape=16, size=2) +
  geom_linerange(aes(ymin = hdi5, ymax = hdi95), alpha=0.3) +
  xlab("Model") +
  ylab("AIC")


# WAIC
df_waic <- data.frame(WAIC=numeric(), SE=numeric(), Order=factor())

for (i in 0:5) {
  waic <- waic(log_lik[[i+1]])
  df_waic <- rbind(df_waic, data.frame(waic=waic$estimates[3,1],
                                       SE=waic$estimates[3,2],
                                       Order=as.factor(i)))
}

# plot
ggplot(data=df_waic, aes(x=c("SN", "SNI", "NF", "NFI", "NFR", "NFRI"), y=waic)) +
  geom_point(shape=16, size=2) +
  geom_linerange(aes(ymin = (waic-SE), ymax = (waic+SE)), alpha=0.3) +
  xlab("Model") +
  ylab("WAIC")


# averaging
# calculate delta_waic
df_waic$delta_waic <- abs(df_waic$waic - min(df_waic$waic))

# calculate weights
df_waic$weight <- exp(-0.5 * df_waic$delta_waic) / sum(exp(-0.5 * df_waic$delta_waic))
df_waic$weight <- round(df_waic$weight, 2)

df_waic$model <- c("SN", "SNI", "NF", "NFI", "NFR", "NFRI")
df_waic$model <- factor(df_waic$model, levels= c("SN", "SNI", "NF", "NFI", "NFR", "NFRI"))

# plot
ggplot(data=df_waic, aes(x=model, y=weight)) +
  geom_bar(stat="identity", fill="skyblue") +
  xlab("Model") +
  ylab("Akaike weight") +
  theme_minimal() +
  ylim(0, 1) + theme(text = element_text(size = 25))

# LOOIC
df_loo <- data.frame(loo=numeric(), SE=numeric(), Order=factor())

for (i in 0:5) {
  r_eff <- relative_eff(log_lik[[i+1]])
  loo <- loo(log_lik[[i+1]], r_eff=r_eff)
  df_loo <- rbind(df_loo, data.frame(loo=loo$estimates[3,1],
                                     SE=loo$estimates[3,2],
                                     Order=as.factor(i)))
}

df_loo$model <- c("SN", "SNI", "NF", "NFI", "NFR", "NFRI")
df_loo$model <- factor(df_loo$model, levels= c("SN", "SNI", "NF", "NFI", "NFR", "NFRI"))

# plot
ggplot(data=df_loo, aes(x=model, y=loo)) +
  geom_point(shape=16, size=3) +
  geom_linerange(aes(ymin = (loo-SE), ymax = (loo+SE)), size = 1.5, alpha=0.3) +
  xlab("Model") +
  ylab("LOOIC") + theme(text = element_text(size = 25))
