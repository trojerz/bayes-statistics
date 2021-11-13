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

set.seed(16)
# modelling and data prep ------------------------------------------------------
# compile the model
model <- cmdstan_model("multilinear.stan")
modelState <- cmdstan_model("multilinearStates.stan")

 # prepare the data
data <- read.csv("50_startups.csv")


unique(data$state)
#"NewYork"   0 0 1
#"California" 1 0 0 
#"Florida"   0 1 0 

X <- data[,-which(names(data) %in% c("state", "profit"))]
XStates <- dummy_cols(data, select_columns = "state")
#contrasts(data$state) <- contr.treatment(n_distinct(data$states))


XStates <- XStates[,-which(names(data) %in% c("state", "profit", "research", "administration", "marketing"))]


#X <- scale(X)
Y <- data[, "profit"]
n <- length(Y)
m <- ncol(X)
mStates <- ncol(XStates)


# correlation
# plot correlation of independent variables
pairs.panels(X, 
             method = "pearson", # correlation method
             hist.col = "skyblue",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

pairs.panels(XStates, 
             method = "pearson", # correlation method
             hist.col = "skyblue",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

# we see that correlation is sometimes high, but never too high (above 0.90). 
# the highest correlation is between research and marketing - 0.72

# prepare input for Stan
stan_data <- list(n = n, m = m, x = X, y = Y)
stan_dataStates <- list(n=n, m=mStates, x=XStates, y=Y)

# fit
fit <- model$sample(
  data = stan_data,
  iter_warmup =1000,
  iter_sampling = 1000
)

fitStates <- modelState$sample(
  data = stan_dataStates,
  iter_warmup =1000,
  iter_sampling = 1000
)

# diagnostics ------------------------------------------------------------------
# traceplot
mcmc_trace(fit$draws())

mcmc_trace(fitStates$draws())

# summary
fit$summary()

fitStates$summary()



# where to put money
df <- as_draws_df(fit$draws())

df %>% select(-.chain, -.iteration, -.draw)

research <- as_draws_df(df['b[1]'])
administration <- as_draws_df(df['b[2]'])
marketing <- as_draws_df(df['b[3]'])

research <- research %>% select(-.chain, -.iteration, -.draw)
administration <- administration %>% select(-.chain, -.iteration, -.draw)
marketing <- marketing %>% select(-.chain, -.iteration, -.draw)

colnames(research) <- "research"
colnames(administration) <- "administration"
colnames(marketing) <- "marketing"

money_data <- bind_cols(research / (research + administration + marketing),
                        administration / (research + administration + marketing), 
                        marketing / (research + administration + marketing))

money_data <- money_data %>% gather(Beta, Value)

ggplot(data = money_data, aes(x = Value, y = Beta)) +
  stat_eye(fill = "skyblue", alpha = 0.75)


mcse(research > administration & administration > marketing)

# from which country
df_2 <- as_draws_df(fitStates$draws())

df_2 %>% select(-.chain, -.iteration, -.draw)

NY <- as_draws_df(df_2['b[1]'])
CA <- as_draws_df(df_2['b[2]'])
FL <- as_draws_df(df_2['b[3]'])

NY <- NY %>% select(-.chain, -.iteration, -.draw)
CA <- CA %>% select(-.chain, -.iteration, -.draw)
FL <- FL %>% select(-.chain, -.iteration, -.draw)

colnames(NY) <- "NewYork"
colnames(CA) <- "California"
colnames(FL) <- "Florida"

state_data <- bind_cols(NY, CA, FL)

state_data <- state_data %>% gather(Beta, Value)

ggplot(data = state_data, aes(x = Value, y = Beta)) +
  stat_eye(fill = "skyblue", alpha = 0.75)

mcse(CA > FL)

mcse(CA > NY)

mcse(FL > NY)
