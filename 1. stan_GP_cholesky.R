##- Load packages and initial setting
library(rstan); library(ggplot2)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
Sys.setenv(LOCAL_CPPFLAGS = '-march=corei7') # For Windows
set.seed(123)

#############################
### Fitting GP regression ###
#############################

##-- Data preparation
path = "Field_6" # Please change the path
data <- read.table(file(description=paste('./data/raw_csv/', path, ".csv", sep=""),open="r"), header=T, sep=",")
color.gradient <- function(x, colors=c("red","yellow","green"), colsteps=10) {
  return(colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
colnames(data) <- c('id', 'x', 'y', 'Yield', 'treatment')

##-- subset data set (Specify the rates to factorize treatments)
high_rate <- 40
control_rate <- 35
low_rate <- 30
data <- subset(data, treatment == low_rate | treatment == control_rate | treatment == high_rate)
nrow(data)
rownames(data) <- 1:nrow(data)

##-- design matrix
X <- matrix(0,nrow(data),3)
X[,1] <- 1
X[,2] <- replace(X[,2], data$treatment == low_rate, 1)
X[,3] <- replace(X[,3], data$treatment == high_rate, 1)
colnames(X) <- c('control', 'low', 'high')

# visualize yield map
ggplot(data, aes(x = x, y = y, fill = Yield)) +
  geom_point(size = 3, shape = 21, col = 'white') +
  scale_fill_continuous(type = "viridis") +
  theme_minimal()

# visualize treatment map
ggplot(data, aes(x = x, y = y, fill = treatment)) +
  geom_point(size = 3, shape = 21, col = 'white') +
  scale_fill_continuous(type = "viridis") +
  theme_minimal()

data2 <- data
data2$treatment <- as.character(data2$treatment)

# boxplot of yield for each treatment
ggplot(data2, aes(x = treatment, y = Yield)) +
  geom_boxplot()

##-- eval dist
dist = as.matrix(dist(data[,c(2,3)], method = "euclidean"))
dim(dist)

##-- Run gaussina process suing Stan
stan_dat <- list(distances = dist, K = 3, X = X, y = data$Yield, N = length(data$Yield))
mod <- stan_model('gp.stan') 
fit <- sampling(mod, data = stan_dat, chains = 4, iter = 1000, refresh = 50, 
                control = list(adapt_delta = 0.99, max_treedepth = 10))
traceplot(fit, pars = c('beta', 'alpha', 'sigma', 'rho'), inc_warmup = TRUE)
summary(fit)

## Save fit as rds
fit@stanmodel@dso <- new("cxxdso")
saveRDS(fit, file = paste("./data/fit_rds/fit_", path, ".rds", sep=""))


###################################
### Profit probability analysis ###
###################################

##-- Extract saved rds
path = "Field_6"
high_rate <- 40
control_rate <- 35
low_rate <- 30
fit <- readRDS(paste("./data/fit_rds/fit_", path, ".rds", sep=""))
samps <- rstan::extract(fit)

##-- Eval profit probability
 
# Attainable profit (low rate vs control or high rate vs control) JPY/ha
# Note the unit conversion. Check if fertilizer input rate was in kg/10a or kg/ha

p_rice = 59.82/60 # USD/kg, the prices is based on the grain weight after husking and sieving
p_input = 23.40/20 # USD/kg (Info from the farmer 2020). Each farmer and year has a different value. To be confirmed.
grain_fac = 0.8 # Factor for husking and sieving. To be discussed.

prof_low <- p_rice * grain_fac * samps$beta[,2] * 1000 - 10 * p_input * (low_rate - control_rate)
prof_high <- p_rice * grain_fac * samps$beta[,3] * 1000 - 10 * p_input * (high_rate - control_rate)

plot_low = bayestestR::hdi(prof_low , separate_chains = T, ci = c(0.5, 0.95))
plot(plot_low) +scale_fill_brewer(palette = "Greens", na.translate=FALSE)+scale_x_continuous(limits = c(-250, 250))+ scale_y_continuous(limits = c(0, 1))

plot_high = bayestestR::hdi(prof_high, separate_chains = T, ci = c(0.5, 0.95))
plot(plot_high) +scale_fill_brewer(palette = "Greens", na.translate=FALSE)+scale_x_continuous(limits = c(-250, 250))+ scale_y_continuous(limits = c(0, 1))
