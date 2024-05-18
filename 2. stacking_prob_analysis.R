library(rstan)
library(lattice); library(ggplot2); library(viridis); library(hrbrthemes)

prob = 0.9 # probability of gaining positive profits. [0.8, 0.9, 0.95] were used in the paper.

##-  Generate combined posteriors 
path_list = c('Field_1',
              'Field_2',
              'Field_3',
              'Field_4',
              'Field_5',
              'Field_6',
              'Field_7',
              'Field_8',
              'Field_9',
              'Field_10',
              'Field_11'
              )

model_list <- list()
for(i in 1:length(path_list)){
  path <- path_list[i]
  model_list[i] <- readRDS(paste("./data/fit_rds/fit_", path, ".rds", sep="")) # Please change the path here
}

# Combine posterior samples for rho, alpha and sigma using stacking weights
samples <- list()
for (param in c('rho', 'alpha', 'sigma')) {
  samples[[param]] <- matrix(0, nrow = nrow(extract(model_list[[1]])[[param]]), ncol = length(model_list))
  for (i in seq_along(model_list)) {
    samples[[param]][,i] <- extract(model_list[[i]])[[param]]
    # print(paste0('param=', param,'; field=',i, '; median=', median(samples[[param]][,i])))
  }
}

# Combine posterior samples for beta
beta_1 <- matrix(0, nrow = nrow(extract(model_list[[1]])[['beta']]), ncol = length(model_list))
beta_2 <- matrix(0, nrow = nrow(extract(model_list[[1]])[['beta']]), ncol = length(model_list))
beta_3 <- matrix(0, nrow = nrow(extract(model_list[[1]])[['beta']]), ncol = length(model_list))
for (i in seq_along(model_list)) {
  beta_1[,i] <- extract(model_list[[i]])[['beta']][,1]
  beta_2[,i] <- extract(model_list[[i]])[['beta']][,2]
  beta_3[,i] <- extract(model_list[[i]])[['beta']][,3]
}


##-- Eval profit probability based on expand grid.
comb <- expand.grid(Fertilizer_Price = seq(1.0, 2.350, 0.01), Grain_Price = seq(0.883, 1.450, 0.01)) ### Conversion of Yen to Dollar in 26 October/2023_Price is in dollars ($).
grain_fac <- 0.8
  
prob_gain_lower <- c()
prob_gain_higher <- c()
for(i in 1:nrow(comb)){
  fertilizer_p <- comb[i, 1]
  grain_p <- comb[i, 2]
  
  # estimate probability for each field
  prob_l_n <- 0
  prob_h_n <- 0
  for(j in 1:length(model_list)){ 
    prob_l <- sum((grain_p *  grain_fac * beta_2[,j] * 1000 + fertilizer_p * 50)>0)/length(beta_2[,j])
    prob_h <- sum((grain_p *  grain_fac * beta_3[,j] * 1000 - fertilizer_p * 50)>0)/length(beta_3[,j])
    
    if(prob_l>prob){
      result <- 1
    } else {
      result <- 0
    }
    prob_l_n <- prob_l_n + result
    
    if(prob_h>prob){
      result <- 1
    } else {
      result <- 0
    }
    prob_h_n <- prob_h_n + result
  }
  
  prob_gain_lower <- c(prob_gain_lower, prob_l_n)
  prob_gain_higher <- c(prob_gain_higher, prob_h_n)
}

prob_gain_lower <- cbind(comb, prob_gain_lower)
colnames(prob_gain_lower) <- c("FertP", "GrainP", "Prob")
prob_gain_higher <- cbind(comb, prob_gain_higher)
colnames(prob_gain_higher) <- c("FertP", "GrainP", "Prob")

# Level plot

actual <- data.frame(year = c(2020,2021,2022,2023), 
                     Fertilizer_Price = c(24/20, 23.44/20, 27.24/20, 39.40/20), ### Conversion of Yen to Dollar in 26 October/2023_Price is in dollars ($)
                     Grain_Price = c(80.56/60, 59.92/60, 64.58/60, 71.24/60))  ### Price is in dollars ($)

v <- ggplot(prob_gain_lower, aes(x = FertP, y = GrainP)) +
  geom_raster(aes(fill = Prob), interpolate = TRUE) +
  scale_fill_viridis(discrete=FALSE, breaks = seq(0, 11, by = 1), limits = c(0, 11)) +
  theme_ipsum() +
  geom_point(data = actual, mapping = aes(x = Fertilizer_Price, y = Grain_Price), color='white') +
  geom_text(data = actual, mapping = aes(x = Fertilizer_Price, y = Grain_Price, label = as.character(year)),
            hjust=-0.3, vjust=-0.3, color='white') +
  labs(fill = "Field number",
       x = expression(paste('Fertilizer price ($ kg'^{-1}, ")")),
       y = expression(paste('Grain price ($ kg'^{-1}, ")"))
  ) +
  theme(text = element_text(family = "Times New Roman"))  # Font family
v

v <- ggplot(prob_gain_higher, aes(x = FertP, y = GrainP)) +
  geom_raster(aes(fill = Prob), interpolate = TRUE) +
  scale_fill_viridis(discrete=FALSE, breaks = seq(0, 11, by = 1), limits = c(0, 11)) +
  theme_ipsum() +
  geom_point(data = actual, mapping = aes(x = Fertilizer_Price, y = Grain_Price), color='white') +
  geom_text(data = actual, mapping = aes(x = Fertilizer_Price, y = Grain_Price, label = as.character(year)),
            hjust=-0.3, vjust=-0.3, color='white') +
  labs(fill = "Field number",
       x = expression(paste('Fertilizer price ($ kg'^{-1}, ")")),
       y = expression(paste('Grain price ($ kg'^{-1}, ")"))
       ) +
theme(text = element_text(family = "Times New Roman"))  # Font family
v
