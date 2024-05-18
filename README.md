# StanGP_RiceStripTrial
This code implements the gaussian process regression for analyzing strip trials for rice production (Mia et al. under reivew). Eleven trial datasets contain original point id, x and y coordinates, rice yield (t/ha), and N treatment (N kg/ha) as "./data/raw_csv/\*.csv". The analyzed outcomes can be found as "./data/out_rds/\*.rds". Yield data was predicted using the multimodal deep learning model based on UAV-based multispectral images and weather data (Mia et al. 2023). 

Mia, M.S.; Tanabe, R.; Habibi, L.N.; Hashimoto, N.; Homma, K.; Maki, M.; Matsui, T.; Tanaka, T.S.T. Multimodal Deep Learning for Rice Yield Prediction Using UAV-Based Multispectral Imagery and Weather Data. Remote Sens. 2023, 15, 2511. https://doi.org/10.3390/rs15102511

# How to use
- ‘1. stan_GP_cholesky.R’ fit the GP regression using gp.stan. Field 7 is used for a test run.
- ‘2. stacking_prob_analysis.R’ generates a probability map (the number of fields that achieve positive profits by changing the fertilizer input rates at the 90% probability).

# Environement 
- R version 4.2.2
- RStan version 2.26.23
- Rstudio Version 2023.12.1+402 (2023.12.1+402)
- ggplot2, lattice, viridis, hrbrthemes for visualization purpose only

# Note
The manuscript using this code is under review. Once the paper is published, the link will be updated.