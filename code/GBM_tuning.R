install.packages("pacman")
library(pacman)

pacman::p_load(here)
# Packages for building machine learning algorithm
p_load(yardstick,gbm)
# Load tidyverse
p_load(tidyverse)

# Check working directory
print(here())

## Create spatial-temporal evaluation data set
load(here("data","tidy","CA","dat.RData"))

# Leave out 2020 March for evaluation
dat_time_train <- dat %>% 
        filter(time < "2020-02-10 00:00:00 UTC")

dat_time_test <- dat %>% 
        filter(time >= "2020-02-10 00:00:00 UTC")

# # Leave out top 20% furthest PA sensors for evaluation
# dat_loc_far_train <- dat %>% 
#         filter(dist < quantile(dat$dist, .8))
# 
# dat_loc_far_test <- dat %>% 
#         filter(dist >= quantile(dat$dist, .8))
# 
# # Leave out random 20% locations of PA sensors for evaluation
# set.seed(0)
# uniq_lat_pa <- unique(dat$lat_pa)
# ind_ran_train <- sample(1:length(uniq_lat_pa), size = round(0.8*length(uniq_lat_pa)), replace = F)
# lat_pa_train <- uniq_lat_pa[ind_ran_train]
# lat_pa_test <- uniq_lat_pa[-ind_ran_train]
# 
# dat_loc_ran_train <- dat %>% 
#         filter(dat$lat_pa %in% lat_pa_train)
# 
# dat_loc_ran_test <- dat %>% 
#         filter(dat$lat_pa %in% lat_pa_test)


### Gradient Boosting Method -------------------------------------
## 1. Number of trees -------------------------------------
# set.seed(123)
# mod_gbm_time <- gbm(
#         formula = pm2.5_epa ~ pm2.5_cf1_a + temp + hum,
#         data = dat_time_train,
#         distribution = "gaussian",  # SSE loss function
#         n.trees = 500,
#         shrinkage = 0.1,
#         interaction.depth = 3,
#         n.minobsinnode = 10,
#         cv.folds = 10,
#         verbose = TRUE
# )
# 
# 
# best <- which.min(mod_gbm_time$cv.error)
# best
# # get MSE and compute RMSE
# sqrt(mod_gbm_time$cv.error[best])
# # plot error curve
# gbm.perf(mod_gbm_time, method = "cv")
# summary(mod_gbm_time)
# 
# save(mod_gbm_time, file = here("data","model","GBM","mod_gbm_time.RData"))


## 2. Learning rate-------------------------------------
# create grid search
hyper_grid <- expand.grid(
        learning_rate = c(0.3, 0.2, 0.1, 0.05, 0.01, 0.005),
        RMSE = NA,
        trees = NA,
        time = NA
)

# execute grid search
for(i in seq_len(nrow(hyper_grid))) {
        # fit gbm
        set.seed(123)  # for reproducibility
        train_time <- system.time({
                m <- gbm(
                        formula = pm2.5_epa ~ pm2.5_cf1_a + temp + hum,
                        data = dat_time_train,
                        distribution = "gaussian",
                        n.trees = 500, 
                        shrinkage = hyper_grid$learning_rate[i], 
                        interaction.depth = 3, 
                        n.minobsinnode = 10,
                        cv.folds = 10 
                )
        })
        
        # add SSE, trees, and training time to results
        hyper_grid$RMSE[i]  <- sqrt(min(m$cv.error))
        hyper_grid$trees[i] <- which.min(m$cv.error)
        hyper_grid$Time[i]  <- train_time[["elapsed"]]
        
}

# results
arrange(hyper_grid, RMSE)

save(hyper_grid, file = here("data","model","GBM","hyper_grid.RData"))

