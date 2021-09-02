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
## Time
mod_gbm_time <- gbm(
        formula = pm2.5_epa ~ pm2.5_cf1_a + temp + hum,
        data = dat_time_train,
        distribution = "gaussian",  # SSE loss function
        n.trees = 500,
        shrinkage = 0.1,
        interaction.depth = 3,
        n.minobsinnode = 10,
        cv.folds = 10
)

best <- which.min(mod_gbm_time$cv.error)
best
# get MSE and compute RMSE
sqrt(mod_gbm_time$cv.error[best])
# plot error curve
p1 <- gbm.perf(mod_gbm_time, method = "cv")
summary(mod_gbm_time)

save(mod_gbm_time, file = here("data","model","GBM","mod_gbm_time.RData"))
save(p1, here("data","model","GBM","p1.RData"))



