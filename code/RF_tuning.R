install.packages("pacman")
install.packages("ranger")
install.packages("h2o")
library(pacman)

pacman::p_load(here)
# Packages for building machine learning algorithm
p_load(yardstick,doParallel,ranger)
p_load(h2o)
# Load tidyverse
p_load(tidyverse)

# library(ranger)
# library(h2o)
# Set ggplot theme
theme_set(theme_minimal(base_size = 22))
# Check working directory
print(here())

cl<-makePSOCKcluster(4)
registerDoParallel(cl)



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

# Ranger --------------------------------------
# number of features
n_features <- 3

# train a default random forest model
mod_test <- ranger(
        formula = pm2.5_epa ~ pm2.5_cf1_a + temp + hum, 
        data = dat_time_train,
        num.trees = 100,
        mtry = floor(n_features / 3),
        respect.unordered.factors = "order",
        seed = 123,
        verbose = TRUE,
        write.forest = TRUE
)

# get OOB RMSE
(default_rmse <- sqrt(mod_test$prediction.error))



pre_test <- predict(mod_test, data = dat_time_test)
pre_test <- cbind(pre = pre_test$predictions, obs = dat_time_test$pm2.5_epa) %>%
        as_tibble()
met_test <- metrics(pre_test, truth = obs, estimate = pre)
met_test

# Tunning process
## 1. number of trees --------------------------------------
n_features <- 3

# tuning grid
tuning_grid <- expand.grid(
        trees = seq(300, 500, by = 20),
        rmse  = NA
)

for(i in seq_len(nrow(tuning_grid))) {
        
        # Fit a random forest
        fit <- ranger(
                formula    = pm2.5_epa ~ pm2.5_cf1_a + temp + hum, 
                data       = dat_time_train, 
                num.trees  = tuning_grid$trees[i],
                mtry       = floor(n_features / 3),
                respect.unordered.factors = 'order',
                verbose    = TRUE,
                seed       = 123,
                write.forest = FALSE
        )
        
        # Extract OOB RMSE
        tuning_grid$rmse[i] <- sqrt(fit$prediction.error)
        
}

p1 <- ggplot(tuning_grid, aes(trees, rmse)) +
        geom_line(size = 1) +
        ylab("OOB Error (RMSE)") +
        xlab("Number of trees")

save(tuning_grid, file = here("data","model","RF","tuning_grid1.RData"))
save(p1, file = here("data","model","RF","p1.RData"))

# load(here("data","model","RF","tuning_grid1.RData"))
# load(p1, file = here("data","model","RF","p1.RData"))

## 2. Number of trees and mtry --------------------------------------
n_features <- 3

tuning_grid <- expand.grid(
        trees = seq(300, 560, by = 20),
        mtry  = c(1:3),
        rmse  = NA
)

for(i in seq_len(nrow(tuning_grid))) {
        fit <- ranger(
                formula    = pm2.5_epa ~ pm2.5_cf1_a + temp + hum, 
                data       = dat_time_train, 
                num.trees  = tuning_grid$trees[i],
                mtry       = tuning_grid$mtry[i],
                respect.unordered.factors = 'order',
                verbose    = TRUE,
                seed       = 123,
                write.forest = FALSE
        )
        
        tuning_grid$rmse[i] <- sqrt(fit$prediction.error)
        
}

labels <- tuning_grid %>%
        filter(trees == 10) %>%
        mutate(mtry = as.factor(mtry))

p2 <- tuning_grid %>%
        mutate(mtry = as.factor(mtry)) %>%
        ggplot(aes(trees, rmse, color = mtry)) +
        geom_line(size = 1, show.legend = TRUE) +
        ggrepel::geom_text_repel(data = labels, aes(trees, rmse, label = mtry), show.legend = FALSE) +
        ylab("OOB Error (RMSE)") +
        xlab("Number of trees")

save(tuning_grid, file = here("data","model","RF","tuning_grid2.RData"))
save(p1, file = here("data","model","RF","p2.RData"))


## 3. Random grid search for other parameters --------------------------------------
h2o.no_progress()
h2o.init(max_mem_size = "10g")
# h2o.init(max_mem_size = "5g")

# convert training data to h2o object
train_h2o <- as.h2o(dat_time_train[,c("pm2.5_epa", "pm2.5_cf1_a", "temp", "hum")])

# set the response column to Sale_Price
response <- "pm2.5_epa"

# set the predictor names
predictors <- c("pm2.5_cf1_a", "temp", "hum")

h2o_rf1 <- h2o.randomForest(
        x = predictors, 
        y = response,
        training_frame = train_h2o, 
        ntrees = 440,
        mtries = 1,
        seed = 123,
        verbose = TRUE
)

h2o_rf1

# hyperparameter grid
hyper_grid <- list(
        min_rows = c(1, 3, 5, 10),
        max_depth = c(10, 20, 30),
        sample_rate = c(.60, .70, .80, .90, 1)
)

# random grid search strategy
search_criteria <- list(
        strategy = "RandomDiscrete",
        stopping_metric = "mse",
        stopping_tolerance = 0.001,   # stop if improvement is < 0.1%
        stopping_rounds = 10         # over the last 10 models
        #        max_runtime_secs = 60*5      # or stop search after 5 min.
)

# perform grid search 
random_grid <- h2o.grid(
        algorithm = "randomForest",
        grid_id = "rf_random_grid",
        x = predictors, 
        y = response, 
        training_frame = train_h2o,
        hyper_params = hyper_grid,
        ntrees = 440,
        mtries = 1,
        seed = 123,
        stopping_metric = "RMSE",   
        stopping_rounds = 10,           # stop if last 10 trees added 
        stopping_tolerance = 0.005,     # don't improve RMSE by 0.5%
        search_criteria = search_criteria
)

# collect the results and sort by our model performance metric 
# of choice
random_grid_perf <- h2o.getGrid(
        grid_id = "rf_random_grid", 
        sort_by = "mse", 
        decreasing = FALSE
)
random_grid_perf

save(random_grid, file = here("data","model","RF","random_grid.RData"))
save(random_grid_perf, file = here("data","model","RF","random_grid_perf.RData"))

# 4 Tuning  --------------------------------------
# hyperparameter grid
n_features <- 3

tuning_grid <- expand.grid(
        min_node = c(1, 3, 5, 10),
        max_depth = c(10, 20, 30),
        sample_fraction = c(.60, .70, .80, .90, 1),
        rmse  = NA
)

for(i in seq_len(nrow(tuning_grid))) {
        fit <- ranger(
                formula    = pm2.5_epa ~ pm2.5_cf1_a + temp + hum, 
                data       = dat_time_train, 
                num.trees  = 440,
                mtry       = floor(n_features / 3),
                min.node.size = tuning_grid$min_node[i],
                max.depth = tuning_grid$max_depth[i],
                sample.fraction = tuning_grid$sample_fraction[i],
                verbose    = TRUE,
                seed       = 123,
                write.forest = FALSE
        )
        
        tuning_grid$rmse[i] <- sqrt(fit$prediction.error)
        
}

tuning_grid[with(tuning_grid,order(rmse)),] %>% 
        head(10)

save(tuning_grid, file = here("data","model","RF","tuning_grid4.RData"))
# save(p1, file = here("data","model","RF","p2.RData"))