# Author: Xiang Chen
# Advisor: Abhirup Datta
# Department of Biostatistics
# Johns Hopkins University
# Last updated: 2021.8.11

##### Table of Content ####
# 0. Loading packages and set environment
# 1. Neural Network Model Tuning
# 1.1 Layers and nodes



##################0. Loading packages and set environment##################

## Check for packages needed to run analyses
# Using pacman to load all packages
# if(!require("pacman", quietly = T)){
#         install.packages(x)
#         require(x,character.only = T)
# }

install.packages("pacman")
if(TRUE){
library(pacman)

pacman::p_load(here)
# Packages for data visualization
# p_load(skimr)
# Packages for building machine learning algorithm
p_load(yardstick,keras,tfruns,tfestimators)
# Packages for creating map/spatial operation
# p_load(sf,lwgeom,geosphere,units,ggmap,MBA)
# p_load(sf,geosphere,units,ggmap,MBA)
# Load tidyverse
p_load(tidyverse)
# Set ggplot theme
theme_set(theme_minimal(base_size = 22))

# Check working directory
print(here())

## Create spatial-temporal evaluation data set
load(here("data","tidy","CA","dat.RData"))

# Leave out 2020 March for evaluation
dat_time_train <- dat %>% 
        filter(time < "2020-02-10 00:00:00 UTC")

dat_time_test <- dat %>% 
        filter(time >= "2020-02-10 00:00:00 UTC")

# Leave out top 20% furthest PA sensors for evaluation
dat_loc_far_train <- dat %>% 
        filter(dist < quantile(dat$dist, .8))

dat_loc_far_test <- dat %>% 
        filter(dist >= quantile(dat$dist, .8))

# Leave out random 20% locations of PA sensors for evaluation
set.seed(0)
uniq_lat_pa <- unique(dat$lat_pa)
ind_ran_train <- sample(1:length(uniq_lat_pa), size = round(0.8*length(uniq_lat_pa)), replace = F)
lat_pa_train <- uniq_lat_pa[ind_ran_train]
lat_pa_test <- uniq_lat_pa[-ind_ran_train]

dat_loc_ran_train <- dat %>% 
        filter(dat$lat_pa %in% lat_pa_train)

dat_loc_ran_test <- dat %>% 
        filter(dat$lat_pa %in% lat_pa_test)


## Split data to X and Y and normalized X and Y
# Time
X_train_time <- dat_time_train[, c("pm2.5_cf1_a","temp","hum")]
Y_train_time <- dat_time_train[, "pm2.5_epa"]

X_test_time  <- dat_time_test[, c("pm2.5_cf1_a","temp","hum")]
Y_test_time  <- dat_time_test[, "pm2.5_epa"]


# Location farthest
X_train_loc_far <- dat_loc_far_train[, c("pm2.5_cf1_a","temp","hum")]
Y_train_loc_far <- dat_loc_far_train[, "pm2.5_epa"]

X_test_loc_far  <- dat_loc_far_test[, c("pm2.5_cf1_a","temp","hum")]
Y_test_loc_far  <- dat_loc_far_test[, "pm2.5_epa"]

# Location random
X_train_loc_ran <- dat_loc_ran_train[, c("pm2.5_cf1_a","temp","hum")]
Y_train_loc_ran <- dat_loc_ran_train[, "pm2.5_epa"]

X_test_loc_ran  <- dat_loc_ran_test[, c("pm2.5_cf1_a","temp","hum")]
Y_test_loc_ran  <- dat_loc_ran_test[, "pm2.5_epa"]


## Normalizing data
norm_dt <- function(train_data, test_data){
        if(is.list(train_data)){
                mean <- apply(train_data, 2, mean)
                std <- apply(train_data, 2, sd)
                train_data <- scale(train_data, center = mean, scale = std) %>% 
                        as.matrix()
                test_data <- scale(test_data, center = mean, scale = std) %>% 
                        as.matrix()

        }else{
                mean <- mean(train_data)
                std <- sd(train_data)
                train_data <- scale(train_data, center = mean, scale = std) %>% 
                        as.matrix()
                test_data <- scale(test_data, center = mean, scale = std) %>% 
                        as.matrix()
        }

        return(list(train = train_data, test = test_data))
}

c(X_train_time_norm, X_test_time_norm) %<-% norm_dt(X_train_time, X_test_time)
c(X_train_loc_far_norm, X_test_loc_far_norm) %<-% norm_dt(X_train_loc_far, X_test_loc_far)
c(X_train_loc_ran_norm, X_test_loc_ran_norm) %<-% norm_dt(X_train_loc_ran, X_test_loc_ran)
}

save(X_train_time_norm, Y_train_time, 
     X_train_loc_far_norm, Y_train_loc_far,
     X_train_loc_ran_norm, Y_train_loc_ran,
     file = here("data","model","NN","NN.RData"))

# 1. Neural Network Model Tuning -----------------------------------------------
## 1.1 Layers and nodes --------------------------------------------------------

compiler <- function(model){
        model %>% compile(
                loss = 'mse',
                metrics = c('mae'),
                optimizer = "rmsprop"
        )
}

trainer <- function(model){
        model %>% fit(
                x = X_train_time_norm,
                y = Y_train_time,
                epochs = 25,
                batch_size = 4096,
                validation_split = 0.2,
                verbose = TRUE
        )
}

# One layer models -------------------------------------------------------------
# small capacity model
`1 layer_small` <- keras_model_sequential() %>%
        layer_dense(units = 16, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# medium small
`1 layer_medium small` <- keras_model_sequential() %>%
        layer_dense(units = 32, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# medium
`1 layer_medium` <- keras_model_sequential() %>%
        layer_dense(units = 64, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# medium large
`1 layer_medium large` <- keras_model_sequential() %>%
        layer_dense(units = 128, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# large
`1 layer_large` <- keras_model_sequential() %>%
        layer_dense(units = 256, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# huge
`1 layer_huge` <- keras_model_sequential() %>%
        layer_dense(units = 512, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# mega
`1 layer_mega` <- keras_model_sequential() %>%
        layer_dense(units = 1024, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# Two layer models -------------------------------------------------------------
# small capacity model
`2 layer_small` <- keras_model_sequential() %>%
        layer_dense(units = 16, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 8, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# medium small
`2 layer_medium small` <- keras_model_sequential() %>%
        layer_dense(units = 32, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 16, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# medium
`2 layer_medium` <- keras_model_sequential() %>%
        layer_dense(units = 64, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 32, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# medium large
`2 layer_medium large` <- keras_model_sequential() %>%
        layer_dense(units = 128, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 64, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# large
`2 layer_large` <- keras_model_sequential() %>%
        layer_dense(units = 256, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 128, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# huge
`2 layer_huge` <- keras_model_sequential() %>%
        layer_dense(units = 512, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 256, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# mega
`2 layer_mega` <- keras_model_sequential() %>%
        layer_dense(units = 1024, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 512, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# Three layer models -------------------------------------------------------------
# small capacity model
`3 layer_small` <- keras_model_sequential() %>%
        layer_dense(units = 16, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 8, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 4, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# medium small
`3 layer_medium small` <- keras_model_sequential() %>%
        layer_dense(units = 32, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 16, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 8, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# medium
`3 layer_medium` <- keras_model_sequential() %>%
        layer_dense(units = 64, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 32, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 16, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# medium large
`3 layer_medium large` <- keras_model_sequential() %>%
        layer_dense(units = 128, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 64, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 32, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# large
`3 layer_large` <- keras_model_sequential() %>%
        layer_dense(units = 256, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 128, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 64, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# huge
`3 layer_huge` <- keras_model_sequential() %>%
        layer_dense(units = 512, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 256, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 128, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# mega
`3 layer_mega` <- keras_model_sequential() %>%
        layer_dense(units = 1024, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 512, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 256, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# Four layer models -------------------------------------------------------------
# small capacity model
`4 layer_small` <- keras_model_sequential() %>%
        layer_dense(units = 16, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 8, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 4, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 2, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# medium small
`4 layer_medium small` <- keras_model_sequential() %>%
        layer_dense(units = 32, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 16, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 8, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 4, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# medium
`4 layer_medium` <- keras_model_sequential() %>%
        layer_dense(units = 64, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 32, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 16, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 8, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# medium large
`4 layer_medium large` <- keras_model_sequential() %>%
        layer_dense(units = 128, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 64, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 32, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 16, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# large
`4 layer_large` <- keras_model_sequential() %>%
        layer_dense(units = 256, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 128, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 64, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 32, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# huge
`4 layer_huge` <- keras_model_sequential() %>%
        layer_dense(units = 512, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 256, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 128, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 64, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# mega
`4 layer_mega` <- keras_model_sequential() %>%
        layer_dense(units = 1024, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 512, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 256, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 128, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# Five layer models -------------------------------------------------------------
# small capacity model
`5 layer_small` <- keras_model_sequential() %>%
        layer_dense(units = 16, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 8, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 4, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 2, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 2, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# medium small
`5 layer_medium small` <- keras_model_sequential() %>%
        layer_dense(units = 32, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 16, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 8, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 4, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 2, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# medium
`5 layer_medium` <- keras_model_sequential() %>%
        layer_dense(units = 64, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 32, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 16, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 8, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 4, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# medium large
`5 layer_medium large` <- keras_model_sequential() %>%
        layer_dense(units = 128, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 64, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 32, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 16, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 8, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# large
`5 layer_large` <- keras_model_sequential() %>%
        layer_dense(units = 256, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 128, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 64, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 32, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 16, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# huge
`5 layer_huge` <- keras_model_sequential() %>%
        layer_dense(units = 512, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 256, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 128, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 64, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 32, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()

# mega
`5 layer_mega` <- keras_model_sequential() %>%
        layer_dense(units = 1024, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dense(units = 512, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 256, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 128, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 64, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dense(units = 1, activation = "linear") %>%
        compiler() %>%
        trainer()


## Aggregate
models <- ls(pattern = "layer_") 
df_batch <- models %>%
        map(get) %>%
        map(~ data.frame(
                `Validation error` = .$metrics$val_loss,
                `Training error`   = .$metrics$loss,
                epoch = seq_len(.$params$epoch)
        )) %>%
        map2_df(models, ~ mutate(.x, model = .y)) %>%
        separate(model, into = c("Middle layers", "Number of nodes"), sep = "_") %>%
        gather(Validation, Loss, Validation.error:Training.error) %>%
        mutate(
                Validation = str_replace_all(Validation, "\\.", " "),
                `Number of nodes` = factor(`Number of nodes`, levels = c("small","medium small","medium","medium large","large","huge","mega")),
                `Batch normalization` = TRUE
        )

write.csv(df_batch, here("data","model","df_batch.csv"))

df2 <- df_batch %>% 
        filter(Validation == "Validation error")

best <- df2 %>% 
        filter(Validation == "Validation error") %>%
        group_by(`Middle layers`, `Number of nodes`) %>% 
        filter(Loss == min(Loss)) %>%
        mutate(label = paste("Min:", round(Loss, 2)))

plot_nn_full <- ggplot(df2, aes(epoch, Loss, color = `Batch normalization`)) + 
        geom_text(data = best, aes(x = 5, y = 85, label = label, 
                                   size = 4, fontface = "bold", 
                                   hjust = 0, vjust = 0)) + 
        geom_point() +
        geom_line() +
        facet_grid(`Number of nodes` ~ `Middle layers`, scales = "free_y") +
        scale_y_continuous(limits = c(0,130)) +
        xlab("Epoch") +
        scale_color_discrete("Batch normalization") +
        theme_bw() +
        theme(legend.position='none',
              text = element_text(size = 25))

ggsave(filename = "plot_nn_full.png",
       path = here("figures","model"),
       plot = plot_nn_full,
       device = "png",
       width = 30,
       height = 25,
       units = "cm",
       dpi = 300
)

plot_nn_trunc <- ggplot(df2, aes(epoch, Loss, color = `Batch normalization`)) + 
        geom_text(data = best, aes(x = 10, y = 30, label = label, 
                                   size = 4, fontface = "bold", 
                                   hjust = 0, vjust = 0)) + 
        geom_point() +
        geom_line() +
        facet_grid(`Number of nodes` ~ `Middle layers`, scales = "free_y") +
        scale_y_continuous(limits = c(20,32.5)) +
        xlab("Epoch") +
        scale_color_discrete("Batch normalization") +
        theme_bw() +
        theme(legend.position='none',
              text = element_text(size = 25))

ggsave(filename = "plot_nn_trunc.png",
       path = here("figures","model"),
       plot = plot_nn_trunc,
       device = "png",
       width = 30,
       height = 25,
       units = "cm",
       dpi = 300
)

## 1.2 Hyperparameters --------------------------------------------------------
runs <- tuning_run(here("code","NN_tfrun.R"), 
                   flags = list(
                           nodes1 = c(64, 128, 256),
                           nodes2 = c(64, 128, 256),
                           nodes3 = c(32, 64, 128),
                           dropout1 = c(0.2, 0.3, 0.4),
                           dropout2 = c(0.2, 0.3, 0.4),
                           dropout3 = c(0.1, 0.2, 0.3),
                           optimizer = c("rmsprop", "adam"),
                           lr_annealing = c(0.1, 0.05)
                   ),
                   sample = 0.05
)

runs %>% 
        filter(metric_val_loss == min(metric_val_loss)) %>% 
        glimpse()
save(runs, file = here("data","model","NN","runs.RData"))


# 2. Model fitting and prediction-----------------------------------------------
met_all <- c()

### Neuaral Network -------------------------------------
## Time
FLAGS <- flags(
        # nodes
        flag_numeric("nodes1", 256),
        flag_numeric("nodes2", 128),
        flag_numeric("nodes3", 64),
        flag_numeric("nodes4", 32),
        flag_numeric("nodes5", 16),
        # dropout
        flag_numeric("dropout1", 0.4),
        flag_numeric("dropout2", 0.3),
        flag_numeric("dropout3", 0.2),
        flag_numeric("dropout4", 0.1),
        flag_numeric("dropout5", 0.05),
        # learning parameters
        flag_string("optimizer", "rmsprop"),
        flag_numeric("lr_annealing", 0.1)
)

## Define Model

model <- keras_model_sequential() %>%
        layer_dense(units = FLAGS$nodes1, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_batch_normalization() %>%
        layer_dropout(rate = FLAGS$dropout1) %>%
        layer_dense(units = FLAGS$nodes2, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dropout(rate = FLAGS$dropout2) %>%
        layer_dense(units = FLAGS$nodes3, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dropout(rate = FLAGS$dropout3) %>%
        layer_dense(units = FLAGS$nodes4, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dropout(rate = FLAGS$dropout4) %>%
        layer_dense(units = FLAGS$nodes5, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dropout(rate = FLAGS$dropout5) %>%
        layer_dense(units = 1, activation = "linear") %>%
        compile(
                loss = 'mse',
                metrics = c('mae'),
                optimizer = FLAGS$optimizer
        ) %>%
        fit(
                x = X_train_time_norm,
                y = Y_train_time,
                epochs = 30,
                batch_size = 4096,
                validation_split = 0.2,
                callbacks = list(
                        callback_early_stopping(patience = 5),
                        callback_reduce_lr_on_plateau(factor = FLAGS$lr_annealing)
                ),
                verbose = TRUE
        )

test_model <- keras_model_sequential() %>%
        layer_dense(units = 256, activation = "relu", input_shape = ncol(X_train_time_norm)) %>%
        layer_dense(units = 256, activation = "relu") %>%
        layer_dense(units = 128, activation = "relu") %>%
        layer_dense(units = 1, activation = "linear") %>%
        compile(
                loss = 'mse',
                metrics = c('mae'),
                optimizer = FLAGS$optimizer
        ) %>%
        fit(
                x = X_train_time_norm,
                y = Y_train_time,
                epochs = 30,
                batch_size = 4096,
                validation_split = 0.2,
                verbose = TRUE
        )
min(test_model$metrics$val_loss)
min(test_model$metrics$loss)

result <- test_model %>% evaluate(X_test_time_norm, Y_test_time)
result
