library(pacman)

# Packages for data import and data wrangling
pacman::p_load(here,keras,tidyverse)


##################2.2 Model fitting and evaluation##################
## Create spatial-temporal evaluation data set
load(here("data","model","NN","NN.RData"))


## Model fitting and prediction
### Neuaral Network -------------------------------------
## Time
FLAGS <- flags(
        # nodes
        flag_numeric("nodes1", 256),
        flag_numeric("nodes2", 128),
        flag_numeric("nodes3", 64),
        # dropout
        flag_numeric("dropout1", 0.4),
        flag_numeric("dropout2", 0.3),
        flag_numeric("dropout3", 0.2),
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
        layer_dense(units = 1, activation = "linear") %>%
        compile(
                loss = 'mse',
                metrics = c('mae'),
                optimizer = FLAGS$optimizer
        ) %>% fit(
                x = X_train_time_norm,
                y = Y_train_time,
                epochs = 30,
                batch_size = 2048,
                validation_split = 0.2,
                callbacks = list(
                        callback_early_stopping(patience = 5),
                        callback_reduce_lr_on_plateau(factor = FLAGS$lr_annealing)
                ),
                verbose = TRUE
        )
