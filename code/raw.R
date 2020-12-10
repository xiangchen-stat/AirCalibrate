# Author: Xiang Chen
# Last updated: 2020.12.6

# 1. Data import and wrangling
## Check for packages needed to run analyses
if(!require("pacman", quietly = T)){
        install.packages(x)
        require(x,character.only = T)
}
pacman::p_load(magrittr,broom,skimr,naniar,lubridate,here,tidyverse)

## Read in csv files. Only include Primary data for the sensor.
csv_files <- list.files(here::here("data","raw","MD"), recursive = TRUE,
                          pattern = "Primary.*\\.csv")
tbl_files <- here("data","raw","MD",csv_files) %>%
        map(~ readr::read_csv(.))
## Rename tibbles
tbl_names <- csv_files %>%
        str_extract(".*(\\(outside\\)|\\(inside\\)|\\(undefined\\))") %>% 
        str_remove("(\\ \\(outside\\)|\\ \\(inside\\)|\\ \\(undefined\\))")
names(tbl_files) <- tbl_names
## Get covariate (label, location, period, etc.) of the sensor
covariate <- csv_files %>% 
        str_extract("(\\(outside\\)|\\(inside\\)|\\(undefined\\)).*") %>% 
        str_remove_all("[\\(\\)\\-]") %>% 
        str_remove("\\.csv") %>% 
        str_split(pattern = " ") %>% 
        as_tibble(., .name_repair = "minimal") %>% 
        t(.) %>% 
        as_tibble(.) %>% 
        rename_all(~c("environment","latitude","longitude","file_type","average","start_time","end_time")) %>% 
        mutate(label = tbl_names,
               latitude = as.numeric(latitude),
               longitude = as.numeric(longitude)) %>% 
        relocate(label)
## Split covariate and data to channel A and B
covariate_A <- covariate %>% 
        filter(!str_detect(label, "( B)$"))
covariate_B <- covariate %>% 
        filter(str_detect(label, pattern = "( B)$"))

tbl_files_A <- tbl_files[!str_detect(covariate$label, "( B)$")]
tbl_files_B <- tbl_files[str_detect(covariate$label, "( B)$")]


## Select key variables from the data, rename variables, remove duplicates, 
## and add missing dates.
## Add missing date
start_time <- as_datetime("2019-01-01 00:00:00 UTC")
end_time <- as_datetime("2021-01-01 00:00:00 UTC")
interval_time <- 600 # Time interval is 10 minutes.
full_time <- as_datetime(seq(start_time, end_time, by = interval_time))
full_time <- tibble(time = full_time)

tbl_files_A_reduced <-  
        tbl_files_A[1:length(tbl_files_A)] %>% 
                map(~ select(., "created_at","UptimeMinutes","PM2.5_ATM_ug/m3",
                             "Temperature_F","Humidity_%")) %>% 
                map(~ rename(., time = created_at,
                             uptime = UptimeMinutes,
                             pm2.5 = "PM2.5_ATM_ug/m3",
                             temp = "Temperature_F",
                             humidity = "Humidity_%")) %>% 
                map(~ mutate(., time = lubridate::as_datetime(time))) %>% 
                map(~ distinct(.)) %>% # Remove duplicated rows
                map(~ right_join(., full_time, by = "time")) %>% # Add missing dates
                map(~ arrange(., time))

## Check if there is still any duplicated time point
tbl_files_A_reduced %>%
        map(~ select(., time)) %>% 
        map(~ sum(duplicated(.))) %>% 
        unlist() %>% 
        sum()

tbl_files_B_reduced <- 
        tbl_files_B[1:length(tbl_files_B)] %>% 
        map(~ select(., "created_at","UptimeMinutes","PM2.5_ATM_ug/m3")) %>% 
        map(~ rename(., time = created_at,
                     uptime = UptimeMinutes,
                     pm2.5 = "PM2.5_ATM_ug/m3")) %>% 
        map(~ mutate(., time = lubridate::as_datetime(time))) %>% 
        map(~ distinct(.)) %>% # Remove duplicated rows
        map(~ right_join(., full_time, by = "time")) %>% # Add missing dates
        map(~ arrange(., time))

## Check if there is still any duplicated time point
tbl_files_B_reduced %>%
        map(~ select(., time)) %>% 
        map(~ sum(duplicated(.))) %>% 
        unlist() %>% 
        sum()
# rm(list = c("tbl_files","tbl_files_A","tbl_files_B","csv_files","tbl_names","covariate",
            # "start_time","end_time","interval_time","full_time"))


