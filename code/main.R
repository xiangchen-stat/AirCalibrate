# Author: Xiang Chen
# Advisor: Abhirup Datta
# Last updated: 2021.4.25

##### Table of Content ####
# 0. Loading packages and set environment
# 1. Data import and wrangling
## 1.1 EPA Data import and reduce
## 1.2 PA Data import and wrangling
## 1.3 Data matching and wrangling
## 1.4 Plot maps
# 2. Data analysis
## 2.1 Exploratory Analysis
## 2.2 Model fitting and evaluation
## 2.3 Simulation
## 2.4 MBA Interpolation Map


##################0. Loading packages and set environment##################

## Check for packages needed to run analyses
# Using pacman to load all packages
# if(!require("pacman", quietly = T)){
#         install.packages(x)
#         require(x,character.only = T)
# }

install.packages("pacman")
library(pacman)

# Packages for data import and data wrangling
pacman::p_load(here,reshape2,lubridate,hms)
# Packages for data visualization
p_load(skimr,GGally,RColorBrewer,viridis,ggpubr)
# Packages for building machine learning algorithm
p_load(randomForest,gbm,yardstick)
# Packages for creating map/spatial operation
# p_load(sf,lwgeom,geosphere,units,ggmap,MBA)
p_load(sf,geosphere,units,ggmap,MBA)
# Load tidyverse
p_load(tidyverse)
# Set ggplot theme
theme_set(theme_minimal(base_size = 22))

# Check working directory
print(here())

##################1. Data import and wrangling##################
##################1.1 EPA Data import and reduce##################

# Function to read in EPA 2019 and 2020 data
read_epa <- function(path_19,path_20){
        dt19 <- read_csv(path_19)
        dt19_reduced <- dt19 %>%
                filter(`State Name`=="California")
        rm(dt19)
        
        dt20 <- read_csv(path_20)
        dt20_reduced <- dt20 %>%
                filter(`State Name`=="California")
        rm(dt20)
        
        dt <- bind_rows(dt19_reduced, dt20_reduced)
}

# Read in pm2.5 data
pm2.5 <- read_epa(here("data","raw","EPA","hourly_88101_2019.zip"),
                  here("data","raw","EPA","hourly_88101_2020.zip"))

# save(pm2.5, file=here("data","tidy","EPA","pm2.5_CA.RData"))
# load(here("data","tidy","EPA",pm2.5_CA.RData))

# Select key variables to prepare for joining data
pm2.5_reduced <- pm2.5 %>% 
        select("State Code", "County Code", "Latitude", "Longitude", 
               "Date Local", "Time Local", "Date GMT", "Time GMT", 
               "Sample Measurement", "State Name", "County Name") %>% 
        rename(pm2.5="Sample Measurement") %>% 
        distinct()

save(pm2.5_reduced, file=here("data","tidy","EPA","pm2.5_CA_reduced.RData"))
# load(here("data","tidy","EPA","pm2.5_CA_reduced.RData"))

# Get EPA covaritate data matrix
pm2.5_reduced_cov <- pm2.5_reduced %>% 
        select("State Code","County Code","Latitude","Longitude","State Name","County Name") %>% 
        distinct()

# Check if there is any duplicates
length(pm2.5_reduced_cov$Latitude)
length(unique(pm2.5_reduced_cov$Latitude))        

# save(pm2.5_reduced_cov, file=here("data","tidy","EPA","pm2.5_CA_reduced_cov.RData"))
##################1.1 EPA Data import and reduce END##################
############################END############################


##################1.2 PA Data import and wrangling##################
# Below are the code for processing raw PurpleAir csv.
# Unzip file
check_ls <- list.files(here::here("data","raw","CA"), recursive = TRUE)
if(length(check_ls)==1){
        unzip(zipfile = here("data","raw","CA",paste0(check_ls)), 
              exdir = here("data","raw","CA"))
}

# Only include Primary data for the sensors.
csv_files <- list.files(here::here("data","raw","CA"), recursive = TRUE,
                        pattern = "Primary.*\\.csv")
# Filter file names with non ASCII characters, such as (¡¯, ?) and (?), duplicated file, and other strange names.
csv_files <- csv_files[!str_detect(csv_files,"\\?|\\([1-9]\\)\\.csv")]
csv_files <- csv_files[stringi::stri_enc_mark(csv_files)=="ASCII"]
csv_files <- csv_files[!str_detect(csv_files," P[1-9] (\\(outside\\)|\\(inside\\)|\\(undefined\\))")]

# Check if there is any strange name, eg: " B B "
check <- csv_files[str_detect(csv_files," B B (\\(outside\\)|\\(inside\\)|\\(undefined\\))")]
if(length(check)!=0){
        print(check)
        stop("You have to delete printed data file for both channel since they have weird names.")
}
rm(check)

# save(csv_files, file = here("data","tidy","CA","csv_files.RData"))
# load(here("data","tidy","CA","csv_files.RData"))

# Read and save data
pa <- here("data","raw","CA",csv_files) %>%
        purrr::map(~ suppressMessages(suppressWarnings(readr::read_csv(.))))
# save(pa, file = here("data","tidy","CA","pa_raw.RData"))

# Load data
# load(here("data","tidy","CA","pa.RData"))
# load(here("data","tidy","CA","csv_files.RData"))

## Rename tibbles
tbl_names <- csv_files %>%
        str_extract(".*(\\(outside\\)|\\(inside\\)|\\(undefined\\))") %>% 
        str_remove("(\\ \\(outside\\)|\\ \\(inside\\)|\\ \\(undefined\\))")
names(pa) <- tbl_names

## Get covariate data (label, location, period, etc.) of the sensors
covariate <- csv_files %>% 
        str_extract("(\\(outside\\)|\\(inside\\)|\\(undefined\\)).*") %>% 
        str_remove_all("[\\(\\)\\-]") %>% 
        str_remove("\\.csv") %>% 
        str_split(pattern = " ")

# Delete records whose covariates fails to be the same length
csv_files <- csv_files[lengths(covariate)==7]
tbl_names <- tbl_names[lengths(covariate)==7]
pa <- pa[lengths(covariate)==7]
covariate <- covariate[lengths(covariate)==7]

# Check the length of each element of covariates
check <- sum(!lengths(covariate)==7)
if(check!=0){
        print(check)
        rm(check)
        stop("You have to check the lengths of the covariate list elements")
}
rm(check)

covariate <- covariate %>% 
        as_tibble(., .name_repair = "minimal") %>% 
        t(.) %>% # transpose
        as_tibble(.) %>% 
        rename_all(~c("environment","latitude","longitude","file_type","average","start_time","end_time")) %>% 
        mutate(name = tbl_names,
               latitude = as.numeric(latitude),
               longitude = as.numeric(longitude),
               longitude = -longitude, # longitude should be negative in California
               channel = ifelse(str_detect(name, "( B)$"), "B", "A"),
               label = str_remove(name, "( B)$")) %>% 
        relocate(label) %>% 
        select(-name)

# Delete records whose columns fail to be the same length (i.e. empty records)
csv_files <- csv_files[lengths(pa)==10]
tbl_names <- tbl_names[lengths(pa)==10]
covariate <- covariate[lengths(pa)==10,]
pa <- pa[lengths(pa)==10]

# Check length of pa
check <- sum(!lengths(pa)==10)
if(check!=0){
        print(check)
        rm(check)
        stop("You have to check the lengths of the purple air data list elements")
}
rm(check)

## Select variables, rename variables, remove duplicates
pa_reduced <-  
        pa[1:length(pa)] %>%
        purrr::map(~ select(., -"X10")) %>% # Empty column
        purrr::map(~ rename(., time = created_at,
                            uptime = UptimeMinutes,
                            pm2.5 = "PM2.5_ATM_ug/m3")) %>% 
        purrr::map(~ mutate(., time = lubridate::as_datetime(time))) %>% 
        purrr::map(~ distinct(.)) %>% # Remove duplicated rows
        purrr::map(~ arrange(., time))

## Check if there is still any duplicated time point
# Check length of pa
check <- pa_reduced %>%
        purrr::map(~ select(., time)) %>% 
        purrr::map(~ sum(duplicated(.))) %>% 
        unlist() %>% 
        sum()
if(check!=0){
        print(check)
        rm(check)
        warning("You have to check the duplicated rows within purple air data")
}
rm(check)

# Combine data with covariate information
for (i in 1:length(csv_files)) {
        pa_reduced[[i]] <- pa_reduced[[i]] %>% 
                cbind(., covariate[i,])
}

# Split data by channel A and B 
# Reason for doing splitting data: Since channel B data doesn't have environment 
# variable information, I have to split data and then rejoin them to get this variable.
pa_A <- pa_reduced[covariate$channel=="A"]
pa_B <- pa_reduced[covariate$channel=="B"]

# Rename and remove NA
pa_A_reduced <-  
        pa_A[1:length(pa_A)] %>% 
        purrr::map(~ select(., -"channel")) %>% 
        purrr::map(~ rename(.,
                            PM1_CF1_A = "PM1.0_CF1_ug/m3", 
                            PM2.5_CF1_A =  "PM2.5_CF1_ug/m3", 
                            PM10_CF1_A = "PM10.0_CF1_ug/m3",
                            uptime_A = "uptime",          
                            rssi = "RSSI_dbm",
                            temp = "Temperature_F",   
                            humidity = "Humidity_%",      
                            pm2.5_A = "pm2.5"         
        )) %>% 
        purrr::map(~ filter(., !is.na(.$PM2.5_CF1_A)))

pa_B_reduced <- 
        pa_B[1:length(pa_B)] %>% 
        purrr::map(~ select(., "time":"longitude", 
                            -"environment")) %>% 
        purrr::map(~ rename(.,
                            PM1_CF1_B = "PM1.0_CF1_ug/m3", 
                            PM2.5_CF1_B =  "PM2.5_CF1_ug/m3", 
                            PM10_CF1_B = "PM10.0_CF1_ug/m3",
                            uptime_B = "uptime",
                            adc = "ADC",
                            pressure = "Pressure_hpa",
                            iaq = "IAQ",                     
                            pm2.5_B = "pm2.5"         
        )) %>% 
        purrr::map(~ filter(., !is.na(.$PM1_CF1_B)))

# Change data from list to matrix
pa_A_reduced_long <- bind_rows(pa_A_reduced)
pa_B_reduced_long <- bind_rows(pa_B_reduced)

# Join Channel A and B data
pa_long <- left_join(pa_A_reduced_long,
                     pa_B_reduced_long,
                     by = c("time", "label", "latitude","longitude"))

pa_long <- pa_long %>% 
        filter(environment=="outside") %>% 
        filter(., !is.na(.$PM2.5_CF1_A)) %>% 
        filter(., !is.na(.$PM2.5_CF1_B)) %>% 
        filter(., !is.na(.$temp)) %>% 
        filter(., !is.na(.$humidity))

# save(pa_long, file = here("data","tidy","CA","pa_long.RData"))
# load(here("data","tidy","CA","pa_long.RData"))

# Remove NAs and PM2.5 greater than 3000
pa_long_reduced1 <- pa_long %>% 
        mutate(apb=abs((PM2.5_CF1_A-PM2.5_CF1_B)/PM2.5_CF1_A),
               ind_big_A=ifelse(PM2.5_CF1_A>3000,1,0),
               ind_big_B=ifelse(PM2.5_CF1_B>3000,1,0)) %>% 
        filter(ind_big_A==0,ind_big_B==0)%>% 
        mutate(ind_apb=ifelse(apb>quantile(apb,0.85,na.rm = T),1,0))

# Remove biggest 15% absolute percentage bias
pa_long_reduced2 <- pa_long_reduced1  %>%
        filter(ind_apb==0)

# save(pa_long_reduced2, file = here("data","tidy","CA","pa_long_reduced.RData"))
# load(here("data","tidy","CA","pa_long_reduced.RData"))

# Get covariate data matrix
pa_long_reduced2_cov <- pa_long_reduced2 %>% 
        select("label","environment","latitude","longitude","file_type",
               "average","start_time","end_time") %>% 
        distinct()
# save(pa_long_reduced2_cov, file = here("data","tidy","CA","cov.RData"))
##################1.2 PA Data import and wrangling END##################
############################END############################


##################1.3 Data matching and wrangling##################
# Load EPA data and covariate
# load(here("data","tidy","EPA","pm2.5_CA_reduced.RData"))
# Mutate negative pm2.5 to 0
pm2.5_reduced$pm2.5 <- ifelse(pm2.5_reduced$pm2.5<0, 0, pm2.5_reduced$pm2.5)
epa <- pm2.5_reduced %>% 
        mutate(`Date GMT` = date(`Date GMT`),
               `Time GMT` = as_hms(`Time GMT`))
# rm(pm2.5_reduced)

# load(here("data","tidy","EPA","pm2.5_CA_reduced_cov.RData"))
epa_cov <- pm2.5_reduced_cov
# rm(pm2.5_reduced_cov)

# Load PA data and covariates
# load(here("data","tidy","CA","pa_long_reduced.RData"))
pa <- pa_long_reduced2
# rm(pa_long_reduced2)

# load(here("data","tidy","CA","cov.RData"))
pa_cov <- pa_long_reduced2_cov
# rm(pa_long_reduced2_cov)

# Transform covariate data to sf dataframe
# Datum = EPSG: 4326, i.e. WGS 84
pa_cov_sf = st_as_sf(pa_cov, coords =c("longitude","latitude"), crs = 4326)
epa_cov_sf = st_as_sf(epa_cov, coords = c("Longitude", "Latitude"), crs = 4326)

# Calculate nearest location index, and distance
ind_near <- st_nearest_feature(pa_cov_sf, epa_cov_sf)
# distance <- sf::st_distance(pa_cov_sf, epa_cov_sf[ind_near, ], by_element = T)
# save(distance, file = here("data","tidy","CA","distance.RData"))
load(here("data","tidy","CA","distance.RData"))

line_near <- st_nearest_points(pa_cov_sf, epa_cov_sf[ind_near, ], pairwise = TRUE)
ind_dis <- distance <= set_units(5000, m)

line_near_5km <- line_near[ind_dis]
# save(line_near, file = here("data","plot","CA","line_near.RData"))
# save(line_near_5km, file = here("data","plot","CA","line_near_5km.RData"))

# Add distance information to covariate
pa_cov <- pa_cov %>% 
        mutate(lon_epa = epa_cov[ind_near,]$Longitude,
               lat_epa = epa_cov[ind_near,]$Latitude,
               dist = distance)

# Reduce number of covariates
pa_cov_reduced <- pa_cov %>% 
        select(label, longitude, latitude, lon_epa, lat_epa, dist)

loc_epa_full <- select(pa_cov_reduced, lon_epa, lat_epa) %>% 
        mutate(group = "EPA") %>% 
        rename(lon = lon_epa, lat = lat_epa) %>% 
        distinct()

loc_pa_full <- select(pa_cov_reduced, longitude, latitude) %>% 
        mutate(group = "PurpleAir") %>% 
        rename(lon = longitude, lat = latitude) %>% 
        distinct()

loc_both_full <- rbind(loc_pa_full,loc_epa_full)

# save(loc_both_full, file=here("data","tidy","CA","loc_both_full.RData"))
# load(here("data","tidy","CA","loc_both_full.RData"))

pa_cov_reduced_filter <- pa_cov_reduced %>% 
        filter(dist<=set_units(5000, m))

loc_epa <- select(pa_cov_reduced_filter, lon_epa, lat_epa) %>% 
        mutate(group = "EPA") %>% 
        rename(lon = lon_epa, lat = lat_epa) %>% 
        distinct()

loc_pa <- select(pa_cov_reduced_filter, longitude, latitude) %>% 
        mutate(group = "PurpleAir") %>% 
        rename(lon = longitude, lat = latitude) %>% 
        distinct()

loc_both <- rbind(loc_pa,loc_epa)

# save(loc_both, file=here("data","tidy","CA","loc_both.RData"))
# load(here("data","tidy","CA","loc_both.RData"))

# Join PA data with covariate information
pa_join <- pa %>% 
        left_join(., y = pa_cov_reduced, by = c("label","latitude","longitude")) %>% 
        mutate(date_GMT = date(time),
               hms_GMT = str_remove(time, " UTC"),
               hms_GMT = str_remove(hms_GMT, ".* "),
               hms_GMT = as_hms(hms_GMT)) # Get hour-minute-second data

# save(pa_join, file=here("data","tidy","CA","pa_join.RData"))
# load(here("data","tidy","CA","pa_join.RData"))

# Some EPA sites has multiple devices. We only use one device.
ind_epa_dup <- epa %>% 
        select(Latitude, Longitude, `Date GMT`, `Time GMT`) %>% 
        duplicated()

epa_reduced <- epa[!ind_epa_dup,]

# save(epa_reduced, file=here("data","tidy","EPA","epa_reduced.RData"))
# load(here("data","tidy","EPA","epa_reduced.RData"))

# Join PA data with nearest EPA data by time and location
pa_join_epa <- pa_join %>% 
        left_join(., y = epa_reduced,
                  by = c("lon_epa"="Longitude",
                         "lat_epa"="Latitude",
                         "date_GMT"="Date GMT",
                         "hms_GMT"="Time GMT"))

# save(pa_join_epa, file = here("data","tidy","CA","pa_join_epa.RData"))
# load(here("data","tidy","CA","pa_join_epa.RData"))

# Choose distance of paired sites less than 5km
pa_join_epa_reduced <- pa_join_epa %>% 
        filter(dist<=set_units(5000, m))

# Check number of matched pa sites
length(unique(pa_join_epa_reduced$label)) # PA sites
length(unique(pa_join_epa_reduced$lat_epa)) # EPA sites

# Get paired EPA+PA data
dat_raw <- pa_join_epa_reduced %>% 
        select("time","pm2.5","PM2.5_CF1_A","PM2.5_CF1_B","pm2.5_A","pm2.5_B","temp","humidity",
               "label","longitude","latitude","lon_epa","lat_epa","dist") %>% 
        rename(pm2.5_epa = pm2.5,
               pm2.5_cf1_a = PM2.5_CF1_A,
               pm2.5_cf1_b = PM2.5_CF1_B,
               pm2.5_atm_a = pm2.5_A,
               pm2.5_atm_b = pm2.5_B,
               hum = humidity,
               lon_pa = longitude,
               lat_pa = latitude) %>% 
        filter(!is.na(pm2.5_epa)) %>%
        filter(!is.na(pm2.5_cf1_a)) %>% 
        filter(!is.na(pm2.5_cf1_b)) %>% 
        filter(!is.na(pm2.5_atm_a)) %>% 
        filter(!is.na(pm2.5_atm_b)) %>% 
        filter(!is.na(temp)) %>% 
        filter(!is.na(hum)) %>% 
        filter(temp >= 0) %>% 
        distinct()

# save(dat_raw, file = here("data","tidy","CA","dat_raw.RData"))
# load(here("data","tidy","CA","dat_raw.RData"))

# Check summary of variables
summary(dat_raw$pm2.5_epa) # EPA
summary(dat_raw$pm2.5_cf1_a) # PA channel A
summary(dat_raw$pm2.5_cf1_b) # PA channel B

dat_raw %>% select(pm2.5_epa:hum) %>% 
        skim()

# Remove temp < 0
dat <- dat_raw %>% 
        filter(temp >= 0) %>% 
        mutate(dist = as.numeric(dist),
               temp_c = (temp-32)/1.8,
               pm2.5_cf1_m = (pm2.5_cf1_a+pm2.5_cf1_b)/2)

save(dat, file = here("data","tidy","CA","dat.RData"))
##################1.3 Data matching and wrangling END##################
############################END############################

##################1.4 Plot maps##################
### California boundaries
# left=-124.6
# bottom=32.2
# right=-114
# top=42.2

### California map
camap = ggmap(get_stamenmap(bbox=c(left=-124.6,bottom=32.2,right=-114,top=42.2),
        source="stamen", maptype="terrain-background", crop=FALSE, zoom = 10))
# save(camap, file = here("data","plot","CA","camap10.RData"))

# load(here("data","plot","CA","camap10.RData"))
# load(here("data","tidy","CA","loc_both.RData"))

map_sites <- camap + 
        geom_jitter(aes(lon, lat, fill = group), data = loc_both, 
                    col="black",stroke=0.1,shape=21,alpha=0.9,size=8,
                    width = 0.05, height = 0.05)+
        labs(fill=expression(PM[2.5]*" Monitors"),
             x="Longitude",
             y="Latitude")+
        scale_x_continuous(limits = c(-123.8, -114.2), expand = c(0, 0)) +
        scale_y_continuous(limits = c(32.15, 42.04), expand = c(0, 0)) +
        theme(legend.position = "bottom",
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 18))


##### (1) CA Map with EPA and PA sites
# Create facet_grid map for monitors
# load(here("data","plot","CA","camap10.RData"))
# load(here("data","tidy","CA","loc_both.RData"))

line_states <- map_data("state")
line_ca_s <- line_states %>% 
        filter(region == "california") %>% 
        rename(grouping = group)

line_counties <- map_data("county")
line_ca_c <- line_counties %>% 
        filter(region == "california") %>% 
        rename(grouping = group)

ca_base <- ggplot() + 
        geom_polygon(data = line_ca_s, aes(x = long, y = lat, group = grouping),
                     color = "black", fill = "gray", alpha = 0.3) + 
        geom_polygon(data = line_ca_c, aes(x = long, y = lat, group = grouping),
                     color = "white", fill = NA) +
        geom_polygon(data = line_ca_s, aes(x = long, y = lat, group = grouping),
                     color = "black", fill = NA) + # get the state border back on top
        theme_nothing()

plot_map <- camap + 
        geom_polygon(data = line_ca_s, aes(x = long, y = lat, group = grouping),
                     color = "#F5F5F5", fill = NA, size = 1) + 
        geom_jitter(aes(lon, lat, fill = group), data = loc_both,
                    col="black",stroke=0.7,shape=21,alpha=1,size=5,
                    width = 0.1, height = 0.1)+
        annotate("rect", xmin = -122.5, xmax = -122.1, ymin = 37.7, ymax = 37.92, 
                 fill = "white", color = "#DC143C", size = 1.1, alpha = 0.8)+
        annotate("text", x = -123.5, y = 37.54, label = "San\nFrancisco", color = "#DC143C",
                 size = 4.9, fontface="bold.italic")+
        facet_grid(. ~ group)+
        labs(fill=expression(PM[2.5]*" Monitors"),
             x="Longitude",
             y="Latitude")+
        scale_x_continuous(limits = c(-124.6, -114), expand = c(0, 0)) +
        scale_y_continuous(limits = c(32.2, 42.2), expand = c(0, 0)) +
        theme(legend.position = "bottom",
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 18))

ggsave(filename = "plot_map.png",
       path = here("figures","map"),
       plot = plot_map,
       device = "png",
       width = 31,
       height = 21,
       units = "cm",
       dpi = 300
)

#####(2) CA Map with EPA and PA sites
## Zoom-in pairing map
# ### Bay area boundaries
# left=-123
# bottom=37
# right=-121
# top=38.5

### Bay area map
base_sf <- ggmap(get_stamenmap(bbox=c(left=-122.5,bottom=37.7,right=-121,top=37.95),
                               source="stamen", maptype="terrain-background", crop=FALSE, zoom = 12))
# save(base_sf, file = here("data","plot","CA","base_sf.RData"))

# load(here("data","plot","CA","base_sf.RData"))
# load(here("data","plot","CA","line_near_5km.RData"))
# load(here("data","tidy","CA","loc_both.RData"))
# load(here("data","tidy","CA","loc_both_full.RData"))

line_near_dt <- as_tibble(line_near_5km) %>% 
        unlist() %>% 
        as.vector(mode = "numeric") %>% 
        matrix(., ncol = 4, byrow = TRUE) %>% 
        as_tibble() %>% 
        mutate(grouping = 1:length(.$V1)) %>% 
        rename(lon1 = V1, lon2 = V2,
               lat1 = V3, lat2 = V4)
line_near_dt1 <- line_near_dt[,c("lon1","lat1","grouping")] %>% 
        rename(lon = lon1, lat = lat1)
line_near_dt2 <- line_near_dt[,c("lon2","lat2","grouping")] %>% 
        rename(lon = lon2, lat = lat2)
line_match <- bind_rows(line_near_dt1, line_near_dt2)


plot_map_sf <- base_sf + 
        geom_polygon(data = line_match, aes(x=lon, y=lat, group = grouping),
                     color = "black", size = 0.6, linetype = "dashed") + 
        geom_point(aes(lon, lat, fill = group), data = loc_both_full,
                    col="black",stroke=0.7,shape=21,alpha=1,size=3) +
        labs(fill=expression(PM[2.5]*" Monitors"),
             x="Longitude",
             y="Latitude")+
        scale_x_continuous(limits = c(-122.5, -122.1), expand = c(0, 0)) +
        scale_y_continuous(limits = c(37.7, 37.92), expand = c(0, 0)) +
        theme(legend.position = "bottom",
              legend.title = element_text(size = 20),
              legend.text = element_text(size = 18))

ggsave(filename = "plot_map_sf.png",
       path = here("figures","map"),
       plot = plot_map_sf,
       device = "png",
       width = 28,
       height = 21,
       units = "cm",
       dpi = 300
)
##################1.4 Plot maps END##################
############################END############################



##################2. Data analysis##################
##################2.1 Exploratory Analysis##################
load(here("data","tidy","CA","dat.RData"))

lm_ca <- lm(pm2.5_epa ~ pm2.5_cf1_m + hum,
            data = dat)
summary(lm_ca)

# Summary statistics
dat %>% select(pm2.5_epa:hum,dist) %>% 
        skim()

# Correlation matrix
corrmat <- dat %>%
        select(pm2.5_epa,pm2.5_cf1_a,pm2.5_cf1_b,temp_c,hum) %>% 
        cor(., method = "pearson")
corrmat

plot_corr <- dat %>%
        select(pm2.5_epa,pm2.5_cf1_a,temp_c,hum) %>%
        ggpairs(aes(alpha=0.001),# Alpha not useful here
                columnLabels = c("EPA PM2.5","PurpleAir PM2.5","Temperature","Humidity"),
                upper = list(continuous = wrap("cor", size = 12)))+
        theme_minimal(base_size = 30)
# dev.new()
# plot_corr

ggsave(filename = "plot_corr.png",
       path = here("figures","EDA"),
       plot = plot_corr,
       device = "png",
       width = 40,
       height = 30,
       units = "cm",
       dpi = 300
)

##################(1) PA PM2.5 A B channel##################
# PA PM2.5 A B channel
plot_ab <- dat %>%
        ggplot(aes(x = pm2.5_cf1_a, y = pm2.5_cf1_b))+
        geom_point(alpha = 1)+
        geom_smooth(method = lm, size = 1.5, color = "red")+
        labs(title = expression(paste("PurpleAir ", PM[2.5], " [",mu,"g/", "m"^3, "]")),
             x = "Channel A",
             y = "Channel B")+
        xlim(c(0,700)) + 
        ylim(c(0,700))

# dev.new()
# plot_ab

ggsave(filename = "plot_ab.png",
       path = here("figures","EDA"),
       plot = plot_ab,
       device = "png",
       width = 25,
       height = 26,
       units = "cm",
       dpi = 300
)

##################(2) Contour plot##################
# Contour plot
load(here("data","tidy","CA","dat.RData"))

plot_pe <- dat %>%
        ggplot(aes(x = pm2.5_epa, y = pm2.5_cf1_a))+
        geom_point(alpha = 0.005)+
        geom_density_2d_filled(contour_var = "ndensity", alpha = 0.9)+
        annotate("segment", x = 0, y = 0, xend = 40, yend = 40, alpha = 0.7,
                 color = "blue", size = 2, linetype = "dashed",)+
        stat_smooth(geom='line', method = "lm", alpha=0.7, se=FALSE, color = "red", size =1.5)+
        labs(#title = (),
             x = expression(paste("EPA ", PM[2.5], " [",mu,"g/", "m"^3, "]")),
             y = expression(paste("PurpleAir ", PM[2.5], " [",mu,"g/", "m"^3, "]")),
             fill = "Density") +
        xlim(c(-0.4,40.4)) + 
        ylim(c(-1.6,81.6)) +
        theme(legend.title = element_text(size = 18),
              legend.text = element_text(size = 14),
              legend.key.size = unit(1.2, "cm"))

# dev.new()
# plot_pe

ggsave(filename = "plot_pe.png",
       path = here("figures","EDA"),
       plot = plot_pe,
       device = "png",
       width = 28,
       height = 21,
       units = "cm",
       dpi = 300
)

##################(3) Contour plot: temp and hum##################
# Contour plot: temp and hum
load(here("data","tidy","CA","dat.RData"))
dat_vis <- dat %>% 
        mutate(month = month(time),
               month = factor(month, 
                              levels = c(10,11,12,1,2,3), 
                              labels = c("Oct","Nov","Dec","Jan","Feb","Mar")),
               ratio = pm2.5_cf1_a/pm2.5_epa)

## PM2.5 PA/EPA vs temp
plot_ratio_temp  <- dat_vis %>%
        filter(ratio <= 10) %>% 
        ggplot(aes(x = temp_c, y = ratio))+
        geom_point(alpha = 0.06)+
        geom_density_2d_filled(contour_var = "ndensity", alpha = 0.85)+
        stat_smooth(geom='line', alpha=0.7, se=FALSE, color = "red", size =2)+
        labs(x = expression("Temperature "*"["~degree*C*"]"),
             y = expression(PM[2.5]*" Ratio"),
             fill = "Density") +
        xlim(c(-0.4,40.4)) +
        ylim(c(-0.1,10.1)) +
        theme(axis.title.x = element_text(size=28),
              axis.title.y = element_text(size=28),
              legend.title = element_text(size = 22),
              legend.text = element_text(size = 22),
              legend.key.size = unit(1.5, "cm"))

# dev.new()
# plot_ratio_temp

# ggsave(filename = "plot_ratio_temp.png",
#        path = here("figures","EDA"),
#        plot = plot_ratio_temp,
#        device = "png",
#        width = 28,
#        height = 21,
#        units = "cm",
#        dpi = 300
# )

## PM2.5 PA/EPA vs humidity
plot_ratio_hum  <- dat_vis %>%
        filter(ratio <= 10) %>% 
        ggplot(aes(x = hum, y = ratio))+
        geom_point(alpha = 0.06)+
        geom_density_2d_filled(contour_var = "ndensity", alpha = 0.85)+
        stat_smooth(geom='line', alpha=0.7, se=FALSE, color = "red", size =2)+
        labs(x = expression("Humidity [%]"),
             y = expression(PM[2.5]*" Ratio"),
             fill = "Density") +
        xlim(c(-1,101)) +
        ylim(c(-0.1,10.1)) +
        theme(axis.title.x = element_text(size=28),
              axis.title.y = element_text(size=28),
              legend.title = element_text(size = 22),
              legend.text = element_text(size = 22),
              legend.key.size = unit(1.5, "cm"))

# dev.new()
# plot_ratio_hum

# ggsave(filename = "plot_ratio_hum.png",
#        path = here("figures","EDA"),
#        plot = plot_ratio_hum,
#        device = "png",
#        width = 28,
#        height = 21,
#        units = "cm",
#        dpi = 300
# )

ggsave(filename = "plot_ratio.png",
       path = here("figures","EDA"),
       plot = ggarrange(plot_ratio_temp, plot_ratio_hum, ncol = 2, nrow = 1,
                        common.legend = T, legend="right",
                        labels = c(" (a) Temperature","  (b) Humidity   "),
                        font.label = list(size = 28),
                        vjust = 1.5),
       device = "png",
       width = 56,
       height = 25,
       units = "cm",
       dpi = 300
)

## Box plot
load(here("data","tidy","CA","dat.RData"))
dat_vis <- dat %>% 
        mutate(month = month(time),
               month = factor(month, 
                              levels = c(10,11,12,1,2,3), 
                              labels = c("Oct","Nov","Dec","Jan","Feb","Mar")),
               ratio = pm2.5_cf1_a/pm2.5_epa)

box_epa <- dat_vis %>% 
        select(month, pm2.5_epa) %>% 
        rename(pm2.5 = pm2.5_epa) %>% 
        mutate(group = "EPA")
box_pa <- dat_vis %>% 
        select(month, pm2.5_cf1_a) %>% 
        rename(pm2.5 = pm2.5_cf1_a) %>% 
        mutate(group = "PA")
box_both <- bind_rows(box_epa, box_pa)

plot_box_group <- box_both %>% 
        ggplot(aes(x = month, y = pm2.5, fill = group)) +
        geom_boxplot(outlier.alpha = 0.008, position=position_dodge(0.9),
                     size = 0.8, alpha = 0.7) +
        stat_summary(aes(group = group), fun = mean, position = position_dodge(0.9),
                     geom = "point", fill = "red", shape = 24, size = 4, alpha = 0.9,
                     color = "black", stroke = 1) +
        labs(x = "Time (2019-2020)",
             y = expression(paste(PM[2.5], " [",mu,"g/", "m"^3, "]")),
             fill = "Group") +
        ylim(c(0,60)) +
        theme(axis.title.x = element_text(size=28),
              axis.title.y = element_text(size=28),
              axis.text = element_text(size = 24),
              legend.title = element_text(size = 24),
              legend.text = element_text(size = 24),
              legend.key.size = unit(1.5, "cm"),
              legend.position = "bottom")

ggsave(filename = "plot_box_group.png",
       path = here("figures","EDA"),
       plot = plot_box_group,
       device = "png",
       width = 38,
       height = 26,
       units = "cm",
       dpi = 300
)

# plot_box1 <- dat_vis %>% 
#         ggplot(aes(x = month, y = pm2.5_epa)) +
#         geom_boxplot(outlier.alpha = 0.008) +
#         stat_summary(fun = mean, geom = "point", color = "red", fill = "red", shape = 24, size = 4) +
#         labs(x = "Time (2019-2020)",
#              y = expression(paste(PM[2.5], " [",mu,"g/", "m"^3, "]"))) +
#         ylim(c(0,60))
# 
# plot_box2 <- dat_vis %>% 
#         ggplot(aes(x = month, y = pm2.5_cf1_a)) +
#         geom_boxplot(outlier.alpha = 0.008) +
#         stat_summary(fun = mean, geom = "point", color = "red", fill = "red", shape = 24, size = 4) +
#         labs(x = "Time (2019-2020)",
#              y = expression(paste(PM[2.5], " [",mu,"g/", "m"^3, "]"))) +
#         ylim(c(0,60))
# 
# ggsave(filename = "plot_box.png",
#        path = here("figures","EDA"),
#        plot = ggarrange(plot_box1, plot_box2, ncol = 2, nrow = 1, 
#                         labels = c("  (a) EPA       "," (b) PurpleAir  "),
#                         font.label = list(size = 24),
#                         vjust = 1.2),
#        device = "png",
#        width = 42,
#        height = 21,
#        units = "cm",
#        dpi = 300
# )

##################2.1 Exploratory Analysis END##################
############################END############################


##################2.2 Model fitting and evaluation##################
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

## Model fitting and prediction
met_all <- c()
### EPA model
## Time
pre_epa_time <- 0.52*(dat_time_test$pm2.5_cf1_a+dat_time_test$pm2.5_cf1_b)/2-0.085*dat_time_test$hum+5.71
pre_epa_time <- pre_epa_time %>% 
        cbind(pre = ., obs = dat_time_test$pm2.5_epa) %>% 
        as_tibble()
met_epa_time <- metrics(pre_epa_time, truth = obs, estimate = pre)
met_all <- rbind(met_all, met_epa_time)

## Location Farthest
pre_epa_loc <- 0.52*(dat_loc_far_test$pm2.5_cf1_a+dat_loc_far_test$pm2.5_cf1_b)/2-0.085*dat_loc_far_test$hum+5.71
pre_epa_loc <- pre_epa_loc %>% 
        cbind(pre = ., obs = dat_loc_far_test$pm2.5_epa) %>% 
        as_tibble()
met_epa_loc <- metrics(pre_epa_loc, truth = obs, estimate = pre)
met_all <- cbind(met_all, met_epa_loc)

## Location Random
pre_epa_loc_ran <- 0.52*(dat_loc_ran_test$pm2.5_cf1_a+dat_loc_ran_test$pm2.5_cf1_b)/2-0.085*dat_loc_ran_test$hum+5.71
pre_epa_loc_ran <- pre_epa_loc_ran %>% 
        cbind(pre = ., obs = dat_loc_ran_test$pm2.5_epa) %>% 
        as_tibble()
met_epa_loc_ran <- metrics(pre_epa_loc_ran, truth = obs, estimate = pre)
met_all <- cbind(met_all, met_epa_loc_ran)



### EPA-retrained MODEL
## Time 
mod_epare_time <- lm(pm2.5_epa ~ pm2.5_cf1_m + hum,
                     data = dat_time_train)
summary(mod_epare_time)
pre_epare_time <- predict(mod_epare_time,newdata = dat_time_test) %>% 
        cbind(pre = ., obs = dat_time_test$pm2.5_epa) %>% 
        as_tibble()
met_epare_time <- metrics(pre_epare_time, truth = obs, estimate = pre)
met_all <- cbind(met_all, met_epare_time)

## Location Farthest
mod_epare_loc <- lm(pm2.5_epa ~ pm2.5_cf1_m + temp + hum,
                    data = dat_loc_far_train)
summary(mod_epare_loc)
pre_epare_loc <- predict(mod_epare_loc,newdata = dat_loc_far_test) %>% 
        cbind(pre = ., obs = dat_loc_far_test$pm2.5_epa) %>% 
        as_tibble()
met_epare_loc <- metrics(pre_epare_loc, truth = obs, estimate = pre)
met_all <- cbind(met_all, met_epare_loc)

## Location Random
mod_epare_loc_ran <- lm(pm2.5_epa ~ pm2.5_cf1_m + temp + hum,
                        data = dat_loc_ran_train)
summary(mod_epare_loc_ran)
pre_epare_loc_ran <- predict(mod_epare_loc_ran,newdata = dat_loc_ran_test) %>% 
        cbind(pre = ., obs = dat_loc_ran_test$pm2.5_epa) %>% 
        as_tibble()
met_epare_loc_ran <- metrics(pre_epare_loc_ran, truth = obs, estimate = pre)
met_all <- cbind(met_all, met_epare_loc_ran)




### LINEAR MODEL
## Time 
mod_lm_time <- lm(pm2.5_epa ~ pm2.5_cf1_a + temp + hum,
                  data = dat_time_train)
summary(mod_lm_time)
pre_lm_time <- predict(mod_lm_time,newdata = dat_time_test) %>% 
        cbind(pre = ., obs = dat_time_test$pm2.5_epa) %>% 
        as_tibble()
met_lm_time <- metrics(pre_lm_time, truth = obs, estimate = pre)
met_all <- cbind(met_all, met_lm_time)

## Location Farthest
mod_lm_loc <- lm(pm2.5_epa ~ pm2.5_cf1_a + temp + hum,
                 data = dat_loc_far_train)
summary(mod_lm_loc)
pre_lm_loc <- predict(mod_lm_loc,newdata = dat_loc_far_test) %>% 
        cbind(pre = ., obs = dat_loc_far_test$pm2.5_epa) %>% 
        as_tibble()
met_lm_loc <- metrics(pre_lm_loc, truth = obs, estimate = pre)
met_all <- cbind(met_all, met_lm_loc)

## Location Random
mod_lm_loc_ran <- lm(pm2.5_epa ~ pm2.5_cf1_a + temp + hum,
                     data = dat_loc_ran_train)
summary(mod_lm_loc_ran)
pre_lm_loc_ran <- predict(mod_lm_loc_ran,newdata = dat_loc_ran_test) %>% 
        cbind(pre = ., obs = dat_loc_ran_test$pm2.5_epa) %>% 
        as_tibble()
met_lm_loc_ran <- metrics(pre_lm_loc_ran, truth = obs, estimate = pre)
met_all <- cbind(met_all, met_lm_loc_ran)




### LINEAR MODEL INTERACTION
## Time 
mod_lmint_time <- lm(pm2.5_epa ~ pm2.5_cf1_a * (temp + hum),
                     data = dat_time_train)
summary(mod_lmint_time)
pre_lmint_time <- predict(mod_lmint_time, newdata = dat_time_test) %>% 
        cbind(pre = ., obs = dat_time_test$pm2.5_epa) %>% 
        as_tibble()
met_lmint_time <- metrics(pre_lmint_time, truth = obs, estimate = pre)
met_all <- cbind(met_all, met_lmint_time)

## Location Farthest
mod_lmint_loc <- lm(pm2.5_epa ~ pm2.5_cf1_a * (temp + hum),
                    data = dat_loc_far_train)
summary(mod_lmint_loc)
pre_lmint_loc <- predict(mod_lmint_loc, newdata = dat_loc_far_test) %>% 
        cbind(pre = ., obs = dat_loc_far_test$pm2.5_epa) %>% 
        as_tibble()
met_lmint_loc <- metrics(pre_lmint_loc, truth = obs, estimate = pre)
met_all <- cbind(met_all, met_lmint_loc)

## Location Random
mod_lmint_loc_ran <- lm(pm2.5_epa ~ pm2.5_cf1_a * (temp + hum),
                        data = dat_loc_ran_train)
summary(mod_lmint_loc_ran)
pre_lmint_loc_ran <- predict(mod_lmint_loc_ran, newdata = dat_loc_ran_test) %>% 
        cbind(pre = ., obs = dat_loc_ran_test$pm2.5_epa) %>% 
        as_tibble()
met_lmint_loc_ran <- metrics(pre_lmint_loc_ran, truth = obs, estimate = pre)
met_all <- cbind(met_all, met_lmint_loc_ran)




### Gradient Boosting Method
## Time
mod_gbm_time <- gbm(
        formula = pm2.5_epa ~ pm2.5_cf1_a + temp + hum,
        data = dat_time_train,
        distribution = "gaussian",  # SSE loss function
        n.trees = 400,
        shrinkage = 0.1,
        interaction.depth = 3,
        n.minobsinnode = 10,
        cv.folds = 10
)
best <- which.min(mod_gbm_time$cv.error)
# get MSE and compute RMSE
sqrt(mod_gbm_time$cv.error[best])
# plot error curve
gbm.perf(mod_gbm_time, method = "cv")
summary(mod_gbm_time)

pre_gbm_time <- predict(mod_gbm_time, newdata = dat_time_test) %>% 
        cbind(pre = ., obs = dat_time_test$pm2.5_epa) %>% 
        as_tibble()
met_gbm_time <- metrics(pre_gbm_time, truth = obs, estimate = pre)
met_all <- cbind(met_all, met_gbm_time)

## Location Farthest
mod_gbm_loc <- gbm(
        formula = pm2.5_epa ~ pm2.5_cf1_a + temp + hum,
        data = dat_loc_far_train,
        distribution = "gaussian",  # SSE loss function
        n.trees = 400,
        shrinkage = 0.1,
        interaction.depth = 3,
        n.minobsinnode = 10,
        cv.folds = 10
)
best <- which.min(mod_gbm_loc$cv.error)
# get MSE and compute RMSE
sqrt(mod_gbm_loc$cv.error[best])
# plot error curve
gbm.perf(mod_gbm_loc, method = "cv")
summary(mod_gbm_loc)

pre_gbm_loc <- predict(mod_gbm_loc, newdata = dat_loc_far_test) %>% 
        cbind(pre = ., obs = dat_loc_far_test$pm2.5_epa) %>% 
        as_tibble()
met_gbm_loc <- metrics(pre_gbm_loc, truth = obs, estimate = pre)
met_all <- cbind(met_all, met_gbm_loc)

## Location Random
mod_gbm_loc_ran <- gbm(
        formula = pm2.5_epa ~ pm2.5_cf1_a + temp + hum,
        data = dat_loc_ran_train,
        distribution = "gaussian",  # SSE loss function
        n.trees = 400,
        shrinkage = 0.1,
        interaction.depth = 3,
        n.minobsinnode = 10,
        cv.folds = 10
)
best <- which.min(mod_gbm_loc_ran$cv.error)
# get MSE and compute RMSE
sqrt(mod_gbm_loc_ran$cv.error[best])
# plot error curve
gbm.perf(mod_gbm_loc_ran, method = "cv")
summary(mod_gbm_loc_ran)

pre_gbm_loc_ran <- predict(mod_gbm_loc_ran, newdata = dat_loc_ran_test) %>% 
        cbind(pre = ., obs = dat_loc_ran_test$pm2.5_epa) %>% 
        as_tibble()
met_gbm_loc_ran <- metrics(pre_gbm_loc_ran, truth = obs, estimate = pre)
met_all <- cbind(met_all, met_gbm_loc_ran)



### Random forest
## Time
X_train_time <- dat_time_train[, c("pm2.5_cf1_a","temp","hum")]
Y_train_time <- dat_time_train[, "pm2.5_epa"]

X_test_time  <- dat_time_test[, c("pm2.5_cf1_a","temp","hum")]
Y_test_time  <- dat_time_test[, "pm2.5_epa"]

p <- 3
n_tree <- 50

# mtry = p is basically just bagging, as above
mod_rf_time <- randomForest(X_train_time, Y_train_time, 
                            xtest = X_test_time, 
                            ytest = Y_test_time, 
                            mtry = p, 
                            ntree = n_tree)

pre_rf_time <- mod_rf_time$test$predicted %>% 
        cbind(pre = ., obs = dat_time_test$pm2.5_epa) %>% 
        as_tibble()
met_rf_time <- metrics(pre_rf_time, truth = obs, estimate = pre)
met_all <- cbind(met_all, met_rf_time)

# plot test RMSE
plot(1:n_tree, sqrt(mod_rf_time$test$mse), 
     col = "aquamarine4", 
     type = "l", 
     xlab = "Number of Trees",
     ylab = "Test RMSE")
legend("topright", c("m=3"), 
       col = c("aquamarine4"),
       cex = 1, lty = 1)

## Location Farthest
X_train_loc <- dat_loc_far_train[, c("pm2.5_cf1_a","temp","hum")]
Y_train_loc <- dat_loc_far_train[, "pm2.5_epa"]

X_test_loc  <- dat_loc_far_test[, c("pm2.5_cf1_a","temp","hum")]
Y_test_loc  <- dat_loc_far_test[, "pm2.5_epa"]

p <- 3
n_tree <- 50

# mtry = p is basically just bagging, as above
mod_rf_loc <- randomForest(X_train_loc, Y_train_loc, 
                           xtest = X_test_loc, 
                           ytest = Y_test_loc, 
                           mtry = p, 
                           ntree = n_tree)

pre_rf_loc <- mod_rf_loc$test$predicted %>% 
        cbind(pre = ., obs = dat_loc_far_test$pm2.5_epa) %>% 
        as_tibble()
met_rf_loc <- metrics(pre_rf_loc, truth = obs, estimate = pre)
met_all <- cbind(met_all, met_rf_loc)

# plot test RMSE
plot(1:n_tree, sqrt(mod_rf_loc$test$mse), 
     col = "aquamarine4", 
     type = "l", 
     xlab = "Number of Trees",
     ylab = "Test RMSE")
legend("topright", c("m=3"), 
       col = c("aquamarine4"),
       cex = 1, lty = 1)

## Location Random
X_train_loc_ran <- dat_loc_ran_train[, c("pm2.5_cf1_a","temp","hum")]
Y_train_loc_ran <- dat_loc_ran_train[, "pm2.5_epa"]

X_test_loc_ran  <- dat_loc_ran_test[, c("pm2.5_cf1_a","temp","hum")]
Y_test_loc_ran  <- dat_loc_ran_test[, "pm2.5_epa"]

p <- 3
n_tree <- 50

# mtry = p is basically just bagging, as above
mod_rf_loc_ran <- randomForest(X_train_loc_ran, Y_train_loc_ran, 
                               xtest = X_test_loc_ran, 
                               ytest = Y_test_loc_ran, 
                               mtry = p, 
                               ntree = n_tree)

pre_rf_loc_ran <- mod_rf_loc_ran$test$predicted %>% 
        cbind(pre = ., obs = dat_loc_ran_test$pm2.5_epa) %>% 
        as_tibble()
met_rf_loc_ran <- metrics(pre_rf_loc_ran, truth = obs, estimate = pre)
met_all <- cbind(met_all, met_rf_loc_ran)

# plot test RMSE
plot(1:n_tree, sqrt(mod_rf_loc_ran$test$mse), 
     col = "aquamarine4", 
     type = "l", 
     xlab = "Number of Trees",
     ylab = "Test RMSE")
legend("topright", c("m=3"), 
       col = c("aquamarine4"),
       cex = 1, lty = 1)

write.csv(round(met_all[,seq(from = 3, to = length(met_all[1,]), by = 3)],2), here("results", "met_all_fit.csv"))


save(pre_epa_time,  file = here("data", "model", "pre_epa_time.RData"))
save(mod_epare_time,  file = here("data", "model", "mod_epare_time.RData"))
save(pre_epare_time,  file = here("data", "model", "pre_epare_time.RData"))
save(mod_lm_time,  file = here("data", "model", "mod_lm_time.RData"))
save(pre_lm_time,  file = here("data", "model", "pre_lm_time.RData"))
save(mod_lmint_time,  file = here("data", "model", "mod_lmint_time.RData"))
save(pre_lmint_time,  file = here("data", "model", "pre_lmint_time.RData"))
save(mod_gbm_time,  file = here("data", "model", "mod_gbm_time.RData"))
save(pre_gbm_time,  file = here("data", "model", "pre_gbm_time.RData"))
save(mod_rf_time,  file = here("data", "model", "mod_rf_time.RData"))
save(pre_rf_time,  file = here("data", "model", "pre_rf_time.RData"))

save(pre_epa_loc,  file = here("data", "model", "pre_epa_loc.RData"))
save(mod_epare_loc,  file = here("data", "model", "mod_epare_loc.RData"))
save(pre_epare_loc,  file = here("data", "model", "pre_epare_loc.RData"))
save(mod_lm_loc,  file = here("data", "model", "mod_lm_loc.RData"))
save(pre_lm_loc,  file = here("data", "model", "pre_lm_loc.RData"))
save(mod_lmint_loc,  file = here("data", "model", "mod_lmint_loc.RData"))
save(pre_lmint_loc,  file = here("data", "model", "pre_lmint_loc.RData"))
save(mod_gbm_loc,  file = here("data", "model", "mod_gbm_loc.RData"))
save(pre_gbm_loc,  file = here("data", "model", "pre_gbm_loc.RData"))
save(mod_rf_loc,  file = here("data", "model", "mod_rf_loc.RData"))
save(pre_rf_loc,  file = here("data", "model", "pre_rf_loc.RData"))

save(pre_epa_loc_ran,  file = here("data", "model", "pre_epa_loc_ran.RData"))
save(mod_epare_loc_ran,  file = here("data", "model", "mod_epare_loc_ran.RData"))
save(pre_epare_loc_ran,  file = here("data", "model", "pre_epare_loc_ran.RData"))
save(mod_lm_loc_ran,  file = here("data", "model", "mod_lm_loc_ran.RData"))
save(pre_lm_loc_ran,  file = here("data", "model", "pre_lm_loc_ran.RData"))
save(mod_lmint_loc_ran,  file = here("data", "model", "mod_lmint_loc_ran.RData"))
save(pre_lmint_loc_ran,  file = here("data", "model", "pre_lmint_loc_ran.RData"))
save(mod_gbm_loc_ran,  file = here("data", "model", "mod_gbm_loc_ran.RData"))
save(pre_gbm_loc_ran,  file = here("data", "model", "pre_gbm_loc_ran.RData"))
save(mod_rf_loc_ran,  file = here("data", "model", "mod_rf_loc_ran.RData"))
save(pre_rf_loc_ran,  file = here("data", "model", "pre_rf_loc_ran.RData"))


##################(1) Time Series Plot##################
# Time series
load(here("data","tidy","CA","dat.RData"))
load(here("data","model","mod_gbm_time.RData"))
# dat_vis <- dat %>%
#         mutate(month = month(time),
#                month = factor(month,
#                               levels = c(10,11,12,1,2,3),
#                               labels = c("Oct","Nov","Dec","Jan","Feb","Mar")))
dat_vis <- dat %>% 
        mutate(month = month(time),
               month = factor(month, 
                              levels = c(10,11,12,1,2,3), 
                              labels = c("Oct","Nov","Dec","Jan","Feb","Mar")),
               ratio = pm2.5_cf1_a/pm2.5_epa)
dat_vis_ts <- dat_vis %>%
        mutate(pm2.5_gbm = predict(mod_gbm_time, newdata = dat_vis),
               pm2.5_pa = pm2.5_cf1_a)

# save(dat_vis_ts, file = here("data","plot","CA","dat_vis_ts.RData"))
# load(here("data","plot","CA","dat_vis_ts.RData"))

plot_ts_epa <- dat_vis_ts %>% 
        ggplot() +
        geom_line(aes(x = time, y = pm2.5_epa, group = lat_epa), 
                  size = 0.01, color = "steelblue", alpha = 0.2) +
        stat_summary(aes(x = time, y = pm2.5_epa), fun = mean, geom = "line", 
                     lwd = 1, colour = "red", alpha = 0.8) +
        labs(x = "Time",
             # y = expression(paste(PM[2.5], " [",mu,"g/", "m"^3, "]"))) +
             y = expression("PM"[2.5]*" ["*mu*"g/m"^3*"]"))+
        ylim(c(0,100)) +
        scale_x_datetime(date_breaks = "1 month", date_labels = "%Y %b")

# dev.new()
# plot_ts_epa

plot_ts_pa <- dat_vis_ts %>% 
        ggplot() +
        geom_line(aes(x = time, y = pm2.5_pa, group = lat_pa), 
                  size = 0.01, color = "steelblue", alpha = 0.1) +
        stat_summary(aes(x = time, y = pm2.5_pa), fun = mean, geom = "line", 
                     lwd = 1, colour = "red", alpha = 0.9) +
        labs(x = "Time",
             # y = expression(paste(PM[2.5], " [",mu,"g/", "m"^3, "]"))) +
             y = expression("PM"[2.5]*" ["*mu*"g/m"^3*"]"))+
        ylim(c(0,100)) +
        scale_x_datetime(date_breaks = "1 month", date_labels = "%Y %b")

# dev.new()
# plot_ts_pa

plot_ts_gbm <- dat_vis_ts %>% 
        ggplot() +
        geom_line(aes(x = time, y = pm2.5_gbm, group = lat_pa), 
                  size = 0.01, color = "steelblue", alpha = 0.1) +
        stat_summary(aes(x = time, y = pm2.5_gbm), fun = mean, geom = "line", 
                     lwd = 1, colour = "red", alpha = 0.9) +
        labs(x = "Time",
             # y = expression(paste(PM[2.5], " [",mu,"g/", "m"^3, "]"))) +
             y = expression("PM"[2.5]*" ["*mu*"g/m"^3*"]"))+
        ylim(c(0,100)) +
        scale_x_datetime(date_breaks = "1 month", date_labels = "%Y %b")

# dev.new()
# plot_ts_gbm


ggsave(filename = "plot_ts.png",
       path = here("figures","EDA"),
       plot = ggarrange(plot_ts_epa, plot_ts_pa, plot_ts_gbm, ncol = 1, nrow = 3,
                        # labels = c(expression(paste("(a) EPA ", PM[2.5])),expression(paste("(b) PurpleAir", PM[2.5]))),
                        labels = c("  (a) EPA                 ","  (b) PurpleAir         ","(c) Gradient Boosting"),
                        font.label = list(size = 28),
                        vjust = 1.2,
                        # hjust = -1,
                        align = "hv"
                        ),
       device = "png",
       width = 45,
       height = 50,
       units = "cm",
       dpi = 300
)

##################2.2 Model fitting and evaluation END##################
############################END############################



##################2.3 Simulation##################
##################2.3.1 EPA Formula##################
## 1. EPA Formula based
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

# Set simulation parameters
nsim <- 20
nsim_train <- 100000
nsim_test <- 20000
set.seed(123)
ind_time_train <- sample(1:length(dat_time_train$time), nsim_train, replace = F)

ind_loc_train <- sample(1:length(dat_loc_far_train$time), nsim_train, replace = F)
 

# Create simulation model specific data set

dat_sim_epa_time_train <- dat_time_train %>%
        mutate(true_sim = 0.52*(dat_time_train$pm2.5_cf1_a+dat_time_train$pm2.5_cf1_b)/2-0.085*dat_time_train$hum+5.71)

dat_sim_epa_time_test <- dat_time_test %>% 
        mutate(true_sim = 0.52*(dat_time_test$pm2.5_cf1_a+dat_time_test$pm2.5_cf1_b)/2-0.085*dat_time_test$hum+5.71)

dat_sim_epa_loc_train <- dat_loc_far_train %>%
        mutate(true_sim = 0.52*(dat_loc_far_train$pm2.5_cf1_a+dat_loc_far_train$pm2.5_cf1_b)/2-0.085*dat_loc_far_train$hum+5.71)

dat_sim_epa_loc_test <- dat_loc_far_test %>% 
        mutate(true_sim = 0.52*(dat_loc_far_test$pm2.5_cf1_a+dat_loc_far_test$pm2.5_cf1_b)/2-0.085*dat_loc_far_test$hum+5.71)

met_sim_epa <- vector(mode = "list", length = nsim)

for(i in 1:nsim){
        print(i)
        # set.seed(i)
        e_train <- rnorm(nsim_train)
        # set.seed(100+i)
        e_test <- rnorm(nsim_test)
        
        sim_time_train <- dat_sim_epa_time_train[ind_time_train,] %>% 
                mutate(pm2.5_sim = true_sim + e_train)
        sim_time_test <- dat_sim_epa_time_test[-ind_time_train,]
        sim_time_test <- sim_time_test[1:nsim_test,] %>% 
                mutate(pm2.5_sim = true_sim + e_test)
        
        sim_loc_train <- dat_sim_epa_loc_train[ind_loc_train,] %>% 
                mutate(pm2.5_sim = true_sim + e_train)
        sim_loc_test <- dat_sim_epa_loc_test[-ind_loc_train,]
        sim_loc_test <- sim_loc_test[1:nsim_test,] %>% 
                mutate(pm2.5_sim = true_sim + e_test)
        
        ## Model fitting and prediction
        ### EPA model
        ## Time
        pre_epa_time <- 0.52*(sim_time_test$pm2.5_cf1_a+sim_time_test$pm2.5_cf1_b)/2-0.085*sim_time_test$hum+5.71
        pre_epa_time <- pre_epa_time %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_time <- metrics(pre_epa_time, truth = obs, estimate = pre)
        met_sim_epa[[i]] <- rbind(met_sim_epa[[i]], met_epa_time)
        
        ## Location Farthest
        pre_epa_loc <- 0.52*(sim_loc_test$pm2.5_cf1_a+sim_loc_test$pm2.5_cf1_b)/2-0.085*sim_loc_test$hum+5.71
        pre_epa_loc <- pre_epa_loc %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_loc <- metrics(pre_epa_loc, truth = obs, estimate = pre)
        met_sim_epa[[i]] <- rbind(met_sim_epa[[i]], met_epa_loc)
        
        
        ### EPA-retrained MODEL
        ## Time 
        mod_epa_time <- lm(pm2.5_sim ~ pm2.5_cf1_m + hum,
                           data = sim_time_train)
        # summary(mod_epa_time)
        pre_epa_time <- predict(mod_epa_time,newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_time <- metrics(pre_epa_time, truth = obs, estimate = pre)
        met_sim_epa[[i]] <- rbind(met_sim_epa[[i]], met_epa_time)
        
        ## Location Farthest
        mod_epa_loc <- lm(pm2.5_sim ~ pm2.5_cf1_m + temp + hum,
                          data = sim_loc_train)
        # summary(mod_epa_loc)
        pre_epa_loc <- predict(mod_epa_loc,newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_loc <- metrics(pre_epa_loc, truth = obs, estimate = pre)
        met_sim_epa[[i]] <- rbind(met_sim_epa[[i]], met_epa_loc)
        
        
        ### LINEAR MODEL
        ## Time 
        mod_epa_time <- lm(pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                           data = sim_time_train)
        # summary(mod_epa_time)
        pre_epa_time <- predict(mod_epa_time,newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_time <- metrics(pre_epa_time, truth = obs, estimate = pre)
        met_sim_epa[[i]] <- rbind(met_sim_epa[[i]], met_epa_time)
        
        ## Location Farthest
        mod_epa_loc <- lm(pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                          data = sim_loc_train)
        # summary(mod_epa_loc)
        pre_epa_loc <- predict(mod_epa_loc,newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_loc <- metrics(pre_epa_loc, truth = obs, estimate = pre)
        met_sim_epa[[i]] <- rbind(met_sim_epa[[i]], met_epa_loc)
        
        
        ### LINEAR MODEL INTERACTION
        ## Time 
        mod_epa_time <- lm(pm2.5_sim ~ pm2.5_cf1_a * (temp + hum),
                           data = sim_time_train)
        # summary(mod_epa_time)
        pre_epa_time <- predict(mod_epa_time, newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_time <- metrics(pre_epa_time, truth = obs, estimate = pre)
        met_sim_epa[[i]] <- rbind(met_sim_epa[[i]], met_epa_time)
        
        ## Location Farthest
        mod_epa_loc <- lm(pm2.5_sim ~ pm2.5_cf1_a * (temp + hum),
                          data = sim_loc_train)
        # summary(mod_epa_loc)
        pre_epa_loc <- predict(mod_epa_loc, newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_loc <- metrics(pre_epa_loc, truth = obs, estimate = pre)
        met_sim_epa[[i]] <- rbind(met_sim_epa[[i]], met_epa_loc)
        
        
        ### Gradient Boosting
        ## Time
        mod_epa_time <- gbm(
                formula = pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                data = sim_time_train,
                distribution = "gaussian",  # SSE loss function
                n.trees = 400,
                shrinkage = 0.1,
                interaction.depth = 3,
                n.minobsinnode = 10,
                cv.folds = 10
        )
        pre_epa_time <- predict(mod_epa_time, newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_time <- metrics(pre_epa_time, truth = obs, estimate = pre)
        met_sim_epa[[i]] <- rbind(met_sim_epa[[i]], met_epa_time)
        
        ## Location Farthest
        mod_epa_loc <- gbm(
                formula = pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                data = sim_loc_train,
                distribution = "gaussian",  # SSE loss function
                n.trees = 400,
                shrinkage = 0.1,
                interaction.depth = 3,
                n.minobsinnode = 10,
                cv.folds = 10
        )
        pre_epa_loc <- predict(mod_epa_loc, newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_loc <- metrics(pre_epa_loc, truth = obs, estimate = pre)
        met_sim_epa[[i]] <- rbind(met_sim_epa[[i]], met_epa_loc)
        
        ### Random forest
        ## Time
        X_train_time <- sim_time_train[, c("pm2.5_cf1_a","temp","hum")]
        Y_train_time <- sim_time_train[, "pm2.5_sim"]
        
        X_test_time  <- sim_time_test[, c("pm2.5_cf1_a","temp","hum")]
        Y_test_time  <- sim_time_test[, "pm2.5_sim"]
        
        p <- 3
        n_tree <- 30
        
        # mtry = p is basically just bagging, as above
        mod_epa_time <- randomForest(X_train_time, Y_train_time, 
                                     xtest = X_test_time, 
                                     ytest = Y_test_time, 
                                     mtry = p, 
                                     ntree = n_tree)
        
        pre_epa_time <- mod_epa_time$test$predicted %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_time <- metrics(pre_epa_time, truth = obs, estimate = pre)
        met_sim_epa[[i]] <- rbind(met_sim_epa[[i]], met_epa_time)
        
        
        ## Location Farthest
        X_train_loc <- sim_loc_train[, c("pm2.5_cf1_a","temp","hum")]
        Y_train_loc <- sim_loc_train[, "pm2.5_sim"]
        
        X_test_loc  <- sim_loc_test[, c("pm2.5_cf1_a","temp","hum")]
        Y_test_loc  <- sim_loc_test[, "pm2.5_sim"]
        
        p <- 3
        n_tree <- 30
        
        # mtry = p is basically just bagging, as above
        mod_epa_loc <- randomForest(X_train_loc, Y_train_loc, 
                                    xtest = X_test_loc, 
                                    ytest = Y_test_loc, 
                                    mtry = p, 
                                    ntree = n_tree)
        
        pre_epa_loc <- mod_epa_loc$test$predicted %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_loc <- metrics(pre_epa_loc, truth = obs, estimate = pre)
        met_sim_epa[[i]] <- rbind(met_sim_epa[[i]], met_epa_loc)
}

met_sim_epa

met_epa_matrix  <- c()

for (i in 1:length(met_sim_epa)) {
        met_epa_matrix <- bind_cols(met_epa_matrix, met_sim_epa[[i]][3])
}

met_epa_mean <- rowMeans(met_epa_matrix)
met_epa_final <- matrix(met_epa_mean, ncol = 6, byrow = T)

# write.csv(met_epa_matrix, here("results", "met_epa_matrix.csv"))
write.csv(round(met_epa_final,2), here("results", "met_epa_final.csv"))



##################2.3.2 EPA Retrained##################
## 1. EPA Retrained based
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

# Set simulation parameters
nsim <- 20
nsim_train <- 100000
nsim_test <- 20000
set.seed(123)
ind_time_train <- sample(1:length(dat_time_train$time), nsim_train, replace = F)

ind_loc_train <- sample(1:length(dat_loc_far_train$time), nsim_train, replace = F)


# Create simulation model specific data set
load(here("data","model","mod_epare_time.RData"))
load(here("data","model","pre_epare_time.RData"))
load(here("data","model","mod_epare_loc.RData"))
load(here("data","model","pre_epare_loc.RData"))

dat_sim_epare_time_train <- dat_time_train %>% 
        mutate(true_sim = mod_epare_time$fitted.values)

dat_sim_epare_time_test <- dat_time_test %>% 
        mutate(true_sim = predict(mod_epare_time, newdata = dat_time_test))

dat_sim_epare_loc_train <- dat_loc_far_train %>% 
        mutate(true_sim = mod_epare_loc$fitted.values)

dat_sim_epare_loc_test <- dat_loc_far_test %>% 
        mutate(true_sim = predict(mod_epare_loc, newdata = dat_loc_far_test))

met_sim_epare <- vector(mode = "list", length = nsim)

for(i in 1:nsim){
        print(i)
        # set.seed(i)
        e_train <- rnorm(nsim_train)
        # set.seed(100+i)
        e_test <- rnorm(nsim_test)
        
        sim_time_train <- dat_sim_epare_time_train[ind_time_train,] %>% 
                mutate(pm2.5_sim = true_sim + e_train)
        sim_time_test <- dat_sim_epare_time_test[-ind_time_train,] 
        sim_time_test <- sim_time_test[1:nsim_test,] %>% 
                mutate(pm2.5_sim = true_sim + e_test)
        
        sim_loc_train <- dat_sim_epare_loc_train[ind_loc_train,] %>% 
                mutate(pm2.5_sim = true_sim + e_train)
        sim_loc_test <- dat_sim_epare_loc_test[-ind_loc_train,] 
        sim_loc_test <- sim_loc_test[1:nsim_test,] %>% 
                mutate(pm2.5_sim = true_sim + e_test)
        
        ## Model fitting and prediction
        ### EPA model
        ## Time
        pre_epa_time <- 0.52*(sim_time_test$pm2.5_cf1_a+sim_time_test$pm2.5_cf1_b)/2-0.085*sim_time_test$hum+5.71
        pre_epa_time <- pre_epa_time %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_time <- metrics(pre_epa_time, truth = obs, estimate = pre)
        met_sim_epare[[i]] <- rbind(met_sim_epare[[i]], met_epa_time)
        
        ## Location Farthest
        pre_epa_loc <- 0.52*(sim_loc_test$pm2.5_cf1_a+sim_loc_test$pm2.5_cf1_b)/2-0.085*sim_loc_test$hum+5.71
        pre_epa_loc <- pre_epa_loc %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_loc <- metrics(pre_epa_loc, truth = obs, estimate = pre)
        met_sim_epare[[i]] <- rbind(met_sim_epare[[i]], met_epa_loc)
        
        
        ### EPA-retrained MODEL
        ## Time 
        mod_epare_time <- lm(pm2.5_sim ~ pm2.5_cf1_m + hum,
                             data = sim_time_train)
        # summary(mod_epare_time)
        pre_epare_time <- predict(mod_epare_time,newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_epare_time <- metrics(pre_epare_time, truth = obs, estimate = pre)
        met_sim_epare[[i]] <- rbind(met_sim_epare[[i]], met_epare_time)
        
        ## Location Farthest
        mod_epare_loc <- lm(pm2.5_sim ~ pm2.5_cf1_m + temp + hum,
                            data = sim_loc_train)
        # summary(mod_epare_loc)
        pre_epare_loc <- predict(mod_epare_loc,newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_epare_loc <- metrics(pre_epare_loc, truth = obs, estimate = pre)
        met_sim_epare[[i]] <- rbind(met_sim_epare[[i]], met_epare_loc)
        
        
        ### LINEAR MODEL
        ## Time 
        mod_lm_time <- lm(pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                          data = sim_time_train)
        # summary(mod_lm_time)
        pre_lm_time <- predict(mod_lm_time,newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_lm_time <- metrics(pre_lm_time, truth = obs, estimate = pre)
        met_sim_epare[[i]] <- rbind(met_sim_epare[[i]], met_lm_time)
        
        ## Location Farthest
        mod_lm_loc <- lm(pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                         data = sim_loc_train)
        # summary(mod_lm_loc)
        pre_lm_loc <- predict(mod_lm_loc,newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_lm_loc <- metrics(pre_lm_loc, truth = obs, estimate = pre)
        met_sim_epare[[i]] <- rbind(met_sim_epare[[i]], met_lm_loc)
        
        
        ### LINEAR MODEL INTERACTION
        ## Time 
        mod_epare_time <- lm(pm2.5_sim ~ pm2.5_cf1_a * (temp + hum),
                             data = sim_time_train)
        # summary(mod_epare_time)
        pre_epare_time <- predict(mod_epare_time, newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_epare_time <- metrics(pre_epare_time, truth = obs, estimate = pre)
        met_sim_epare[[i]] <- rbind(met_sim_epare[[i]], met_epare_time)
        
        ## Location Farthest
        mod_epare_loc <- lm(pm2.5_sim ~ pm2.5_cf1_a * (temp + hum),
                            data = sim_loc_train)
        # summary(mod_epare_loc)
        pre_epare_loc <- predict(mod_epare_loc, newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_epare_loc <- metrics(pre_epare_loc, truth = obs, estimate = pre)
        met_sim_epare[[i]] <- rbind(met_sim_epare[[i]], met_epare_loc)
        
        
        ### Gradient Boosting Method
        ## Time
        mod_gbm_time <- gbm(
                formula = pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                data = sim_time_train,
                distribution = "gaussian",  # SSE loss function
                n.trees = 400,
                shrinkage = 0.1,
                interaction.depth = 3,
                n.minobsinnode = 10,
                cv.folds = 10
        )
        pre_gbm_time <- predict(mod_gbm_time, newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_gbm_time <- metrics(pre_gbm_time, truth = obs, estimate = pre)
        met_sim_epare[[i]] <- rbind(met_sim_epare[[i]], met_gbm_time)
        
        ## Location Farthest
        mod_gbm_loc <- gbm(
                formula = pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                data = sim_loc_train,
                distribution = "gaussian",  # SSE loss function
                n.trees = 400,
                shrinkage = 0.1,
                interaction.depth = 3,
                n.minobsinnode = 10,
                cv.folds = 10
        )
        pre_gbm_loc <- predict(mod_gbm_loc, newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_gbm_loc <- metrics(pre_gbm_loc, truth = obs, estimate = pre)
        met_sim_epare[[i]] <- rbind(met_sim_epare[[i]], met_gbm_loc)
        
        ### Random forest
        ## Time
        X_train_time <- sim_time_train[, c("pm2.5_cf1_a","temp","hum")]
        Y_train_time <- sim_time_train[, "pm2.5_sim"]
        
        X_test_time  <- sim_time_test[, c("pm2.5_cf1_a","temp","hum")]
        Y_test_time  <- sim_time_test[, "pm2.5_sim"]
        
        p <- 3
        n_tree <- 30
        
        # mtry = p is basically just bagging, as above
        mod_epare_time <- randomForest(X_train_time, Y_train_time, 
                                       xtest = X_test_time, 
                                       ytest = Y_test_time, 
                                       mtry = p, 
                                       ntree = n_tree)
        
        pre_epare_time <- mod_epare_time$test$predicted %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_epare_time <- metrics(pre_epare_time, truth = obs, estimate = pre)
        met_sim_epare[[i]] <- rbind(met_sim_epare[[i]], met_epare_time)
        
        
        ## Location Farthest
        X_train_loc <- sim_loc_train[, c("pm2.5_cf1_a","temp","hum")]
        Y_train_loc <- sim_loc_train[, "pm2.5_sim"]
        
        X_test_loc  <- sim_loc_test[, c("pm2.5_cf1_a","temp","hum")]
        Y_test_loc  <- sim_loc_test[, "pm2.5_sim"]
        
        p <- 3
        n_tree <- 30
        
        # mtry = p is basically just bagging, as above
        mod_epare_loc <- randomForest(X_train_loc, Y_train_loc, 
                                      xtest = X_test_loc, 
                                      ytest = Y_test_loc, 
                                      mtry = p, 
                                      ntree = n_tree)
        
        pre_epare_loc <- mod_epare_loc$test$predicted %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_epare_loc <- metrics(pre_epare_loc, truth = obs, estimate = pre)
        met_sim_epare[[i]] <- rbind(met_sim_epare[[i]], met_epare_loc)
}

met_sim_epare

met_epare_matrix  <- c()

for (i in 1:length(met_sim_epare)) {
        met_epare_matrix <- bind_cols(met_epare_matrix, met_sim_epare[[i]][3])
}

met_epare_mean <- rowMeans(met_epare_matrix)
met_epare_final <- matrix(met_epare_mean, ncol = 6, byrow = T)

# write.csv(met_epa_matrix, here("results", "met_epare_matrix.csv"))
write.csv(round(met_epare_final,2), here("results", "met_epare_final.csv"))


##################2.3.3 Linear Model##################
## 1. Linear Model based
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

# Set simulation parameters
nsim <- 20
nsim_train <- 100000
nsim_test <- 20000
set.seed(123)
ind_time_train <- sample(1:length(dat_time_train$time), nsim_train, replace = F)

ind_loc_train <- sample(1:length(dat_loc_far_train$time), nsim_train, replace = F)


# Create simulation model specific data set
load(here("data","model","mod_lm_time.RData"))
load(here("data","model","pre_lm_time.RData"))
load(here("data","model","mod_lm_loc.RData"))
load(here("data","model","pre_lm_loc.RData"))

dat_sim_lm_time_train <- dat_time_train %>% 
        mutate(true_sim = mod_lm_time$fitted.values)

dat_sim_lm_time_test <- dat_time_test %>% 
        mutate(true_sim = predict(mod_lm_time, newdata = dat_time_test))

dat_sim_lm_loc_train <- dat_loc_far_train %>% 
        mutate(true_sim = mod_lm_loc$fitted.values)

dat_sim_lm_loc_test <- dat_loc_far_test %>% 
        mutate(true_sim = predict(mod_lm_loc, newdata = dat_loc_far_test))

met_sim_lm <- vector(mode = "list", length = nsim)

for(i in 1:nsim){
        print(i)
        # set.seed(i)
        e_train <- rnorm(nsim_train)
        # set.seed(100+i)
        e_test <- rnorm(nsim_test)
        
        sim_time_train <- dat_sim_lm_time_train[ind_time_train,] %>% 
                mutate(pm2.5_sim = true_sim + e_train)
        sim_time_test <- dat_sim_lm_time_test[-ind_time_train,] 
        sim_time_test <- sim_time_test[1:nsim_test,] %>% 
                mutate(pm2.5_sim = true_sim + e_test)
        
        sim_loc_train <- dat_sim_lm_loc_train[ind_loc_train,] %>% 
                mutate(pm2.5_sim = true_sim + e_train)
        sim_loc_test <- dat_sim_lm_loc_test[-ind_loc_train,] 
        sim_loc_test <- sim_loc_test[1:nsim_test,] %>% 
                mutate(pm2.5_sim = true_sim + e_test)
        
        ## Model fitting and prediction
        ### EPA model
        ## Time
        pre_epa_time <- 0.52*(sim_time_test$pm2.5_cf1_a+sim_time_test$pm2.5_cf1_b)/2-0.085*sim_time_test$hum+5.71
        pre_epa_time <- pre_epa_time %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_time <- metrics(pre_epa_time, truth = obs, estimate = pre)
        met_sim_lm[[i]] <- rbind(met_sim_lm[[i]], met_epa_time)
        
        ## Location Farthest
        pre_epa_loc <- 0.52*(sim_loc_test$pm2.5_cf1_a+sim_loc_test$pm2.5_cf1_b)/2-0.085*sim_loc_test$hum+5.71
        pre_epa_loc <- pre_epa_loc %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_loc <- metrics(pre_epa_loc, truth = obs, estimate = pre)
        met_sim_lm[[i]] <- rbind(met_sim_lm[[i]], met_epa_loc)
        
        
        ### EPA-retrained MODEL
        ## Time 
        mod_lm_time <- lm(pm2.5_sim ~ pm2.5_cf1_m + hum,
                          data = sim_time_train)
        # summary(mod_lm_time)
        pre_lm_time <- predict(mod_lm_time,newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_lm_time <- metrics(pre_lm_time, truth = obs, estimate = pre)
        met_sim_lm[[i]] <- rbind(met_sim_lm[[i]], met_lm_time)
        
        ## Location Farthest
        mod_lm_loc <- lm(pm2.5_sim ~ pm2.5_cf1_m + temp + hum,
                         data = sim_loc_train)
        # summary(mod_lm_loc)
        pre_lm_loc <- predict(mod_lm_loc,newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_lm_loc <- metrics(pre_lm_loc, truth = obs, estimate = pre)
        met_sim_lm[[i]] <- rbind(met_sim_lm[[i]], met_lm_loc)
        
        
        ### LINEAR MODEL
        ## Time 
        mod_lm_time <- lm(pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                          data = sim_time_train)
        # summary(mod_lm_time)
        pre_lm_time <- predict(mod_lm_time,newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_lm_time <- metrics(pre_lm_time, truth = obs, estimate = pre)
        met_sim_lm[[i]] <- rbind(met_sim_lm[[i]], met_lm_time)
        
        ## Location Farthest
        mod_lm_loc <- lm(pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                         data = sim_loc_train)
        # summary(mod_lm_loc)
        pre_lm_loc <- predict(mod_lm_loc,newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_lm_loc <- metrics(pre_lm_loc, truth = obs, estimate = pre)
        met_sim_lm[[i]] <- rbind(met_sim_lm[[i]], met_lm_loc)
        
        
        ### LINEAR MODEL INTERACTION
        ## Time 
        mod_lm_time <- lm(pm2.5_sim ~ pm2.5_cf1_a * (temp + hum),
                          data = sim_time_train)
        # summary(mod_lm_time)
        pre_lm_time <- predict(mod_lm_time, newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_lm_time <- metrics(pre_lm_time, truth = obs, estimate = pre)
        met_sim_lm[[i]] <- rbind(met_sim_lm[[i]], met_lm_time)
        
        ## Location Farthest
        mod_lm_loc <- lm(pm2.5_sim ~ pm2.5_cf1_a * (temp + hum),
                         data = sim_loc_train)
        # summary(mod_lm_loc)
        pre_lm_loc <- predict(mod_lm_loc, newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_lm_loc <- metrics(pre_lm_loc, truth = obs, estimate = pre)
        met_sim_lm[[i]] <- rbind(met_sim_lm[[i]], met_lm_loc)
        
        
        ### Gradient Boosting Method
        ## Time
        mod_gbm_time <- gbm(
                formula = pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                data = sim_time_train,
                distribution = "gaussian",  # SSE loss function
                n.trees = 400,
                shrinkage = 0.1,
                interaction.depth = 3,
                n.minobsinnode = 10,
                cv.folds = 10
        )
        pre_gbm_time <- predict(mod_gbm_time, newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_gbm_time <- metrics(pre_gbm_time, truth = obs, estimate = pre)
        met_sim_lm[[i]] <- rbind(met_sim_lm[[i]], met_gbm_time)
        
        ## Location Farthest
        mod_gbm_loc <- gbm(
                formula = pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                data = sim_loc_train,
                distribution = "gaussian",  # SSE loss function
                n.trees = 400,
                shrinkage = 0.1,
                interaction.depth = 3,
                n.minobsinnode = 10,
                cv.folds = 10
        )
        pre_gbm_loc <- predict(mod_gbm_loc, newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_gbm_loc <- metrics(pre_gbm_loc, truth = obs, estimate = pre)
        met_sim_lm[[i]] <- rbind(met_sim_lm[[i]], met_gbm_loc)
        
        ### Random forest
        ## Time
        X_train_time <- sim_time_train[, c("pm2.5_cf1_a","temp","hum")]
        Y_train_time <- sim_time_train[, "pm2.5_sim"]
        
        X_test_time  <- sim_time_test[, c("pm2.5_cf1_a","temp","hum")]
        Y_test_time  <- sim_time_test[, "pm2.5_sim"]
        
        p <- 3
        n_tree <- 30
        
        # mtry = p is basically just bagging, as above
        mod_lm_time <- randomForest(X_train_time, Y_train_time, 
                                    xtest = X_test_time, 
                                    ytest = Y_test_time, 
                                    mtry = p, 
                                    ntree = n_tree)
        
        pre_lm_time <- mod_lm_time$test$predicted %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_lm_time <- metrics(pre_lm_time, truth = obs, estimate = pre)
        met_sim_lm[[i]] <- rbind(met_sim_lm[[i]], met_lm_time)
        
        
        ## Location Farthest
        X_train_loc <- sim_loc_train[, c("pm2.5_cf1_a","temp","hum")]
        Y_train_loc <- sim_loc_train[, "pm2.5_sim"]
        
        X_test_loc  <- sim_loc_test[, c("pm2.5_cf1_a","temp","hum")]
        Y_test_loc  <- sim_loc_test[, "pm2.5_sim"]
        
        p <- 3
        n_tree <- 30
        
        # mtry = p is basically just bagging, as above
        mod_lm_loc <- randomForest(X_train_loc, Y_train_loc, 
                                   xtest = X_test_loc, 
                                   ytest = Y_test_loc, 
                                   mtry = p, 
                                   ntree = n_tree)
        
        pre_lm_loc <- mod_lm_loc$test$predicted %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_lm_loc <- metrics(pre_lm_loc, truth = obs, estimate = pre)
        met_sim_lm[[i]] <- rbind(met_sim_lm[[i]], met_lm_loc)
}

met_sim_lm

met_lm_matrix  <- c()

for (i in 1:length(met_sim_lm)) {
        met_lm_matrix <- bind_cols(met_lm_matrix, met_sim_lm[[i]][3])
}

met_lm_mean <- rowMeans(met_lm_matrix)
met_lm_final <- matrix(met_lm_mean, ncol = 6, byrow = T)

# write.csv(met_lm_matrix, here("results", "met_lm_matrix.csv"))
write.csv(round(met_lm_final,2), here("results", "met_lm_final.csv"))


##################2.3.4 Linear Interaction Model##################
## 1. Linear Interaction Model based
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

# Set simulation parameters
nsim <- 20
nsim_train <- 100000
nsim_test <- 20000
set.seed(123)
ind_time_train <- sample(1:length(dat_time_train$time), nsim_train, replace = F)

ind_loc_train <- sample(1:length(dat_loc_far_train$time), nsim_train, replace = F)


# Create simulation model specific data set
load(here("data","model","mod_lmint_time.RData"))
load(here("data","model","pre_lmint_time.RData"))
load(here("data","model","mod_lmint_loc.RData"))
load(here("data","model","pre_lmint_loc.RData"))

dat_sim_lmint_time_train <- dat_time_train %>% 
        mutate(true_sim = mod_lmint_time$fitted.values)

dat_sim_lmint_time_test <- dat_time_test %>% 
        mutate(true_sim = predict(mod_lmint_time, newdata = dat_time_test))

dat_sim_lmint_loc_train <- dat_loc_far_train %>% 
        mutate(true_sim = mod_lmint_loc$fitted.values)

dat_sim_lmint_loc_test <- dat_loc_far_test %>% 
        mutate(true_sim = predict(mod_lmint_loc, newdata = dat_loc_far_test))

met_sim_lmint <- vector(mode = "list", length = nsim)

for(i in 1:nsim){
        print(i)
        # set.seed(i)
        e_train <- rnorm(nsim_train)
        # set.seed(100+i)
        e_test <- rnorm(nsim_test)
        
        sim_time_train <- dat_sim_lmint_time_train[ind_time_train,] %>% 
                mutate(pm2.5_sim = true_sim + e_train)
        sim_time_test <- dat_sim_lmint_time_test[-ind_time_train,] 
        sim_time_test <- sim_time_test[1:nsim_test,] %>% 
                mutate(pm2.5_sim = true_sim + e_test)
        
        sim_loc_train <- dat_sim_lmint_loc_train[ind_loc_train,] %>% 
                mutate(pm2.5_sim = true_sim + e_train)
        sim_loc_test <- dat_sim_lmint_loc_test[-ind_loc_train,] 
        sim_loc_test <- sim_loc_test[1:nsim_test,] %>% 
                mutate(pm2.5_sim = true_sim + e_test)
        
        ## Model fitting and prediction
        ### EPA model
        ## Time
        pre_epa_time <- 0.52*(sim_time_test$pm2.5_cf1_a+sim_time_test$pm2.5_cf1_b)/2-0.085*sim_time_test$hum+5.71
        pre_epa_time <- pre_epa_time %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_time <- metrics(pre_epa_time, truth = obs, estimate = pre)
        met_sim_lmint[[i]] <- rbind(met_sim_lmint[[i]], met_epa_time)
        
        ## Location Farthest
        pre_epa_loc <- 0.52*(sim_loc_test$pm2.5_cf1_a+sim_loc_test$pm2.5_cf1_b)/2-0.085*sim_loc_test$hum+5.71
        pre_epa_loc <- pre_epa_loc %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_loc <- metrics(pre_epa_loc, truth = obs, estimate = pre)
        met_sim_lmint[[i]] <- rbind(met_sim_lmint[[i]], met_epa_loc)
        
        
        ### EPA-retrained MODEL
        ## Time 
        mod_epare_time <- lm(pm2.5_sim ~ pm2.5_cf1_m + hum,
                             data = sim_time_train)
        # summary(mod_epare_time)
        pre_epare_time <- predict(mod_epare_time,newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_epare_time <- metrics(pre_epare_time, truth = obs, estimate = pre)
        met_sim_lmint[[i]] <- rbind(met_sim_lmint[[i]], met_epare_time)
        
        ## Location Farthest
        mod_epare_loc <- lm(pm2.5_sim ~ pm2.5_cf1_m + temp + hum,
                            data = sim_loc_train)
        # summary(mod_epare_loc)
        pre_epare_loc <- predict(mod_epare_loc,newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_epare_loc <- metrics(pre_epare_loc, truth = obs, estimate = pre)
        met_sim_lmint[[i]] <- rbind(met_sim_lmint[[i]], met_epare_loc)
        
        
        ### LINEAR MODEL
        ## Time 
        mod_lm_time <- lm(pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                          data = sim_time_train)
        # summary(mod_lm_time)
        pre_lm_time <- predict(mod_lm_time,newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_lm_time <- metrics(pre_lm_time, truth = obs, estimate = pre)
        met_sim_lmint[[i]] <- rbind(met_sim_lmint[[i]], met_lm_time)
        
        ## Location Farthest
        mod_lm_loc <- lm(pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                         data = sim_loc_train)
        # summary(mod_lm_loc)
        pre_lm_loc <- predict(mod_lm_loc,newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_lm_loc <- metrics(pre_lm_loc, truth = obs, estimate = pre)
        met_sim_lmint[[i]] <- rbind(met_sim_lmint[[i]], met_lm_loc)
        
        
        ### LINEAR MODEL INTERACTION
        ## Time 
        mod_lmint_time <- lm(pm2.5_sim ~ pm2.5_cf1_a * (temp + hum),
                             data = sim_time_train)
        # summary(mod_lmint_time)
        pre_lmint_time <- predict(mod_lmint_time, newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_lmint_time <- metrics(pre_lmint_time, truth = obs, estimate = pre)
        met_sim_lmint[[i]] <- rbind(met_sim_lmint[[i]], met_lmint_time)
        
        ## Location Farthest
        mod_lmint_loc <- lm(pm2.5_sim ~ pm2.5_cf1_a * (temp + hum),
                            data = sim_loc_train)
        # summary(mod_lmint_loc)
        pre_lmint_loc <- predict(mod_lmint_loc, newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_lmint_loc <- metrics(pre_lmint_loc, truth = obs, estimate = pre)
        met_sim_lmint[[i]] <- rbind(met_sim_lmint[[i]], met_lmint_loc)
        
        
        ### Gradient Boosting Method
        ## Time
        mod_gbm_time <- gbm(
                formula = pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                data = sim_time_train,
                distribution = "gaussian",  # SSE loss function
                n.trees = 400,
                shrinkage = 0.1,
                interaction.depth = 3,
                n.minobsinnode = 10,
                cv.folds = 10
        )
        pre_gbm_time <- predict(mod_gbm_time, newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_gbm_time <- metrics(pre_gbm_time, truth = obs, estimate = pre)
        met_sim_lmint[[i]] <- rbind(met_sim_lmint[[i]], met_gbm_time)
        
        ## Location Farthest
        mod_gbm_loc <- gbm(
                formula = pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                data = sim_loc_train,
                distribution = "gaussian",  # SSE loss function
                n.trees = 400,
                shrinkage = 0.1,
                interaction.depth = 3,
                n.minobsinnode = 10,
                cv.folds = 10
        )
        pre_gbm_loc <- predict(mod_gbm_loc, newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_gbm_loc <- metrics(pre_gbm_loc, truth = obs, estimate = pre)
        met_sim_lmint[[i]] <- rbind(met_sim_lmint[[i]], met_gbm_loc)
        
        ### Random forest
        ## Time
        X_train_time <- sim_time_train[, c("pm2.5_cf1_a","temp","hum")]
        Y_train_time <- sim_time_train[, "pm2.5_sim"]
        
        X_test_time  <- sim_time_test[, c("pm2.5_cf1_a","temp","hum")]
        Y_test_time  <- sim_time_test[, "pm2.5_sim"]
        
        p <- 3
        n_tree <- 30
        
        # mtry = p is basically just bagging, as above
        mod_lmint_time <- randomForest(X_train_time, Y_train_time, 
                                       xtest = X_test_time, 
                                       ytest = Y_test_time, 
                                       mtry = p, 
                                       ntree = n_tree)
        
        pre_lmint_time <- mod_lmint_time$test$predicted %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_lmint_time <- metrics(pre_lmint_time, truth = obs, estimate = pre)
        met_sim_lmint[[i]] <- rbind(met_sim_lmint[[i]], met_lmint_time)
        
        
        ## Location Farthest
        X_train_loc <- sim_loc_train[, c("pm2.5_cf1_a","temp","hum")]
        Y_train_loc <- sim_loc_train[, "pm2.5_sim"]
        
        X_test_loc  <- sim_loc_test[, c("pm2.5_cf1_a","temp","hum")]
        Y_test_loc  <- sim_loc_test[, "pm2.5_sim"]
        
        p <- 3
        n_tree <- 30
        
        # mtry = p is basically just bagging, as above
        mod_lmint_loc <- randomForest(X_train_loc, Y_train_loc, 
                                      xtest = X_test_loc, 
                                      ytest = Y_test_loc, 
                                      mtry = p, 
                                      ntree = n_tree)
        
        pre_lmint_loc <- mod_lmint_loc$test$predicted %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_lmint_loc <- metrics(pre_lmint_loc, truth = obs, estimate = pre)
        met_sim_lmint[[i]] <- rbind(met_sim_lmint[[i]], met_lmint_loc)
}

met_sim_lmint

met_lmint_matrix  <- c()

for (i in 1:length(met_sim_lmint)) {
        met_lmint_matrix <- bind_cols(met_lmint_matrix, met_sim_lmint[[i]][3])
}

met_lmint_mean <- rowMeans(met_lmint_matrix)
met_lmint_final <- matrix(met_lmint_mean, ncol = 6, byrow = T)

# write.csv(met_lmint_matrix, here("results", "met_lmint_matrix.csv"))
write.csv(round(met_lmint_final,2), here("results", "met_lmint_final.csv"))


##################2.3.5 Gradient Boosting Method##################
## 1. Gradient Boosting Method based
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

# Set simulation parameters
nsim <- 20
nsim_train <- 100000
nsim_test <- 20000
set.seed(123)
ind_time_train <- sample(1:length(dat_time_train$time), nsim_train, replace = F)

ind_loc_train <- sample(1:length(dat_loc_far_train$time), nsim_train, replace = F)


# Create simulation model specific data set
load(here("data","model","mod_gbm_time.RData"))
load(here("data","model","pre_gbm_time.RData"))
load(here("data","model","mod_gbm_loc.RData"))
load(here("data","model","pre_gbm_loc.RData"))

dat_sim_gbm_time_train <- dat_time_train %>% 
        mutate(true_sim = mod_gbm_time$fit)

dat_sim_gbm_time_test <- dat_time_test %>% 
        mutate(true_sim = predict(mod_gbm_time, newdata = dat_time_test))

dat_sim_gbm_loc_train <- dat_loc_far_train %>% 
        mutate(true_sim = mod_gbm_loc$fit)

dat_sim_gbm_loc_test <- dat_loc_far_test %>% 
        mutate(true_sim = predict(mod_gbm_loc, newdata = dat_loc_far_test))

met_sim_gbm <- vector(mode = "list", length = nsim)

for(i in 1:nsim){
        print(i)
        # set.seed(i)
        e_train <- rnorm(nsim_train)
        # set.seed(100+i)
        e_test <- rnorm(nsim_test)
        
        sim_time_train <- dat_sim_gbm_time_train[ind_time_train,] %>% 
                mutate(pm2.5_sim = true_sim + e_train)
        sim_time_test <- dat_sim_gbm_time_test[-ind_time_train,] 
        sim_time_test <- sim_time_test[1:nsim_test,] %>% 
                mutate(pm2.5_sim = true_sim + e_test)
        
        sim_loc_train <- dat_sim_gbm_loc_train[ind_loc_train,] %>% 
                mutate(pm2.5_sim = true_sim + e_train)
        sim_loc_test <- dat_sim_gbm_loc_test[-ind_loc_train,] 
        sim_loc_test <- sim_loc_test[1:nsim_test,] %>% 
                mutate(pm2.5_sim = true_sim + e_test)
        
        ## Model fitting and prediction
        ### EPA model
        ## Time
        pre_epa_time <- 0.52*(sim_time_test$pm2.5_cf1_a+sim_time_test$pm2.5_cf1_b)/2-0.085*sim_time_test$hum+5.71
        pre_epa_time <- pre_epa_time %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_time <- metrics(pre_epa_time, truth = obs, estimate = pre)
        met_sim_gbm[[i]] <- rbind(met_sim_gbm[[i]], met_epa_time)
        
        ## Location Farthest
        pre_epa_loc <- 0.52*(sim_loc_test$pm2.5_cf1_a+sim_loc_test$pm2.5_cf1_b)/2-0.085*sim_loc_test$hum+5.71
        pre_epa_loc <- pre_epa_loc %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_loc <- metrics(pre_epa_loc, truth = obs, estimate = pre)
        met_sim_gbm[[i]] <- rbind(met_sim_gbm[[i]], met_epa_loc)
        
        
        ### EPA-retrained MODEL
        ## Time 
        mod_gbm_time <- lm(pm2.5_sim ~ pm2.5_cf1_m + hum,
                           data = sim_time_train)
        # summary(mod_gbm_time)
        pre_gbm_time <- predict(mod_gbm_time,newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_gbm_time <- metrics(pre_gbm_time, truth = obs, estimate = pre)
        met_sim_gbm[[i]] <- rbind(met_sim_gbm[[i]], met_gbm_time)
        
        ## Location Farthest
        mod_gbm_loc <- lm(pm2.5_sim ~ pm2.5_cf1_m + temp + hum,
                          data = sim_loc_train)
        # summary(mod_gbm_loc)
        pre_gbm_loc <- predict(mod_gbm_loc,newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_gbm_loc <- metrics(pre_gbm_loc, truth = obs, estimate = pre)
        met_sim_gbm[[i]] <- rbind(met_sim_gbm[[i]], met_gbm_loc)
        
        
        ### LINEAR MODEL
        ## Time 
        mod_gbm_time <- lm(pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                           data = sim_time_train)
        # summary(mod_gbm_time)
        pre_gbm_time <- predict(mod_gbm_time,newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_gbm_time <- metrics(pre_gbm_time, truth = obs, estimate = pre)
        met_sim_gbm[[i]] <- rbind(met_sim_gbm[[i]], met_gbm_time)
        
        ## Location Farthest
        mod_gbm_loc <- lm(pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                          data = sim_loc_train)
        # summary(mod_gbm_loc)
        pre_gbm_loc <- predict(mod_gbm_loc,newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_gbm_loc <- metrics(pre_gbm_loc, truth = obs, estimate = pre)
        met_sim_gbm[[i]] <- rbind(met_sim_gbm[[i]], met_gbm_loc)
        
        
        ### LINEAR MODEL INTERACTION
        ## Time 
        mod_gbm_time <- lm(pm2.5_sim ~ pm2.5_cf1_a * (temp + hum),
                           data = sim_time_train)
        # summary(mod_gbm_time)
        pre_gbm_time <- predict(mod_gbm_time, newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_gbm_time <- metrics(pre_gbm_time, truth = obs, estimate = pre)
        met_sim_gbm[[i]] <- rbind(met_sim_gbm[[i]], met_gbm_time)
        
        ## Location Farthest
        mod_gbm_loc <- lm(pm2.5_sim ~ pm2.5_cf1_a * (temp + hum),
                          data = sim_loc_train)
        # summary(mod_gbm_loc)
        pre_gbm_loc <- predict(mod_gbm_loc, newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_gbm_loc <- metrics(pre_gbm_loc, truth = obs, estimate = pre)
        met_sim_gbm[[i]] <- rbind(met_sim_gbm[[i]], met_gbm_loc)
        
        
        ### Gradient Boosting Method
        ## Time
        mod_gbm_time <- gbm(
                formula = pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                data = sim_time_train,
                distribution = "gaussian",  # SSE loss function
                n.trees = 400,
                shrinkage = 0.1,
                interaction.depth = 3,
                n.minobsinnode = 10,
                cv.folds = 10
        )
        pre_gbm_time <- predict(mod_gbm_time, newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_gbm_time <- metrics(pre_gbm_time, truth = obs, estimate = pre)
        met_sim_gbm[[i]] <- rbind(met_sim_gbm[[i]], met_gbm_time)
        
        ## Location Farthest
        mod_gbm_loc <- gbm(
                formula = pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                data = sim_loc_train,
                distribution = "gaussian",  # SSE loss function
                n.trees = 400,
                shrinkage = 0.1,
                interaction.depth = 3,
                n.minobsinnode = 10,
                cv.folds = 10
        )
        pre_gbm_loc <- predict(mod_gbm_loc, newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_gbm_loc <- metrics(pre_gbm_loc, truth = obs, estimate = pre)
        met_sim_gbm[[i]] <- rbind(met_sim_gbm[[i]], met_gbm_loc)
        
        ### Random forest
        ## Time
        X_train_time <- sim_time_train[, c("pm2.5_cf1_a","temp","hum")]
        Y_train_time <- sim_time_train[, "pm2.5_sim"]
        
        X_test_time  <- sim_time_test[, c("pm2.5_cf1_a","temp","hum")]
        Y_test_time  <- sim_time_test[, "pm2.5_sim"]
        
        p <- 3
        n_tree <- 30
        
        # mtry = p is basically just bagging, as above
        mod_gbm_time <- randomForest(X_train_time, Y_train_time, 
                                     xtest = X_test_time, 
                                     ytest = Y_test_time, 
                                     mtry = p, 
                                     ntree = n_tree)
        
        pre_gbm_time <- mod_gbm_time$test$predicted %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_gbm_time <- metrics(pre_gbm_time, truth = obs, estimate = pre)
        met_sim_gbm[[i]] <- rbind(met_sim_gbm[[i]], met_gbm_time)
        
        
        ## Location Farthest
        X_train_loc <- sim_loc_train[, c("pm2.5_cf1_a","temp","hum")]
        Y_train_loc <- sim_loc_train[, "pm2.5_sim"]
        
        X_test_loc  <- sim_loc_test[, c("pm2.5_cf1_a","temp","hum")]
        Y_test_loc  <- sim_loc_test[, "pm2.5_sim"]
        
        p <- 3
        n_tree <- 30
        
        # mtry = p is basically just bagging, as above
        mod_gbm_loc <- randomForest(X_train_loc, Y_train_loc, 
                                    xtest = X_test_loc, 
                                    ytest = Y_test_loc, 
                                    mtry = p, 
                                    ntree = n_tree)
        
        pre_gbm_loc <- mod_gbm_loc$test$predicted %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_gbm_loc <- metrics(pre_gbm_loc, truth = obs, estimate = pre)
        met_sim_gbm[[i]] <- rbind(met_sim_gbm[[i]], met_gbm_loc)
}

met_sim_gbm

met_gbm_matrix  <- c()

for (i in 1:length(met_sim_gbm)) {
        met_gbm_matrix <- bind_cols(met_gbm_matrix, met_sim_gbm[[i]][3])
}

met_gbm_mean <- rowMeans(met_gbm_matrix)
met_gbm_final <- matrix(met_gbm_mean, ncol = 6, byrow = T)

# write.csv(met_gbm_matrix, here("results", "met_gbm_matrix.csv"))
write.csv(round(met_gbm_final,2), here("results", "met_gbm_final.csv"))



##################2.3.6 RF##################
## 1. RF based
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

# Set simulation parameters
nsim <- 20
nsim_train <- 100000
nsim_test <- 20000
set.seed(123)
ind_time_train <- sample(1:length(dat_time_train$time), nsim_train, replace = F)

ind_loc_train <- sample(1:length(dat_loc_far_train$time), nsim_train, replace = F)


# Create simulation model specific data set
load(here("data","model","mod_rf_time.RData"))
load(here("data","model","pre_rf_time.RData"))
load(here("data","model","mod_rf_loc.RData"))
load(here("data","model","pre_rf_loc.RData"))

dat_sim_rf_time_train <- dat_time_train %>% 
        mutate(true_sim = mod_rf_time$predicted)

dat_sim_rf_time_test <- dat_time_test %>% 
        mutate(true_sim = mod_rf_time$test$predicted)

dat_sim_rf_loc_train <- dat_loc_far_train %>% 
        mutate(true_sim = mod_rf_loc$predicted)

dat_sim_rf_loc_test <- dat_loc_far_test %>% 
        mutate(true_sim = mod_rf_loc$test$predicted)

met_sim_rf <- vector(mode = "list", length = nsim)

for(i in 1:nsim){
        print(i)
        # set.seed(i)
        e_train <- rnorm(nsim_train)
        # set.seed(100+i)
        e_test <- rnorm(nsim_test)
        
        sim_time_train <- dat_sim_rf_time_train[ind_time_train,] %>% 
                mutate(pm2.5_sim = true_sim + e_train)
        sim_time_test <- dat_sim_rf_time_test[-ind_time_train,] 
        sim_time_test <- sim_time_test[1:nsim_test,] %>% 
                mutate(pm2.5_sim = true_sim + e_test)
        
        sim_loc_train <- dat_sim_rf_loc_train[ind_loc_train,] %>% 
                mutate(pm2.5_sim = true_sim + e_train)
        sim_loc_test <- dat_sim_rf_loc_test[-ind_loc_train,] 
        sim_loc_test <- sim_loc_test[1:nsim_test,] %>% 
                mutate(pm2.5_sim = true_sim + e_test)
        
        ## Model fitting and prediction
        ### EPA model
        ## Time
        pre_epa_time <- 0.52*(sim_time_test$pm2.5_cf1_a+sim_time_test$pm2.5_cf1_b)/2-0.085*sim_time_test$hum+5.71
        pre_epa_time <- pre_epa_time %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_time <- metrics(pre_epa_time, truth = obs, estimate = pre)
        met_sim_rf[[i]] <- rbind(met_sim_rf[[i]], met_epa_time)
        
        ## Location Farthest
        pre_epa_loc <- 0.52*(sim_loc_test$pm2.5_cf1_a+sim_loc_test$pm2.5_cf1_b)/2-0.085*sim_loc_test$hum+5.71
        pre_epa_loc <- pre_epa_loc %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_epa_loc <- metrics(pre_epa_loc, truth = obs, estimate = pre)
        met_sim_rf[[i]] <- rbind(met_sim_rf[[i]], met_epa_loc)
        
        
        ### EPA-retrained MODEL
        ## Time 
        mod_epare_time <- lm(pm2.5_sim ~ pm2.5_cf1_m + hum,
                             data = sim_time_train)
        # summary(mod_epare_time)
        pre_epare_time <- predict(mod_epare_time,newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_epare_time <- metrics(pre_epare_time, truth = obs, estimate = pre)
        met_sim_rf[[i]] <- rbind(met_sim_rf[[i]], met_epare_time)
        
        ## Location Farthest
        mod_epare_loc <- lm(pm2.5_sim ~ pm2.5_cf1_m + temp + hum,
                            data = sim_loc_train)
        # summary(mod_epare_loc)
        pre_epare_loc <- predict(mod_epare_loc,newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_epare_loc <- metrics(pre_epare_loc, truth = obs, estimate = pre)
        met_sim_rf[[i]] <- rbind(met_sim_rf[[i]], met_epare_loc)
        
        
        ### LINEAR MODEL
        ## Time 
        mod_lm_time <- lm(pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                          data = sim_time_train)
        # summary(mod_lm_time)
        pre_lm_time <- predict(mod_lm_time,newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_lm_time <- metrics(pre_lm_time, truth = obs, estimate = pre)
        met_sim_rf[[i]] <- rbind(met_sim_rf[[i]], met_lm_time)
        
        ## Location Farthest
        mod_lm_loc <- lm(pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                         data = sim_loc_train)
        # summary(mod_lm_loc)
        pre_lm_loc <- predict(mod_lm_loc,newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_lm_loc <- metrics(pre_lm_loc, truth = obs, estimate = pre)
        met_sim_rf[[i]] <- rbind(met_sim_rf[[i]], met_lm_loc)
        
        
        ### LINEAR MODEL INTERACTION
        ## Time 
        mod_lmint_time <- lm(pm2.5_sim ~ pm2.5_cf1_a * (temp + hum),
                             data = sim_time_train)
        # summary(mod_lmint_time)
        pre_lmint_time <- predict(mod_lmint_time, newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_lmint_time <- metrics(pre_lmint_time, truth = obs, estimate = pre)
        met_sim_rf[[i]] <- rbind(met_sim_rf[[i]], met_lmint_time)
        
        ## Location Farthest
        mod_lmint_loc <- lm(pm2.5_sim ~ pm2.5_cf1_a * (temp + hum),
                            data = sim_loc_train)
        # summary(mod_lmint_loc)
        pre_lmint_loc <- predict(mod_lmint_loc, newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_lmint_loc <- metrics(pre_lmint_loc, truth = obs, estimate = pre)
        met_sim_rf[[i]] <- rbind(met_sim_rf[[i]], met_lmint_loc)
        
        
        ### Gradient Boosting Method
        ## Time
        mod_gbm_time <- gbm(
                formula = pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                data = sim_time_train,
                distribution = "gaussian",  # SSE loss function
                n.trees = 400,
                shrinkage = 0.1,
                interaction.depth = 3,
                n.minobsinnode = 10,
                cv.folds = 10
        )
        pre_gbm_time <- predict(mod_gbm_time, newdata = sim_time_test) %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_gbm_time <- metrics(pre_gbm_time, truth = obs, estimate = pre)
        met_sim_rf[[i]] <- rbind(met_sim_rf[[i]], met_gbm_time)
        
        ## Location Farthest
        mod_gbm_loc <- gbm(
                formula = pm2.5_sim ~ pm2.5_cf1_a + temp + hum,
                data = sim_loc_train,
                distribution = "gaussian",  # SSE loss function
                n.trees = 400,
                shrinkage = 0.1,
                interaction.depth = 3,
                n.minobsinnode = 10,
                cv.folds = 10
        )
        pre_gbm_loc <- predict(mod_gbm_loc, newdata = sim_loc_test) %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_gbm_loc <- metrics(pre_gbm_loc, truth = obs, estimate = pre)
        met_sim_rf[[i]] <- rbind(met_sim_rf[[i]], met_gbm_loc)
        
        ### Random forest
        ## Time
        X_train_time <- sim_time_train[, c("pm2.5_cf1_a","temp","hum")]
        Y_train_time <- sim_time_train[, "pm2.5_sim"]
        
        X_test_time  <- sim_time_test[, c("pm2.5_cf1_a","temp","hum")]
        Y_test_time  <- sim_time_test[, "pm2.5_sim"]
        
        p <- 3
        n_tree <- 30
        
        # mtry = p is basically just bagging, as above
        mod_rf_time <- randomForest(X_train_time, Y_train_time, 
                                    xtest = X_test_time, 
                                    ytest = Y_test_time, 
                                    mtry = p, 
                                    ntree = n_tree)
        
        pre_rf_time <- mod_rf_time$test$predicted %>% 
                cbind(pre = ., obs = sim_time_test$pm2.5_sim) %>% 
                as_tibble()
        met_rf_time <- metrics(pre_rf_time, truth = obs, estimate = pre)
        met_sim_rf[[i]] <- rbind(met_sim_rf[[i]], met_rf_time)
        
        
        ## Location Farthest
        X_train_loc <- sim_loc_train[, c("pm2.5_cf1_a","temp","hum")]
        Y_train_loc <- sim_loc_train[, "pm2.5_sim"]
        
        X_test_loc  <- sim_loc_test[, c("pm2.5_cf1_a","temp","hum")]
        Y_test_loc  <- sim_loc_test[, "pm2.5_sim"]
        
        p <- 3
        n_tree <- 30
        
        # mtry = p is basically just bagging, as above
        mod_rf_loc <- randomForest(X_train_loc, Y_train_loc, 
                                   xtest = X_test_loc, 
                                   ytest = Y_test_loc, 
                                   mtry = p, 
                                   ntree = n_tree)
        
        pre_rf_loc <- mod_rf_loc$test$predicted %>% 
                cbind(pre = ., obs = sim_loc_test$pm2.5_sim) %>% 
                as_tibble()
        met_rf_loc <- metrics(pre_rf_loc, truth = obs, estimate = pre)
        met_sim_rf[[i]] <- rbind(met_sim_rf[[i]], met_rf_loc)
}

met_sim_rf

met_rf_matrix  <- c()

for (i in 1:length(met_sim_rf)) {
        met_rf_matrix <- bind_cols(met_rf_matrix, met_sim_rf[[i]][3])
}

met_rf_mean <- rowMeans(met_rf_matrix)
met_rf_final <- matrix(met_rf_mean, ncol = 6, byrow = T)

# write.csv(met_rf_matrix, here("results", "met_rf_matrix.csv"))
write.csv(round(met_rf_final,2), here("results", "met_rf_final.csv"))
############################END############################


##################2.4 MBA Interpolation Map##################
# Mean function
mymean = function(x){
        if(all(is.na(x))) NA 
        else mean(x,na.rm=T)
}

# (1) Create data set for ploting
load(here("data","model","mod_gbm_time.RData"))
load(here("data","tidy","CA","pa_join_epa.RData"))

### California map
camap = ggmap(get_stamenmap(bbox=c(left=-124.6,bottom=32.2,right=-114,top=42.2),
                            source="stamen", maptype="terrain-background", crop=FALSE, zoom = 10))

dat_ca <- pa_join_epa %>% 
        select("time","pm2.5","PM2.5_CF1_A","PM2.5_CF1_B","pm2.5_A","pm2.5_B","temp","humidity",
               "label","longitude","latitude","lon_epa","lat_epa","dist") %>% 
        rename(pm2.5_epa = pm2.5,
               pm2.5_cf1_a = PM2.5_CF1_A,
               pm2.5_cf1_b = PM2.5_CF1_B,
               pm2.5_atm_a = pm2.5_A,
               pm2.5_atm_b = pm2.5_B,
               hum = humidity,
               lon_pa = longitude,
               lat_pa = latitude) %>% 
        filter(!is.na(pm2.5_epa)) %>%
        filter(!is.na(pm2.5_cf1_a)) %>% 
        filter(!is.na(pm2.5_cf1_b)) %>% 
        filter(!is.na(pm2.5_atm_a)) %>% 
        filter(!is.na(pm2.5_atm_b)) %>% 
        filter(!is.na(temp)) %>% 
        filter(!is.na(hum)) %>% 
        filter(temp >= 0) %>% 
        distinct()
dat_ca_pre <- dat_ca %>% 
        mutate(pm2.5_gbm = predict(mod_gbm_time, newdata = dat_ca),
               pm2.5_pa = pm2.5_cf1_a)

load(here("data","tidy","CA_South","pa_join_epa_CF1.RData"))
dat_cas <- pa_join_epa %>% 
        select("time","pm2.5","PM2.5_CF1_A","PM2.5_CF1_B","pm2.5_A","pm2.5_B","temp","humidity",
               "label","longitude","latitude","lon_epa","lat_epa","dist") %>% 
        rename(pm2.5_epa = pm2.5,
               pm2.5_cf1_a = PM2.5_CF1_A,
               pm2.5_cf1_b = PM2.5_CF1_B,
               pm2.5_atm_a = pm2.5_A,
               pm2.5_atm_b = pm2.5_B,
               hum = humidity,
               lon_pa = longitude,
               lat_pa = latitude) %>% 
        filter(!is.na(pm2.5_epa)) %>%
        filter(!is.na(pm2.5_cf1_a)) %>% 
        filter(!is.na(pm2.5_cf1_b)) %>% 
        filter(!is.na(pm2.5_atm_a)) %>% 
        filter(!is.na(pm2.5_atm_b)) %>% 
        filter(!is.na(temp)) %>% 
        filter(!is.na(hum)) %>% 
        filter(temp >= 0) %>% 
        distinct()

dat_cas_pre <- dat_cas %>% 
        mutate(pm2.5_gbm = predict(mod_gbm_time, newdata = dat_cas),
               pm2.5_pa = pm2.5_cf1_a)

dat_both_pre <- bind_rows(dat_ca_pre, dat_cas_pre) %>% 
        distinct()
# save(dat_both_pre, file = here("data","plot","CA","dat_both_pre.RData"))
# load(here("data","plot","CA","dat_both_pre.RData"))

dat_mba <- c()

dat_mba[[1]] <- dat_both_pre

dat_mba[[2]] <- dat_both_pre %>% 
        filter(time >= "2019-10-01 00:00:00 UTC") %>% 
        filter(time < "2019-11-01 00:00:00 UTC") 

dat_mba[[3]] <- dat_both_pre %>% 
        filter(time >= "2019-11-01 00:00:00 UTC") %>% 
        filter(time < "2019-12-01 00:00:00 UTC") 

dat_mba[[4]] <- dat_both_pre %>% 
        filter(time >= "2019-12-01 00:00:00 UTC") %>% 
        filter(time < "2020-01-01 00:00:00 UTC") 

dat_mba[[5]] <- dat_both_pre %>% 
        filter(time >= "2020-01-01 00:00:00 UTC") %>% 
        filter(time < "2020-02-01 00:00:00 UTC")

dat_mba[[6]] <- dat_both_pre %>% 
        filter(time >= "2020-02-01 00:00:00 UTC") %>% 
        filter(time < "2020-03-01 00:00:00 UTC")

dat_mba[[7]] <- dat_both_pre %>% 
        filter(time >= "2020-03-01 00:00:00 UTC") %>% 
        filter(time < "2020-04-01 00:00:00 UTC")

name_mba <- c("all","oct","nov","dec","jan","feb","mar")
names(dat_mba) <- name_mba

# save(dat_mba, file = here("data","plot","CA","dat_mba.RData"))
load(here("data","tidy","EPA","pm2.5_CA_reduced.RData"))

epa <- pm2.5_reduced %>% 
        mutate(`Date GMT` = date(`Date GMT`),
               `Time GMT` = as_hms(`Time GMT`)) %>%
        rename(lon_epa = Longitude, lat_epa = Latitude, pm2.5_epa = pm2.5) %>% 
        select(lon_epa, lat_epa, pm2.5_epa, `Date GMT`, `Time GMT`) %>% 
        distinct()

time_join <- pa_join_epa %>% 
        select(time, date_GMT, hms_GMT) %>% 
        distinct()

epa_mba_all <- epa %>% 
        left_join(., y = time_join,
                  by = c("Date GMT"="date_GMT",
                         "Time GMT"="hms_GMT")) %>% 
        filter(!is.na(time))

# save(epa_mba_all, file = here("data","plot","CA","epa_mba_all.RData"))
# load(here("data","plot","CA","epa_mba_all.RData"))

epa_mba <- c()

epa_mba[[1]] <- epa_mba_all

epa_mba[[2]] <- epa_mba_all %>% 
        filter(time >= "2019-10-01 00:00:00 UTC") %>% 
        filter(time < "2019-11-01 00:00:00 UTC") 

epa_mba[[3]] <- epa_mba_all %>% 
        filter(time >= "2019-11-01 00:00:00 UTC") %>% 
        filter(time < "2019-12-01 00:00:00 UTC") 

epa_mba[[4]] <- epa_mba_all %>% 
        filter(time >= "2019-12-01 00:00:00 UTC") %>% 
        filter(time < "2020-01-01 00:00:00 UTC") 

epa_mba[[5]] <- epa_mba_all %>% 
        filter(time >= "2020-01-01 00:00:00 UTC") %>% 
        filter(time < "2020-02-01 00:00:00 UTC")

epa_mba[[6]] <- epa_mba_all %>% 
        filter(time >= "2020-02-01 00:00:00 UTC") %>% 
        filter(time < "2020-03-01 00:00:00 UTC")

epa_mba[[7]] <- epa_mba_all %>% 
        filter(time >= "2020-03-01 00:00:00 UTC") %>% 
        filter(time < "2020-04-01 00:00:00 UTC")

name_mba <- c("all","oct","nov","dec","jan","feb","mar")
names(epa_mba) <- name_mba

# save(epa_mba, file = here("data","plot","CA","epa_mba.RData"))
# load(here("data","plot","CA","camap10.RData"))
# load(here("data","plot","CA","dat_mba.RData"))
# load(here("data","plot","CA","epa_mba.RData"))

# Weight 1
epa_col <- c("#00e400", "#ffff00", "#ff7e00", "#ff0000", "#99004c", "#7e0023")

# Weight 2
# epa_col <- c("#00e400", "#00e400", "#ffff00", "#ffff00", "#ff7e00", "#ff0000", "#99004c", "#7e0023")

##################(1.1) monthly separate##################
for(i in 1:length(name_mba)){
        print(i)
        dat_pre <- dat_mba[[i]]
        epa_pre <- epa_mba[[i]]
        ## EPA
        cross_epa = epa_pre %>% 
                group_by(lat_epa,lon_epa) %>% 
                summarize(n=length(which(!is.na(pm2.5_epa))),
                          pm2.5_epa=mymean(pm2.5_epa)) %>% 
                arrange(n) %>% 
                as.data.frame()
        
        mba_epa <- mba.surf(cross_epa[,c("lon_epa","lat_epa","pm2.5_epa")], no.X=150, no.Y=150, h=5, m=1, extend=FALSE)$xyz.est
        dimnames(mba_epa$z) <- list(mba_epa$x, mba_epa$y)
        
        grid_epa <- melt(mba_epa$z, varnames = c('lon_epa', 'lat_epa'), value.name = 'value')
        
        map_epa = camap + 
                ggplot2::geom_tile(data=grid_epa, 
                                   aes(x=lon_epa,y=lat_epa,fill=value),
                                   alpha=0.5) +
                geom_point(data=cross_epa,
                           aes(x=lon_epa,y=lat_epa,fill=pm2.5_epa),
                           shape=21,alpha=0.9,col="black",stroke=0.65,size=6) +
                scale_fill_gradientn(colours = epa_col,limits = c(0,40),
                                     oob = scales::squish)+
                labs(fill=expression(atop(paste(PM[2.5]),paste(" [", mu, "g/","m"^3, "]"))),
                     x="Longitude",
                     y="Latitude")+
                scale_x_continuous(limits = c(-123.8, -114.2), expand = c(0, 0)) +
                scale_y_continuous(limits = c(32.15, 42.04), expand = c(0, 0)) +
                theme(text = element_text(size=28),
                      legend.text = element_text(size = 28),
                      legend.key.size = unit(1.5, "cm"))
        
        ## PA
        cross_pa = dat_pre %>% 
                group_by(lat_pa,lon_pa) %>% 
                summarize(n=length(which(!is.na(pm2.5_pa))),
                          pm2.5_pa=mymean(pm2.5_pa),
                          nlabel=length(unique(label)),
                          label=head(label,1)) %>% 
                arrange(n) %>% 
                as.data.frame()
        
        mba_pa <- mba.surf(cross_pa[,c("lon_pa","lat_pa","pm2.5_pa")], no.X=150, no.Y=150, h=5, m=1, extend=FALSE)$xyz.est
        dimnames(mba_pa$z) <- list(mba_pa$x, mba_pa$y)
        
        grid_pa <- melt(mba_pa$z, varnames = c('lon_pa', 'lat_pa'), value.name = 'value')
        
        map_pa = camap + 
                ggplot2::geom_tile(data=grid_pa, 
                                   aes(x=lon_pa,y=lat_pa,fill=value),
                                   alpha=0.5) +
                geom_point(data=cross_pa,
                           aes(x=lon_pa,y=lat_pa,fill=pm2.5_pa),
                           shape=21,alpha=0.9,col="black",stroke=0.65,size=6) +
                scale_fill_gradientn(colours = epa_col,limits = c(0,40),
                                     oob = scales::squish)+
                labs(fill=expression(atop(paste(PM[2.5]),paste(" [", mu, "g/","m"^3, "]"))),
                     x="Longitude",
                     y="Latitude")+
                scale_x_continuous(limits = c(-123.8, -114.2), expand = c(0, 0)) +
                scale_y_continuous(limits = c(32.15, 42.04), expand = c(0, 0)) +
                theme(text = element_text(size=28),
                      legend.text = element_text(size = 28),
                      legend.key.size = unit(1.5, "cm"))
        
        ## GBM
        cross_gbm = dat_pre %>% 
                group_by(lat_pa,lon_pa) %>% 
                summarize(n=length(which(!is.na(pm2.5_gbm))),
                          pm2.5_gbm=mymean(pm2.5_gbm),
                          nlabel=length(unique(label)),
                          label=head(label,1)) %>% 
                arrange(n) %>% 
                as.data.frame()
        
        mba_gbm <- mba.surf(cross_gbm[,c("lon_pa","lat_pa","pm2.5_gbm")], no.X=150, no.Y=150, h=5, m=1, extend=FALSE)$xyz.est
        dimnames(mba_gbm$z) <- list(mba_gbm$x, mba_gbm$y)
        
        grid_gbm <- melt(mba_gbm$z, varnames = c('lon_pa', 'lat_pa'), value.name = 'value')
        
        map_gbm = camap + 
                ggplot2::geom_tile(data=grid_gbm, 
                                   aes(x=lon_pa,y=lat_pa,fill=value),
                                   alpha=0.5) +
                geom_point(data=cross_gbm,
                           aes(x=lon_pa,y=lat_pa,fill=pm2.5_gbm),
                           shape=21,alpha=0.9,col="black",stroke=0.65,size=6) +
                scale_fill_gradientn(colours = epa_col,limits = c(0,40),
                                     oob = scales::squish)+
                labs(fill=expression(atop(paste(PM[2.5]),paste(" [", mu, "g/","m"^3, "]"))),
                     x="Longitude",
                     y="Latitude")+
                scale_x_continuous(limits = c(-123.8, -114.2), expand = c(0, 0)) +
                scale_y_continuous(limits = c(32.15, 42.04), expand = c(0, 0)) +
                theme(text = element_text(size=28),
                      legend.text = element_text(size = 28),
                      legend.key.size = unit(1.5, "cm"))
        
        ggsave(filename = paste("plot_mba_",name_mba[i],".png",sep = ""),
               path = here("figures","mba"),
               plot = ggarrange(map_epa,map_pa,map_gbm, ncol=3, nrow=1,
                                common.legend = TRUE, legend="right",
                                labels = c("        (a) EPA","      (b) PurpleAir","(c) Gradient Boosting"),
                                font.label = list(size = 28),
                                vjust = 1.3),
               device = "png",
               width = 60,
               height = 25,
               units = "cm",
               dpi = 300
        )
}

##################(1.2) dual-month Panel for Oct and Nov##################
mba_ls <- c()
for(i in 2:3){
        print(i)
        dat_pre <- dat_mba[[i]]
        epa_pre <- epa_mba[[i]]
        if(i==2){lab_month = "Oct"}
        if(i==3){lab_month = "Nov"}
        ## EPA
        cross_epa = epa_pre %>% 
                group_by(lat_epa,lon_epa) %>% 
                summarize(n=length(which(!is.na(pm2.5_epa))),
                          pm2.5_epa=mymean(pm2.5_epa)) %>% 
                arrange(n) %>% 
                as.data.frame()
        
        mba_epa <- mba.surf(cross_epa[,c("lon_epa","lat_epa","pm2.5_epa")], no.X=150, no.Y=150, h=5, m=1, extend=FALSE)$xyz.est
        dimnames(mba_epa$z) <- list(mba_epa$x, mba_epa$y)
        
        grid_epa <- melt(mba_epa$z, varnames = c('lon_epa', 'lat_epa'), value.name = 'value')
        
        map_epa = camap + 
                ggplot2::geom_tile(data=grid_epa, 
                                   aes(x=lon_epa,y=lat_epa,fill=value),
                                   alpha=0.5) +
                geom_point(data=cross_epa,
                           aes(x=lon_epa,y=lat_epa,fill=pm2.5_epa),
                           shape=21,alpha=0.9,col="black",stroke=0.65,size=6) +
                scale_fill_gradientn(colours = epa_col,limits = c(0,40),
                                     oob = scales::squish)+
                labs(fill=expression(atop(paste(PM[2.5]),paste(" [", mu, "g/","m"^3, "]"))),
                     x="Longitude",
                     y="Latitude"
                )+
                scale_x_continuous(limits = c(-123.8, -114.2), expand = c(0, 0)) +
                scale_y_continuous(limits = c(32.15, 42.04), expand = c(0, 0)) +
                theme(text = element_text(size=34),
                      legend.text = element_text(size = 28),
                      legend.key.size = unit(3, "cm"),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank())
        
        
        ## PA
        cross_pa = dat_pre %>% 
                group_by(lat_pa,lon_pa) %>% 
                summarize(n=length(which(!is.na(pm2.5_pa))),
                          pm2.5_pa=mymean(pm2.5_pa),
                          nlabel=length(unique(label)),
                          label=head(label,1)) %>% 
                arrange(n) %>% 
                as.data.frame()
        
        mba_pa <- mba.surf(cross_pa[,c("lon_pa","lat_pa","pm2.5_pa")], no.X=150, no.Y=150, h=5, m=1, extend=FALSE)$xyz.est
        dimnames(mba_pa$z) <- list(mba_pa$x, mba_pa$y)
        
        grid_pa <- melt(mba_pa$z, varnames = c('lon_pa', 'lat_pa'), value.name = 'value')
        
        map_pa = camap + 
                ggplot2::geom_tile(data=grid_pa, 
                                   aes(x=lon_pa,y=lat_pa,fill=value),
                                   alpha=0.5) +
                geom_point(data=cross_pa,
                           aes(x=lon_pa,y=lat_pa,fill=pm2.5_pa),
                           shape=21,alpha=0.9,col="black",stroke=0.65,size=6) +
                annotate("text", x = -119, y = 41, label = lab_month, color = "black",
                         size = 20
                         # , fontface="bold.italic"
                )+
                scale_fill_gradientn(colours = epa_col,limits = c(0,40),
                                     oob = scales::squish)+
                labs(fill=expression(atop(paste(PM[2.5]),paste(" [", mu, "g/","m"^3, "]"))),
                     x="Longitude",
                     y="Latitude"
                )+
                scale_x_continuous(limits = c(-123.8, -114.2), expand = c(0, 0)) +
                scale_y_continuous(limits = c(32.15, 42.04), expand = c(0, 0)) +
                theme(text = element_text(size=34),
                      legend.text = element_text(size = 28),
                      legend.key.size = unit(3, "cm"),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank())
        
        
        ## GBM
        cross_gbm = dat_pre %>% 
                group_by(lat_pa,lon_pa) %>% 
                summarize(n=length(which(!is.na(pm2.5_gbm))),
                          pm2.5_gbm=mymean(pm2.5_gbm),
                          nlabel=length(unique(label)),
                          label=head(label,1)) %>% 
                arrange(n) %>% 
                as.data.frame()
        
        mba_gbm <- mba.surf(cross_gbm[,c("lon_pa","lat_pa","pm2.5_gbm")], no.X=150, no.Y=150, h=5, m=1, extend=FALSE)$xyz.est
        dimnames(mba_gbm$z) <- list(mba_gbm$x, mba_gbm$y)
        
        grid_gbm <- melt(mba_gbm$z, varnames = c('lon_pa', 'lat_pa'), value.name = 'value')
        
        map_gbm = camap + 
                ggplot2::geom_tile(data=grid_gbm, 
                                   aes(x=lon_pa,y=lat_pa,fill=value),
                                   alpha=0.5) +
                geom_point(data=cross_gbm,
                           aes(x=lon_pa,y=lat_pa,fill=pm2.5_gbm),
                           shape=21,alpha=0.9,col="black",stroke=0.65,size=6) +
                scale_fill_gradientn(colours = epa_col,limits = c(0,40),
                                     oob = scales::squish)+
                labs(fill=expression(atop(paste(PM[2.5]),paste(" [", mu, "g/","m"^3, "]"))),
                     x="Longitude",
                     y="Latitude"
                )+
                scale_x_continuous(limits = c(-123.8, -114.2), expand = c(0, 0)) +
                scale_y_continuous(limits = c(32.15, 42.04), expand = c(0, 0)) +
                theme(text = element_text(size=34),
                      legend.text = element_text(size = 28),
                      legend.key.size = unit(3, "cm"),
                      axis.title.x = element_blank(),
                      axis.title.y = element_blank())
        
        mba_ls[[10*i+1]] <- map_epa
        mba_ls[[10*i+2]] <- map_pa
        mba_ls[[10*i+3]] <- map_gbm
}

mba_on <- c()
mba_on[1:3] <- mba_ls[21:23]
mba_on[4:6] <- mba_ls[31:33]

ggsave(filename = paste("plot_mba_octnov",".png",sep = ""),
       path = here("figures","mba"),
       plot = ggarrange(mba_on[[1]],mba_on[[2]],mba_on[[3]],
                        mba_on[[4]],mba_on[[5]],mba_on[[6]],
                        ncol=3, nrow=2,
                        common.legend = TRUE, legend="right",
                        labels = c("        (a) EPA","      (b) PurpleAir","(c) Gradient Boosting"),
                        font.label = list(size = 38),
                        vjust = 1.3),
       device = "png",
       width = 60,
       height = 50,
       units = "cm",
       dpi = 300
)


##################(1.3) monthly differences between PA/GBM vs EPA ##################
mymean = function(x){
        if(all(is.na(x))) NA 
        else mean(x,na.rm=T)
}

# load(here("data","plot","CA","camap10.RData"))
# load(here("data","plot","CA","dat_mba.RData"))
# load(here("data","plot","CA","epa_mba.RData"))

name_mba <- c("all","oct","nov","dec","jan","feb","mar")

# Weight 1
epa_col <- c("#00e400", "#ffff00", "#ff7e00", "#ff0000", "#99004c", "#7e0023")

# Weight 2
# epa_col <- c("#00e400", "#00e400", "#ffff00", "#ffff00", "#ff7e00", "#ff0000", "#99004c", "#7e0023")

dif_col <- c("midnightblue", "cyan", "green","yellow", "red")

for(i in 1:length(name_mba)){
        print(i)
        dat_pre <- dat_mba[[i]] %>% 
                mutate(pm2.5_dif_pa = pm2.5_pa - pm2.5_epa,
                       pm2.5_dif_gbm = pm2.5_gbm - pm2.5_epa)
        
        epa_pre <- epa_mba[[i]]
        
        ## EPA
        cross_epa = epa_pre %>% 
                group_by(lat_epa,lon_epa) %>% 
                summarize(n=length(which(!is.na(pm2.5_epa))),
                          pm2.5_epa=mymean(pm2.5_epa)) %>% 
                arrange(n) %>% 
                as.data.frame()
        
        mba_epa <- mba.surf(cross_epa[,c("lon_epa","lat_epa","pm2.5_epa")], no.X=150, no.Y=150, h=5, m=1, extend=FALSE)$xyz.est
        dimnames(mba_epa$z) <- list(mba_epa$x, mba_epa$y)
        
        grid_epa <- melt(mba_epa$z, varnames = c('lon_epa', 'lat_epa'), value.name = 'value')
        
        map_epa = camap + 
                ggplot2::geom_tile(data=grid_epa, 
                                   aes(x=lon_epa,y=lat_epa,fill=value),
                                   alpha=0.5) +
                geom_point(data=cross_epa,
                           aes(x=lon_epa,y=lat_epa,fill=pm2.5_epa),
                           shape=21,alpha=0.9,col="black",stroke=0.65,size=6) +
                scale_fill_gradientn(colours = epa_col,limits = c(0,40),
                                     oob = scales::squish)+
                labs(fill=expression(PM[2.5]*" ["*mu*"g/"*"m"^3*"]"),
                     # fill=expression(atop(paste(PM[2.5]),paste(" [", mu, "g/","m"^3, "]"))),
                     x="Longitude",
                     y="Latitude")+
                scale_x_continuous(limits = c(-123.8, -114.2), expand = c(0, 0)) +
                scale_y_continuous(limits = c(32.15, 42.04), expand = c(0, 0)) +
                theme(text = element_text(size=28),
                      legend.text = element_text(size = 24),
                      legend.key.size = unit(1.2, "cm"))
        
        
        ## PA
        cross_pa = dat_pre %>% 
                group_by(lat_pa,lon_pa) %>% 
                summarize(n=length(which(!is.na(pm2.5_dif_pa))),
                          pm2.5_dif_pa=mymean(pm2.5_dif_pa),
                          nlabel=length(unique(label)),
                          label=head(label,1)) %>% 
                arrange(n) %>% 
                as.data.frame()
        
        mba_pa <- mba.surf(cross_pa[,c("lon_pa","lat_pa","pm2.5_dif_pa")], no.X=150, no.Y=150, h=5, m=1, extend=FALSE)$xyz.est
        dimnames(mba_pa$z) <- list(mba_pa$x, mba_pa$y)
        
        grid_pa <- melt(mba_pa$z, varnames = c('lon_pa', 'lat_pa'), value.name = 'value')
        
        map_pa = camap + 
                ggplot2::geom_tile(data=grid_pa, 
                                   aes(x=lon_pa,y=lat_pa,fill=value),
                                   alpha=0.5) +
                scale_fill_gradientn(colours = dif_col,limits = c(-10,10),
                                     oob = scales::squish)+
                labs(fill=expression(PM[2.5]*" ["*mu*"g/"*"m"^3*"]"),
                     # fill=expression(atop(paste(PM[2.5]),paste(" [", mu, "g/","m"^3, "]"))),
                     x="Longitude",
                     y="Latitude")+
                scale_x_continuous(limits = c(-123.8, -114.2), expand = c(0, 0)) +
                scale_y_continuous(limits = c(32.15, 42.04), expand = c(0, 0)) +
                theme(text = element_text(size=28),
                      legend.text = element_text(size = 24),
                      legend.key.size = unit(1.2, "cm"))
        
        
        ## GBM
        cross_gbm = dat_pre %>% 
                group_by(lat_pa,lon_pa) %>% 
                summarize(n=length(which(!is.na(pm2.5_dif_gbm))),
                          pm2.5_dif_gbm=mymean(pm2.5_dif_gbm),
                          nlabel=length(unique(label)),
                          label=head(label,1)) %>% 
                arrange(n) %>% 
                as.data.frame()
        
        mba_gbm <- mba.surf(cross_gbm[,c("lon_pa","lat_pa","pm2.5_dif_gbm")], no.X=150, no.Y=150, h=5, m=1, extend=FALSE)$xyz.est
        dimnames(mba_gbm$z) <- list(mba_gbm$x, mba_gbm$y)
        
        grid_gbm <- melt(mba_gbm$z, varnames = c('lon_pa', 'lat_pa'), value.name = 'value')
        
        map_gbm = camap + 
                ggplot2::geom_tile(data=grid_gbm, 
                                   aes(x=lon_pa,y=lat_pa,fill=value),
                                   alpha=0.5) +
                scale_fill_gradientn(colours = dif_col,limits = c(-10,10),
                                     oob = scales::squish)+
                labs(fill=expression(PM[2.5]*" ["*mu*"g/"*"m"^3*"]"),
                     # fill=expression(atop(paste(PM[2.5]),paste(" [", mu, "g/","m"^3, "]"))),
                     x="Longitude",
                     y="Latitude")+
                scale_x_continuous(limits = c(-123.8, -114.2), expand = c(0, 0)) +
                scale_y_continuous(limits = c(32.15, 42.04), expand = c(0, 0)) +
                theme(text = element_text(size=28),
                      legend.text = element_text(size = 24),
                      legend.key.size = unit(1.2, "cm"))
        
        ggsave(filename = paste("plot_mba_dif_",name_mba[i],".png",sep = ""),
               path = here("figures","mba"),
               plot = ggarrange(map_epa,map_pa,map_gbm, ncol=3, nrow=1,
                                common.legend = FALSE, legend="bottom",
                                labels = c("        (a) EPA","      (b) PurpleAir","(c) Gradient Boosting"),
                                font.label = list(size = 28),
                                vjust = 1.3),
               device = "png",
               width = 55,
               height = 28,
               units = "cm",
               dpi = 300
        )
}

##################2.4 MBA Interpolation Map END##################
############################END############################