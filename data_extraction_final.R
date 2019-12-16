#### Objective: Creating dataframes for visualisation ------------

## Load the required libraries ------------
library(dplyr)
library(plyr)
library(tidyr)
library(tabulizer)
library(rvest)
library(stringr)
library(data.table)
library(DataCombine)
library(janitor)
library(tidyverse)
library(openxlsx)
library(stringr)
library(plotly)

data_folder <- "C:/Users/norzarifah.k/Documents/Masters/WQD7001/Assingment"
data_source <- "/src"
file_location <- setwd(paste0(data_folder, data_source))

#### ASEAN_STAT_BOOK #####
asean_stat_src <- "3_asyb-2018.pdf"
asean_stat_data <- extract_tables(asean_stat_src, pages = 185:187, method = "stream", output = "data.frame")

# road_condition
road_condition <- asean_stat_data[[1]]
colnames(road_condition) <- road_condition[1,]
names(road_condition)[c(1:3)] <- c("Condition", "Year", "Brunei")
road_condition <- road_condition[-c(1,17),]
rownames(road_condition) <- NULL
road_condition[c(1:10),1] <- "total_length_km"
road_condition[c(11:20),1] <- "paved_length_km"
road_condition[,2:ncol(road_condition)] <- t(data.frame(apply(road_condition[,2:ncol(road_condition)], 1, function(x) as.numeric(gsub(",", "", x)))))
names(road_condition)[3] <- "Brunei Darussalam"
# motor_vehicle
motor_vehicle <- asean_stat_data[[2]]
colnames(motor_vehicle) <- motor_vehicle[1,]
motor_vehicle <- motor_vehicle[-c(1, 6, 9), -c(1)]
names(motor_vehicle)[1:2] <- c("Year", "Brunei Darussalam")
rownames(motor_vehicle) <- NULL
motor_vehicle[,] <- t(data.frame(apply(motor_vehicle[,], 1, function(x) as.numeric(gsub(",", "", x)))))
# motor_vehicle (transformed)
motor_vehicle.T <- t(motor_vehicle)
motor_vehicle.T <- data.frame(motor_vehicle.T)
colnames(motor_vehicle.T) <- motor_vehicle.T[1,]
motor_vehicle.T <- motor_vehicle.T[-c(1),]
motor_vehicle.T$country <- rownames(motor_vehicle.T)
rownames(motor_vehicle.T) <- NULL
motor_vehicle <- copy(motor_vehicle.T)
motor_vehicle$category <-  "registered road motor vehicles"
motor_vehicle <- motor_vehicle[c(11,1:10,12)]
# total_vehicle (per 1000 population)
total_vehicle <- read.csv("totalVehicle_2017_aseanstat_2008_2017.csv", header=TRUE, sep=",")
colnames(total_vehicle) <- str_replace_all(colnames(total_vehicle), "[:punct:]", " ")
total_vehicle[,] <- t(data.frame(apply(total_vehicle[,], 1, function(x) as.numeric(gsub(",", "", x)))))
# motor_cycle
motor_cycle <- read.csv("motorCycle_2017_aseanstat_2008_2017.csv", header=TRUE, sep=",")
list <- list(motor_cycle[1,])
colnames(motor_cycle) <- unlist(list)
motor_cycle <- motor_cycle[-c(1),]
rownames(motor_cycle) <- NULL
motor_cycle[,2:ncol(motor_cycle)] <- t(data.frame(apply(motor_cycle[,2:ncol(motor_cycle)], 1, function(x) as.numeric(gsub(" ", "", x)))))
motor_cycle <- data.frame(motor_cycle)
motor_cycle$category <-  "registered motorcycles"
names(motor_cycle) <- names(motor_cycle) %>% gsub(pattern = "X", replacement = "") %>% str_trim()
# pop_density (per sq.km)
pop_density <- read.csv("pop_2017_aseanstat_2008_2017.csv", header=TRUE, sep=",")
names(pop_density) <- names(pop_density) %>% gsub(pattern = "X", replacement ="") %>% str_trim()
names(pop_density)[c(1:2,13)] <- c("country", "total_area_sqkm", "rank_2017")
pop_density[,2:ncol(pop_density)] <- t(data.frame(apply(pop_density[,2:ncol(pop_density)], 1, function(x) as.numeric(gsub(",", "", x)))))
# pop_mid (in 1000)
pop_mid <- read.csv("midpop_2017_aseanstat_2008_2017.csv", header=TRUE, sep=",")
names(pop_mid) <- names(pop_mid) %>% gsub(pattern = "X", replacement ="") %>% str_trim()
names(pop_mid)[1] <- c("country")
pop_mid[,2:ncol(pop_mid)] <- t(data.frame(apply(pop_mid[,2:ncol(pop_mid)], 1, function(x) as.numeric(gsub(",", "", x)))))

## Data Transformation----

# combine pop_density and total_vehicle
total_vehicle.T <- t(total_vehicle)
total_vehicle.T <- data.frame(total_vehicle.T)
colnames(total_vehicle.T) <- total_vehicle.T[1,]
total_vehicle.T <- total_vehicle.T[-c(1),]
total_vehicle.T$country <- rownames(total_vehicle.T)
rownames(total_vehicle.T) <- NULL
total_vehicle <- copy(total_vehicle.T)
new_total_vehicle <- gather(total_vehicle, year, total_vehicle, -c("country"))
new_pop_mid <- gather(pop_mid, year, pop_mid, -c("country"))
new_pop_density <- gather(pop_density, year, pop_density, -c("country", "total_area_sqkm", "rank_2017"))
asean_data <- merge(new_pop_density, new_total_vehicle, by.x = c("country", "year"), by.y = c("country", "year"))
asean_data <- merge(asean_data, new_pop_mid, by.x = c("country", "year"), by.y = c("country", "year") )
asean_data$year <- as.numeric(asean_data$year)
levels(asean_data$country)[levels(asean_data$country)=="Brunei Darussalam"] <- "Brunei"
#new_asean_data <- mutate(asean_data, pop_density = -pop_density)
#new_asean_data <- mutate(asean_data, pop_density = -((pop_density-mean(pop_density)) / sd(pop_density)))
asean_data_2017 <- asean_data %>% filter(year == 2017)

##### TRANSPORT_STAT_BOOK #####
transport_stat_src <- "1_Transport Statistics Malaysia 2018.pdf"
transport_stat_data <- extract_tables(transport_stat_src, pages = 25:28, method = "stream", output = "data.frame")

# accident_state
accident_state <- transport_stat_data[[1]]
colnames(accident_state) <- accident_state[1,]
names(accident_state)[1] <- "State"
accident_state <- accident_state[-c(1,16),-c(2)]
rownames(accident_state) <- NULL
accident_state[,2:ncol(accident_state)] <- t(data.frame(apply(accident_state[,2:ncol(accident_state)], 1, function(x) as.numeric(gsub(",", "", x)))))
accident_state <- accident_state[accident_state$State != "Total",]
# accident_users
accident_users <- transport_stat_data[[2]]
colnames(accident_users) <- accident_users[1,]
accident_users <- accident_users[-c(1),]
accident_users[,2:ncol(accident_users)] <- t(data.frame(apply(accident_users[,2:ncol(accident_users)], 1, function(x) as.numeric(gsub(",", "", x)))))
accident_users$Total <- NULL
rownames(accident_users) <- NULL
names(accident_users)[1] <- "year"
accident_users[,"Others"] <- accident_users[,"Others"] + accident_users[,"Taxi"]
accident_users$Taxi <- NULL
accident_users <- gather(accident_users, vehicle_category, accident_count, -c("year"))
accident_users[,c(1,3)] <- lapply(accident_users[,c(1,3)],function(x){as.numeric(gsub(",", "", x))})
# fatal_state
fatal_state <- transport_stat_data[[3]]
colnames(fatal_state) <- fatal_state[1,]
names(fatal_state)[1] <- "State"
fatal_state <- fatal_state[-c(1,16),-c(2)]
rownames(fatal_state) <- NULL
fatal_state[,2:ncol(fatal_state)] <- t(data.frame(apply(fatal_state[,2:ncol(fatal_state)], 1, function(x) as.numeric(gsub(",", "", x))))) # need to add 2007 & 2098 data?
fatal_state <- fatal_state[fatal_state$State != "Total",]
# total_check <- numcolwise(sum)(fatal_state[1:14,])
# total_given <- fatal_state[fatal_state$State == "Total",-c(1)]
# if (identical(total_check, total_given) == TRUE) {
#   fatal_state <- fatal_state[fatal_state$State != "Total",]
# } else {
#   fatal_state <- copy(fatal_state)
# }

# casualty
casualty <- transport_stat_data[[4]]
casualty[3,1:2] <- casualty[2,1:2]
colnames(casualty) <- casualty[3,]
casualty <- casualty[-c(1:3),]
rownames(casualty) <- NULL
to_separate <- str_split_fixed(casualty$`Serious Minor`, " ", 2)
casualty$Serious <- to_separate[,1]
casualty$Minor <- to_separate[,2]
casualty <- casualty[,-c(4)]
casualty[,] <- t(data.frame(apply(casualty[,], 1, function(x) as.numeric(gsub(",", "", x)))))
casualty$Total_check <- rowSums(cbind(casualty$Death, casualty$Serious, casualty$Minor))
if (identical(casualty$Total, casualty$Total_check) == TRUE) {
  casualty <- casualty[ ,!(names(casualty) == "Total")]
} else {
  casualty_new <- copy(casualty)
}

## Data Transformation----

# combine accident_state and fatal_state (2009 - 2018)
new_accident_state <- gather(accident_state, year, accident_state, -c("State"))
new_fatal_state <- gather(fatal_state, year, fatal_state, -c("State"))
state_data <- merge(new_accident_state, new_fatal_state, by.x = c("State", "year"), by.y = c("State", "year"))
state_data$year <- as.numeric(state_data$year)
# accident_decade (by state)
state_ten <- state_data %>% group_by(State) %>% filter(year == 2009 | year == 2018)
accident_2018 <-  state_ten$accident_state[state_ten$year == "2018"]
accident_2009 <- state_ten$accident_state[state_ten$year == "2009"]
accident_decade <- accident_2018 - accident_2009
# fatal_decade (by state)
fatal_2018 <-  state_ten$fatal_state[state_ten$year == "2018"]
fatal_2009 <- state_ten$fatal_state[state_ten$year == "2009"]
fatal_decade <- fatal_2018 - fatal_2009
# merge accident_decade and fatal_decade
state <- unique(state_ten$State)
state_decade <- list(state, accident_decade, fatal_decade)
state_decade <- as.data.frame(state_decade)
colnames(state_decade) <- c("State", "accident_decade", "fatal_decade")

#### JKJR_STAT_BOOK ####
jkjr_stat_src <- "2_Buku+Statistik+Kemalangan+Jalan+Raya+Tahun+2018+(kemaskini+17.05.2019).pdf"
jkjr_stat_data <- extract_tables(jkjr_stat_src, pages = c(5, 14, 18), output = "data.frame", header = TRUE)

# general_stat
general_stat <- jkjr_stat_data[[1]]
names(general_stat) <- names(general_stat) %>% gsub(pattern = "X", replacement = "") %>% str_trim()
new_row <- names(general_stat)
new_row[2:7] <- gsub(pattern = "\\.", replacement = "", new_row[2:7])
general_stat <- InsertRow(general_stat, NewRow = new_row, RowNum = 1)
colnames(general_stat) <- c("year", "registered_vehicles", "population", "road_crashes", "road_deaths", "serious_injury", 
                            "slight_injury", "index_per_10K_vehicles", "index_per_100K_population", "index_per_B_VKT")
general_stat[20,3] <- c("31,660,000")
general_stat[20,4] <- c("521,466")
general_stat[20,10] <- c("10.70")
general_stat[21,3] <- c("32,049,700")
general_stat[21,4] <- c("533,875")
general_stat[21,6] <- c("3,310")
general_stat[,] <- lapply(general_stat[,],function(general_stat){as.numeric(gsub(",", "", general_stat))})

# fatal_users
fatal_users <- jkjr_stat_data[[3]]
fatal_users[1,] <- paste0(fatal_users[1,], fatal_users[2,])
fatal_users[5,] <- paste0(fatal_users[5,], fatal_users[6,], fatal_users[7,])
fatal_users <- fatal_users[-c(2, 6, 7),]
names(fatal_users) <- (fatal_users[1,])
colnames(fatal_users)[which(colnames(fatal_users) %in% c("KATEGORI", "Jan-Mac", "JUMLAH"))] <- c("vehicle_category", "2019", "total")
fatal_users <- fatal_users[-1,]
fatal_users[,-c(1)] <- lapply(fatal_users[,-c(1)],function(x){as.numeric(gsub(",", "", x))})
vehicle_cat <- c("Car", "Motorcycle", "Pedestrian", "Bicycle", "Bus", "Lorry", "Van", "4WD", "Others", "Total")
fatal_users[,1] <- vehicle_cat
rownames(fatal_users) <-  fatal_users$vehicle_category
fatal_users["Others",2:ncol(fatal_users)] <- fatal_users["Others",2:ncol(fatal_users)] + fatal_users["Pedestrian",2:ncol(fatal_users)]
fatal_users <- fatal_users[rownames(fatal_users) != "Pedestrian", ]
rownames(fatal_users) <- NULL
fatal_users <- fatal_users[-c(9),-c(12,13)]
# fatal_users (transformed)
fatal_users.T <- t(fatal_users)
fatal_users.T <- data.frame(fatal_users.T)
headers <- fatal_users$vehicle_category
colnames(fatal_users.T) <- headers
fatal_users.T$year <- rownames(fatal_users.T)
fatal_users.T <- fatal_users.T[-c(1),]
rownames(fatal_users.T) <- NULL
fatal_users <- copy(fatal_users.T)
fatal_users <- gather(fatal_users, vehicle_category, fatality_count, -c("year"))
fatal_users[,c(1,3)] <- lapply(fatal_users[,c(1,3)],function(x){as.numeric(gsub(",", "", x))})
# fatal_age
fatal_age <- jkjr_stat_data[[4]]
fatal_age[1,] <- paste0(fatal_age[1,], fatal_age[2,])
colnames(fatal_age) <- fatal_age[1,]
fatal_age <- fatal_age[-c(1,2,19),]
fatal_age[,-c(1)] <- lapply(fatal_age[,-c(1)],function(x){as.numeric(gsub(",", "", x))})
fatal_age[,c(1)] <- gsub("\\.", "", fatal_age[,1])
colnames(fatal_age)[1] <- "Age"
fatal_age <- gather(fatal_age, year, fatality_count, -c("Age"))
fatal_age$Age <- as.factor(fatal_age$Age)

## Data Transformation----

# combine fatal_users and accident_users (consistent levels)
accident_users$vehicle_category <- as.factor(accident_users$vehicle_category)
fatal_users$vehicle_category <- as.factor(fatal_users$vehicle_category)
levels(accident_users$vehicle_category)[levels(accident_users$vehicle_category)=="Motocar"] <- "Car"
levels(accident_users$vehicle_category)[levels(accident_users$vehicle_category)=="Four Wheel Drive"] <- "4WD"
accident_data <- merge(accident_users, fatal_users, by.x = c("year", "vehicle_category"), by.y = c("year", "vehicle_category"))
accident_data <- accident_data %>%
  mutate(text1 = paste0("x: ", year, "\n", "y: ", vehicle_category, "\n", "Value: ", accident_count)) %>%
  mutate(text2 = paste0("x: ", year, "\n", "y: ", vehicle_category, "\n", "Value: ", fatality_count))
  
  
#### WEB_SOURCES ####
# asean_info
url <- 'https://en.wikipedia.org/wiki/Southeast_Asia'
webpage <- read_html(url)
asean_info <- webpage %>% html_nodes('table') %>% .[2] %>% html_table(fill = TRUE, header = NA, trim = TRUE)
asean_info <- as.data.frame(asean_info)
asean_info <- asean_info[-c(3),]
rownames(asean_info) <- NULL
colnames(asean_info) <- c("Country", "Area_Km2", "Population_2018", "Density_perKm2", "GDP_nominal_2019_USD", 
                          "GDP_PPP_percapita_2019_USD", "HDI_2018", "Capital")
asean_info[["Area_Km2"]] <- str_remove(asean_info$Area_Km2, "\\[\\w+\\]")
asean_info[["GDP_PPP_percapita_2019_USD"]] <- str_remove(asean_info$GDP_PPP_percapita_2019_USD, "\\$")
asean_info[5,8] <- "Kuala Lumpur"
asean_info[,2:6] <- lapply(asean_info[,2:6],function(x){as.numeric(gsub(",", "", x))})
country_code <- c("BN", "KH", "ID", "LA", "MY", "MM", "PH", "SG", "TH", "VN")
asean_info$id <- country_code
# extract total_vehicle (per 1000 population) from total_vehicle (2017)
asean_vehicle <- total_vehicle[,c("country", "2017")]
asean_vehicle$country[asean_vehicle$country == "Brunei Darussalam"] <- "Brunei"
asean_vehicle$country[asean_vehicle$country == "Viet Nam"] <- "Vietnam"
asean_vehicle$country[asean_vehicle$country == "Lao PDR"] <- "Laos"
names(asean_vehicle)[1] <- "Country"
asean_info <- merge(asean_info, asean_vehicle, by.x = c("Country"), by.y = ("Country"))
colnames(asean_info)[10] <- "Total_vehicle_2017"









