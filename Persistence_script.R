Sys.setenv('http_proxy' = 'http://10.74.1.25:8080/')
Sys.setenv('https_proxy' = 'http://10.74.1.25:8080/') 

install.packages("stargazer")
library("stargazer")
install.packages("lubridate")
library("lubridate")
install.packages("vrtest")
library(vrtest)
install.packages("emh")
library(devtools)
devtools::install_github(repo="stuartgordonreid/emh")
install.packages("tseries")
library(tseries)


elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

SA3_regions = read.csv("SA3_2016_AUST.csv", header = TRUE, stringsAsFactors = FALSE)

## capital

IDXH_IMP_MTD_S_P = read.csv("IDXH_IMP_MTD_S_P_SA4.csv", header = TRUE, stringsAsFactors = FALSE)
IDXH_IMP_MTD_S_P$rid = substr(IDXH_IMP_MTD_S_P$region_id,7,9)
IDXH_IMP_MTD_S_P$log_value = log(IDXH_IMP_MTD_S_P$calculation_value)
IDXH_IMP_MTD_S_P$value_at_date2 = as.Date(IDXH_IMP_MTD_S_P$value_at_date)
IDXH_IMP_MTD_S_P_1991 = IDXH_IMP_MTD_S_P[year(IDXH_IMP_MTD_S_P$value_at_date2)>=1991,]
IDXH_IMP_MTD_S_P_1991_SORT =  IDXH_IMP_MTD_S_P_1991[order(IDXH_IMP_MTD_S_P_1991$region_id, IDXH_IMP_MTD_S_P_1991$value_at_date2, decreasing = FALSE),]
IDXH_IMP_MTD_S_P_1991_SORT_Merge = merge(IDXH_IMP_MTD_S_P_1991_SORT, SA3_regions, by.x = "rid", by.y = "SA4_CODE_2016", all.x = TRUE)



  raw_index = index_raw_data[index_raw_data$region_id == 'R_SA3_70202_D' & year(index_raw_data$value_at_date2) >= 1997 ,]
  n_obs  = length(raw_index$value_at_date)
  raw_index = raw_index[order(raw_index$value_at_date, decreasing = FALSE),]

  plot(ts(diff(raw_index$log_value)), type = 'l', lwd = 2, col = "red", xlab = "TIme", ylab = "Log Index")

  raw_index$log_ret[2:n_obs] = diff(raw_index$log_value)

  raw_index$log_ret2[2:n_obs] = raw_index$log_value[2:n_obs] - raw_index$log_value[1:(n_obs-1)]

  head(raw_index)

  x = factor(ifelse(raw_index$log_ret[2:n_obs] > 0, 1, -1))
  m = runs.test(x)
  ml = runs.test(x, "l")
  mg = runs.test(x, "g")
  
  m


calc_runs = function(x){
  
  n_obs  = length(x$value_at_date2)
  x$log_ret[2:n_obs] = diff(x$log_value)
  y = as.factor(ifelse(x$log_ret[2:n_obs] > 0, 1, -1))
 m = runs.test(y)
 ml = runs.test(y,alternative = "less")
 mg = runs.test(y, alternative = "greater")
 
 return(rbind(m,ml,mg,n_obs))
}

## below is capital
IDXH_IMP_MTD_S_P_1991_SORT_Merge_SPLIT = split(IDXH_IMP_MTD_S_P_1991_SORT_Merge , list(IDXH_IMP_MTD_S_P_1991_SORT_Merge$region_id), drop = TRUE)
test.split_runs = IDXH_IMP_MTD_S_P_1991_SORT_Merge_SPLIT[1:2]
result = sapply(test.split_runs, calc_runs)
write.csv(result_t, file = "runs.test_results.csv")

bootstrap_auto_variance_ratio_test$rid = substr(bootstrap_auto_variance_ratio_test$X1,7,11)
bootstrap_auto_variance_ratio_test_regname = merge(bootstrap_auto_variance_ratio_test, SA3_regions, by.x = "rid", by.y = "SA3_CODE_2016", all.x = TRUE)
write.csv(bootstrap_auto_variance_ratio_test_regname, file = "bootstrap_AVR.csv")



calc_bootVR = function(x){
  x= na.remove(x)
  n_obs  = length(x$log_value)
    # print(Auto.VR(diff(x$log_value)))
    # print(class(m))
  m = AutoBoot.test(diff(x$log_value),500, wild = "Normal", prob = c(0.025,0.975))
  x$log_ret[2:n_obs] = diff(x$log_value)
  y = as.factor(ifelse(x$log_ret[2:n_obs] > 0, 1, -1))
  n = runs.test(y)
  ml = runs.test(y,alternative = "less")
  mg = runs.test(y, alternative = "greater")
 c(m, n_obs, n, ml, mg)
}

SA4_regions = unique(SA3_regions[,c(3:8)])

## below is for capital
IDXH_IMP_MTD_S_P_1991_SORT_Merge_SPLIT = split(IDXH_IMP_MTD_S_P_1991_SORT_Merge , list(IDXH_IMP_MTD_S_P_1991_SORT_Merge$region_id), drop = TRUE)
test.split_VR = IDXH_IMP_MTD_S_P_1991_SORT_Merge_SPLIT#[1:2]
result = t(sapply(test.split_VR, calc_bootVR))

result.df = as.data.frame(result)
result.df$SA4_id = substr(row.names(result.df),7,9)
result.df = merge(result.df, SA4_regions, by.x = "SA4_id", by.y = "SA4_CODE_2016", all.x = TRUE )
result.df_mat = as.matrix(result.df)
write.csv(result.df_mat, file = "bootstrap_AVR_and_runs_test.csv")

str(result)

## below is for rental..

## below is rental

IDXH_IMP_YIELD_MTD_S_P = read.csv("IDXH_IMP_YIELD_MTD_S_P.csv", header = TRUE, stringsAsFactors = FALSE)
IDXH_IMP_YIELD_MTD_S_P$rid = substr(IDXH_IMP_YIELD_MTD_S_P$region_id,7,11)
IDXH_IMP_YIELD_MTD_S_P$log_value = log(IDXH_IMP_YIELD_MTD_S_P$calculation_value)
IDXH_IMP_YIELD_MTD_S_P$value_at_date2 = as.Date(IDXH_IMP_YIELD_MTD_S_P$value_at_date)
IDXH_IMP_YIELD_MTD_S_P_2005 = IDXH_IMP_YIELD_MTD_S_P[IDXH_IMP_YIELD_MTD_S_P$value_at_date2>= as.Date("2005-05-01") & !is.na(IDXH_IMP_YIELD_MTD_S_P$confidence),]
IDXH_IMP_YIELD_MTD_S_P_2005_SORT =  IDXH_IMP_YIELD_MTD_S_P_2005[order(IDXH_IMP_YIELD_MTD_S_P_2005$region_id, IDXH_IMP_YIELD_MTD_S_P_2005$value_at_date2, decreasing = FALSE),]

IDXH_IMP_YIELD_MTD_S_P_2005_SORT_Merge = merge(IDXH_IMP_YIELD_MTD_S_P_2005_SORT, SA3_regions, by.x = "rid", by.y = "SA3_CODE_2016", all.x = TRUE)


IDXH_IMP_YIELD_MTD_S_P_2005_SORT_Merge_SPLIT = split(IDXH_IMP_YIELD_MTD_S_P_2005_SORT_Merge , list(IDXH_IMP_YIELD_MTD_S_P_2005_SORT_Merge$region_id), drop = TRUE)
test.split_VR = IDXH_IMP_YIELD_MTD_S_P_2005_SORT_Merge_SPLIT#[1:15]
result = t(sapply(test.split_VR, calc_bootVR))
write.csv(result, file = "bootstrap_auto_variance_ratio_test_rental.csv")




    index_raw_data_sort[index_raw_data_sort$region_id == 'R_SA3_10102_D',]



test.split_2 = index_raw_data_1990_sort_split[1:2]
test = index_raw_data_1990_sort[index_raw_data_1990_sort$region_id == 'R_SA3_10102_D',]


install.packages('sp')
library(sp)
install.packages("maptools")
library(maptools)

install.packages("libgdal1-dev")

install.packages("rgdal")

install.packages("dev")
require(rgdal)
 install.packages('rgdal', type = "source")
install.packages('rgdal',repos="http://www.stats.ox.ac.uk/pub/RWin")
install.packages("maps")
library(maps)
library(sp)

search()

ozDATA<-readShapeSpatial("SA3_2016_AUST")

install.packages("tmaptools"); library("tmaptools")

install.packages("tmap")
install.packages("tmaptools")
install.packages("sf")
install.packages("leaflet")
library("tmap")
library("tmaptools")
library("sf")
library("leaflet")

###############
# also produce descriptive test statistics for all regions -- mean, variance , kurtosis, skewness and JB test 
## test statistics for normality test.. also perform sharpe's ratios.. to assess the 

Auto.Q(log_ret,12)
m = AutoBoot.test(diff(log_value, lag = 1),nboot = 500, wild = "Normal")
Boot.test(log_ret, c(2,5,10),nboot = 100, wild = "No")
###########333333333




log_value = (test$log_value)

m = Auto.VR(diff(log_value))

plot(diff(log_ret, differences = 1), type = 'l')

m

class(m)

i=1
rm(x)
region = sort(unique(index_raw_data$region_id)) #   c('R_SA3_11703_D', 'R_SA3_30102_D')

x = numeric()

length(unique(index_raw_data$region_id))

for (i in seq_along(region))
     { 
  raw_index = index_raw_data[index_raw_data$region_id == region[i],]
  
       n_obs  = length(raw_index$value_at_date)
       raw_index = raw_index[order(raw_index$value_at_date, decreasing = FALSE),]
       raw_index$log_ret[2:n_obs] = raw_index$log_value[2:n_obs] - raw_index$log_value[1:(n_obs-1)]
       
       d = factor(ifelse(raw_index$log_ret[2:n_obs] > 0, 1, -1))
       m = runs.test(d)
       ml = runs.test(d, "l")
       mg = runs.test(d, "g")
       
  mi = c(m$p.value , ml$p.value, mg$p.value)
  
  
  x = data.frame(rbind(x , mi))
  y = region
}


write.csv(x, file = "runs_test_SA3.csv")

write.csv(region, file = "regions_SA3.csv")

m[3]


Auto.VR(raw_index[year(raw_index$value_at_date2) >= 1990,]$log_ret)
#r = rnorm(100)
#AutoBoot.test(r, nboot = 500, wild = "Normal")
AutoBoot.test(raw_index[year(raw_index$value_at_date2) >= 1990,]$log_ret, nboot = 500, wild = "Normal")

