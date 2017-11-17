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

index_raw_data = read.csv("IDXH_IMP_MTD_S_P.csv", header = TRUE, stringsAsFactors = FALSE)
index_raw_data$rid = substr(index_raw_data$region_id,7,11)
index_raw_data$log_value = log(index_raw_data$calculation_value)
index_raw_data$value_at_date2 = as.Date(index_raw_data$value_at_date)
index_raw_data_1990 = index_raw_data[year(index_raw_data$value_at_date2)>=1990,]
index_raw_data_1990_sort =  index_raw_data_1990[order(index_raw_data_1990$region_id, index_raw_data_1990$value_at_date2, decreasing = FALSE),]

SA3_regions = read.csv("SA3_2016_AUST.csv", header = TRUE, stringsAsFactors = FALSE)

index_raw_data_1990_sort_merge = merge(index_raw_data_1990_sort, SA3_regions, by.x = "rid", by.y = "SA3_CODE_2016", all.x = TRUE)


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


calc = function(x){
  
  n_obs  = length(x$value_at_date2)
  x$log_ret[2:n_obs] = diff(x$log_value)
  y = as.factor(ifelse(x$log_ret[2:n_obs] > 0, 1, -1))
 m = runs.test(y)
 ml = runs.test(y,alternative = "less")
 mg = runs.test(y, alternative = "greater")
 
 return(rbind(m,ml,mg,n_obs))
}

index_raw_data_1990_sort_split = split(index_raw_data_1990_sort_merge , list(index_raw_data_1990_sort_merge$region_id), drop = TRUE)

test.split = index_raw_data_1990_sort_split[1:2]
result = sapply(test.split, calc)
write.csv(result_t, file = "runs.test_results.csv")

bootstrap_auto_variance_ratio_test$rid = substr(bootstrap_auto_variance_ratio_test$X1,7,11)
bootstrap_auto_variance_ratio_test_regname = merge(bootstrap_auto_variance_ratio_test, SA3_regions, by.x = "rid", by.y = "SA3_CODE_2016", all.x = TRUE)
write.csv(bootstrap_auto_variance_ratio_test_regname, file = "bootstrap_AVR.csv")

test.split = index_raw_data_1990_sort_split
calc = function(x){
  n_obs  = length(x$log_value)
# print(Auto.VR(diff(x$log_value)))
  m = AutoBoot.test(diff(x$log_value),500, wild = "Normal", prob = c(0.025,0.975))
# print(class(m))
 return(c( m, n_obs))
}
result = t(sapply(test.split, calc))

write.csv(result, file = "bootstrap_auto_variance_ratio_test.csv")
index_raw_data_sort[index_raw_data_sort$region_id == 'R_SA3_10102_D',]

    result_t = t(result)



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

