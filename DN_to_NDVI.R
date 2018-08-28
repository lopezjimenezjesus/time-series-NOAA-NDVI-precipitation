##################################################
## Project: LUMA GIS: 13. Data Integration
## Script purpose: Time Series Analysis
## Date:
## Author: Jesús
##################################################


## Section: Cleaning
##################################################

# read and clean csv file

require(dplyr)
require(reshape2)

ndvi.clst <- read.csv('data/NDVI_climate_stations.txt', sep = ',')

ndvi.clst <- ndvi.clst[1:34, 2:27]
class(ndvi.clst)
head(ndvi.clst)

n <- length(colnames(ndvi.clst))

# function to calculate NDVI from DN

toNDVI <- function(x) {
  
  (x-128) * 0.008
}


# apply toNDVI function to columns


require(dplyr)
ndvi.clst <- dplyr::select(ndvi.clst,  c(1:23, 25:26))
foo <- apply(ndvi.clst[, 4:22], 2 , toNDVI)
foo <- as.data.frame(foo)

foo2 <- dplyr::select(ndvi.clst, c(1,2,3,24,25))
ndvi.clst <- cbind(foo2, foo)
colnames(ndvi.clst)[6:24] <- as.character(c(1:19))

head(ndvi.clst)
tail(ndvi.clst)


# convert to long format

ndvi.clst.long <- melt(ndvi.clst, id.vars = c("station_na", "station_nu","CLIMATE_STATIONS", "X", "Y"),variable.name = "year", value.name = "ndvi" )

# plot 

require(ggplot2)
ggplot(ndvi.clst.long, aes(x=year, y=ndvi)) +
  geom_point()


## Section: Linear model; simple linear regression per point (station)
##################################################


ndvi.lm <- lm(formula = ndvi ~ station_na, data = ndvi.clst.long)


summary(ndvi.lm)

# this model is wrong. Need a linear regresion per station

library(lme4)
library(lattice)
ndvi.clst.long$year <- as.numeric(levels(ndvi.clst.long$year))[ndvi.clst.long$year]
xyplot(ndvi ~ year, groups=station_na, data=ndvi.clst.long, type='l')
fits <- lmList(ndvi ~ year | station_na, data=ndvi.clst.long)
fits
coef(fits)

# plot 

ggplot(ndvi.clst.long, aes(x=year, y=ndvi, group= station_na)) +
  geom_point() +
  geom_line() +
  stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95) +
  facet_wrap(~station_na)


# build a data frame to plot trends per station

# use levels to maintain name value par 

# check this way
# station = levels(ndvi.clst.long$station_na)
# station_2 = variable.names(ndvi.lm)

fits.out.sum <- summary(fits)

df <- data.frame(station = names(fits), slope = fits.out.sum$coefficients[,,2][,1])



ggplot(df, aes(x=station, y=slope)) +
  geom_bar(stat = "identity", position = "identity") +
  labs(x="Stations", y="NDVI Trend") +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))


## Section: Time series 
##################################################
# Produce a time series graph (year on the x-axis) for the station
# showing the highest positive linear trend of NDVI and include the trend line
# equation and R 2 . What land cover type surrounds this station?

name.st <-  as.character(df[which.max(df$slope),]$station)

max.station <- ndvi.clst.long[ndvi.clst.long$station_na==name.st,]
max.station.lm <- lm(max.station$ndvi ~ max.station$year)

# Manual example
coef(max.station.lm)[1] + 12*coef(max.station.lm)[2]

intercept <- coef(max.station.lm)[1]
slope <- coef(max.station.lm)[2]


ii <- 1982:2000
ggplot(max.station, aes(x=year, y=ndvi, group=station_na)) +
  geom_point() +
  geom_line() +
  stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95) +
  labs(title=paste0('NDVI time series (',name.st, ' station)'), x="Year", y = "NDVI", caption =  bquote(bold(~R^2~'= 0.7162; Adjusted' ~R^2~'=  0.6995;' ~Y~'=-0.2952 + 0.0125X'))) +
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5), plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(labels = ii, breaks=seq(ii))


# Get the Land Use of station

require(raster)
require(sf)
r.1 <- raster("data/r_1.img")
lu <- st_read("data/LandUse_1992.shp")
cl <- st_read("data/climate_stations.shp")

lu.cl.inter <- st_intersection(lu, cl) 
st_geometry(lu.cl.inter) <- NULL
lu.cl.inter[lu.cl.inter$station_na==name.st,]$LU_NAME # get the land use


## Section: Task 3
##################################################
# Task 3. What is the highest positive gain value of NDVI in the image?
# Additionally describe what the gain value means in simple words
# understandable to a non-scientific person.


# Gain coefficient alfa offset and beta gain coef
#Need Y and X


years <- 1982:2000
n <- 1:19
n.mean <- mean(n)
n.diff <- (n - n.mean)
n.diff2 <- (n - n.mean)^2 

df.x <- data.frame(years, n, n.diff, n.diff2)

sum(n.diff2)

## Section: Task 7 Correlation NDVI / Precipitation
##################################################


## Stratified random sampling
##################################################


# Stratified random sampling


set.seed(14)

luToJoin <- lu.cl.inter[, c(8:9, 15)]

ndvi.clst.lu <-left_join(ndvi.clst, luToJoin, "station_na")

ndvi.clst.lu.sample <- ndvi.clst.lu %>% group_by(MAJ_LUNAME) %>% sample_n(1) # sampling


ndvi.clst.lu.sample <- melt(data = ndvi.clst.lu.sample, 
                         id.vars = c("CLIMATE_STATIONS", "X", "Y","station_nu", "station_na", "MAJ_LUNAME", "LU_NAME"),
                         variable.name = "year",value.name = "ndvi")


require(tidyverse)

# read xls

require(readxl) 

path <- "IM_rain.xls"

rain.ts <- path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path)

class(rain.ts)

require(plyr)

# merge dataframe and remove columns rows

rain.ts.df <- ldply(rain.ts, data.frame)
rain.ts.df <- rain.ts.df[,c(1:18)]
colnames(rain.ts.df)[2] <- "station.number"
rain.ts.df <- rain.ts.df[-c(35,36,37),]

# add year
rain.ts.df$.id <- as.numeric(rain.ts.df$.id)
rain.ts.df$year <- rain.ts.df$.id

#rain.ts.df$year <- rep(1982:1999, 1, each =34)


# order columns

rain.ts.df <- rain.ts.df[,c(2,3,4,5,6, 19,7,8,9,10,11,12,13,14,15,16,17, 18)]


# tidy format

require(reshape2)
names(rain.ts.df)[1:3] <- c("number", "station.number", "station.name")
rain.ts.df <- melt(rain.ts.df, id.vars = c("year","number", "station.number", "station.name","decilat", "decilong"),variable.name = "month")

rain.ts.df$value <- as.numeric(rain.ts.df$value)
write.csv(rain.ts.df, file = 'rain1989_1999.csv')


# accumulate monthly rainfall data to annual rainfall, or
# accumulated seasonal rainfall (e.g. Jan – September)


# define seasons

rain.ts.df <-    rain.ts.df %>%
  mutate(season = 
           ifelse(month %in% c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep'), "Season 1",
                  ifelse(month %in% c('Oct', 'Nov', 'Dec'), "Season 2",
                         NA)))






# precipitation mean by season

rain.ts.season.df <- ddply(rain.ts.df, .(number, station.number, station.name, decilat, decilong, year, season), summarize, mean=mean(value))

# mean by year

rain.ts.month.df <- ddply(rain.ts.df, .(year, month), summarize, mean=mean(value, na.rm=TRUE))
rain.ts.month.df$month_n <- rep(1:12, 18) 
require(lubridate)


rain.ts.month.df <- rain.ts.month.df %>%
  mutate(date = as.Date(paste0(rain.ts.month.df$year, "-", rain.ts.month.df$month_n, "-", "01"))) 


min <- as.Date("1989-1-1")
max <-  as.Date("1999-12-1")

# http://www.sthda.com/english/articles/32-r-graphics-essentials/128-plot-time-series-data-using-ggplot/
ggplot(rain.ts.month.df, aes(x = date, y= mean)) + 
  geom_line(color = "#00AFBB", size = 1)+ 
  stat_smooth(method = "lm", color = "#FC4E07", fill = "#FC4E07", formula = y ~ x, se=F,level=0.95) +
  scale_x_date(date_labels = "%b/%Y", breaks = c(min, max)) +
  labs(title="Monthly precipitation values (mm)", x="", y="Precipitation") + 
  theme_minimal()



# Join selected stations with data

ndvi.clst.lu.sample$year <- rep(1982:2000, times=1, each=5)

# year 2000 is not present in precipitation df
ndvi.clst.lu.sample <- filter(ndvi.clst.lu.sample, year %in% 1982:1999)

# unique code to filter
code <- unique(ndvi.clst.lu.sample$station_na)

station.filtered <- filter(rain.ts.season.df, station.name %in% code & season %in% "Season 1")


# time to bind once both has the same lenght


station.filtered
ndvi.clst.sample


station.merged <- left_join(station.filtered, ndvi.clst.lu.sample, by = c("station.number" = "station_nu", "year"="year"), type = "left", match = "all")

station.merged <- station.merged[,c(1:8, 13:15)]


# get land use (already done)
# station.merged.sf <- st_as_sf(station.merged, coords = c("decilong", "decilat"), crs = 4326, agr = "constant")
# station.merged.sf <- st_intersection(lu, station.merged.sf)
# st_geometry(station.merged.sf) <- NULL
# as.character(unique(station.merged.sf[station.merged.sf$station.name=="Linxi",]$LU_NAME))
# 

# Correlation aqui

fits <- lmList(ndvi ~ mean | station.name, data=station.merged)
fits


fits.out.sum <- summary(fits)

df <- data.frame(station = names(fits), slope = fits.out.sum$coefficients[,,2][,1], alfa=fits.out.sum$coefficients[,,1][,1])

# get R-squared
# https://stackoverflow.com/questions/23501852/print-r-squared-for-all-of-the-models-fit-with-lmlist

rsquared <- sapply(fits,function(x) summary(x)$r.squared)


# plot
require(ggplot2)
ggplot(station.merged, aes(x=mean, y=ndvi)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95) +
  facet_wrap(~station.name)

# Facet by station
require(ggplot2)
ggplot(station.merged, aes(x=year, y=mean)) +
  geom_bar(stat = "identity") +
  geom_point(aes(x=year, y =ndvi)) +
  stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95) +
  facet_wrap(~station.name)

# Facet by Land use
ggplot(station.merged, aes(x=mean, y=ndvi)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95) +
  facet_wrap(~MAJ_LUNAME)

# Facet by Land use
ggplot(station.merged, aes(x=mean, y=ndvi)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95) +
  facet_wrap(station.name~MAJ_LUNAME)


ggplot(station.merged, aes(x= station.name, y=mean)) +
  geom_bar(stat = "identity") 

rsquared

intercept <- coef(max.station.lm)[1]
slope <- coef(max.station.lm)[2]

lu <- unique(station.merged[c("MAJ_LUNAME", "station.name")])

# as separate groups

for (var in unique(station.merged$station.name)) {
  
  if(df[df$station==var,"slope"]>0){
    caption <- as.expression(
      bquote(
        R^2 == .(round(rsquared[var],4)) ~ 
          Y == .(round(df[df$station==var,"alfa"],4)) +
          .(round(df[df$station==var,"slope"],4))*X))
  } else {
    caption <- as.expression(
      bquote(
        R^2 == .(round(rsquared[var],4)) ~ 
          Y == .(round(df[df$station==var,"alfa"],4)) -
          .(abs(round(df[df$station==var,"slope"],4)))*X))
  }
  
  
  print( ggplot(station.merged[station.merged$station.name==var,], aes(mean, ndvi)) 
         + geom_point() +
           stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95) +
           labs(title= 'Rainfall against NDVI', 
                subtitle = paste0(var,' station; Land use: ', as.character(lu$MAJ_LUNAME[lu$station.name==var])),
                x="Rainfall", y = "NDVI",
                caption = caption) +
           scale_x_continuous(labels = seq(1:22),breaks = seq(1:22)))
  #ggsave(last_plot(), filename = paste0('task7_', str_replace_all(var, " ", "_"), ".png"))
}

ggplot(station.merged, aes(mean, ndvi, col=station.name)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95) +
  labs(title= 'Rainfall against NDVI') +
  scale_x_continuous()

# plot by Land Use

ggplot(station.merged.sf, aes(year, ndvi, col=LU_NAME)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95) +
  labs(title= 'NDVI trend') +
  scale_x_continuous()


#Weird

# use accumulative precipitation per station?

# na values as 0
station.merged[ is.na(station.merged) ] <- 0

station.merged
station.merged <- station.merged %>%
  group_by(station.name) %>%
  arrange(station.name, year) %>%
  mutate(rain.acu=cumsum(mean))


# Correlation
station.merged <- as.data.frame(station.merged)

fits <- lmList(ndvi ~ rain.acu | station.name, data=station.merged)
fits


fits.out.sum <- summary(fits)

df <- data.frame(station = names(fits), slope = fits.out.sum$coefficients[,,2][,1], alfa=fits.out.sum$coefficients[,,1][,1])

# get R-squared
# https://stackoverflow.com/questions/23501852/print-r-squared-for-all-of-the-models-fit-with-lmlist

rsquared <- sapply(fits,function(x) summary(x)$r.squared)


for (var in unique(station.merged$station.name)) {
  
  if(df[df$station==var,"slope"]>0){
    caption <- as.expression(
      bquote(
        R^2 == .(round(rsquared[var],4)) ~ 
          Y == .(round(df[df$station==var,"alfa"],4)) +
          .(round(df[df$station==var,"slope"],4))*X))
  } else {
    caption <- as.expression(
      bquote(
        R^2 == .(round(rsquared[var],4)) ~ 
          Y == .(round(df[df$station==var,"alfa"],4)) -
          .(abs(round(df[df$station==var,"slope"],4)))*X))
  }
  
  
  print( ggplot(station.merged[station.merged$station.name==var,], aes(rain.acu, ndvi)) 
         + geom_point() +
           stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95) +
           labs(title= 'Rainfall against NDVI', 
                subtitle = paste0(var,' station; Land use: ', as.character(lu$LU_NAME[lu$station.name==var])),
                x="Rainfall", y = "NDVI",
                caption = caption) +
           scale_x_continuous()
  )
  ggsave(last_plot(), filename = paste0('task7_', str_replace_all(var, " ", "_"), ".png"))
}

##################################################################################################

## Section:  Check station with agriculture land use
##################################################

st.Ch <- filter(rain.ts.season.df, station.name %in% "Chifeng" & season %in% "Season 1")


# time to bind once both has the same lenght


station.filtered
ndvi.clst.sample

st.Ch$year <- seq(1982:1999)

st.Ch.merged <- left_join(st.Ch, ndvi.clst.long, by = c("station.number" = "station_nu", "year"="year"), type = "left", match = "all")

#st.Ch.merged <- st.Ch.merged[,c(1:8, 13)]


# get land use
st.Ch.merged.sf <- st_as_sf(st.Ch.merged, coords = c("decilong", "decilat"), crs = 4326, agr = "constant")
st.Ch.merged.sf <- st_intersection(lu, st.Ch.merged.sf)
st_geometry(st.Ch.merged.sf) <- NULL
as.character(unique(st.Ch.merged.sf[st.Ch.merged.sf$station.name=="Chifeng",]$LU_NAME))


# Correlation

fits <- lm(formula = ndvi ~ mean,data=st.Ch.merged.sf)
fits


fits.out.sum <- summary(fits)

df <- data.frame(station = "Chifeng", slope = fits.out.sum$coefficients[2], alfa=fits.out.sum$coefficients[1])

# get R-squared
# https://stackoverflow.com/questions/23501852/print-r-squared-for-all-of-the-models-fit-with-lmlist

rsquared <- summary(fits)$r.squared


# plot
require(ggplot2)
ggplot(st.Ch.merged.sf, aes(x=mean, y=ndvi)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95)



for (var in unique(st.Ch.merged$station.name)) {
  
  if(df[df$station==var,"slope"]>0){
    caption <- as.expression(
      bquote(
        R^2 == .(round(rsquared[var],4)) ~ 
          Y == .(round(df[df$station==var,"alfa"],4)) +
          .(round(df[df$station==var,"slope"],4))*X))
  } else {
    caption <- as.expression(
      bquote(
        R^2 == .(round(rsquared[var],4)) ~ 
          Y == .(round(df[df$station==var,"alfa"],4)) -
          .(abs(round(df[df$station==var,"slope"],4)))*X))
  }
  
  
  print( ggplot(st.Ch.merged[st.Ch.merged$station.name==var,], aes(mean, ndvi)) 
         + geom_point() +
           stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95) +
           labs(title= 'Rainfall against NDVI', 
                subtitle = paste0(var,' station; Land use: ', as.character(lu$LU_NAME[lu$station.name==var])),
                x="Rainfall", y = "NDVI",
                caption = caption) +
           scale_x_continuous())
  ggsave(last_plot(), filename = paste0('task7_', str_replace_all(var, " ", "_"), ".png"))
}


## Random sampling 
##################################################

# Random sampling 

set.seed(3)



ndvi.clst.sample <- ndvi.clst[sample(nrow(ndvi.clst), 5), ]

require(reshape2)
ndvi.clst.sample <- melt(data = ndvi.clst.sample, 
                         id.vars = c("CLIMATE_STATIONS", "X", "Y", "station_nu", "station_na"),
                         variable.name = "year",value.name = "ndvi")

require(tidyverse)

# read xls

require(readxl) 

path <- "IM_rain.xls"

rain.ts <- path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path)

class(rain.ts)

require(plyr)

# merge dataframe and remove columns rows

rain.ts.df <- ldply(rain.ts, data.frame)
rain.ts.df <- rain.ts.df[,c(1:18)]
colnames(rain.ts.df)[2] <- "station.number"
rain.ts.df <- rain.ts.df[-c(35,36,37),]

# add year
rain.ts.df$.id <- as.numeric(rain.ts.df$.id)
rain.ts.df$year <- rain.ts.df$.id

#rain.ts.df$year <- rep(1982:1999, 1, each =34)


# order columns

rain.ts.df <- rain.ts.df[,c(2,3,4,5,6, 19,7,8,9,10,11,12,13,14,15,16,17, 18)]


# tidy format

require(reshape2)
names(rain.ts.df)[1:3] <- c("number", "station.number", "station.name")
rain.ts.df <- melt(rain.ts.df, id.vars = c("year","number", "station.number", "station.name","decilat", "decilong"),variable.name = "month")

rain.ts.df$value <- as.numeric(rain.ts.df$value)
write.csv(rain.ts.df, file = 'rain1989_1999.csv')


# accumulate monthly rainfall data to annual rainfall, or
# accumulated seasonal rainfall (e.g. Jan – September)


# define seasons

rain.ts.df <-    rain.ts.df %>%
    mutate(season = 
          ifelse(month %in% c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep'), "Season 1",
          ifelse(month %in% c('Oct', 'Nov', 'Dec'), "Season 2",
          NA)))






# precipitation mean by season

rain.ts.season.df <- ddply(rain.ts.df, .(number, station.number, station.name, decilat, decilong, year, season), summarize, mean=mean(value))

# mean by year

rain.ts.month.df <- ddply(rain.ts.df, .(year, month), summarize, mean=mean(value, na.rm=TRUE))
rain.ts.month.df$month_n <- rep(1:12, 18) 
require(lubridate)


rain.ts.month.df <- rain.ts.month.df %>%
  mutate(date = as.Date(paste0(rain.ts.month.df$year, "-", rain.ts.month.df$month_n, "-", "01"))) 


min <- as.Date("1989-1-1")
max <-  as.Date("1999-12-1")

# http://www.sthda.com/english/articles/32-r-graphics-essentials/128-plot-time-series-data-using-ggplot/
ggplot(rain.ts.month.df, aes(x = date, y= mean)) + 
  geom_line(color = "#00AFBB", size = 1)+ 
  stat_smooth(method = "lm", color = "#FC4E07", fill = "#FC4E07", formula = y ~ x, se=F,level=0.95) +
  scale_x_date(date_labels = "%b/%Y", breaks = c(min, max)) +
  labs(title="Monthly precipitation values (mm)", x="", y="Precipitation") + 
  theme_minimal()



# Join selected stations with data

ndvi.clst.sample$year <- rep(1982:2000, times=1, each=5)

# year 2000 is not present in precipitation df
ndvi.clst.sample <- filter(ndvi.clst.sample, year %in% 1982:1999)

# unique code to filter
code <- unique(ndvi.clst.sample$station_na)

station.filtered <- filter(rain.ts.season.df, station.name %in% code & season %in% "Season 1")


# time to bind once both has the same lenght


station.filtered
ndvi.clst.sample


station.merged <- left_join(station.filtered, ndvi.clst.sample, by = c("station.number" = "station_nu", "year"="year"), type = "left", match = "all")

station.merged <- station.merged[,c(1:8, 13)]


# get land use
station.merged.sf <- st_as_sf(station.merged, coords = c("decilong", "decilat"), crs = 4326, agr = "constant")
station.merged.sf <- st_intersection(lu, station.merged.sf)
st_geometry(station.merged.sf) <- NULL
as.character(unique(station.merged.sf[station.merged.sf$station.name=="Linxi",]$LU_NAME))


# Correlation

fits <- lmList(ndvi ~ mean | station.name, data=station.merged.sf)
fits


fits.out.sum <- summary(fits)

df <- data.frame(station = names(fits), slope = fits.out.sum$coefficients[,,2][,1], alfa=fits.out.sum$coefficients[,,1][,1])

# get R-squared
# https://stackoverflow.com/questions/23501852/print-r-squared-for-all-of-the-models-fit-with-lmlist

rsquared <- sapply(fits,function(x) summary(x)$r.squared)


# plot
require(ggplot2)
ggplot(station.merged.sf, aes(x=mean, y=ndvi)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95) +
  facet_wrap(~station.name)

# Facet by station
require(ggplot2)
ggplot(station.merged.sf, aes(x=year, y=mean)) +
  geom_bar(stat = "identity") +
  geom_point(aes(x=year, y =ndvi)) +
  stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95) +
  facet_wrap(~station.name)

# Facet by Land use
ggplot(station.merged.sf, aes(x=mean, y=ndvi)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95) +
  facet_wrap(~MAJ_LUNAME)

# Facet by Land use
ggplot(station.merged.sf, aes(x=mean, y=ndvi)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95) +
  facet_wrap(station.name~MAJ_LUNAME)


ggplot(station.merged, aes(x= station.name, y=mean)) +
  geom_bar(stat = "identity") 

rsquared

intercept <- coef(max.station.lm)[1]
slope <- coef(max.station.lm)[2]

lu <- unique(station.merged.sf[c("LU_NAME", "station.name")])

# as separate groups

for (var in unique(station.merged$station.name)) {

  if(df[df$station==var,"slope"]>0){
    caption <- as.expression(
      bquote(
        R^2 == .(round(rsquared[var],4)) ~ 
          Y == .(round(df[df$station==var,"alfa"],4)) +
          .(round(df[df$station==var,"slope"],4))*X))
  } else {
    caption <- as.expression(
      bquote(
        R^2 == .(round(rsquared[var],4)) ~ 
          Y == .(round(df[df$station==var,"alfa"],4)) -
          .(abs(round(df[df$station==var,"slope"],4)))*X))
  }
  
  
  print( ggplot(station.merged[station.merged$station.name==var,], aes(mean, ndvi)) 
         + geom_point() +
           stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95) +
           labs(title= 'Rainfall against NDVI', 
                subtitle = paste0(var,' station; Land use: ', as.character(lu$LU_NAME[lu$station.name==var])),
                x="Rainfall", y = "NDVI",
                caption = caption) +
           scale_x_continuous(labels = seq(1:22),breaks = seq(1:22)))
  #ggsave(last_plot(), filename = paste0('task7_', str_replace_all(var, " ", "_"), ".png"))
}

ggplot(station.merged, aes(mean, ndvi, col=station.name)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95) +
  labs(title= 'Rainfall against NDVI') +
  scale_x_continuous()

# plot by Land Use

ggplot(station.merged.sf, aes(year, ndvi, col=LU_NAME)) + 
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95) +
  labs(title= 'NDVI trend') +
  scale_x_continuous()


#Weird

# use accumulative precipitation per station

# na values as 0
station.merged[ is.na(station.merged) ] <- 0

station.merged
station.merged <- station.merged %>%
  group_by(station.name) %>%
  arrange(station.name, year) %>%
  mutate(rain.acu=cumsum(mean))


# Correlation
station.merged <- as.data.frame(station.merged)

fits <- lmList(ndvi ~ rain.acu | station.name, data=station.merged)
fits


fits.out.sum <- summary(fits)

df <- data.frame(station = names(fits), slope = fits.out.sum$coefficients[,,2][,1], alfa=fits.out.sum$coefficients[,,1][,1])

# get R-squared
# https://stackoverflow.com/questions/23501852/print-r-squared-for-all-of-the-models-fit-with-lmlist

rsquared <- sapply(fits,function(x) summary(x)$r.squared)


for (var in unique(station.merged$station.name)) {
  
  if(df[df$station==var,"slope"]>0){
    caption <- as.expression(
      bquote(
        R^2 == .(round(rsquared[var],4)) ~ 
          Y == .(round(df[df$station==var,"alfa"],4)) +
          .(round(df[df$station==var,"slope"],4))*X))
  } else {
    caption <- as.expression(
      bquote(
        R^2 == .(round(rsquared[var],4)) ~ 
          Y == .(round(df[df$station==var,"alfa"],4)) -
          .(abs(round(df[df$station==var,"slope"],4)))*X))
  }
  
  
  print( ggplot(station.merged[station.merged$station.name==var,], aes(rain.acu, ndvi)) 
         + geom_point() +
           stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95) +
           labs(title= 'Rainfall against NDVI', 
                subtitle = paste0(var,' station; Land use: ', as.character(lu$LU_NAME[lu$station.name==var])),
                x="Rainfall", y = "NDVI",
                caption = caption) +
           scale_x_continuous()
         )
  ggsave(last_plot(), filename = paste0('task7_', str_replace_all(var, " ", "_"), ".png"))
}



## Section:  Check station with agriculture land use
##################################################

st.Ch <- filter(rain.ts.season.df, station.name %in% "Chifeng" & season %in% "Season 1")


# time to bind once both has the same lenght


station.filtered
ndvi.clst.sample

st.Ch$year <- seq(1982:1999)

st.Ch.merged <- left_join(st.Ch, ndvi.clst.long, by = c("station.number" = "station_nu", "year"="year"), type = "left", match = "all")

#st.Ch.merged <- st.Ch.merged[,c(1:8, 13)]


# get land use
st.Ch.merged.sf <- st_as_sf(st.Ch.merged, coords = c("decilong", "decilat"), crs = 4326, agr = "constant")
st.Ch.merged.sf <- st_intersection(lu, st.Ch.merged.sf)
st_geometry(st.Ch.merged.sf) <- NULL
as.character(unique(st.Ch.merged.sf[st.Ch.merged.sf$station.name=="Chifeng",]$LU_NAME))


# Correlation

fits <- lm(formula = ndvi ~ mean,data=st.Ch.merged.sf)
fits


fits.out.sum <- summary(fits)

df <- data.frame(station = "Chifeng", slope = fits.out.sum$coefficients[2], alfa=fits.out.sum$coefficients[1])

# get R-squared
# https://stackoverflow.com/questions/23501852/print-r-squared-for-all-of-the-models-fit-with-lmlist

rsquared <- summary(fits)$r.squared


# plot
require(ggplot2)
ggplot(st.Ch.merged.sf, aes(x=mean, y=ndvi)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95)



for (var in unique(st.Ch.merged$station.name)) {
  
  if(df[df$station==var,"slope"]>0){
    caption <- as.expression(
      bquote(
        R^2 == .(round(rsquared[var],4)) ~ 
          Y == .(round(df[df$station==var,"alfa"],4)) +
          .(round(df[df$station==var,"slope"],4))*X))
  } else {
    caption <- as.expression(
      bquote(
        R^2 == .(round(rsquared[var],4)) ~ 
          Y == .(round(df[df$station==var,"alfa"],4)) -
          .(abs(round(df[df$station==var,"slope"],4)))*X))
  }
  
  
  print( ggplot(st.Ch.merged[st.Ch.merged$station.name==var,], aes(mean, ndvi)) 
         + geom_point() +
           stat_smooth(method = "lm", formula = y ~ x, se=F,level=0.95) +
           labs(title= 'Rainfall against NDVI', 
                subtitle = paste0(var,' station; Land use: ', as.character(lu$LU_NAME[lu$station.name==var])),
                x="Rainfall", y = "NDVI",
                caption = caption) +
           scale_x_continuous())
  ggsave(last_plot(), filename = paste0('task7_', str_replace_all(var, " ", "_"), ".png"))
}




## Section: Optional Time Series analysis
##################################################
# time series object
ndvi.clst.ts <- as.ts(ndvi.clst[,c(2, 5:n)], start=1984, end = 2000)

head(ndvi.clst.ts)
class(ndvi.clst.ts)
plot(ndvi.clst.ts[1,], type = "l")
lines(ndvi.clst.ts[2,])

ndvi.clst.ts

lm()
