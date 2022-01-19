
vecPackage=c("ggplot2","ggmap","mapproj","lubridate","maps","mapdata","dplyr","rgdal","maptools","raster","sf","data.table","ggsn","gridExtra","OpenStreetMap","ggrepel")
ip <- installed.packages()[,1]

for(p in vecPackage){
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "https://pbil.univ-lyon1.fr/CRAN/",dependencies=TRUE)
     library(p,character.only = TRUE)

}



## get flying altitude

vec_file=c("Multiselect_20210820_124200.csv")
f <- paste0("data/",vec_file)
d <- fread(f,dec=",")
head(d)
dim(d)

d <- d[!(is.na(Latitude)),]
d <- d[Latitude > 20,]


d[,migr := ifelse(as.numeric(substring(UTC_date,6,7)) %in% (4:5),"pre",ifelse(as.numeric(substring(UTC_date,6,7)) %in% (6:7),"post",NA))]
d <- d[!is.na(migr)]
d[,device_id := as.character(device_id)]
d[,year := as.numeric(substring(UTC_date,1,4))]

d_flight <- d[speed_km_h > 50 & Altitude_m > 100,]
dim(d_flight)

d_flight[,migr := ifelse(as.numeric(substring(UTC_date,6,7)) %in% (3:5),"migration prenuptial",ifelse(as.numeric(substring(UTC_date,6,7)) %in% (6:8),"migratoin postuptial",NA))]
d_flight <- d_flight[!is.na(migr)]
d_flight[,device_id := as.character(device_id)]
d_flight[,year := as.numeric(substring(UTC_date,1,4))]
dagg_flight <- d_flight[ ,.(median = as.numeric(median(Altitude_m)),ICinf = quantile(Altitude_m, 0.025),ICsup = quantile(Altitude_m,0.975)),by=.(device_id,migr)]
head(dagg_flight)
dim(dagg_flight)

gg <- ggplot(data=d_flight,aes(x=Altitude_m,group=device_id,fill=device_id))+ facet_grid(year~migr)
gg <- gg + geom_histogram(color="white")
gg <- gg + xlim(c(0,6000))
gg


d_flight[Altitude_m == max(d_flight$Altitude_m),]

d_fligt_higest <- d[device_id == d_flight[Altitude_m == max(d_flight$Altitude_m),device_id] & year == d_flight[Altitude_m == max(d_flight$Altitude_m),year] & migr == d_flight[Altitude_m == max(d_flight$Altitude_m),migr],]

dim(d_fligt_higest)

gg <- ggplot(data= d_fligt_higest,aes(x=as.POSIXct(UTC_datetime),y=Altitude_m)) + geom_line() + geom_point()
gg

