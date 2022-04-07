
vecPackage=c("ggplot2","ggmap","mapproj","lubridate","maps","mapdata","dplyr","rgdal","maptools","raster","sf","data.table","ggsn","gridExtra","OpenStreetMap","ggrepel")
ip <- installed.packages()[,1]

for(p in vecPackage){
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "https://pbil.univ-lyon1.fr/CRAN/",dependencies=TRUE)
     library(p,character.only = TRUE)

}



source("functions/fun_daynight.r")


## get flying altitude

vec_file=c("Multiselect_20210820_124200.csv")
f <- paste0("data/",vec_file)
d <- fread(f,dec=",")
head(d)
dim(d)

d <- d[!(is.na(Latitude)),]




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

gg <- ggplot(data = d_fligt_higest,aes(x=as.POSIXct(UTC_datetime),y=Altitude_m)) + geom_line() + geom_point()
gg



vec_file=c("Multiselect_20210820_124200.csv")
f <- paste0("data/",vec_file)
d <- fread(f)
d <- d[!(is.na(Latitude)),]
head(d)
dim(d)


d <- daynight(d,colname_datetime = "UTC_datetime",colname_date = "UTC_date", colname_latitude = "Latitude",colname_longitude = "Longitude")

head(d)
dim(d)
write.csv(d,"data/Multiselect_20210820_124200_daynight.csv",row.names=FALSE)


##################

d <- fread("data/Multiselect_20210820_124200_daynight.csv")
####################

d[,id := 1:.N]
setkeyv(d,"id")

head(d)
d <- d[!is.na(day_crepuscule_astro_night),]
dim(d)
#################

d[,month := month(UTC_date)]
dim(d)

hist(d$satcount)
d <- d[satcount > 4,]
dim(d)

d_flight <- d[speed_km_h > 20 & Altitude_m > 50,]
dim(d_flight)
print(head(d_flight))



###################

###### {r search migration flight}

all_birds <- unique(d[,device_id])
print(length(all_birds))

d_flight_all_birds  <-  d_flight[,.(id)]
#d_flight_all_birds[,`:=`(migr = NA, migration = NA, migr_period_id = NA, migr_period_day = NA, duration = NA, dist1_km = NA, dist2_km = NA, duration1_h = NA, duration2_h = NA, vitesse1_km_h = NA, vitesse2_km_h = NA)]


for(i in 14:length(all_birds)) {


      bird_id <- all_birds[i]
  cat("### ",i,"/",length(all_birds)," : ",bird_id,"\n",sep="")


    d_flight_bird <- d_flight[device_id == bird_id,.(device_id,id,UTC_datetime,Latitude,Longitude)]
    print(dim(d_flight_bird))
    if(nrow(d_flight_bird)> 30) {
    d_flight_bird <- add_migration_class(d_flight_bird)

    d_flight_bird[,year := format(date,"%Y")]


    vecCol_migr <- c("wintering"="#2166ac","spring"="#1a9850","breeding"="#FFFF00","fall"="#f46d43","stop_over"="#878787")


    gg1 <- ggplot(d_flight_bird,aes(x=date,y=vitesse2_km_h)) + geom_point(aes(colour=migration),size=1.5,alpha=.8)+ geom_line()+ geom_point(aes(colour=migration),size=1.5,alpha=.05)
    gg1  <- gg1 + scale_colour_manual(values = vecCol_migr)+scale_x_date(labels = function(x) format(x, "%Y-%m"),date_breaks = "1 month")
    gg1 <- gg1 + theme(axis.text.x=element_text(angle=90, hjust=2,vjust=1)) + theme(legend.position="none")
  #  gg1


    d_stay <- d_flight_bird[migr == FALSE & migr_period_day == 1,]
 corner.m <- get_corner(d_flight_bird ,colname_latitude = "Latitude", colname_longitude = "Longitude")
setDF(corner.m)
   xLim=c(corner.m[1,2],corner.m[2,2]);yLim=c(corner.m[2,3],corner.m[1,3])

      world1     <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))

    gg2 <- ggplot()+facet_grid(.~year)
    gg2 <- gg2 + geom_sf(data = world1,fill="white", colour="#7f7f7f", size=0.5)
    gg2 <- gg2 + coord_sf(xlim=xLim,ylim=yLim)
    gg2 <- gg2 + geom_point(data=d_stay , mapping=aes(x=Longitude,y=Latitude,colour = migration,size = duration),alpha = 0.8)
    gg2 <- gg2 + geom_point(data=d_flight_bird , mapping=aes(x=Longitude,y=Latitude,colour = migration),alpha = 0.8,size=1.1)
    gg2 <- gg2 + geom_path(data=d_flight_bird , mapping=aes(x=Longitude,y=Latitude),alpha = 1,size=0.8)
    gg2 <- gg2 + geom_point(data=d_flight_bird , mapping=aes(x=Longitude,y=Latitude,colour = migration),alpha = 0.05,size=1.1)
    gg2 <- gg2 + scale_color_manual(values = vecCol_migr)
    gg2 <- gg2 + labs(title = bird_id, x="",y="",colour="",size="nb days")+ theme(legend.position="top")
  #  print(gg2)


    gg <- grid.arrange(gg2, gg1, heights = c(2,1), ncol = 1, nrow = 2)
   # print(gg)
   ggfile <- paste0("output/courlis_migration_",bird_id,".png")
    ggsave(ggfile,gg)


    d_flight_bird <- d_flight_bird[,names(d_flight_all_birds),with=FALSE]
    setkeyv(d_flight_bird,"id")


    p_cols <- names(d_flight_all_birds)[-1]


     d_flight_all_birds <- d_flight_all_birds[d_flight_bird,on = .(id = id),(p_cols) := mget(sprintf("i.%s", p_cols))]
    }

}



######

## d[,season := ifelse(month %in% 2:6,"spring",ifelse(month %in% 7:9,"fall",NA)),]


#################
vecCol <- c("day" = "#d0d1e6","crepuscule_civil"="#74a9cf","crepuscule_astro"="#0570b0","night"="#023858")
d_flight[,day_crepuscule_astro_night := factor(day_crepuscule_astro_night, levels = c("day","crepuscule_civil","crepuscule_astro","night"))]

d_flight[,heure := as.numeric(format(as.POSIXct(UTC_datetime),"%H"))]
d_flight[,year := as.numeric(format(as.POSIXct(UTC_datetime),"%Y"))]

d_flight_agg <- d_flight[,.(ICinf=quantile(heure,.05),median = as.numeric(median(heure)),ICsup=quantile(heure,.975)),by = day_crepuscule_astro_night]
print(d_flight_agg)

gg <- ggplot(data = d_flight[!is.na(heure),], aes(x=heure,fill=day_crepuscule_astro_night)) + geom_histogram(breaks=1:24,colour = NA, alpha= 0.7) + facet_grid(day_crepuscule_astro_night~.,scales="free")
gg <- gg + scale_fill_manual(values=vecCol) + theme(legend.position="none")
print(gg)

########################

d_flight_agg <- d_flight[,.(ICinf=quantile(solar_I_mA,.05),median = as.numeric(median(solar_I_mA)),ICsup=quantile(solar_I_mA,.975)),by = day_crepuscule_astro_night]
print(d_flight_agg)

gg <- ggplot(data = d_flight[!is.na(solar_I_mA),], aes(x=solar_I_mA,fill=day_crepuscule_astro_night)) + geom_histogram(colour = NA, alpha= 0.7) + facet_grid(day_crepuscule_astro_night~.,scales="free")
gg <- gg + scale_fill_manual(values=vecCol) + theme(legend.position="none")
print(gg)

######{r birds}

d_birds <- d_flight[,.(first_datetime= min(UTC_datetime), last_datetime=max(UTC_datetime),
                duration = round(difftime(max(UTC_datetime),min(UTC_datetime))),
                nb_data_x100 = round(.N /100,2)),by=device_id]
setorder(d_birds, -nb_data_x100)
print(d_birds)

######

######{r selected_birds}

the_birds <- d_birds[nb_data_x100 > 1, device_id]

print(the_birds)
######


######{r d_flight_birds}
d_flight <- d_flight[device_id %in% the_birds,]
print(dim(d_flight))

######


######{r flight_example}

vecCol2 <- vecCol[c(1,4)]

for(i in 1:3){ #length(the_birds)) {


  bird_id <- the_birds[i]
  cat("### ",i,"/",length(the_birds)," : ",bird_id,"\n",sep="")


d_flight_bird <- d_flight[device_id == bird_id,]



print(summary(d_flight_bird[, Altitude_m]))



gg <- ggplot(data = d_flight_bird, aes(y=direction_deg,x=as.POSIXct(UTC_datetime))) + geom_line() + geom_point(aes(colour=day_night))
gg <- gg + facet_grid(season~year, scales="free")
gg <- gg + scale_colour_manual(values=vecCol2) + theme(legend.position="none")
gg <- gg + labs(title = bird_id)
print(gg)

 corner.m <- get_corner(d_flight_bird ,colname_latitude = "Latitude", colname_longitude = "Longitude")
setDF(corner.m)
   xLim=c(corner.m[1,2],corner.m[2,2]);yLim=c(corner.m[2,3],corner.m[1,3])

  d_flight_bird_day <- d_flight_bird[day_night == "day",]

  # d_flight_bird_day[day_night == "night", day_night := NA]

  d_flight_bird_night <- d_flight_bird[day_night == "night",]
 # d_flight_bird_night[day_night == "day", day_night := NA]


  world1     <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))

  gg <- ggplot()+facet_grid(year~season)
    gg <- gg + geom_sf(data = world1,fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_sf(xlim=xLim,ylim=yLim)
    gg <- gg + geom_point(data=d_flight_bird_night , mapping=aes(x=Longitude,y=Latitude),alpha = 0.8,size=2,colour = vecCol2[2])
        gg <- gg + geom_point(data=d_flight_bird_day , mapping=aes(x=Longitude,y=Latitude),alpha = 0.8,size=2, colour= vecCol2[1])
       gg <- gg + geom_path(data=d_flight_bird , mapping=aes(x=Longitude,y=Latitude,colour=Altitude_m),alpha = 1,size=1)
    gg <- gg + scale_color_gradientn(colours = terrain.colors(10))
  gg <- gg + labs(title = bird_id)
   print(gg)


gg <- ggplot(data = d_flight_bird, aes(y=Altitude_m,x=as.POSIXct(UTC_datetime))) + geom_line() + geom_point(aes(colour=day_night))
gg <- gg + facet_grid(year~season, scales="free")
gg <- gg + scale_colour_manual(values=vecCol2) + theme(legend.position="none")
gg <- gg + labs(title = bird_id)
print(gg)

gg <- ggplot(data = d_flight_bird, aes(y=Altitude_m,x=Latitude)) + geom_line() + geom_point(aes(colour=day_night))
gg <- gg + facet_grid(year~season, scales="free")
gg <- gg + scale_colour_manual(values=vecCol2) + theme(legend.position="none")
gg <- gg + labs(title = bird_id)
print(gg)

gg <- ggplot(data = d_flight_bird, aes(y=Altitude_m,x=day_night,fill=day_night)) + geom_boxplot()
gg <- gg + facet_grid(year~season, scales="free")
gg <- gg + scale_fill_manual(values=vecCol2) + theme(legend.position="none")
gg <- gg + labs(title = bird_id)
print(gg)


}

######

vecCourlis <- c(212073,212071,212069,212068,212066,212065,211004,211001,211000,2008838,2002206,200204,200203,200202,200201,200200,200199,200198,200196,200195,200192,200191,200190,200189,200187,200186,200185,200066)

vecCourlis <- intersect(vecCourlis,the_birds)



d_flight_bird_2 <- d_flight_bird[device_id %in% vecCourlis & season %in% c("spring","fall"),]

fwrite(d_flight_bird_2,"data/courlis_migration_journuit.csv")

## model





