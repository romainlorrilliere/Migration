


### function to add the column daynight in data
### the data should to have some column
### datetime, latitude, longitude

daynight <- function(d, colname_datetime = "UTC_datetime",colname_date = "UTC_date", colname_latitude = "latitude_wgs84", colname_longitude = "longitude_wgs84") {


   ## ---- initializing parameters for debugging ----
##   colname_datetime = "UTC_datetime"; colname_date = "UTC_date"; colname_latitude = "Latitude"; colname_longitude = "Longitude"
    ## ---

    require(maptools)
    require(data.table)

    setDT(d)

    ## checking colnames
    if(is.null(colname_date)) colname_date  <- ""
    colnames_ref <- c("UTC_datetime", "UTC_date","latitude_wgs84", "longitude_wgs84")
    colnames_data <- c(colname_datetime,colname_date,colname_latitude, colname_longitude)
    colnames_diff <- data.table(ref=colnames_ref,old = colnames_data)[ref != old & old != "",]
    ## print(colnames_diff)

    colnames_save <- intersect(colnames(d),colnames_diff)
    ##  print(colnames_save)

    if(length(colnames_save) > 0) setnames(d,colnames_save,paste0("XXX_OLD_",colnames_save,"_OLD_"))

    if(nrow(colnames_diff) >0)  setnames(x=d,old=colnames_diff$old,new=colnames_diff$ref)

    ## head(d)

    d <- d[!is.na(UTC_date) & !is.na(UTC_datetime) & !is.na(longitude_wgs84) & !is.na(latitude_wgs84),]
    d[,UTC_date := as.POSIXct(UTC_date,tz = "GMT")]
    d[,UTC_datetime := as.POSIXct(UTC_datetime,tz = "GMT")]

    cat(nrow(d),"samples with valid date and time and location\n")
    coordinates(d) <- c("longitude_wgs84", "latitude_wgs84")
    lonlat <- SpatialPoints(coordinates(d),proj4string=CRS("+proj=longlat +datum=WGS84"))

    ##  using function form sun-methods{maptools}. They use algorithms provided by the National Oceanic & Atmospheric Administration (NOAA)
    cat("calculating sunrise...")
    d$sunrise <- sunriset(crds = lonlat, dateTime = d$UTC_date, direction="sunrise", POSIXct=TRUE)$time
    cat("Done !\n")
    cat("calculating sunset...")
    d$sunset  <- sunriset(crds = lonlat, dateTime = d$UTC_date, direction="sunset", POSIXct=TRUE)$time
    cat("Done !\n")
    cat("calculating civil dawn...")
    d$dawn_civil <- crepuscule(crds = lonlat, dateTime = d$UTC_date,solarDep=6, direction="dawn", POSIXct=TRUE)$time
    cat("Done !\n")
    cat("calculating civil dusk...")
    d$dusk_civil <- crepuscule(crds = lonlat, dateTime = d$UTC_date,solarDep=6, direction="dusk", POSIXct=TRUE)$time
    cat("Done !\n")
    cat("calculating astronmical dawn...")
    d$dawn_astro <- crepuscule(crds = lonlat, dateTime = d$UTC_date,solarDep=18, direction="dawn", POSIXct=TRUE)$time
    cat("Done !\n")
    cat("calculating astronomical dusk...")
    d$dusk_astro <- crepuscule(crds = lonlat, dateTime = d$UTC_date,solarDep=18, direction="dusk", POSIXct=TRUE)$time
    cat("Done !\n")

     d <- as.data.table(d)

    cat("adding day_night column...")
    d[,day_night := ifelse(UTC_datetime >= sunrise & UTC_datetime <= sunset,"day","night")]
    cat("Done !\n")
    cat("adding day_crepuscule_civil_night column...")
    d[,day_crepuscule_civil_night :=  ifelse(day_night == "day","day",ifelse(d$UTC_datetime > dawn_civil & UTC_datetime < d$dusk_civil, "crepuscule_civil","night"))]
    cat("Done !\n")
    cat("adding day_crepuscule_astro_night column...")
    d[,day_crepuscule_astro_night :=  ifelse(day_crepuscule_civil_night %in% c("day","crepuscule_civil"),day_crepuscule_civil_night,ifelse(d$UTC_datetime > d$dawn_astro & d$UTC_datetime < d$dusk_astro, "crepuscule_astro","night"))]
    cat("Done !\n")

    ## backtransform colnames

    if(nrow(colnames_diff) >0) setnames(d,colnames_diff$ref,colnames_diff$old)
    if(length(colnames_save) > 0) setnames(d,paste0("XXX_OLD_",colnames_save,"_OLD_"),colnames_save)

    cat("==> Done !\n\n")
    return(d)

}



add_migration_class <- function(d_ind,seuil_migr_vitesse = 25) {

   sf_ind <- st_as_sf(d_ind,coords=c("Longitude","Latitude"),crs = 4326)

    d_ind[, dist1_km := c(as.vector(st_distance(sf_ind[-1,],sf_ind[-nrow(sf_ind),],by_element=TRUE))/1000,NA)]
    d_ind[, dist2_km := c(as.vector(st_distance(sf_ind[3:nrow(sf_ind),],sf_ind[1:(nrow(sf_ind)-2),],by_element=TRUE))/1000,NA,NA)]

    d_ind[,UTC_datetime := as.POSIXct(UTC_datetime)]

    d_ind[,duration1_h :=  c(as.vector(difftime(d_ind[-1,UTC_datetime],d_ind[-nrow(d_ind),UTC_datetime],units="hour")),NA)]

    d_ind[,duration2_h :=  c(as.vector(difftime(d_ind[3:nrow(sf_ind),UTC_datetime],d_ind[1:(nrow(sf_ind)-2),UTC_datetime],units="hour")),NA,NA)]

    d_ind[,`:=`(vitesse1_km_h = dist1_km / duration1_h, vitesse2_km_h = dist2_km / duration2_h)]

    ## head(d_ind)


     d_ind[,date := as.Date(d_ind$UTC_datetime)]

   ##  gg <- ggplot(d_ind,aes(x=UTC_datetime,y=vitesse2_km_h)) + geom_line()
   ##  gg

    agg_day <- d_ind[,.(vitesse1_km_h = as.numeric(median(vitesse1_km_h)), vitesse2_km_h = as.numeric(median(vitesse2_km_h))),by = date]


    agg_day[,migr := vitesse2_km_h > seuil_migr_vitesse & vitesse1_km_h > seuil_migr_vitesse]
    agg_day[is.na(migr),migr := FALSE]

    agg_day[,migr_num := as.numeric(migr)]

    agg_day[,migr_neig := c(agg_day[-1,migr_num],0) + c(0,agg_day[-nrow(agg_day),migr_num])]

    agg_day[,migr := ifelse(migr,migr,ifelse(migr_neig >1 ,TRUE,migr))]

    agg_day[,migr_period_id := cumsum(c(1,as.numeric(agg_day[-nrow(agg_day),migr] !=  agg_day[-1,migr])))]

    agg_day[,migr_period_day := 1:.N, by = migr_period_id]
    agg_day[, duration := max(migr_period_day), by = migr_period_id]


    agg_period <- agg_day[migr_period_day == 1,]
    agg_period[,month := as.numeric(format(date,"%m"))]
   ## agg_period[,migration := ifelse(migr,ifelse(month < 6, "spring","fall"),
   ##                          ifelse(duration < 10, "stop_over",
   ##                          ifelse(month >= 6, "wintering","breeding")))]


    agg_period[,migration := ifelse(migr,ifelse(month < 6, "spring","fall"),NA)]
    agg_period[is.na(migration) & c(agg_period[-1,migration],NA) == c(NA,agg_period[-nrow(agg_period),migration]),migration := "stop_over"]

    agg_period[is.na(migration) &  c(NA,agg_period[-nrow(agg_period),migration]) == "spring", migration := "breeding"]
    agg_period[is.na(migration) &  c(NA,agg_period[-nrow(agg_period),migration]) == "fall", migration := "wintering"]
    agg_period[is.na(migration) &  c(agg_period[-1,migration],NA) == "fall", migration := "breeding"]
    agg_period[is.na(migration),migration := "wintering"]



    agg_period <- agg_period[,.(migr_period_id,migration)]

    agg_day <- merge(agg_day,agg_period,by="migr_period_id")

    agg_day <- agg_day[,.(date,migr,migration,migr_period_id,migr_period_day,duration)]
    d_ind <- merge(agg_day,d_ind,"date")

    return(d_ind)

    }



## plot simple figure of curlew data with rainbow gradian on the datetime
fig_curlew <- function(d_bird,bird_id,plot=TRUE,output=FALSE,file_save=NULL) {

    corner.m <- get_corner(d_bird ,colname_latitude = "Latitude", colname_longitude = "Longitude")
    setDF(corner.m)
    xLim=c(corner.m[1,2],corner.m[2,2]);yLim=c(corner.m[2,3],corner.m[1,3])

    world1     <- sf::st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))


    myBreaks <- function(x){
        breaks <- c(min(x),median(x),max(x))
        attr(breaks,"labels") <- breaks
        names(breaks) <- attr(breaks,"labels")
        return(breaks)
    }

    dates_legend <- seq(min(d_bird[,UTC_datetime]),max(d_bird[,UTC_datetime]),length.out=5)
    dates_legend

    gg <- ggplot()
    gg <- gg + geom_sf(data = world1,fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_sf(xlim=xLim,ylim=yLim)
    gg <- gg + geom_path(data=d_bird , mapping=aes(x=Longitude,y=Latitude),alpha = 1,size=0.5)
    gg <- gg + geom_point(data=d_bird , mapping=aes(x=Longitude,y=Latitude,colour=UTC_datetime),size=0.9,alpha = 0.8)
    gg <- gg + labs(title = bird_id, x="",y="",colour="")
    gg <- gg + scale_color_gradientn(colours = rainbow(10),breaks=dates_legend,labels=as.Date(dates_legend))


    if(!is.null(file_save)) ggsave(ggfile,gg)
    if(plot) print(gg)
    if(output) retun(gg)

}


migration_cat_retreiving <- function(d,figure=TRUE) {

## keeping only flying data such as speed above 20 km/h and altitude above 50m
d_flight <- d[speed_km_h > 20 & Altitude_m > 50,]

all_birds <- unique(d[,device_id])
print(length(all_birds))

## preparing table to save the migratory period assignation for flights
d_flight_all_birds  <-  d_flight[,.(id)]
d_flight_all_birds[,`:=`(migr = TRUE, migration = "", migr_period_id = 999, migr_period_day = 999, duration = 999, dist1_km = 999.99, dist2_km = 999.99, duration1_h = 999.99, duration2_h = 999.99, vitesse1_km_h = 999.99, vitesse2_km_h = 999.99)]
  if(figure)  vecCol_migr <- c("wintering"="#2166ac","spring"="#1a9850","breeding"="#FFFF00","fall"="#f46d43","stop_over"="#878787")


## loop on all birds to assign each flight on a migration category
for(i in 1:length(all_birds)) {


      bird_id <- all_birds[i]
    cat("### ",i,"/",length(all_birds)," : ",bird_id,"\n",sep="")

    ## plot of all data of this bird
    if(figure) {
        ggfile <- paste0("output/courlis_migration_",bird_id,"_all.png")
        fig_curlew(d[device_id == bird_id,],bird_id,plot=FALSE,file_save=ggfile)
    }


    ## select id_bird flight data
    d_flight_bird <- d_flight[device_id == bird_id,.(device_id,id,UTC_datetime,Latitude,Longitude)]
    print(dim(d_flight_bird))

    ## only if we have enough data
    if(nrow(d_flight_bird)> 30) {
    d_flight_bird <- add_migration_class(d_flight_bird)

    d_flight_bird[,year := format(date,"%Y")]


 if(figure) {

   gg1 <- ggplot(d_flight_bird,aes(x=date,y=vitesse2_km_h)) + geom_point(aes(colour=migration),size=1.5,alpha=.8)+ geom_line()+ geom_point(aes(colour=migration),size=1.5,alpha=.05)
   gg1  <- gg1 + scale_colour_manual(values = vecCol_migr)+scale_x_date(labels = function(x) format(x, "%Y-%m"),date_breaks = "1 month")
   gg1 <- gg1 + theme(axis.text.x=element_text(angle=90, hjust=2,vjust=1)) + theme(legend.position="none")
 #  gg1


   d_stay <- d_flight_bird[migr == FALSE & migr_period_day == 1,]
   corner.m <- get_corner(d_flight_bird ,colname_latitude = "Latitude", colname_longitude = "Longitude")
   setDF(corner.m)
   xLim=c(corner.m[1,2],corner.m[2,2]);yLim=c(corner.m[2,3],corner.m[1,3])

   world1     <- sf::st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))

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


   gg <- gridExtra::grid.arrange(gg2, gg1, heights = c(2,1), ncol = 1, nrow = 2)
  # print(gg)
  ggfile <- paste0("output/courlis_migration_",bird_id,".png")
   ggsave(ggfile,gg)
    }


    d_flight_bird <- d_flight_bird[,names(d_flight_all_birds),with=FALSE]
    setkeyv(d_flight_bird,"id")


    p_cols <- names(d_flight_all_birds)[-1]

    d_flight_all_birds <- d_flight_all_birds[d_flight_bird,on = .(id = id),(p_cols) := mget(sprintf("i.%s", p_cols))]
    }

}

return(d_flight_all_birds)

}
