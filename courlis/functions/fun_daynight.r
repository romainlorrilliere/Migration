
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

    colnames_save <- intersect(colnames(d),colnames_ref)
    ##  print(colnames_save)

    if(length(colnames_save) > 0) sextnames(d,colnames_save,paste0("XXX_OLD_",colnames_save,"_OLD_"))
    if(nrow(colnames_diff) >0)  setnames(d,colnames_diff$old,colnames_diff$ref)

    ## head(d)

    d <- d[!is.na(date_UTC) & !is.na(datetime_UTC) & !is.na(longitude_wgs84) & !is.na(latitude_wgs84),]
    d[,date_UTC := as.POSIXct(date_UTC,tz = "GMT")]
    d[,datetime_UTC := as.POSIXct(strptime(datetime_UTC,"%Y-%m-%d %H:%M:%S"))]

    cat(nrow(d),"samples with valid date and time and location\n")
    coordinates(d) <- c("longitude_wgs84", "latitude_wgs84")
    lonlat <- SpatialPoints(coordinates(d),proj4string=CRS("+proj=longlat +datum=WGS84"))

    ##  using function form sun-methods{maptools}. They use algorithms provided by the National Oceanic & Atmospheric Administration (NOAA)
    cat("calculating sunrise...")
    d$sunrise <- sunriset(crds = lonlat, dateTime = d$date_UTC, direction="sunrise", POSIXct=TRUE)$time
    cat("Done !\n")
    cat("calculating sunset...")
    d$sunset  <- sunriset(crds = lonlat, dateTime = d$date_UTC, direction="sunset", POSIXct=TRUE)$time
    cat("Done !\n")
    cat("calculating civil dawn...")
    d$dawn_civil <- crepuscule(crds = lonlat, dateTime = d$date_UTC,solarDep=6, direction="dawn", POSIXct=TRUE)$time
    cat("Done !\n")
    cat("calculating civil dusk...")
    d$dusk_civil <- crepuscule(crds = lonlat, dateTime = d$date_UTC,solarDep=6, direction="dusk", POSIXct=TRUE)$time
    cat("Done !\n")
    cat("calculating astronmical dawn...")
    d$dawn_astro <- crepuscule(crds = lonlat, dateTime = d$date_UTC,solarDep=18, direction="dawn", POSIXct=TRUE)$time
    cat("Done !\n")
    cat("calculating astronomical dusk...")
    d$dusk_astro <- crepuscule(crds = lonlat, dateTime = d$date_UTC,solarDep=18, direction="dusk", POSIXct=TRUE)$time
    cat("Done !\n")

     d <- as.data.table(d)

    cat("adding day_night column...")
    d[,day_night := ifelse(datetime_UTC >= sunrise & datetime_UTC <= sunset,"day","night")]
    cat("Done !\n")
    cat("adding day_crepuscule_civil_night column...")
    d[,day_crepuscule_civil_night :=  ifelse(day_night == "day","day",ifelse(d$datetime_UTC > dawn_civil & datetime_UTC < d$dusk_civil, "crepuscule_civil","night"))]
    cat("Done !\n")
    cat("adding day_crepuscule_astro_night column...")
    d[,day_crepuscule_astro_night :=  ifelse(day_crepuscule_civil_night %in% c("day","crepuscule_civil"),day_crepuscule_civil_night,ifelse(d$datetime_UTC > d$dawn_astro & d$datetime_UTC < d$dusk_astro, "crepuscule_astro","night"))]
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
    agg_period[is.na(migration) &  c(agg_period[-1,migration]) == "fall", migration := "breeding"]
    agg_period[is.na(migration),migration := "wintering"]



    agg_period <- agg_period[,.(migr_period_id,migration)]

    agg_day <- merge(agg_day,agg_period,by="migr_period_id")

    agg_day <- agg_day[,.(date,migr,migration,migr_period_id,migr_period_day,duration)]
    d_ind <- merge(agg_day,d_ind,"date")

    return(d_ind)

    }
