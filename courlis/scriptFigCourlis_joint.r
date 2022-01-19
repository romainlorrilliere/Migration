###################################################################
###  Script pour le graphe de synth√®se pour le papier ortolan  ####
###################################################################

vecPackage=c("ggplot2","ggmap","mapproj","lubridate","maps","mapdata","dplyr","rgdal","maptools","raster","sf","data.table","ggsn","gridExtra","OpenStreetMap","ggrepel")
ip <- installed.packages()[,1]

for(p in vecPackage){
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "https://pbil.univ-lyon1.fr/CRAN/",dependencies=TRUE)
     library(p,character.only = TRUE)

}



main_graph <- function(vec_numfig=1:4,prefix_file_data= "courlis_joint_Figure_",prefix_file_fig ="courlis_joint_v3_fig_",  ratio_ref=2,fixe_orientation = TRUE) {
    for(num in vec_numfig) fig_courlis_joint(num,prefix_file_data,prefix_file_fig,ratio_ref,fixe_orientation)
}



fig_courlis_joint <- function(numfig = 1,prefix_file_data,prefix_file_fig,ratio_ref=2,fixe_orientation=FALSE) {

    vec_file=paste0(prefix_file_data,numfig,".csv")
    f <- paste0("data/",vec_file)
    cat("\n -",vec_file,"\n")
    d <- fread(f,dec=",")

    d <- d[!(is.na(Latitude)),]
    d <- d[Latitude > 20,]


    d[,time_num := as.numeric(as.POSIXlt(UTC_datetime ,format="%d/%m/%Y %H:%M"))]
    d[,UTC_datetime := format(as.POSIXlt(UTC_datetime ,format="%d/%m/%Y %H:%M"))]

    setorder(d, UTC_datetime)

    d.mercator <- as.data.frame( OpenStreetMap::projectMercator( lat = d$Latitude, long = d$Longitude ) )
    d <- cbind(d,d.mercator)


    dfirst <- d[, list(UTC_datetime = min(UTC_datetime)),by = device_id]
    dfirst <- merge(dfirst,d,by=c("UTC_datetime","device_id"))

    dlast <- d[,list(UTC_datetime = max(UTC_datetime)),by = device_id]
    dlast <- merge(dlast,d,by=c("UTC_datetime","device_id"))

    dlastred <- d[cat=="red",list(UTC_datetime = max(UTC_datetime)),by = device_id]
    dlastred <- merge(dlastred,d,by=c("UTC_datetime","device_id"))



    if(!(is.null(ratio_ref))){

        upperLm <- c(min(d[,x]),max(d[,y]))
        lowerRm <- c(max(d[,x]),min(d[,y]))


        Ydist <- abs(upperLm[2] - lowerRm[2])
        Xdist <- abs(upperLm[1] - lowerRm[1])



        if(Xdist >= Ydist | fixe_orientation){
            ratio <- Xdist / Ydist
            if(ratio > ratio_ref) {
                Ydistnew <- Xdist / ratio_ref
                ajout <- (Ydistnew - Ydist)/2
                upperLm[2] <- upperLm[2]+ ajout
                lowerRm[2] <- lowerRm[2]- ajout

            }
            if(ratio < ratio_ref){
                Xdistnew <- Ydist * ratio_ref
                ajout <- (Xdistnew - Xdist)/2
                upperLm[1] <- upperLm[1]- ajout
                lowerRm[1] <- lowerRm[1]+ ajout
            }
        }else{
            ratio <- Ydist / Xdist
            if(ratio > ratio_ref) {
                Xdistnew <- Ydist / ratio_ref
                ajout <- (Xdistnew - Xdist)/2
                upperLm[1] <- upperLm[1]- ajout
                lowerRm[1] <- lowerRm[1]+ ajout

            }
            if(ratio < ratio_ref) {
                Ydistnew <- Xdist * ratio_ref
                ajout <- (Ydistnew - Ydist)/2
                upperLm[2] <- upperLm[2]+ ajout
                lowerRm[2] <- lowerRm[2]- ajout

            }
          }

        Xdist <- abs(upperLm[1] - lowerRm[1])
        Ydist <- abs(upperLm[2] - lowerRm[2])

        margeX <- (Xdist * 0.1)/2
        margeY <- (Ydist * 0.1)/2

        upperLm <- upperLm + (c(margeX,margeY) * c(-1,1))
        lowerRm <- lowerRm + (c(margeX,margeY) * c(1,-1))

        corner.m <- data.table(corner = c("upperL","lowerR"),x=c(upperLm[1],lowerRm[1]),y=c(upperLm[2],lowerRm[2]))

        setDF(corner.m)
        sfc = st_as_sf(corner.m,coords = c("x", "y"), crs =osm()@projargs[1])#3857)
        sfc <- st_transform(sfc,4326)
        ##   proj4string(as.character(osm()@projargs[1])))

        upperL <- st_coordinates(sfc)[1,]
        lowerR <- st_coordinates(sfc)[2,]

    } else {
        upperLa <- c(min(d[,Longitude]-2),max(d[,Latitude]+2))
        lowerRa <- c(max(d[,Longitude]+2),min(d[,Latitude]-2))

    }


    d.axis <- data.table(expand.grid(Latitude = seq(30,70,5),Longitude = seq(-10,70,5)))
    d.mercator.axis <- as.data.frame( OpenStreetMap::projectMercator( lat = d.axis$Latitude, long = d.axis$Longitude ) )
    d.axis <- cbind(d.axis,d.mercator.axis)
    d.axis <- d.axis[Longitude > upperL[1] & Longitude <lowerR[1] & Latitude > lowerR[2] & Latitude < upperL[2] ,]
    d.axis.x <- unique(d.axis[,.(Longitude,x)])
    d.axis.y <- unique(d.axis[,.(Latitude,y)])

    setnames(d.axis.x,"x","breaks")
    d.axis.x[,label := paste0(Longitude,"∞ ",ifelse(Longitude > 0, "E","W"))]

    setnames(d.axis.y,"y","breaks")
    d.axis.y[,label := paste0(Latitude,"∞ ",ifelse(Latitude > 0, "N","S"))]

     vecCol <- c("green"="#09d703","red"="#ff0700","yellow"="#fff800")
    ## vecCol <- c("green"="#1b9e77","red"="#d95f02","yellow"="#7570b3") # bleu ‡ la place de vert


    file_fig <- paste0("output/",prefix_file_fig,numfig,".png")
    cat("figure",file_fig,"\n")

    mp <- openmap(upperL[2:1],lowerR[2:1],type='bing')

    gg <- autoplot.OpenStreetMap(mp)
    gg <- gg + labs(x="",y="",colour="")+ theme(legend.position="none")
    gg <- gg + scale_x_continuous(breaks = d.axis.x$breaks, labels = d.axis.x$label,expand=c(0,0))
    gg <- gg + scale_y_continuous(breaks = d.axis.y$breaks, labels = d.axis.y$label,expand=c(0,0))
    gg <- gg + geom_path(data=d,mapping=aes(x=x,y=y,colour=cat,group=device_id),alpha= 1 ,size=1)
    gg <- gg + geom_point(data=dfirst,mapping=aes(x=x,y=y,colour=cat),alpha= 1 ,size=3.3)
    gg <- gg + geom_point(data=dfirst,mapping=aes(x=x,y=y),colour="white",alpha= 1 ,size=1.2)
    gg <- gg + geom_point(data=dlast,mapping=aes(x=x,y=y,colour=cat),alpha= 1 ,size=3.3)
    gg <- gg + geom_point(data=dlast,mapping=aes(x=x,y=y),colour="black",alpha= 1 ,size=1.2)
    gg <- gg + geom_point(data=dlastred,mapping=aes(x=x,y=y,colour=cat),alpha= 0.8 ,size=3.3)
    gg <- gg + geom_point(data=dlastred,mapping=aes(x=x,y=y),colour="black",alpha= 0.8 ,size=1.2)

    gg <- gg + scale_colour_manual(values=vecCol)

    gg_scale  <- 1/ratio_ref
    if(!fixe_orientation & Ydist >= Xdist) gg_scale  <- ratio_ref
    ggsave(file_fig,gg,scale = gg_scale,height=7)

    cat("    ==> DONE !\n")
}

