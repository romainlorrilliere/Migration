


get_corner <- function(d, colname_latitude = "y", colname_longitude = "x",ratio_ref=1.33,
                       fixe_orientation=TRUE ,margin_prop = 0.1,openstreetmap=FALSE) {


    colnames_ref <- c("y", "x")
    colnames_data <- c(colname_latitude, colname_longitude)

    colnames_diff <- data.table(ref=colnames_ref,old = colnames_data)[ref != old & old != "",]
    ## print(colnames_diff)

    colnames_save <- intersect(colnames(d),colnames_ref)
    ##  print(colnames_save)

    if(length(colnames_save) > 0) setnames(d,colnames_save,paste0("XXX_OLD_",colnames_save,"_OLD_"))
    if(nrow(colnames_diff) >0)  setnames(d,colnames_diff[,old],colnames_diff[,ref])

    upperLm <- c(min(d[,x]),max(d[,y]))
    lowerRm <- c(max(d[,x]),min(d[,y]))

    Ydist <- abs(upperLm[2] - lowerRm[2])
    Xdist <- abs(upperLm[1] - lowerRm[1])


    if(!(is.null(ratio_ref))){

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

    }

    margeX <- (Xdist * margin_prop)/2
    margeY <- (Ydist * margin_prop)/2

    upperLm <- upperLm + (c(margeX,margeY) * c(-1,1))
    lowerRm <- lowerRm + (c(margeX,margeY) * c(1,-1))

    corner.m <- data.table(corner = c("upperL","lowerR"),x=c(upperLm[1],lowerRm[1]),y=c(upperLm[2],lowerRm[2]))


    if(openstreetmap) { #in the case of map with openstreetmap
        setDF(corner.m)
        sfc = st_as_sf(corner.m,coords = c("x", "y"), crs =osm()@projargs[1])#3857)
        sfc <- st_transform(sfc,4326)
        ##   proj4string(as.character(osm()@projargs[1])))

        upperL <- st_coordinates(sfc)[1,]
        lowerR <- st_coordinates(sfc)[2,]
        corner.m <- data.table(corner = c("upperL","lowerR"),x=c(upperL[1],lowerR[1]),y=c(upperL[2],lowerR[2]))


        }


        ## backtransform colnames

    if(nrow(colnames_diff) >0) setnames(d,colnames_diff[,ref],colnames_diff[,old])
    if(length(colnames_save) > 0) setnames(d,paste0("XXX_OLD_",colnames_save,"_OLD_"),colnames_save)

    cat("==> Done !\n\n")
    return(corner.m)

}



fig_migr_altitude <- function(d, colname_datetime = "UTC_datetime",colname_date = "UTC_date", colname_latitude = "latitude_wgs84", colname_longitude = "longitude_wgs84",numfig = 1,prefix_file_data=NULL,prefix_file_fig=NULL,ratio_ref=2,fixe_orientation=FALSE) {

    require(maptools)
    require(data.table)
    require(sf)

    setDT(d)


    ## checking colnames
    if(is.null(colname_date)) colname_date  <- ""
    colnames_ref <- c("UTC_datetime", "UTC_date","latitude_wgs84", "longitude_wgs84")
    colnames_data <- c(colname_datetime,colname_date,colname_latitude, colname_longitude)
    colnames_diff <- data.table(ref=colnames_ref,old = colnames_data)[ref != old & old != "",]
    ## print(colnames_diff)

    colnames_save <- intersect(colnames(d),colnames_ref)
    ##  print(colnames_save)

    if(length(colnames_save) > 0) setnames(d,colnames_save,paste0("XXX_OLD_",colnames_save,"_OLD_"))
    if(nrow(colnames_diff) >0)  setnames(d,colnames_diff$old,colnames_diff$ref)





    setorder(d, UTC_datetime)


    corner.m <- get_corner(d)


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

