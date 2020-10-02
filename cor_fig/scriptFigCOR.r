###################################################################
###  Script pour le graphe de synthèse pour le papier ortolan  ####
###################################################################

vecPackage=c("ggplot2","ggmap","mapproj","lubridate","maps","mapdata","dplyr","rgdal","maptools","raster","sf","data.table","ggsn","gridExtra")
ip <- installed.packages()[,1]

for(p in vecPackage){
    print(p)
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "http://cran.univ-paris1.fr/",dependencies=TRUE)
    cat("..")
    library(p,character.only = TRUE)
    cat("**\n")
}



main_graph <- function() {
   d <- data_preparation()
    make.ggplot_gradient_track(d)

}



data_preparation <- function(vec_file=c("corcor.csv","cormon.csv","corrax.csv"),vec_sp=c("carrion crow","Western jackdaw","raven"),vec_code = c("CORCOR","CORMON","CORRAX")) {
  #  vec_file=c("corcor.csv","cormon.csv","corrax.csv");vec_sp=c("Carion crow","Jackdaw","Raven");vec_code = c("CORCOR","CORMON","CORRAX")
    d <- NULL
    for(i in 1:length(vec_file)) {
        f <- paste0("data/",vec_file[i])
        cat(f,"\n")
        sp <- vec_sp[i]
        code <- vec_code[i]

        df <- fread(f)
        df[,species:=sp]

        if(sp == "carrion crow") {
            colnames(df) <- c("id_bird","dist","lat_wgs84","lon_wgs84","lat_wgs84_2","lon_wgs84_2","species")
            df[,id_bird := paste0(code,"_",id_bird)]
            df1 <- df[,.(id_bird,lat_wgs84,lon_wgs84,species)]
            df1[, order := 1]
            df2 <- df[,.(id_bird,lat_wgs84_2,lon_wgs84_2,species)]
            df2[, order := 2]
            colnames(df2) <- c("id_bird","lat_wgs84","lon_wgs84","species","order")

            df <- rbind(df1,df2)

        }

        if(sp == "Western jackdaw") {

            colnames(df) <- c("id_bird","UTC_datetime","UTC_date","UTC_time","lat_wgs84","lon_wgs84","species")
            df[,id_bird := paste0(code,"_",id_bird)]
            df[,UTC_datetime := format(as.POSIXlt(UTC_datetime ,format="%d/%m/%Y %H:%M"))]
            setorder(df,species,id_bird,UTC_datetime)
            df[,order := 1:.N,by = id_bird]


        }

        if(sp == "raven") {

            colnames(df) <- c("id","UTC_datetime","lon_wgs84","lat_wgs84","id_bird","species")
            df[,id_bird := paste0(code,"_",id_bird)]
            setorder(df,species,id_bird,UTC_datetime)
            df[,order := 1:.N,by = id_bird]



        }



        df <- df[,.(species,id_bird,order,lat_wgs84,lon_wgs84)]
        df <- df[lat_wgs84 > 40 & lat_wgs84 < 60 & lon_wgs84 > - 6 & lon_wgs84 < 9]
        setorder(df,species,id_bird,order)

        d <- rbind(d,df)
    }

    write.csv(d,"data/data_cor.csv")
    return(d)


}



make.ggplot_gradient_track <- function(d,w=15,h=5,filedata="data/data_cor.csv",outputFile="corvid_2020-06-17",  vecCol_start= c("carrion crow" = "#fcae91","raven" = "#bae4b3","Western jackdaw"="#2171b5"),  vecCol_end= c("carrion crow" = "#cb181d","raven" = "#238443","Western jackdaw"="#6baed6"),legend=TRUE,print_fig=TRUE) {

# vecCol_start= c("Carion crow" = "#fcae91","Raven" = "#bae4b3","Jackdaw"="#bdd7e7");  vecCol_end= c("Carion crow" = "#cb181d","Raven" = "#238443","Jackdaw"="#2171b5");legend=TRUE;print_fig=TRUE

    library(lubridate)
    library(ggplot2)
    library(ggmap)
    library(maps)
    library(sf)
    library(data.table)

    nbsp <- length(vecCol_start)

    veccol <- NULL
    for(i in 1:nbsp) {
        sp <- names(vecCol_start[i])
        id <- unique(d[species == sp,id_bird])
        nbid <- length(id)
        veci <- colorRampPalette(c(vecCol_start[i],vecCol_end[i]))(nbid)
        print(sp)
        print(nbid)
        print(length(veci))
        names(veci) <- id
        veccol <- c(veccol,veci)
    }


    world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
    france <- sf::st_as_sf(map('france', plot = FALSE, fill = TRUE))


    gg <- ggplot()
    gg <- gg + geom_sf(data = world1,fill="white", colour="#7f7f7f", size=0.5) + geom_sf(data = france,fill="white", colour="#7f7f7f", size=0.1) + geom_sf(data = world1,fill=NA, colour="#7f7f7f", size=0.5)
    gg <- gg + coord_sf(xlim=c(-4.5,7.7),ylim=c(42,51.5))
    gg <- gg + labs(x="",y="",colour="")+ theme(legend.position="none")
    gg <- gg + scale_colour_manual(values=veccol)
    gg <- gg + geom_path(data=d,mapping=aes(x=lon_wgs84,y=lat_wgs84,group=id_bird,colour=id_bird),alpha = .8,size=.6)
    gg <- gg + geom_point(data=d[order==1],mapping = aes(x=lon_wgs84,y=lat_wgs84,colour=id_bird),size=0.8)
    gg <- gg + geom_point(data=d[order==1],mapping = aes(x=lon_wgs84,y=lat_wgs84,colour=id_bird),shape=21, fill="white",size=0.5)
    gg <- gg + scalebar(dist = 100, dist_unit = "km", transform = TRUE, model = "WGS84",x.min=-4.5,x.max=7,y.min=42,y.max=51.5,box.fill=c(NA,"black"),border.size=0.2,st.size=2.5)

    ggfile <- paste0("plotMap_",outputFile,".png")
    ggsave(ggfile,gg,width=7,height=7)


    gg_corcor <- sub_graph(d,"carrion crow",veccol)
    ggfile <- paste0("plotMap_corcor_",outputFile,".png")
    ggsave(ggfile,gg_corcor,width=4,height=4)

    gg_corrax <- sub_graph(d,"raven",veccol)
  ggfile <- paste0("plotMap_corrax_",outputFile,".png")
    ggsave(ggfile,gg_corrax,width=4,height=4)

    gg_cormon <- sub_graph(d,"Western jackdaw",veccol)
  ggfile <- paste0("plotMap_cormon_",outputFile,".png")
    ggsave(ggfile,gg_cormon,width=4,height=4)


    lay <- rbind(c(1,2),c(1,3),c(1,4))




   # list_cor <- list(gg_corcor,gg_corrax,gg_cormon)

  #  margin <- theme(plot.margin = unit(c(0,1,1,0), "line"))
  #  gg_grid_right <- grid.arrange(grobs = lapply(list_cor,"+",margin),padding=0)

    gg <- gg + theme(plot.margin = unit(c(0,0,0,0), "line"),base_size = 14)#,plot.background = element_rect( fill = "grey90",  colour = "black", size = 1 ))
    gg_grid <- grid.arrange(gg,gg_corcor,gg_corrax,gg_cormon,ncol = 2, nrow = 3,layout_matrix=lay,widths=c(8.5,4.5))

##    gg_grid2 <- grid.arrange(gg_corcor,gg_corrax,gg_cormon,ncol = 1, nrow = 3)
###    gg_grid <- grid.arrange(gg,gg_grid_right,ncol = 2,widths=c(17,7),padding=0,clip=TRUE,top=textGrob("Head Line",gp=gpar(cex=1.5, fontface="bold", col="#990000")))
##
##    library(cowplot)
##    plots <- align_plots(gg,gg_corcor,gg_cormon,align="h",axis = "tb")
##
##    right_col <- plot_grid(plots[[2]],gg_corrax,plots[[3]],ncol=1)
##    plot_grid(plots[[1]],right_col,ncol=2,rel_widths=c(3,1))
##
##    left <- plot_grid(gg)
##    right <- plot_grid(gg_corcor,gg_cormon,ncol=1)
##    plot_grid(left,gg_corcor,ncol=2,rel_widths=c(2,1),align="h",axis="t")
##
##
##    library(patchwork)
##
##    (gg_corcor | gg_corrax | gg_cormon) / gg
##
 # browser()

    fileOutput_gg<- paste0("plotMultiMap_",outputFile,".png")
    cat("\n[PNG]:",fileOutput_gg)
    ggsave(fileOutput_gg,gg_grid,width=9,height=9)
    cat("   DONE ! \n")



}



sub_graph <- function(d,sp,veccol) {

    degloc <- 0.3
    dsp <- d[species == sp]

    lat_min <- min(dsp$lat_wgs84,na.rm=TRUE) - degloc
    lat_max <- max(dsp$lat_wgs84,na.rm=TRUE) + degloc
    lon_min <- min(dsp$lon_wgs84,na.rm=TRUE) - degloc
    lon_max <- max(dsp$lon_wgs84,na.rm=TRUE) + degloc

    france <- sf::st_as_sf(map("france", plot = FALSE, fill = TRUE))

    ggsp <- ggplot()
    ggsp <- ggsp + geom_sf(data = france,fill="white", colour="#7f7f7f", size=0.3)
    ggsp <- ggsp + coord_sf(xlim=c(lon_min,lon_max),ylim=c(lat_min,lat_max))


    ggsp <- ggsp + geom_path(data=dsp,mapping=aes(x=lon_wgs84,y=lat_wgs84,group=id_bird,colour=id_bird),alpha = .8,size=1.1)

    ggsp <- ggsp + geom_point(data=dsp[order==1],mapping = aes(x=lon_wgs84,y=lat_wgs84,colour=id_bird),size=2.2)
    ggsp <- ggsp + geom_point(data=dsp[order==1],mapping = aes(x=lon_wgs84,y=lat_wgs84,colour=id_bird),shape=21, fill="white",size=1.5)

    ggsp <- ggsp + scalebar(dist = 50, dist_unit = "km", transform = TRUE, model = "WGS84",x.min=lon_min,x.max=lon_max,y.min=lat_min,y.max=lat_max,box.fill=c(NA,"black"),border.size=0.2,st.size=3.5,st.bottom=FALSE,st.dist=0.05)
    ggsp <- ggsp + labs(x="",y="",colour="") + theme(legend.position="none",axis.title=element_blank(), axis.text=element_blank(),axis.ticks=element_blank(),plot.margin = unit(c(0,0,0,0), "line"),base_size=14,strip.text = element_text(size = 14, color = "red", face = "bold"))#,plot.background = element_rect( fill = "grey90",  colour = "black", size = 1 ))
    ggsp <- ggsp + facet_wrap(.~species)


    ggsp <- ggsp + scale_colour_manual(values=veccol)
    return(ggsp)


}

