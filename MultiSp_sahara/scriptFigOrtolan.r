###################################################################
###  Script pour le graphe de synthèse pour le papier ortolan  ####
###################################################################

vecPackage=c("ggplot2","ggmap","mapproj","lubridate","maps","mapdata","dplyr","rgdal","maptools","raster","sf")
ip <- installed.packages()[,1]

for(p in vecPackage)
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "http://cran.univ-paris1.fr/",dependencies=TRUE)

library(ggplot2)
library(ggmap)
library(mapproj)
library(lubridate)
library(maps)
library(mapdata)
library(dplyr)
library(maptools)
library(rgdal)
library(raster)
require(sf)




main_graph <- function(w=7,h=7.5) {
    w=9;h=9
    make.ggT_gls(w,h)
}


make.ggT_gls <- function(w=15,h=5,filedata="gls/figure_map2_allspecies-final.csv",outputFile="final_2019-09-17") {

    library(lubridate)
    library(ggplot2)
    library(ggmap)
    library(maps)
    library(sf)
    library(data.table)

#   w=15;h=5
    ##   d <- read.csv2("gls/figure_map2_allspecies.csv")
    ##   d$species <- "Ortolan bunting"
    ##   d$species[grep("BD",d$bird)] <- "Spotted flycatcher"

    d <- fread(filedata,dec=",")
    d <- subset(d,species != "")

    d$species <- gsub("bunting","Bunting",d$species)
    d$species <- gsub("flycatcher","Flycatcher",d$species)
    d$species <- gsub("Tyrrhenian","Mediterranean",d$species)

    panel1 <- c("Ortolan Bunting")
    panel2 <- c("Mediterranean Flycatcher","Spotted Flycatcher")
    panel3 <- c("Willow Warbler","Wood Warbler")
    panel4 <- c("Rufous-tailed Scrub-robin","Eurasian Reed Warbler")


    panel_name <- c("Ortolan","Flycatcher","Warbler","Reed Warbler & Robin")

d$panel <- ifelse(d$species %in% panel1,panel_name[1],ifelse(d$species %in% panel2,panel_name[2],ifelse(d$species %in% panel3,panel_name[3],panel_name[4])))

d$panel <- factor(d$panel,levels=panel_name)

    WorldData <- map_data(map="world")
    WorldData <- fortify(WorldData)


######

    os <- st_read("../../GIS/tnc_terr_ecoregions.shp")
    desert <- os[grep("DESERT",toupper(os$WWF_MHTNAM)),]

    world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))

    vecCol <- c("Ortolan Bunting"="#e84207","Spotted Flycatcher" = "#2680d7","Mediterranean Flycatcher"="#1d00d0","Willow Warbler" = "#41d61c","Wood Warbler"="#155616","Eurasian Reed Warbler"="#e5930e","Rufous-tailed Scrub-robin"="#a31125")



    gg <- ggplot()+facet_grid(season~panel)
    gg <- gg + geom_sf(data = world1,fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + geom_sf(data = desert,fill="#ffeda0",colour=NA,alpha=.7)
    gg <- gg + geom_sf(data = world1,fill=NA, colour="#7f7f7f", size=0.5)
    gg <- gg + coord_sf(xlim=c(-17,50),ylim=c(-10,65))

    gg <- gg + geom_errorbar(data=d,mapping=aes(x=beforemedlon,ymin = beforelat25,ymax = beforelat75,colour=species),alpha=.8,size=.5)
     gg <- gg + geom_errorbarh(data=d,aes(y=beforemedlat,xmin = beforelon25,xmax = beforelon75,colour=species),alpha=.8,height=0,size=.5)
       gg <- gg + geom_errorbar(data=d,mapping=aes(x=aftermedlon,ymin = afterlat25,ymax = afterlat75,colour=species),alpha=.8,size=.5)
     gg <- gg + geom_errorbarh(data=d,aes(y=aftermedlat,xmin = afterlon25,xmax = afterlon75,colour=species),alpha=.8,height=0,size=.5)

    gg <- gg + geom_segment(data=d,mapping=aes(x=beforemedlon,y=beforemedlat,xend=aftermedlon,yend=aftermedlat,colour=species),alpha = .8,size=.65)

    gg <- gg + geom_point(data=d,mapping = aes(x=beforemedlon,y=beforemedlat,colour=species),size=2.2)
    gg <- gg + geom_point(data=d,mapping = aes(x=aftermedlon,y=aftermedlat,colour=species),size=2.2)
    gg <- gg + geom_point(data=d,mapping = aes(x=beforemedlon,y=beforemedlat),colour="white",alpha=.8,size=.8)
    gg <- gg + geom_point(data=d,mapping = aes(x=aftermedlon,y=aftermedlat),colour="white",alpha=.8,size=.8)

     gg <- gg + scale_colour_manual(values=vecCol,breaks=names(vecCol))
    gg <- gg + labs(x="",y="",colour="")#+ theme(legend.position="none")


    ggfile <- paste0("plotMap_AllSp_desert_panel_legend_",outputFile,".png")
    ggsave(ggfile,gg,width=20,height=10)



    gg <- ggplot()+facet_grid(~season)
    gg <- gg + geom_sf(data = world1,fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + geom_sf(data = desert,fill="#ffeda0",colour=NA,alpha=.7)
    gg <- gg + geom_sf(data = world1,fill=NA, colour="#7f7f7f", size=0.5)
    gg <- gg + coord_sf(xlim=c(-17,50),ylim=c(-10,65))

    gg <- gg + geom_errorbar(data=d,mapping=aes(x=beforemedlon,ymin = beforelat25,ymax = beforelat75,colour=species),alpha=.8,size=.5)
     gg <- gg + geom_errorbarh(data=d,aes(y=beforemedlat,xmin = beforelon25,xmax = beforelon75,colour=species),alpha=.8,height=0,size=.5)
       gg <- gg + geom_errorbar(data=d,mapping=aes(x=aftermedlon,ymin = afterlat25,ymax = afterlat75,colour=species),alpha=.8,size=.5)
     gg <- gg + geom_errorbarh(data=d,aes(y=aftermedlat,xmin = afterlon25,xmax = afterlon75,colour=species),alpha=.8,height=0,size=.5)

    gg <- gg + geom_segment(data=d,mapping=aes(x=beforemedlon,y=beforemedlat,xend=aftermedlon,yend=aftermedlat,colour=species),alpha = .8,size=.65)

    gg <- gg + geom_point(data=d,mapping = aes(x=beforemedlon,y=beforemedlat,colour=species),size=2.2)
    gg <- gg + geom_point(data=d,mapping = aes(x=aftermedlon,y=aftermedlat,colour=species),size=2.2)
    gg <- gg + geom_point(data=d,mapping = aes(x=beforemedlon,y=beforemedlat),colour="white",alpha=.8,size=.8)
    gg <- gg + geom_point(data=d,mapping = aes(x=aftermedlon,y=aftermedlat),colour="white",alpha=.8,size=.8)

     gg <- gg + scale_colour_manual(values=vecCol,breaks=names(vecCol))
    gg <- gg + labs(x="",y="",colour="")#+ theme(legend.position="none")

    ggfile <- paste0("plotMap_AllSp_desert_legend_",outputFile,".png")
    ggsave(ggfile,gg,width=w,height=h)


}

