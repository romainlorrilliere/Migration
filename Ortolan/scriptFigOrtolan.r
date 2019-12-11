###################################################################
###  Script pour le graphe de synthèse pour le papier ortolan  ####
###################################################################

vecPackage=c("ggplot2","ggmap","mapproj","lubridate","maps","mapdata","dplyr","rgdal","maptools","raster")
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




main_graph <- function(w=7,h=7.5) {
    w=7;h=7.5
    make.ggT_distrib(w,h)
    make.ggT_gls(w,h)
    make.ggT_iso(w,h)
}


make.ggT_gls <- function(w,h) {
    library(lubridate)
    ttracks <- read.csv("gls/All tracks Sept17.csv")
    ttracksG <- read.csv2("gls/Bernardy_alleLogger_stopovers_with_SD.csv")
    ttracks$darrival <- as.character(ttracks$date.arrival)
    ttracks$darrival[grep("/",ttracks$darrival)] <-  as.character(as.Date(as.character(ttracks$darrival[grep("/",ttracks$darrival)]),format="%d/%m/%Y"))

    ttracks$ddeparture <- as.character(ttracks$date.departure)
    ttracks$ddeparture[grep("/",ttracks$ddeparture)] <-  as.character(as.Date(as.character(ttracks$ddeparture[grep("/",ttracks$ddeparture)]),format="%d/%m/%Y"))

    ttracks$jarrival <- format(as.Date(ttracks$darrival),"%j")
    ttracks$marrival <- month(as.Date(ttracks$darrival))
    ttracks$year <- as.numeric(format(as.Date(ttracks$darrival),"%Y"))

    ttracks$idmigr <- paste(ttracks$id,ifelse(ttracks$marrival<5,ttracks$year-1,ttracks$year),sep="_")

    ttracks <- ttracks[order(ttracks$id,ttracks$jarrival),]

    ttracks$longitude <- ttracks$calculated.longitude..mode.
    ttracks$latitude <- as.character(ttracks$adjusted.latitude.mode.)

    ## crrection manuel data swiss

    ttracks$longitude[ttracks$id == "17KV" & ttracks$position == 2] <- 17.1
    ttracks$latitude[ttracks$id == "17KV" & ttracks$position == 2] <- 38.9

    ttracks$longitude[ttracks$id == "17KV" & ttracks$position == 3] <- 10.85
    ttracks$latitude[ttracks$id == "17KV" & ttracks$position == 3] <- 36.8

    ttracks$longitude[ttracks$id == "15MJ" & ttracks$position == 3] <- 18.5
    ttracks$latitude[ttracks$id == "15MJ" & ttracks$position == 3] <- 37.9

    vecCol <- colnames(ttracks)[c(1:5,10,21:28)]

    ttracks <- ttracks[,vecCol]

    ttracksG$id <- ttracksG$birdName
    ttracksG$Type <- "English"
    ttracksG$Flyway <- "West"
    ttracksG$Country <- "Germany"
    ttracksG$position <- ttracksG$stopoverNumber
    ttracksG$category2 <- as.character(ttracksG$Spalte1)
    ttracksG$category2[ttracksG$category2 %in% c("autumn","spring")] <- "move"

    ttracks$category <- as.character(ttracks$category2)
    ttracks$category[ttracks$category %in% c("moving","unknown","stopping") & ttracks$marrival %in% 7:12] <- "autumn"
    ttracks$category[ttracks$category %in% c("moving","unknown","stopping")  & !(ttracks$marrival %in% 7:12)] <- "spring"
    vecCol <- c(vecCol,"category")

    ttracksG$category <- as.character(ttracksG$Spalte1)
    ttracksG$category[ttracksG$category=="breeding site"] <- "breeding"
    ttracksG$category[ttracksG$category=="wintering area"] <- "wintering"
    ttracksG$darrival <- NA
    ttracksG$ddeparture <- NA
    ttracksG$jarrival <- NA
    ttracksG$marrival <- NA


    ttracksG$idmigr <- paste(ttracksG$id,ttracksG$year,sep="_")


    ttracksG <- ttracksG[,vecCol]

    ttracks <- rbind(ttracks,ttracksG)



    ttracks$uncertainty_cause <- "none"
    ttracks$uncertainty_cause[grep("\\?",ttracks$latitude)] <- "equinox"
    ttracks$latitude <- gsub("\\?","",ttracks$latitude)
    ttracks$latitude <- as.numeric(ttracks$latitude)




    ttracks0 <- subset(ttracks,category %in% c("autumn","breeding","wintering"))
    ttracks1 <- subset(ttracks,category %in% c("autumn"))
    ttracks1$cat <- "migration"

    ttracks2 <- subset(ttracks, category %in% c("breeding"))
    ttracks2$cat <- "breeding"
    ttracks2min <- aggregate(position~idmigr, ttracks2,min)
    colnames(ttracks2min)[2] <- "position_min"
    colOrder <- colnames(ttracks2)
    ttracks2 <- merge(ttracks2, ttracks2min,by="idmigr")
    ttracks2 <- subset(ttracks2,position == position_min)
    ttracks2 <- ttracks2[,colOrder]

    ttracks2 <- subset(ttracks2, idmigr %in% ttracks1$idmigr)

    ttracks3 <- subset(ttracks, category %in% c("wintering"))
    ttracks3$cat <- "wintering"
    ttracks3min <- aggregate(position~idmigr, ttracks3,min)
    colnames(ttracks3min)[2] <- "position_min"
    colOrder <- colnames(ttracks3)
    ttracks3 <- merge(ttracks3, ttracks3min,by="idmigr")
    ttracks3 <- subset(ttracks3,position == position_min)
    ttracks3 <- ttracks3[,colOrder]

    ttracks1 <- rbind(ttracks1,ttracks2,ttracks3)

    ttracks1 <- ttracks1[order(ttracks1$id,ttracks1$position),]
    ttracks1$Type <- as.character(ttracks1$Type)
                                        #  ttracks1$Type[which(ttracks1$Type %in% c("English","English_equinox") & ttracks1$uncertainty_cause=="equinox")] <- "English_equinox"

    ttracks1$cat <- as.character(ttracks1$cat)
    ttracks1$cat[ttracks1$category2 %in% c("moving","unknown")]<-""

    ttracks1 <- ttracks1[order(ttracks1$id,ttracks1$position),]

    vecCol <- c("East"="#1b9e77","West"="#7570b3","Central?"="#d95f02")

                                        # vecShape <- c("none"=16,"equinox"=1)
                                        # vecShape <- c("English"=16,"English_equinox"=13,"Swiss"=7)
    vecShape <- c("English"=16,"Swiss"=1)
                                        # vecTypeLine <- c("English"=1,"Swiss"=2)
    WorldData <- map_data(map="world")
    WorldData <- fortify(WorldData)


######



    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-80, 90))
    ## gg <- gg +  coord_fixed(ratio = 1)
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
    gg <- gg + geom_path(data=ttracks1,aes(x=longitude,y=latitude,group=idmigr,colour=Flyway),size=1,alpha=.6)

    gg <- gg + geom_point(data=subset(ttracks1,cat %in% c("migration","breeding","wintering")),mapping = aes(x=longitude,y=latitude,shape=Type),alpha=.8,size=1.7)
    gg <- gg + scale_shape_manual(values = vecShape)  + scale_linetype_manual(values = vecTypeLine)
    gg <- gg + scale_colour_manual(values=vecCol)
    gg <- gg + labs(x="",y="")+ theme(legend.position="none")
    ggsave("plotGLS_0.png",gg,width = w, height = h)

    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-80, 90))
    ## gg <- gg +  coord_fixed(ratio = 1)
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
    gg <- gg + geom_path(data=ttracks1,aes(x=longitude,y=latitude,group=idmigr,colour=Flyway),size=1,alpha=.6)

    gg <- gg + geom_point(data=subset(ttracks1,cat %in% c("migration","breeding","wintering") ),mapping = aes(x=longitude,y=latitude,shape=Type),alpha=.8,size=1.7)
    gg <- gg + scale_shape_manual(values = vecShape)  + scale_linetype_manual(values = vecTypeLine)
    gg <- gg + scale_colour_manual(values=vecCol)
    gg <- gg + labs(x="",y="")
    ggsave("plotGLS_0_legend.png",gg,width = w, height = h)


    ## fig 1 tout english
    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-80, 90))
    ## gg <- gg +  coord_fixed(ratio = 1)
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
    gg <- gg + geom_path(data=subset(ttracks1,Type %in% c("English","English_equinox")),aes(x=longitude,y=latitude,group=idmigr,colour=Flyway),size=1,alpha=.6)

    gg <- gg + geom_point(data=subset(ttracks1,cat %in% c("migration","breeding","wintering") & Type %in% c("English","English_equinox")),mapping = aes(x=longitude,y=latitude,shape=Type),alpha=.8,size=1.7)
    gg <- gg + scale_shape_manual(values = vecShape)  + scale_linetype_manual(values = vecTypeLine)
    gg <- gg + scale_colour_manual(values=vecCol)
    gg <- gg + labs(x="",y="")+ theme(legend.position="none")
    ggsave("plotGLS_1_Brit.png",gg,width = w, height = h)


    ## fig 2_Brit_E East english
    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-80, 90))
    ## gg <- gg +  coord_fixed(ratio = 1)
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
    gg <- gg + geom_path(data=subset(ttracks1,Type %in% c("English","English_equinox") & Flyway=="East"),aes(x=longitude,y=latitude,group=idmigr,colour=Flyway),size=1,alpha=.6)

    gg <- gg + geom_point(data=subset(ttracks1,cat %in% c("migration","breeding","wintering") & Type %in% c("English","English_equinox") & Flyway=="East"),mapping = aes(x=longitude,y=latitude,shape=Type),alpha=.8,size=1.7)
    gg <- gg + scale_shape_manual(values = vecShape)  + scale_linetype_manual(values = vecTypeLine)
    gg <- gg + scale_colour_manual(values=vecCol)
    gg <- gg + labs(x="",y="")+ theme(legend.position="none")
    ggsave("plotGLS_2_Brit_E.png",gg,width = w, height = h)

    ## fig 2_E East english
    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-80, 90))
    ## gg <- gg +  coord_fixed(ratio = 1)
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
    gg <- gg + geom_path(data=subset(ttracks1,Type %in% c("English","English_equinox") & Flyway=="West"),aes(x=longitude,y=latitude,group=idmigr,colour=Flyway),size=1,alpha=.6)

    gg <- gg + geom_point(data=subset(ttracks1,cat %in% c("migration","breeding","wintering") & Type %in% c("English","English_equinox") & Flyway=="West"),mapping = aes(x=longitude,y=latitude,shape=Type),alpha=.8,size=1.7)
    gg <- gg + scale_shape_manual(values = vecShape)  + scale_linetype_manual(values = vecTypeLine)
    gg <- gg + scale_colour_manual(values=vecCol)
    gg <- gg + labs(x="",y="")+ theme(legend.position="none")
    ggsave("plotGLS_2_Brit_W.png",gg,width = w, height = h)

    ## fig 3_Swiss France
    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-80, 90))
    ## gg <- gg +  coord_fixed(ratio = 1)
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
    gg <- gg + geom_path(data=subset(ttracks1, Type=="Swiss" & Country=="France"),aes(x=longitude,y=latitude,group=idmigr,colour=Flyway),size=1.2,alpha=.7)

    gg <- gg + geom_point(data=subset(ttracks1,cat %in% c("migration","breeding","wintering") & Type %in% c("English","English_equinox") & Country=="France"),mapping = aes(x=longitude,y=latitude,shape=Type),alpha=.8,size=1.8)
    gg <- gg + scale_shape_manual(values = vecShape)  + scale_linetype_manual(values = vecTypeLine)
    gg <- gg + scale_colour_manual(values=vecCol)
    gg <- gg + labs(x="",y="")+ theme(legend.position="none")
    ggsave("plotGLS_3_Swiss_France.png",gg,width = w, height = h)

    ## fig 4_Swiss espagne
    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-80, 90))
    ## gg <- gg +  coord_fixed(ratio = 1)
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
    gg <- gg + geom_path(data=subset(ttracks1,Type=="Swiss" & Country=="Spain"),aes(x=longitude,y=latitude,group=idmigr,colour=Flyway),size=1.2,alpha=.7)

    gg <- gg + geom_point(data=subset(ttracks1,cat %in% c("migration","breeding","wintering") & Type=="Swiss" & Country=="Spain"),mapping = aes(x=longitude,y=latitude,shape=Type),alpha=.8,size=1.8)
    gg <- gg + scale_shape_manual(values = vecShape)  + scale_linetype_manual(values = vecTypeLine)
    gg <- gg + scale_colour_manual(values=vecCol)
    gg <- gg + labs(x="",y="")+ theme(legend.position="none")
    ggsave("plotGLS_4_Swiss_Spain.png",gg,width = w, height = h)

    ## fig 4=5_Swiss Pologne
    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-80, 90))
    ## gg <- gg +  coord_fixed(ratio = 1)
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
    gg <- gg + geom_path(data=subset(ttracks1,Type=="Swiss" & Country=="Poland"),aes(x=longitude,y=latitude,group=idmigr,colour=Flyway),size=1.2,alpha=.7)

    gg <- gg + geom_point(data=subset(ttracks1,cat %in% c("migration","breeding","wintering") & Type=="Swiss" & Country=="Poland"),mapping = aes(x=longitude,y=latitude,shape=Type),alpha=.8,size=1.8)
    gg <- gg + scale_shape_manual(values = vecShape)  + scale_linetype_manual(values = vecTypeLine)
    gg <- gg + scale_colour_manual(values=vecCol)
    gg <- gg + labs(x="",y="")+ theme(legend.position="none")
    ggsave("plotGLS_5_Swiss_Poland.png",gg,width = w, height = h)



    ## fig 6a_Swiss Finland
    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-80, 90))
    ## gg <- gg +  coord_fixed(ratio = 1)
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
    gg <- gg + geom_path(data=subset(ttracks1,Type=="Swiss" & Country=="Finland"),aes(x=longitude,y=latitude,group=idmigr,colour=Flyway),size=1.2,alpha=.7)

    gg <- gg + geom_point(data=subset(ttracks1,cat %in% c("migration","breeding","wintering") & Type=="Swiss" & Country=="Finland"),mapping = aes(x=longitude,y=latitude,shape=Type),alpha=.8,size=1.8)
    gg <- gg + scale_shape_manual(values = vecShape)  + scale_linetype_manual(values = vecTypeLine)
    gg <- gg + scale_colour_manual(values=vecCol)
    gg <- gg + labs(x="",y="")+ theme(legend.position="none")

    ggsave("plotGLS_6a_Swiss_Finland.png",gg,width = w, height = h)

    ## fig 6b_Swiss Finland
    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-80, 90))
    ## gg <- gg +  coord_fixed(ratio = 1)
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
    gg <- gg + geom_path(data=subset(ttracks1,Type %in% c("English","English_equinox") & Country=="Finland"),aes(x=longitude,y=latitude,group=idmigr,colour=Flyway),size=1.2,alpha=.7)

    gg <- gg + geom_point(data=subset(ttracks1,cat %in% c("migration","breeding","wintering") & Type %in% c("English","English_equinox") & Country=="Finland"),mapping = aes(x=longitude,y=latitude,shape=Type),alpha=.8,size=1.8)
    gg <- gg + scale_shape_manual(values = vecShape)  + scale_linetype_manual(values = vecTypeLine)
    gg <- gg + scale_colour_manual(values=vecCol)
    gg <- gg + labs(x="",y="")+ theme(legend.position="none")
    ggsave("plotGLS_6b_Brit_Finland.png",gg,width = w, height = h)



    ## fig 7_Swiss Russie
    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-80, 90))
    ## gg <- gg +  coord_fixed(ratio = 1)
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
    gg <- gg + geom_path(data=subset(ttracks1,Type=="Swiss" & Country=="Russia"),aes(x=longitude,y=latitude,group=idmigr,colour=Flyway),size=1.2,alpha=.7)

    gg <- gg + geom_point(data=subset(ttracks1,cat %in% c("migration","breeding","wintering") & Type=="Swiss" & Country=="Russia"),mapping = aes(x=longitude,y=latitude,shape=Type),alpha=.8,size=1.8)
    gg <- gg + scale_shape_manual(values = vecShape)  + scale_linetype_manual(values = vecTypeLine)
    gg <- gg + scale_colour_manual(values=vecCol)
    gg <- gg + labs(x="",y="")+ theme(legend.position="none")
    ggsave("plotGLS_7_Swiss_Russia.png",gg,width = w, height = h)


    ## fig 8_East

    vecColType <- c("English"="#1b9e77","Swiss"="#d95f02")


    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-80, 90))
    ## gg <- gg +  coord_fixed(ratio = 1)
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
    gg <- gg + geom_path(data=subset(ttracks1,Flyway=="East"),aes(x=longitude,y=latitude,group=idmigr,colour=as.factor(Type)),size=1,alpha=.6)

    gg <- gg + geom_point(data=subset(ttracks1,cat %in% c("migration","breeding","wintering") &  Flyway=="East"),mapping = aes(x=longitude,y=latitude,shape=Type),alpha=.8,size=1.8)
    gg <- gg + scale_shape_manual(values = vecShape)  + scale_linetype_manual(values = vecTypeLine)
    gg <- gg + scale_colour_manual(values=vecColType)
    gg <- gg + labs(x="",y="")+ theme(legend.position="none")
    ggsave("plotGLS_8_East.png",gg,width = w, height = h)

    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-80, 90))
    ## gg <- gg +  coord_fixed(ratio = 1)
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
    gg <- gg + geom_path(data=subset(ttracks1,Flyway=="East"),aes(x=longitude,y=latitude,group=idmigr,colour=as.factor(Type)),size=1,alpha=.6)

    gg <- gg + geom_point(data=subset(ttracks1,cat %in% c("migration","breeding","wintering") &  Flyway=="East"),mapping = aes(x=longitude,y=latitude,shape=Type),alpha=.8,size=1.8)
    gg <- gg + scale_shape_manual(values = vecShape)  + scale_linetype_manual(values = vecTypeLine)
    gg <- gg + scale_colour_manual(values=vecColType)
    gg <- gg + labs(x="",y="")
    ggsave("plotGLS_8_East_legend.png",gg,width = w, height = h)



    ## fig 9_West


    vecColType <- c("English"="#7570b3","Swiss"="#d95f02")

    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-80, 90))
    ## gg <- gg +  coord_fixed(ratio = 1)
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
    gg <- gg + geom_path(data=subset(ttracks1,Flyway=="West"),aes(x=longitude,y=latitude,group=idmigr,colour=as.factor(Type)),size=1,alpha=.6)

    gg <- gg + geom_point(data=subset(ttracks1,cat %in% c("migration","breeding","wintering") &  Flyway=="West"),mapping = aes(x=longitude,y=latitude,shape=Type),alpha=.8,size=1.8)
    gg <- gg + scale_shape_manual(values = vecShape)  + scale_linetype_manual(values = vecTypeLine)
    gg <- gg + scale_colour_manual(values=vecColType)
    gg <- gg + labs(x="",y="")+ theme(legend.position="none")
    ggsave("plotGLS_9_West.png",gg,width = w, height = h)

    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-80, 90))
    ## gg <- gg +  coord_fixed(ratio = 1)
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
    gg <- gg + geom_path(data=subset(ttracks1,Flyway=="West"),aes(x=longitude,y=latitude,group=idmigr,colour=as.factor(Type)),size=1,alpha=.6)

    gg <- gg + geom_point(data=subset(ttracks1,cat %in% c("migration","breeding","wintering") &  Flyway=="West"),mapping = aes(x=longitude,y=latitude,shape=Type),alpha=.8,size=1.8)
    gg <- gg + scale_shape_manual(values = vecShape)  + scale_linetype_manual(values = vecTypeLine)
    gg <- gg + scale_colour_manual(values=vecColType)
    gg <- gg + labs(x="",y="")
    ggsave("plotGLS_9_West_legend.png",gg,width = w, height = h)


    ## fig10_Germany_all
    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-80, 90))
    ## gg <- gg +  coord_fixed(ratio = 1)
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
    gg <- gg + geom_path(data=subset(ttracks1,Type %in% c("English","English_equinox")&Country=="Germany"),aes(x=longitude,y=latitude,group=idmigr,colour=Flyway),size=1,alpha=.6)

    gg <- gg + geom_point(data=subset(ttracks1,cat %in% c("migration","breeding","wintering") & Type %in% c("English","English_equinox")&Country=="Germany"),mapping = aes(x=longitude,y=latitude,shape=Type),alpha=.8,size=1.7)
    gg <- gg + scale_shape_manual(values = vecShape)  + scale_linetype_manual(values = vecTypeLine)
    gg <- gg + scale_colour_manual(values=vecCol)
    gg <- gg + labs(x="",y="")+ theme(legend.position="none")
    ggsave("plotGLS_10_British_Germany.png",gg,width = w, height = h)


    ## fig11_Germany_all
    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-80, 90))
    ## gg <- gg +  coord_fixed(ratio = 1)
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
    gg <- gg + geom_path(data=subset(ttracks1,Type %in% c("English","English_equinox")&Country!="Germany"),aes(x=longitude,y=latitude,group=idmigr,colour=Flyway),size=1,alpha=.6)

    gg <- gg + geom_point(data=subset(ttracks1,cat %in% c("migration","breeding","wintering") & Type %in% c("English","English_equinox")&Country!="Germany"),mapping = aes(x=longitude,y=latitude,shape=Type),alpha=.8,size=1.7)
    gg <- gg + scale_shape_manual(values = vecShape)
    gg <- gg + scale_colour_manual(values=vecCol)
    gg <- gg + labs(x="",y="")+ theme(legend.position="none")
    ggsave("plotGLS_11_British_Not_Germany.png",gg,width = w, height = h)















}



make.ggT_iso <- function(w=8.32,h=7,nb_fr=74,nb_west=238,nb_east=297,nb_ku=78) {
                                        # w=8.32;h=7
                                        # nb_fr=h4;nb_west=238;nb_east=297;nb_ku=78
    library(raster)
    library(ggplot2)
    library(ggmap)


                                        #   # Ortolan_France_Breed74_Bowen_MAD
    r <- raster("isotope/Ortolan_W.Flyway_Winter_Bowen_MAD.asc") # any raster you want to plot

    rtp <- rasterToPolygons(r)
    rtp@data$id <- 1:nrow(rtp@data)   # add id column for join

    rtpFort <- fortify(rtp, data = rtp@data)
    rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
    rtpFortMer$Ortolan_W.Flyway_Winter_Bowen_MAD <- as.numeric(rtpFortMer$Ortolan_W.Flyway_Winter_Bowen_MAD)

    rtpFortMer$prop <- rtpFortMer$Ortolan_W.Flyway_Winter_Bowen_MAD/nb_west

    rtpFortMer$prop2 <- rtpFortMer$Ortolan_W.Flyway_Winter_Bowen_MAD/max(rtpFortMer$Ortolan_W.Flyway_Winter_Bowen_MAD)


    r2 <- raster("isotope/Ortolan_France_Breed74_Bowen_MAD.asc") # any raster you want to plot
    rtp2 <- rasterToPolygons(r2)
    rtp2@data$id <- 1:nrow(rtp2@data)   # add id column for join

    rtpFort2 <- fortify(rtp2, data = rtp2@data)
    rtpFortMer2 <- merge(rtpFort2, rtp2@data, by.x = 'id', by.y = 'id')  # join data
    rtpFortMer2$prop <- rtpFortMer2$Ortolan_France_Breed74_Bowen_MAD/nb_fr
    rtpFortMer2$prop2 <- rtpFortMer2$Ortolan_France_Breed74_Bowen_MAD/max(rtpFortMer2$Ortolan_France_Breed74_Bowen_MAD)

    WorldData <- map_data(map="world")
    WorldData <- fortify(WorldData)


    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-80, 90))
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
    ##  gg <- gg +  coord_fixed(ratio = 1)

    gg <- gg + geom_polygon(data = rtpFortMer,
                            aes(x = long, y = lat, group = group, fill = prop),
                            alpha = 0.9,
                            size = 0)   ## size = 0 to remove the polygon outlines
    gg <- gg + geom_polygon(data = rtpFortMer2,
                            aes(x = long, y = lat, group = group, fill = prop),
                            alpha = 0.9,
                            size = 0)   ## size = 0 to remove the polygon outlines
    gg <- gg + scale_fill_distiller(palette = "YlGnBu",direction = 1)#,limits = c(0,0.5))
    gg <- gg + xlab("")+ylab("")
    ggsave("plotIsotopeWest_1_legend.png",gg,width = w, height = h)



    gg <- gg + theme(legend.position="none")


    ggsave("plotIsotopeWest_1.png",gg,width = w, height = h)



    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("mercator")
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
    gg <- gg + geom_polygon(data = rtpFortMer,
                            aes(x = long, y = lat, group = group, fill = prop2),
                            alpha = 0.9,
                            size = 0)   ## size = 0 to remove the polygon outlines
    gg <- gg + geom_polygon(data = rtpFortMer2,
                            aes(x = long, y = lat, group = group, fill = prop2),
                            alpha = 0.9,
                            size = 0)   ## size = 0 to remove the polygon outlines
    gg <- gg + scale_fill_distiller(palette = "YlGnBu",direction = 1,limits = c(0,1))
    gg <- gg + xlab("")+ylab("")
    ggsave("plotIsotopeWest_2_legend.png",gg,width = w, height = h)


    gg <- gg + theme(legend.position="none")
    ggsave("plotIsotopeWest_2.png",gg,width = w, height = h)



    r <- raster("isotope/Ortolan_E.Flyway_Winter_Bowen_MAD.asc") # any raster you want to plot
    rtp <- rasterToPolygons(r)
    rtp@data$id <- 1:nrow(rtp@data)   # add id column for join

    rtpFort <- fortify(rtp, data = rtp@data)
    rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
    rtpFortMer$prop <- rtpFortMer$Ortolan_E.Flyway_Winter_Bowen_MAD /nb_east
    rtpFortMer$prop2 <- rtpFortMer$Ortolan_E.Flyway_Winter_Bowen_MAD /max(rtpFortMer$Ortolan_E.Flyway_Winter_Bowen_MAD)



    r2 <-  raster("isotope/Ortolan_Kuwait_east_Bowen_MAD.asc")


    rtp2 <- rasterToPolygons(r2)
    rtp2@data$id <- 1:nrow(rtp2@data)   # add id column for join

    rtpFort2 <- fortify(rtp2, data = rtp2@data)
    rtpFortMer2 <- merge(rtpFort2, rtp2@data, by.x = 'id', by.y = 'id')  # join data

    rtpFortMer2$prop <- rtpFortMer2$Ortolan_Kuwait_east_Bowen_MAD / nb_ku
    rtpFortMer2$prop2 <- rtpFortMer2$Ortolan_Kuwait_east_Bowen_MAD / max(rtpFortMer2$Ortolan_Kuwait_east_Bowen_MAD)



    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("mercator")
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
    gg <- gg + geom_polygon(data = rtpFortMer,
                            aes(x = long, y = lat, group = group, fill = prop),
                            alpha = 0.9,
                            size = 0)   ## size = 0 to remove the polygon outlines
    gg <- gg + geom_polygon(data = rtpFortMer2,
                            aes(x = long, y = lat, group = group, fill = prop),
                            alpha = 0.9,
                            size = 0)   ## size = 0 to remove the polygon outlines
    gg <- gg + scale_fill_distiller(palette = "YlGnBu",direction = 1)
    gg <- gg + xlab("")+ylab("")

    ggsave("plotIsotopeEst_1_legend.png",gg,width=w,height = h)
    gg <- gg + theme(legend.position="none")
    ggsave("plotIsotopeEst_1.png",gg,width=w,height = h)


    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("mercator")
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
    gg <- gg + geom_polygon(data = rtpFortMer,
                            aes(x = long, y = lat, group = group, fill = prop2),
                            alpha = 0.9,
                            size = 0)   ## size = 0 to remove the polygon outlines
    gg <- gg + geom_polygon(data = rtpFortMer2,
                            aes(x = long, y = lat, group = group, fill = prop2),
                            alpha = 0.9,
                            size = 0)   ## size = 0 to remove the polygon outlines
    gg <- gg + scale_fill_distiller(palette = "YlGnBu",direction = 1,limits = c(0,1))
    gg <- gg + xlab("")+ylab("")
    ggsave("plotIsotopeEst_2_legend.png",gg,width = w, height = h)
    gg <- gg + theme(legend.position="none")

    ggsave("plotIsotopeEst_2.png",gg,width = w, height = h)




}



make.ggT_distrib <- function(w,h) {
    library(maptools)
    library(rgdal)

    distrib.shp <- readOGR("isotope/Emberiza_hortulana_22720916_KK_modified.shp")
    distrib.shp@data$id <- rownames(distrib.shp@data)

    distrib <- fortify(distrib.shp)
    distrib <- merge(distrib, distrib.shp@data, by="id")

    genet.shp <- readOGR("genet/genet_iso.shp")
    genet.shp@data$id <- rownames(genet.shp@data)
    genet <- as.data.frame(genet.shp)

    colnames(genet)[4:5] <- c("long","lat")



    WorldData <- map_data(map="world")
    WorldData <- fortify(WorldData)

    vecCol <- c("Western"="#1758ee","Northern"="#12e01e","Eastern"="#e01612","NA"="black")#
    vecShape <- c("TRUE"=16,"FALSE"=17)

    gg <- ggplot()
    gg <-  gg + geom_map(data=WorldData, map=WorldData,
                         aes(x=long, y=lat, group=group, map_id=region),
                         fill="white", colour="#7f7f7f", size=0.5)
    gg <- gg + coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-80, 90))
    gg <- gg + coord_cartesian(xlim=c(-17,50),ylim=c(5,65))
                                        #gg <- gg +  coord_fixed(ratio = 1)

    gg <- gg + geom_polygon(data = distrib,
                            aes(x = long, y = lat, group = group), alpha =.8,fill="#fecc5c",size = 1)   ## size = 0 to remove the polygon outlines
    gg <- gg + geom_point(data=genet,aes(x=long,y=lat,colour=cluster,shape=genet),size=3,alpha=.8)
    gg <- gg + scale_shape_manual(values = vecShape)
    gg <- gg + scale_colour_manual(values=vecCol)
    gg <- gg + labs(x="",y="")+ theme(legend.position="none")
    gg


    ggsave("plotDistrib.png",gg,width = w, height = h)

}




make.ortolan_shp <- function(w=8.32,h=7,nb_fr=74,nb_west=238,nb_east=297,nb_ku=78) {
                                        # w=8.32;h=7
                                        # nb_fr=h4;nb_west=238;nb_east=297;nb_ku=78
    library(raster)
    library(ggplot2)
    library(ggmap)
    library(sf)
    library(rgdal)
    library(sp)



    r1 <- readGDAL("isotope/Ortolan_France_Breed74_Bowen_MAD.asc")
    r1@data[!is.na(r1@data)] <- 1
    r1p <- as( r1,'SpatialPolygonsDataFrame')
    r1p_sf <- st_as_sf(r1p)
    st_crs(r1p_sf) <- 4326
    r1p_sf_u <- st_union(st_buffer(r1p_sf,0.0001))
    plot(r1p_sf_u)

        r2 <- readGDAL("isotope/Ortolan_Kuwait_east_Bowen_MAD.asc")
    r2@data[!is.na(r2@data)] <- 1
    r2p <- as(r2,'SpatialPolygonsDataFrame')
    r2p_sf <- st_as_sf(r2p)
    st_crs(r2p_sf) <- 4326
    r2p_sf_u <- st_union(st_buffer(r2p_sf,0.0001))
    plot(r2p_sf_u)


    r3 <- readGDAL("isotope/Ortolan_W.Flyway_Winter_Bowen_MAD.asc")
    r3@data[!is.na(r3@data)] <- 1
    r3p <- as(r3,'SpatialPolygonsDataFrame')
    r3p_sf <- st_as_sf(r3p)
    st_crs(r3p_sf) <- 4326
    r3p_sf_u <- st_union(st_buffer(r3p_sf,0.0001))
   plot(r3p_sf_u)



    r <- st_union(r1p_sf_u,r2p_sf_u)
    r <- st_union(r,r3p_sf_u)
    plot(r)


      world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
    gg <- ggplot()
    gg <- gg + geom_sf(data = world1,fill="white", colour="#7f7f7f", size=0.2)
    gg <- gg + geom_sf(data=r,fill="red",alpha=.5)
     gg <- gg + coord_sf(xlim=c(-17,100),ylim=c(5,65))
ggsave("ortolan_distribution.png",gg)

    st_write(r,dsn = "ortolan_distribution", driver = 'ESRI Shapefile')
    write_shape(r,"ortolan_distribution")


    d <-  st_read("isotope/Emberiza_hortulana_22720916_KK_modified.shp")
 gg <- ggplot()
    gg <- gg + geom_sf(data = world1,fill="white", colour="#7f7f7f", size=0.2)
    gg <- gg + geom_sf(data=d,fill="red",alpha=.5)
    gg <- gg + coord_sf(xlim=c(-17,100),ylim=c(5,65))
    gg
ggsave("ortolan_distribution_modified.png",gg)

}


