###################################################################
###  Script pour le graphe de synthèse pour le papier ortolan  ####
###################################################################

vecPackage=c("ggplot2","ggmap","mapproj","lubridate","maps","mapdata","dplyr","rgdal","maptools","raster","sf","data.table","ggsn","gridExtra","OpenStreetMap","ggrepel")
ip <- installed.packages()[,1]

for(p in vecPackage){
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "https://pbil.univ-lyon1.fr/CRAN/",dependencies=TRUE)
     library(p,character.only = TRUE)

}



main_graph <- function() {

    vec_file=c("200209_aout2020.txt")
    f <- paste0("data/",vec_file)
    d <- fread(f,dec=",")

    d[,time_num := as.numeric(as.POSIXlt(UTC_datetime ,format="%d/%m/%Y %H:%M"))]

    d[,UTC_datetime := format(as.POSIXlt(UTC_datetime ,format="%d/%m/%Y %H:%M"))]

    d <- d[UTC_datetime >= as.POSIXlt("2020-08-07 12:00") & UTC_datetime <= as.POSIXlt("2020-08-09 12:00") ]


    date <- data.table(cat = 1:3,cat_txt = c("7_August_noon_to_8_August_noon","8_August_noon_to_8_August_22:30","8_August_22:30_to_9_August_noon"))


    d1 <- d[UTC_datetime <= as.POSIXlt("2020-08-08 12:00")]
    d1[,cat := 1]
    d1[,time_num_sc := (time_num - min(time_num)) / (max(time_num) - min(time_num))]


    d2 <- d[UTC_datetime >= as.POSIXlt("2020-08-08 12:00") & UTC_datetime <= as.POSIXlt("2020-08-08 22:42")]
    d2[,cat := 2]
    d2[,time_num_sc := (time_num - min(time_num)) / (max(time_num) - min(time_num))]

    d3 <- d[UTC_datetime >= as.POSIXlt("2020-08-08 22:42")]
    d3[,cat := 3]
    d3[,time_num_sc := (time_num - min(time_num)) / (max(time_num) - min(time_num))]

    d <- rbind(d1,d2,d3)

    #d[,cat := ifelse(UTC_datetime <= as.POSIXlt("2020-08-08 12:00"), 1 ,
     #         ifelse(UTC_datetime <= as.POSIXlt("2020-08-08 22:30"), 2 ,3))]



    d <- merge(d,date)


    d[,cat_txt_fact := gsub("_"," ",cat_txt)]



    d[,cat_txt_fact := factor(cat_txt_fact,levels = c("7 August noon to 8 August noon" ,"8 August noon to 8 August 22:30" ,"8 August 22:30 to 9 August noon"))]

    d.mercator <- as.data.frame( OpenStreetMap::projectMercator( lat = d$Latitude, long = d$Longitude ) )
    d <- cbind(d,d.mercator)


    dfirst <- d[, list(UTC_datetime = min(UTC_datetime)),by = cat_txt]
    dfirst <- merge(dfirst,d,by=c("UTC_datetime","cat_txt"))

    dlast <- d[,list(UTC_datetime = max(UTC_datetime)),by = cat_txt]
    dlast <- merge(dlast,d,by=c("UTC_datetime","cat_txt"))


    upperL <- c(max(d[,Latitude]) + 0.005,min(d[,Longitude])-0.001)
    lowerR <- c(min(d[,Latitude]),max(d[,Longitude]) + 0.005)

    mp <- openmap(upperL,lowerR,type='bing')




   d[,i :=1:.N]
   ## d[,i_to := i_from -1]


    ## d_from  <- d[,.(cat_txt_fact,i=i_from,x_from = x,y_from=y,time_num,UTC_datetime,time_num_sc)]
    ## d_to  <- d[,.(cat_txt_fact,i=i_to,x_to = x,y_to = y)]

#    d_arrow <- merge(d_from,d_to,by=c("cat_txt_fact","i"))

 #   select_label  <-  c("2020-08-07 14:37:00","2020-08-08 09:55:00","2020-08-08 04:56:00","2020-08-08 16:20:00","2020-08-08 17:01:00","2020-08-08 17:00:00","2020-08-08 20:13:00","2020-08-08 22:42:00","2020-08-09 02:12:00","2020-08-09 11:45:00")

 #   d_arrow <- d_arrow[UTC_datetime %in% select_arrow]
                                        #  vecCol <- c("07_August_noon_to_08_August_noon" ="#ffffb3" ,"08_August_noon_to_08_August_22:30"="#fdb863" ,"08_August_22:30_to_09_August_noon"="#e41a1c")



#scale_colour_gradientn(colours = c("darkblue","lightblue","yellow","orange","red"), values = c(1596794760,1596812625,1596880950,1596895395,1596967200))
library(ggrepel)

    gg <- autoplot.OpenStreetMap(mp) + facet_wrap(.~cat_txt_fact)
    gg <- gg + labs(x="",y="",colour="")+ theme(legend.position="none")
                                        #gg <- gg + scale_colour_manual(values=vecCol) + scale_alpha_manual(values=vecAlpha)
#gg <- gg + scale_colour_gradientn(colours = c("darkblue","blue","lightblue","yellow","orange","red","darkred"),values=c(0,.1,.3,.5,.7,.9,1))
gg <- gg + scale_colour_gradientn(colours = c("yellow","yellow","orange","red","darkred","darkred"),values=c(0,.1,.3,.6,.9,1))

    gg <- gg + geom_point(data = dfirst,mapping=aes(x=x,y=y,colour=time_num_sc),size=4,shape=15) + geom_point(data = dfirst,mapping=aes(x=x,y=y),colour="white",size=2,shape=15)+ geom_point(data = dlast,mapping=aes(x=x,y=y,colour=time_num_sc),size=4,shape=17) +  geom_point(data = dlast,mapping=aes(x=x,y=y),colour="white",size=2,shape=17)

    gg <- gg + geom_path(data=d,mapping=aes(x=x,y=y,colour=time_num_sc),alpha= 0.45 ,size=1.5)

     gg



#ggplot(data=d,aes(x=x,y=y,colour=time_num_sc)) + geom_path() +  scale_colour_gradientn(colours = c("darkblue","blue","lightblue","yellow","orange","red","darkred"),values=c(0,.1,.3,.5,.7,.9,1))+ facet_wrap(.~cat_txt_fact)


    ggsave("output/courlis_3_panel_bis.png",gg)




##    gg <- autoplot.OpenStreetMap(mp)
##    gg <- gg + labs(x="",y="",colour="")+ theme(legend.position="none")
##    gg <- gg + scale_colour_manual(values=vecCol) + scale_alpha_manual(values=vecAlpha)
##    gg <- gg + geom_path(data=d,mapping=aes(x=x,y=y,colour=cat_txt,alpha=cat_txt),size=1)
##    gg <- gg + geom_point(data = dfirst,mapping=aes(x=x,y=y,colour=cat_txt,alpha=cat_txt),size=4,shape=15) + geom_point(data = dfirst,mapping=aes(x=x,y=y,alpha=cat_txt),colour="white",size=2,shape=15)+ geom_point(data = dlast,mapping=aes(x=x,y=y,colour=cat_txt,alpha=cat_txt),size=4,shape=17) +  geom_point(data = dlast,mapping=aes(x=x,y=y,alpha=cat_txt),colour="white",size=2,shape=17)
##    gg
##
##
##    ggsave("output/courlis_1_panel.png",gg)
##
##
##
##
##    d <- d[cat>1]
##    dfirst <- dfirst[cat>1]
##    dlast <- dlast[cat>1]
##
##    gg <- autoplot.OpenStreetMap(mp)
##    gg <- gg + labs(x="",y="",colour="")+ theme(legend.position="none")
##    gg <- gg + scale_colour_manual(values=vecCol) + scale_alpha_manual(values=vecAlpha)
##    gg <- gg + geom_path(data=d,mapping=aes(x=x,y=y,colour=cat_txt,alpha=cat_txt),size=1)
##    gg <- gg + geom_point(data = dfirst,mapping=aes(x=x,y=y,colour=cat_txt,alpha=cat_txt),size=4,shape=15) + geom_point(data = dfirst,mapping=aes(x=x,y=y,alpha=cat_txt),colour="white",size=2,shape=15)+ geom_point(data = dlast,mapping=aes(x=x,y=y,colour=cat_txt,alpha=cat_txt),size=4,shape=17) +  geom_point(data = dlast,mapping=aes(x=x,y=y,alpha=cat_txt),colour="white",size=2,shape=17)
##    gg
##
##
##    ggsave("output/courlis_1bis_panel.png",gg)
##
##
##    gg <- autoplot.OpenStreetMap(mp) + facet_wrap(.~cat_txt_fact)
##    gg <- gg + labs(x="",y="",colour="")+ theme(legend.position="none")
##    gg <- gg + scale_colour_manual(values=vecCol) + scale_alpha_manual(values=vecAlpha)
##    gg <- gg + geom_path(data=d,mapping=aes(x=x,y=y,colour=cat_txt),alpha= 0.8 ,size=1)
##
##     gg <- gg + geom_point(data = dfirst,mapping=aes(x=x,y=y,colour=cat_txt),size=4,shape=15) + geom_point(data = dfirst,mapping=aes(x=x,y=y),colour="white",size=2,shape=15)+ geom_point(data = dlast,mapping=aes(x=x,y=y,colour=cat_txt),size=4,shape=17) +  geom_point(data = dlast,mapping=aes(x=x,y=y),colour="white",size=2,shape=17)
##    gg
##    ggsave("output/courlis_2_panel.png",gg)
##



}





