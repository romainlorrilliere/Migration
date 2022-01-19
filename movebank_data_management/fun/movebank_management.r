

library(move)
library(data.table)
library(ggplot2)
library(maps)





getDataMovebank <- function(username="romainlorrilliere",pw,study_id="1077731101"){

                                        # username="romainlorrilliere";pw;study_id=1077731101

    log <- movebankLogin(username,pw)
    study <- move::getMovebank("study",login= log,study_id=study_id)

tag <- getMovebank("tag",login= log,study_id=study_id)
ind <- getMovebank("individual",login= log,study_id=study_id)

d <- getMovebank("event",login= log,study_id=study_id)

setDT(d)
file <- paste0("data_courlis_",as.Date(Sys.time()),".csv")
fwrite(d,file)
the_ind <- unique(d[,individual_id])


d[,individual_id_txt := as.character(individual_id)]
d1 <- d[individual_id==the_ind[1]]
dim(d1)

dd <- d[!is.na(individual_id)]
world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))

gg <- ggplot()
gg <- gg + geom_sf(data = world1,fill="white", colour="#7f7f7f", size=0.5)
gg <- gg + geom_sf(data = world1,fill=NA, colour="#7f7f7f", size=0.5)
gg <- gg + coord_sf(xlim=c(-10,60),ylim=c(40,65))
gg <- gg + geom_path(data=dd,aes(x=location_long,y=location_lat,group=individual_id_txt,colour=individual_id_txt),size=0.8,alpha=.5)
gg

}
