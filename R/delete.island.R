delete.island <- function(sf){
    sf2=st_geometry(sf)
    for (i in 1:nrow(sf)) {
        cast=st_cast(sf2[i],'POLYGON')
        area=as.numeric(st_area(cast))/10000000000
        area
        if (length(area)<2){
            next(i)
        }else{
            st_geometry(sf[i,])=st_combine(cast[area>=0.1758531715,])
        }
    }
    sf
}
sf_area <- function(sf){
    cast=st_cast(st_geometry(sf),'POLYGON')
    area=as.numeric(st_area(cast))
    area/10000000000
}
