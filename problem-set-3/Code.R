##REGRESIONES##

#librerias
require(pacman)
p_load(tidyverse,stargazer,coefplot,rio,skimr, sf, leaflet, tmaptools,ggmap,osmdata)

#datos
df = import("input/data_regresiones.rds")

#modelos
modelo_1 = lm(price ~ dist_cbd + as.factor(property_type) , data= df)
modelo_2 = lm(price ~ dist_cbd + as.factor(property_type) + rooms , data= df)
modelo_3 = lm(price ~ dist_cbd + as.factor(property_type) + rooms + bathrooms, data= df)

#visualizacion
coefplot(model = modelo_3) + theme_test()

##exportar resultados 
ggsave(filename = "output/plot_regresiones.png")
stargazer(modelo_1,modelo_2,modelo_3,
          type = "text", 
          out = "output/resultados_regresiones.xlsx")


##DATOS ESPACIALES##

#descargar datos
restaurantes <- opq(bbox = getbb("Cucuta Colombia")) %>%
                add_osm_feature(key = "amenity", value = "restaurant") %>%
                osmdata_sf() %>% .$osm_points %>% select(osm_id, name)


parques <- opq(bbox = getbb("Cucuta Colombia")) %>%
           add_osm_feature(key = "leisure", value = "park") %>%
           osmdata_sf() %>% .$osm_polygons %>% select(osm_id,name)

#
leaflet() %>% addTiles() %>% addPolygons(data=parques)





