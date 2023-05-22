##REGRESIONES##

#librerias
require(pacman)
p_load(tidyverse,stargazer,coefplot,rio,skimr, sf, leaflet, tmaptools,ggmap,osmdata, rvest)

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
restaurantes <- opq(bbox = getbb("Bogota")) %>%
                add_osm_feature(key = "amenity", value = "restaurant") %>%
                osmdata_sf() %>% .$osm_points %>% select(osm_id, name)


parques <- opq(bbox = getbb("Bogota")) %>%
           add_osm_feature(key = "leisure", value = "park") %>%
           osmdata_sf() %>% .$osm_polygons %>% select(osm_id,name)

mapa <- st_union(x=restaurantes, y=parques)

#Visualizaciones
leaflet() %>% addTiles() %>% addPolygons(data= bogota)
leaflet() %>% addTiles() %>% addCircles(data=restaurantes)


#geocodificar direcciones
geocode_OSM("calle 19a # 1-10", as.sf=T)


#exportar 


##WEB-SCRAPING Y PROCESAMIENTO DE TEXTO##

url = "https://es.wikipedia.org/wiki/Departamentos_de_Colombia"
my_html = read_html(url)
class(my_html)

my_html %>% html_element(xpath = '//*[@id="firstHeading"]/span') %>% html_text()

tablas = my_html %>% html_table()
tablaDEPA <- tablas[[4]]
export(tablaDEPA, file= "output/tabla_departamentos.xlsx")


