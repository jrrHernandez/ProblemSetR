##REGRESIONES##

#librerias
require(pacman)
p_load(tidyverse,stargazer,coefplot,rio,skimr, sf, leaflet, tmaptools,ggmap,osmdata, rvest, wordcloud, purrr, dplyr, tm)

#datos
df = import("input/data_regresiones.rds")

#modelos
modelo_1 = lm(price ~ dist_cbd + as.factor(property_type) , data= df)
modelo_2 = lm(price ~ dist_cbd + as.factor(property_type) + rooms , data= df)
modelo_3 = lm(price ~ dist_cbd + as.factor(property_type) + rooms + bathrooms, data= df)

#visualizacion
coefplot(model = modelo_3) + theme_test()

#exportar resultados 
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

parrafos <- my_html %>% html_elements("p") %>% html_text() 
texto <- c(
  "La Constitución de 1991 establece a Colombia como una república unitaria y descentralizada que se divide administrativa y políticamente en 33 divisiones: 32 departamentos, los cuales son gobernados desde sus respectivas ciudades capitales, y un distrito capital, Bogotá. Los departamentos forman áreas culturales y económicas dentro de las regiones geográficas. En Colombia los recursos pasan de la nación al departamento y de este al municipio; a excepción de Bogotá, que como distrito capital, recibe directamente de la nación (a través de ley de regalías).",
  "La soberanía recae sobre la Nación como unidad y, a su vez, Colombia posee una descentralización administrativa por medio de la cual parte de la administración del Estado se reparte entre los 32 departamentos y los municipios.",
  "El origen de los departamentos colombianos se encuentra en la división político-administrativa de la Nueva Granada (hoy Colombia y Panamá), la cual se organizaba en provincias que más o menos se correspondían territorialmente a los departamentos actuales. Con la constitución de 1858 el sistema político de la nación cambió al de una república federal, su nombre pasó a ser Estados Unidos de Colombia y las provincias fueron reemplazas por nueve estados soberanos (Antioquia, Bolívar, Boyacá, Cauca, Cundinamarca, Magdalena, Panamá, Santander y Tolima).",
  "La constitución centralista de 1886 cambió el nombre del país de forma definitiva a República de Colombia y convirtió a los estados soberanos en departamentos. Con las reformas de 1905 y 1910 se dieron las pautas para escindir territorios de los distintos departamentos y formar nuevas entidades (departamentos, intendencias y comisarías). Con la nueva constitución de 1991, las intendencias y comisarías existentes fueron promovidas a departamentos.",
  "Debe resaltarse que, en Colombia, el poder legislativo es centralizado, por ser el país una república unitaria (no federal); esto es, solo el Congreso de la República puede legislar, en tanto el poder ejecutivo sí tienen representantes en los Departamentos, Municipios y/o Distritos. Erróneamente se cree que las Asambleas Departamentales como los Concejos municipales son cuerpos legislativos, pero estas dos corporaciones públicas son órganos del poder ejecutivo y sus actos, denominados actos administrativos, si bien se constituyen en órdenes no son leyes. Por ende, no existe poder legislativo ni judicial que emane de una entidad territorial: ya sea un departamento, municipio o distrito. El poder legislativo y judicial emana totalmente de la Nación y solo el poder ejecutivo del orden Nacional también emana de la Nación.",
  "Así las cosas, en el ente territorial de nivel departamental puede perfectamente afirmarse que el poder ejecutivo está a cargo de un gobernador, elegido por voto popular desde 1992, y que ejerce por un período de 4 años a partir de 2004. El gobernador nombra un gabinete compuesto por sus secretarios.",
  "Las Asambleas departamentales o sus miembros son elegidos por voto popular para el mismo período que el gobernador. La Asamblea cuenta entre 11 y 31 diputados de acuerdo a la población del departamento.",
  "Dos o más departamentos pueden asociarse en regiones administrativas de planificación.",
  "Los departamentos se subdividen en municipios, cuya administración está a cargo de un alcalde y un concejo municipal elegidos por voto popular para un período igual que el de los dignatarios departamentales. Algunos departamentos con elevado número de municipios han optado por reconocer la existencia de provincias o subregiones, siendo estas un conjunto de municipios con afinidades culturales, geográficas o históricas, donde se localiza un centro urbano de mayor importancia como capital y desde allí funcionan instituciones locales con jurisdicción sobre los otros municipios. Estas agrupaciones sin embargo carecen de reconocimiento jurídico.",
  "Colombia es una república unitaria y descentralizada, está última según lo establece el artículo 306 de la actual Constitución de 1991, dos o más departamentos podrán constituirse en regiones administrativas y de planificación, con personería jurídica, autonomía y patrimonio propio, para desarrollar económica y socialmente el territorio.",
  "Actualmente se encuentran constituidas las siguientes regiones administrativas de planificación:",
  "El decreto 1421 por el cual se dicta el régimen especial para el Distrito Capital de Bogotá enuncia que «de conformidad con lo dispuesto en el artículo 322 de la constitución política, la ciudad de Bogotá, capital de la República y del departamento de Cundinamarca, se organiza como distrito capital».",
  "Según el decreto «las atribuciones administrativas que la Constitución y las leyes confieren a los departamentos se entienden otorgadas al Distrito Capital, en lo que fuere compatible con el régimen especial de este último. Las disposiciones de la Asamblea y de la Gobernación de Cundinamarca no rigen en el territorio del Distrito»,[9] lo cual convierte a Bogotá en una entidad territorial de primer orden en Colombia.",
  "El gobierno y la administración de Bogotá están a cargo de:",
  "Bogotá se divide a su vez en localidades las cuales son controladas por:",
  "A nivel urbano, Cundinamarca presenta la mayor densidad poblacional. Adicionalmente, presentan altas densidades: Antioquia, Risaralda y Bolívar en sus áreas urbanas que van desde 11.366 hasta 10.571 hab./km².",
  "A nivel de las áreas rurales o «resto», los departamentos con mayor densidad son: San Andrés y Providencia con 476 hab./km²; Risaralda con 52 hab./km² y Caldas con 41 hab./km²."
)

texto <- tolower(texto)
words <- unlist(strsplit(texto, "\\W+"))
word_freq <- table(words)
wordcloud(words = names(word_freq), freq = word_freq, random.order = FALSE)


