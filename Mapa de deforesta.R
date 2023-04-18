#------------------------------------------------------------------------
library(RPostgres)
library(sf)
library(ggplot2)
library(ggspatial)
library(raster)
library(ggnewscale)
dvr         <- RPostgres::Postgres()
db          <- 'postgres'  ##Nombre de la BBDD
host_db     <- 'localhost'
db_port     <- '5432' 
db_user     <- 'postgres'  ##Tu usuario
db_password <- 'gflorezc' ##Tu contraseña 

# 3.0 Conexión
con <- dbConnect(dvr, dbname = db, host=host_db, port=db_port, user=db_user, password=db_password)  
dbListTables(con)


Per          <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Loreto         <- subset(Per  , NAME_1  == "Loreto")


#Parte01 = stack("Raster/11.tif")
#Parte01_alt    <- crop(Parte01, Loreto)                           #
#Parte01_alt   <- Parte01_alt <- mask(Parte01_alt, Loreto)
#plot(Parte01_alt)

#writeRaster(Parte01_alt, "Raster/Loreto.tif")

Parte01 = stack("Raster/Loreto.tif")

Parte01_df  <-  rasterToPoints(Parte01)
Parte01_d   <-  data.frame(Parte01_df)
colnames(Parte01_d) = c("x", "y", "Defore")

PaleN02020 <- c("#004b23", "#006400", "#007200", "#008000", "#38b000", "#70e000", "#9ef01a", "#ccff33")

PaleN02020 <- c( "#ccff33", "#9ef01a", "#70e000", "#38b000", "#008000", "#007200", "#006400","#004b23")
library(tmap)

Mapa= tm_shape(Parte01) +
  tm_raster( palette = PaleN02020, n = 5,  title = "Porcentaje de cobertura \narbórea", style = "cont")+
  tm_layout( title = "",
             bg.color="black", 
             legend.bg.color = "black", 
             title.color  = "white",
             title.size = 14,
             legend.title.color = "white",
             legend.text.color = "white",

             legend.position = c(0.005,0.10) , scale=0.61, legend.frame = T,
             fontface="bold",
             legend.format = c(text.align = "right", 
                               text.separator = "-"))+
  tm_scale_bar(width = 0.25, text.size = 0.5, text.color = "white", color.dark = "lightsteelblue4", 
               position = c(.01, 0.005), lwd = 1, color.light= "white")+
  tm_compass(type="rose", position=c(.86, 0.05), text.color = "white")+
  tm_credits("Bosque de Loreto 2000 - 2021", position = c(.58, .9), col = "white", fontface="bold", size=2, fontfamily = "serif")+
 
  tm_credits("Data: High-resolution global maps of 21st-century \nforest cover change : \nhttps://doi.org/10.1126/science.1244693 \n@Ing. gflorezc", position = c(0.01, .04), col = "white", fontface="bold")+
  tm_logo(c("https://imagizer.imageshack.com/img922/8348/QhP0ud.png",
            system.file("img/tmap.png", package = "tmap")),height = 6, position = c(0.5, 0.05))

Mapa

tmap_save(Mapa, "Loreto.png", dpi = 1200, width = 9, height = 9)



library(grid)
library(png)
library(ggimage)
Logo <- readPNG("Logo R.png", FALSE)
Logo_png <- rasterGrob(Logo, x = unit(0.9, "npc"),y = unit(0.8, "npc"), width = unit(0.2, "npc"))


Mapa=ggplot()+
  geom_sf(data = Loreto, fill=NA, color="white", size=1)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        
        legend.key.size = unit(0.5, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.9,"cm"),
        legend.position = c(0.25,0.10),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=10, family="serif"),
        legend.title = element_text(size=10, family="mono", face='bold',hjust=0.5))+
  labs(color="")+
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  ggplot2::annotate(geom = "text", x = -73, y =-7, hjust = 0, vjust = 1, 
                    label = "Bosque del Perú 2000 - 2021",size = 7, family="serif", color = 
                      "white",  face = "bold")+
                      ggplot2::annotate(geom = "text", x = -73, y =-7.3, hjust = 0, vjust = 1, 
                                        label = "Data: High-resolution global maps of 21st-century \nforest cover change : \nhttps://doi.org/10.1126/science.1244693",size = 5, family="serif", color = 
                                          "white",  face = "bold")+
                                          ggplot2::annotate(geom = "text", x = -73, y =-8, hjust = 0, vjust = 1, 
                                                            label = "@Ing. gflorezc",size = 5, family="serif", color = 
                                                              "white",  face = "bold")+
                                                              annotation_custom(Logo_png)


ggsave(plot=Mapa,"Mapa de deforestacion.png",units = "cm",width = 29, #alto
       height = 29, #ancho
       dpi=1200)





















