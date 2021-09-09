# ########################################################### #
# ########  MONITOREO DE LOS AVANCES DE LA BD IGA's  ######## #
# #########################  BY LF  ######################### #
# ########################################################### #


### ######################## ###
###     I) SETEO GENERAL     ####
### ######################## ###

# BORRAR TODO LO ANTERIOR
rm(list=ls())

# INFO DEL USUARIO
USER=Sys.info()
USER=USER[7]

# SETEO DEL WD
setwd(paste('C:/Users/',USER,'/Google Drive/4) R/2) BD/1) OEFA/2) IGAs',sep = ""))

# CARGANDO ARCHIVO BASE
source(paste('C:/Users/',USER,'/Google Drive/4) R/1) Scripts/0) SCRIPT BASE/BASE_LF.R',sep = ""))

# CARGAR LA LIBRER?A PARA ORACLE
# library(RJDBC)

# OTRAS LIB
library(profvis)


################################



### ############################################ ###
###     II) BD_AFA: TRABAJAR DATA Y GRAFICAR     ####
### ############################################ ###


# --------- -
# DATA AFA
# --------- -

# ESPECIFICAR RUTAS
LINK_1 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vS6XJXlmvr-OmD1L6C-5E36LvjLge5hLVrEDeP_0jYHVcp6ma0SPXcjuBU6ymPT6ov6U5-6tujj2cSm/pub?output=xlsx"



# GENERANDO UN TEMPORAL
tp1<-tempfile()

# DESCARGAR
download.file(LINK_1,tp1,mode ="wb")

# SELECCIONAR LA PESTA?A DEL TEMPORAL
BD_AFA<-read_xlsx(path = tp1, sheet = "Hoja 1")

# TABLA GENERAL
TABLA_AFA = BD_AFA %>%
  subset(select = c(NombreFinalGrupo,
                    entidadCapacitada,
                    Entrada,
                    Salida)) %>% 
  mutate(entidadCapacitada = if_else(entidadCapacitada=="Entidad Capacitada", 1,0, missing = 0)) %>% 
  group_by(NombreFinalGrupo) %>% 
  summarise(Matriculados = n(),
            Capacitados = sum(entidadCapacitada),
            Entrada = as.numeric(round(mean(Entrada, na.rm = TRUE),1)),
            Salida = round(mean(Salida, na.rm = TRUE),1)) %>% 
  mutate(Mejoria = round(100*(Salida/Entrada-1),1)) %>% 
  arrange(desc(Matriculados))

  
  
# TABLA RESUMEN
TABLA_AFA_RESUM = TABLA_AFA %>%
  summarise(Matriculados = sum(Matriculados),
            Capacitados = sum(Capacitados),
            Entrada = round(mean(Entrada, na.rm = TRUE),1),
            Salida = round(mean(Salida, na.rm = TRUE),1))


# TABLA PIE
TABLA_AFA_PIE = BD_AFA %>%
  subset(select = c(sexo,
                    codMatricula)) %>%
  group_by(sexo) %>%
  summarise(Matriculados = n_distinct(codMatricula)) %>% 
  mutate(Porcentaje = Matriculados/sum(Matriculados)) %>% 
  arrange(desc(Matriculados))


# TABLA BARRAS
TABLA_AFA_BARRAS = BD_AFA %>%
  subset(select = c(filtroArea,
                    entidadCapacitada)) %>%
  mutate(entidadCapacitada = if_else(entidadCapacitada=="Entidad Capacitada", 1,0, missing = 0),
         filtroArea = ifelse(filtroArea == "Otras áreas del OEFA", "Otras áreas", filtroArea)) %>%
  group_by(filtroArea) %>%
  summarise(entidadCapacitada = sum(entidadCapacitada)) %>%
  mutate(Porcentaje = round(100*entidadCapacitada/sum(entidadCapacitada),1)) %>% 
  arrange(desc(entidadCapacitada))




# ------------- -
# GRAFICOS AFA
# ------------- -

# Generar los resumenes
TABLA_AFA_RESUM = TABLA_AFA_RESUM %>%
  rename("Total de matriculados" = "Matriculados",
         "Total de capacitados" = "Capacitados",
         "Evaluación de entrada" = "Entrada",
         "Evaluación de salida" = "Salida") %>%


  formattable(align=c("c","c","c","c"),
              list(`Indicator Name` = formatter("span",
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   "Total de matriculados" = color_text("white", PALETA.PRINCIPAL[1]),
                   "Total de capacitados" = color_text("white", PALETA.PRINCIPAL[2]),
                   "Evaluación de entrada" = color_text("white", PALETA.PRINCIPAL[3]),
                   "Evaluación de salida" = color_text("white", PALETA.PRINCIPAL[4])
                   )
              )

TABLA_AFA_RESUM




# Generar tabla general
TABLA_AFA = TABLA_AFA %>% 
  mutate(Entrada = ifelse(is.nan(Entrada) == T, NA, Entrada),
         Salida = ifelse(is.nan(Salida) == T, NA, Salida),
         Mejoria = ifelse(is.nan(Mejoria) == T, NA, Mejoria),
         Mejoria = ifelse(is.infinite(Mejoria) == T, NA, Mejoria)) %>% 
  rename("Evaluación de entrada" = "Entrada",
         "Evaluación de salida" = "Salida",
         "Mejoría (%)" = "Mejoria") %>% 
  
  formattable(align=c("l","l","l","l","l","c"),
              list(`Indicator Name` = formatter("span", 
                                                 style = ~ style(color = "grey",
                                                                 font.weight = "bold")),
                   Matriculados = color_bar(PALETA.PRINCIPAL[4]),
                   Capacitados = color_bar(PALETA.PRINCIPAL[3]),
                   `Evaluación de entrada` = color_bar(na.rm = TRUE, PALETA.PRINCIPAL[5]),
                   `Evaluación de salida` = color_bar(na.rm = TRUE, PALETA.SECUNDARIA[2]),
                   `Mejoría (%)` = color_bar(na.rm = TRUE, PALETA.SECUNDARIA[4])
                   )
              )
TABLA_AFA




# Grafico donut
RATIO=0.6 #El % que representa espacio interno de la dona respecto al total 
TAMAÑO=10 #Tamaño de la dona


GRAF_AFA_PIE = TABLA_AFA_PIE %>% 
  
  ggplot(aes(x = TAMAÑO*(1+RATIO)/2, y = Matriculados, fill = reorder(sexo,Matriculados)))+
  geom_col(width = (1-RATIO)*TAMAÑO,color="white",lwd=2)+ #,
  coord_polar("y", start = 0) +
  xlim(0, TAMAÑO)+
  geom_shadowtext(aes(x=TAMAÑO,label = paste0(round(Porcentaje*100), "%")),
                  color="white", fontface="bold", size=5,
                  position = position_stack(vjust = 0.5), check_overlap = T)+
  labs(x="", y="",
       # title = "Resumen de las medidas por imponer, por estado",
       # subtitle = paste0("(Del ",format(Inicio, format = "%d/%m/%Y")," al ",format(Fin, format = "%d/%m/%Y"),")"),
       caption = "")+
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(face="bold",hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        plot.caption = element_text(hjust = 0,size=10)) +
  scale_fill_manual(values = PALETA.PRINCIPAL)+
  ggeasy::easy_add_legend_title("")+
  ggeasy::easy_legend_at("bottom")+
  ggeasy::easy_plot_legend_size(size=10)

GRAF_AFA_PIE




# Gráfico barras
GRAF_BARRAS = TABLA_AFA_BARRAS %>% 
  ggplot(aes(x = reorder(filtroArea,desc(entidadCapacitada)), y = entidadCapacitada)) + 
  geom_bar(stat = "identity", fill = PALETA.PRINCIPAL[1]) +
  geom_shadowtext(aes(x=filtroArea,label = entidadCapacitada),
                  color="white", fontface="bold", size=5,
                  nudge_y = 10, check_overlap = T)+
  theme_minimal()+
  labs(x="Áreas", y="Cantidad de capacitados",
       title = "", 
       caption = "")+
  theme(legend.position = "bottom",
                panel.grid.minor = element_blank(),
                axis.line = element_line(colour = "black",color="black", arrow =arrow(angle = 30,length = unit(.1,"inches"),type = "closed")))+
  scale_y_continuous(expand = expand_scale(mult = c(0, .15)))
  
  
GRAF_BARRAS




####################################################
  


### ############################################# ###
###     III) BD_AFA: TRABAJAR DATA Y GRAFICAR     ####
### ############################################# ###



# --------- -
# DATA EFA
# --------- -

# ESPECIFICAR RUTAS
LINK_2 = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQhPmIpp-appFXARozd_OkzITIEvtLkxFnVrqBkkF8esYy6_UZSGzZhcX9qlH4A8muR2dQsuBTqr9X5/pub?output=xlsx"



# GENERANDO UN TEMPORAL
tp1<-tempfile()

# DESCARGAR
download.file(LINK_2,tp1,mode ="wb")

# SELECCIONAR LA PESTA?A DEL TEMPORAL
BD_EFA<-read_xlsx(path = tp1, sheet = "Hoja 3")


# TABLA GENERAL
TABLA_EFA = BD_EFA %>%
  subset(select = c(entidadConRegion,
                    Entrada,
                    Salida)) %>% 
  group_by(entidadConRegion) %>% 
  summarise(Matriculados = n(),
            Entrada = as.numeric(round(mean(Entrada, na.rm = TRUE),1)),
            Salida = round(mean(Salida, na.rm = TRUE),1)) %>% 
  mutate(Mejoria = round(100*(Salida/Entrada-1),1)) %>% 
  arrange(desc(Matriculados))


# TABLA RESUMEN
TABLA_EFA_RESUM = TABLA_EFA %>%
  summarise(Matriculados = sum(Matriculados),
            Entrada = round(mean(Entrada, na.rm = TRUE),1),
            Salida = round(mean(Salida, na.rm = TRUE),1))



# TABLA PIE
TABLA_EFA_PIE = BD_EFA %>%
  subset(select = c(sexo)) %>%
  group_by(sexo) %>%
  summarise(Matriculados = n()) %>% 
  mutate(Porcentaje = round(100*Matriculados/sum(Matriculados),1)) %>% 
  arrange(desc(Matriculados))



# TABLA MAPA
TABLA_EFA_MAPA = BD_EFA %>% 
  subset(select = dptoEntidadMatricula) %>% 
  group_by(dptoEntidadMatricula) %>% 
  summarise(Cantidad = n())




# ------------- -
# GRAFICOS EFA
# ------------- -

# Generar los resumenes
TABLA_EFA_RESUM = TABLA_EFA_RESUM %>%
  
  formattable(align=c("c","c","c"),
              list(`Indicator Name` = formatter("span",
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   "Matriculados" = color_text("white", PALETA.PRINCIPAL[1]),
                   "Entrada" = color_text("white", PALETA.PRINCIPAL[3]),
                   "Salida" = color_text("white", PALETA.PRINCIPAL[4])
              )
  )

TABLA_EFA_RESUM




# Generar tabla general
TABLA_EFA = TABLA_EFA %>% 
  mutate(Entrada = ifelse(is.nan(Entrada) == T, NA, Entrada),
         Salida = ifelse(is.nan(Salida) == T, NA, Salida),
         Mejoria = ifelse(is.nan(Mejoria) == T, NA, Mejoria),
         Mejoria = ifelse(is.infinite(Mejoria) == T, NA, Mejoria)) %>% 
  rename("Evaluación de entrada" = "Entrada",
         "Evaluación de salida" = "Salida",
         "Mejoría (%)" = "Mejoria") %>% 
  
  formattable(align=c("l","l","l","l","c"),
              list(`Indicator Name` = formatter("span", 
                                                style = ~ style(color = "grey",
                                                                font.weight = "bold")),
                   Matriculados = color_bar(PALETA.PRINCIPAL[4]),
                   `Evaluación de entrada` = color_bar(na.rm = TRUE, PALETA.PRINCIPAL[5]),
                   `Evaluación de salida` = color_bar(na.rm = TRUE, PALETA.SECUNDARIA[2]),
                   `Mejoría (%)` = color_bar(na.rm = TRUE, PALETA.SECUNDARIA[4])
              )
  )
TABLA_EFA




# Grafico donut
RATIO=0.6 #El % que representa espacio interno de la dona respecto al total 
TAMAÑO=10 #Tamaño de la dona


GRAF_EFA_PIE = TABLA_EFA_PIE %>% 
  
  ggplot(aes(x = TAMAÑO*(1+RATIO)/2, y = Matriculados, fill = reorder(sexo,Matriculados)))+
  geom_col(width = (1-RATIO)*TAMAÑO,color="white",lwd=2)+ #,
  coord_polar("y", start = 0) +
  xlim(0, TAMAÑO)+
  geom_shadowtext(aes(x=TAMAÑO,label = paste0(Porcentaje, "%")),
                  color="white", fontface="bold", size=5,
                  position = position_stack(vjust = 0.5), check_overlap = T)+
  labs(x="", y="",
       # title = "Resumen de las medidas por imponer, por estado",
       # subtitle = paste0("(Del ",format(Inicio, format = "%d/%m/%Y")," al ",format(Fin, format = "%d/%m/%Y"),")"),
       caption = "")+
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(face="bold",hjust = 0.5, size = 12),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        plot.caption = element_text(hjust = 0,size=10)) +
  scale_fill_manual(values = PALETA.PRINCIPAL)+
  ggeasy::easy_add_legend_title("")+
  ggeasy::easy_legend_at("bottom")+
  ggeasy::easy_plot_legend_size(size=10)

GRAF_EFA_PIE




# Gráfico mapa
N_ARCHIVO = 'BAS_LIM_DEPARTAMENTO.shp'
DIR_ARCHIVO = paste('C:/Users/',USER,'/Google Drive/4) R/4) Shape files/1) Departamentos-Regiones/',N_ARCHIVO,sep = "")


GRAF_MAPA = read_sf(DIR_ARCHIVO) %>% 
  mutate(REGIONES = case_when(NOMBDEP == "LIMA" ~ "LIMA Y CALLAO" ,
                              NOMBDEP == "CALLAO" ~ "LIMA Y CALLAO" ,
                              TRUE  ~ NOMBDEP)) %>% 
  merge(TABLA_EFA_MAPA,
        by.x = c("NOMBDEP"),
        by.y = c("dptoEntidadMatricula"),
        all.x = T) %>% 
  
  ggplot() +
  geom_sf(data = BD_DEPARTAMENTO,
          aes(fill = Cantidad),
          color = "white", size = 0.25) +
  theme_void()+
  coord_sf(crs = "+proj=robin")+
  ggeasy::easy_move_legend(to="right")+
  ggeasy::easy_add_legend_title("Matriculados")+
  scale_fill_continuous(high = PALETA.PRINCIPAL[1], low = PALETA.PRINCIPAL[3]) 


GRAF_MAPA


#####################################################  





