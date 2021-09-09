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
library(shiny)
library(shinydashboard)

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


# TABLA MAPA
N_ARCHIVO = 'BAS_LIM_DEPARTAMENTO.shp'
DIR_ARCHIVO = paste('C:/Users/',USER,'/Google Drive/4) R/4) Shape files/1) Departamentos-Regiones/',N_ARCHIVO,sep = "")


BD_MAPA = read_sf(DIR_ARCHIVO) %>%
  mutate(REGIONES = case_when(NOMBDEP == "LIMA" ~ "LIMA Y CALLAO" ,
                              NOMBDEP == "CALLAO" ~ "LIMA Y CALLAO" ,
                              TRUE  ~ NOMBDEP))

#####################################################




