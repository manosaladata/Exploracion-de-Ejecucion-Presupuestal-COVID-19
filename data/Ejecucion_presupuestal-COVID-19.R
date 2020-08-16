#-------------------------------------------------------------------
# Para limpiar el workspace, por si hubiera algun dataset 
# o informacion cargada
rm(list = ls())

#-------------------------------------------------------------------
# Para limpiar el área de gráficos
dev.off()

#-------------------------------------------------------------------
# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#------------------------------------------------------------------
# Limpiar la consola
cat("\014")


library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(readr)
library(dplyr)
library(lubridate)# para manejar fechas
library(stringr)
library(ggalluvial) #grafico del analisis exploratoria very interesting
library(parcats)
library(easyalluvial)
library(forcats)
library(tint)
library(extrafont)
library(ggthemes)

setwd("E:/R clases/Taller manos a la data/github/data/Exploracion-de-Ejecucion-Presupuestal-COVID-19/data")

Ejecucion_presupuestal <- readxl::read_xlsx("pcm_covid.xlsx")
glimpse(Ejecucion_presupuestal)


unique(Ejecucion_presupuestal$SECTOR_NOMBRE)
unique(Ejecucion_presupuestal$PLIEGO_NOMBRE)

Ejecucion_presupuestal %>% 
  filter(DEPARTAMENTO_EJECUTORA_NOMBRE) 
  
  
  
  empresas_reactiva_peru %>%# la BD!
  filter(`TIPO DE ENTIDAD OTORGANTE DEL CRÉDITO` %in% c("CMAC") ) %>% # c() es una función para crear vectores.  
  group_by(`NOMBRE ENTIDAD OTORGANTE DEL CRÉDITO`) %>% # agrupala segun...
  summarise(promedio_prestamos=sum(`MONTO PRÉSTAMO`),numero=n())%>%# crea una nueva variable
  arrange(desc(numero))%>% # reordenala de mayor a menor
  View() # mira la data


#### 2 ¿Cuál es el monto promedio de los préstamos otorgados por Reactiva Perú según departamento? ordenar por monto promedio

empresas_reactiva_peru %>%# la BD!
  group_by(DEPARTAMENTO) %>% # agrupala segun...
  summarise(monto_promedio_prestamos=mean(`MONTO PRÉSTAMO`))%>%# crea una nueva variable
  arrange(desc(monto_promedio_prestamos))%>% # reordenala de mayor a menor
  View() # mira la data
