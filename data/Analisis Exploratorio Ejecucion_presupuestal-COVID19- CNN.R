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

setwd("C:/Users/Christian/Desktop/Exploracion-de-Ejecucion-Presupuestal-COVID-19-master/data")

Ejecucion_presupuestal <- readxl::read_xlsx("pcm_covid.xlsx")
glimpse(Ejecucion_presupuestal)

##EXPLORANDO CONCEPTOS CLAVES
unique(Ejecucion_presupuestal$MES_EJE) #CAMBIAR A NOMBRE DEL MES
unique(Ejecucion_presupuestal$TIPO_GOBIERNO_NOMBRE) #OK
unique(Ejecucion_presupuestal$TIPO_GOBIERNO)
unique(Ejecucion_presupuestal$PLIEGO_NOMBRE) 
unique(Ejecucion_presupuestal$SECTOR_NOMBRE) #OK
unique(Ejecucion_presupuestal$EJECUTORA_NOMBRE)
unique(Ejecucion_presupuestal$DISTRITO_EJECUTORA_NOMBRE)
unique(Ejecucion_presupuestal$DEPARTAMENTO_EJECUTORA_NOMBRE)
unique(Ejecucion_presupuestal$MONTO_PIM)
unique(Ejecucion_presupuestal$MONTO_DEVENGADO)
unique(Ejecucion_presupuestal$MONTO_GIRADO)
unique(Ejecucion_presupuestal$FUNCION_NOMBRE)

##VALIDANDO TIPO DE DATA
sapply(Ejecucion_presupuestal,class)


##Evaluacion de la CANTIDAD de solicitudes de gasto por DEPARTAMENTO
Ejecucion_presupuestal %>% 
  group_by(`DEPARTAMENTO_EJECUTORA_NOMBRE`) %>% 
  summarise(Cantidad_de_solicitudes_ingresadas=n())%>% 
  arrange(desc(Cantidad_de_solicitudes_ingresadas))%>% 
  View() 

##Evaluacion del MONTO GIRADO promedio segun el DEPARTAMENTO solicitante
Ejecucion_presupuestal %>% 
  group_by(`DEPARTAMENTO_EJECUTORA_NOMBRE`) %>% 
  summarise(promedio_gasto=mean(`MONTO_GIRADO`))%>%  
  arrange(desc(promedio_gasto))%>% 
  View() 

#------
  

##Evaluacion de la CANTIDAD de solicitudes de gasto por TIPO DE GOBIERNO
Ejecucion_presupuestal %>% 
  group_by(`TIPO_GOBIERNO_NOMBRE`) %>% 
  summarise(Cantidad_de_solicitudes_ingresadas=n())%>% 
  arrange(desc(Cantidad_de_solicitudes_ingresadas))%>% 
  View() # mira la data

##Evaluacion del MONTO GIRADO promedio segun el TIPO DE GOBIERNO solicitante
Ejecucion_presupuestal %>% 
  group_by(`TIPO_GOBIERNO_NOMBRE`) %>%
  summarise(promedio_gasto=mean(`MONTO_GIRADO`))%>%
  arrange(desc(promedio_gasto))%>% 
  View() 
  
#------

##Evaluacion de la cantidad de unidad ejecutoras por departamentos, que solicitaron acceso al presupuesto Covid
Ejecucion_presupuestal %>%
  group_by(`DEPARTAMENTO_EJECUTORA_NOMBRE`,`EJECUTORA_NOMBRE`) %>% 
  summarise(numero=n())%>% 
  arrange(desc(numero))%>% 
  View() 

#------

###¿En que mes se solicito ejecutar mas presupuesto covid?
  table(Ejecucion_presupuestal$MES_EJE) 
  Ejecucion_presupuestal_2<-Ejecucion_presupuestal %>%
    mutate(MES_EJE=replace(MES_EJE, MES_EJE=="3", "MARZO")) %>%
    mutate(MES_EJE=replace(MES_EJE, MES_EJE=="2", "MARZO")) %>%
    mutate(MES_EJE=replace(MES_EJE, MES_EJE=="0", "MARZO")) %>%
    mutate(MES_EJE=replace(MES_EJE, MES_EJE=="4", "ABRIL")) %>%
    mutate(MES_EJE=replace(MES_EJE, MES_EJE=="5", "MAYO")) %>%
    mutate(MES_EJE=replace(MES_EJE, MES_EJE=="6", "JUNIO")) %>%
    mutate(MES_EJE=replace(MES_EJE, MES_EJE=="7", "JULIO")) %>%
    mutate(MES_EJE=replace(MES_EJE, MES_EJE=="8", "AGOSTO")) %>%
    as.data.frame()

    Ejecucion_presupuestal_2 %>% 
    group_by(`MES_EJE`) %>% 
    summarise(Cantidad_de_solicitudes_ingresadas=n())%>% 
    arrange(desc(Cantidad_de_solicitudes_ingresadas))%>% 
    View() 
    
###