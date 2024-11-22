################################################################################
##################  Repito análisis de tesis MM & EDCN  ########################
################################################################################

#CARGO LIBRERÍAS : ----
library(tidyverse)
library(readr)
library(readxl)
library(haven)
library(knitr)

library(rio) #To import and export data#
library(reshape2) #Modifiy data set shape design with grammar
library(dplyr) #dplyr: a grammar of data manipulation#
library(ggplot2) #plot your results
library(survey) #Analise using survey design
library(srvyr) #Analise using survey design with grammargetwd()
library(tidyverse) # plot your results
library(gtsummary)

library(descr)
library(psych)
library(PMCMRplus)
library(tinytex)
library(summarytools)

library(janitor)
library(purrr)

#______________________________________________________________________________#
# IMPORTO bases de datos:
# Variable MM compartida por Álvaro (2022):
ENS2003_2010_2017_MMC <- read_csv("ENS2003_2010_2017_MMC.csv")

# Base de datos ENS 2003 y 2016-17:
ENS_2003 <- read_sav("ENS_2003_Ministerio_de_Salud_Chile_Dpto_Epidemiolog¡a_VENT_region_comuna.sav")
ENS_2017 <- read_sav("Base de datos Encuesta Nacional de Salud 2016-2017(ENS).Formulario 1_2_EX.MINSAL_EPI. (2)_CIDI_SF_Comuna_metales pesados.sav")
#______________________________________________________________________________#
# Objetivos de este script:
# 1.- Eliminar NEDU_low de MMC.
# 2.- Seleccionar variables dentales de cada ENS.
# 3.- Juntar bases de datos y seleccionar variables de interés.
#______________________________________________________________________________#
nombres_ens2017 <- as.data.frame(names(ENS_2017))
nombres_ens2003 <- as.data.frame(names(ENS_2003))
nombres_MMC <- as.data.frame(names(ENS2003_2010_2017_MMC))


# 1.- Eliminar NEDU_low de MMC.# library(DT) ----

df0_total<- rio::import("ENS2003_2010_2017_MMC.csv")
View(df0_total)
names_df0_total<-names(df0_total)
names_df0_total<-as.data.frame(names_df0_total)

#[4] "MMC_Score_0M"      "MMC_cat_gob"       "MMC_Score"        
#[7] "MMC_cat_gob_M" 

SINmmcx6<- select(df0_total, 
                  "V1" , "ENS","FOLIO",           
                  "fum_actual_","SME2_", "asma", "Depresion_1_",
                  "DIABETES_P1_","ACV","liver_disease","epoc","Ckd","ackd" ,            
                  "infarto","pfeffer_6","glaucoma","HTA2003_10_17_ATC",
                  "thyroid_disorders" ,"neuro_pain","Obeso_","coag",
                  "mmcx1","mmcx2" ,"mmcx3","mmcx4","mmcx5",
                  "mmcx7","mmcx8" ,"mmcx9" ,"mmcx10", "mmcx11",
                  "mmcx12","mmcx14","mmcx15" ,"mmcx16", "mmcx17",
                  "mmcx19","mmcx20","mmcx21","MMC_Score_0M","MMC_cat_gob" )

### Creación de score categorizaicón según definición MINSAL:
# "conteo simple ponderado de aptologáis o condiciones cróncias."
#las siguientes 5 patologías tienen doble puntaje ponderado por: 
# Impacto en la calidad de vida, 
# Complejidad de atención, 
# Frecuencia de controles
# Diabetes MellitusAR / glucemia ayunas ≥126mg/dL
# Enf. Cerebrovascular /accidente –infarto cerebrovascular(AVE)AR
#Enf. Renal crónica avanzada (eGFR <30) 
#Enf. Caridovascular / IAM/ cardiopatía isquémicaAR
#Funcionalidad limitada/discapacidad/dependenciaPfeffer>6 en pobl. >60 años
# "DIABETES_P1_","ACV","ackd", "infarto","pfeffer_6"
# "mmcx5","mmcx7", "mmcx11","mmcx12","mmcx14"

### ELIMINO NEDU LOW : mmcx6, se *2 patologías con doble puntaje ponderado
#     "MMC_Score_0M"      "MMC_cat_gob"
attach(SINmmcx6)
SINmmcx6$score <- NA
SINmmcx6$score <- ( mmcx1 + mmcx2 + mmcx3 + mmcx4 + mmcx5 +
                      mmcx7 + mmcx8 +  mmcx9 + mmcx10 +
                      mmcx11 + mmcx12 + mmcx14 + mmcx15 +
                      mmcx16 + mmcx17 + mmcx19 + mmcx20 + mmcx21)
SINmmcx6$categ <- NA

SINmmcx6$categ [(SINmmcx6$score == 0)] <- 1
SINmmcx6$categ [(SINmmcx6$score == 1)] <- 2
SINmmcx6$categ [(SINmmcx6$score >=2 & SINmmcx6$score <5 )] <- 3
SINmmcx6$categ [(SINmmcx6$score >=5)] <- 4

SINmmcx6$categ_n<-factor(SINmmcx6$categ,
                         levels=c("1","2",
                                  "3","4" ),
                         labels=c("0","1",
                                  "2-4","5+"))
#"MMC_Score_0M","MMC_cat_gob"   
MMC_sinmmxc6<- select(SINmmcx6, 
                      "V1" , "ENS","FOLIO",           
                      "fum_actual_","SME2_", "asma", "Depresion_1_",
                      "DIABETES_P1_","ACV","liver_disease", "epoc","Ckd","ackd" ,            
                      "infarto","pfeffer_6","glaucoma","HTA2003_10_17_ATC",
                      "thyroid_disorders" ,"neuro_pain","Obeso_","coag",
                      "mmcx1","mmcx2" ,"mmcx3","mmcx4","mmcx5",
                      "mmcx7","mmcx8" ,"mmcx9" ,"mmcx10", "mmcx11",
                      "mmcx12","mmcx14","mmcx15" ,"mmcx16", "mmcx17",
                      "mmcx19","mmcx20","mmcx21", "score", "categ", "categ_n")

MMC_sinmmxc6$MMC_Score_0M <-  MMC_sinmmxc6$score
MMC_sinmmxc6$MMC_cat_gob <-  MMC_sinmmxc6$categ_n

ENS2003_2010_2017_MMC_sinmmxc6<- select(MMC_sinmmxc6, 
                                        "V1" , "ENS","FOLIO",           
                                        "fum_actual_","SME2_", "asma", "Depresion_1_",
                                        "DIABETES_P1_","ACV", "liver_disease", "epoc","Ckd","ackd" ,            
                                        "infarto","pfeffer_6","glaucoma","HTA2003_10_17_ATC",
                                        "thyroid_disorders" ,"neuro_pain","Obeso_","coag",
                                        "mmcx1","mmcx2" ,"mmcx3","mmcx4","mmcx5",
                                        "mmcx7","mmcx8" ,"mmcx9" ,"mmcx10", "mmcx11",
                                        "mmcx12","mmcx14","mmcx15" ,"mmcx16", "mmcx17",
                                        "mmcx19","mmcx20","mmcx21", "MMC_Score_0M", "MMC_cat_gob")

#______________________________________________________________________________#
# 2.- Seleccionar variables dentales de cada ENS.





#______________________________________________________________________________#
# 3.- Juntar bases de datos y seleccionar variables de interés.














#______________________________________________________________________________#
# EXPORTO base de datos: ----
# base MMC sin nedu_low:
write.csv(ENS2003_2010_2017_MMC_sinmmxc6,"ENS2003_2010_2017_MMC_sin_mmcx6.csv")
