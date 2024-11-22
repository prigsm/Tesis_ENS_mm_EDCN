# Selecciono variables dentales ENS 2003:

ENS2003 <- ENS_2003%>%
  mutate(FOLIO = folio,
         Region = region,
         Comuna = comuna,
         Zona = zona,
         Sexo = sexo,
         Edad = edad,
         Estrato = region,
         Conglomerado = comuna,
         Fac_Exp = fact_aex,
         NEDU = VAR00035,
         visita_dentista = p117,
         usa_protesis = d_15,
         creatinina = creatin)

ENS2017<- ENS_2017%>%
  mutate(FOLIO = IdEncuesta,
         Fac_Exp = Fexp_F1F2EX3p_Corr,
         NEDU = NEDU1_MINSAL_1,
         visita_dentista = sb2,
         usa_protesis = m5p1,
         drem_ms = m5p3,
         drem_mi = m5p6,
         dcar_ms = m5p5,
         dcar_mi = m5p8,
         creatinina = Creatinina_en_Sangre)

ENS2003_ <- ENS2003%>%
  select(
    FOLIO,
    Region, 
    Comuna, 
    Zona, 
    Sexo, 
    Edad,
    NEDU,
    Estrato, 
    Conglomerado, 
    Fac_Exp,
    v1_f1,
    v2_f2,
    lab_ex,
    fact_af1,
    fact_af2,
    visita_dentista,
    usa_protesis,
    drem_ms,
    drem_mi,
    dcar_ms,
    dcar_mi,
    creatinina) 

ENS2017_ <- ENS2017%>%
  select(
  FOLIO,
  Region, 
  Comuna, 
  Zona, 
  Sexo, 
  Edad,
  NEDU,
  Estrato, 
  Conglomerado, 
  Fac_Exp,
  Fexp_F1p_Corr,
  Fexp_F2p_Corr,
  Fexp_F1F2p_Corr,
  Fexp_EX1p_Corr,
  Fexp_F1F2EX1p_Corr,
  Fexp_EX2p_Corr,
  Fexp_F1F2EX2p_Corr,
  Fexp_EX3p_Corr,
  Fexp_F1F2EX3p_Corr,
  visita_dentista,
  usa_protesis,
  drem_ms,
  drem_mi,
  dcar_ms,
  dcar_mi,
  creatinina) 

class(ENS2017_$Region)
class(ENS2003_$Region)

ENS2017_$FOLIO <- as.character(ENS2017_$FOLIO)
ENS2017_$Comuna <- as.character(ENS2017_$Comuna)

ENS2017_mmc <-left_join(ENS2017_, MMC_sinmmxc6, by = "FOLIO")
ENS2003_mmc <-left_join(ENS2003_, MMC_sinmmxc6, by = "FOLIO")
ENS2003_2017_mmc <-full_join(ENS2003_mmc, ENS2017_mmc, by = "FOLIO")


a<-ENS2003_mmc%>%
  subset(Edad >=17)

a<-a%>%
  subset(!is.na(Fexp))

a<-a%>%
  subset(!is.na(creatinina))

a<-a%>%
  subset(!is.na(dcar_ms) & !is.na(dcar_mi)&
           !is.na(drem_ms) & !is.na(drem_mi)&
           !is.na(visita_dentista) & !is.na(usa_protesis))

a<-a %>%
  subset(!is.na(NEDU) & !is.na(Sexo)&
           !is.na(Edad) & !is.na(Estrato)&
           !is.na(Conglomerado) & !is.na(MMC_Score_0M))

b<-ENS2017_mmc%>%
  subset(Edad >=17)

b<-b%>%
  subset(!is.na(Fexp_F1F2p_Corr))

b<-b%>%
  subset(!is.na(creatinina))

# 
# # df2 <- subset(df1,!is.na(ENS_n) & !is.na(FOLIO) & !is.na(person) &
#                 !is.na(Sexo_n) & !is.na(Nivel_educacional) &
#                 !is.na(Zona_n) & !is.na(Grupo_etario) &
#                 !is.na(Poblacion_n) & !is.na(dcar_total) &
#                 !is.na(drem_total) & !is.na(dcar_total) &

rio::export(ENS2003_2017_mmc,"ENS2003_2017_mmc.xlsx")


(V1,
ENS,
FOLIO,
Region, 
Comuna, 
Zona, 
Sexo, 
Edad,
NEDU,
Estrato, 
Conglomerado, 
Fac_Exp,
visita_dentista,
usa_protesis,
drem_ms,
drem_mi,
dcar_ms,
dcar_mi,
creatinina,
fum_actual_,
SME2_,
asma,
Depresion_1_,
DIABETES_P1_,
ACV,
liver_disease,
epoc,
Ckd,
ackd,
infarto,
pfeffer_6,
glaucoma,
HTA2003_10_17_ATC,
thyroid_disorders,
neuro_pain,
Obeso_,
coag,
mmcx1,
mmcx2,
mmcx3,
mmcx4,
mmcx5,
mmcx7,
mmcx8,
mmcx9,
mmcx10,
mmcx11,
mmcx12,
mmcx14,
mmcx15,
mmcx16,
mmcx17,
mmcx19,
mmcx20,
mmcx21,
MMC_Score_0M,
MMC_cat_gob,
score,
categ,
categ_n)