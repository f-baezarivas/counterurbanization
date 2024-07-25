#####COUNTERURBANIZATION IN THE RURAL GLOBAL SOUTH: EVIDENCE FROM CHILE####

# WORKING DIRECTORY AND LIBRARIES -----------------------------------------

#Working directory
#setwd("C:/Users/Your Folder") #Change it at your location. Download script file and data folder in this folder

#Libraries
library(tidyverse)
library(readxl)
library(summarytools)
library(sf)
library(gtsummary)
library(gt)
library(ggrepel)
library(ggsflabel)
library(gridExtra)
library(ggspatial)
library(networkD3)

#A small function to change from lower to upper case the first letter and to lower case the rest of the word (useful for comunas)
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# 2. QUANTITATIVE DATA -----------------------------------------------------------------

# 2.1. Administrative division and municipalities classification-----------

###DPA codes
dpa_cod<-read_excel("Data/DPA code.xls")

###Rural "entities" 
ent17 <- read_delim("Data/Censo2017_Manzana.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)%>% 
  filter((REGION==9 | REGION==10 | REGION==14) &
           (AREA==2)) %>% 
  left_join(y=(read_delim("Data/microdato_Censo2017-Geografia_Manzanas.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
              filter((REGION==9 | REGION==10 | REGION==14) & (AREA==2)) %>%
              select(ID_MANZENT, NOM_LOCALIDAD, CAT_ENT, NOM_ENTIDAD)), 
            by="ID_MANZENT")%>% 
  select(ID_MANZENT, REGION, PROVINCIA, COMUNA, DC, ZC_LOC, NOM_LOCALIDAD, ID_ZONA_LOC, MZ_ENT, CAT_ENT, NOM_ENTIDAD, 
         PERSONAS, HOMBRES, MUJERES, EDAD_0A5, EDAD_6A14, EDAD_15A64, EDAD_65YMAS, INMIGRANTES, PUEBLO,
         TOTAL_VIV, VIV_PART, VIV_COL, VPOMP, CANT_HOG, 
         starts_with("P0"), 
         starts_with("MAT"))%>% 
  mutate(id=paste(COMUNA, DC, ZC_LOC, MZ_ENT,sep = "-"), 
         cat_ent_rec=case_when(CAT_ENT==3 ~ "Aldea", 
                               CAT_ENT==4 ~ "Caserío",
                               CAT_ENT==9 ~ "Parcela",
                               (CAT_ENT>=5 & CAT_ENT<9) | (CAT_ENT>=10 & CAT_ENT<15) ~ "Otros",
                               CAT_ENT==15 ~ "Indeterminada"),
         cat_ent_nom=case_when(CAT_ENT==3 ~ "Aldea", 
                               CAT_ENT==4 ~ "Caserío",
                               CAT_ENT==5 ~ "Asentamiento minero",
                               CAT_ENT==6 ~ "Asentamiento pesquero",
                               CAT_ENT==7 ~ "Fundo-Estancia-Hacienda",
                               CAT_ENT==8 ~ "Parcela-Hijuela",
                               CAT_ENT==9 ~ "Parcela de agrado",
                               CAT_ENT==10 ~ "Comunidad indígena",
                               CAT_ENT==12 ~ "Campamento",
                               CAT_ENT==13 ~ "Veranada-Majada-Aguada",
                               CAT_ENT==14 ~ "Otros",
                               CAT_ENT==15 ~ "Indeterminada"))%>% 
  mutate_at(vars(HOMBRES:PUEBLO), as.double)%>%
  mutate(NOM_LOCALIDAD=tolower(NOM_LOCALIDAD),
         NOM_LOCALIDAD=capwords(NOM_LOCALIDAD),
         NOM_ENTIDAD=tolower(NOM_ENTIDAD),
         NOM_ENTIDAD=capwords(NOM_ENTIDAD),
         geocode=substr(ID_MANZENT, 1, 11)) 

###Municipalities classification

#Municipalities typology
com17<-st_read(dsn = "Data/2017_Districts/Distrito_Densid_Superficie.shp", as_tibble = TRUE, quiet = FALSE, drivers = "ESRI Shapefile")%>%
  st_drop_geometry()%>%
  mutate(dens_rec=case_when(Densidad<=150 ~ "menos_150",
                            Densidad>150 ~ "mas_150"),
         REGION=as.double(REGION),
         COMUNA=as.double(COMUNA))%>% 
  select(reg=REGION, com=COMUNA, nom_com=NOM_COMUNA, dis=CODIGO_C17, pop=T_POB, dens=Densidad, dens_rec)%>%
  group_by(reg, com, nom_com, dens_rec)%>% 
  summarise(pop=sum(pop))%>%
  pivot_wider(names_from=dens_rec, values_from = pop)%>% 
  left_join(y=(st_read(dsn="Data/2017_Cities/Ciudades_2017.shp", as_tibble = TRUE, quiet = FALSE, drivers = "ESRI Shapefile")%>%
                 st_drop_geometry()%>%
                 mutate(reg=as.integer(REGION))%>%
                 select(pop_nucleo=TOT_PERSON, com=COMUNA, tipo=TIPO)%>%
                 mutate(com=as.integer(com))%>%
                 group_by(com)%>%
                 summarise(pop_nucleo=sum(pop_nucleo, na.rm=T))), 
            by="com")%>%
  mutate(pop_nucleo=replace_na(pop_nucleo,0),
         mas_150=replace_na(mas_150, 0),
         menos_150=replace_na(menos_150,0),
         pop=mas_150 + menos_150,
         pct_rur=100*menos_150/pop,
         urb_rec=case_when(pct_rur>=50 ~ "Rural",
                           pct_rur<25 ~ "Urban",
                           pct_rur>=25 & pct_rur<50 ~ "Mixed"),
         urb_rec=factor(case_when(urb_rec=="Rural" & pop_nucleo>=0 ~ "Rural",
                                  urb_rec=="Mixed" & pop_nucleo>=50000 ~ "Urban", #Less than half population in rural districts AND a urban agglomeration with over 50k: URBAN
                                  urb_rec=="Mixed" & pop_nucleo<50000 ~ "Mixed",  #Less than half population in rural districts NOT a urban agglomeration with over 50k: Mixed
                                  urb_rec=="Urban" & pop_nucleo<50000 ~ "Mixed",
                                  urb_rec=="Urban" & pop_nucleo>=50000 ~ "Urban"),
                        levels=c("Urban", "Mixed", "Rural")))%>%
  mutate(nom_com=tolower(nom_com),
         nom_com=capwords(nom_com))

# 2.2. Census: 1992 -------------------------------------------------------

#A full version (useful for Table 2 and some descriptive analysis in the text)
censo92_full<-readRDS("Data/Censo1992_Persona_Full.Rds")%>%
  mutate(reg=as.integer(region),
         reg_cat=factor(case_when(reg==9 | reg==10 | reg==14 ~ "La Araucanía, Los Ríos, Los Lagos",
                                  reg<9 | reg==11 | reg==12 | reg==13 | reg>14 ~ "Rest of Chile"),
                        levels=c("La Araucanía, Los Ríos, Los Lagos", "Rest of Chile")),
         com=as.integer(comuna),
         com_res=factor(case_when(comuna_habitual_origen3==com ~ "Same municipality of residence",
                                  comuna_habitual_origen3!=com & comuna_habitual_origen3>=1000 & comuna_habitual_origen3<99999 ~ "Other municipality of residence",
                                  comuna_habitual_origen3!=com & comuna_habitual_origen3<1000 ~ "Other country of residence",
                                  comuna_habitual_origen3==99999 ~ "Unknown place of residence"),
                        levels=c("Same municipality of residence", "Other municipality of residence", "Other country of residence", "Unknown place of residence")),
         sex=factor(case_when(sexo==1 ~ "Male",
                              sexo==2 ~ "Female"),
                    levels=c("Male", "Female")),
         edad_rec=factor(case_when(edad<15 ~ "0-14",
                                   edad>=15 & edad<30 ~ "15-29",
                                   edad>=30 & edad<45 ~ "30-44",
                                   edad>=45 & edad<60 ~ "45-59",
                                   edad>=60 ~ ">=60"),
                         levels=c("0-14", "15-29", "30-44", "45-59", ">=60")),
         esc=factor(case_when(tipo_educacion==0 ~ "Did not attend",
                              tipo_educacion==1 ~ "Less than full primary",
                              tipo_educacion==2 & curso<8 ~ "Less than full primary", 
                              tipo_educacion==2 & curso==8 ~ "Primary (completed)",
                              tipo_educacion==3 & curso<4 ~ "Incomplete secondary",
                              tipo_educacion==3 & curso>=4 ~ "Secondary (completed)",
                              tipo_educacion==4 & curso==1 ~ "Less than full primary",
                              tipo_educacion==4 & curso==2 ~ "Primary (completed)",
                              tipo_educacion==4 & curso>2 ~ "Incomplete secondary",
                              tipo_educacion==4 & curso>=6 ~ "Secondary (completed)",
                              tipo_educacion>=5 & tipo_educacion<=11 & curso<4 ~ "Incomplete secondary",
                              tipo_educacion>=5 & tipo_educacion<=11 & curso>=4 ~ "Secondary (completed)",
                              tipo_educacion>=12 ~ "Higher education", 
                              edad<25 ~ NA), 
                    levels=c("Did not attend", "Less than full primary", "Primary (completed)", "Incomplete secondary", "Secondary (completed)", "Higher education")),
         esc_rec=factor(case_when(esc=="Did not attend" | esc=="Less than full primary" | esc== "Primary (completed)" ~ "Primary (0-8 years)",
                                  esc=="Incomplete secondary" | esc== "Secondary (completed)" ~ "Secondary (9-12 years)",
                                  esc=="Higher education" ~ "Higher education"),
                        levels=c("Primary (0-8 years)", "Secondary (9-12 years)", "Higher education")),
         pue=factor(case_when(cultura>=1 & cultura<4 ~ "Indigenous",
                              cultura==4 ~ "Non-indigenous"),
                    levels=c("Non-indigenous", "Indigenous")),
         sfdt=factor(case_when(edad<15 ~ NA,
                               situacion_empleo<=3 ~ "Employed",
                               situacion_empleo==4 | situacion_empleo==5 ~ "Unemployed",
                               situacion_empleo>=6 ~ "Outside the labour force"),
                     levels=c("Employed", "Unemployed", "Outside the labour force")),
         rama=factor(case_when(rama_actividad>0 & rama_actividad<100 ~ "Agriculture",
                               rama_actividad>=100 & rama_actividad<450 ~ "Mining; Electricity, gas and water supply; Manufacturing",
                               rama_actividad>=450 & rama_actividad<500 ~ "Construction",
                               rama_actividad>=500 & rama_actividad<650 ~ "Trade; Transportation; Accommodation and food",
                               rama_actividad>=650 ~ "Business, administrative and non-market services"),
                     levels=c("Agriculture", "Mining; Electricity, gas and water supply; Manufacturing", "Construction", "Trade; Transportation; Accommodation and food",
                              "Business, administrative and non-market services")))%>%
  select(area, com, com_res, reg_cat, sex, edad, edad_rec, esc_rec, pue, sfdt, rama)

#The study area database
censo92<-readRDS("Data/Censo1992_Persona_Full.Rds")%>%
  filter((region==9 | region==10 | region==14) & area==2)%>%
  mutate(geocode=case_when(nchar(as.character(zc_loc))==1~paste(comuna, distrito, area, "0", zc_loc, sep=""), 
                           nchar(as.character(zc_loc))==2~paste(comuna, distrito, area, zc_loc, sep="")),
         reg=as.integer(region),
         pro=as.integer(provincia),
         com=as.integer(comuna),
         sex=factor(case_when(sexo==1 ~ "Male",
                              sexo==2 ~ "Female"),
                    levels=c("Male", "Female")),
         edad_rec=factor(case_when(edad<15 ~ "0-14",
                                   edad>=15 & edad<30 ~ "15-29",
                                   edad>=30 & edad<45 ~ "30-44",
                                   edad>=45 & edad<60 ~ "45-59",
                                   edad>=60 ~ ">=60"),
                         levels=c("0-14", "15-29", "30-44", "45-59", ">=60")),
         esc=factor(case_when(edad<25 ~ NA,
                              edad>=25 & tipo_educacion==0 ~ "Did not attend",
                              edad>=25 & tipo_educacion==1 ~ "Less than full primary",
                              edad>=25 & tipo_educacion==2 & curso<8 ~ "Less than full primary",
                              edad>=25 & tipo_educacion==2 & curso==8 ~ "Primary (completed)",
                              edad>=25 & tipo_educacion==3 & curso<4 ~ "Incomplete secondary", 
                              edad>=25 & tipo_educacion==3 & curso>=4 ~ "Secondary (completed)",
                              edad>=25 & tipo_educacion==4 & curso==1 ~ "Less than full primary",
                              edad>=25 & tipo_educacion==4 & curso==2 ~ "Primary (completed)",
                              edad>=25 & tipo_educacion==4 & curso>2 ~ "Incomplete secondary",
                              edad>=25 & tipo_educacion==4 & curso>=6 ~ "Secondary (completed)",
                              edad>=25 & tipo_educacion>=5 & tipo_educacion<=11 & curso<4 ~ "Incomplete secondary",
                              edad>=25 & tipo_educacion>=5 & tipo_educacion<=11 & curso>=4 ~ "Secondary (completed)",
                              edad>=25 & tipo_educacion>=12 ~ "Higher education"),
                    levels=c("Did not attend", "Less than full primary", "Primary (completed)", "Incomplete secondary", "Secondary (completed)", "Higher education")),
         esc_rec=factor(case_when(esc=="Did not attend" | esc=="Less than full primary" | esc== "Primary (completed)" ~ "Primary (0-8 years)",
                                  esc=="Incomplete secondary" | esc== "Secondary (completed)" ~ "Secondary (9-12 years)",
                                  esc=="Higher education" ~ "Higher education"),
                        levels=c("Primary (0-8 years)", "Secondary (9-12 years)", "Higher education")),
         pue=factor(case_when(cultura>=1 & cultura<4 ~ "Indigenous",
                              cultura==4 ~ "Non-indigenous"),
                    levels=c("Non-indigenous", "Indigenous")),
         sfdt=factor(case_when(edad<15 ~ NA,
                               situacion_empleo<=3 ~ "Employed",
                               situacion_empleo==4 | situacion_empleo==5 ~ "Unemployed",
                               situacion_empleo>=6 ~ "Outside the labour force"),
                     levels=c("Employed", "Unemployed", "Outside the labour force")),
         rama=factor(case_when(rama_actividad>0 & rama_actividad<100 ~ "Agriculture",
                               rama_actividad>=100 & rama_actividad<450 ~ "Mining; Electricity, gas and water supply; Manufacturing",
                               rama_actividad>=450 & rama_actividad<500 ~ "Construction",
                               rama_actividad>=500 & rama_actividad<650 ~ "Trade; Transportation; Accommodation and food",
                               rama_actividad>=650 ~ "Business, administrative and non-market services"),
                     levels=c("Agriculture", "Mining; Electricity, gas and water supply; Manufacturing", "Construction", "Trade; Transportation; Accommodation and food",
                              "Business, administrative and non-market services")),
         com_res=factor(case_when(comuna_habitual_origen3==com ~ "Same municipality of residence",
                                  com==9112 & comuna_habitual_origen3==9101 ~ "Same municipality of residence", #Excepction for change from Temuco to Padre las Casas
                                  com==9121 & comuna_habitual_origen3==9111 ~ "Same municipality of residence", #Excepction for change from Nueva Imperial to Cholchol
                                  comuna_habitual_origen3!=com & comuna_habitual_origen3>=1000 & comuna_habitual_origen3<99999 ~ "Other municipality of residence",
                                  comuna_habitual_origen3!=com & comuna_habitual_origen3<1000 ~ "Other country of residence",
                                  comuna_habitual_origen3==99999 ~ "Unknown place of residence"),
                        levels=c("Same municipality of residence", "Other municipality of residence", "Other country of residence", "Unknown place of residence")),
         com_res_cod=case_when(com_res=="Same municipality of residence" ~ com,
                               com_res=="Other municipality of residence" ~ comuna_habitual_origen3),
         com_ori=factor(case_when(edad<5 | comuna_1987_origen3==0 ~ "Under 5",
                                  comuna_1987_origen3==com ~ "Same municipality as 5 years ago",
                                  com==9112 & comuna_1987_origen3==9101 ~ "Same municipality as 5 years ago", #Excepction for change from Temuco to Padre las Casas
                                  com==9121 & comuna_1987_origen3==9111 ~ "Same municipality as 5 years ago", #Excepction for change from Nueva Imperial to Cholchol
                                  comuna_1987_origen3!=com & comuna_1987_origen3>=1000 & comuna_1987_origen3<99999 ~ "Other municipality as 5 years ago",
                                  comuna_1987_origen3!=com & comuna_1987_origen3<1000 ~ "Other country as 5 years ago",
                                  comuna_1987_origen3==99999 ~ "Unknown place 5 years ago"), 
                        levels=c("Under 5", "Same municipality as 5 years ago", "Other municipality as 5 years ago", "Other country as 5 years ago", "Unknown place 5 years ago")),
         com_ori_cod=case_when(com_ori=="Same municipality as 5 years ago" ~ com,
                               com_ori=="Other municipality as 5 years ago" ~ comuna_1987_origen3),
         com_nac=factor(case_when(comuna_madre_origen3==com ~ "Same municipality of birth",
                                  com==9112 & comuna_madre_origen3==9101 ~ "Same municipality of birth", #Excepction for change from Temuco to Padre las Casas
                                  com==9121 & comuna_madre_origen3==9111 ~ "Same municipality of birth", #Excepction for change from Nueva Imperial to Cholchol
                                  comuna_madre_origen3!=com & comuna_madre_origen3>=1000 & comuna_madre_origen3<99999 ~ "Other municipality of birth",
                                  comuna_madre_origen3!=com & comuna_madre_origen3<1000 ~ "Other country of birth",
                                  comuna_madre_origen3==99999 ~ "Unknown place of birth"),
                        levels=c("Same municipality of birth", "Other municipality of birth", "Other country of birth", "Unknown place of birth")),
         com_nac_cod=case_when(com_nac=="Same municipality of birth" ~ com,
                               com_nac=="Other municipality of birth" ~ comuna_madre_origen3))%>%
  left_join(y=select(com17, com, nom_com, urb_rec), by=c("com" = "com"), multiple="any")%>% 
  left_join(y=select(com17, com, nom_com, urb_rec), by=c("com_res_cod" = "com"), multiple="any", suffix=c("", "_res"))%>%
  left_join(y=select(com17, com, nom_com, urb_rec), by=c("com_ori_cod" = "com"), multiple="any", suffix=c("", "_ori"))%>%
  left_join(y=select(com17, com, nom_com, urb_rec), by=c("com_nac_cod" = "com"), multiple="any", suffix=c("", "_nac"))%>%
  rename(reg_res=reg, reg=reg.x)%>%
  mutate(mig_res=factor(case_when(com_res=="Same municipality of residence" ~ "Non-migrant of residence",
                                  com_res=="Other municipality of residence" & urb_rec_res== "Urban" ~ "Migrant residing in an urban municipality",
                                  com_res=="Other municipality of residence" & urb_rec_res== "Mixed" ~ "Migrant residing in a mixed municipality",
                                  com_res=="Other municipality of residence" & urb_rec_res== "Rural" ~ "Migrant residing in a rural municipality",
                                  com_res=="Other country of residence" ~ "Migrant of foreign residence"),
                        levels=c("Non-migrant of residence", "Migrant residing in an urban municipality", "Migrant residing in a mixed municipality", "Migrant residing in a rural municipality", "Migrant of foreign residence")),
         mig_res_reg=factor(case_when(com_res=="Same municipality of residence" ~ "Non-migrant of residence",
                                      com_res=="Other municipality of residence" & urb_rec_res== "Urban" & reg_res==reg ~ "Urban migrant residing in the same region",
                                      com_res=="Other municipality of residence" & urb_rec_res!= "Urban" & reg_res==reg ~ "Non-urban migrant residing in the same region",
                                      com_res=="Other municipality of residence" & urb_rec_res== "Urban" & reg_res!=reg ~ "Urban migrant residing in a different region",
                                      com_res=="Other municipality of residence" & urb_rec_res!= "Urban" & reg_res!=reg ~ "Non-urban migrant residing in a different region",
                                      com_res=="Other country of residence" ~ "Migrant of foreign residence"),
                            levels=c("Non-migrant of residence",  "Urban migrant residing in the same region", "Non-urban migrant residing in the same region", 
                                     "Urban migrant residing in a different region", "Non-urban migrant residing in a different region", "Migrant of foreign residence")),
         mig_ori=factor(case_when(com_ori=="Under 5" ~ "Under 5",
                                  com_ori=="Same municipality as 5 years ago" ~ "Non-migrant 5 years ago",
                                  com_ori=="Other municipality as 5 years ago" & urb_rec_ori== "Urban" ~ "5 years ago migrant, urban municipality",
                                  com_ori=="Other municipality as 5 years ago" & urb_rec_ori== "Mixed" ~ "5 years ago migrant, mixed municipality",
                                  com_ori=="Other municipality as 5 years ago" & urb_rec_ori== "Rural" ~ "5 years ago migrant, rural municipality",
                                  com_ori=="Other country as 5 years ago" ~ "5 years ago migrant, foreign"),
                        levels=c("Under 5", "Non-migrant 5 years ago", "5 years ago migrant, urban municipality", "5 years ago migrant, mixed municipality", "5 years ago migrant, rural municipality", "5 years ago migrant, foreign")),
         mig_ori_reg=factor(case_when(com_ori=="Under 5" ~ "Under 5",
                                      com_ori=="Same municipality as 5 years ago" ~ "Non-migrant 5 years ago",
                                      com_ori=="Other municipality as 5 years ago" & urb_rec_ori== "Urban" & reg_res==reg_ori ~ "5 years ago urban migrant, same region",
                                      com_ori=="Other municipality as 5 years ago" & urb_rec_ori!= "Urban" & reg_res==reg_ori ~ "5 years ago non-urban migrant, same region",
                                      com_ori=="Other municipality as 5 years ago" & urb_rec_ori== "Urban" & reg_res!=reg_ori ~ "5 years ago urban migrant, a different region",
                                      com_ori=="Other municipality as 5 years ago" & urb_rec_ori!= "Urban" & reg_res!=reg_ori ~ "5 years ago non-urban migrant, a different region",
                                      com_ori=="Other country as 5 years ago" ~ "5 years ago migrant, foreign"),
                            levels=c("Under 5", "Non-migrant 5 years ago", "5 years ago urban migrant, same region", "5 years ago non-urban migrant, same region", 
                                     "5 years ago urban migrant, a different region", "5 years ago non-urban migrant, a different region", "5 years ago migrant, foreign")),
         mig_nac=factor(case_when(com_nac=="Same municipality of birth" ~ "Non-migrant of birth",
                                  com_nac=="Other municipality of birth" & urb_rec_nac== "Urban" ~ "Urban municipality birth migrant",
                                  com_nac=="Other municipality of birth" & urb_rec_nac== "Mixed" ~ "Mixed municipality birth migrant",
                                  com_nac=="Other municipality of birth" & urb_rec_nac== "Rural" ~ "Rural municipality birth migrant",
                                  com_nac=="Other country of birth" ~ "Foreign birth migrant"),
                        levels=c("Non-migrant of birth", "Urban municipality birth migrant", "Mixed municipality birth migrant", "Rural municipality birth migrant", "Foreign birth migrant")),
         mig_nac_reg=factor(case_when(com_nac=="Same municipality of birth" ~ "Non-migrant of birth",
                                      com_nac=="Other municipality of birth" & urb_rec_nac== "Urban" & reg_res==reg_nac ~ "Urban birth migrant, same region",
                                      com_nac=="Other municipality of birth" & urb_rec_nac!= "Urban" & reg_res==reg_nac ~ "Non-urban birth migrant, same region",
                                      com_nac=="Other municipality of birth" & urb_rec_nac== "Urban" & reg_res!=reg_nac ~ "Urban birth migrant, a different region",
                                      com_nac=="Other municipality of birth" & urb_rec_nac!= "Urban" & reg_res!=reg_nac ~ "Non-urban birth migrant, a different region",
                                      com_nac=="Other country of birth" ~ "Foreign birth migrant"),
                            levels=c("Non-migrant of birth", "Urban birth migrant, same region", "Non-urban birth migrant, same region",
                                     "Urban birth migrant, a different region", "Non-urban birth migrant, a different region", "Foreign birth migrant")),
         mig_tip=factor(case_when((com_ori=="Same municipality as 5 years ago" | com_ori=="Under 5") & com_nac=="Same municipality of birth" ~ "Non-migrant",
                                  (com_ori=="Same municipality as 5 years ago" | com_ori=="Under 5") & (com_nac=="Other municipality of birth" | com_nac=="Other country of birth") ~ "Absolute migrant",
                                  (com_ori=="Other municipality as 5 years ago" | com_ori=="Other country as 5 years ago") & com_nac=="Same municipality of birth" ~ "Return migrant",
                                  (com_ori=="Other municipality as 5 years ago" | com_ori=="Other country as 5 years ago") & (com_nac=="Other municipality of birth" | com_nac=="Other country of birth") ~ "Recent migrant",
                                  (com_ori=="Same municipality as 5 years ago" | com_ori=="Under 5") & com_nac=="Unknown place of birth" ~ "Non-recent migrant, unknown birth",
                                  (com_ori=="Other municipality as 5 years ago" | com_ori=="Other country as 5 years ago") & com_nac=="Unknown place of birth" ~ "Recent migrant, unknown birth",
                                  com_ori=="Unknown place 5 years ago" & com_nac=="Same municipality of birth" ~ "Non-absolute migrant, unknown recent residence",
                                  com_ori=="Unknown place 5 years ago" & (com_nac=="Other municipality of birth" | com_nac=="Other country of birth") ~ "Absolute migrant, recent residence unknown",
                                  com_ori=="Unknown place 5 years ago" & com_nac=="Unknown place of birth" ~ "Birth and recent residence unknown"),
                        levels=c("Absolute migrant, recent residence unknown", 
                                 "Non-absolute migrant, unknown recent residence", "Non-recent migrant, unknown birth",
                                 "Birth and recent residence unknown",
                                 "Non-migrant", "Absolute migrant", "Return migrant", "Recent migrant, unknown birth", "Recent migrant")))%>%
  select(reg, pro, com, nom_com, urb_rec, dc=distrito, zc=zc_loc, geocode, vn=vivienda, hn=hogar, pn=persona, portafolio, 
         sex, edad, edad_rec, esc, esc_rec, pue, sfdt, rama, 
         reg_res, com_res, com_res_cod, com_res_nom=nom_com_res, com_res_urb=urb_rec_res, 
         reg_ori, com_ori, com_ori_cod, com_ori_nom=nom_com_ori, com_ori_urb=urb_rec_ori,
         reg_nac, com_nac, com_nac_cod, com_nac_nom=nom_com_nac, com_nac_urb=urb_rec_nac,
         starts_with("mig_"))


# 2.3. Census: 2002 -------------------------------------------------------

#A full version (useful for Table 2 and some descriptive analysis in the text)
censo02_full<-readRDS("Data/Censo2002_Persona_Full.Rds")%>%
  mutate(reg=as.integer(region),
         reg_cat=factor(case_when(reg==9 | reg==10 | reg==14 ~ "La Araucanía, Los Ríos, Los Lagos",
                                  reg<9 | reg==11 | reg==12 | reg==13 | reg>14 ~ "Rest of Chile"),
                        levels=c("La Araucanía, Los Ríos, Los Lagos", "Rest of Chile")),
         com=as.integer(comuna),
         com_res=factor(case_when(p23a==1 ~ "Same municipality of residence",
                                  p23a==2 ~ "Other municipality of residence",
                                  p23a==3 ~ "Other country of residence",
                                  p23a==9 ~ "Unknown place of residence"),
                        levels=c("Same municipality of residence", "Other municipality of residence", "Other country of residence", "Unknown place of residence")),
         sex=factor(case_when(p18==1 ~ "Male",
                              p18==2 ~ "Female"),
                    levels=c("Male", "Female")),
         edad_rec=factor(case_when(p19<15 ~ "0-14",
                                   p19>=15 & p19<30 ~ "15-29",
                                   p19>=30 & p19<45 ~ "30-44",
                                   p19>=45 & p19<60 ~ "45-59",
                                   p19>=60 ~ ">=60"),
                         levels=c("0-14", "15-29", "30-44", "45-59", ">=60")),
         esc=factor(case_when(p19<25~NA,
                              p19>=25 & p26a<=2 ~ "Did not attend", 
                              p19>=25 & (p26a==3 | p26a==4) & p26b<8 ~ "Less than full primary", 
                              p19>=25 & (p26a==6 | p26a==11 | p26a==12) & p26b<2 ~ "Less than full primary", 
                              p19>=25 & (p26a==3 | p26a==4) & p26b==8 ~ "Primary (completed)",
                              p19>=25 & (p26a==6 | p26a==11 | p26a==12) & p26b==2 ~ "Primary (completed)",
                              p19>=25 & (p26a==5 | (p26a>=7 & p26a<=10))  & p26b<4 ~ "Incomplete secondary", 
                              p19>=25 & (p26a==6 | p26a==11 | p26a==12) & p26b>2 & p26b<6 ~ "Incomplete secondary", 
                              p19>=25 & (p26a==5 | (p26a>=7 & p26a<=10))  & p26b>=4 ~ "Secondary (completed)", 
                              p19>=25 & (p26a==6 | p26a==11 | p26a==12) & p26b>2 & p26b>=6 ~ "Secondary (completed)", 
                              p19>=25 & p26a>=13 ~ "Higher education"),
                    levels=c("Did not attend", "Less than full primary", "Primary (completed)", "Incomplete secondary", "Secondary (completed)", "Higher education")),
         esc_rec=factor(case_when(esc=="Did not attend" | esc=="Less than full primary" | esc== "Primary (completed)" ~ "Primary (0-8 years)",
                                  esc=="Incomplete secondary" | esc== "Secondary (completed)" ~ "Secondary (9-12 years)",
                                  esc=="Higher education" ~ "Higher education"),
                        levels=c("Primary (0-8 years)", "Secondary (9-12 years)", "Higher education")),
         pue=factor(case_when(p21<9 ~ "Indigenous",
                              p21==9 ~ "Non-indigenous")),
         sfdt=factor(case_when(p29==0 ~ NA,
                               p29==1 | p29==2 | p29== 4 ~ "Employed",
                               p29==3 | p29==5 ~ "Unemployed",
                               p29>=6 ~ "Outside the labour force"),
                     levels=c("Employed", "Unemployed", "Outside the labour force")),
         rama=factor(case_when(p32>=1 & p32 <=3 ~ "Agriculture",
                               (p32>=5 & p32<=9) | (p32>=10 & p32<=33) | (p32>=34  & p32<=39) ~ "Mining; Electricity, gas and water supply; Manufacturing",
                               (p32>=40 & p32<=43)  ~ "Construction",
                               (p32>=45 & p32<=47) | (p32>=49 & p32<=53) | (p32>=55 & p32<=56) | (p32>=56 & p32<=63) ~ "Trade; Transportation; Accommodation and food",
                               (p32>=64 & p32<=67) | (p32==68) | (p32>=69 & p32<=75) | (p32>=77 & p32<=82) | 
                                 (p32==84) | (p32==85) | (p32>=86 & p32<=88) | (p32>=90 & p32<=93) | (p32>=94 & p32<=96) | (p32>=97 & p32<=98) | (p32==99) ~ "Business, administrative and non-market services"),
                     levels=c("Agriculture", "Mining; Electricity, gas and water supply; Manufacturing", "Construction", "Trade; Transportation; Accommodation and food",
                              "Business, administrative and non-market services")))%>%
  select(area, com, com_res, reg_cat, sex, edad=p19, edad_rec, esc_rec, pue, sfdt, rama)

#The study area database
censo02<-readRDS("Data/Censo2002_Persona_Full.Rds")%>%
  filter((region==9 | region==10 | region==14) & area==2)%>%
  mutate(reg=as.integer(region),
         pro=as.integer(provincia),
         com=as.integer(comuna),
         sex=factor(case_when(p18==1 ~ "Male",
                              p18==2 ~ "Female")),
         edad_rec=factor(case_when(p19<15 ~ "0-14",
                                   p19>=15 & p19<30 ~ "15-29",
                                   p19>=30 & p19<45 ~ "30-44",
                                   p19>=45 & p19<60 ~ "45-59",
                                   p19>=60 ~ ">=60"),
                         levels=c("0-14", "15-29", "30-44", "45-59", ">=60")),
         esc=factor(case_when(p19<25~NA,
                              p19>=25 & p26a<=2 ~ "Did not attend", 
                              p19>=25 & (p26a==3 | p26a==4) & p26b<8 ~ "Less than full primary", 
                              p19>=25 & (p26a==6 | p26a==11 | p26a==12) & p26b<2 ~ "Less than full primary", 
                              p19>=25 & (p26a==3 | p26a==4) & p26b==8 ~ "Primary (completed)",
                              p19>=25 & (p26a==6 | p26a==11 | p26a==12) & p26b==2 ~ "Primary (completed)",
                              p19>=25 & (p26a==5 | (p26a>=7 & p26a<=10))  & p26b<4 ~ "Incomplete secondary", 
                              p19>=25 & (p26a==6 | p26a==11 | p26a==12) & p26b>2 & p26b<6 ~ "Incomplete secondary", 
                              p19>=25 & (p26a==5 | (p26a>=7 & p26a<=10))  & p26b>=4 ~ "Secondary (completed)", 
                              p19>=25 & (p26a==6 | p26a==11 | p26a==12) & p26b>2 & p26b>=6 ~ "Secondary (completed)", 
                              p19>=25 & p26a>=13 ~ "Higher education"),
                    levels=c("Did not attend", "Less than full primary", "Primary (completed)", "Incomplete secondary", "Secondary (completed)", "Higher education")),
         esc_rec=factor(case_when(esc=="Did not attend" | esc=="Less than full primary" | esc== "Primary (completed)" ~ "Primary (0-8 years)",
                                  esc=="Incomplete secondary" | esc== "Secondary (completed)" ~ "Secondary (9-12 years)",
                                  esc=="Higher education" ~ "Higher education"),
                        levels=c("Primary (0-8 years)", "Secondary (9-12 years)", "Higher education")),
         pue=factor(case_when(p21<9 ~ "Indigenous",
                              p21==9 ~ "Non-indigenous")),
         sfdt=factor(case_when(p29==0 ~ NA,
                               p29==1 | p29==2 | p29== 4 ~ "Employed",
                               p29==3 | p29==5 ~ "Unemployed",
                               p29>=6 ~ "Outside the labour force"),
                     levels=c("Employed", "Unemployed", "Outside the labour force")),
         rama=factor(case_when(p32>=1 & p32 <=3 ~ "Agriculture",
                               (p32>=5 & p32<=9) | (p32>=10 & p32<=33) | (p32>=34  & p32<=39) ~ "Mining; Electricity, gas and water supply; Manufacturing",
                               (p32>=40 & p32<=43)  ~ "Construction",
                               (p32>=45 & p32<=47) | (p32>=49 & p32<=53) | (p32>=55 & p32<=56) | (p32>=56 & p32<=63) ~ "Trade; Transportation; Accommodation and food",
                               (p32>=64 & p32<=67) | (p32==68) | (p32>=69 & p32<=75) | (p32>=77 & p32<=82) | 
                                 (p32==84) | (p32==85) | (p32>=86 & p32<=88) | (p32>=90 & p32<=93) | (p32>=94 & p32<=96) | (p32>=97 & p32<=98) | (p32==99) ~ "Business, administrative and non-market services"),
                     levels=c("Agriculture", "Mining; Electricity, gas and water supply; Manufacturing", "Construction", "Trade; Transportation; Accommodation and food",
                              "Business, administrative and non-market services")),
         com_res=factor(case_when(p23a==1 ~ "Same municipality of residence",
                                  p23a==2 ~ "Other municipality of residence",
                                  p23a==2 & com==9121 & p23b==9111 ~ "Same municipality of residence", #Excepction for change from Nueva Imperial to Cholchol
                                  p23a==3 ~ "Other country of residence",
                                  p23a==9 ~ "Unknown place of residence"),
                        levels=c("Same municipality of residence", "Other municipality of residence", "Other country of residence", "Unknown place of residence")),
         com_res_cod=case_when(com_res=="Same municipality of residence" ~ com,
                               com_res=="Other municipality of residence" ~ p23b),
         com_ori=factor(case_when(p19<5 ~ "Under 5",
                                  p24a==1 ~ "Same municipality as 5 years ago",
                                  p24a==2 ~ "Other municipality as 5 years ago",
                                  p24a==2 & com==9121 & p24b==9111 ~ "Same municipality as 5 years ago", #Excepción flujo Cholchol-Nueva Imperial
                                  p24a==3 ~ "Other country as 5 years ago",
                                  p24a==9  ~ "Unknown place 5 years ago"),
                        levels=c("Under 5", "Same municipality as 5 years ago", "Other municipality as 5 years ago", "Other country as 5 years ago", "Unknown place 5 years ago")),
         com_ori_cod=case_when(com_ori=="Same municipality as 5 years ago" ~ com_res_cod,
                               com_ori=="Other municipality as 5 years ago" ~ p24b),
         com_nac=factor(case_when(p22a==1 ~ "Same municipality of birth",
                                  p22a==2 ~ "Other municipality of birth",
                                  p22a==2 & com==9121 & p22b==9111 ~ "Same municipality of birth", #Excepción flujo Cholchol-Nueva Imperial
                                  p22a==3 ~ "Other country of birth",
                                  p22a==9 ~ "Unknown place of birth"),
                        levels=c("Same municipality of birth", "Other municipality of birth", "Other country of birth", "Unknown place of birth")),
         com_nac_cod=case_when(com_nac=="Same municipality of birth" ~ com,
                               com_nac=="Other municipality of birth" ~ p22b))%>%
  left_join(y=select(com17, com, nom_com, urb_rec), by=c("com" = "com"), multiple="any")%>% #Traemos los nombres y tipos de comuna de residencia actual y de origen en caso de migrantes.
  left_join(y=select(com17, com, nom_com, urb_rec), by=c("com_res_cod" = "com"), multiple="any", suffix=c("", "_res"))%>%
  left_join(y=select(com17, com, nom_com, urb_rec), by=c("com_ori_cod" = "com"), multiple="any", suffix=c("", "_ori"))%>%
  left_join(y=select(com17, com, nom_com, urb_rec), by=c("com_nac_cod" = "com"), multiple="any", suffix=c("", "_nac"))%>%
  rename(reg_res=reg, reg=reg.x)%>%
  mutate(mig_res=factor(case_when(com_res=="Same municipality of residence" ~ "Non-migrant of residence",
                                  com_res=="Other municipality of residence" & urb_rec_res== "Urban" ~ "Migrant residing in an urban municipality",
                                  com_res=="Other municipality of residence" & urb_rec_res== "Mixed" ~ "Migrant residing in a mixed municipality",
                                  com_res=="Other municipality of residence" & urb_rec_res== "Rural" ~ "Migrant residing in a rural municipality",
                                  com_res=="Other country of residence" ~ "Migrant of foreign residence"),
                        levels=c("Non-migrant of residence", "Migrant residing in an urban municipality", "Migrant residing in a mixed municipality", "Migrant residing in a rural municipality", "Migrant of foreign residence")),
         mig_res_reg=factor(case_when(com_res=="Same municipality of residence" ~ "Non-migrant of residence",
                                      com_res=="Other municipality of residence" & urb_rec_res== "Urban" & reg_res==reg ~ "Urban migrant residing in the same region",
                                      com_res=="Other municipality of residence" & urb_rec_res!= "Urban" & reg_res==reg ~ "Non-urban migrant residing in the same region",
                                      com_res=="Other municipality of residence" & urb_rec_res== "Urban" & reg_res!=reg ~ "Urban migrant residing in a different region",
                                      com_res=="Other municipality of residence" & urb_rec_res!= "Urban" & reg_res!=reg ~ "Non-urban migrant residing in a different region",
                                      com_res=="Other country of residence" ~ "Migrant of foreign residence"),
                            levels=c("Non-migrant of residence",  "Urban migrant residing in the same region", "Non-urban migrant residing in the same region", 
                                     "Urban migrant residing in a different region", "Non-urban migrant residing in a different region", "Migrant of foreign residence")),
         mig_ori=factor(case_when(com_ori=="Under 5" ~ "Under 5",
                                  com_ori=="Same municipality as 5 years ago" ~ "Non-migrant 5 years ago",
                                  com_ori=="Other municipality as 5 years ago" & urb_rec_ori== "Urban" ~ "5 years ago migrant, urban municipality",
                                  com_ori=="Other municipality as 5 years ago" & urb_rec_ori== "Mixed" ~ "5 years ago migrant, mixed municipality",
                                  com_ori=="Other municipality as 5 years ago" & urb_rec_ori== "Rural" ~ "5 years ago migrant, rural municipality",
                                  com_ori=="Other country as 5 years ago" ~ "5 years ago migrant, foreign"),
                        levels=c("Under 5", "Non-migrant 5 years ago", "5 years ago migrant, urban municipality", "5 years ago migrant, mixed municipality", "5 years ago migrant, rural municipality", "5 years ago migrant, foreign")),
         mig_ori_reg=factor(case_when(com_ori=="Under 5" ~ "Under 5",
                                      com_ori=="Same municipality as 5 years ago" ~ "Non-migrant 5 years ago",
                                      com_ori=="Other municipality as 5 years ago" & urb_rec_ori== "Urban" & reg_res==reg_ori ~ "5 years ago urban migrant, same region",
                                      com_ori=="Other municipality as 5 years ago" & urb_rec_ori!= "Urban" & reg_res==reg_ori ~ "5 years ago non-urban migrant, same region",
                                      com_ori=="Other municipality as 5 years ago" & urb_rec_ori== "Urban" & reg_res!=reg_ori ~ "5 years ago urban migrant, a different region",
                                      com_ori=="Other municipality as 5 years ago" & urb_rec_ori!= "Urban" & reg_res!=reg_ori ~ "5 years ago non-urban migrant, a different region",
                                      com_ori=="Other country as 5 years ago" ~ "5 years ago migrant, foreign"),
                            levels=c("Under 5", "Non-migrant 5 years ago", "5 years ago urban migrant, same region", "5 years ago non-urban migrant, same region", 
                                     "5 years ago urban migrant, a different region", "5 years ago non-urban migrant, a different region", "5 years ago migrant, foreign")),
         mig_nac=factor(case_when(com_nac=="Same municipality of birth" ~ "Non-migrant of birth",
                                  com_nac=="Other municipality of birth" & urb_rec_nac== "Urban" ~ "Urban municipality birth migrant",
                                  com_nac=="Other municipality of birth" & urb_rec_nac== "Mixed" ~ "Mixed municipality birth migrant",
                                  com_nac=="Other municipality of birth" & urb_rec_nac== "Rural" ~ "Rural municipality birth migrant",
                                  com_nac=="Other country of birth" ~ "Foreign birth migrant"),
                        levels=c("Non-migrant of birth", "Urban municipality birth migrant", "Mixed municipality birth migrant", "Rural municipality birth migrant", "Foreign birth migrant")),
         mig_nac_reg=factor(case_when(com_nac=="Same municipality of birth" ~ "Non-migrant of birth",
                                      com_nac=="Other municipality of birth" & urb_rec_nac== "Urban" & reg_res==reg_nac ~ "Urban birth migrant, same region",
                                      com_nac=="Other municipality of birth" & urb_rec_nac!= "Urban" & reg_res==reg_nac ~ "Non-urban birth migrant, same region",
                                      com_nac=="Other municipality of birth" & urb_rec_nac== "Urban" & reg_res!=reg_nac ~ "Urban birth migrant, a different region",
                                      com_nac=="Other municipality of birth" & urb_rec_nac!= "Urban" & reg_res!=reg_nac ~ "Non-urban birth migrant, a different region",
                                      com_nac=="Other country of birth" ~ "Foreign birth migrant"),
                            levels=c("Non-migrant of birth", "Urban birth migrant, same region", "Non-urban birth migrant, same region",
                                     "Urban birth migrant, a different region", "Non-urban birth migrant, a different region", "Foreign birth migrant")),
         mig_tip=factor(case_when((com_ori=="Same municipality as 5 years ago" | com_ori=="Under 5") & com_nac=="Same municipality of birth" ~ "Non-migrant",
                                  (com_ori=="Same municipality as 5 years ago" | com_ori=="Under 5") & (com_nac=="Other municipality of birth" | com_nac=="Other country of birth") ~ "Absolute migrant",
                                  (com_ori=="Other municipality as 5 years ago" | com_ori=="Other country as 5 years ago") & com_nac=="Same municipality of birth" ~ "Return migrant",
                                  (com_ori=="Other municipality as 5 years ago" | com_ori=="Other country as 5 years ago") & (com_nac=="Other municipality of birth" | com_nac=="Other country of birth") ~ "Recent migrant",
                                  (com_ori=="Same municipality as 5 years ago" | com_ori=="Under 5") & com_nac=="Unknown place of birth" ~ "Non-recent migrant, unknown birth",
                                  (com_ori=="Other municipality as 5 years ago" | com_ori=="Other country as 5 years ago") & com_nac=="Unknown place of birth" ~ "Recent migrant, unknown birth",
                                  com_ori=="Unknown place 5 years ago" & com_nac=="Same municipality of birth" ~ "Non-absolute migrant, unknown recent residence",
                                  com_ori=="Unknown place 5 years ago" & (com_nac=="Other municipality of birth" | com_nac=="Other country of birth") ~ "Absolute migrant, recent residence unknown",
                                  com_ori=="Unknown place 5 years ago" & com_nac=="Unknown place of birth" ~ "Birth and recent residence unknown"),
                        levels=c("Absolute migrant, recent residence unknown",
                                 "Non-absolute migrant, unknown recent residence", "Non-recent migrant, unknown birth",
                                 "Birth and recent residence unknown",
                                 "Non-migrant", "Absolute migrant", "Return migrant", "Recent migrant, unknown birth", "Recent migrant")))%>%
  select(reg, pro, com, nom_com, urb_rec, dc=distrito, zc=zc_loc, geocode, vn, hn, pn, portafolio, 
         sex, edad=p19, edad_rec, esc, esc_rec, pue, sfdt, rama, 
         reg_res, com_res, com_res_cod, com_res_nom=nom_com_res, com_res_urb=urb_rec_res, 
         reg_ori, com_ori, com_ori_cod, com_ori_nom=nom_com_ori, com_ori_urb=urb_rec_ori,
         reg_nac, com_nac, com_nac_cod, com_nac_nom=nom_com_nac, com_nac_urb=urb_rec_nac,
         starts_with("mig_"))

# 2.3. Census: 2017 -------------------------------------------------------

#A full version (useful for Table 2 and some descriptive analysis in the text)
censo17_full<-readRDS("Data/Censo2017_Persona_Full.Rds")%>%
  mutate(reg=as.integer(region),
         reg_cat=factor(case_when(region==9 | region==10 | region==14 ~ "La Araucanía, Los Ríos, Los Lagos",
                                  region<9 | region==11 | region==12 | region==13 | region>14 ~ "Rest of Chile"),
                        levels=c("La Araucanía, Los Ríos, Los Lagos", "Rest of Chile")),
         com=as.integer(comuna),
         com_res=factor(case_when(p10==1 | p10==2 ~ "Same municipality of residence",
                                  p10==3 ~ "Other municipality of residence",
                                  p10==4 ~ "Other country of residence",
                                  p10==99 ~ "Unknown place of residence"),
                        levels=c("Same municipality of residence", "Other municipality of residence", "Other country of residence", "Unknown place of residence")),
         sex=factor(case_when(p08==1 ~ "Male",
                              p08==2 ~ "Female")),
         edad_rec=factor(case_when(p09<15 ~ "0-14",
                                   p09>=15 & p09<30 ~ "15-29",
                                   p09>=30 & p09<45 ~ "30-44",
                                   p09>=45 & p09<60 ~ "45-59",
                                   p09>=60 ~ ">=60"),
                         levels=c("0-14", "15-29", "30-44", "45-59", ">=60")),
         esc=factor(case_when(p09<25~NA,
                              p09>=25 & escolaridad==0 ~ "Did not attend", 
                              p09>=25 & p15<=3 ~ "Less than full primary", 
                              p09>=25 & (p15>3 & p15<=5) & p15a==2 ~ "Less than full primary", 
                              p09>=25 & p15==6 ~ "Less than full primary",
                              p09>=25 & (p15==9 | p15==10) & p15a==2 & p14==1 ~ "Less than full primary",
                              p09>=25 & (p15==4 | p15==5) & p15a==1 ~ "Primary (completed)", 
                              p09>=25 & (p15==9 | p15==10) & p15a==2 & p14==2 ~ "Primary (completed)",
                              p09>=25 & (p15==7 | p15==8) & p15a==2 ~ "Incomplete secondary",
                              p09>=25 & (p15==9 | p15==10) & p15a==2 & p14>2 ~ "Incomplete secondary",
                              p09>=25 & (p15>=7 & p15<=10) & p15a==1 ~ "Secondary (completed)",
                              p09>=25 & p15>=11 ~ "Higher education"), 
                    levels=c("Did not attend", "Less than full primary", "Primary (completed)", "Incomplete secondary", "Secondary (completed)", "Higher education")),
         esc_rec=factor(case_when(esc=="Did not attend" | esc=="Less than full primary" | esc== "Primary (completed)" ~ "Primary (0-8 years)",
                                  esc=="Incomplete secondary" | esc== "Secondary (completed)" ~ "Secondary (9-12 years)",
                                  esc=="Higher education" ~ "Higher education"),
                        levels=c("Primary (0-8 years)", "Secondary (9-12 years)", "Higher education")),
         pue=factor(case_when(p16==1 ~"Indigenous",
                              p16==2 ~ "Non-indigenous")),
         sfdt=factor(case_when(p17==98 ~ NA,
                               p17==1 | p17==2 | p17==3 ~ "Employed",
                               p17==4 ~ "Unemployed",
                               p17>=5 ~ "Outside the labour force"),
                     levels=c("Employed", "Unemployed", "Outside the labour force")),
         rama=factor(case_when(p18=="A" ~ "Agriculture",
                               p18=="B" | p18=="C" | p18=="D" | p18=="E" ~ "Mining; Electricity, gas and water supply; Manufacturing",
                               p18=="F" ~ "Construction",
                               p18=="G" | p18=="H" | p18=="I" | p18=="J" ~ "Trade; Transportation; Accommodation and food",
                               p18=="K" | p18=="L" | p18=="M" | p18=="N" | p18=="O" | p18=="P" | p18=="Q" | p18=="R" | p18=="S" | p18=="T" | p18=="U" ~ "Business, administrative and non-market services",
                               p18=="Z" | p18=="99" ~ NA),
                     levels=c("Agriculture", "Mining; Electricity, gas and water supply; Manufacturing", "Construction", "Trade; Transportation; Accommodation and food",
                              "Business, administrative and non-market services")))%>%
  select(area, com, com_res, reg_cat, sex, edad=p09, edad_rec, esc_rec, pue, sfdt, rama)

#The study area database
censo17<-readRDS("Data/Censo2017_Persona_Full.Rds")%>%
  filter((region==9 | region==10 | region==14) & area==2)%>%
  mutate(com=as.integer(comuna),
         sex=factor(case_when(p08==1 ~ "Male",
                              p08==2 ~ "Female")),
         edad_rec=factor(case_when(p09<15 ~ "0-14",
                                   p09>=15 & p09<30 ~ "15-29",
                                   p09>=30 & p09<45 ~ "30-44",
                                   p09>=45 & p09<60 ~ "45-59",
                                   p09>=60 ~ ">=60"),
                         levels=c("0-14", "15-29", "30-44", "45-59", ">=60")),
         esc=factor(case_when(p09<25~NA,
                              p09>=25 & escolaridad==0 ~ "Did not attend", 
                              p09>=25 & p15<=3 ~ "Less than full primary", 
                              p09>=25 & (p15>3 & p15<=5) & p15a==2 ~ "Less than full primary", 
                              p09>=25 & p15==6 ~ "Less than full primary",
                              p09>=25 & (p15==9 | p15==10) & p15a==2 & p14==1 ~ "Less than full primary",
                              p09>=25 & (p15==4 | p15==5) & p15a==1 ~ "Primary (completed)", 
                              p09>=25 & (p15==9 | p15==10) & p15a==2 & p14==2 ~ "Primary (completed)",
                              p09>=25 & (p15==7 | p15==8) & p15a==2 ~ "Incomplete secondary",
                              p09>=25 & (p15==9 | p15==10) & p15a==2 & p14>2 ~ "Incomplete secondary",
                              p09>=25 & (p15>=7 & p15<=10) & p15a==1 ~ "Secondary (completed)",
                              p09>=25 & p15>=11 ~ "Higher education"), 
                    levels=c("Did not attend", "Less than full primary", "Primary (completed)", "Incomplete secondary", "Secondary (completed)", "Higher education")),
         esc_rec=factor(case_when(esc=="Did not attend" | esc=="Less than full primary" | esc== "Primary (completed)" ~ "Primary (0-8 years)",
                                  esc=="Incomplete secondary" | esc== "Secondary (completed)" ~ "Secondary (9-12 years)",
                                  esc=="Higher education" ~ "Higher education"),
                        levels=c("Primary (0-8 years)", "Secondary (9-12 years)", "Higher education")),
         pue=factor(case_when(p16==1 ~"Indigenous",
                              p16==2 ~ "Non-indigenous")),
         sfdt=factor(case_when(p17==98 ~ NA,
                               p17==1 | p17==2 | p17==3 ~ "Employed",
                               p17==4 ~ "Unemployed",
                               p17>=5 ~ "Outside the labour force"),
                     levels=c("Employed", "Unemployed", "Outside the labour force")),
         rama=factor(case_when(p18=="A" ~ "Agriculture",
                               p18=="B" | p18=="C" | p18=="D" | p18=="E" ~ "Mining; Electricity, gas and water supply; Manufacturing",
                               p18=="F" ~ "Construction",
                               p18=="G" | p18=="H" | p18=="I" | p18=="J" ~ "Trade; Transportation; Accommodation and food",
                               p18=="K" | p18=="L" | p18=="M" | p18=="N" | p18=="O" | p18=="P" | p18=="Q" | p18=="R" | p18=="S" | p18=="T" | p18=="U" ~ "Business, administrative and non-market services",
                               p18=="Z" | p18=="99" ~ NA),
                     levels=c("Agriculture", "Mining; Electricity, gas and water supply; Manufacturing", "Construction", "Trade; Transportation; Accommodation and food",
                              "Business, administrative and non-market services")),
         com_res=factor(case_when(p10==1 | p10==2 ~ "Same municipality of residence",
                                  p10==3 ~ "Other municipality of residence",
                                  p10==4 ~ "Other country of residence",
                                  p10==99 ~ "Unknown place of residence"),
                        levels=c("Same municipality of residence", "Other municipality of residence", "Other country of residence", "Unknown place of residence")),
         com_res_cod=case_when(com_res=="Same municipality of residence" ~ com,
                               com_res=="Other municipality of residence" ~ p10comuna),
         com_ori=factor(case_when(p11==1 ~ "Under 5",
                                  p11==2 ~ "Same municipality as 5 years ago",
                                  p11==3 ~ "Other municipality as 5 years ago",
                                  p11>=4 & p11<10 ~ "Other country as 5 years ago",
                                  p11==99 ~ "Unknown place 5 years ago"),
                        levels=c("Under 5", "Same municipality as 5 years ago", "Other municipality as 5 years ago", "Other country as 5 years ago", "Unknown place 5 years ago")),
         com_ori_cod=case_when(com_ori=="Same municipality as 5 years ago" ~ com_res_cod,
                               com_ori=="Other municipality as 5 years ago" ~ p11comuna),
         com_nac=factor(case_when(p12==1 ~ "Same municipality of birth",
                                  p12==2 ~ "Other municipality of birth",
                                  p12>=3 & p12<99 ~ "Other country of birth",
                                  p12==99 ~ "Unknown place of birth"),
                        levels=c("Same municipality of birth", "Other municipality of birth", "Other country of birth", "Unknown place of birth")),
         com_nac_cod=case_when(com_nac=="Same municipality of birth" ~ com,
                               com_nac=="Other municipality of birth" ~ p12comuna))%>%
  left_join(y=select(com17, com, nom_com, urb_rec), by=c("com" = "com"), multiple="any")%>%
  left_join(y=select(com17, com, nom_com, urb_rec), by=c("com_res_cod" = "com"), multiple="any", suffix=c("", "_res"))%>%
  left_join(y=select(com17, com, nom_com, urb_rec), by=c("com_ori_cod" = "com"), multiple="any", suffix=c("", "_ori"))%>%
  left_join(y=select(com17, com, nom_com, urb_rec), by=c("com_nac_cod" = "com"), multiple="any", suffix=c("", "_nac"))%>%
  #left_join(y=select(loc17, geocode, nom_loc=NOM_LOCALIDAD), by="geocode", multiple="any")%>%
  #left_join(y=select(loc17_ent, geocode, cat_ent_rec), by="geocode", multiple="any")%>%
  mutate(mig_res=factor(case_when(com_res=="Same municipality of residence" ~ "Non-migrant of residence",
                                  com_res=="Other municipality of residence" & urb_rec_res== "Urban" ~ "Migrant residing in an urban municipality",
                                  com_res=="Other municipality of residence" & urb_rec_res== "Mixed" ~ "Migrant residing in a mixed municipality",
                                  com_res=="Other municipality of residence" & urb_rec_res== "Rural" ~ "Migrant residing in a rural municipality",
                                  com_res=="Other country of residence" ~ "Migrant of foreign residence"),
                        levels=c("Non-migrant of residence", "Migrant residing in an urban municipality", "Migrant residing in a mixed municipality", "Migrant residing in a rural municipality", "Migrant of foreign residence")),
         mig_res_reg=factor(case_when(com_res=="Same municipality of residence" ~ "Non-migrant of residence",
                                      com_res=="Other municipality of residence" & urb_rec_res== "Urban" & reg_res==reg ~ "Urban migrant residing in the same region",
                                      com_res=="Other municipality of residence" & urb_rec_res!= "Urban" & reg_res==reg ~ "Non-urban migrant residing in the same region",
                                      com_res=="Other municipality of residence" & urb_rec_res== "Urban" & reg_res!=reg ~ "Urban migrant residing in a different region",
                                      com_res=="Other municipality of residence" & urb_rec_res!= "Urban" & reg_res!=reg ~ "Non-urban migrant residing in a different region",
                                      com_res=="Other country of residence" ~ "Migrant of foreign residence"),
                            levels=c("Non-migrant of residence",  "Urban migrant residing in the same region", "Non-urban migrant residing in the same region", 
                                     "Urban migrant residing in a different region", "Non-urban migrant residing in a different region", "Migrant of foreign residence")),
         mig_ori=factor(case_when(com_ori=="Under 5" ~ "Under 5",
                                  com_ori=="Same municipality as 5 years ago" ~ "Non-migrant 5 years ago",
                                  com_ori=="Other municipality as 5 years ago" & urb_rec_ori== "Urban" ~ "5 years ago migrant, urban municipality",
                                  com_ori=="Other municipality as 5 years ago" & urb_rec_ori== "Mixed" ~ "5 years ago migrant, mixed municipality",
                                  com_ori=="Other municipality as 5 years ago" & urb_rec_ori== "Rural" ~ "5 years ago migrant, rural municipality",
                                  com_ori=="Other country as 5 years ago" ~ "5 years ago migrant, foreign"),
                        levels=c("Under 5", "Non-migrant 5 years ago", "5 years ago migrant, urban municipality", "5 years ago migrant, mixed municipality", "5 years ago migrant, rural municipality", "5 years ago migrant, foreign")),
         mig_ori_reg=factor(case_when(com_ori=="Under 5" ~ "Under 5",
                                      com_ori=="Same municipality as 5 years ago" ~ "Non-migrant 5 years ago",
                                      com_ori=="Other municipality as 5 years ago" & urb_rec_ori== "Urban" & reg_res==reg_ori ~ "5 years ago urban migrant, same region",
                                      com_ori=="Other municipality as 5 years ago" & urb_rec_ori!= "Urban" & reg_res==reg_ori ~ "5 years ago non-urban migrant, same region",
                                      com_ori=="Other municipality as 5 years ago" & urb_rec_ori== "Urban" & reg_res!=reg_ori ~ "5 years ago urban migrant, a different region",
                                      com_ori=="Other municipality as 5 years ago" & urb_rec_ori!= "Urban" & reg_res!=reg_ori ~ "5 years ago non-urban migrant, a different region",
                                      com_ori=="Other country as 5 years ago" ~ "5 years ago migrant, foreign"),
                            levels=c("Under 5", "Non-migrant 5 years ago", "5 years ago urban migrant, same region", "5 years ago non-urban migrant, same region", 
                                     "5 years ago urban migrant, a different region", "5 years ago non-urban migrant, a different region", "5 years ago migrant, foreign")),
         mig_nac=factor(case_when(com_nac=="Same municipality of birth" ~ "Non-migrant of birth",
                                  com_nac=="Other municipality of birth" & urb_rec_nac== "Urban" ~ "Urban municipality birth migrant",
                                  com_nac=="Other municipality of birth" & urb_rec_nac== "Mixed" ~ "Mixed municipality birth migrant",
                                  com_nac=="Other municipality of birth" & urb_rec_nac== "Rural" ~ "Rural municipality birth migrant",
                                  com_nac=="Other country of birth" ~ "Foreign birth migrant"),
                        levels=c("Non-migrant of birth", "Urban municipality birth migrant", "Mixed municipality birth migrant", "Rural municipality birth migrant", "Foreign birth migrant")),
         mig_nac_reg=factor(case_when(com_nac=="Same municipality of birth" ~ "Non-migrant of birth",
                                      com_nac=="Other municipality of birth" & urb_rec_nac== "Urban" & reg_res==reg_nac ~ "Urban birth migrant, same region",
                                      com_nac=="Other municipality of birth" & urb_rec_nac!= "Urban" & reg_res==reg_nac ~ "Non-urban birth migrant, same region",
                                      com_nac=="Other municipality of birth" & urb_rec_nac== "Urban" & reg_res!=reg_nac ~ "Urban birth migrant, a different region",
                                      com_nac=="Other municipality of birth" & urb_rec_nac!= "Urban" & reg_res!=reg_nac ~ "Non-urban birth migrant, a different region",
                                      com_nac=="Other country of birth" ~ "Foreign birth migrant"),
                            levels=c("Non-migrant of birth", "Urban birth migrant, same region", "Non-urban birth migrant, same region",
                                     "Urban birth migrant, a different region", "Non-urban birth migrant, a different region", "Foreign birth migrant")),
         mig_tip=factor(case_when((com_ori=="Same municipality as 5 years ago" | com_ori=="Under 5") & com_nac=="Same municipality of birth" ~ "Non-migrant",
                                  (com_ori=="Same municipality as 5 years ago" | com_ori=="Under 5") & (com_nac=="Other municipality of birth" | com_nac=="Other country of birth") ~ "Absolute migrant",
                                  (com_ori=="Other municipality as 5 years ago" | com_ori=="Other country as 5 years ago") & com_nac=="Same municipality of birth" ~ "Return migrant",
                                  (com_ori=="Other municipality as 5 years ago" | com_ori=="Other country as 5 years ago") & (com_nac=="Other municipality of birth" | com_nac=="Other country of birth") ~ "Recent migrant",
                                  (com_ori=="Same municipality as 5 years ago" | com_ori=="Under 5") & com_nac=="Unknown place of birth" ~ "Non-recent migrant, unknown birth",
                                  (com_ori=="Other municipality as 5 years ago" | com_ori=="Other country as 5 years ago") & com_nac=="Unknown place of birth" ~ "Recent migrant, unknown birth",
                                  com_ori=="Unknown place 5 years ago" & com_nac=="Same municipality of birth" ~ "Non-absolute migrant, unknown recent residence",
                                  com_ori=="Unknown place 5 years ago" & (com_nac=="Other municipality of birth" | com_nac=="Other country of birth") ~ "Absolute migrant, recent residence unknown",
                                  com_ori=="Unknown place 5 years ago" & com_nac=="Unknown place of birth" ~ "Birth and recent residence unknown"),
                        levels=c("Absolute migrant, recent residence unknown",
                                 "Non-absolute migrant, unknown recent residence", "Non-recent migrant, unknown birth",
                                 "Birth and recent residence unknown",
                                 "Non-migrant", "Absolute migrant", "Return migrant", "Recent migrant, unknown birth", "Recent migrant")))%>%
  select(reg, pro=provincia, com, nom_com, urb_rec, dc, zc=zc_loc, geocode, vn=nviv, hn=nhogar, pn=personan, 
         sex, edad=p09, edad_rec, esc, esc_rec, pue, sfdt, rama, 
         reg_res, com_res, com_res_cod, com_res_nom=nom_com_res, com_res_urb=urb_rec_res, 
         reg_ori, com_ori, com_ori_cod, com_ori_nom=nom_com_ori, com_ori_urb=urb_rec_ori,
         reg_nac, com_nac, com_nac_cod, com_nac_nom=nom_com_nac, com_nac_urb=urb_rec_nac,
         starts_with("mig_"))

# 3. SPATIAL DATA ---------------------------------------------------------

# 3.1. Municipalities ------------------------------------------------------

#Municipalities
dpa_shp<-st_read(dsn = "Data/2010_Comunas/Comuna.shp", as_tibble = TRUE, quiet = FALSE, drivers = "ESRI Shapefile")%>%
  st_transform(32719)%>%
  select(reg=REGION, nom_reg=DESC_REGIO, pro=PROVINCIA, nom_pro=DESC_PROVI, com=COMUNA, nom_com=DESC_COMUN)%>%
  filter(reg=="9" | reg=="10" | reg=="14")%>%
  mutate(reg=as.double(reg), pro=as.double(pro), com=as.double(com),
         nom_reg=factor(case_when(nom_reg=="REGIÓN DE LA ARAUCANÍA" ~ "La Araucanía",
                           nom_reg=="REGIÓN DE LOS LAGOS" ~ "Los Lagos",
                           nom_reg=="REGIÓN DE LOS RÍOS" ~ "Los Ríos"),
                        levels=c("La Araucanía", "Los Ríos", "Los Lagos")))%>%
  select(-c(reg, nom_com))%>%
  left_join(y=com17, by="com")%>% 
  left_join(y=(ent17%>%
      group_by(COMUNA)%>%
      summarise(pop_rur_ine=sum(PERSONAS, na.rm=T))%>%
      rename(com=COMUNA)),
    by="com")%>%
  mutate(pct_rur_ine=100*pop_rur_ine/pop)

#Study area for figures

stu_are<-dpa_shp%>% 
  filter(reg==9 | reg==10 | reg==14)%>%
  mutate(zona="Study area")%>%
  group_by(zona)%>%
  summarise(n=n())

# 3.2. Rural areas ---------------------------------------------------

#Entities
ent17_shp<-st_read(dsn="Data/2017_Entities/Microdatos_Entidad.shp", as_tibble = TRUE, quiet = FALSE, drivers = "ESRI Shapefile")%>%
  st_transform(32719)%>%
  filter((COD_REGION==9 | COD_REGION==10 | COD_REGION==14)& (AREA==2))%>% 
  mutate(id=paste(COD_COMUNA, DISTRITO, CODIGO_LOC, CODIGO_ENT, sep="-"))%>%
  select(id, geometry)%>%
  inner_join(y=ent17, by="id")

#Districts
loc17_shp<-ent17_shp%>%
  st_make_valid()%>% 
  group_by(COMUNA, DC, ID_ZONA_LOC, NOM_LOCALIDAD, geocode)%>% #Para combinar con ID_ZONA_LOC
  summarise(n_ent=n())

# 3.3. Urban agglomerations -----------------------------------------------

#Urban agglomerations
urb17<-st_read(dsn="Data/2017_Cities/Conurbaciones_2017.shp", as_tibble = TRUE, quiet = FALSE, drivers = "ESRI Shapefile")%>%
  filter(COD_REGION==9 | COD_REGION==10 | COD_REGION==14)%>%
  mutate(cat="CONURBACIÓN",
         reg=as.integer(COD_REGION))%>%
  select(cat, pop=TOT_PERSON, viv=TOT_VIV, nom=CONURB, reg, geometry)%>%
  rbind(st_read(dsn="Data/2017_Cities/Ciudades_2017.shp", as_tibble = TRUE, quiet = FALSE, drivers = "ESRI Shapefile")%>%
          filter(REGION=="9" | REGION=="10" | REGION=="14")%>%
          mutate(reg=as.integer(REGION))%>%
          select(cat=CATEGORIA, pop=TOT_PERSON, viv=TOT_VIV, nom=URBANO, reg, geometry))%>%
  rbind(st_read(dsn="Data/2017_Cities/Pueblos_2017.shp", as_tibble = TRUE, quiet = FALSE, drivers = "ESRI Shapefile")%>%
          filter(REGION=="9" | REGION=="10" | REGION=="14")%>%
          mutate(reg=as.integer(REGION))%>%
          select(cat=CATEGORIA, pop=TOT_PERSON, viv=TOT_VIV, nom=URBANO, reg, geometry))%>%
  st_transform(32719)%>%
  mutate(nom=tolower(nom),
         nom=capwords(nom))


# 3.4. Roads --------------------------------------------------------------

red<-st_read(dsn="Data/2020_Roads/Red_Vial_Chile_05_05_2020.gdb", layer="Red_Vial_Chile", 
             as_tibble = TRUE, quiet = FALSE, drivers = "OpenFileGDB")%>%
  filter(REGION=="Región de La Araucanía" | REGION=="Región de Los Lagos" | REGION== "Región de Los Ríos")%>%
  mutate(carpeta_cat=as.integer(case_when(CARPETA=="Pavimento Doble Calzada" ~ 4,
                                          CARPETA=="Pavimento"~ 3,
                                          CARPETA== "Pavimento Básico" ~ 2,
                                          CARPETA=="Ripio" ~ 1,
                                          CARPETA=="Grava Tratada" ~ 0,
                                          CARPETA=="Suelo Natural" ~ 0)),
         rou=factor(case_when(ROL=="Ruta 5" ~ "South Panamerican",
                              ROL!="Ruta 5" ~ "Main roads"),
                    levels=c("South Panamerican", "Main roads")),
         rou_alpha=case_when(ROL=="Ruta 5" ~ 1,
                             ROL!="Ruta 5" ~ 0.5))%>%
  filter(carpeta_cat>=3)%>%
  st_transform(crs=32719)%>%
  st_zm()

# 4. RESULTS --------------------------------------------------------------

#Colors for scale_manual
colors <- c("Cities (over 30,000)" = "gray20", "South Panamerican" = "red4", "Main roads" = "red1")

# 4.1. Figure 2 -------------------------------------------------------------

fig2a<-ggplot()+
  geom_sf(data=ent17_shp, aes(fill=PERSONAS), alpha=0.7, color="transparent")+
  geom_sf(data=stu_are, fill="transparent", color="gray80")+
  geom_sf(data=filter(st_centroid(urb17), pop>=30000 & nom!="Alerce"), aes(color="Cities (over 30,000)"), size=5, shape=21, fill="white")+
  geom_sf_label_repel(data=filter(urb17,pop>=30000 & nom!="Alerce" & nom!="Temuco- Padre Las Casas"), aes(label=nom))+
  theme_minimal()+
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin())+
  scale_color_manual(values = colors)+
  scale_fill_viridis_b(name="Inhabitants", position="bottom", breaks=c(50, 100, 300, 1000), direction=-1, alpha = 0.7)+
  labs(x="", y="", subtitle="Rural population at the entity level (INE)", color="", caption="
       ")+
  annotation_scale(location = "bl", style="bar", unit_category="metric") +
  annotation_north_arrow(location = "tr", which_north= "true")
  

fig2b<-ggplot()+
  geom_sf(data=dpa_shp, aes(fill=urb_rec), alpha=0.7, color="transparent")+
  geom_sf(data=stu_are, fill="transparent", color="gray80")+
  geom_sf(data=filter(st_centroid(urb17), pop>=30000 & nom!="Alerce"), aes(color="Cities (over 30,000)"), size=5, shape=21, fill="white")+
  geom_sf_label_repel(data=filter(urb17,pop>=30000 & nom!="Alerce" & nom!="Temuco- Padre Las Casas"), aes(label=nom))+
  theme_minimal()+
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin())+
  scale_color_manual(values = colors)+
  scale_fill_viridis_d(name="Municipality of residence", position="bottom", direction=1, alpha = 0.7)+
  labs(x="", y="", subtitle="Municipality typology by rurality", color="", caption="
       Projection system: UTM, Datum WGS84, Zone 18 South.
       ")+ 
  annotation_scale(location = "bl", style="bar", unit_category="metric") +
  annotation_north_arrow(location = "tr", which_north= "true")

fig2<-grid.arrange(fig2a, fig2b, nrow=1)%>%
  ggsave(filename="Figure 2.jpg", width=25, height=25, units="cm", dpi=300)


# 4.2. Table 1 ------------------------------------------------------------

#A database with the selected data
tab1<-censo92%>%
  filter(com_res=="Same municipality of residence")%>%
  select(reg, mig_ori_reg)%>%
  mutate(año="1992")%>%
  rbind(censo02%>%
          filter(com_res=="Same municipality of residence")%>%
          select(reg, mig_ori_reg)%>%
          mutate(año="2002"))%>%
  rbind(censo17%>%
          filter(com_res=="Same municipality of residence")%>%
          select(reg, mig_ori_reg)%>%
          mutate(año="2017"))%>%
  mutate(reg_nom=factor(case_when(reg==9 ~ "La Araucanía",
                                  reg==14 ~ "Los Ríos",
                                  reg==10 ~ "Los Lagos"),
                        levels=c("La Araucanía", "Los Ríos", "Los Lagos")),
         mig_ori_reg=factor(case_when(mig_ori_reg=="Under 5" ~ "Under 5 years old",
                                      mig_ori_reg=="Non-migrant 5 years ago" ~ "Non-migrant",
                                      mig_ori_reg=="5 years ago urban migrant, same region" ~ "Migrant of urban origin, same region",
                                      mig_ori_reg=="5 years ago urban migrant, a different region" ~ "Migrant of urban origin, a different region",
                                      mig_ori_reg=="5 years ago non-urban migrant, same region" ~ "Migrant of non-urban origin, same region",
                                      mig_ori_reg=="5 years ago non-urban migrant, a different region" ~ "Migrant of non-urban origin, a different region",
                                      mig_ori_reg=="5 years ago migrant, foreign" ~ "Migrant of foreign origin"),
                            levels=c("Under 5 years old", "Non-migrant", "Migrant of urban origin, same region", "Migrant of urban origin, a different region",
                                     "Migrant of non-urban origin, same region", "Migrant of non-urban origin, a different region", "Migrant of foreign origin")),
         n=case_when(reg==9 | reg==10 | reg==14 ~ 1))%>%
  select(reg_nom, n, mig_ori_reg, año)


#4 tables (one for each region, one for total)
tab1a<-tbl_summary(data=tab1,
                   by=año,
                   include=c(n, mig_ori_reg),
                   label=list(n ~ "Total",
                              mig_ori_reg ~ "Total"),
                   statistic = list(n ~ "{n}",
                                    mig_ori_reg ~ "{n} ({p}%)"),
                   digits = list(all_categorical() ~ c(0, 1)),
                   missing="always",
                   missing_text="Origin not reported")%>%
  bold_labels()%>%
  remove_row_type(n, type="missing")%>%
  remove_row_type(mig_ori_reg, type="header")%>%
  modify_header(all_stat_cols() ~ "**{level}**",
                label="**Region and migrant category**")

tab1b<-tbl_summary(data=filter(tab1, reg_nom=="La Araucanía"),
                   by=año,
                   include=c(n, mig_ori_reg),
                   label=list(n ~ "La Araucanía",
                              mig_ori_reg ~ "La Araucanía"),
                   statistic = list(n ~ "{n}",
                                    mig_ori_reg ~ "{n} ({p}%)"),
                   digits = list(all_categorical() ~ c(0, 1)),
                   missing="always",
                   missing_text="Origin not reported")%>%
  bold_labels()%>%
  remove_row_type(n, type="missing")%>%
  remove_row_type(mig_ori_reg, type="header")%>%
  modify_header(all_stat_cols() ~ "**{level}**",
                label="**Region and migrant category**")

tab1c<-tbl_summary(data=filter(tab1, reg_nom=="Los Ríos"),
                   by=año,
                   include=c(n, mig_ori_reg),
                   label=list(n ~ "Los Ríos",
                              mig_ori_reg ~ "Los Ríos"),
                   statistic = list(n ~ "{n}",
                                    mig_ori_reg ~ "{n} ({p}%)"),
                   digits = list(all_categorical() ~ c(0, 1)),
                   missing="always",
                   missing_text="Origin not reported")%>%
  bold_labels()%>%
  remove_row_type(n, type="missing")%>%
  remove_row_type(mig_ori_reg, type="header")%>%
  modify_header(all_stat_cols() ~ "**{level}**",
                label="**Region and migrant category**")

tab1d<-tbl_summary(data=filter(tab1, reg_nom=="Los Lagos"),
                   by=año,
                   include=c(n, mig_ori_reg),
                   label=list(n ~ "Los Lagos",
                              mig_ori_reg ~ "Los Lagos"),
                   statistic = list(n ~ "{n}",
                                    mig_ori_reg ~ "{n} ({p}%)"),
                   digits = list(all_categorical() ~ c(0, 1)),
                   missing="always",
                   missing_text="Origin not reported")%>%
  bold_labels()%>%
  remove_row_type(n, type="missing")%>%
  remove_row_type(mig_ori_reg, type="header")%>%
  modify_header(all_stat_cols() ~ "**{level}**",
                label="**Region and migrant category**")

#The (stacked) table
tab1<-tbl_stack(list(tab1a, tab1b, tab1c, tab1d))%>%
  as_gt()%>%
  gtsave("Table 1.docx")

# 4.3. Figure 3 -----------------------------------------------------------

#Population and migration databases by municipality

pop92_com<-censo92%>%
  filter(com_res=="Same municipality of residence")%>%
  group_by(com)%>%
  summarise(pop92_com=n())
mig_tot92_com<-censo92%>%
  filter(com_res=="Same municipality of residence" & com_ori!="Under 5" & com_ori!="Same municipality as 5 years ago")%>%
  group_by(com)%>%
  summarise(mig_tot92_com=n())
mig_urb92_com<-censo92%>%
  filter(com_res=="Same municipality of residence" & com_ori=="Other municipality as 5 years ago" & com_ori_urb=="Urban")%>%
  group_by(com)%>%
  summarise(mig_urb92_com=n())

pop02_com<-censo02%>%
  filter(com_res=="Same municipality of residence")%>%
  group_by(com)%>%
  summarise(pop02_com=n())
mig_tot02_com<-censo02%>%
  filter(com_res=="Same municipality of residence" & com_ori!="Under 5" & com_ori!="Same municipality as 5 years ago")%>%
  group_by(com)%>%
  summarise(mig_tot02_com=n())
mig_urb02_com<-censo02%>%
  filter(com_res=="Same municipality of residence" & com_ori=="Other municipality as 5 years ago" & com_ori_urb=="Urban")%>%
  group_by(com)%>%
  summarise(mig_urb02_com=n())

pop17_com<-censo17%>%
  filter(com_res=="Same municipality of residence")%>%
  group_by(com)%>%
  summarise(pop17_com=n())
mig_tot17_com<-censo17%>%
  filter(com_res=="Same municipality of residence" & com_ori!="Under 5" & com_ori!="Same municipality as 5 years ago")%>%
  group_by(com)%>%
  summarise(mig_tot17_com=n())
mig_urb17_com<-censo17%>%
  filter(com_res=="Same municipality of residence" & com_ori=="Other municipality as 5 years ago" & com_ori_urb=="Urban")%>%
  group_by(com)%>%
  summarise(mig_urb17_com=n())

pop_mig_9202<-pop92_com%>%
  left_join(y=mig_tot92_com, by="com")%>%
  left_join(y=mig_urb92_com, by="com")%>%
  left_join(y=pop02_com, by="com")%>%
  left_join(y=mig_tot02_com, by="com")%>%
  left_join(y=mig_urb02_com, by="com")%>%
  left_join(y=dpa_shp, by="com")%>%
  mutate(pct_mig_tot92_com=100*mig_tot92_com/pop92_com,
         pct_mig_urb92_com=100*mig_urb92_com/pop92_com,
         pct_mig_tot02_com=100*mig_tot02_com/pop02_com,
         pct_mig_urb02_com=100*mig_urb02_com/pop02_com,
         pct_cre_pop_9202_com=100*(pop02_com-pop92_com)/pop92_com,
         pct_cre_mig_tot_9202_com=100*mig_tot02_com/(pop02_com-pop92_com),
         pct_cre_mig_urb_9202_com=100*mig_urb02_com/(pop02_com-pop92_com))

pop_mig_9217<-pop92_com%>%
  left_join(y=mig_tot92_com, by="com")%>%
  left_join(y=mig_urb92_com, by="com")%>%
  left_join(y=pop17_com, by="com")%>%
  left_join(y=mig_tot17_com, by="com")%>%
  left_join(y=mig_urb17_com, by="com")%>%
  left_join(y=dpa_shp, by="com")%>%
  mutate(pct_mig_tot92_com=100*mig_tot92_com/pop92_com,
         pct_mig_urb92_com=100*mig_urb92_com/pop92_com,
         pct_mig_tot17_com=100*mig_tot17_com/pop17_com,
         pct_mig_urb17_com=100*mig_urb17_com/pop17_com,
         pop1792_com=pop17_com-pop92_com,
         pct_cre_pop_9217_com=100*(pop17_com-pop92_com)/pop92_com,
         pct_cre_mig_tot_9217_com=100*mig_tot17_com/(pop17_com-pop92_com),
         pct_cre_mig_urb_9217_com=100*mig_urb17_com/(pop17_com-pop92_com))

pop_mig_0217<-pop02_com%>%
  left_join(y=mig_tot02_com, by="com")%>%
  left_join(y=mig_urb02_com, by="com")%>%
  left_join(y=pop17_com, by="com")%>%
  left_join(y=mig_tot17_com, by="com")%>%
  left_join(y=mig_urb17_com, by="com")%>%
  left_join(y=dpa_shp, by="com")%>%
  mutate(pct_mig_tot02_com=100*mig_tot02_com/pop02_com,
         pct_mig_urb02_com=100*mig_urb02_com/pop02_com,
         pct_mig_tot17_com=100*mig_tot17_com/pop17_com,
         pct_mig_urb17_com=100*mig_urb17_com/pop17_com,
         pct_cre_pop_0217_com=100*(pop17_com-pop02_com)/pop02_com,
         pct_cre_mig_tot_0217_com=100*mig_tot17_com/(pop17_com-pop02_com),
         pct_cre_mig_urb_0217_com=100*mig_urb17_com/(pop17_com-pop02_com))

remove(pop92_com, mig_tot92_com, mig_urb92_com,
       pop02_com, mig_tot02_com, mig_urb02_com, 
       pop17_com, mig_tot17_com, mig_urb17_com)

#Gráfico de crecimiento poblacional y aporte de los migrantes urbanos extracomunales - versión artículo
fig3a<-ggplot()+
  geom_point(data=pop_mig_9202, aes(x=pct_mig_urb02_com, y=100*((pop02_com/pop92_com)^(1/10)-1), size=mig_urb02_com, color=urb_rec), alpha=0.7, show.legend = F)+
  geom_label_repel(data=filter(pop_mig_9202, mig_urb02_com>=900 | urb_rec=="Urban"), aes(x=pct_mig_urb02_com, y=100*((pop02_com/pop92_com)^(1/10)-1), color=urb_rec, label=nom_com), show.legend = F)+
  scale_size(name="Urban immigrants", breaks=c(500,1000,1500,2000,2500))+
  scale_color_manual(values=c("#440154FF", "#2A788EFF", "#7AD151FF"),  name="Municipality of residence")+
  scale_x_continuous(limits=c(0,17), breaks=c(4,8,12,16))+
  scale_y_continuous(limits=c(-7,4), breaks=c(-6,-3,0,3))+
  facet_wrap(vars(nom_reg))+
  labs(x="Immigration from urban municipalities between 1997-2002 (% of rural population)
       
       
       ", y="Annual change in rural population (%)", 
       subtitle="1992-2002")+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size=14))

fig3b<-ggplot()+
  geom_point(data=pop_mig_0217, aes(x=pct_mig_urb17_com, y=100*((pop17_com/pop02_com)^(1/15)-1), size=mig_urb17_com, color=urb_rec), alpha=0.7)+
  geom_label_repel(data=filter(pop_mig_0217, mig_urb17_com>=900 | urb_rec=="Urban"), aes(x=pct_mig_urb17_com, y=100*((pop17_com/pop02_com)^(1/15)-1), color=urb_rec, label=nom_com), show.legend = F)+
  scale_size(name="Urban immigrants", breaks=c(500,1000,1500,2000,2500))+
  scale_color_manual(values=c("#440154FF", "#2A788EFF", "#7AD151FF"),  name="Municipality of residence")+
  scale_x_continuous(limits=c(0,17), breaks=c(4,8,12,16))+
  scale_y_continuous(limits=c(-7,4), breaks=c(-6,-3,0,3))+
  facet_wrap(vars(nom_reg))+
  labs(x="Immigration from urban municipalities between 2012-2017 (% of rural population)", y="Annual change in rural population (%)", 
       subtitle="2002-2017")+
  theme_bw()+
  theme(legend.position = "bottom",
        text = element_text(size=14))

fig3<-grid.arrange(fig3a, fig3b, nrow=2, ncol=1)%>%
  ggsave(filename="Figure 3.jpg", width=30, height=25, units="cm", dpi=300)

# 4.4. Figure 4 -----------------------------------------------------------

#A database with summary frequencies of migration by type of municipality and region (same/other) for 1992, 2002, 2017
censo_mig<-censo92%>%
  filter(com_res=="Same municipality of residence" & mig_ori_reg!="Under 5" & mig_ori_reg!="Non-migrant 5 years ago" & mig_ori_reg!=is.na(mig_ori_reg))%>%
  group_by(com_res_urb, mig_ori_reg)%>%
  summarise(pop=n())%>%
  ungroup()%>%
  mutate(año="1992")%>%
  rbind(censo02%>%
          filter(com_res=="Same municipality of residence" & mig_ori_reg!="Under 5" & mig_ori_reg!="Non-migrant 5 years ago" & mig_ori_reg!=is.na(mig_ori_reg))%>%
          group_by(com_res_urb, mig_ori_reg)%>%
          summarise(pop=n())%>%
          ungroup()%>%
          mutate(año="2002"))%>%
  rbind(censo17%>%
          filter(com_res=="Same municipality of residence" & mig_ori_reg!="Under 5" & mig_ori_reg!="Non-migrant 5 years ago" & mig_ori_reg!=is.na(mig_ori_reg))%>%
          group_by(com_res_urb, mig_ori_reg)%>%
          summarise(pop=n())%>%
          ungroup()%>%
          mutate(año="2017"))
censo_mig_tot<-censo_mig%>%
  group_by(mig_ori_reg, año)%>%
  summarise(pop=sum(pop, na.rm=T))%>%
  mutate(com_res_urb="Total")
censo_mig<-censo_mig%>%
  bind_rows(censo_mig_tot)%>%
  mutate(com_res_urb=factor(com_res_urb,
                            levels=c("Total", "Urban", "Mixed", "Rural")),
         mig_ori_reg=factor(case_when(mig_ori_reg=="Non-migrant 5 years ago" ~ "Non-migrant",
                                      mig_ori_reg=="5 years ago non-urban migrant, same region" ~ "Mixed or rural\nsame region",
                                      mig_ori_reg=="5 years ago non-urban migrant, a different region" ~ "Mixed or rural\na different region",
                                      mig_ori_reg=="5 years ago urban migrant, same region" ~ "Urban\nsame region",
                                      mig_ori_reg=="5 years ago urban migrant, a different region" ~ "Urban\na different region",
                                      mig_ori_reg=="5 years ago migrant, foreign" ~ "Foreign migrant"),
                            levels=c("Mixed or rural\nsame region", "Mixed or rural\na different region", "Urban\nsame region", "Urban\na different region", "Foreign migrant", "Non-migrant")))%>%
  group_by(com_res_urb, mig_ori_reg, año)%>%
  summarise(pop=sum(pop, na.rm=T))
remove(censo_mig_tot)

#The figure
fig4<-ggplot(data=filter(censo_mig, año!="Total" & com_res_urb!="Total" & mig_ori_reg!= "Foreign migrant" & mig_ori_reg!= "5 years ago migrant, foreign"), aes(fill=com_res_urb, x=mig_ori_reg, y=pop))+
  geom_bar(stat="identity")+
  geom_label(data=filter(censo_mig, com_res_urb!="Total" & mig_ori_reg!= "Foreign migrant" & mig_ori_reg!= "5 years ago migrant, foreign"), aes(label=pop, x=mig_ori_reg, y=pop, fill=com_res_urb), 
             color="white",  size=4, show.legend = F, position = position_stack(vjust = 0.5))+
  scale_fill_viridis_d(name="Municipality of residence", breaks=c("Urban", "Mixed", "Rural"))+
  facet_grid(cols=vars(año))+
  geom_text(data=filter(censo_mig, com_res_urb=="Total" & mig_ori_reg!= "Foreign migrant" & mig_ori_reg!= "5 years ago migrant, foreign"), aes(label=pop, x=mig_ori_reg, y=pop+2000), size=5)+
  theme_light()+
  labs(x="
       Origin of migrants", y="")+
  theme(legend.position = "bottom", text = element_text(size=12), axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5))

ggsave(filename="Figure 4.jpg", width=30, height=25, units="cm", dpi=300)

# 4.5. Figure 5 -----------------------------------------------------------

#A database at municipal level 
mig17_com<-dpa_shp%>%
  filter(reg==9 | reg==10 | reg==14)%>%
  left_join(y=(censo17%>%
      filter(com_res=="Same municipality of residence" & com_ori_urb=="Urban")%>%
      group_by(com, nom_com, com_ori_urb)%>%
      summarise(mig=n())%>%
      filter(mig>0)), 
            by=c("com", "nom_com"))%>%
  filter(mig!=is.na(mig))

#The figure
fig5a<-ggplot()+
  geom_sf(data=mig17_com, aes(fill=mig/1000), alpha=0.8, color="transparent")+
  scale_fill_viridis_b(breaks=c(1,5,10,15), na.value="transparent", direction=-1,
                       name="Immigrants from urban municipalities (000s)")+
  geom_sf(data=stu_are, fill="transparent", color="gray80")+
  geom_sf(data=red, aes(color=rou, alpha=rou_alpha))+
  geom_sf(data=filter(st_centroid(urb17), pop>=30000 & nom!="Alerce"), aes(color="Cities (over 30,000)"), size=5, shape=21, fill="white")+
  geom_sf_label_repel(data=filter(urb17,pop>=30000 & nom!="Alerce" & nom!="Temuco- Padre Las Casas"), aes(label=nom))+
  scale_color_manual(values = colors, name="")+
  scale_alpha(guide="none")+
  labs(subtitle="Municipality of residence",
       caption="
       ",
       x="", y="")+
  theme_minimal()+
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin())+
  annotation_scale(location = "bl", style="bar", unit_category="metric") +
  annotation_north_arrow(location = "tr", which_north= "true")

#A database at district level 
mig17_loc<-loc17_shp%>%
  left_join(y=(censo17%>%
              filter(com_res=="Same municipality of residence" & 
                       com_ori=="Other municipality as 5 years ago" & com_ori_urb=="Urban")%>%
              group_by(nom_com, com_ori_urb, geocode)%>%
              summarise(mig=n())%>%
              filter(mig>0)),
            by="geocode")%>%
  filter(mig!=is.na(mig))

fig5b<-ggplot()+
  geom_sf(data=mig17_loc, aes(fill=mig), alpha=0.8, color="transparent")+
  scale_fill_viridis_b(breaks=c(50,100,150,200), na.value = "transparent", direction=-1,
                       name="Immigrants from urban municipalities")+
  geom_sf(data=stu_are, fill="transparent", color="gray80")+
  geom_sf(data=red, aes(color=rou, alpha=rou_alpha))+
  geom_sf(data=filter(st_centroid(urb17), pop>=30000 & nom!="Alerce"), aes(color="Cities (over 30,000)"), size=5, shape=21, fill="white")+
  geom_sf_label_repel(data=filter(urb17,pop>=30000 & nom!="Alerce" & nom!="Temuco- Padre Las Casas"), aes(label=nom))+
  scale_color_manual(values = colors, name="")+
  scale_alpha(guide="none")+
  labs(subtitle="District of residence", x="", y="", caption="
       Projection system: UTM, Datum WGS84, Zone 18 South.")+
  theme_minimal()+
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin())+
  annotation_scale(location = "bl", style="bar", unit_category="metric") +
  annotation_north_arrow(location = "tr", which_north= "true")
  
fig5<-grid.arrange(fig5a, fig5b, nrow=1)%>%
  ggsave(filename="Figure 5.jpg", width=25, height=25, units="cm", dpi=300)

# 4.6. Figure 6 -----------------------------------------------------------

###This figure is a static version of three Sankey graphs, which are originally conceived as interactive objetcts. 
#Export and merged was done manually (from the Viewer pane and "Export"), after changing left size categories position also manually.

#Color scale for figures
ColourScal <-'d3.scaleOrdinal() .range(["#21918c", "#21918c","#fde725", "#465090"])'

#1992
censo92_flu<-censo92%>%
  mutate(nom_com_ori=case_when(reg_ori==13 ~ "Metropolitan region",
                               reg_ori!=9 & reg_ori!= 10 & reg_ori!= 14 & reg_ori!=13 ~ "Rest of Chile",
                               reg_ori==9 ~ "La Araucanía",
                               reg_ori==10 ~ "Los Lagos",
                               reg_ori==14 ~ "Los Ríos"),
         reg_cat=case_when(reg==9 ~ "La Araucanía",
                           reg==10 ~ "Los Lagos",
                           reg==14 ~ "Los Ríos"))%>%
  filter(com_res=="Same municipality of residence" & com_ori_urb=="Urban" & mig_ori=="5 years ago migrant, urban municipality")%>%
  group_by(nom_com, nom_com_ori, reg_cat)%>%
  summarise(mig=n())%>%
  #filter(mig>=100)%>%
  as.data.frame()%>%
  left_join(y=(censo92%>%
                 filter(com_res=="Same municipality of residence")%>%
                 mutate(mig=case_when(com_ori=="Other municipality as 5 years ago" & com_ori_urb=="Urban" ~ 1,
                                      com_ori=="Same municipality as 5 years ago" |
                                        (com_ori=="Other municipality as 5 years ago" & com_ori_urb!="Urban") ~ 0))%>%
                 group_by(reg_res, com_res_nom)%>%
                 summarise(n_mig=sum(mig, na.rm=T))%>%
                 ungroup()%>%
                 select(com_res_nom, n_mig)%>%
                 mutate(rank=rank(-n_mig))), 
            by=join_by("nom_com"=="com_res_nom"))%>%
  filter(rank<=10)

censo92_flu_nodes<-data.frame(node=c(as.character(censo92_flu$nom_com), as.character(censo92_flu$nom_com_ori))%>%
                                unique())%>%
  left_join(y=(censo92_flu%>%
                 group_by(nom_com, reg_cat)%>%
                 summarise(n=n())),
            by=join_by("node"=="nom_com"))%>%
  mutate(reg_cat=factor(case_when(node=="Metropolitan region" ~ "Metropolitan region",
                                  node=="Rest of Chile" ~ "Rest of Chile",
                                  node!="Metropolitan region" & node!="Rest of Chile" ~ reg_cat)))
censo92_flu$IDori<-match(censo92_flu$nom_com_ori, censo92_flu_nodes$node)-1
censo92_flu$IDdes<-match(censo92_flu$nom_com, censo92_flu_nodes$node)-1
censo92_flu<-censo92_flu%>%
  mutate(orides=case_when(nom_com==nom_com_ori ~ 1,
                          nom_com!=nom_com_ori ~ 0))%>%
  filter(orides==0)%>%
  select(-orides)

#The figure
fig6a<-sankeyNetwork(Links = censo92_flu, Nodes = censo92_flu_nodes,
              Source = "IDori", Target = "IDdes",
              Value = "mig", NodeID = "node", 
              fontFamily = "sans-serif", fontSize=16,
              sinksRight=F, colourScale=ColourScal, 
              nodeWidth=10,  nodePadding=15,
              LinkGroup ="nom_com_ori", 
              NodeGroup = "reg_cat")

###2002
censo02_flu<-censo02%>%
  mutate(nom_com_ori=case_when(reg_ori==13 ~ "Metropolitan region",
                               reg_ori!=9 & reg_ori!= 10 & reg_ori!= 14 & reg_ori!=13 ~ "Rest of Chile",
                               reg_ori==9 ~ "La Araucanía",
                               reg_ori==10 ~ "Los Lagos",
                               reg_ori==14 ~ "Los Ríos"),
         reg_cat=case_when(reg==9 ~ "La Araucanía",
                           reg==10 ~ "Los Lagos",
                           reg==14 ~ "Los Ríos"))%>%
  filter(com_res=="Same municipality of residence" & com_ori_urb=="Urban" & mig_ori=="5 years ago migrant, urban municipality")%>%
  group_by(nom_com, nom_com_ori, reg_cat)%>%
  summarise(mig=n())%>%
  #filter(mig>=100)%>%
  as.data.frame()%>%
  left_join(y=(censo02%>%
                 filter(com_res=="Same municipality of residence")%>%
                 mutate(mig=case_when(com_ori=="Other municipality as 5 years ago" & com_ori_urb=="Urban" ~ 1,
                                      com_ori=="Same municipality as 5 years ago" |
                                        (com_ori=="Other municipality as 5 years ago" & com_ori_urb!="Urban") ~ 0))%>%
                 group_by(reg_res, com_res_nom)%>%
                 summarise(n_mig=sum(mig, na.rm=T))%>%
                 ungroup()%>%
                 select(com_res_nom, n_mig)%>%
                 mutate(rank=rank(-n_mig))), 
            by=join_by("nom_com"=="com_res_nom"))%>%
  filter(rank<=10)

censo02_flu_nodes<-data.frame(node=c(as.character(censo02_flu$nom_com), as.character(censo02_flu$nom_com_ori))%>%
                                unique())%>%
  left_join(y=(censo02_flu%>%
                 group_by(nom_com, reg_cat)%>%
                 summarise(n=n())),
            by=join_by("node"=="nom_com"))%>%
  mutate(reg_cat=factor(case_when(node=="Metropolitan region" ~ "Metropolitan region",
                                  node=="Rest of Chile" ~ "Rest of Chile",
                                  node!="Metropolitan region" & node!="Rest of Chile" ~ reg_cat)))
censo02_flu$IDori<-match(censo02_flu$nom_com_ori, censo02_flu_nodes$node)-1
censo02_flu$IDdes<-match(censo02_flu$nom_com, censo02_flu_nodes$node)-1
censo02_flu<-censo02_flu%>%
  mutate(orides=case_when(nom_com==nom_com_ori ~ 1,
                          nom_com!=nom_com_ori ~ 0))%>%
  filter(orides==0)%>%
  select(-orides)

#The figure
fig6b<-sankeyNetwork(Links = censo02_flu, Nodes = censo02_flu_nodes,
              Source = "IDori", Target = "IDdes",
              Value = "mig", NodeID = "node", 
              fontFamily = "sans-serif", fontSize=16,
              sinksRight=F, colourScale=ColourScal, 
              nodeWidth=10,  nodePadding=15,
              LinkGroup ="nom_com_ori", 
              NodeGroup = "reg_cat")

###2017
censo17_flu<-censo17%>%
  mutate(nom_com_ori=case_when(reg_ori==13 ~ "Metropolitan region",
                               reg_ori!=9 & reg_ori!= 10 & reg_ori!= 14 & reg_ori!=13 ~ "Rest of Chile",
                               reg_ori==9 ~ "La Araucanía",
                               reg_ori==10 ~ "Los Lagos",
                               reg_ori==14 ~ "Los Ríos"),
         reg_cat=case_when(reg==9 ~ "La Araucanía",
                           reg==10 ~ "Los Lagos",
                           reg==14 ~ "Los Ríos"))%>%
  filter(com_res=="Same municipality of residence" & com_ori_urb=="Urban" & mig_ori=="5 years ago migrant, urban municipality")%>%
  group_by(nom_com, nom_com_ori, reg_cat)%>%
  summarise(mig=n())%>%
  #filter(mig>=100)%>%
  as.data.frame()%>%
  left_join(y=(censo17%>%
                 filter(com_res=="Same municipality of residence")%>%
                 mutate(mig=case_when(com_ori=="Other municipality as 5 years ago" & com_ori_urb=="Urban" ~ 1,
                                      com_ori=="Same municipality as 5 years ago" |
                                        (com_ori=="Other municipality as 5 years ago" & com_ori_urb!="Urban") ~ 0))%>%
                 group_by(reg_res, com_res_nom)%>%
                 summarise(n_mig=sum(mig, na.rm=T))%>%
                 ungroup()%>%
                 select(com_res_nom, n_mig)%>%
                 mutate(rank=rank(-n_mig))), 
            by=join_by("nom_com"=="com_res_nom"))%>%
  filter(rank<=10)

censo17_flu_nodes<-data.frame(node=c(as.character(censo17_flu$nom_com), as.character(censo17_flu$nom_com_ori))%>%
                                unique())%>%
  left_join(y=(censo17_flu%>%
                 group_by(nom_com, reg_cat)%>%
                 summarise(n=n())),
            by=join_by("node"=="nom_com"))%>%
  mutate(reg_cat=factor(case_when(node=="Metropolitan region" ~ "Metropolitan region",
                                  node=="Rest of Chile" ~ "Rest of Chile",
                                  node!="Metropolitan region" & node!="Rest of Chile" ~ reg_cat)))
censo17_flu$IDori<-match(censo17_flu$nom_com_ori, censo17_flu_nodes$node)-1
censo17_flu$IDdes<-match(censo17_flu$nom_com, censo17_flu_nodes$node)-1
censo17_flu<-censo17_flu%>%
  mutate(orides=case_when(nom_com==nom_com_ori ~ 1,
                          nom_com!=nom_com_ori ~ 0))%>%
  filter(orides==0)%>%
  select(-orides)

#The figure
fig6c<-sankeyNetwork(Links = censo17_flu, Nodes = censo17_flu_nodes,
              Source = "IDori", Target = "IDdes",
              Value = "mig", NodeID = "node", 
              fontFamily = "sans-serif", fontSize=16,
              sinksRight=F, colourScale=ColourScal, 
              nodeWidth=10,  nodePadding=15,
              LinkGroup ="nom_com_ori", 
              NodeGroup = "reg_cat")


# 4.7. Table 2 ------------------------------------------------------------

###This table is made of 4 tables merged (1992 and 2017/Study area and urban rest of Chile)

tab2a<-censo92%>%
  filter(edad>=5 & com_res=="Same municipality of residence" & mig_ori_reg!="Under 5")%>% 
  mutate(mig_ori_reg=factor(case_when(mig_ori_reg=="Non-migrant 5 years ago" ~ "Non-migrant",
                                      mig_ori_reg=="5 years ago non-urban migrant, same region" | mig_ori_reg=="5 years ago non-urban migrant, a different region" ~ "Non-urban migrant",
                                      mig_ori_reg=="5 years ago urban migrant, same region" ~ "Urban migrant - same region",
                                      mig_ori_reg=="5 years ago urban migrant, a different region" ~ "Urban migrant - different region",
                                      mig_ori_reg=="5 years ago migrant, foreign" ~ "Foreign migrant"),
                            levels=c("Non-migrant", "Non-urban migrant", "Urban migrant - same region", "Urban migrant - different region", "Foreign migrant")))%>%
  select(sex, edad, edad_rec, esc_rec, pue, mig_ori_reg, sfdt, rama)%>%
  tbl_summary(by=mig_ori_reg,
              label=list(sex ~ "Sex: Female",
                         edad ~ "Age",
                         edad_rec ~ "Age group",
                         esc_rec ~ "Educational level (≥ 25 years)",
                         pue ~ "Indigenous people",
                         sfdt ~ "Labor force status",
                         rama ~ "Economic activity (broad sector)"),
              type = list(sex ~ "dichotomous",
                          edad ~ "continuous",
                          edad_rec ~ "categorical",
                          esc_rec ~ "categorical",
                          pue ~ "dichotomous",
                          sfdt ~"categorical",
                          rama ~ "categorical"),
              value = list(sex ~ "Female",
                           pue ~ "Indigenous"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{p}%",
                               all_dichotomous() ~ "{p}%"),
              digits = list(all_continuous() ~ c(1, 1),
                            all_categorical() ~ c(1, 1),
                            all_dichotomous() ~ c(1, 1)),
              missing="no")%>%
  bold_labels()%>%
  modify_column_hide(columns = c("stat_2", "stat_5"))
  
tab2b<-censo92_full%>%
  filter(area==1 & edad>=5 & reg_cat=="Rest of urban Chile")%>% 
  select(reg_cat, sex, edad, edad_rec, esc_rec, pue, sfdt, rama)%>%
  tbl_summary(by=reg_cat,
              label=list(sex ~ "Sex: Female",
                         edad ~ "Age",
                         edad_rec ~ "Age group",
                         esc_rec ~ "Educational level (≥ 25 years)",
                         pue ~ "Indigenous people",
                         sfdt ~ "Labor force status",
                         rama ~ "Economic activity (broad sector)"),
              type = list(sex ~ "dichotomous",
                          edad ~ "continuous",
                          edad_rec ~ "categorical",
                          esc_rec ~ "categorical",
                          pue ~ "dichotomous",
                          sfdt ~"categorical",
                          rama ~ "categorical"),
              value = list(sex ~ "Female",
                           pue ~ "Indigenous"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{p}%",
                               all_dichotomous() ~ "{p}%"),
              digits = list(all_continuous() ~ c(1, 1),
                            all_categorical() ~ c(1, 1),
                            all_dichotomous() ~ c(1, 1)),
              missing="no")%>%
  bold_labels()%>%
  modify_column_hide(columns = "stat_1")

tab2c<-censo17%>%
  filter(edad>=5 & com_res=="Same municipality of residence" & mig_ori_reg!="Under 5")%>% 
  mutate(mig_ori_reg=factor(case_when(mig_ori_reg=="Non-migrant 5 years ago" ~ "Non-migrant",
                                      mig_ori_reg=="5 years ago non-urban migrant, same region" | mig_ori_reg=="5 years ago non-urban migrant, a different region" ~ "Non-urban migrant",
                                      mig_ori_reg=="5 years ago urban migrant, same region" ~ "Urban migrant - same region",
                                      mig_ori_reg=="5 years ago urban migrant, a different region" ~ "Urban migrant - different region",
                                      mig_ori_reg=="5 years ago migrant, foreign" ~ "Foreign migrant"),
                            levels=c("Non-migrant", "Non-urban migrant", "Urban migrant - same region", "Urban migrant - different region", "Foreign migrant")))%>%
  select(sex, edad, edad_rec, esc_rec, pue, mig_ori_reg, sfdt, rama)%>%
  tbl_summary(by=mig_ori_reg,
              label=list(sex ~ "Sex: Female",
                         edad ~ "Age",
                         edad_rec ~ "Age group",
                         esc_rec ~ "Educational level (≥ 25 years)",
                         pue ~ "Indigenous people",
                         sfdt ~ "Labor force status",
                         rama ~ "Economic activity (broad sector)"),
              type = list(sex ~ "dichotomous",
                          edad ~ "continuous",
                          edad_rec ~ "categorical",
                          esc_rec ~ "categorical",
                          pue ~ "dichotomous",
                          sfdt ~"categorical",
                          rama ~ "categorical"),
              value = list(sex ~ "Female",
                           pue ~ "Indigenous"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{p}%",
                               all_dichotomous() ~ "{p}%"),
              digits = list(all_continuous() ~ c(1, 1),
                            all_categorical() ~ c(1, 1),
                            all_dichotomous() ~ c(1, 1)),
              missing="no")%>%
  bold_labels()%>%
  modify_column_hide(columns = c("stat_2", "stat_5"))

tab2d<-censo17_full%>%
  filter(area==1 & edad>=5 & reg_cat=="Rest of urba Chile")%>% 
  select(reg_cat, sex, edad, edad_rec, esc_rec, pue, sfdt, rama)%>%
  tbl_summary(by=reg_cat,
              label=list(sex ~ "Sex: Female",
                         edad ~ "Age",
                         edad_rec ~ "Age group",
                         esc_rec ~ "Educational level (≥ 25 years)",
                         pue ~ "Indigenous people",
                         sfdt ~ "Labor force status",
                         rama ~ "Economic activity (broad sector)"),
              type = list(sex ~ "dichotomous",
                          edad ~ "continuous",
                          edad_rec ~ "categorical",
                          esc_rec ~ "categorical",
                          pue ~ "dichotomous",
                          sfdt ~"categorical",
                          rama ~ "categorical"),
              value = list(sex ~ "Female",
                           pue ~ "Indigenous"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{p}%",
                               all_dichotomous() ~ "{p}%"),
              digits = list(all_continuous() ~ c(1, 1),
                            all_categorical() ~ c(1, 1),
                            all_dichotomous() ~ c(1, 1)),
              missing="no")%>%
  bold_labels()%>%
  modify_column_hide(columns = "stat_1")

#The (merged) table

tab2<-tbl_merge(tbls=list(tab2a, tab2b, tab2c, tab2d), tab_spanner=c("**1992**", "", "**2017**", ""))%>%
  as_gt()%>%
  gtsave("Table 2.docx")
