#*******************************************************************************************************************
#
# 0. Identification ---------------------------------------------------
# Title: Data preparation for EDUMER Panel Survey Studentes
# Author: Andreas Laffert            
# Overview: EDUMER Panel Survey Students       
# Date: 29-10-2024            
#
#******************************************************************************************************************

# 1. Packages ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               sjlabelled, 
               sjmisc, 
               rio,
               here, 
               haven,
               panelr,
               glue)

options(scipen=999)
rm(list = ls())

# 2. Data --------------------------------------------------------------

load(file = here("output/data/db_proc_students.RData"))
load(file = here("output/data/db_proc_students_w02.RData"))

w01 <- db_students
w02 <- db_students_w02

rm(db_students, db_students_w02)

# 3. Processing -----------------------------------------------------------

## *****************
## 3.1 Wide format 
## *****************

# names and sub fix wave

w01_wide <- w01 %>% 
  select(-fecha) %>% 
  rename_with(.cols = everything(), .fn = ~ paste0(., "_w01")) %>% 
  as_tibble()

names(w01_wide)

## standardize names wave 2

w02 <- w02 %>%
  select(-d1_o2, -sbj_num_o1) %>% 
  rename(
    d3_def_w01_V = d3_def_o1,
    check_atencion = p10_check_atencion_o2 ,
    p17_1 = p11_1_o2 ,
    p17_2 = p11_2_o2,
    p19 = p12_o2,
    p20 = p14_o2,
    p21_mes = p15_mes_o2,
    p21_ano = p15_ano_o2,
    p25 = p16_o2,
    p26 = p17_o2,
    p27  = p18_o2,
    p30  = p19_o2,
    )


w02_wide <- w02 %>%
  rename(id_estudiante_w02 = id_estudiante,
         matched_sbj_num_w01 = matched_sbj_num) %>%
  rename_with(~ ifelse(!str_detect(., "_w01$|_w02$|_o1$|_o2$"), paste0(., "_w02"), .), .cols = everything()) %>%
  rename_with(~ str_replace_all(., c("o2" = "w02", "o1" = "w01")), .cols = everything()) %>%
  as_tibble()

names(w02_wide)

# id_estudiante

sum(duplicated(w01_wide$id_estudiante_w01))

sum(duplicated(w02_wide$id_estudiante_w02))

sum(duplicated(w02_wide$matched_sbj_num_w01)) # only NA


# merge
key_match <- c("id_estudiante_w01" = "matched_sbj_num_w01")

names(w01_wide)
names(w02_wide)

edumer_students_wide <- full_join(
  w01_wide,
  w02_wide,
  by = key_match) 

nombres_ordenados <- colnames(edumer_students_wide) %>%
  str_sort(numeric = TRUE)

edumer_students_wide <- edumer_students_wide %>%
  select(all_of(nombres_ordenados))

names(edumer_students_wide)

edumer_students_wide <- edumer_students_wide %>% 
  select(id_estudiante = id_estudiante_w01,
         starts_with("consentimiento_"),
         starts_with("d2_"),
         starts_with("d3_"),
         starts_with("nivel_"),
         asignacion_w01,
         tratamiento_w01,
         control_w01,
         starts_with("exp"),
         starts_with("p1_"),
         starts_with("p2_"),
         starts_with("p3_"),
         starts_with("p4_"),
         starts_with("p5_"),
         starts_with("p6_"),
         starts_with("p7_"),
         starts_with("p8_"),
         starts_with("p9_"),
         starts_with("p10_"),
         starts_with("p11_"),
         starts_with("p12_"),
         starts_with("p13_"),
         starts_with("p14_"),
         starts_with("p15_"),
         starts_with("p16_"),
         s_115_6_w01,
         starts_with("p17_"),
         starts_with("p18_"),
         starts_with("p19_"),
         starts_with("p20_"),
         starts_with("p21_"),
         starts_with("p22_"),
         starts_with("p23_"),
         starts_with("p24_"),
         starts_with("p25_"),
         starts_with("p26_"),
         starts_with("p27_"),
         starts_with("p28_"),
         starts_with("p29_"),
         starts_with("p30_"))

# otorgar un id aleatorio a los 5 ids NA

l_id <- max(edumer_students_wide$id_estudiante, na.rm = T)
m_id <- 999999999

sum(is.na(edumer_students_wide$id_estudiante))

ids_aleatorios <- sample((l_id + 1):m_id, 5)

edumer_students_wide$id_estudiante[is.na(edumer_students_wide$id_estudiante)] <- ids_aleatorios

sum(duplicated(edumer_students_wide$id_estudiante)) # ok

## *****************
## 3.2 Long format 
## *****************

diccionario_etiquetas <- sapply(edumer_students_wide, attr, "label")
names(diccionario_etiquetas) <- sub("_w0[1-2]$", "", names(diccionario_etiquetas))

edumer_students_long <- edumer_students_wide %>%
  pivot_longer(
    cols = -id_estudiante,
    names_pattern = "(.*)(_w01|_w02)$",
    names_to = c(".value", "ola"),
    values_drop_na = T
  )

for (var in names(diccionario_etiquetas)) {
  if (!is.null(diccionario_etiquetas[[var]]) && var %in% names(edumer_students_long)) {
    attr(edumer_students_long[[var]], "label") <- diccionario_etiquetas[[var]]
  }
}




## Etiquetar

labels_or <- sjlabelled::get_labels(edumer_students_wide, values = T, non.labelled = T, drop.na = T)

names(edumer_students_long)

edumer_students_long$ola <- car::recode(edumer_students_long$ola, 
                                        recodes = c("'_w01' = 1; '_w02' = 2"),
                                        levels = 1:2,
                                        as.factor = T)

edumer_students_long$ola <- sjlabelled::set_labels(edumer_students_long$ola,
                                                   labels = c("Ola 1" = 1, "Ola 2" = 2))

edumer_students_long$consentimiento <- sjlabelled::copy_labels(edumer_students_long$consentimiento, edumer_students_wide$consentimiento_w01)

edumer_students_long$consentimiento <- sjlabelled::copy_labels(edumer_students_long$consentimiento, edumer_students_wide$consentimiento_w01)


# 4. Save and export ------------------------------------------------------

base::save(edumer_students_long, file = here("output/data/edumer_students_long.RData"))
haven::write_dta(edumer_students_long, path = here("output/data/edumer_students_long.dta"))
haven::write_sav(edumer_students_long, path = here("output/data/edumer_students_long.sav"))

base::save(edumer_students_wide, file = here("output/data/edumer_students_wide.RData"))
haven::write_dta(edumer_students_wide, path = here("output/data/edumer_students_wide.dta"))
haven::write_sav(edumer_students_wide, path = here("output/data/edumer_students_wide.sav"))

rm(list = ls(pattern = "^labels[0-9]+$"))
