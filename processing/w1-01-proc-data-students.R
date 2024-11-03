#*******************************************************************************************************************
#
# 0. Identification ---------------------------------------------------
# Title: Data preparation for EDUMER Studentes Survey Wave 1
# Author: Andreas Laffert            
# Overview: EDUMER Students Survey Wave 1         
# Date: 13-066-2024            
#
#******************************************************************************************************************

# 1. Packages ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               sjlabelled, 
               sjmisc, 
               rio,
               here, 
               codebook,
               haven)

options(scipen=999)
rm(list = ls())

# 2. Data --------------------------------------------------------------

db <- rio::import(file = here("input","data", "original", "310524_BDD_edumer.sav")) %>% 
  as_tibble()

names(db)

db_or_label <- sjlabelled::get_label(db)

# 3. Processing -----------------------------------------------------------

clean_labels <- function(x) {
  current_label <- sjlabelled::get_label(x)
  if (!is.null(current_label)) {
    new_label <- stringr::str_remove(current_label, "^\\d+\\.\\d+\\s|^\\d+\\.\\s")
    x <- sjlabelled::set_label(x, label = new_label)
  }
  return(x)
}


# Select ----
db1 <- db[,c(1:105)] %>% 
  select(-starts_with("T_d")) # only students variables and eliminate names


# Colnames ----
names(db1)

db1 <- db1 %>% 
  rename(id_estudiante = SbjNum,
         fecha = Date,
         consentimiento = ACEPTA,
         nivel_estudiante = Nivel_def,
         asignacion = aleatorio)

names(db1)
glimpse(db1)

names(which(colSums(is.na(db1)) > 0))

# Manipulation ----

## ID 
frq(db1$id_estudiante)
get_label(db1$id_estudiante)
db1$id_estudiante <- sjlabelled::set_label(db1$id_estudiante, label = "Identificador único estudiante")

## Fecha
frq(db1$fecha)
class(db1$fecha)
db1$fecha <- lubridate::as_datetime(db1$fecha)
get_label(db1$fecha)

## Consentimiento
frq(db1$consentimiento)
get_label(db1$consentimiento)
db1$consentimiento <- sjlabelled::set_label(db1$consentimiento, label = "Autorización adulto para participación de menor de edad en encuesta")
get_labels(db1$consentimiento)

## d2
frq(db1$d2)
get_label(db1$d2)
db1$d2 <- sjlabelled::set_label(db1$d2, label = "Establecimiento eduacional")
get_labels(db1$d2)

## d3_def
frq(db1$d3_def)
get_label(db1$d3_def)
db1$d3_def <- factor(tolower(db1$d3_def), 
                       levels = c("6a", "6b", "6c", "7a", "7b", 
                                  "1a", "1b", "1c", "2a", "2b", "2c"),
                       labels = c("6a", "6b", "6c", "7a", "7b", 
                                  "1a", "1b", "1c", "2a", "2b", "2c"))

db1$d3_def <- sjlabelled::set_label(db1$d3_def, label = "Curso al que pertenece")

## nivel_estudiante
frq(db1$nivel_estudiante)
get_label(db1$nivel_estudiante)
db1$nivel_estudiante <- sjlabelled::set_label(db1$nivel_estudiante, label = "Nivel del estudiante")

## asignacion
frq(db1$asignacion)
get_label(db1$asignacion)
db1 <- db1 %>%
  mutate(asignacion = asignacion %>%
           set_labels(labels = c("Tratamiento" = 1, "Control" = 2)) %>%
           set_label(label = "Identificador asignación aleatoria al tratamiento"))


## tratamiento
frq(db1$tratamiento) # OK: Alto NA
get_label(db1$tratamiento)

## control
frq(db1$control) # OK: Alto NA
get_label(db1$control)

## p1_

db1 %>% 
  select(starts_with("p1_")) %>% 
  frq(.)

labels1 <- c("Muy en desacuerdo" = 1, 
             "En desacuerdo" = 2, 
             "De acuerdo" = 3, 
             "Muy de acuerdo" = 4, 
             "No sabe" = 88, 
             "No responde" = 99)

db1 <- db1 %>% 
  mutate(across(.cols = starts_with("p1_"),.fns = ~ sjlabelled::set_labels(., labels = labels1)))

db1 %>% 
  select(starts_with("p1_")) %>% 
  get_label()

db1$p1_1 <- sjlabelled::set_label(db1$p1_1, 
                                  label = "En Chile, las personas son recompensadas por sus esfuerzos")

db1$p1_2 <- sjlabelled::set_label(db1$p1_2, 
                                  label = "En Chile, las personas son recompensadas por su inteligencia y habilidad")

db1$p1_3 <- sjlabelled::set_label(db1$p1_3, 
                                  label = "En Chile, a quienes tienen padres ricos les va mucho mejor en la vida")

db1$p1_4 <- sjlabelled::set_label(db1$p1_4, 
                                  label = "En Chile, quienes tienen buenos contactos les va mejor en la vida")

db1$p1_5 <- sjlabelled::set_label(db1$p1_5, 
                                  label = "Quienes más se esfuerzan deberían obtener mayores recompensas que quienes se esfuerzan menos")

db1$p1_6 <- sjlabelled::set_label(db1$p1_6, 
                                  label = "Quienes poseen más talento deberían obtener mayores recompensas que quienes poseen menos talento")

db1$p1_7 <- sjlabelled::set_label(db1$p1_7, 
                                  label = "Está bien que quienes tienen padres ricos les vaya bien en la vida")

db1$p1_8 <- sjlabelled::set_label(db1$p1_8, 
                                  label = "Está bien que quienes tienen buenos contactos les vaya bien en la vida")

db1$p1_9 <- sjlabelled::set_label(db1$p1_9, 
                                  label = "En Chile, todas las personas tienen las mismas oportunidades para salir adelante")

db1$p1_10 <- sjlabelled::set_label(db1$p1_10, 
                                  label = "En Chile, todas las personas obtienen lo que merecen")

## p2_

db1 %>% 
  select(starts_with("p2_")) %>% 
  frq(.)

db1 <- db1 %>% 
  mutate(across(.cols = starts_with("p2_"),.fns = ~ sjlabelled::set_labels(., labels = labels1)))

db1 %>% 
  select(starts_with("p2_")) %>% 
  get_label()

db1$p2_1 <- sjlabelled::set_label(db1$p2_1, 
                                 label = "En esta escuela, quienes se esfuerzan obtienen buenas notas")

db1$p2_2 <- sjlabelled::set_label(db1$p2_2, 
                                 label = "En esta escuela, quienes son inteligentes obtienen buenas notas")

db1$p2_3 <- sjlabelled::set_label(db1$p2_3, 
                                  label = "En esta escuela, los/as estudiantes obtienen las notas que merecen")

## p3
frq(db1$p3)
get_label(db1$p3)

db1 <- db1 %>% 
  mutate(
    p3 = if_else(p3 == 0, 1, p3),
    p3 = sjlabelled::set_label(p3, label = "¿Qué es más importante para obtener buenas notas, el esfuerzo o la inteligencia? Escala 1 (esfuerzo) a 10 (inteligencia)")
  )

## p4
frq(db1$p4)
get_label(db1$p4)
db1$p4 <- clean_labels(db1$p4)

## p5
frq(db1$p5)
db1$p5 <- sjlabelled::set_labels(db1$p5, 
                                 labels = c("Menos de lo que merecía" = 1, 
                                          "Lo que merecía" = 2, 
                                          "Más de lo que merecía" = 3, 
                                          "No sabe" = 88, 
                                          "No responde" = 99))

get_label(db1$p5)
db1$p5 <- clean_labels(db1$p5)

## p6
frq(db1$p6)
get_label(db1$p6)

db1 %>% 
  mutate(incon_p6 = if_else(p5 %in% c(1,3) & is.na(p6), TRUE, FALSE)) %>% 
  tally(incon_p6) 

db1 %>% 
  mutate(incon_p6 = if_else(p5 == 2 & !is.na(p6), TRUE, FALSE)) %>% 
  tally(incon_p6) # ok

db1$p6 <- clean_labels(db1$p6)

## p7
frq(db1$p7)
db1$p7 <- sjlabelled::set_labels(db1$p7, 
                                 labels = c("Menos de lo que merezco" = 1, 
                                            "Lo que merezco" = 2, 
                                            "Más de lo que merezco" = 3, 
                                            "No sabe" = 88, 
                                            "No responde" = 99))
get_label(db1$p7)
db1$p7 <- clean_labels(db1$p7)

## p8_

db1 %>% 
  select(starts_with("p8_")) %>% 
  frq(.)

db1 <- db1 %>% 
  mutate(across(.cols = starts_with("p8_"),.fns = ~ sjlabelled::set_labels(., labels = labels1)))

db1 %>% 
  select(starts_with("p8_")) %>% 
  get_label()

db1$p8_1 <- clean_labels(db1$p8_1)
db1$p8_2 <- clean_labels(db1$p8_2)

## p9_

db1 %>% 
  select(starts_with("p9_")) %>% 
  frq(.)

db1 <- db1 %>% 
  mutate(across(.cols = starts_with("p9_"),.fns = ~ sjlabelled::set_labels(., labels = labels1)))

db1 %>% 
  select(starts_with("p9_")) %>% 
  get_label()

db1$p9_1 <- clean_labels(db1$p9_1)
db1$p9_2 <- clean_labels(db1$p9_2)
db1$p9_3 <- clean_labels(db1$p9_3)
db1$p9_4 <- clean_labels(db1$p9_4)
db1$p9_5 <- clean_labels(db1$p9_5)
db1$p9_6 <- clean_labels(db1$p9_6)

## check atencion
frq(db1$check_atencion)
db1$check_atencion <- sjlabelled::set_labels(db1$check_atencion, 
                                             labels = labels1)

get_label(db1$check_atencion)

## p10_

db1 %>% 
  select(starts_with("p10_")) %>% 
  frq(.)

labels2 <- c("Nada importante" = 1, 
             "Algo importante" = 2, 
             "Importante" = 3, 
             "Muy importante" = 4, 
             "No sabe" = 88, 
             "No responde" = 99)

db1 <- db1 %>% 
  mutate(across(.cols = starts_with("p10_"),.fns = ~ sjlabelled::set_labels(., labels = labels2)))

db1 %>% 
  select(starts_with("p10_")) %>% 
  get_label()

db1$p10_1 <- clean_labels(db1$p10_1)
db1$p10_2 <- clean_labels(db1$p10_2)
db1$p10_3 <- clean_labels(db1$p10_3)
db1$p10_4 <- clean_labels(db1$p10_4)
db1$p10_5 <- clean_labels(db1$p10_5)
db1$p10_6 <- clean_labels(db1$p10_6)
db1$p10_7 <- clean_labels(db1$p10_7)
db1$p10_8 <- clean_labels(db1$p10_8)

## p11_

db1 %>% 
  select(starts_with("p11_")) %>% 
  frq()

labels3 <- c("Seguro no haré esto" = 1, 
             "Tal vez haré esto" = 2, 
             "Probablemente haré esto" = 3, 
             "Seguro haré esto" = 4, 
             "No sabe" = 88, 
             "No responde" = 99)

db1 <- db1 %>% 
  mutate(across(.cols = starts_with("p11_"),.fns = ~ sjlabelled::set_labels(., labels = labels3)))

db1 %>% 
  select(starts_with("p11_")) %>% 
  get_label()

db1$p11_1 <- clean_labels(db1$p11_1)
db1$p11_2 <- clean_labels(db1$p11_2)
db1$p11_3 <- clean_labels(db1$p11_3)

## p12_

db1 %>% 
  select(starts_with("p12_")) %>% 
  frq(.)

db1 <- db1 %>% 
  mutate(
    across(.cols = starts_with("p12_"),
           .fns = ~ sjlabelled::set_labels(., labels = c("Sí" = 1,
                                                         "No" = 2,
                                                         "No responde" = 99)))
    )


db1 %>% 
  select(starts_with("p12_")) %>% 
  get_label()

db1$p12_1 <- clean_labels(db1$p12_1)
db1$p12_2 <- clean_labels(db1$p12_2)
db1$p12_3 <- clean_labels(db1$p12_3)

## p13_

db1 %>% 
  select(starts_with("p13_")) %>% 
  frq(.)

labels4 <- c("Nunca" = 1, 
             "Una vez al año" = 2, 
             "Una vez al mes" = 3, 
             "Semanalmente" = 4,
             "Todos los días" = 5,
             "No sabe" = 88, 
             "No responde" = 99)

db1 <- db1 %>% 
  mutate(across(.cols = starts_with("p13_"),.fns = ~ sjlabelled::set_labels(., labels = labels4)))

db1 %>% 
  select(starts_with("p13_")) %>% 
  get_label()

db1$p13_1 <- clean_labels(db1$p13_1)
db1$p13_2 <- clean_labels(db1$p13_2)
db1$p13_3 <- clean_labels(db1$p13_3)
db1$p13_4 <- clean_labels(db1$p13_4)
db1$p13_5 <- clean_labels(db1$p13_5)
db1$p13_6 <- clean_labels(db1$p13_6)

## p14
frq(db1$p14)
db1$p14 <- sjlabelled::set_labels(db1$p14, 
                                 labels = c("Papá" = 1, 
                                            "Mamá" = 2, 
                                            "Abuela" = 3, 
                                            "Abuelo" = 4, 
                                            "Hermana mayor" = 5, 
                                            "Hermano mayor" = 6,
                                            "Otro, ¿Cuál?" = 7,
                                            "No sabe" = 88, 
                                            "No responde" = 99))

get_label(db1$p14)
db1$p14 <- clean_labels(db1$p14)

frq(db1$p14_otra)
get_label(db1$p14_otra)
db1$p14_otra <- sjlabelled::set_label(db1$p14_otra, 
                                      label = "¿Quién es la persona adulta de tu hogar con quien más conversas generalmente? [Otra (especifíca)]")


## p15
frq(db1$p15)
db1$p15 <- sjlabelled::set_labels(db1$p15, 
                                  labels = c("Menos de 5-10 minutos" = 1, 
                                             "Alrededor de media hora" = 2, 
                                             "Alrededor de 1 hora" = 3, 
                                             "Más de 1 hora" = 4,
                                             "No sabe" = 88, 
                                             "No responde" = 99))

get_label(db1$p15)
db1$p15 <- clean_labels(db1$p15)


## p16
db1 %>% 
  select(starts_with("p16_")) %>% 
  frq()

db1 <- db1 %>% 
  mutate(
    across(.cols = c(p16_O1_orig, p16_O2_orig,
                     p16_O3_orig, p16_O4_orig,
                     p16_O5_orig, p16_O6_orig),
           .fns = ~ sjlabelled::set_labels(., labels = c("Colegio" = 1,
                                                         "Deportes" = 2,
                                                         "Política" = 3,
                                                         "Películas o series" = 4,
                                                         "Juegos" = 5,
                                                         "Otro, ¿Cuál?" = 6,
                                                         "No sabe" = 88,
                                                         "No responde" = 99)))
  )


db1 %>% 
  select(starts_with("p16_")) %>% 
  get_label()

db1$P16_o1 <- clean_labels(db1$P16_o1)
db1$P16_o2 <- clean_labels(db1$P16_o2)
db1$P16_o3 <- clean_labels(db1$P16_o3)
db1$P16_o4 <- clean_labels(db1$P16_o4)
db1$P16_o5 <- clean_labels(db1$P16_o5)
db1$P16_o6 <- clean_labels(db1$P16_o6)
db1$P16_o88 <- clean_labels(db1$P16_o88)
db1$P16_o99 <- clean_labels(db1$P16_o99)

db1$p16_O1_orig <- sjlabelled::set_label(db1$p16_O1_orig, label = paste("¿De qué temas conversan generalmente con este adulto?", "(Op. 1)", sep = " "))
db1$p16_O2_orig <- sjlabelled::set_label(db1$p16_O2_orig, label = paste("¿De qué temas conversan generalmente con este adulto?", "(Op. 2)", sep = " "))
db1$p16_O3_orig <- sjlabelled::set_label(db1$p16_O3_orig, label = paste("¿De qué temas conversan generalmente con este adulto?", "(Op. 3)", sep = " "))
db1$p16_O4_orig <- sjlabelled::set_label(db1$p16_O4_orig, label = paste("¿De qué temas conversan generalmente con este adulto?", "(Op. 4)", sep = " "))
db1$p16_O5_orig <- sjlabelled::set_label(db1$p16_O5_orig, label = paste("¿De qué temas conversan generalmente con este adulto?", "(Op. 5)", sep = " "))
db1$p16_O6_orig <- sjlabelled::set_label(db1$p16_O6_orig, label = paste("¿De qué temas conversan generalmente con este adulto?", "(Op. 6)", sep = " "))

frq(db1$S_115_6)
db1$S_115_6 <- sjlabelled::set_label(db1$S_115_6, label = "¿De qué temas conversan generalmente con este adulto? (Otro, ¿Cuál?)")

## p17_

db1 %>% 
  select(starts_with("p17_")) %>% 
  frq(.)

db1 <- db1 %>% 
  mutate(across(.cols = starts_with("p17_"),.fns = ~ sjlabelled::set_labels(., labels = labels1)))

db1 %>% 
  select(starts_with("p17_")) %>% 
  get_label()

db1$p17_1 <- clean_labels(db1$p17_1)
db1$p17_2 <- clean_labels(db1$p17_2)

## p18_

db1 %>% 
  select(starts_with("p18_")) %>% 
  frq(.)

labels5 <- c("Nunca" = 1, 
             "Casi nunca" = 2, 
             "Casi siempre" = 3, 
             "Siempre" = 4,
             "No sabe" = 88, 
             "No responde" = 99)

db1 <- db1 %>% 
  mutate(across(.cols = starts_with("p18_"),.fns = ~ sjlabelled::set_labels(., labels = labels5)))

db1 %>% 
  select(starts_with("p18_")) %>% 
  get_label()

db1$p18_1 <- clean_labels(db1$p18_1)
db1$p18_2 <- clean_labels(db1$p18_2)
db1$p18_3 <- clean_labels(db1$p18_3)
db1$p18_4 <- clean_labels(db1$p18_4)
db1$p18_5 <- clean_labels(db1$p18_5)
db1$p18_6 <- clean_labels(db1$p18_6)


## p19
frq(db1$p19)
db1$p19 <- sjlabelled::set_labels(db1$p19, 
                                  labels = c("Ninguna importancia" = 1, 
                                             "Algo de importancia" = 2, 
                                             "Mediana importancia" = 3, 
                                             "Mucha importancia" = 4,
                                             "No sabe" = 88, 
                                             "No responde" = 99))

get_label(db1$p19)
db1$p19 <- clean_labels(db1$p19)

## p20
frq(db1$p20)
get_label(db1$p20)
db1$p20 <- sjlabelled::set_label(db1$p20, label = "Género")

## p21
db1 %>% 
  select(starts_with("p21_")) %>% 
  frq()

db1 %>% 
  select(starts_with("p21_")) %>% 
  get_label()

db1$p21_mes <- clean_labels(db1$p21_mes)
db1$p21_ano <- clean_labels(db1$p21_ano)

## p22
frq(db1$p22_1)
db1$p22_1 <- clean_labels(db1$p22_1)

frq(db1$p22_otro_1)
recodi <- c("Bolivia" = 068, 
            "Colombia" = 170, 
            "Ecuador" = 218, 
            "Estados Unidos" = 840, 
            "España" = 724, 
            "Haití" = 332, 
            "México" = 484, 
            "Panamá" = 591, 
            "Perú" = 604, 
            "República dominicana" = 214, 
            "Rusia" = 643, 
            "Venezuela" = 862)

recodificar_nacionalidades <- function(x) {
  case_when(
    tolower(x) %in% c("bolivia") ~ recodi["Bolivia"],
    tolower(x) %in% c("colombia") ~ recodi["Colombia"],
    tolower(x) %in% c("ecuador") ~ recodi["Ecuador"],
    tolower(x) %in% c("ee.uu", "eeuu new york", "estados unidos") ~ recodi["Estados Unidos"],
    tolower(x) %in% c("españa") ~ recodi["España"],
    tolower(x) %in% c("haiti", "haíti", "haití") ~ recodi["Haití"],
    tolower(x) %in% c("méxico") ~ recodi["México"],
    tolower(x) %in% c("panama") ~ recodi["Panamá"],
    tolower(x) %in% c("per", "peru", "perú") ~ recodi["Perú"],
    tolower(x) %in% c("republica dominicana") ~ recodi["República Dominicana"],
    tolower(x) %in% c("rusia") ~ recodi["Rusia"],
    tolower(x) %in% c("venezuela", "venezuela, aragua") ~ recodi["Venezuela"],
    TRUE ~ NA_integer_
  )
}

db1 <- db1 %>%
  mutate(p22_otro_1 = recodificar_nacionalidades(p22_otro_1),
         p22_otro_1 = sjlabelled::set_labels(p22_otro_1, labels = recodi),
         p22_otro_1 = sjlabelled::set_label(p22_otro_1, "¿En qué País naciste? [Otro (especifíca)]")) 


frq(db1$p22_2)
db1$p22_2 <- clean_labels(db1$p22_2)

frq(db1$p22_2_S)
recodi <- c("Bolivia" = 068, 
            "Colombia" = 170, 
            "Ecuador" = 218, 
            "Estados Unidos" = 840, 
            "España" = 724, 
            "Haití" = 332, 
            "México" = 484, 
            "Panamá" = 591, 
            "Perú" = 604, 
            "República Dominicana" = 214, 
            "Rusia" = 643, 
            "Venezuela" = 862,
            "Argentina" = 032,
            "Brasil" = 076,
            "Chile" = 152,
            "Uruguay" = 858,
            "Australia" = 036)

recodificar_nacionalidades <- function(x) {
  case_when(
    tolower(x) %in% c("bolivia", "boliviana") ~ recodi["Bolivia"],
    tolower(x) %in% c("colombia") ~ recodi["Colombia"],
    tolower(x) %in% c("ecuador") ~ recodi["Ecuador"],
    tolower(x) %in% c("ee.uu", "eeuu new york", "estados unidos") ~ recodi["Estados Unidos"],
    tolower(x) %in% c("españa", "valencia") ~ recodi["España"],
    tolower(x) %in% c("haiti", "haíti", "haití", "hati") ~ recodi["Haití"],
    tolower(x) %in% c("méxico") ~ recodi["México"],
    tolower(x) %in% c("panama") ~ recodi["Panamá"],
    tolower(x) %in% c("per", "peru", "perú", "eru") ~ recodi["Perú"],
    tolower(x) %in% c("republica dominicana") ~ recodi["República Dominicana"],
    tolower(x) %in% c("rusia") ~ recodi["Rusia"],
    tolower(x) %in% c("venezuela", "venezuela, caracas") ~ recodi["Venezuela"],
    tolower(x) %in% c("argentina") ~ recodi["Argentina"],
    tolower(x) %in% c("brasil") ~ recodi["Brasil"],
    tolower(x) %in% c("chile") ~ recodi["Chile"],
    tolower(x) %in% c("uruguay") ~ recodi["Uruguay"],
    tolower(x) %in% c("australia") ~ recodi["Australia"],
    TRUE ~ NA_integer_
  )
}

db1 <- db1 %>%
  mutate(p22_2_S = recodificar_nacionalidades(p22_2_S),
         p22_2_S = sjlabelled::set_labels(p22_2_S, labels = recodi),
         p22_2_S = sjlabelled::set_label(p22_2_S, label = "¿En qué País nació tu madre o cuidadora mujer? [Otro (especifíca)]")
         ) 


frq(db1$p22_3)
db1$p22_3 <- clean_labels(db1$p22_3)

frq(db1$p22_3_S)
recodi <- c("Argentina" = 032,
            "Bolivia" = 068, 
            "Chile" = 152,
            "Colombia" = 170, 
            "Ecuador" = 218, 
            "España" = 724, 
            "Haití" = 332, 
            "México" = 484, 
            "Perú" = 604, 
            "Venezuela" = 862,
            "Estados Unidos" = 840)

recodificar_nacionalidades <- function(x) {
  case_when(
    tolower(x) %in% c("argentina", "arjentina") ~ recodi["Argentina"],
    tolower(x) %in% c("bolivia") ~ recodi["Bolivia"],
    tolower(x) %in% c("chile") ~ recodi["Chile"],
    tolower(x) %in% c("colombia") ~ recodi["Colombia"],
    tolower(x) %in% c("ecuador", "ecuador (lo que me acuerdo)") ~ recodi["Ecuador"],
    tolower(x) %in% c("españa", "valencia") ~ recodi["España"],
    tolower(x) %in% c("haiti", "haíti", "haití", "hati") ~ recodi["Haití"],
    tolower(x) %in% c("méxico") ~ recodi["México"],
    tolower(x) %in% c("per", "peru", "perú", "eru", "peruano") ~ recodi["Perú"],
    tolower(x) %in% c("venezuela", "venezuela, aragua", "venzuelana", "venzuelano") ~ recodi["Venezuela"],
    tolower(x) %in% c("estados unidos", "ee.uu", "usa") ~ recodi["Estados Unidos"],
    TRUE ~ NA_integer_
  )
}

db1 <- db1 %>%
  mutate(p22_3_S = recodificar_nacionalidades(p22_3_S),
         p22_3_S = sjlabelled::set_labels(p22_3_S, labels = recodi),
         p22_3_S = sjlabelled::set_label(p22_3_S, label = "¿En qué País nació tu Padre o cuidador hombre? [Otro (especifíca)]")) 

## p23
frq(db1$p23) # ok

## p24
frq(db1$p24)
db1$p24 <- sjlabelled::set_labels(db1$p24, 
                                  labels = c("Mapuche" = 1, 
                                             "Aimara" = 2, 
                                             "Rapa Nui" = 3, 
                                             "Quechua" = 4,
                                             "No, a ninguno de las anteriores" = 5,
                                             "No sabe" = 88, 
                                             "No responde" = 99))

get_label(db1$p24)
db1$p24 <- clean_labels(db1$p24)

## p25
frq(db1$p25)
db1$p25 <- sjlabelled::set_labels(db1$p25, 
                                  labels = c("Educación Media" = 1, 
                                             "Instituto profesional o Centro de Formación Técnica" = 2, 
                                             "Una carrera en la Universidad" = 3, 
                                             "No sabe" = 88, 
                                             "No responde" = 99))

get_label(db1$p25)
db1$p25 <- clean_labels(db1$p25)

## p26
frq(db1$p26)
db1$p26 <- sjlabelled::set_labels(db1$p26, 
                                  labels = c("No terminó la educación básica" = 1, 
                                             "Completó la educación básica" = 2, 
                                             "Completó la enseñanza media" = 3, 
                                             "Completó estudios en un Instituto Profesional o Centro de Formación Técnica" = 4,
                                             "Completó una carrera en la Universidad" = 5,
                                             "Tiene estudios de posgrado" = 6,
                                             "No sabe" = 88, 
                                             "No responde" = 99))

get_label(db1$p26)
db1$p26 <- clean_labels(db1$p26)


## p27
frq(db1$p27)
db1$p27 <- sjlabelled::set_labels(db1$p27, 
                                  labels = c("No terminó la educación básica" = 1, 
                                             "Completó la educación básica" = 2, 
                                             "Completó la enseñanza media" = 3, 
                                             "Completó estudios en un Instituto Profesional o Centro de Formación Técnica" = 4,
                                             "Completó una carrera en la Universidad" = 5,
                                             "Tiene estudios de posgrado" = 6,
                                             "No sabe" = 88, 
                                             "No responde" = 99))

get_label(db1$p27)
db1$p27 <- clean_labels(db1$p27)

## p28
frq(db1$p28)
db1$p28 <- sjlabelled::set_labels(db1$p28, 
                                  labels = c("Católico" = 1, 
                                             "Evangélico" = 2, 
                                             "Protestante" = 3, 
                                             "Judío" = 4,
                                             "Creyente no adherente" = 5,
                                             "Ateo" = 6,
                                             "Agnóstico" = 7,
                                             "Ninguna" = 8,
                                             "No sabe" = 88, 
                                             "No responde" = 99))

get_label(db1$p28)
db1$p28 <- clean_labels(db1$p28)


## p29
frq(db1$p29)
db1$p29 <- sjlabelled::set_labels(db1$p29, 
                                  labels = c("Nunca" = 1, 
                                             "Menos de una vez al año" = 2, 
                                             "Una vez al año" = 3, 
                                             "Al menos una vez al año" = 4,
                                             "Al menos una vza la semana" = 5,
                                             "Diario" = 6,
                                             "No sabe" = 88, 
                                             "No responde" = 99))

get_label(db1$p29)
db1$p29 <- clean_labels(db1$p29)

## p30
frq(db1$p30)
db1$p30 <- sjlabelled::set_labels(db1$p30, 
                                  labels = c("Entre 0 y 10 libros" = 1, 
                                             "Entre 11 y 25 libros" = 2, 
                                             "Entre 26 y 100 libros" = 3, 
                                             "Entre 100 y 200 libros" = 4,
                                             "Entre 201 y 500 libros" = 5,
                                             "Más de 500 libros" = 6,
                                             "No sabe" = 88, 
                                             "No responde" = 99))

get_label(db1$p30)
db1$p30 <- clean_labels(db1$p30)

#4. Save and remove ----------------------------------------------------

db_students <- db1 %>% 
  select(-c(S_116_6)) %>% 
  rename(p22_otro_2 = p22_2_S, 
         p22_otro_3 = p22_3_S) %>% 
  janitor::clean_names() %>% 
  as.data.frame()

base::save(db_students, file = here("output/data/db_proc_students.RData"))
haven::write_dta(db_students, path = here("output/data/db_proc_students.dta"))
haven::write_sav(db_students, path = here("output/data/db_proc_students.sav"))

rm(list = ls(pattern = "^labels[0-9]+$"))