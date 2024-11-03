#*******************************************************************************************************************
#
# 0. Identification ---------------------------------------------------
# Title: Data preparation for EDUMER Full Survey Wave 1
# Author: Andreas Laffert            
# Overview: EDUMER Full Survey Wave 1         
# Date: 13-066-2024            
#
#******************************************************************************************************************

# 1. Packages ---------------------------------------------------------
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               sjlabelled, 
               sjmisc, 
               here, 
               haven)

options(scipen=999)
rm(list = ls())
# 2. Data --------------------------------------------------------------

load(file = here("output", "data", "db_proc_students.RData"))
load(file = here("output", "data", "db_proc_teachers.RData"))
load(file = here("output", "data", "db_proc_parents.RData"))

glimpse(db_students)
glimpse(db_teachers)
glimpse(db_parents)

# 3. Manipulation ---------------------------------------------------------

cases <- c(nrow(db_students), nrow(db_teachers), nrow(db_parents))

if (length(unique(cases)) == 1) {
  db_full <- cbind(db_students, db_teachers, db_parents)
  print("Casos iguales y bases de datos unidas")
} else {
  print("No correspondencia número casos, no se pueden unir las columnas")
  db_full <- NULL 
}

glimpse(db_full)

#4. Save and remove ----------------------------------------------------

base::save(db_full, file = here("output/data/db_proc_full.RData"))
haven::write_dta(db_full, path = here("output/data/db_proc_full.dta"))
haven::write_sav(db_full, path = here("output/data/db_proc_full.sav"))
