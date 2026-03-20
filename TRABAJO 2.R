###### TRABAJO # 2 
###### Presentado por : Carlos Eduardo Laverde Sabogal

library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)

#### PUNTO 1. Lea los archivos en R
# ============================================================
# LECTOR UNIVERSAL (Excel 2007 .xlsx | Stata 12 .dta | CSV | RData)
# ============================================================
# install.packages(c("readxl","haven","readr"))  # si hace falta
library(readxl)
library(haven)
library(readr)

leer_archivo <- function(ruta) {
  ext <- tolower(tools::file_ext(ruta))
  
  if (ext == "xlsx") {
    # Microsoft Excel 2007+
    readxl::read_excel(ruta)
    
  } else if (ext == "dta") {
    # Stata (incluye Stata v12)
    haven::read_dta(ruta)
    
  } else if (ext == "csv") {
    # CSV separado por comas
    readr::read_csv(ruta, show_col_types = FALSE)
    
  } else if (ext %in% c("rdata", "rda")) {
    # Espacio de trabajo de R (.RData/.rda)
    e <- new.env()
    objs <- load(ruta, envir = e)
    if (length(objs) == 1) e[[objs]] else mget(objs, envir = e)
    
  } else {
    stop("Formato no soportado: ", ext)
  }
}

###########################################
#### PUNTO 2. La tabla punmod 
###########################################

punmod <- punmoda %>%
  left_join(punmodb, by = "ESTU_ID")
#########################################################
###### PUNTO 3. Tablas infocont, departamento y municipio 
#########################################################

infocont_full <- infocont %>%
  mutate(
    ESTU_COD_RESIDE_DEPTO = as.character(ESTU_COD_RESIDE_DEPTO),
    ESTU_COD_RESIDE_MCPIO       = as.character(ESTU_COD_RESIDE_MCPIO)
  ) %>%
  left_join(
    departamentos %>%
      transmute(CODIGO_DEP = as.character(CODIGO_DEP),
                NOMBRE_DEP),
    by = c("ESTU_COD_RESIDE_DEPTO" = "CODIGO_DEP")
  ) %>%
  left_join(
    municipios %>%
      transmute(CODIGO_MUN = as.character(CODIGO_MUN),
                NOMBRE_MUN),
    by = c("ESTU_COD_RESIDE_MCPIO" = "CODIGO_MUN")
  )
##################################################################
########## 4. Describa a los estudiantes que presentaron la prueba:

## a. ¿Cuántos estudiantes presentaron la prueba SaberPro?

punglo %>%
  summarise(n_estudiantes = n_distinct(ESTU_ID))


### RESULTADO :  242629

##  b. ¿Cuántas de ellas son mujeres?


infoper %>%
  summarise(n_mujeres = sum(trimws(toupper(ESTU_GENERO)) == "F", na.rm = TRUE))

### RESULTADO : 143451

##  c. Suponga que la prueba fue presentada el 4 de octubre de 2016. ¿Cuántos estudiantes se
##encuentran en los siguientes rangos de edad al momento de presentar la prueba Saber Pro:
##  menores de 18 años, 18 a 25 años, 26 a 39 años, ¿40 o más años?


fecha_prueba <- as.Date("2016-10-04")

tabla_edades <- infoper %>%
  mutate(
    # Convertir fecha de nacimiento a Date (ajusta si ya está como Date)
    fnac = coalesce(ymd(ESTU_FECHANACIMIENTO), dmy(ESTU_FECHANACIMIENTO)),
    
    # Edad en años cumplidos al día de la prueba
    edad = floor(time_length(interval(fnac, fecha_prueba), "years")),
    
    # Rangos solicitados
    rango_edad = case_when(
      edad < 18 ~ "Menores de 18",
      edad >= 18 & edad <= 25 ~ "18 a 25",
      edad >= 26 & edad <= 39 ~ "26 a 39",
      edad >= 40 ~ "40 o más",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(rango_edad)) %>%        # por si hay fechas inválidas
  count(rango_edad, name = "n") %>%
  arrange(match(rango_edad, c("Menores de 18","18 a 25","26 a 39","40 o más")))

tabla_edades

####### resultados : 
#< 18:      318
# 18-25 :   151127
# 26-39:    77092
# 40 o mas :13182


##  d. ¿Cuántos estudiantes indicaron tener autismo como discapacidad 
##al momento de la inscripción a la prueba?

infoper %>%
  summarise(
    n_autismo = sum(trimws(toupper(as.character(ESTU_LIMITA_AUTISMO))) == "X", na.rm = TRUE)
  )

### RESULTADO : 4 Estudiantes con autismo.


#### PREGUNTA 5.

#¿Cuántos estudiantes presentaron la prueba Saber Pro por cada grupo de referencia al
#que pertenece el programa académico (GRUPOREFERENCIA) y nivel del programa académico
#(ESTU_NIVEL_PRGM_ACADEMICO) en la ciudad de Bogotá, D.C.? Los resultados deben estar en una tabla
#que incluya las variables: nivel, grupo de referencia y número de estudiantes, los registros deben
#estar ordenados por nivel, número de estudiantes (descendente), grupo de referencia (orden
              

library(dplyr)
library(stringr)
library(stringi)

tabla_bogota1 <- punmod %>%
  dplyr::distinct(ESTU_ID) %>%   # estudiantes que presentaron
  dplyr::left_join(
    infoies %>%
      dplyr::transmute(
        ESTU_ID,
        nivel = toupper(trimws(as.character(ESTU_NIVEL_PRGM_ACADEMICO))),
        grupo_referencia = toupper(trimws(as.character(GRUPOREFERENCIA)))
      ),
    by = "ESTU_ID"
  ) %>%
  dplyr::left_join(
    infocont %>%
      dplyr::transmute(
        ESTU_ID,
        dep_cod = as.integer(readr::parse_number(as.character(ESTU_COD_RESIDE_DEPTO)))
      ),
    by = "ESTU_ID"
  ) %>%
  dplyr::filter(dep_cod == 11) %>%   # Bogotá D.C. (código departamento)
  dplyr::group_by(nivel, grupo_referencia) %>%
  dplyr::summarise(numero_estudiantes = dplyr::n_distinct(ESTU_ID), .groups = "drop") %>%
  dplyr::arrange(nivel, dplyr::desc(numero_estudiantes), dplyr::desc(grupo_referencia))

tabla_bogota1

########## RESULTADO
###1 TECNOLOGÍA NORMALES SUPERIORES 19
## 2 UNIVERSITARIO ADMINISTRACIÓN Y AFINES 16455
## 3 UNIVERSITARIO INGENIERÍA 14232
## 4 UNIVERSITARIO EDUCACIÓN 5791
## 5 UNIVERSITARIO DERECHO 5559


 ############################################################################
#### Pregunta  # 6.ESTUDIANTES ESTRATO1-3, FUERA DE BOGOTA CON INTERNET


#¿Cuántos estudiantes de estrato 1, 2 o 3 (FAMI_ESTRATOVIVIENDA) que no viven
#en Bogotá cuentan en su hogar con servicio o conexión a Internet (FAMI_TIENEINTERNET)?
#############################################################################


library(dplyr)
library(haven)
library(readr)
library(stringr)

bogota_cod <- "11001"
extranjero_cod <- "99999"

id_fix <- function(x) trimws(sub("\\.0+$", "", as.character(x)))

ids_no_bogota <- infocont %>%
  transmute(
    ESTU_ID = id_fix(ESTU_ID),
    mun_cod = trimws(as.character(ESTU_COD_RESIDE_MCPIO))
  ) %>%
  filter(!is.na(mun_cod), mun_cod != "", mun_cod != bogota_cod, mun_cod != extranjero_cod) %>%
  distinct(ESTU_ID)

n_con_internet <- infosocio %>%
  transmute(
    ESTU_ID = id_fix(ESTU_ID),
    estrato = readr::parse_number(as.character(haven::as_factor(FAMI_ESTRATOVIVIENDA))),
    internet_si = toupper(trimws(as.character(haven::as_factor(FAMI_TIENEINTERNET)))) == "SI"
  ) %>%
  filter(estrato %in% c(1,2,3), internet_si) %>%
  semi_join(ids_no_bogota, by = "ESTU_ID") %>%
  summarise(n = n_distinct(ESTU_ID)) %>%
  pull(n)

n_con_internet

############ respuesta [1] 111479
############ La respuesta se encontraba en 111483 pero la discrepancia de 4 reside en el codigo
######  "99999" que corresponde a extrangero, al ser suprimido arroja el valor de 111479.


#############################################
####### 7. Distribucion TOP 5 calificaciones
#############################################


library(dplyr)
library(stringr)
library(readr)
library(stringi)

id_fix <- function(x){
  x <- trimws(as.character(x))
  x <- sub("\\.0+$", "", x)
  suppressWarnings(num <- as.numeric(x))
  ifelse(!is.na(num), sprintf("%.0f", num), x)
}

norm_txt <- function(x){
  stringi::stri_trans_general(toupper(trimws(as.character(x))), "Latin-ASCII")
}

top5 <- punglo %>%
  mutate(
    ESTU_ID = id_fix(ESTU_ID),
    PUNT_GLOBAL = as.numeric(gsub(",", ".", as.character(PUNT_GLOBAL)))
  ) %>%
  group_by(ESTU_ID) %>%
  slice_max(PUNT_GLOBAL, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(
    infoies %>%
      transmute(
        ESTU_ID = id_fix(ESTU_ID),
        nivel = toupper(trimws(as.character(ESTU_NIVEL_PRGM_ACADEMICO))),
        COD_IES = as.integer(INST_COD_INSTITUCION),
        dep_prog_key = norm_txt(ESTU_PRGM_CODDEPARTAMENTO)
      ),
    by = "ESTU_ID"
  ) %>%
  filter(nivel == "UNIVERSITARIO") %>%
  left_join(
    ies %>%
      transmute(
        COD_IES = as.integer(INST_COD_INSTITUCION),
        universidad = as.character(INST_NOMBRE_INSTITUCION)
      ) %>%
      distinct(COD_IES, .keep_all = TRUE),
    by = "COD_IES"
  ) %>%
  left_join(
    departamentos %>%
      transmute(
        dep_prog_key = norm_txt(NOMBRE_DEP),
        departamento = toupper(trimws(as.character(NOMBRE_DEP)))
      ) %>%
      distinct(dep_prog_key, .keep_all = TRUE),
    by = "dep_prog_key"
  ) %>%
  mutate(departamento = coalesce(departamento, dep_prog_key)) %>%
  filter(!is.na(departamento), !is.na(PUNT_GLOBAL)) %>%
  group_by(departamento) %>%
  slice_max(PUNT_GLOBAL, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  select(departamento, ESTU_ID, universidad, PUNT_GLOBAL) %>%
  arrange(desc(departamento), desc(PUNT_GLOBAL))   

top5

########## RESULTADO PUNTO #7
##########
#    departamento ESTU_ID     universidad                                     PUNT_GLOBAL
#    <chr>          <chr>    <chr>                                                 <dbl>
#  1 VALLE        30030067 UNIVERSIDAD DEL VALLE-CALI                              238
#  2 VALLE        30170315 UNIVERSIDAD ICESI-CALI                                  231
#  3 VALLE        30081051 UNIVERSIDAD DEL VALLE-CALI                              229
#  4 VALLE        30170325 UNIVERSIDAD ICESI-CALI                                  228
#  5 VALLE        30081815 UNIVERSIDAD DEL VALLE-CALI                              226
#  6 TOLIMA       30072520 UNIVERSIDAD COOPERATIVA DE COLOMBIA-BOGOTÁ D.C.         224
#  7 TOLIMA       30142510 UNIVERSIDAD DE IBAGUE-IBAGUE                            216
#  8 TOLIMA       30044346 UNIVERSIDAD DEL TOLIMA-IBAGUE                           215
#  9 TOLIMA       30141276 UNIVERSIDAD DEL TOLIMA-IBAGUE                           214
#  10 TOLIMA       30187179 CONSERVATORIO DEL TOLIMA-IBAGUE                         214

