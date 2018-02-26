#Population projection scraper- INDEC

#Retrieve official information about population by province and administrative department.
#Apply the growth rate to each city (localidad censal).
#IMPORTANT: There exists an assumption that the department growth is homogeneous between the cities.
#This is probably not true (but it's the only way I could figure out to do it).

#Load packages.
library(rvest)
library(tidyverse)
library(readxl)

#Specify the url
url <- "https://www.indec.gob.ar/nivel4_default.asp?id_tema_1=2&id_tema_2=24&id_tema_3=119/"

# I must complete the scraping (Couldn't get around the proxy)

# I removed the rows containing Subtitles in the Buenos Aires file and a skipped line in La Rioja. 
# Should do it here though.
setwd("~/Scraper pop proj/projs1")
filelist <- list.files(pattern = ".*xls")    # read all file names according to pattern
files <- lapply(filelist, function(x) read_excel(x, skip = 8, col_names = c("departamento", paste0("y",2010:2025))))
files_subset <- lapply(files, function(x) slice(x, 1:min(which(is.na(x))))) #Subset only Totals
df1 <- bind_rows(files_subset, .id = 'id') # Merge all the files

#Recode the provinces according to INDEC's nomenclative (from CABA, 2, to Neuquén, 58)
df1$id_provincia <- dplyr::recode(df1$id, "1"="6", "2" = "2", "3" = "10", "4" = "22", "5" = "26", 
                               "6" = "14", "7" = "18", "8" = "30", "9" = "34", "10" = "38", 
                               "11" = "42", "12" = "46", "13" = "50", "14" = "54", "15" = "58")

# These datasets have one more row to skip.
setwd("~/Scraper pop proj/projs2")
filelist <- list.files(pattern = ".*xls")    # read all file names according to pattern
files <- lapply(filelist, function(x) read_excel(x, skip = 9, col_names = c("departamento", paste0("y",2010:2025))))
files_subset <- lapply(files, function(x) slice(x, 1:min(which(is.na(x))))) #Subset only Totals
df2 <- bind_rows(files_subset, .id = 'id') # Merge all the files

#Recode the provinces according to INDEC's nomenclative (from Río Negro, 62, to Tucumán, 90)
df2$id_provincia <- dplyr::recode(df2$id, "1"="62", "2" = "66", "3" = "70", "4" = "74", "5" = "78", 
                               "6" = "82", "7" = "86", "8" = "94", "9" = "90")

#Create the dataset
df <- bind_rows(list(df1, df2), .id = 'id') # Bind both datasets
df <- df[,3:ncol(df)] #Remove duplicated id columns
df <- df %>%
    filter(!is.na(departamento)) %>% 
    mutate(y2025 = as.numeric(y2025)) %>% 
    arrange(as.integer(id_provincia)) %>% 
    select(id_provincia, everything()) #reorder the columns

#Keep the names of the provinces for the columns.
#colnames(df) <- str_replace(colnames(df), "^.*?\\. ","") %>% 
#    str_replace("\\..*","")
growth_rates <- df %>%
    group_by(id_provincia,departamento) %>% 
    mutate_all(funs(rate = . / y2010)) %>% 
    select(id_provincia, departamento, y2010_rate:y2025_rate) %>% 
    ungroup() %>% 
    mutate(departamento = iconv(toupper(departamento),from="UTF-8",to="ASCII//TRANSLIT")) #Recode

#Remove unused objects
rm(df1, df2)

#Apply rates to census cities
setwd("~/Scraper pop proj/")
cities <- read_csv2("poblacion.csv", locale = locale(encoding = "latin1")) %>% 
    mutate(id_provincia = as.character(id_provincia), 
    departamento = iconv(toupper(departamento),from="UTF-8",to="ASCII//TRANSLIT"))


#There are 49 rows that can't join because of error in departments names. Here I solve it:
cities$departamento <- cities$departamento %>%
    recode("LIBERTADOR GRL. SAN MARTIN" = "LIBERTADOR GENERAL SAN MARTIN",
           "GRL. JOSE DE SAN MARTIN" = "GENERAL JOSE DE SAN MARTIN",
           "CORONEL DE MARINA L. ROSALES" = "CORONEL DE MARINA LEONARDO ROSALES",
           "O' HIGGINS" = "O'HIGGINS",
           "CHICALCO" = "CHICAL CO",
           "JUAN BAUTISTA ALBERDI" = "JUAN B. ALBERDI")
#IMPORTANT: since Ciudad Autónoma de Buenos Aires ir a single city (with multiple departments) I
#can't get the projected population with this method. Eventually I will merge all its departments.

#Get the population projections for the cities
cities_projs <- cities %>%
    left_join(growth_rates, by = c("id_provincia", "departamento")) %>% 
    select(id_provincia, departamento, localidad, personas, y2011_rate:y2025_rate) %>% 
    group_by(id_provincia, departamento, localidad, personas) %>%
    mutate_all(funs(proj = personas * .)) %>%  #apply growth rates
    mutate_all(funs(round(.,2))) %>%  #round the projections up to two decimals
    left_join(cities, by = c("id_provincia", "departamento", "localidad")) %>%
    select(-c(personas.y, y2011_rate:y2025_rate,geom)) %>% 
    rename(personas = personas.x) %>% 
    select(link,id_provincia,id_depto,id_localidad,everything()) %>% 
    arrange(link)
