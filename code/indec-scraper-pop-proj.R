#Population projection scraper- INDEC

#Retrieve official information about population by province and administrative department.
#Apply the growth rate to each city (localidad censal).

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
files <- lapply(filelist, function(x) read_excel(x, skip = 8, col_names = c("Depto", 2010:2025)))
files_subset <- lapply(files, function(x) slice(x, 1:min(which(is.na(x))))) #Subset only Totals
df1 <- bind_rows(files_subset, .id = 'id') # Merge all the files
df1$provincia <- dplyr::recode(df1$id, "1"="6", "2" = "2", "3" = "10", "4" = "22", "5" = "26", 
                               "6" = "14", "7" = "18", "8" = "30", "9" = "34", "10" = "38", 
                               "11" = "42", "12" = "46", "13" = "50", "14" = "54", "15" = "58")

# These datasets have one more row to skip.
setwd("~/Scraper pop proj/projs2")
filelist <- list.files(pattern = ".*xls")    # read all file names according to pattern
files <- lapply(filelist, function(x) read_excel(x, skip = 9, col_names = c("Depto", 2010:2025)))
files_subset <- lapply(files, function(x) slice(x, 1:min(which(is.na(x))))) #Subset only Totals
df2 <- bind_rows(files_subset, .id = 'id') # Merge all the files
df2$provincia <- dplyr::recode(df2$id, "1"="62", "2" = "66", "3" = "70", "4" = "74", "5" = "78", 
                               "6" = "82", "7" = "86", "8" = "94", "9" = "90")

df <- bind_rows(list(df1, df2), .id = 'id') # Bind both datasets
df <- df[,3:ncol(df)] #Remove duplicated id columns
df <- df %>%
    filter(!is.na(Depto)) %>% 
    arrange(as.integer(provincia))

View(table(as.integer(df$provincia)))


#Keep the names of the provinces for the columns.
colnames(df) <- str_replace(colnames(df), "^.*?\\. ","") %>% 
    str_replace("\\..*","")

col <- df %>%
    select(2, 19:41) %>% 
    replace(is.na(.), "") %>% 
    unite(col, 1:ncol(col), sep="")
    
provincias <- df %>% 
    select(2, 19:41) %>% 
    ifelse()