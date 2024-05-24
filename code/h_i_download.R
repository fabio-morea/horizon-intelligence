


###### DOWNLOAD ##########
# download datasets from: https://data.europa.eu/data/datasets
##  Horizon 2020 (2014-2020)
##  access page: https://data.europa.eu/data/datasets/cordish2020projects 
### download URL https://cordis.europa.eu/data/cordis-h2020projects-csv.zip

##  Horizon Europe (2021-2027)
##  access page: https://data.europa.eu/data/datasets/cordis-eu-research-projects-under-horizon-europe-2021-2027 
### download URL https://cordis.europa.eu/data/cordis-HORIZONprojects-csv.zip
### 
### download programme description
### https://cordis.europa.eu/data/reference/cordisref-HORIZONprogrammes-csv.zip
### 
### 
### download organisation activity type
### https://cordis.europa.eu/data/reference/cordisref-HORIZONprogrammes-csv.zip
### 
### 
### 

# libraries
options(warn=-1)
library(tidyverse)
library(lubridate)
library(readxl)

# parameters: 
download_H2020 <- F
download_HEUROPE <- F


folder_H2020 <- "./data/H_2020_csv/"
folder_HEurope <- "./data/H_EU_csv/"

############# functions ############# 


download_dataset <- function(cordis_address, local_folder){
    print("Downloading CORDIS data from cordis.europa.eu ")
    print("...download may take some minutes...")
    temp_dir <- "./tmp/"
    dir.create(temp_dir)
    raw_data_filename <- paste0(temp_dir,"rawdata.zip")
    print(raw_data_filename)
    download.file(cordis_address, raw_data_filename)
    if (dir.exists(local_folder)) {unlink(local_folder, recursive = TRUE)}  
    unzip(raw_data_filename, overwrite = TRUE)
    unlink(local_folder)
    dir.create(local_folder)
    print(local_folder)
    invisible(lapply(list.files("./csv",full.names = TRUE), function(file) file.copy(file, local_folder, overwrite = TRUE)))
    unlink(temp_dir)  # Remove temporary directory
    unlink("./csv/", recursive = TRUE)  # Remove temporary directory
    return(1)
}

read_orgs <- function(local_cordis_folder){
    filename <- paste0(local_cordis_folder, "organization.csv")
    print(paste("Reading ORGANISATIONS from", local_cordis_folder))
    data <- read_delim(filename, 
                       delim = ";", 
                       quote = "\"",
                       na = c("", "NA"),
                       col_types = list(col_character()))
    return(data)
}


read_prjs <- function(local_cordis_folder){
    filename <- paste0(local_cordis_folder, "project.csv")
    print(paste("Reading PROJECTS from", local_cordis_folder))
    data <- read_delim(filename,
                       delim = ";",
                       quote = "\"",
                       col_types = list(col_character()))
    data <- data %>%
        mutate(projectID = id)%>%
        mutate(endYear = as.integer(year(endDate)))  
    return(data)
}

read_legalbasis <- function(local_cordis_folder){
    print(paste("Reading LEGAL BASIS from", local_cordis_folder))
    data <- read_delim(paste0(local_cordis_folder, "legalBasis.csv"), 
                       delim = ";", 
                       quote = "\"", 
                       col_types = 'c') %>%
        mutate(len = str_length(legalBasis))%>%
        arrange(projectID, len, legalBasis)%>%
        select(projectID, title, legalBasis, len) %>%
        distinct(projectID, .keep_all = TRUE)%>%
        mutate(group = substring(legalBasis, 1,10) )%>%
        mutate(group2 = substring(legalBasis, 1,12) )%>%
        mutate(call_topic = title)%>%
        relocate(projectID,group)
    return(data)
}
############# download ############# 


if (download_H2020 == TRUE){
    download_dataset (
        cordis_address = "https://cordis.europa.eu/data/cordis-h2020projects-csv.zip", 
        local_folder = folder_H2020)
} 

if (download_HEUROPE == TRUE){
    download_dataset (
        cordis_address = "https://cordis.europa.eu/data/cordis-HORIZONprojects-csv.zip", 
        local_folder = folder_HEurope)
} 



# merge H2020 and HEUROPE data and save CSV files

## organisations: merge
print("Merging organisations...")
orgs <- rbind( read_orgs( folder_H2020 ), read_orgs( folder_HEurope) )  %>%
    mutate(orgID = paste0('o',organisationID)) %>% 
    mutate(projID = paste0('p', projectID)) %>%
    mutate(postCode = paste0(country, postCode)) %>%
    mutate(shortName = if_else( is.na(shortName), name, shortName )) %>% 
    distinct()

print("Adding NUTS3 geo-tags")
## organisations: add NUTS3 from zip codes from https://gisco-services.ec.europa.eu/tercet/flat-files https://gisco-services.ec.europa.eu/tercet/flat-files
# TODO download_dataset (cordis_address =  ..., local_folder = ...)
NUTS3zipcodes <- read_delim('./data_keys/nuts_zip_codes/pc2020_IT_NUTS-2021_v1.0.csv',
                            show_col_types = FALSE,
                            delim = ";",
                            quote = "'",
                            na = c("", "NA"),
                            col_types = list(col_character())) %>%
    rename(postCode = CODE)%>%
    rename(nuts3 = NUTS3) %>%
    mutate(country = substr(nuts3,1,2)) %>%
    mutate(postCode = paste0(country, postCode))  %>%
    select(-country)

orgs <- merge(x = orgs, y = NUTS3zipcodes, by = 'postCode', all.x = TRUE)
#str(orgs)
# add italian region names
nuts23 <- read_delim('./data_keys/nuts_zip_codes/NUTS_IT.csv',
                     delim = ";",
                     show_col_types = FALSE) %>%
    select(nuts1,nuts2,nuts3, regione)

orgs <- merge(x = orgs, y = nuts23, by = 'nuts3', all.x = TRUE)
#str(orgs)
orgs_unique <- orgs %>%
    select(orgID,vatNumber,name,shortName,SME,activityType,country, postCode, nuts3, regione, city,geolocation, role) %>%
    #  distinct(orgID, .keep_all = TRUE)%>%
    arrange(country, shortName,orgID)%>%
    relocate(country, shortName,orgID) 

orgs_unique %>% write_csv('./data/orgs.csv')

# projects
print("Merging projects...")

projects <- rbind( read_prjs( folder_H2020 ), read_prjs( folder_HEurope) ) %>%
    mutate(projID = paste0('p',projectID)) %>% 
    distinct(projID, .keep_all = TRUE) %>%
    filter(startDate > 0, endDate > 0, !is.na(startDate), !is.na(endDate)) %>%
    select(-projectID) 

projects %>% select(projID, objective) %>% write_csv('./data/prj_objectives.csv')

projects %>% select(contentUpdateDate) %>% 
    arrange(desc(contentUpdateDate)) %>%
    slice(1) %>% write_csv('./data/latest_update.csv')

projects <- projects %>%
    select(projID, acronym, startDate, endDate, subCall, 
           fundingScheme,legalBasis, masterCall, subCall)


str(projects)
calls <- rbind(read_legalbasis(folder_HEurope), read_legalbasis(folder_H2020)) %>%
    mutate(projID = paste0('p',projectID)) 
str(projects)
#participation table
print("Preparing participation table")

participation <- orgs %>%
    select("projID", "orgID","rcn","role", "ecContribution") %>% 
    mutate(coordinator = ifelse(role == "coordinator", TRUE, FALSE)) %>%
    select(-role)%>%
    filter(ecContribution > 10.0) %>%
    mutate(ecContribution = round(as.numeric(ecContribution)/1000,3))  
participation %>% write_csv('./data/participation.csv')
 

print("preparing euroSciVoc codes")
# TODO: add esv codes for both Horizon 2020 and Horizon Europe!!!!!!!!!!!!!!!!!!!!!
print("Reading EuroSciVoc codes from local data folder")

voc1 <- read_delim(paste0(folder_H2020, "euroSciVoc.csv") , delim = ";", quote = "\"", col_types = 'c') 
voc2 <- read_delim(paste0(folder_HEurope, "euroSciVoc.csv") , delim = ";", quote = "\"", col_types = 'c') 
euroSciVoc <- rbind(voc1, voc2) %>%
    select(projectID, euroSciVocTitle, euroSciVocPath) %>%
    mutate(projID = paste0('p', projectID)) %>% select(-projectID)%>%
    mutate(level1 = substring(euroSciVocPath,2,999))%>%
    mutate(level1 = sub("/.*", "", level1))%>%
    arrange(euroSciVocTitle) %>%
    select(projID,level1, euroSciVocTitle)

euroSciVoc %>% write_csv("voc.csv")

#extract euroSciVoc codes list

euroSciVoc %>% 
    select(level1,euroSciVocTitle) %>% 
    distinct()	%>% arrange(level1,euroSciVocTitle)%>%
    write_csv("./data/topic_esv.csv")

project_with_codes <- merge(x=projects, y=euroSciVoc, by = "projID",all.x=TRUE) %>% 
    arrange(startDate, endDate, acronym)

project_with_codes %>% write_csv("./data/project_topic_esv.csv")
print("completed the analysis of ESV topics")
print("note: some projects are NOT associated with any EuroSciVOc code")

## TODO project_topic_table.csv
project_topic_table <- project_with_codes %>%
    select(projID,level1)%>%
    distinct()%>%
    mutate(value = 1)%>%
    pivot_wider( names_from = level1, values_from = value, values_fill = 0 ) 

colnames(project_topic_table) <- c("projID","nat_sci","soc_sci", "humani", "eng_tec", "med_heal", "agr_sci","not_spec")
project_topic_table <- project_topic_table %>%
    mutate(total = nat_sci + soc_sci + humani + eng_tec + med_heal + agr_sci + not_spec)%>%
    mutate(nat_sci  = if_else(nat_sci  > 0, nat_sci/total, 0.0)) %>%
    mutate(soc_sci  = if_else(soc_sci  > 0, soc_sci/total, 0.0)) %>%
    mutate(humani   = if_else(humani   > 0, humani/total, 0.0)) %>%
    mutate(eng_tec  = if_else(eng_tec  > 0, eng_tec/total, 0.0)) %>%
    mutate(med_heal = if_else(med_heal > 0, med_heal/total, 0.0)) %>%
    mutate(agr_sci = if_else(agr_sci > 0, agr_sci/total, 0.0)) 

colnames(project_topic_table)
project_topic_table %>% head()
project_topic_table %>% write_csv("./data/project_topic_esv_table.csv")


print("analysing call topics")


level_2_labels <- calls %>%
    select(group2,call_topic)%>%
    distinct(group2, .keep_all = TRUE)%>%
    arrange(group2)%>%
    write_csv("./data/project_codes_call_2.csv")	

level_1_labels <- read_excel('./data_keys/codes_call_1.xlsx')

calls <- merge(calls, level_1_labels, by="group")
calls %>%
    select(projID,group, description)%>%
    write_csv("./data/project_call.csv")

print("at this stage each project is associated with a level 1 label")
print(level_1_labels)
print("and a level 2 label")
print(level_2_labels)

print("Saving to projects file")
projects %>% write_csv("./data/projects.csv")


 

print("Update completed ;-D")