
# procedure: 
#   first subset by topic
#   then calculate weight by year (on the subset only)

################################################
################################################
 
selected_esv_topic <- "electron microscopy"
destination_path <- "./data/filtered-h/"

################################################
################################################

# libraries

library(tidyverse)
library(lubridate)


###################### function to calculate weight by year
# Function to calculate the number of days of project activity in a given year
days_in_year <- function(startDate, endDate, year) {
    year_start <- as.Date(paste0(year, "-01-01"))
    year_end <- as.Date(paste0(year, "-12-31"))
    
    if (endDate < year_start || startDate > year_end || F) {
        return(0)
    } else {
        actual_start <- max(startDate, year_start)
        actual_end <- min(endDate, year_end)
        return(as.numeric(difftime(actual_end, actual_start, units = "days")) + 1)
        
    }
}

################### load data 

print(paste("Start reading files..."))

orgs <- read_csv('./data/orgs.csv', show_col_types = TRUE) 

prjs <- read_csv('./data/project_topic_esv.csv', show_col_types = TRUE) %>%
    select(-level1) %>% rename(esv_topic = euroSciVocTitle)

objectives <- read_csv('./data/prj_objectives.csv', show_col_types = TRUE)  

participation <- read_csv('./data/participation.csv')

######## subset based on keyword

print("selecting a subset ")

projects <- prjs %>% 
    filter( str_detect(tolower(esv_topic), selected_esv_topic)) %>%
    #distinct( .keep_all = TRUE)  %>% 
    select(-esv_topic) 

projects %>% write_csv(paste0(destination_path, "prjs.csv"))

print(paste("selected projects: ", nrow(projects))) 

selected_projects <- unique(projects$projID)
participation <- participation %>% filter(projID %in% selected_projects)  

selected_orgs <- orgs %>% 
    filter(orgID %in% unique(participation$orgID)) %>%
    distinct(orgID, .keep_all = TRUE)

selected_orgs  %>% write_csv(paste0(destination_path,"orgs.csv"))


df <- left_join(participation, projects, by = 'projID') %>%
    filter(startDate > 0, endDate > 0, !is.na(startDate), !is.na(endDate))  


# Apply the function to each row and each year
for(year in 2014:2028) {
    print(paste("calculating weight by year...", year))
    df[[as.character(year)]] <- mapply(days_in_year, df$startDate, df$endDate, MoreArgs = list(year = year))
}

participation <- df %>%
    pivot_longer(cols = `2014`:`2028`, names_to = "year_weight", values_to = "days_active" ) %>%
    group_by(projID) %>%
    mutate(
        project_duration = as.numeric(difftime(endDate, startDate, units = "days")) + 1,
        normalized_by_project_duration = days_active / project_duration
    ) %>%
    ungroup() %>%
    mutate(weight = round(ecContribution * normalized_by_project_duration, 3)) %>%
    mutate(year = year_weight)%>%
    select(-days_active,-project_duration,-normalized_by_project_duration, -year_weight, -startDate, -endDate)%>% 
    filter(weight > 0) 

participation %>% write_csv(paste0(destination_path,'participation.csv'))

 

print("Done :-D")