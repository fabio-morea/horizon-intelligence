
# procedure: 
#   first subset by topic
#   then calculate weight by year (on the subset only)
# 

source('./code/hi_0_parameters.R')
source('./code/hi_functions.R')

# libraries

library(tidyverse)
library(lubridate)
library(igraph)
library(communities)




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
participation <- participation %>% 
    filter(projID %in% selected_projects)  

selected_orgs <- orgs %>% 
    filter(orgID %in% unique(participation$orgID)) %>%
    distinct(orgID, .keep_all = TRUE)

selected_orgs  %>% write_csv(paste0(destination_path,"orgs.csv"))

selected_objectives <- objectives %>%
    filter(projID %in% selected_projects) 

selected_objectives  %>% 
    write_csv(paste0(destination_path,"prj_objectives.csv"))


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


######### network centrality measures ######### ######### 


# libraries

 
 
 

part <- participation
part %>% ggplot()+geom_histogram(aes(x = weight))

centrality_measures <- data.frame()

par(mar = rep(1,4))
ystart = min(part$year)
yend = max(part$year)
for (yy in ystart:yend ) {
    print(yy)
    part_y <- part %>% filter(year == yy)
    gi <- part_y %>%
        select(projID, orgID, weight) %>%
        mutate(weight = weight / 1000) %>%
        make_orgs_network(network_name = paste("Y", yy),
                          plot_network = F)
    
    V(gi)$deg <- degree(gi)
    V(gi)$str <- round(strength(gi),2)
    
    V(gi)$R_strength <- ifelse(V(gi)$str == 0, 0, V(gi)$str / max(V(gi)$str))  
    V(gi)$core <- coreness(gi)
    
    gi %>% igraph::write.graph(file = paste0(destination_path, yy, '.graphml' ), 
                               format = 'graphml')
    
    
    # save degree, strength and coreness to file
    df <- data.frame(year = paste0("Y", yy),
                     orgID = V(gi)$name,
                     degree = V(gi)$deg,
                     strength = V(gi)$str, 
                     R_strength = V(gi)$R_strength, 
                     
                     coreness = V(gi)$core)
    
    centrality_measures <- rbind(centrality_measures, df)
    
    
    # plot(gi,
    #      vertex.color =  "white",
    #      vertex.label = NA,
    #      vertex.size = 10*V(gi)$R_strength,
    #      edge.width = E(gi)$weight ,
    #      layout = layout.fruchterman.reingold(gi),
    #      main =  paste0("Y", yy)
    # )
    
    # to save in a different formt
    #as_long_data_frame(gi) %>% write_csv(paste0(destination_path,"df_Y", yy, '.csv'))
}

centrality_measures %>% write_csv(paste0(destination_path,'centrality_measures.csv'))

 

print("Done :-D")