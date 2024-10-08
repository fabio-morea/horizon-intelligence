### 
### Horizon Intelligence 
### developed by Fabio Morea, Area Science Park, Trieste, Italy.
### contact: fabio.morea@areasciencepark.it
### ORCID: 0000-0002-2034-2951
###  


source('./code/hi_0_parameters.R')
source('./code/hi_functions.R')

# libraries

library(tidyverse)
library(lubridate)
library(igraph)
library(communities)

################### load data 

print(paste("Start reading files..."))

info <- read_csv('./data/latest_update.csv') %>%
    mutate(visual_title, selected_esv_topic, miny, maxy) 

info %>% write_csv(paste0(destination_path, "info.csv")) 


orgs <- read_csv('./data/orgs.csv', show_col_types = TRUE) 

prjs <- read_csv('./data/project_topic_esv.csv', show_col_types = TRUE) %>%
    select(-level1) %>% rename(esv_topic = euroSciVocTitle)

objectives <- read_csv('./data/prj_objectives.csv', show_col_types = TRUE)  

participation <- read_csv('./data/participation.csv') %>%
    mutate(totalCost = round(totalCost, 3)) %>%
    arrange(totalCost)

if ( selected_esv_topic == "hydrogen energy"){
    AIcategories <- read_delim(paste0(destination_path,'AI_tech_market.csv'),
                               show_col_types = TRUE) %>%
        select(projID, tech)
    
}


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

# data enriched by AI generated categries

if ( selected_esv_topic == "hydrogen energy"){
    projects <- merge(projects, AIcategories, by = "projID")
}

selected_orgs <- orgs %>% 
    filter(orgID %in% unique(participation$orgID)) %>%
    distinct(orgID, .keep_all = TRUE)

selected_orgs  %>% write_csv(paste0(destination_path,"orgs.csv"))

selected_objectives <- objectives %>%
    filter(projID %in% selected_projects) 

selected_objectives  %>% 
    write_csv(paste0(destination_path,"prj_objectives.csv"))

prjs %>% select(projID, esv_topic) %>% 
    filter(projID %in% selected_projects) %>%
    write_csv(paste0(destination_path, "esv_topics.csv")) 

df <- left_join(participation, projects, by = 'projID') %>%
    filter(startDate > 0, endDate > 0, !is.na(startDate), !is.na(endDate))  


# Apply the function to each row and each year
for(year in miny:maxy) {
    print(paste("calculating weight by year...", year))
    df[[as.character(year)]] <- mapply(days_in_year, df$startDate, df$endDate, MoreArgs = list(year = year))
}

participation <- df %>%
    pivot_longer(cols = `2015`:`2029`, names_to = "year_weight", values_to = "days_active" ) %>%
    group_by(projID) %>%
    mutate(
        project_duration = as.numeric(difftime(endDate, startDate, units = "days")) + 1,
        normalized_by_project_duration = days_active / project_duration
    ) %>%
    ungroup() %>%
    
    mutate(w_total_cost = round(totalCost * normalized_by_project_duration, 3)) %>%
    mutate(w_ec_contrib = round(netEcContribution * normalized_by_project_duration, 3)) %>%
    
    mutate(year = year_weight)%>%
    select(-days_active,-project_duration,-normalized_by_project_duration, -year_weight, -startDate, -endDate)%>% 
    mutate(perc = w_ec_contrib/w_total_cost)%>%
    filter(w_total_cost > 1.0) %>%
    filter(w_ec_contrib > 1.0) %>%
    filter(perc > .01)


participation %>% write_csv(paste0(destination_path,'participation.csv'))


######### network centrality measures ######### ######### 
part <-participation

# select the weight
part <- part %>%
    mutate(weight = w_ec_contrib)
#mutate(weight = w_total_cost)


centrality_measures <- data.frame()
centrality_measures_tech <- data.frame()
centrality_measures_market <- data.frame()


par(mar = rep(1,4))
ystart = min(part$year)
yend = max(part$year)

for (yy in ystart:yend ) {
    print(paste("Processing year ",yy, "creating network Gy"))
    part_y <- part %>% filter(year == yy) 
    
    
    ############ AI enriched categories in graph ##########
    if ( selected_esv_topic == "hydrogen energy"){
        
        g_tech <- part_y %>%
            select(projID, orgID, weight, tech) %>%
            filter(tech == TRUE) %>%
            make_orgs_network(network_name = paste("Y", yy))
        E(g_tech)$tech <- TRUE
        E(g_tech)$weight <- E(g_tech)$weight %>% round(3)
        g_tech <- delete.edges(g_tech, which(E(g_tech)$weight <.001) )
        dft<-igraph::as_data_frame(g_tech, what = "edges")
        
        
        g_market <- part_y %>%
            select(projID, orgID, weight, tech) %>%
            filter(tech == FALSE) %>%
            make_orgs_network(network_name = paste("Y", yy))
        E(g_market)$tech <- FALSE
        E(g_market)$weight <- E(g_market)$weight %>% round(3)
        g_market <- delete.edges(g_market, which(E(g_market)$weight <.001) )
        dfm<-igraph::as_data_frame(g_market, what = "edges")
        
        dfi <- rbind(dfm,dft)
        gi <- graph_from_data_frame(dfi, directed = FALSE)
        
    } else {
        gi <- part_y %>%
            select(projID, orgID, weight) %>%
            mutate(weight = weight / 1000) %>%
            make_orgs_network(network_name = paste("Y", yy))
        
    }
    
    
    
    ########################################
    print("centrality measures")
    E(gi)$weight <- E(gi)$weight %>% round(6)   
    gi <- delete_edges(gi, E(gi)[weight <= .0001])
    gi <- delete_vertices(gi, V(gi)[degree(gi) == 0])
    if(vcount(gi)==0){next}
    
    V(gi)$deg <- degree(gi)
    V(gi)$str <- strength(gi)
    
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
    
    if ( selected_esv_topic == "hydrogen energy"){
        
        V(g_tech)$deg <- degree(g_tech)
        V(g_tech)$str <- strength(g_tech)
        V(g_tech)$R_strength <- ifelse(
            V(g_tech)$str == 0, 0, V(g_tech)$str / max(V(g_tech)$str))  
        V(g_tech)$core <- coreness(g_tech)
        df_tech <- data.frame(year = paste0("Y", yy),
                              orgID = V(g_tech)$name,
                              degree = V(g_tech)$deg,
                              strength = V(g_tech)$str, 
                              R_strength = V(g_tech)$R_strength, 
                              coreness = V(g_tech)$core)
        centrality_measures_tech <- rbind(centrality_measures_tech, df_tech)
        
        V(g_market)$deg <- degree(g_market)
        V(g_market)$str <- strength(g_market)
        V(g_market)$R_strength <- ifelse(
            V(g_market)$str == 0, 0, V(g_market)$str / max(V(g_market)$str))  
        V(g_market)$core <- coreness(g_market)
        df_market <- data.frame(year = paste0("Y", yy),
                                orgID = V(g_market)$name,
                                degree = V(g_market)$deg,
                                strength = V(g_market)$str, 
                                R_strength = V(g_market)$R_strength, 
                                coreness = V(g_market)$core)
        centrality_measures_market <- rbind(centrality_measures_market, df_market)
        
    }
}



centrality_measures <- centrality_measures %>% mutate(scope = "all")
centrality_measures_tech <- centrality_measures_tech %>% mutate(scope = "tech")
centrality_measures_market <- centrality_measures_market %>% mutate(scope = "market")

centrality_measures <- rbind(centrality_measures,centrality_measures_tech)
centrality_measures <- rbind(centrality_measures,centrality_measures_market)

centrality_measures <- centrality_measures %>%
    mutate(yy = substr(year, 2, 5) )
centrality_measures%>% 
    write_csv(paste0(destination_path,'centrality_measures.csv'))

centrality_measures %>% 
    #filter(scope == "all") %>%
    ggplot()+
    geom_boxplot(aes(x = yy, y = coreness, group = yy))

print("Done :-D")




