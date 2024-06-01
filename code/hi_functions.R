
#######################################

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

################### functions network
################### 


make_orgs_network <- function(participation, network_name = '', plot_network = FALSE){
    # input:  a long dataframe of edges (bimodal network): org, project weight
    # output: a long dataframe of edges (one-mode network): org1, org2, weight
    
    # B is the bi-adjacency matrix (orgs x projects)
    tmp <- participation %>% pivot_wider(
        names_from = projID, 
        values_from = weight, 
        values_fill = 0,
        values_fn = sum)
    
    B <- tmp %>% 
        select(-orgID) %>% 
        as.matrix()
    
    # projection as matrix product, distributes the weight among orgs
    P <- B %*% t(B)
    
    diag(P) <- 0 #remove self-loop
    
    
    rownames(P) <- tmp$orgID
    colnames(P)<- tmp$orgID
    
    df_P <- data.frame(P) %>% 
        mutate(org1 = as.character(rownames(P))) %>% 
        pivot_longer( cols = -starts_with( c('org1', 'org2')), names_to = 'org2',  values_to = 'weight'  ) %>%
        mutate(weight = sqrt(weight)/2)
    
    g_orgs <- igraph::simplify(graph_from_data_frame(df_P, directed = FALSE))
    delete.edges (g_orgs, which (E (g_orgs)$weight==0))
    
    if (plot_network){
        plot(g_orgs, main = network_name, 
             vertex.label = NA,
             vertex.size = 1,
             vertex.color = "green",
             vertex.shape = "circle")
    }
    
    return(g_orgs)
}
#######################################
#######################################

print("functions loaded successfully")
