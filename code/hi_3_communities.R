# Author: Fabio Morea @ Area Science Park
# Package: Horizon-intelligence
# script: communities over time

# input:
# clean data from CORDIS 

# output
# community detection by year

# SPDX-License-Identifier: CC-BY-4.0
# GitLab: https://gitlab.com/fabio-morea/

# devtools::install_github('fabio-morea/')

# libraries
#options(warn = -1)
library(tidyverse)
library(igraph)
library(aricode)

source('./code/hi_0_parameters.R')
source('./code/hi_functions.R')

co_occurrence <- function(M, names, alpha) {
    # calculates normalized co-occurrence matrix
    # using the results of explore_communities
    
    n_trials <- length(alpha)
    n_nodes <- nrow(M)
    CO <- matrix(0, nrow = n_nodes, ncol = n_nodes)
    #colnames(CO) <- names
    
    for (t in (1:n_trials)) {
        print(t)
        nclusters <- max(M[, t])
        for (k in 1:nclusters) {
            samecluster <- (which(M[, t] == k))
            nc <- length(samecluster)
            for (i in 1:nc) {
                for (j in (i+1):nc) {
                    CO[samecluster[j], samecluster[i]] <- CO[samecluster[j], samecluster[i]] + alpha[t]
                    CO[samecluster[i], samecluster[j]] <- CO[samecluster[j], samecluster[i]]
                }
            }
        }
    }
    #X_normalized <- CO / n_trials
    #diag(X_normalized) <- 1.0
    
    return (CO)
}

yy = 2024
results <- data.frame()
for (yy in miny:maxy) {
    print(yy)
    gy <- igraph::read.graph(
        file = paste0(destination_path, yy, '.graphml'),format = 'graphml')
    
    # there are multiple edges (using AI-generated boolean flag E(gy)$tech)
    gy <- igraph::simplify(gy, edge.attr.comb = 'sum')
    
    # remove thin connections 
    #min_weight <- quantile( E(gy)$weight, .1)
    #gy <- delete_edges(gy, E(gy)[weight < min_weight])
    
    # remove singletons
    #gy <- delete_vertices(gy, V(gy)[degree(gy) <= 1])
    
    print(paste(vcount(gy), ecount(gy)))
    
    hist(E(gy)$weight)
    
    
    if(vcount(gy)==1){next}
    
    
    ssp <- communities::solutions_space(gy,
                                        n_trials = 200,
                                        met = community_detection_method, 
                                        IM.nb.trials = 1)
    
    communities::plot_sol_space(ssp)$pl2 +
        ggtitle(paste("Solution space year", yy, "steps = ", param))
    
    n_solutions <- nrow(ssp$data) 
    if (ssp$data$cumsum[1] >= 0.5) {
        if (n_solutions==1){
            mm <- ssp$M 
            print(paste0(yy, ": single solution."))
        } else {
            mm <- ssp$M[,1]
            print(paste0(yy, ": dominant solution."))
        }
        
        nc <- length(table(mm))
        V(gy)$community <- mm
        community_sizes <- table(V(gy)$community)
        V(gy)$community_size <- community_sizes[as.character(V(gy)$community)]
        print(paste0(yy, "  NC = ", nc))
        
    } else {
        print(paste0(yy, ": multiple solutions: consensus!"))
        # use ssp$M to perform consensus
        M <- ssp$M 
        names <- V(gy)$name
        dd<-ssp$data
        
        
        # prune solutions that have cumsum > 50% 
        prune_idx <- which(ssp$data$cumsum > 0.5)[1] 
        dd<- dd %>% head(prune_idx)
        M <- M[,1:prune_idx]
        
        n<-dd$a+dd$b-2
        k<- n-dd$b+1
        alpha <- k/ sum(k)
        D <- co_occurrence(M, V(gy)$name, alpha)
        D <- D %>% round(3)
        diag(D)<-1.0
        colnames(D)<- names
        rownames(D)<- names
        
        comms<-CCD::consensus_communities(D, p =.8)
        
        #aggregate singletons in community 0
        #comms$community[ comms$community_size == 1] <- 0
        
        nc <- max(comms$cons_comm_label)
        V(gy)$community <- comms$cons_comm_label
        community_sizes <- table(V(gy)$community)
        V(gy)$community_size <- community_sizes[as.character(V(gy)$community)]
        print(paste0(yy, ": consensus done. NC = ", nc))
        print(community_sizes)
    }
    
    gy %>% write.graph(file = paste0(destination_path,yy,'.graphml'),
                       format = 'graphml')
    df<- data.frame(orgID = V(gy)$name) 
    df$community <- V(gy)$community
    df$community_size <- V(gy)$community_size
    df$year = yy
    df$weight <- strength(gy) 
    results <- rbind(results, df)
}

########## 

results %>% 
    select(year, orgID, weight , community, community_size) %>%
    write_csv(paste0(destination_path,"ntwk_communities.csv"))

comms <- results %>%
    mutate(CommY = paste0("temp", year, "_", community)) %>%
    rename(w = weight)  %>%
    filter(w > 0)   %>%
    arrange(year, w)



 




print(paste("Analysing communities from", miny, "to", maxy))

evolution <- data.frame()


for (yy in (miny + 1):maxy) {
    print(paste("Processing year ", yy))
    last_year <- which(comms$year == (yy - 1))
    this_year <- which(comms$year == yy)
    com_last_year <- sort(unique(comms$CommY[last_year]))
    com_this_year <- sort(unique(comms$CommY[this_year]))
    
    
    
    for (i in 1:length(com_last_year)) {
        print(i)
        for (j in 1:length(com_this_year)) {
            #print(paste("--- checking ", com_last_year[i], com_this_year[j]))
            label_i <- substr(com_last_year[i], 10, 13)
            label_j <- substr(com_this_year[j], 10, 13)
            
            ci <- comms %>% filter(comms$year == (yy - 1) & comms$community == label_i) 
            cj <- comms %>% filter(comms$year == (yy) & comms$community == label_j) 
            if (nrow(ci) * nrow(cj) > 0) {
                intersectionIDs <- intersect(ci$orgID, cj$orgID)
                x <- ci %>% filter(orgID %in% intersectionIDs)
                if(sum(x$w)==0){next}
                if ((nrow(x) > 0) & (sum(ci$w) > 0) & (sum(cj$w) > 0)) {
                    
                    xi <- sum(x$w) / sum(ci$w) + 0
                    xj <- sum(x$w) / sum(cj$w) + 0
                    print(paste("assessing intersection between", 
                                com_last_year[i], xi, "and", com_this_year[j], xj))
                    
                    if ((xi > 0.5) & (xj > 0.5)) {
                        print(paste("SAME COMMUNITY over time!!", 
                                    com_last_year[i], com_this_year[j]))
                        evolution_type <- "Continue"
                        #dict$global_label[ dict$temp_label == com_this_year[j] ] <- dict$global_label[ dict$temp_label == com_last_year[i]]
                    } else if ((xi <= 0.5) & (xj <= 0.5)) {
                        print(paste("Shuffle: a small part of the community in year", yy - 1, "becomes a small part or another community in year ", yy))
                        evolution_type <- "_shuffle"
                        #GlobalComLabel <- GlobalComLabel + 1
                        #dict$global_label[ dict$temp_label == com_this_year[j] ] <- GlobalComLabel
                    } else if ((xi > 0.5) & (xj <= 0.5)) {
                        print(paste("MERGE: most of the community in year", yy - 1, "becomes a minor part or another community in year ", yy))
                        evolution_type <- "Merge"
                        # GlobalComLabel <- GlobalComLabel + 1
                        # dict$global_label[ dict$temp_label == com_this_year[j] ] <- GlobalComLabel
                    } else if ((xi <= 0.5) & (xj > 0.5)) {
                        print(paste("SPLIT: a small part of the community in year", yy - 1, "becomes the largest part or another community in year ", yy))
                        evolution_type <- "Split"
                        # GlobalComLabel <- GlobalComLabel + 1
                        # dict$global_label[ dict$temp_label == com_this_year[j] ] <- GlobalComLabel
                    }
                } else {
                    if (dict$global_label[ dict$temp_label == com_last_year[i]] == "X"){
                        print(paste("from X", com_last_year[i], com_this_year[j]))
                        evolution_type <- "fromX"
                    } else if (dict$global_label[ dict$temp_label == com_this_year[j]] == "X"){
                        print(paste("to X", com_last_year[i], com_this_year[j]))
                        evolution_type <- "toX"
                    } else {
                        print("No intersection")
                        #next
                    }
                    
                    
                }
                
                evolution <- rbind(
                    evolution,
                    data.frame(
                        y1 = yy - 1,
                        y2 = yy,
                        CY1 = com_last_year[i],
                        CY2 = com_this_year[j],
                        type = evolution_type,
                        w = sum(x$w)
                    )
                )
                
            }
        }
    }
}


# relabels

GlobalComLabel <- 0



dict <- comms %>% 
    arrange(CommY) %>% 
    distinct(CommY) %>% 
    mutate(temp_label = CommY) %>% 
    select(temp_label) %>%
    mutate(global_label="X")



for (test_comm in dict$temp_label){
    print(paste("Processing ", test_comm))
    #year <- test_comm %>% substr(5,8) %>% as.numeric()
    mask<-evolution$CY2 == test_comm
    if (sum(mask)==0){#no predecessors
        GlobalComLabel <- GlobalComLabel + 1
        dict$global_label[ dict$temp_label == test_comm ] <- GlobalComLabel
        next
    } else { # some predecessors exist
        type<- evolution$type[mask]
        preds<-    evolution %>% filter(CY1 %in% evolution$CY1[mask])
        print(paste("there are some predecessors:", nrow(preds)))
        print(preds)
        relevant_preds <- preds %>% 
            filter(type %in% c("Continue", "Merge", "Split"))%>%
            arrange(-w)
        print(relevant_preds)
        
        if (nrow(relevant_preds) > 0){
            pred_global_label<- 
                dict$global_label[ dict$temp_label == relevant_preds$CY1[1] ]
            print(relevant_preds[1,])
            
            dict$global_label[ dict$temp_label == test_comm ] <- pred_global_label
            next
        } else {
            
        }
        
        GlobalComLabel <- GlobalComLabel + 1
        dict$global_label[ dict$temp_label == test_comm ] <- GlobalComLabel
        next
    }
    
    
    
    
    
    
}


dict1 <- dict %>%
    mutate(CY1 = temp_label, GCL1 = global_label) %>%
    select(CY1, GCL1)
evolution <- evolution %>% merge(dict1) 


dict2 <- dict %>%
    mutate(CY2 = temp_label, GCL2 = global_label) %>%
    select(CY2, GCL2)
evolution <- evolution %>% merge(dict2) 

evolution %>% write_csv(paste0(destination_path,"evolution.csv"))

comms <- comms %>%
    rename(temp_label = CommY)%>%
    left_join(dict, by = 'temp_label')

comms %>% write_csv(paste0(destination_path,"communities_over_time.csv"))

c_table <- comms %>% 
    select(orgID, year,global_label) %>%
    pivot_wider(names_from = year, values_from = global_label)

c_table %>% write_csv(paste0(destination_path,"communities_table.csv"))

ct1 <- comms %>% 
    select(orgID, year, global_label) %>% 
    group_by(year, global_label) %>% 
    summarize(count = n(), .groups = 'drop') %>% 
    pivot_wider(names_from = year, values_from = count, values_fill = 0)

ct1 %>% write_csv(paste0(destination_path,"communities_size_table.csv"))

# View the resulting table
print(ct1)


#assign global labels to yearly graphs and save
for (yy in miny:maxy){
    print(yy)
    gy <- read.graph(file = paste0(destination_path, yy,'.graphml'),format = 'graphml')
    comm_labels <- comms %>% filter(year==yy) %>% select(orgID, global_label)
    
    ordered_labels <- comm_labels[match(V(gy)$name, comm_labels$orgID), ]
    
    V(gy)$GlobalCommunityLabel <- ordered_labels$global_label
    
    gy %>% igraph::write.graph(file = paste0(destination_path,'yy','.graphml'),
                               format = 'graphml')
    
}


print("Done :-D")