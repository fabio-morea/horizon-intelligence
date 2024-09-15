#parameters for horizon intelligent scripts

#download
download_H2020 <- F
download_HEUROPE <- F

folder_H2020 <- "./data/H_2020_csv/"
folder_HEurope <- "./data/H_EU_csv/"

#network centrality measures and communities
miny <- 2015 
maxy <- 2029

# community detection
community_detection_method <- "WT"
param = 5 #steps

#filter 
# selected_esv_topic <- "electron microscopy"
# destination_path <- "./data/filtered-m/"
# visual_title <- "subset topic = 'electron microscopy'"

selected_esv_topic <- "hydrogen energy"
destination_path <- "./data/filtered-h/"
visual_title <- "subset topic = 'hydrogen energy'"

# 
# #https://cordis.europa.eu/project/id/101137192
# selected_esv_topic <- "pandemics"
# destination_path <- "./data/filtered-p/"
# visual_title <- "subset topic = 'pandemics'"