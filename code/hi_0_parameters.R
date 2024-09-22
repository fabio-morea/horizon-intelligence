### 
### Horizon Intelligence 
### developed by Fabio Morea, Area Science Park, Trieste, Italy.
### contact: fabio.morea@areasciencepark.it
### ORCID: 0000-0002-2034-2951
###  

### PARAMETERS and choice of case study 

#download
download_H2020 <- F
download_HEUROPE <- F

folder_H2020 <- "./data/H_2020_csv/"
folder_HEurope <- "./data/H_EU_csv/"


case_studies <- list(
    "hydrogen" = list(
        selected_esv_topic = "hydrogen energy",
        destination_path = "./data/filtered-h/",
        visual_title = "subset topic = 'hydrogen energy'",
        miny = 2015, 
        maxy = 2029,
        community_detection_method = "WT",
        param = 5 #steps
    ),
    "microscopy" = list( 
        selected_esv_topic = "electron microscopy",
        destination_path = "./data/filtered-m/",
        visual_title = "subset topic = 'electron microscopy'",
        miny = 2015, 
        maxy = 2029,
        community_detection_method = "WT",
        param = 5 #steps
    ),
    "pandemics" = list( 
        selected_esv_topic = "pandemics",
        destination_path = "./data/filtered-p/",
        visual_title = "subset topic = 'pandemics'",
        miny = 2015, 
        maxy = 2029,
        community_detection_method = "WT",
        param = 5 #steps
    )
    
)


select_case_study <- function(case_name) {
    if (!case_name %in% names(case_studies)) {
        stop("Invalid case study name. Choose from: ", paste(names(case_studies), collapse = ", "))
    }
    return(case_studies[[case_name]])
}


############# SELECT THE APPROPRIATE CASE STUDY ##################

 
case <- select_case_study("hydrogen")  


# apply to variables that are used in other parts of the code
# 
selected_esv_topic <- case$selected_esv_topic
destination_path <- case$destination_path
visual_title <- case$visual_title
miny <-  case$miny
maxy <- case$maxy
community_detection_method <- case$community_detection_method 
param = case$param
