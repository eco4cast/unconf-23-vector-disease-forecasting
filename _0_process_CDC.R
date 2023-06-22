library(tidyverse)
setwd("~/Library/CloudStorage/GoogleDrive-edeyle@bu.edu/My Drive/GALILE(c)O/Projects/Vector Born Disease Dynamics")

# list all files in a folder
L_files <- list.files("./Data/CDC West Nile By County/") %>% str_subset("by county for year selected above")

# smash it all together
df_cases_by_county <- map_dfr(L_files,function(file_i){
  read_csv(paste0("./Data/CDC West Nile By County/",file_i)) %>% 
    select(FullGeoName,County,Year,Activity,
           `Reported human cases`,
           `Neuroinvasive disease cases`) %>%
    mutate(County = as.numeric(County))
})

save(df_cases_by_county,file="./DATA/PROCESSED/CDC_WNV_cases_by_county.Rdata")
