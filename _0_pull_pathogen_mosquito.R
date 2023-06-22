# don't forget to set NEON_TOKEN
library(duckdbfs)
library(neonstore)
library(dplyr)
library(lubridate)

if(!file.exists("./DATA/PROCESSED/mos_pathogen_all_sites.Rdata")){
  df <-  neonstore:::neon_data(product = "DP1.10041.001",
                               start_date = "2014-01-01", 
                               end_date = "2022-01-01",
                               site = NA,
                               type="basic"
  )
}else{
  load("./DATA/PROCESSED/mos_pathogen_all_sites.Rdata")
}

# does "bout" mean sampling season or finer scale; all plots within a restricted window?


# ds_patho : taxonID, testPathogenName, sequenceDatabaseID, startCollectDate, endCollectDate, testResult, cqValue


urls_patho <- df |>
  dplyr::filter(grepl("mos_pathogenresults", name)) |>
  pull(url)

ds_patho <- duckdbfs::open_dataset(urls_patho,
                                  format="csv",
                                  filename = TRUE,
                                  unify_schemas = FALSE) %>% {.}

ds_patho_flav <- ds_patho %>%
  select(siteID, taxonID, testPathogenName, sequenceDatabaseID, startCollectDate, endCollectDate, testResult, cqValue) %>%
  filter(str_detect(testPathogenName,"Flavivirus"))

df_patho_flav <- ds_patho_flav %>%
  mutate(testResult = (testResult == "Positive")) %>%
  collect()
  
df_patho_flav_by_site <- df_patho_flav %>%
  mutate(Year=year(startCollectDate)) %>%
  group_by(Year,siteID) %>%
  summarise(frac_pos_test=sum(testResult)/n(),.groups = "keep")


save(df,
     df_patho_flav_by_site,
     file="./DATA/PROCESSED/mos_pathogen_all_sites.Rdata")

# ds %>% select(siteID,setDate,collectDate,proportionIdentified)

# left join mos_expertTaxonomistIDProcessed with mos_sorting taking by subsampleID
# mos_sorting; subsampleID, proportionIdentified
# mos_expertTaxonomistIDProcessed; subsampleID, taxonID, scientificName, individualCount

