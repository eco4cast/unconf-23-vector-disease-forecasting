load("./DATA/PROCESSED/_0_merged_CDC_NEON.Rdata")



## Scatter plot of max Culex for the year and cases for the year


df_merged <- df_total_counts_by_county %>%
  ungroup() %>%
  select(FullGeoName,Year,starts_with("mos")) %>%
  group_by(FullGeoName,Year) %>%
  summarise(across(starts_with("mos"),max),.groups = "keep") %>%
  ungroup() %>%
  left_join(df_patho_by_county %>% ungroup() %>% select(FullGeoName,Year,frac_pos_test)) %>%
  full_join(df_cases_by_county,by=c("FullGeoName","Year"))

df_merged %>%
  ggplot(aes(x=mos_Culex_all,y=`Reported human cases`)) + geom_hex() +
  scale_x_continuous(trans="sqrt") +
  theme_bw() +
  labs(x="Culex individuals")


v_hot_counties <- df_merged %>%
  group_by(FullGeoName) %>%
  summarise(`Reported human cases`=sum(`Reported human cases`)) %>%
  filter(`Reported human cases`>5) %>%
  pull(FullGeoName)
  


df_merged %>%
  filter(FullGeoName %in% v_hot_counties) %>%
  ggplot(aes(x=mos_Culex_all,y=`Reported human cases`)) + geom_hex() +
  scale_x_continuous(trans="sqrt") +
  theme_bw() +
  labs(x="Culex individuals")


df_merged %>%
  filter(FullGeoName %in% v_hot_counties) %>%
  ggplot(aes(x=mos_Culex_all,y=`Reported human cases`)) + geom_point(pch=3,alpha=.8) +
  scale_x_continuous(trans="sqrt") +
  theme_bw() +
  labs(x="Culex individuals")


df_merged %>%
  # filter(FullGeoName %in% v_hot_counties) %>%
  ggplot(aes(x=mos_Culex_all,y=`Reported human cases`)) + geom_point(pch=3,alpha=.8) +
  scale_x_continuous(trans="sqrt") +
  theme_bw() +
  labs(x="Culex individuals")

df_merged %>%
  filter(FullGeoName %in% v_hot_counties) %>%
  ggplot(aes(x=mos_all,y=`Reported human cases`)) + geom_point(pch=3,alpha=.8) +
  scale_x_continuous(trans="sqrt") +
  theme_bw() +
  labs(x="total Mos individuals")
