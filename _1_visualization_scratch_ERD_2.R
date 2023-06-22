load("./DATA/PROCESSED/_0_merged_CDC_NEON.Rdata")



## Scatter plot of max Culex for the year and cases for the year


df_merged <- df_total_counts_by_county %>%
  ungroup() %>%
  select(FullGeoName,Year,starts_with("mos")) %>%
  group_by(FullGeoName,Year) %>%
  summarise(across(starts_with("mos"),max),.groups = "keep") %>%
  ungroup() %>%
  left_join(df_patho_by_county %>% ungroup() %>% select(FullGeoName,Year,frac_pos_test)) %>%
  left_join(df_bird_by_county %>% ungroup() %>% 
              select(FullGeoName,Year,count) %>% rename(all_bird_count=count),by=c("FullGeoName","Year")) %>%
  full_join(df_cases_by_county,by=c("FullGeoName","Year"))

df_merged %>%
  ggplot(aes(x=mos_Culex_all,y=`Reported human cases`)) + geom_hex() +
  scale_x_continuous(trans="sqrt") +
  theme_bw() +
  labs(x="Culex individuals")


v_hot_counties <- df_merged %>%
  group_by(FullGeoName) %>%
  summarise(`Reported human cases`=sum(`Reported human cases`)) %>%
  filter(`Reported human cases`>=3) %>%
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
  mutate(FLAV_POSITIVE = frac_pos_test>0) %>%
  mutate(FLAV_POSITIVE = factor(FLAV_POSITIVE,levels=c("TRUE","FALSE")) %>% addNA()) %>%
  ggplot(aes(x=mos_all,y=`Reported human cases`)) + 
  # geom_point(aes(pch=FLAV_POSITIVE,size=FLAV_POSITIVE,alpha=FLAV_POSITIVE)) +
  geom_jitter(aes(pch=FLAV_POSITIVE,
                  size=FLAV_POSITIVE,
                  alpha=FLAV_POSITIVE,
                  color=FLAV_POSITIVE),
              width=0.15,height=0.15) +
  scale_x_continuous(trans="sqrt") +
  scale_shape_manual(breaks=c(TRUE,FALSE,NA),values=c(3,1,0)) +
  scale_color_manual(breaks=c(TRUE,FALSE,NA),values=c("red","blue","green")) +
  scale_size_manual(breaks=c(TRUE,FALSE,NA),values=c(4,3,3)) +
  scale_alpha_manual(breaks=c(TRUE,FALSE,NA),values=c(1,.5,.5)) +
  theme_bw() +
  labs(x="total Mos individuals")


df_merged %>%
  filter(FullGeoName %in% v_hot_counties) %>%
  mutate(FLAV_POSITIVE = frac_pos_test>0) %>%
  mutate(FLAV_POSITIVE = factor(FLAV_POSITIVE,levels=c("TRUE","FALSE")) %>% addNA()) %>%
  ggplot(aes(x=mos_Culex_all,y=all_bird_count)) + 
  # geom_point(aes(pch=FLAV_POSITIVE,size=FLAV_POSITIVE,alpha=FLAV_POSITIVE)) +
  geom_jitter(aes(pch=FLAV_POSITIVE,
                  size=FLAV_POSITIVE,
                  alpha=FLAV_POSITIVE,
                  color=FLAV_POSITIVE),
              width=0.15,height=0.15) +
  scale_x_continuous(trans="sqrt") +
  scale_shape_manual(breaks=c(TRUE,FALSE,NA),values=c(3,1,0)) +
  scale_color_manual(breaks=c(TRUE,FALSE,NA),values=c("red","blue","green")) +
  scale_size_manual(breaks=c(TRUE,FALSE,NA),values=c(4,3,3)) +
  scale_alpha_manual(breaks=c(TRUE,FALSE,NA),values=c(1,.5,.5)) +
  theme_bw() +
  labs(x="mos (Culex counts)",y="birds (all counts)")


df_merged %>%
  filter(FullGeoName %in% v_hot_counties) %>%
  mutate(FLAV_POSITIVE = frac_pos_test>0) %>%
  mutate(FLAV_POSITIVE = factor(FLAV_POSITIVE,levels=c("TRUE","FALSE")) %>% addNA()) %>%
  ggplot(aes(x=Year,y=mos_Culex_all)) + 
  geom_line() +
  # geom_point(aes(pch=FLAV_POSITIVE,size=FLAV_POSITIVE,alpha=FLAV_POSITIVE)) +
  geom_point(aes(pch=FLAV_POSITIVE,
                  size=FLAV_POSITIVE)) +
  scale_shape_manual(breaks=c(TRUE,FALSE,NA),values=c(3,1,0)) +
  scale_color_manual(breaks=c(TRUE,FALSE,NA),values=c("red","blue","green")) +
  scale_y_continuous(trans="sqrt") +
  theme_bw() +
  labs(y="Culex individuals") + facet_wrap(~FullGeoName)
