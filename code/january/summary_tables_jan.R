



#importing telegram data on secondary market
telegram_regions_jan <- read_excel(here("data", "proc", "january", "telegram_regionsjan_jan.xlsx"))

#joining telegram and olx data on secondary market 

secondary_data_jan_all <- bind_rows(telegram_regions_jan, secondary_market_jan) %>% 
  mutate(region = recode(region, 
                         "Фаргона" = "Фарғона"))

#creating dataset for secondary market   
write_xlsx(secondary_data_jan_all, "secondary_data_jan_all.xlsx")

price_secondary_market_jan  <- secondary_data_jan_all %>% 
  group_by(region) %>% 
  summarise(mean_jan = mean(price_m2),
            median_jan= median(price_m2),
            num_obs_jan = n(),
            min_jan = min(price_m2), 
            max_jan= max(price_m2)) 

write_xlsx(price_secondary_market_jan, "price__secondary_jan_regions.xlsx")


price_secondary_market_tashkent_jan <-  secondary_data_jan_all %>%
  filter(region == "Тошкент", condition != "Евроремонт",
         condition != "Авторский проект") %>%  
  group_by(city) %>% 
  summarise(mean = mean(price_m2),
            median= median(price_m2),
            num_obs = n(),
            min = min(price_m2), 
            max= max(price_m2)) 

write_xlsx(price_secondary_market_tashkent_jan, "price_jan_tash_secondary.xlsx")

#primary market
primary_price_reg_jan <- primary_market_jan %>% 
  group_by(region) %>% 
  summarise(mean = mean(price_m2),
            median= median(price_m2),
            num_obs = n(),
            min = min(price_m2), 
            max= max(price_m2))

#primary_market_tashkent
primary_price_tash_jan <- primary_market_jan %>% 
  select(region, month, price_m2 = price_true, price, area_full=area, num_rooms, condition, city) %>% 
  filter(region == "Тошкент", condition != "Евроремонт",
         condition != "Авторский проект"
         ) %>% 
  #ggplot(aes(price_m2))+
  #geom_histogram()+
  #facet_wrap(~city)
  group_by(city) %>% 
  summarise(mean_jan = mean(price_m2),
            median_jan= median(price_m2),
            num_obs_jan = n(),
            min_jan = min(price_m2), 
            max_jan= max(price_m2)) 

write_xlsx(primary_price_tash_jan, "price_jan_tash_primary.xlsx")



dec_jan_tash_primary <- primary_price_tash_jan %>% left_join(primary_price_tash_dec)
