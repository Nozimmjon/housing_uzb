


telegram_regions_dec <- read_excel(here("data", "proc", "dec", "telegram_regions.xlsx")) %>% 
  filter(month == "дек") %>% 
  mutate(region = recode(region, 
                         "Фаргона" = "Фарғона"))

#joining telegram and olx data on secondary market 

secondary_data_dec_all <- bind_rows(telegram_regions_dec, secondary_market_dec)  
 

#creating dataset for secondary market   
write_xlsx(secondary_data_dec_all, "secondary_data_dec_all.xlsx")

price_secondary_market_dec  <- secondary_data_dec_all %>% 
  #group_by(region) %>% 
  filter(region == "Жиззах", price_m2 < 600) %>% 
  summarise(mean_dec = mean(price_m2),
            median_dec= median(price_m2),
            num_obs_dec = n(),
            min_dec = min(price_m2), 
            max_dec= max(price_m2)) 

write_xlsx(price_secondary_market_dec, "price__secondary_dec_regions.xlsx")

#tashkent secondary market
price_secondary_market_tashkent_dec <-  secondary_data_dec_all %>%
  filter(region == "Тошкент", condition != "Евроремонт",
         condition != "Авторский проект") %>%  
  group_by(city) %>% 
  summarise(mean = mean(price_m2),
            median= median(price_m2),
            num_obs = n(),
            min = min(price_m2), 
            max= max(price_m2)) 

write_xlsx(price_secondary_market_tashkent_dec, "price_dec_tash_secondary.xlsx")




primary_price_tash_dec <- price_res_dec %>% 
  mutate(region = if_else(city == "Мирабад", "Тошкент", region)) %>% 
  mutate(city= recode(city, "Мирабад" = "Мирабадский")) %>% 
  filter(home_type != "Вторичный рынок") %>% 
  filter(area > 15, area < 400) %>% 
  filter(price_m2 <= quantile(price_m2, 0.97), price_m2 >= quantile(price_m2, 0.03)) %>% 
  filter(region == "Тошкент", condition != "Евроремонт",
         condition != "Авторский проект") %>% 
  mutate(date = dmy(date), 
         month = month(date, label = TRUE)) %>% 
  filter(month == "дек") %>% 
  group_by(num_rooms, price, apart_floor, home_floor, area) %>%
  slice(1L) %>% 
  ungroup() %>% 
  #filter(city == "Яшнабадский")
  group_by(city) %>% 
  summarise(mean_dec = mean(price_m2),
            median_dec= median(price_m2),
            num_obs_dec = n(),
            min_dec = min(price_m2), 
            max_dec= max(price_m2)) 
  
write_xlsx(primary_market_dec, "price_dec_tash_primary.xlsx") 
