cbgs <- read_csv(file.path(input_folder_path, 'data_cbg.csv'))
# Import data
restaurants <- read_csv(file.path(input_folder_path, 'data_restaurants.csv'))

# Set price to missing if it is -1
restaurants <- restaurants %>% mutate(price = ifelse(price == -1, NA, price))

# Replace missing (null) categories with NAs
restaurants <- restaurants %>% mutate(categories = ifelse(categories == 'null', 
                                                          NA, 
                                                          categories))

# Transform the total minutes open variable
restaurants <- restaurants %>%
    mutate(perc_time_open = 100 * total_minutes_open / (24 * 60 * 31)) %>%
    mutate(perc_time_open = replace(perc_time_open,  
                                    which(perc_time_open <= 0.1 | perc_time_open >= 100.1), 
                                    NA))

# Encode 'urban' restaurants
restaurants <- restaurants %>%
    mutate(urban = !is.na(cbsa)) 

# Count restaurant's categories
restaurants <- restaurants %>%
    mutate(n_categories = sapply(categories, 
                                 function(x) {
                                     ifelse(is.na(x), 
                                            NA,
                                            ifelse(x == "[]",
                                                   0,
                                                   nrow(fromJSON(x))
                                            )
                                     )
                                 }
    )
    )

# Extract restaurant's first category
restaurants <- restaurants %>%
    mutate(category1 = sapply(categories, 
                              function(x) {
                                  ifelse(is.na(x) || x == "[]",
                                         NA,
                                         fromJSON(x)[1, 'title']
                                  )
                              }
    )
    )

rest_for_reg <- left_join(restaurants, cbgs, by = c('cbg' = 'cbg')) %>% 
    replace_na(list(brands = 'None', 
                    category1 = 'None')) %>% 
    mutate(ct = factor(ct), 
           brands = factor(brands), 
           category1 = factor(category1)) %>%
    mutate(price = case_when(price == 1 ~ 'low',
                             price == 2 ~ 'medium', 
                             price %in% c(3,4) ~ 'high',
                             TRUE ~ 'na'), 
           rating = case_when(rating %in% c(0, 1.0, 1.5, 2.0) ~ 'low', 
                              rating %in% c(2.5, 3.0, 3.5) ~ 'medium', 
                              rating %in% c(4.0, 4.5, 5.0) ~ 'high', 
                              TRUE ~ 'na')) %>% 
    mutate(price = factor(price, levels = c('low', 'medium', 'high', 'na')), 
           rating = factor(rating, levels = c('low', 'medium', 'high', 'na')
                           )
           )

fit <- felm(data = rest_for_reg, raw_visit_counts ~ price * rating + factor(naics_code) + area_m2.x + phone + number_devices_residing + rest_number + est_number  | ct + brands + category1 | 0 | ct)