share_delta_data <- restaurants_40900 %>% select(sname_place_id) %>% left_join(deltas)
new_data <- left_join(pairs_40900, share_delta_data, by = 'sname_place_id')
new_data <- as.data.table(new_data)
rho1 <- -0.267
rho2 <- 0
sg_id <- 'sg:03671d4b8bb84140ac73d59b5ad4a5c7'

new_data[, 
         `:=`(
           choice_utility =  exp(delta + rho1 * distance_km + rho2 * distance_km_2)
           )
         ]

new_data[,
         `:=`(
           total_utility = 1 + sum(choice_utility)),
         by = .(home_cbg)
         ]

new_data[,
         choices_made := number_devices_residing * days * (choice_utility / total_utility)
         ]

new_data <- new_data[,
                     .(predicted_choices = sum(choices_made)
                       ),
                     by = .(sname_place_id)]


share_delta_data_1 <- share_delta_data
share_delta_data_1$delta[share_delta_data_1$sname_place_id == sg_id] <- share_delta_data_1$delta[share_delta_data_1$sname_place_id == sg_id] + 1.977e-03
new_data_1 <- left_join(pairs_40900, share_delta_data_1, by = 'sname_place_id')
new_data_1 <- as.data.table(new_data_1)
new_data_1[,
`:=`(
choice_utility =  exp(delta + rho1 * distance_km + rho2 * distance_km_2)
)
]
new_data_1[,
`:=`(
total_utility = 1 + sum(choice_utility)),
by = .(home_cbg)
]
new_data_1[,
choices_made := number_devices_residing * days * (choice_utility / total_utility)
]
new_data_1 <- new_data_1[,
.(predicted_choices = sum(choices_made)
),
by = .(sname_place_id)]
new_data %>% filter(sname_place_id == sg_id)
new_data_1 %>% filter(sname_place_id == sg_id)

