############################## Plot the results ###############################

output_file_path <- file.path('/home/quser/project_dir/urban/data/output', 
                              'counterfactuals', 
                              'reallocation', 
                              'reallocate_pareto.csv')
# Import the results of the Pareto-set exploration
reallocate_output <- read_csv(output_file_path,
                              col_names = TRUE)
# Normalize
reallocate_output <- reallocate_output %>%
  mutate(rho_equivalent = (rho_equivalent - rho1) / abs(rho1))
# Get the convex hull
hull <- reallocate_output %>%
  slice(chull(rho_equivalent, change_profit))
# Produce the plot
pic <- ggplot(data = reallocate_output,
              aes(x = rho_equivalent, y = change_profit)) +
  geom_point(size = 0.5) +
  geom_polygon(data = hull, alpha = 0.25) +
  geom_point(aes(x = 0, y = 0), size = 0.5, colour = mycolorscheme3[2]) +
  geom_vline(xintercept = 0, colour = mycolorscheme3[2]) +
  geom_hline(yintercept = 0, colour = mycolorscheme3[2]) +
  ylab('% change in profits') +
  xlab('% reduction in distance costs') +
  theme_bw() +
  my_theme +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(labels = scales::percent) +
  theme(text = element_text(family = 'Times'))

ggsave(filename = file.path('/home/quser/project_dir/urban/output/plots/model',
                            'pareto_set_example.pdf'),
       device = cairo_pdf, plot = pic, width = 3.75, height = 2.05)
embed_fonts(file = file.path('/home/quser/project_dir/urban/output/plots/model',
                      'pareto_set_example.pdf'))

###############################################################################


############################# Best reallocation ###############################

best_allocations_file_path  <- file.path('/home/quser/project_dir/urban/data/output', 
                                            'counterfactuals', 
                                            'reallocation', 
                                            'reallocate_best_test.csv')

best_allocations <- read_csv(best_allocations_file_path)

# Compute changes in profits and rho-equivalents
best_allocations <- best_allocations %>% 
  group_by(cbsa) %>% 
  mutate(change_profits = (profits - profits[type == 'initial']) / profits[type == 'initial'])
best_allocations <- best_allocations %>% 
  group_by(cbsa) %>% 
  mutate(change_rho = (rho_equivalent - rho_equivalent[type == 'initial']) / abs(rho_equivalent[type == 'initial']))

# Compare correlations in initial and best alternative allocations
best_allocations <- best_allocations %>% 
  group_by(cbsa) %>% 
  mutate(change_cor_delta_density = ifelse(delta_density[type == 'final'] - delta_density[type == 'initial'] < 0, 
                                           mycolorscheme3[2],
                                           mycolorscheme3[1]
                                           )
  )

best_allocations_final <- best_allocations %>% filter(type == 'final')

quantile(best_allocations_final$change_profits, probs = c(0.1, 0.9))
quantile(best_allocations_final$change_rho, probs = c(0.1, 0.9))

# Plot the join distribution of profits and welfare increases
pic <- ggplot(data = best_allocations_final,
              aes(x = change_rho, y = change_profits)) + 
  geom_smooth(data = best_allocations_final %>% filter(change_profits < 1),
              aes(x = change_rho, y = change_profits),
              method = 'lm', color = 'black', alpha = 0.15, size = 0.25) + 
  geom_point() + 
  geom_vline(aes(xintercept = median(change_rho)), 
             col = 'black',
             size = 0.25,
             linetype = 'dashed') + 
  geom_hline(aes(yintercept = median(change_profits)), 
             col = 'black', 
             size = 0.25, 
             linetype = 'dashed') + 
  geom_rug(col = 'black', alpha = 0.25) + 
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent) +
  theme_bw(base_family = 'Times') +
  my_theme +
  ylab('Increase in profits') +
  xlab('Reduction in distance costs') + 
  annotate('text',
           x = median(best_allocations_final$change_rho) + 0.01, 
           y = max(best_allocations_final$change_profits - 0.0125), 
           label = paste0('Median: ',
                          percent(median(best_allocations_final$change_rho), 
                                  accuracy = 0.01)), 
           family = 'Times', 
           size = 2.75) + 
  annotate('text', 
           y = median(best_allocations_final$change_profits) + 0.05, 
           x = max(best_allocations_final$change_rho - 0.0075), 
           label = paste0('Median: ', 
                          percent(median(best_allocations_final$change_profits), 
                                  accuracy = 0.01)), 
           family = 'Times', 
           size = 2.75)

ggsave(filename = file.path('/home/quser/project_dir/urban/output/plots/model',
                            'best_allocations_scatterplot.pdf'),
       device = cairo_pdf, plot = pic, width = 3.75, height = 2.05)
embed_fonts(file = file.path('/home/quser/project_dir/urban/output/plots/model',
                             'best_allocations_scatterplot.pdf'))

# Median deltas-population density correlations in the status-quo and final allocations
median(best_allocations %>% filter(type == 'initial') %>% pull(delta_density))
median(best_allocations %>% filter(type == 'final') %>% pull(delta_density))
# Relationship between deltas and population density
pic <- ggplot(data = best_allocations %>% arrange(cbsa), 
              aes(x = factor(cbsa), 
                  y = delta_density, 
                  group = type,
                  color = type, 
                  fill = type)) + 
  geom_line() + geom_point() + 
  theme_bw(base_family = 'Times') +
  my_theme +
  theme(axis.text.x = element_blank()) +
  theme(legend.position = c(0.85, 0.85)) + 
  ylab('Local correlation between rest. quality and pop. density') +
  xlab('Market ID') + 
  guides(group = FALSE, fill = FALSE) + 
  scale_color_manual(name = 'Allocation', 
                     labels = c('Best alternative', 
                                'Status-quo'), 
                     values = mycolorscheme3[2:1]) 
  
ggsave(filename = file.path('/home/quser/project_dir/urban/output/plots/model',
                            'best_allocations_deltas_density.pdf'),
       device = cairo_pdf, plot = pic, width = 4.5, height = 2.5)
embed_fonts(file = file.path('/home/quser/project_dir/urban/output/plots/model',
                             'best_allocations_deltas_density.pdf'))
# Same graph, different presentation
pic <- ggplot(data = best_allocations %>% arrange(cbsa), 
              aes(x = reorder(factor(cbsa), delta_density), 
                  y = delta_density)) + geom_point(size = 0.1, color = 'black', 
                                                   fill = 'black') + geom_line(aes(group = factor(cbsa), color = change_cor_delta_density)) +
  theme_bw(base_family = 'Times') +
  my_theme +
  theme(axis.text.x = element_blank()) +
  theme(legend.position = c(0.85, 0.85), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) + 
  ylab('Local correlation between rest. quality and pop. density') +
  xlab('Market ID') + 
  guides(group = FALSE, fill = FALSE) + 
  scale_colour_identity()

# Median deltas-population density correlations in the status-quo and final allocations
median(best_allocations %>% filter(type == 'initial') %>% pull(delta_count))
median(best_allocations %>% filter(type == 'final') %>% pull(delta_count))
# Relationship between deltas and local restaurant count
pic <- ggplot(data = best_allocations %>% arrange(cbsa), 
              aes(x = factor(cbsa), 
                  y = delta_count, 
                  group = type,
                  color = type, 
                  fill = type)) + 
  geom_line() + geom_point() + 
  theme_bw(base_family = 'Times') +
  my_theme +
  theme(axis.text.x = element_blank()) +
  theme(legend.position = c(0.85, 0.85)) + 
  ylab('Local correlation between rest. quality and rest. count') +
  xlab('Market ID') + 
  guides(group = FALSE, fill = FALSE) + 
  scale_color_manual(name = 'Allocation', 
                     labels = c('Best alternative', 
                                'Status-quo'), 
                     values = mycolorscheme3[2:1]) 

ggsave(filename = file.path('/home/quser/project_dir/urban/output/plots/model',
                            'best_allocations_deltas_count.pdf'),
       device = cairo_pdf, plot = pic, width = 4.5, height = 2.5)
embed_fonts(file = file.path('/home/quser/project_dir/urban/output/plots/model',
                             'best_allocations_deltas_count.pdf'))

###############################################################################


