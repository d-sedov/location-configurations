############################## Plot the results ###############################

delete_worst_summary_file_path <- file.path('/home/quser/project_dir',
                                            'urban/data/output/counterfactuals',
                                            'number/delete_worst_summary.csv')

delete_worst_summary <- read_csv(delete_worst_summary_file_path)

# Normalize variables
delete_worst_summary <- delete_worst_summary %>% 
  group_by(cbsa) %>% 
  mutate(rhos_normalized = (rhos - rhos[perc_removed == 0]) / rhos[perc_removed == 0],
         count_normalized = (average_count - average_count[perc_removed == 0]) / average_count[perc_removed == 0], 
         density_normalized = (average_density - average_density[perc_removed == 0]) / average_density[perc_removed == 0],
         quality_normalized = (average_quality - average_quality[perc_removed == 0]) / 
           abs(average_quality[perc_removed == 0])) %>%
  select(perc_removed, cbsa, ends_with('normalized')) %>%
  filter(perc_removed != 0) %>%
  mutate(perc_removed = 0.01 * perc_removed)

delete_worst_summary_averages <- delete_worst_summary %>%
  ungroup() %>%
  select(-cbsa) %>% 
  group_by(perc_removed) %>% 
  summarise_all(mean)

# Depict, how consumer utility changes as restaurants are deleted
pic <- ggplot() + 
  geom_line(data = delete_worst_summary, 
            aes(x = perc_removed, 
                y = rhos_normalized, 
                group = factor(cbsa), color = 'gray'), alpha = 0.5) + 
  geom_point(data = delete_worst_summary, 
             aes(x = perc_removed, 
                 y = rhos_normalized, 
                 group = factor(cbsa), color = 'gray'), size = 0.5, alpha = 0.5) + 
  geom_line(data = delete_worst_summary_averages, 
            aes(x = perc_removed,
                y = rhos_normalized, color = 'black')) +
  geom_point(data = delete_worst_summary_averages,
             aes(x = perc_removed,
                 y = rhos_normalized, color = 'black'), size = 0.5) +
  scale_color_identity(name = '', 
                       guide = 'legend', 
                       labels = c('Average across markets', 'Individual markets')) + 
  xlab('Firms removed') + 
  ylab('Welfare-equivalent change in distance costs') + 
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw(base_family = 'Times') +
  my_theme +
  theme(legend.title = element_blank(), 
        legend.position = c(0.25, 0.8)) 

ggsave(filename = file.path('/home/quser/project_dir/urban/output/plots/model', 
                            'delete_worst_rhos.pdf'), 
       device = cairo_pdf, plot = pic, width = 4.5, height = 3.00)
embed_fonts(file = file.path('/home/quser/project_dir/urban/output/plots/model', 
                             'delete_worst_rhos.pdf'))


# Depict, from which neighborhoods (in terms of number of available options) are the restaurants deleted
pic <- ggplot() + 
  geom_line(data = delete_worst_summary, 
            aes(x = perc_removed, 
                y = count_normalized, 
                group = factor(cbsa), color = 'gray'), alpha = 0.25) + 
  geom_point(data = delete_worst_summary, 
             aes(x = perc_removed, 
                 y = count_normalized, 
                 group = factor(cbsa), color = 'gray'), size = 0.5, alpha = 0.25) + 
  geom_line(data = delete_worst_summary_averages, 
            aes(x = perc_removed,
                y = count_normalized, color = 'black')) +
  geom_point(data = delete_worst_summary_averages,
             aes(x = perc_removed,
                 y = count_normalized, color = 'black'), size = 0.5) +
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'black') +
  annotate('text',
           x = 0.05, 
           y = 0, 
           label = 'Baseline: average restaurants per CBG', 
           vjust = 1.5,
           hjust = -0.0,
           family = 'Times', 
           size = 2.75) + 
  scale_color_identity(name = '', 
                       guide = 'legend', 
                       labels = c('Average across markets', 'Individual markets')) + 
  xlab('Firms removed') + 
  ylab('Status-quo firm count in affected CBGs (relative to baseline)') + 
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw(base_family = 'Times') +
  my_theme +
  theme(legend.title = element_blank(), 
        legend.position = c(0.8, 0.25)) 

ggsave(filename = file.path('/home/quser/project_dir/urban/output/plots/model', 
                            'delete_worst_count.pdf'), 
       device = cairo_pdf, plot = pic, width = 4.5, height = 3.00)
embed_fonts(file = file.path('/home/quser/project_dir/urban/output/plots/model', 
                             'delete_worst_count.pdf'))


# Depict, from which neighborhoods (in terms of population density) are the restaurants deleted
pic <- ggplot() + 
  geom_line(data = delete_worst_summary, 
            aes(x = perc_removed, 
                y = density_normalized, 
                group = factor(cbsa), color = 'gray'), alpha = 0.5) + 
  geom_point(data = delete_worst_summary, 
             aes(x = perc_removed, 
                 y = density_normalized, 
                 group = factor(cbsa), color = 'gray'), size = 0.5, alpha = 0.5) + 
  geom_line(data = delete_worst_summary_averages, 
            aes(x = perc_removed,
                y = density_normalized, color = 'black')) +
  geom_point(data = delete_worst_summary_averages,
             aes(x = perc_removed,
                 y = density_normalized, color = 'black'), size = 0.5) +
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'black') +
  annotate('text',
           x = 0.05, 
           y = 0, 
           label = 'Baseline: average population density in CBGs', 
           vjust = 1.5,
           hjust = -0.0,
           family = 'Times', 
           size = 2.75) + 
  scale_color_identity(name = '', 
                       guide = 'legend', 
                       labels = c('Average across markets', 'Individual markets')) + 
  xlab('Firms removed') + 
  ylab('Pop. density in affected CBGs (relative to baseline)') + 
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw(base_family = 'Times') +
  my_theme +
  theme(legend.title = element_blank(), 
        legend.position = c(0.8, 0.8))

ggsave(filename = file.path('/home/quser/project_dir/urban/output/plots/model', 
                            'delete_worst_density.pdf'), 
       device = cairo_pdf, plot = pic, width = 4.5, height = 3.00)
embed_fonts(file = file.path('/home/quser/project_dir/urban/output/plots/model', 
                             'delete_worst_density.pdf'))

# Depict, which neighborhoods (in terms of quality) are deleted
pic <- ggplot() + 
  geom_line(data = delete_worst_summary, 
            aes(x = perc_removed, 
                y = quality_normalized, 
                group = factor(cbsa), color = 'gray'), alpha = 0.5) + 
  geom_point(data = delete_worst_summary, 
             aes(x = perc_removed, 
                 y = quality_normalized, 
                 group = factor(cbsa), color = 'gray'), size = 0.5, alpha = 0.5) + 
  geom_line(data = delete_worst_summary_averages, 
            aes(x = perc_removed,
                y = quality_normalized, color = 'black')) +
  geom_point(data = delete_worst_summary_averages,
             aes(x = perc_removed,
                 y = quality_normalized, color = 'black'), size = 0.5) +
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'black') +
  annotate('text',
           x = 0.05, 
           y = 0, 
           label = 'Baseline: average restaurant quality', 
           vjust = 1.5,
           hjust = -0.0,
           family = 'Times', 
           size = 2.75) + 
  scale_color_identity(name = '', 
                       guide = 'legend', 
                       labels = c('Average across markets', 'Individual markets')) + 
  xlab('Firms removed') + 
  ylab('Quality of removed firms') + 
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw(base_family = 'Times') +
  my_theme +
  theme(legend.title = element_blank(), 
        legend.position = c(0.8, 0.25))

ggsave(filename = file.path('/home/quser/project_dir/urban/output/plots/model', 
                            'delete_worst_quality.pdf'), 
       device = cairo_pdf, plot = pic, width = 4.5, height = 3.00)
embed_fonts(file = file.path('/home/quser/project_dir/urban/output/plots/model', 
                             'delete_worst_quality.pdf'))

data_removed <- data.frame(n = seq(1, length(rhos)), rho = rhos)
data_added <- data.frame(n = seq(1, length(rhos_add)), rho = rhos_add)

# Normalize
data_removed <- data_removed %>%
  mutate(rho = (rho - rho1) / abs(rho1))
data_added <- data_added %>%
  mutate(rho = (rho - rho1) / abs(rho1))

# Produce the plot
pic1 <- ggplot(data = data_removed, 
               aes(x = n, y = rho)) +
  geom_line() + 
  ylab('Distance costs increased by %') +
  xlab('Worst firms removed') + 
  theme_bw() +
  my_theme +
  scale_y_continuous(labels = percent) +
  theme(text = element_text(family = 'Times'))

data_removed <- data.frame(n = seq(1, length(rhos)), rho = rhos)
data_added <- data.frame(n = seq(1, length(rhos_add)), rho = rhos_add)

# Normalize
data_removed <- data_removed %>%
  mutate(rho = (rho - rho1) / abs(rho1))
data_added <- data_added %>%
  mutate(rho = (rho - rho1) / abs(rho1))

# Produce the plot
pic1 <- ggplot(data = data_removed, 
               aes(x = n, y = rho)) +
  geom_line() + 
  ylab('Distance costs increased by %') +
  xlab('Worst firms removed') + 
  theme_bw() +
  my_theme +
  scale_y_continuous(labels = percent) +
  theme(text = element_text(family = 'Times'))

pic2 <- ggplot(data = data_added, 
               aes(x = n, y = rho)) +
  geom_line() + 
  ylab('Distance costs reduced by %') +
  xlab('Firms of average quality added') + 
  theme_bw() +
  my_theme +
  scale_y_continuous(labels = percent) +
  theme(text = element_text(family = 'Times'))

ggsave(filename = file.path('/home/quser/project_dir/urban/output/plots/model', 
                            'worst_firms_removed.pdf'), 
       device = cairo_pdf, plot = pic1, width = 4.5, height = 2.5)
embed_fonts(file = file.path('/home/quser/project_dir/urban/output/plots/model', 
                             'worst_firms_removed.pdf'))

ggsave(filename = file.path('/home/quser/project_dir/urban/output/plots/model', 
                            'average_firms_added.pdf'), 
       device = cairo_pdf, plot = pic2, width = 4.5, height = 2.5)
embed_fonts(file = file.path('/home/quser/project_dir/urban/output/plots/model', 
                             'average_firms_added.pdf'))

###############################################################################