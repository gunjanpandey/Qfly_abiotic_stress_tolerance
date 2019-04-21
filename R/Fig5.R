
#---
# load data for figues
#---

source("R/desiccation_and_starvation_data_import_and_cleaning.R")

#---
# Figure 5: Correlation between G2 desiccation tolerance of the primary 
# and resampled collections from Cape Tribulation, Alice Springs and Sydney
#---

repeated.desiccation.plot <- repeated.desiccation %>% 
  dplyr::select(-c("exp", "gen")) %>% 
  tidyr::spread(sample, pmedian)


ggplot(repeated.desiccation.plot, aes(x = first, y = second)) + 
  geom_point(aes(colour = pop), size = 3) + 
  theme_bw() + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  xlab("Median survival time (hrs) \n Primary survery") +
  ylab("Median survival time (hrs) \n Resampled populations") +
  ggtitle("Desiccation tolerance in resample colonies of the \n Qfly from concurrent summers") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = lm, se = FALSE, colour = "gray") + 
  labs(colour = "Populations") + 
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin(c(0, 0, 0, 0)), 
        legend.text = element_text(margin = margin(r = 2, unit = "pt")))

ggsave("fig/fig5.tiff", width = 8, height = 4, units = "in", dpi = 600)