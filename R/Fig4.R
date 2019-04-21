
#---
# load data for figues
#---

source("R/desiccation_and_starvation_data_import_and_cleaning.R")

#---
# Figure 4: Trends over generations in the desiccation and starvation tolerance data
#---

p3 <-  ggplot(des.domes.sd, aes(x = gen, y = pmedian, 
                                group =trt, colour = trt)) + 
  geom_point(size = 3) + 
  geom_line() +
  geom_errorbar(aes(ymin=pmedian-SD, ymax=pmedian+SD), width=.2,
                position=position_dodge(0.05)) +
  scale_color_grey() + 
  labs(colour = "Treatment") + 
  ylab("Median survival time (hrs)") +
  xlab(" ") + 
  guides(shape = FALSE) + 
  facet_grid(. ~ pop) + 
  theme_bw() +
  theme(strip.background = element_blank(), 
        strip.text.x = element_text(size = 14), 
        strip.text.y = element_text(size = 12), 
        panel.border = element_rect(colour="gray", 
                                    fill=NA, size=1)) + #, 
  theme(legend.position = "bottom", legend.box = "vertical", 
        legend.margin = margin(c(0, 5, 5, 0)), 
        legend.text = element_text(margin = margin(r = 8, 
                                                   unit = "pt")))

ggsave("fig/fig4.tiff", width = 12, height = 4, units = "in", dpi = 600)