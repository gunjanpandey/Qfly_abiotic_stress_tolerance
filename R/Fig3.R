

#---
# load data for figues
#---

source("R/desiccation_and_starvation_data_import_and_cleaning.R")

#---
# Figure 3: Desiccation and starvation tolerance data
#---

desiccation.no.change <- data.frame(sex = c("Male"), change = c(1,1))


fig.rep.1 <- ggplot(des.wild.repeat.2[[1]], aes(x = (lat*-1), y = pmedian)) + 
  geom_point(aes(colour = pop, shape = status.1), size = 3) + 
  scale_colour_Qfly() +
  scale_fill_Qfly() + 
  labs(colour = "Populations") + 
  ylab("Median \n survival time \n (hrs)") +
  xlab(" ") + 
  ylim(20, 80) +
  scale_x_continuous(breaks = seq(12, 40, by =5)) +
  facet_grid(. ~ trt) + 
  stat_smooth(aes(linetype = status), colour = c("black"), method = "lm", se = FALSE, size = 0.7) +
  theme_bw(base_size = 12) + 
  guides(linetype = FALSE, shape = guide_legend(order = 2), colour = guide_legend(order = 1)) + 
  labs(shape = "Domestication status") + 
  ggtitle("Tolerance response in wild populations") +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin(c(0, 0, 0, 0)), 
        legend.text = element_text(margin = margin(r = 3, unit = "pt"))) + 
  scale_linetype_manual(name = "", values = c("solid", "twodash"))+ 
  theme(plot.title = element_text(size = rel(0.9)))

fig.rep.2 <- ggplot(des.wild.repeat.2[[2]], aes(x = (lat*-1), y = pmedian)) + 
  geom_point(aes(colour = pop, shape = status.1), size = 3) + 
  scale_colour_Qfly() +
  scale_fill_Qfly() + 
  labs(colour = "Populations") + 
  ylab("Median \n survival time \n (hrs)") +
  xlab(" ") + 
  ylim(10, 60) +
  scale_x_continuous(breaks = seq(12, 40, by =5)) +
  facet_grid(. ~ trt) + 
  stat_smooth(aes(linetype = status), colour = c("black"), method = "lm", se = FALSE, size = 0.7) +
  theme_bw(base_size = 12) + 
  guides(linetype = FALSE, shape = guide_legend(order = 2), colour = guide_legend(order = 1)) + 
  labs(shape = "Domestication status") + 
  ggtitle("Tolerance response in domesticated populations") +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin(c(0, 0, 0, 0)), 
        legend.text = element_text(margin = margin(r = 3, unit = "pt"))) + 
  scale_linetype_manual(name = "", values = c("solid", "twodash"))+ 
  theme(plot.title = element_text(size = rel(0.9)))

des.change.p1 <- ggplot(desiccation.change.domestication, aes(x=(lat*-1), y = change)) + 
  geom_point(aes(colour = pop), size = 3) + 
  scale_colour_Qfly()+
  scale_fill_Qfly() +
  labs(colour = "Populations") + 
  ylab("Proportion \n (Domesticated/Wild)") +
  xlab(" ") + 
  ylim(0, 2.8) +
  scale_x_continuous(breaks = seq(12, 40, by =5)) +
  facet_grid(. ~ trt.x) +
  geom_hline(data = desiccation.no.change, aes(yintercept = change), colour = "black", size =0.7) +
  guides(linetype = FALSE, colour = FALSE, shape = FALSE) + 
  labs(shape = "Domestication status") +
  ggtitle("Tolerance domesticated populations over wild (Domesticated/Wild)") +
  theme_bw() + 
  guides(colour = guide_legend(order = 1)) + 
  labs(shape = "Domestication status") + 
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin(c(0, 0, 0, 0)), 
        legend.text = element_text(margin = margin(r = 2, unit = "pt")))+ 
  theme(plot.title = element_text(size = rel(0.9)))

des.wild.p1 <- ggplot(desiccation.change.domestication, aes(x=(lat*-1), y = proportion.wild)) + 
  geom_point(aes(colour = pop), size = 3) + 
  scale_colour_Qfly()+
  scale_fill_Qfly() +
  labs(colour = "Populations") + 
  ylab("Proportion \n (Wild/S06") +
  xlab(" ") + 
  ylim(0, 2.8) +
  scale_x_continuous(breaks = seq(12, 40, by =5)) +
  facet_grid(. ~ trt.x) +
  geom_hline(data = desiccation.no.change, aes(yintercept = change), colour = "black", size =0.7) +
  guides(linetype = FALSE, colour = FALSE, shape = FALSE) + 
  labs(shape = "Domestication status") +
  ggtitle("Tolerance relative to S06 in wild populations (Wild/S06)") +
  theme_bw()+ 
  theme(plot.title = element_text(size = rel(0.9)))

des.domes.p1 <- ggplot(desiccation.change.domestication, aes(x=(lat*-1), y = proportion.domes)) + 
  geom_point(aes(colour = pop), size = 3) + 
  scale_colour_Qfly()+
  scale_fill_Qfly() +
  labs(colour = "Populations") + 
  ylab("Proportion \n (Domesticated/S06)") +
  xlab("Latitude (South)") + 
  ylim(0, 2.8) +
  scale_x_continuous(breaks = seq(12, 40, by =5)) +
  facet_grid(. ~ trt.x) +
  geom_hline(data = desiccation.no.change, aes(yintercept = change), colour = "black", size =0.7) +
  guides(linetype = FALSE, colour = FALSE, shape = FALSE) + 
  labs(shape = "Domestication status") +
  ggtitle("Tolerance relative to S06 in domesticated populations (Domesticated/S06)") +
  theme_bw()+ 
  theme(plot.title = element_text(size = rel(0.9)))


plot.des <- plot_grid(fig.rep.1 + theme(legend.position = "none"), 
                      fig.rep.2 + theme(legend.position = "none"), 
                      des.wild.p1 + theme(legend.position = "none"), 
                      des.domes.p1 + theme(legend.position = "none"),
                      des.change.p1 + theme(legend.position = "none"),
                      ncol = 1, 
                      align = "v", 
                      axis = "r", rel_heights = c(0.9,0.9,0.9,0.9,0.9))

legends <- get_legend(fig.rep.1 +theme(legend.position = "bottom"))

fig3 <- plot_grid(plot.des, NULL, legends, ncol = 1, rel_heights = c(1, 0.005,.1))

ggsave("fig/fig3.tiff", width = 10, height = 14, units = "in", dpi = 300)
