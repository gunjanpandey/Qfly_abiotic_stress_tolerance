

#---
# load data for figues
#---

source("R/cold_data_import_and_cleaning.R")

#---
# Figure 2: Cold tolerance data for all wild and domesticated populations 
#---

cold.no.change <- data.frame(sex = c("Female", "Male"),
                             change = c(1,1))

cold.p1 <- ggplot(cold.trt.s[[1]], aes(x = (lat*-1), y = pmedian)) + 
  geom_point(aes(colour = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  labs(colour = "Populations") + 
  ylab("Median \n recovery time \n (min)") +
  xlab(" ") + 
  theme(axis.text=element_text(size=4)) +
  scale_y_continuous(breaks = seq(120, 155, by = 5), limits = c(120, 155)) + 
  scale_x_continuous(breaks = seq(12, 40, by = 5), limits = c(12, 38)) +
  stat_smooth(aes(linetype = status), colour = c("black"), method = "lm", se = FALSE, size = 0.7) +
  theme_bw(base_size = 12) + 
  guides(linetype = FALSE) + 
  theme(axis.title.y = element_text(size=rel(0.8))) + 
  theme(legend.position = "bottom", legend.box = "horizontal", legend.margin = margin(c(0, 0, 0, 0)), 
        legend.text = element_text(size = rel(0.8))) + 
  theme(legend.title = element_text(size = rel(0.8)), legend.title.align = 1) + 
  ggtitle("Tolerance response in wild Qfly populations")

cold.p2 <- ggplot(cold.trt.s[[2]], aes(x = (lat*-1), y = pmedian)) + 
  geom_point(aes(colour = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  labs(colour = "Populations") + 
  ylab("Median \n recovery time \n (min)") +
  xlab(" ") + 
  theme(axis.text=element_text(size=4)) +
  scale_y_continuous(breaks = seq(120, 155, by = 5), limits = c(120, 155)) + 
  scale_x_continuous(breaks = seq(12, 40, by = 5), limits = c(12, 38)) +
  stat_smooth(aes(linetype = status), colour = c("black"), method = "lm", se = FALSE, size = 0.7) +
  theme_bw(base_size = 12) + 
  guides(linetype = FALSE) + 
  theme(axis.title.y = element_text(size=rel(0.8))) + 
  theme(legend.position = "bottom", legend.box = "horizontal", legend.margin = margin(c(0, 0, 0, 0)), 
        legend.text = element_text(size = rel(0.8))) + 
  theme(legend.title = element_text(size = rel(0.8)), legend.title.align = 1) + 
  ggtitle("Tolerance response in domesticated Qfly populations")


cold.change.p1 <- ggplot(corr.cold.domes.wild.males.prop, aes(x=(lat*-1), y = change)) + 
  geom_point(aes(colour = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  labs(colour = "Qfly populations") + 
  ylab("Proportion \n (Domesticated/Wild)") +
  xlab("Latitude (South)") + 
  ylim(0.5, 1.5) +
  theme(axis.text=element_text(size=4)) +
  scale_x_continuous(breaks = seq(12, 40, by =5), limits = c(12, 38)) +
  geom_hline(data = cold.no.change, aes(yintercept = change), colour = "black", size =0.7, linetype = "twodash") +
  guides(linetype = FALSE, colour = FALSE, shape = FALSE) + 
  theme_bw() + 
  theme(axis.title.y = element_text(size=rel(0.8))) + 
  ggtitle("Tolerance change in domesticated flies over wild \n (Domesticated/Wild)")


cold.wild.p1 <- ggplot(corr.cold.domes.wild.males.prop, aes(x=(lat*-1), y = proportion.wild)) + 
  geom_point(aes(colour = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  labs(colour = "Qfly populations") + 
  ylab("Proportion \n (Wild/S06)") +
  xlab("Latitude (South)") + 
  ylim(0.5, 1.5) +
  scale_x_continuous(breaks = seq(12, 40, by =5)) +
  geom_hline(data = cold.no.change, aes(yintercept = change), colour = "black", size =0.7, linetype = "twodash") +
  guides(linetype = FALSE, colour = FALSE, shape = FALSE) + 
  theme_bw() + 
  theme(axis.title.y = element_text(size=rel(0.8))) + 
  ggtitle("Tolerance change relative to S06 in wild Qfly populations \n (Wild/S06)")

cold.domes.p1 <- ggplot(corr.cold.domes.wild.males.prop, aes(x=(lat*-1), y = proportion.domes)) + 
  geom_point(aes(colour = pop), size = 4) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  labs(colour = "Qfly populations") + 
  ylab("Proportion \n (Domesticated/S06)") +
  xlab("Latitude (South)") + 
  ylim(0.5, 1.5) +
  scale_x_continuous(breaks = seq(12, 40, by =5)) +
  geom_hline(data = cold.no.change, aes(yintercept = change), colour = "black", size =0.7, linetype = "twodash") +
  guides(linetype = FALSE, colour = FALSE, shape = FALSE) + 
  theme_bw() +
  theme(axis.title.y = element_text(size=rel(0.8))) + 
  ggtitle("Tolerance change relative to S06 in domesticated Qfly \n populations (Domesticated/S06)")


prow <- plot_grid(cold.p1 + theme(legend.position = "none"), 
                  cold.p2 + theme(legend.position = "none"), 
                  cold.wild.p1 + theme(legend.position = "none"), 
                  cold.domes.p1 + theme(legend.position = "none"), 
                  cold.change.p1 + theme(legend.position = "none"), 
                  ncol = 1, 
                  align = "v", 
                  axis = "r")
legend_a <- get_legend(cold.p1 + theme(legend.position="bottom"))

fig2 <- plot_grid(prow, NULL, legend_a, ncol = 1, rel_heights = c(1, 0.005,.1))

ggsave("fig/fig2.tiff", width = 6, height = 12, units = "in", dpi = 600)

