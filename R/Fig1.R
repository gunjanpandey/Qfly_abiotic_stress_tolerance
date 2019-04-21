
#---
# load data for figues
#---

source("R/heat_data_clean_import.R")

#---
# Figure 1: Heat tolerance for all wild and domesticated populations
#---


heat.no.change <- data.frame(sex = c("Male"), change = c(1))

heat.domes.2$status <- factor(heat.domes.2$status, levels = c("Wild", "Domesticated"))

heat.domes.s <- heat.domes.2 %>% 
  ungroup() %>%
  group_by(status) %>% 
  group_split() %>%
  purrr::map(function(x) droplevels(x))


p0 <- ggplot(heat.domes.s[[1]], aes(x = (lat*-1), y = pmedian)) + 
  geom_point(aes(colour = pop), size = 3) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  labs(colour = "Populations") + 
  ylab("Median \n knockdown time \n (min)") +
  xlab(" ") + 
  theme(axis.text=element_text(size=4)) +
  scale_y_continuous(breaks = seq(15, 60, by = 5), limits = c(22, 48)) + 
  scale_x_continuous(breaks = seq(12, 40, by = 5), limits = c(12, 38)) +
  stat_smooth(aes(linetype = status), colour = c("black"), method = "lm", se = FALSE, size = 0.7) +
  theme_bw(base_size = 12) + 
  guides(linetype = FALSE) + 
  theme(axis.title.y = element_text(size=rel(0.8))) + 
  theme(legend.position = "bottom", legend.box = "horizontal", legend.margin = margin(c(0, 0, 0, 0)), 
        legend.text = element_text(size = rel(0.8))) + 
  theme(legend.title = element_text(size = rel(0.8)), legend.title.align = 1) + 
  ggtitle("Tolerance response in wild Qfly populations")

p2 <- ggplot(heat.domes.s[[2]], aes(x = (lat*-1), y = pmedian)) + 
  geom_point(aes(colour = pop), size = 3) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  labs(colour = "Qfly populations") + 
  ylab("Median \n knockdown time \n (min)") +
  xlab(" ") + 
  scale_y_continuous(breaks = seq(15, 60, by = 5), limits = c(22, 48)) + 
  scale_x_continuous(breaks = seq(12, 40, by = 5), limits = c(12, 38)) +
  stat_smooth(aes(linetype = status), colour = c("black"), method = "lm", se = FALSE, size = 0.7) +
  theme_bw(base_size = 12) +
  guides(linetype = FALSE) + 
  theme(axis.title.y = element_text(size=rel(0.8))) + 
  ggtitle("Tolerance response in domesticated Qfly populations")



change.p1 <- ggplot(corr.heat.wild.males.prop, aes(x=(lat*-1), y = change)) + 
  geom_point(aes(colour = pop), size = 3) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  labs(colour = "Qfly populations") + 
  ylab("Proportion \n (Domesticated/Wild)") +
  xlab("Latitude (South)") + 
  ylim(0.2, 2.1) +
  theme(axis.text=element_text(size=4)) +
  scale_x_continuous(breaks = seq(12, 40, by =5), limits = c(12, 38)) +
  geom_hline(data = heat.no.change, aes(yintercept = change), colour = "black", size =0.7, linetype = "twodash") +
  guides(linetype = FALSE, colour = FALSE, shape = FALSE) + 
  labs(shape = "Domestication status") +
  theme_bw() + 
  theme(axis.title.y = element_text(size=rel(0.8))) + 
  ggtitle("Tolerance change in domesticated over wild Qfly \n (Domesticated/Wild)")


wild.p1 <- ggplot(corr.heat.wild.males.prop, aes(x=(lat*-1), y = proportion.wild)) + 
  geom_point(aes(colour = pop), size = 3) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  labs(colour = "Qfly populations") + 
  ylab("Proportion \n (Wild/S06)") +
  xlab(" ") + 
  ylim(0.2, 2.1) +
  scale_x_continuous(breaks = seq(12, 40, by =5), limits = c(12, 38)) +
  geom_hline(data = heat.no.change, aes(yintercept = change), colour = "black", size =0.7, linetype = "twodash") +
  guides(linetype = FALSE, colour = FALSE, shape = FALSE) + 
  labs(shape = "Domestication status") +
  theme_bw() + 
  theme(axis.title.y = element_text(size=rel(0.8))) + 
  ggtitle("Tolerance change relative to S06 in wild Qfly populations \n (Wild/S06)")

domes.p1 <- ggplot(corr.heat.wild.males.prop, aes(x=(lat*-1), y = proportion.domes)) + 
  geom_point(aes(colour = pop), size = 3) + 
  scale_colour_Qfly() + 
  scale_fill_Qfly() +
  labs(colour = "Qfly populations") + 
  ylab("Proportion \n (Domesticated/S06)") +
  xlab(" ") + 
  ylim(0.2, 2.1) +
  theme(axis.text=element_text(size=4)) +
  scale_x_continuous(breaks = seq(12, 40, by =5), limits = c(12, 38)) +
  geom_hline(data = heat.no.change, aes(yintercept = change), colour = "black", size =0.7, linetype = "twodash") +
  guides(linetype = FALSE, colour = FALSE, shape = FALSE) + 
  labs(shape = "Domestication status") +
  theme_bw() + 
  theme(axis.title.y = element_text(size=rel(0.8))) + 
  ggtitle("Tolerance change relative to S06 in domesticated Qfly \n populations (Domesticated/Wild)")


prow <- plot_grid(p0 + theme(legend.position = "none"), 
                  p2 + theme(legend.position = "none"), 
                  wild.p1 + theme(legend.position = "none"), 
                  domes.p1 + theme(legend.position = "none"), 
                  change.p1 + theme(legend.position = "none"), 
                  ncol = 1, 
                  align = "v", 
                  axis = "r")
legend_a <- get_legend(p0 + theme(legend.position="bottom"))

fig1 <- plot_grid(prow, legend_a, ncol = 1, rel_heights = c(1, .1))


ggsave("fig/fig1.tiff", width = 6, height = 12, units = "in", dpi = 600)




