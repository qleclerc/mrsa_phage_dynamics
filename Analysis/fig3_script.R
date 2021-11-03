
library(ggplot2)
library(dplyr)
library(scales)
library(reshape2)
library(readxl)
library(ggpubr)
library(cowplot)
library(png)

data = read_xlsx(here::here("Data", "lysogeny_test_data.xlsx"))

data$B_count = data$B_count*(10^data$B_dil)*(1000/20)
data$E_count = data$E_count*(10^data$E_dil)*(1000/20)
data$T_count = data$T_count*(10^data$T_dil)*(1000/20)
data$D_count = data$D_count*(10^data$D_dil)*(1000/500)
data$P_count = data$P_count*(10^data$P_dil)*(1000/100)

data = data %>%
  select(Culture, Time, contains("count"))

colnames(data) = gsub("_count", "", colnames(data))

data = melt(data, id.vars = c("Time", "Culture"))

data = data %>%
  group_by(Time, variable) %>%
  summarise(mean = mean(value), se = sd(value)/sqrt(3)) %>%
  ungroup %>%
  filter(variable != "B")

data$group = rep(c("B", "B", "B", "P"), 4)

data$variable = as.factor(data$variable)
data$variable = factor(data$variable, c("Bacteria:", levels(data$variable)[c(2,3,4)],
                                                " ", "Phage:", levels(data$variable)[5]))

p3a = ggplot(data, aes(x=Time, y=mean, colour=variable))+
  geom_point(aes(shape = group), size = 2)+
  geom_line(alpha = 0.5, linetype = "dashed")+
  geom_line(data = data %>% filter(Time < 2), size = 0.8) +
  geom_line(data = data %>% filter(Time > 1), size = 0.8) +
  geom_errorbar(aes(x=Time, ymin=pmax(0,mean-se), ymax=mean+se), size = 0.7) +
  scale_y_continuous(trans=log10_trans(),
                     breaks=trans_breaks("log10", function(x) 10^x),
                     labels=trans_format("log10", math_format(10^.x))) +
  scale_x_continuous(breaks = c(0,1,2,3), labels = c("0", "24", "0", "24")) +
  labs(y = "cfu or pfu per mL", x = "Time (hours)", colour = "")+
  theme_bw() +
  scale_colour_manual(values=c("white","#685cc4","#6db356","#c2484d","white","white","#c88a33"),
                      drop = FALSE,
                      labels = c("Bacteria:",
                                 bquote(B[E]),
                                 bquote(B[T]),
                                 bquote(B[ET]),
                                 " ",
                                 "Phage:",
                                 bquote(P[L])),
                      guide = guide_legend(override.aes = list(shape = c(19,19,19,19,19,17,17)))) +
  guides(linetype = F, shape = F) +
  theme(axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.title.y = element_text(size=12),
        legend.text = element_text(size=12),
        strip.text.x = element_text(size=12))

p3a

ggsave(here::here("Figures", "fig3a.png"), dpi = 600)

p3b = ggplot() + background_image(readPNG(here::here("Figures", "fig3b.png"))) + theme_void()

plot_grid(p3a,
          NULL,
          plot_grid(NULL, p3b, NULL, nrow = 1, rel_widths = c(0.06,1,0.01)),
          nrow = 3,
          rel_heights = c(1,0.05,1),
          labels = c("a", "", "b"))

ggsave(here::here("Figures", "fig3.png"), dpi = 600, height = 10, width = 7)
