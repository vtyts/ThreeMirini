library(vegan)
library(grid)
library(ggrepel)
library(cowplot)
library(scales)
library(car)
library(tidyverse)
library(ggpattern)
library(wesanderson)
library(stringr)
library(splitstackshape)

## Data -----
var_data <- read.delim("variables_ranges_all_species_named.csv", sep = ",", header = F, quote = "", dec = ".") #for now manually added a column for species names and groups (record, all, reduced)
var_data <- var_data %>% 
  mutate(across(
    everything(),
    ~ map_chr(.x, ~ gsub("\"", "", .x))
  )) # for some reason there was a ton of quotes, deleted them all at this step
colnames(var_data) <- var_data[1,]
var_data <- var_data[-1,]
var_data$Species_Model <- as.factor(paste(var_data$Species, var_data$Model))
var_data <- var_data %>% mutate_at(c(4:41), as.numeric) %>% mutate_at(c("Species", "Model"), as.factor)
str(var_data)
#add mean for each bio value in separate column

#theme_set(theme_bw(base_size = 15))
theme_set(theme_linedraw())

# Palette to tie the exact color, pattern and  to each species and model ----
#palette_S = setNames(object = scales::hue_pal()(3), nm = unique(var_data$Species))
#palette_B = setNames(object = brewer_pal("div", 'Set2')(3), nm = unique(var_data$Species))
palette_W = setNames(object = wes_palette(name = "FantasticFox1", n = 3, type = "discrete" ), nm = unique(var_data$Species))
vars_pattern <- c(record = "none", CF = "circle", CR = "stripe")
var_shape <- c(record = 15, CF = 16, CR = 17)
var_labels <- c("Liocoris tripustulatus record" = "record", "Liocoris tripustulatus CF" = "CF", "Liocoris tripustulatus CR" = "CR", "Lygus punctatus record" = "record", "Lygus punctatus CF" = "CF", "Lygus punctatus CR" = "CR", "Lygocoris pabulinus record" = "record", "Lygocoris pabulinus CF" = "CF", "Lygocoris pabulinus CR" = "CR")

# Mapping each variable -----

#bio1
bio1_plot <- ggplot(var_data, aes(x = Species_Model, fill = Species, pattern = Model)) + 
  geom_crossbar_pattern(aes(ymin = bio01min, ymax = bio01max, 
                            y = rowMeans(var_data[,c(4:5)])),
                        colour = 'black', 
                        pattern_color = NA, 
                        pattern_fill = 'black',
                        show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = stringr::str_wrap("Bio1: Annual Mean Temperature", width = 25)) + 
  scale_fill_manual(values = palette_W, 
                    guide = guide_legend(override.aes = list(pattern = "none", 
                                                             colour = NA), 
                                         label.theme = element_text(face = "italic",
                                                                    size = 10))) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = -0.5, size = 12, face = 'bold'), 
        axis.text=element_text(colour="black"),
        legend.key = element_rect(colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(0, 0, 0, 0), "mm")) +
  scale_pattern_manual(values = vars_pattern,
                       guide = guide_legend(override.aes = list(colour = NA, 
                                                                pattern_color = 'black'))) +
  scale_x_discrete(labels=var_labels) +
  scale_y_continuous(n.breaks = 10)

#bio2
bio2_plot <- ggplot(var_data, aes(x = Species_Model, fill = Species, pattern = Model)) + 
  geom_crossbar_pattern(aes(ymin = bio02min, ymax = bio02max, 
                            y = rowMeans(var_data[,c(6:7)])), 
                        colour = 'black', 
                        pattern_color = NA, 
                        pattern_fill = 'black',
                        show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = stringr::str_wrap("Bio2: Mean Diurnal Range", width = 25)) + 
  scale_fill_manual(values = palette_W, 
                    guide = guide_legend(override.aes = list(pattern = "none", 
                                                             colour = NA), 
                                         label.theme = element_text(face = "italic", 
                                                                    size = 10))) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 12, face = 'bold'), 
        axis.text=element_text(colour="black"),
        legend.key = element_rect(colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(4, 0, 0, 1), "mm")) +
  scale_pattern_manual(values = vars_pattern,
                       guide = guide_legend(override.aes = list(colour = NA, 
                                                                pattern_color = 'black'))) +
  scale_x_discrete(labels=var_labels) +
  scale_y_continuous(n.breaks = 10)

#bio3
bio3_plot <- ggplot(var_data, aes(x = Species_Model, fill = Species, pattern = Model)) + 
  geom_crossbar_pattern(aes(ymin = bio03min, ymax = bio03max, 
                            y = rowMeans(var_data[,c(8:9)])), 
                        colour = 'black', 
                        pattern_color = NA, 
                        pattern_fill = 'black',
                        show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = stringr::str_wrap("\nBio3: Isothermality", width = 25)) + 
  scale_fill_manual(values = palette_W, 
                    guide = guide_legend(override.aes = list(pattern = "none", 
                                                             colour = NA), 
                                         label.theme = element_text(face = "italic", 
                                                                    size = 10))) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 12, face = 'bold'), 
        axis.text=element_text(colour="black"),
        legend.key = element_rect(colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(4, 0, 0, 1), "mm")) +
  scale_pattern_manual(values = vars_pattern,
                       guide = guide_legend(override.aes = list(colour = NA, 
                                                                pattern_color = 'black'))) +
  scale_x_discrete(labels=var_labels) +
  scale_y_continuous(n.breaks = 10)

#bio4
bio4_plot <- ggplot(var_data, aes(x = Species_Model, fill = Species, pattern = Model)) + 
  geom_crossbar_pattern(aes(ymin = bio04min, ymax = bio04max, 
                            y = rowMeans(var_data[,c(10:11)])), 
                        colour = 'black', 
                        pattern_color = NA, 
                        pattern_fill = 'black',
                        show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = stringr::str_wrap("Bio4: Temperature Seasonality", width = 25)) + 
  scale_fill_manual(values = palette_W, 
                    guide = guide_legend(override.aes = list(pattern = "none", 
                                                             colour = NA), 
                                         label.theme = element_text(face = "italic", 
                                                                    size = 10))) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = -0.5, size = 12, face = 'bold'), 
        axis.text=element_text(colour="black"),
        legend.key = element_rect(colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(0, 0, 0, 1), "mm")) +
  scale_pattern_manual(values = vars_pattern,
                       guide = guide_legend(override.aes = list(colour = NA, 
                                                                pattern_color = 'black'))) +
  scale_x_discrete(labels=var_labels) +
  scale_y_continuous(n.breaks = 10)

#bio5
bio5_plot <- ggplot(var_data, aes(x = Species_Model, fill = Species, pattern = Model)) + 
  geom_crossbar_pattern(aes(ymin = bio05min, ymax = bio05max, 
                            y = rowMeans(var_data[,c(12:13)])), 
                        colour = 'black', 
                        pattern_color = NA, 
                        pattern_fill = 'black',
                        show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = stringr::str_wrap("Bio5: Max Temperature of Warmest Month", width = 25)) + 
  scale_fill_manual(values = palette_W, 
                    guide = guide_legend(override.aes = list(pattern = "none", 
                                                             colour = NA), 
                                         label.theme = element_text(face = "italic", 
                                                                    size = 10))) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = -0.5, size = 12, face = 'bold'), 
        axis.text=element_text(colour="black"),
        legend.key = element_rect(colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(0, 0, 0, 0), "mm")) +
  scale_pattern_manual(values = vars_pattern,
                       guide = guide_legend(override.aes = list(colour = NA, 
                                                                pattern_color = 'black'))) +
  scale_x_discrete(labels=var_labels) +
  scale_y_continuous(n.breaks = 10)

#bio6
bio6_plot <- ggplot(var_data, aes(x = Species_Model, fill = Species, pattern = Model)) + 
  geom_crossbar_pattern(aes(ymin = bio06min, ymax = bio06max, 
                            y = rowMeans(var_data[,c(14:15)])), 
                        colour = 'black', 
                        pattern_color = NA, 
                        pattern_fill = 'black',
                        show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = stringr::str_wrap("Bio6: Min Temperature of Coldest Month", width = 25)) + 
  scale_fill_manual(values = palette_W, 
                    guide = guide_legend(override.aes = list(pattern = "none", 
                                                             colour = NA), 
                                         label.theme = element_text(face = "italic", 
                                                                    size = 10))) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = -0.5, size = 12, face = 'bold'), 
        axis.text=element_text(colour="black"),
        legend.key = element_rect(colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(0, 0, 0, 1), "mm")) +
  scale_pattern_manual(values = vars_pattern,
                       guide = guide_legend(override.aes = list(colour = NA, 
                                                                pattern_color = 'black'))) +
  scale_x_discrete(labels=var_labels) +
  scale_y_continuous(n.breaks = 10)

#bio7
bio7_plot <- ggplot(var_data, aes(x = Species_Model, fill = Species, pattern = Model)) + 
  geom_crossbar_pattern(aes(ymin = bio07min, ymax = bio07max, 
                            y = rowMeans(var_data[,c(16:17)])), 
                        colour = 'black', 
                        pattern_color = NA, 
                        pattern_fill = 'black',
                        show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = stringr::str_wrap("Bio7: Temperature Annual Range", width = 25)) + 
  scale_fill_manual(values = palette_W, 
                    guide = guide_legend(override.aes = list(pattern = "none", 
                                                             colour = NA), 
                                         label.theme = element_text(face = "italic", 
                                                                    size = 10))) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = -0.5, size = 12, face = 'bold'), 
        axis.text=element_text(colour="black"),
        legend.key = element_rect(colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(0, 0, 0, 1), "mm")) +
  scale_pattern_manual(values = vars_pattern,
                       guide = guide_legend(override.aes = list(colour = NA, 
                                                                pattern_color = 'black'))) +
  scale_x_discrete(labels=var_labels) +
  scale_y_continuous(n.breaks = 10)

#bio8
bio8_plot <- ggplot(var_data, aes(x = Species_Model, fill = Species, pattern = Model)) + 
  geom_crossbar_pattern(aes(ymin = bio08min, ymax = bio08max, 
                            y = rowMeans(var_data[,c(18:19)])), 
                        colour = 'black', 
                        pattern_color = NA, 
                        pattern_fill = 'black',
                        show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = stringr::str_wrap("Bio8: Mean Temperature of Wettest Quarter", width = 25)) + 
  scale_fill_manual(values = palette_W, 
                    guide = guide_legend(override.aes = list(pattern = "none", 
                                                             colour = NA), 
                                         label.theme = element_text(face = "italic",
                                                                    size = 10))) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = -0.5, size = 12, face = 'bold'), 
        axis.text=element_text(colour="black"),
        legend.key = element_rect(colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(0, 0, 0, 1), "mm")) +
  scale_pattern_manual(values = vars_pattern,
                       guide = guide_legend(override.aes = list(colour = NA, 
                                                                pattern_color = 'black'))) +
  scale_x_discrete(labels=var_labels) +
  scale_y_continuous(n.breaks = 10)

#bio9
bio9_plot <- ggplot(var_data, aes(x = Species_Model, fill = Species, pattern = Model)) + 
  geom_crossbar_pattern(aes(ymin = bio09min, ymax = bio09max, 
                            y = rowMeans(var_data[,c(20:21)])), 
                        colour = 'black', 
                        pattern_color = NA, 
                        pattern_fill = 'black',
                        show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = stringr::str_wrap("Bio9: Mean Temperature of Driest Quarter", width = 25)) + 
  scale_fill_manual(values = palette_W, 
                    guide = guide_legend(override.aes = list(pattern = "none", 
                                                             colour = NA), 
                                         label.theme = element_text(face = "italic",
                                                                    size = 10))) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = -0.5, size = 12, face = 'bold'), 
        axis.text=element_text(colour="black"),
        legend.key = element_rect(colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(0, 0, 0, 0), "mm")) +
  scale_pattern_manual(values = vars_pattern,
                       guide = guide_legend(override.aes = list(colour = NA, 
                                                                pattern_color = 'black'))) +
  scale_x_discrete(labels=var_labels) +
  scale_y_continuous(n.breaks = 10)


#bio10
bio10_plot <- ggplot(var_data, aes(x = Species_Model, fill = Species, pattern = Model)) + 
  geom_crossbar_pattern(aes(ymin = bio10min, ymax = bio10max, 
                            y = rowMeans(var_data[,c(22:23)])), 
                        colour = 'black', 
                        pattern_color = NA, 
                        pattern_fill = 'black',
                        show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = stringr::str_wrap("Bio10: Mean Temperature of Warmest Quarter", width = 25)) + 
  scale_fill_manual(values = palette_W, 
                    guide = guide_legend(override.aes = list(pattern = "none", 
                                                             colour = NA), 
                                         label.theme = element_text(face = "italic", 
                                                                    size = 10))) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1), 
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = -0.5, size = 12, face = 'bold'), 
        axis.text=element_text(colour="black"),
        legend.key = element_rect(colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(0, 0, 0, 1), "mm")) +
  scale_pattern_manual(values = vars_pattern,
                       guide = guide_legend(override.aes = list(colour = NA, 
                                                                pattern_color = 'black'))) +
  scale_x_discrete(labels=var_labels) +
  scale_y_continuous(n.breaks = 10)

#bio11
bio11_plot <- ggplot(var_data, aes(x = Species_Model, fill = Species, pattern = Model)) + 
  geom_crossbar_pattern(aes(ymin = bio11min, ymax = bio11max, 
                            y = rowMeans(var_data[,c(24:25)])), 
                        colour = 'black', 
                        pattern_color = NA, 
                        pattern_fill = 'black',
                        show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = stringr::str_wrap("Bio11: Mean Temperature of Coldest Quarter", width = 25)) + 
  scale_fill_manual(values = palette_W, 
                    guide = guide_legend(override.aes = list(pattern = "none", 
                                                             colour = NA), 
                                         label.theme = element_text(face = "italic", 
                                                                    size = 10))) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1), 
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = -0.5, size = 12, face = 'bold'), 
        axis.text=element_text(colour="black"),
        legend.key = element_rect(colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(0, 0, 0, 1), "mm")) +
  scale_pattern_manual(values = vars_pattern,
                       guide = guide_legend(override.aes = list(colour = NA, 
                                                                pattern_color = 'black'))) +
  scale_x_discrete(labels=var_labels) +
  scale_y_continuous(n.breaks = 10)

#bio12
bio12_plot <- ggplot(var_data, aes(x = Species_Model, fill = Species, pattern = Model)) + 
  geom_crossbar_pattern(aes(ymin = bio12min, ymax = bio12max, 
                            y = rowMeans(var_data[,c(26:27)])), 
                        colour = 'black', 
                        pattern_color = NA, 
                        pattern_fill = 'black',
                        show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = stringr::str_wrap("Bio12: Annual Precipitation", width = 25)) + 
  scale_fill_manual(values = palette_W, 
                    guide = guide_legend(override.aes = list(pattern = "none", 
                                                             colour = NA), 
                                         label.theme = element_text(face = "italic", 
                                                                    size = 10))) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1), 
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = -0.5, size = 12, face = 'bold'), 
        axis.text=element_text(colour="black"),
        legend.key = element_rect(colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(0, 0, 0, 1), "mm")) +
  scale_pattern_manual(values = vars_pattern,
                       guide = guide_legend(override.aes = list(colour = NA, 
                                                                pattern_color = 'black'))) +
  scale_x_discrete(labels=var_labels) +
  scale_y_continuous(n.breaks = 10)

#bio13
bio13_plot <- ggplot(var_data, aes(x = Species_Model, fill = Species, pattern = Model)) + 
  geom_crossbar_pattern(aes(ymin = bio13min, ymax = bio13max, 
                            y = rowMeans(var_data[,c(28:29)])), 
                        colour = 'black', 
                        pattern_color = NA, 
                        pattern_fill = 'black',
                        show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = stringr::str_wrap("Bio13: Precipitation of Wettest Month", width = 25)) + 
  scale_fill_manual(values = palette_W, 
                    guide = guide_legend(override.aes = list(pattern = "none", 
                                                             colour = NA), 
                                         label.theme = element_text(face = "italic", 
                                                                    size = 10))) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1), 
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = -0.5, size = 12, face = 'bold'), 
        axis.text=element_text(colour="black"),
        legend.key = element_rect(colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(0, 0, 0, 0), "mm")) +
  scale_pattern_manual(values = vars_pattern,
                       guide = guide_legend(override.aes = list(colour = NA, 
                                                                pattern_color = 'black'))) +
  scale_x_discrete(labels=var_labels) +
  scale_y_continuous(n.breaks = 10)

#bio14
bio14_plot <- ggplot(var_data, aes(x = Species_Model, fill = Species, pattern = Model)) + 
  geom_crossbar_pattern(aes(ymin = bio14min, ymax = bio14max, 
                            y = rowMeans(var_data[,c(30:31)])), 
                        colour = 'black', 
                        pattern_color = NA, 
                        pattern_fill = 'black',
                        show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = stringr::str_wrap("Bio14: Precipitation of Driest Month", width = 25)) + 
  scale_fill_manual(values = palette_W, 
                    guide = guide_legend(override.aes = list(pattern = "none", 
                                                             colour = NA), 
                                         label.theme = element_text(face = "italic", 
                                                                    size = 10))) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1), 
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = -0.5, size = 12, face = 'bold'), 
        axis.text=element_text(colour="black"),
        legend.key = element_rect(colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(0, 0, 0, 1), "mm")) +
  scale_pattern_manual(values = vars_pattern,
                       guide = guide_legend(override.aes = list(colour = NA, 
                                                                pattern_color = 'black'))) +
  scale_x_discrete(labels=var_labels) +
  scale_y_continuous(n.breaks = 10)

#bio15
bio15_plot <- ggplot(var_data, aes(x = Species_Model, fill = Species, pattern = Model)) + 
  geom_crossbar_pattern(aes(ymin = bio15min, ymax = bio15max, 
                            y = rowMeans(var_data[,c(32:33)])), 
                        colour = 'black', 
                        pattern_color = NA, 
                        pattern_fill = 'black',
                        show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = stringr::str_wrap("Bio15: Precipitation Seasonality", width = 25)) + 
  scale_fill_manual(values = palette_W, 
                    guide = guide_legend(override.aes = list(pattern = "none", 
                                                             colour = NA), 
                                         label.theme = element_text(face = "italic", 
                                                                    size = 10))) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1), 
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = -0.5, size = 12, face = 'bold'), 
        axis.text=element_text(colour="black"),
        legend.key = element_rect(colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(0, 0, 0, 1), "mm")) +
  scale_pattern_manual(values = vars_pattern,
                       guide = guide_legend(override.aes = list(colour = NA, 
                                                                pattern_color = 'black'))) +
  scale_x_discrete(labels=var_labels) +
  scale_y_continuous(n.breaks = 10)

#bio16
bio16_plot <- ggplot(var_data, aes(x = Species_Model, fill = Species, pattern = Model)) + 
  geom_crossbar_pattern(aes(ymin = bio16min, ymax = bio16max, 
                            y = rowMeans(var_data[,c(34:35)])), 
                        colour = 'black', 
                        pattern_color = NA, 
                        pattern_fill = 'black',
                        show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = stringr::str_wrap("Bio16: Precipitation of Wettest Quarter", width = 25)) + 
  scale_fill_manual(values = palette_W, 
                    guide = guide_legend(override.aes = list(pattern = "none", 
                                                             colour = NA), 
                                         label.theme = element_text(face = "italic", 
                                                                    size = 10))) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1), 
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = -0.5, size = 12, face = 'bold'), 
        axis.text=element_text(colour="black"),
        legend.key = element_rect(colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(0, 0, 0, 1), "mm")) +
  scale_pattern_manual(values = vars_pattern,
                       guide = guide_legend(override.aes = list(colour = NA, 
                                                                pattern_color = 'black'))) +
  scale_x_discrete(labels=var_labels) +
  scale_y_continuous(n.breaks = 10)

#bio17
bio17_plot <- ggplot(var_data, aes(x = Species_Model, fill = Species, pattern = Model)) + 
  geom_crossbar_pattern(aes(ymin = bio17min, ymax = bio17max, 
                            y = rowMeans(var_data[,c(36:37)])), 
                        colour = 'black', 
                        pattern_color = NA, 
                        pattern_fill = 'black',
                        show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = stringr::str_wrap("Bio17: Precipitation of Driest Quarter", width = 25)) + 
  scale_fill_manual(values = palette_W, 
                    guide = guide_legend(override.aes = list(pattern = "none", 
                                                             colour = NA), 
                                         label.theme = element_text(face = "italic", 
                                                                    size = 10))) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1), 
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = -0.5, size = 12, face = 'bold'), 
        axis.text=element_text(colour="black"),
        legend.key = element_rect(colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(0, 0, 0, 0), "mm")) +
  scale_pattern_manual(values = vars_pattern,
                       guide = guide_legend(override.aes = list(colour = NA, 
                                                                pattern_color = 'black'))) +
  scale_x_discrete(labels=var_labels) +
  scale_y_continuous(n.breaks = 10)

#bio18
bio18_plot <- ggplot(var_data, aes(x = Species_Model, fill = Species, pattern = Model)) + 
  geom_crossbar_pattern(aes(ymin = bio18min, ymax = bio18max, 
                            y = rowMeans(var_data[,c(38:39)])), 
                        colour = 'black', 
                        pattern_color = NA, 
                        pattern_fill = 'black',
                        show.legend = FALSE) +
  labs(x = NULL, y = NULL,
       title = stringr::str_wrap("Bio18: Precipitation of Warmest Quarter", width = 25)) + 
  scale_fill_manual(values = palette_W, 
                    guide = guide_legend(override.aes = list(pattern = "none", 
                                                             colour = NA), 
                                         label.theme = element_text(face = "italic", 
                                                                    size = 10))) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1), 
        legend.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, vjust = -0.5, size = 12, face = 'bold'), 
        axis.text=element_text(colour="black"),
        legend.key = element_rect(colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(0, 0, 0, 1), "mm")) +
  scale_pattern_manual(values = vars_pattern,
                       guide = guide_legend(override.aes = list(colour = NA, 
                                                                pattern_color = 'black'))) +
  scale_x_discrete(labels=var_labels) +
  scale_y_continuous(n.breaks = 10)

#bio19
bio19_plot <- ggplot(var_data, aes(x = Species_Model, fill = Species, pattern = Model)) + 
  geom_crossbar_pattern(aes(ymin = bio19min, ymax = bio19max, 
                            y = rowMeans(var_data[,c(40:41)])), 
                        colour = 'black', 
                        pattern_color = NA, 
                        pattern_fill = 'black') +
  labs(x = NULL, y = NULL,
       title = stringr::str_wrap("Bio19: Precipitation of Coldest Quarter", width = 25)) + 
  scale_fill_manual(values = palette_W, 
                    guide = guide_legend(override.aes = list(pattern = "none", 
                                                             colour = NA), 
                                         label.theme = element_text(face = "italic", 
                                                                    size = 12),
                                         order = 1)) +
  theme(axis.text.x=element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 12),
        legend.title = element_text(face = "bold", size = 12),
        plot.title = element_text(hjust = 0.5, vjust = -0.5, size = 12, face = 'bold'), 
        axis.text=element_text(colour="black"),
        legend.key = element_rect(colour = "black"),
        panel.background = element_rect(fill = NA),
        plot.margin = unit(c(0, 0, 0, 1), "mm")) +
  scale_pattern_manual(values = vars_pattern,
                       guide = guide_legend(override.aes = list(colour = NA, 
                                                                pattern_color = 'black'),
                                            order = 2)) +
  scale_x_discrete(labels=var_labels) +
  scale_y_continuous(n.breaks = 10)

#Arranging all together
first_col_4 <- plot_grid(bio1_plot, bio2_plot, bio3_plot, bio4_plot, bio5_plot, bio6_plot, bio7_plot, bio8_plot, bio9_plot, bio10_plot, bio11_plot, bio12_plot, bio13_plot, bio14_plot, bio15_plot, bio16_plot, ncol = 4)
second_col_4 <- plot_grid(bio17_plot, bio18_plot, bio19_plot, NULL, ncol = 4, rel_widths = c(1, 1, 1.9, 0.1))
var_col_4 <- plot_grid(first_col_4, second_col_4, ncol = 1, rel_heights = c(4, 1))

ggsave('All_vars_plots_black_grid.tiff', plot = var_col_4, path = './Final_tables', scale = 2.1, width = 119, height = 195, device='tiff', dpi=600,  units = c("mm"))

print('done')

## PCA all -----
lygus_data <- read.csv('punctatus_for_maxent.csv')
lygocoris_data <- read.csv('pabulinus_swd_for_maxent.csv')
liocoris_data <- read.csv('liocoris_swd_for_maxent.csv')
all_data <- rbind(lygus_data, lygocoris_data, liocoris_data)
all_data <-all_data %>% 
  mutate(species = str_replace(species, "punctatus", "Lygus punctatus")) %>% 
  mutate(species = str_replace(species, "pabulinus", "Lygocoris pabulinus")) %>% 
  mutate(species = str_replace(species, "liocoris", "Liocoris tripustulatus"))%>% 
  mutate_at(c(4:22), as.numeric) %>%
  mutate_at(1, as.factor)
str(all_data)
summary(all_data)

str(palette_W)
var_pca <- rda(all_data[, -c(1:3)], scale = TRUE)
summary(var_pca)
eig <- eigenvals(var_pca)[1:17]
eig*100/sum(eig)
ar <- arrow(length = unit(0.2, "cm")) #arrow for loading plot from grid package

# Estimating the number of significant PCs using broken-stick model
screeplot(var_pca, type = "lines", bstick = TRUE)

### Loadings and loading plot ----
all_sc <- scores(var_pca, display = "species", choices = c(1, 2, 3), scaling = 0)
write.table(all_sc, file="all_scores.tsv", sep = "\t")
df_load <- as.data.frame(scores(var_pca, display = "species", choices = c(1, 2, 3), scaling = "species"))

p_load <- ggplot(df_load) +
  geom_text_repel(aes(x = PC1, y = PC2, label = rownames(df_load)),
                  size = 2.7, max.overlaps = 30, force_pull=1, nudge_x = 0.125, nudge_y = 0, seed = 5, segment.colour = 'grey') +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               color = "coral2", arrow = ar, alpha = 0.8) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')) +
#  coord_equal(xlim = c(-0.5, 2.5), ylim = c(-1.5, 1.5)) + 
  ggtitle('Loading plot')

p_load_3 <- ggplot(df_load) +
  geom_text_repel(aes(x = PC2, y = PC3, label = rownames(df_load)),
                  size = 2, max.overlaps = 50, force_pull=2, nudge_x = 0.2, seed = 256) +
  geom_segment(aes(x = 0, y = 0, xend = PC2, yend = PC3),
               color = "coral2", arrow = ar, alpha = 0.7) +
  #  coord_equal(xlim = c(-0.5, 2.5), ylim = c(-1.5, 1.5)) + 
  ggtitle('Loading plot')

# Data for score plots
df_scores <- data.frame(all_data$species,
                          scores(var_pca, display = "sites", 
                                 choices = c(1, 2, 3), 
                                 scaling = "sites"))
colnames(df_scores)[1] <- "species"
str(df_scores)

### Score plot ----
p_scores <- ggplot(df_scores, aes(x = PC1, y = PC2, color = species)) + 
  geom_point(size = 1.5, alpha = 0.5,
             show.legend = FALSE) + 
#  coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2)) + 
  ggtitle('Score plot') +
  scale_color_manual(values = palette_W) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        legend.text = element_text(face = "italic"))
  
p_scores_3 <- ggplot(df_scores, aes(x = PC2, y = PC3, color = species)) + 
  geom_point(size = 2.5, alpha = 0.5) + 
    #  coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2)) + 
  ggtitle('Score plot') +
  scale_color_manual(values = palette_W)+
  theme(legend.text = element_text(face = "italic"))

# Loading and score plots
both_plots<-plot_grid(p_load, p_scores, align = "h") 
both_plots_3<-plot_grid(p_load_3, p_scores_3, align = "h")
                      #rel_widths = c(0.45, 0.67)

ggsave('both_plots.tiff', plot = both_plots, path = './Final_tables', scale = 2, width = 20, height = 10, device='tiff', dpi=300,  units = c("cm"))
ggsave('both_plots3.tiff', plot = both_plots_3, path = './Final_tables', scale = 2, width = 20, height = 10, device='tiff', dpi=300,  units = c("cm"))

### Analysis of variance of PC1  ----
df <- data.frame(species = all_data$species,
                   scores(var_pca, display = "sites", choices = c(1, 2, 3), scaling = "sites"))
str(df)
mod1 <- lm(PC1 ~ species, data = df)
anova(mod1)

# Testing for conditions
mod_diag1 <- fortify(mod1)
res_p1 <- ggplot(data = mod_diag1, aes(x = .fitted, y = .stdresid)) + 
  geom_point(aes(size = .cooksd)) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(method="loess", se=FALSE)
mean_val1 <- mean(mod_diag1$.stdresid)
sd_val1 <- sd(mod_diag1$.stdresid)
norm_p1 <- ggplot(mod_diag1, aes(sample = .stdresid)) + 
  geom_point(stat = "qq") + 
  geom_abline(intercept = mean_val1, slope = sd_val1)
plot_grid(res_p1, norm_p1, ncol = 2, rel_widths = c(0.55, 0.45))

leveneTest(PC1 ~ species, data = df)
ggplot(mod_diag1, aes(x = .fitted, y = .stdresid)) + geom_point() + ggtitle("Residuals plot of model predictors") + geom_hline(yintercept = 0)
ggplot(mod_diag1, aes(x = species, y = .stdresid)) + geom_boxplot() + ggtitle("Residuals plot of model predictors") + geom_hline(yintercept = 0)

# Point range plot for PC1
df$species <- reorder(df$species, df$PC1, FUN=mean)

pc1_plot <- ggplot(df, aes(x = species, y = PC1, color = species)) +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_boot", size = 1, linewidth = 1) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) + 
  ggtitle('Point range plot for PC1') +
  scale_color_manual(values=palette_W) +
  theme(legend.position = 'none', axis.text.x = element_text(face = "italic"),
        plot.title = element_text(hjust = 0.5, face = 'bold'))

# Tukey's test of PC1
TukeyHSD(aov(mod1))

### Analysis of variance of PC2 ----
mod2 <- lm(PC2 ~ species, data = df)
anova(mod2)

# Testing for conditions
mod_diag2 <- fortify(mod2)
res_p2 <- ggplot(data = mod_diag2, aes(x = .fitted, y = .stdresid)) + 
  geom_point(aes(size = .cooksd)) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(method="loess", se=FALSE)
mean_val2 <- mean(mod_diag2$.stdresid)
sd_val2 <- sd(mod_diag2$.stdresid)
norm_p2 <- ggplot(mod_diag2, aes(sample = .stdresid)) + 
  geom_point(stat = "qq") + 
  geom_abline(intercept = mean_val2, slope = sd_val2)
plot_grid(res_p2, norm_p2, ncol = 2, rel_widths = c(0.55, 0.45))

leveneTest(PC2 ~ species, data = df)
ggplot(mod_diag2, aes(x = .fitted, y = .stdresid)) + geom_point() + ggtitle("Residuals plot of model predictors") + geom_hline(yintercept = 0)
ggplot(mod_diag2, aes(x = species, y = .stdresid)) + geom_boxplot() + ggtitle("Residuals plot of model predictors") + geom_hline(yintercept = 0)

# Point range plot for PC2
df$species <- reorder(df$species, df$PC2, FUN=mean)

pc2_plot <- ggplot(df, aes(x = species, y = PC2, color = species)) +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_boot", size = 1, linewidth = 1) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) + 
  ggtitle('Point range plot for PC2') +
  scale_color_manual(values=palette_W) +
  theme(legend.text = element_text(face = "italic", size = 12), 
        axis.text.x = element_text(face = "italic"), 
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        legend.title = element_text(face = "bold", size = 14)) +
  labs(color='Species') 

# Tukey's test of PC2
TukeyHSD(aov(mod2))

### Analysis of variance of PC3 ----
mod3 <- lm(PC3 ~ species, data = df)
anova(mod3)

# Testing for conditions
mod_diag3 <- fortify(mod3)
res_p3 <- ggplot(data = mod_diag3, aes(x = .fitted, y = .stdresid)) + 
  geom_point(aes(size = .cooksd)) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(method="loess", se=FALSE)
mean_val3 <- mean(mod_diag3$.stdresid)
sd_val3 <- sd(mod_diag3$.stdresid)
norm_p3 <- ggplot(mod_diag3, aes(sample = .stdresid)) + 
  geom_point(stat = "qq") + 
  geom_abline(intercept = mean_val3, slope = sd_val3)
plot_grid(res_p3, norm_p3, ncol = 2, rel_widths = c(0.55, 0.45))

leveneTest(PC3 ~ species, data = df)
ggplot(mod_diag3, aes(x = .fitted, y = .stdresid)) + geom_point() + ggtitle("Residuals plot of model predictors") + geom_hline(yintercept = 0)
ggplot(mod_diag3, aes(x = species, y = .stdresid)) + geom_boxplot() + ggtitle("Residuals plot of model predictors") + geom_hline(yintercept = 0)

# Point range plot for PC3
df$species <- reorder(df$species, df$PC3, FUN=mean)

pc3_plot <- ggplot(df, aes(x = species, y = PC3, color = species)) +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_boot", size = 3) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) + 
  ggtitle('Point range plot for PC3') +
  scale_color_manual(values=palette_W) +
  theme(legend.text = element_text(face = "italic"), axis.text.x = element_text(face = "italic"))

# Tukey's test of PC3
TukeyHSD(aov(mod3))

both_plots_pc<- plot_grid(pc1_plot, pc2_plot, rel_widths = c(0.40, 0.58))
ggsave('PC_plot.tiff', plot = both_plots_pc, path = './Final_tables', scale = 1.5, width = 20, height = 10, device='tiff', dpi=300,  units = c("cm"))

all_three_all <- plot_grid(p_load, p_scores, pc1_plot, pc2_plot, ncol = 4, rel_widths = c(0.92, 0.94, 0.84, 1.4))
ggsave('All_PCA_plots_all.tiff', plot = all_tree_all, path = './Final_tables', scale = 3.5, width = 119, height = 30, device='tiff', dpi=600,  units = c("mm"))

first_quad <- plot_grid(p_load, p_scores, ncol = 2, rel_widths = c(0.98, 1.02))
second_quad <- plot_grid(pc1_plot, pc2_plot, ncol = 2, rel_widths = c(0.78, 1.22))
all_three_quad <- plot_grid(first_quad, second_quad, ncol = 1)
ggsave('All_PCA_plots_all_quad.tiff', plot = all_three_quad, path = './Final_tables', scale = 1.7, width = 119, height = 115, device='tiff', dpi=600,  units = c("mm"))

## PCA liocoris, lygocoris -----
liocoris_lygocoris_data <- all_data[all_data$species !='Lygus punctatus', ]
summary(liocoris_lygocoris_data)
theme_set(theme_linedraw(base_size = 11))
str(palette_W)

liocoris_lygocoris_var_pca <- rda(liocoris_lygocoris_data[, -c(1:3)], scale = TRUE)
summary(liocoris_lygocoris_var_pca)
liocoris_lygocoris_eig <- eigenvals(liocoris_lygocoris_var_pca)[1:17]
liocoris_lygocoris_eig*100/sum(liocoris_lygocoris_eig)
ar <- arrow(length = unit(0.2, "cm")) #arrow for loading plot from grid package

# Estimating the number of significant PCs using broken-stick model
screeplot(liocoris_lygocoris_var_pca, type = "lines", bstick = TRUE)

### Loadings and loading plot ----
liocoris_lygocoris_sc <- scores(liocoris_lygocoris_var_pca, display = "species", choices = c(1, 2, 3), scaling = 0)
write.table(liocoris_lygocoris_sc, file="liocoris_lygocoris_scores.tsv", sep = "\t")

liocoris_lygocoris_df_load <- as.data.frame(scores(liocoris_lygocoris_var_pca, display = "species", choices = c(1, 2, 3), scaling = "species"))

liocoris_lygocoris_p_load <- ggplot(liocoris_lygocoris_df_load) +
  geom_text_repel(aes(x = PC1, y = PC2, label = rownames(liocoris_lygocoris_df_load)),
                  size = 2.7, max.overlaps = 30, force_pull=1, nudge_x = 0.125, nudge_y = 0, seed = 5, segment.colour = 'grey') +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               color = "coral2", arrow = ar, alpha = 0.8) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')) +
  #  coord_equal(xlim = c(-0.5, 2.5), ylim = c(-1.5, 1.5)) + 
  ggtitle('Loading plot')


liocoris_lygocoris_p_load_3 <- ggplot(liocoris_lygocoris_df_load) +
  geom_text_repel(aes(x = PC2, y = PC3, label = rownames(liocoris_lygocoris_df_load)),
                  size = 2, max.overlaps = 50, force_pull=2, nudge_x = 0.2, seed = 256) +
  geom_segment(aes(x = 0, y = 0, xend = PC2, yend = PC3),
               color = "coral2", arrow = ar, alpha = 0.7) +
  #  coord_equal(xlim = c(-0.5, 2.5), ylim = c(-1.5, 1.5)) + 
  ggtitle('Loading plot')

# Data for score plots
liocoris_lygocoris_df_scores <- data.frame(liocoris_lygocoris_data$species,
                        scores(liocoris_lygocoris_var_pca, display = "sites", 
                               choices = c(1, 2, 3), 
                               scaling = "sites"))
colnames(liocoris_lygocoris_df_scores)[1] <- "species"
str(liocoris_lygocoris_df_scores)

### Score plot ----
liocoris_lygocoris_p_scores <- ggplot(liocoris_lygocoris_df_scores,
                                      aes(x = PC1, y = PC2, color = species)) + 
  geom_point(size = 1.5, alpha = 0.5,
             show.legend = FALSE) + 
  #  coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2)) + 
  ggtitle('Score plot') +
  scale_color_manual(values = palette_W) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        legend.text = element_text(face = "italic"))

liocoris_lygocoris_p_scores_3 <- ggplot(liocoris_lygocoris_df_scores, 
                                        aes(x = PC2, y = PC3, color = species)) + 
  geom_point(size = 2.5, alpha = 0.5) + 
  #  coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2)) + 
  ggtitle('Score plot') +
  scale_color_manual(values = palette_W)+
  theme(legend.text = element_text(face = "italic"))

# Loading and score plots
liocoris_lygocoris_both_plots<-plot_grid(liocoris_lygocoris_p_load, liocoris_lygocoris_p_scores, align = "h") 
liocoris_lygocoris_both_plots_3<-plot_grid(liocoris_lygocoris_p_load_3, liocoris_lygocoris_p_scores_3, align = "h")
#rel_widths = c(0.45, 0.67)

ggsave('liocoris_lygocoris_both_plots.tiff', plot = liocoris_lygocoris_both_plots, path = './Final_tables', scale = 2, width = 20, height = 10, device='tiff', dpi=300,  units = c("cm"))
ggsave('liocoris_lygocoris_both_plots3.tiff', plot = liocoris_lygocoris_both_plots_3, path = './Final_tables', scale = 2, width = 20, height = 10, device='tiff', dpi=300,  units = c("cm"))

### Analysis of variance of PC1  ----
liocoris_lygocoris_df <- data.frame(species = liocoris_lygocoris_data$species,
                 scores(liocoris_lygocoris_var_pca, display = "sites", choices = c(1, 2, 3), scaling = "sites"))
str(liocoris_lygocoris_df)
mod1 <- lm(PC1 ~ species, data = liocoris_lygocoris_df)
anova(mod1)

# Testing for conditions
mod_diag1 <- fortify(mod1)
res_p1 <- ggplot(data = mod_diag1, aes(x = .fitted, y = .stdresid)) + 
  geom_point(aes(size = .cooksd)) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(method="loess", se=FALSE)
mean_val1 <- mean(mod_diag1$.stdresid)
sd_val1 <- sd(mod_diag1$.stdresid)
norm_p1 <- ggplot(mod_diag1, aes(sample = .stdresid)) + 
  geom_point(stat = "qq") + 
  geom_abline(intercept = mean_val1, slope = sd_val1)
plot_grid(res_p1, norm_p1, ncol = 2, rel_widths = c(0.55, 0.45))

leveneTest(PC1 ~ species, data = liocoris_lygocoris_df)
ggplot(mod_diag1, aes(x = .fitted, y = .stdresid)) + geom_point() + ggtitle("Residuals plot of model predictors") + geom_hline(yintercept = 0)
ggplot(mod_diag1, aes(x = species, y = .stdresid)) + geom_boxplot() + ggtitle("Residuals plot of model predictors") + geom_hline(yintercept = 0)

# Point range plot for PC1
liocoris_lygocoris_df$species <- reorder(liocoris_lygocoris_df$species, liocoris_lygocoris_df$PC1, FUN=mean)

liocoris_lygocoris_pc1_plot <- ggplot(liocoris_lygocoris_df, aes(x = species, y = PC1, color = species)) +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_boot", size = 1, linewidth = 1) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) + 
  ggtitle('Point range plot for PC1') +
  scale_color_manual(values=palette_W) +
  theme(legend.position = 'none', axis.text.x = element_text(face = "italic"),
        plot.title = element_text(hjust = 0.5, face = 'bold'))

### Analysis of variance of PC2 ----
mod2 <- lm(PC2 ~ species, data = liocoris_lygocoris_df)
anova(mod2)

# Testing for conditions
mod_diag2 <- fortify(mod2)
res_p2 <- ggplot(data = mod_diag2, aes(x = .fitted, y = .stdresid)) + 
  geom_point(aes(size = .cooksd)) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(method="loess", se=FALSE)
mean_val2 <- mean(mod_diag2$.stdresid)
sd_val2 <- sd(mod_diag2$.stdresid)
norm_p2 <- ggplot(mod_diag2, aes(sample = .stdresid)) + 
  geom_point(stat = "qq") + 
  geom_abline(intercept = mean_val2, slope = sd_val2)
plot_grid(res_p2, norm_p2, ncol = 2, rel_widths = c(0.55, 0.45))

leveneTest(PC2 ~ species, data = liocoris_lygocoris_df)
ggplot(mod_diag2, aes(x = .fitted, y = .stdresid)) + geom_point() + ggtitle("Residuals plot of model predictors") + geom_hline(yintercept = 0)
ggplot(mod_diag2, aes(x = species, y = .stdresid)) + geom_boxplot() + ggtitle("Residuals plot of model predictors") + geom_hline(yintercept = 0)

# Point range plot for PC2
liocoris_lygocoris_df$species <- reorder(liocoris_lygocoris_df$species, liocoris_lygocoris_df$PC2, FUN=mean)

liocoris_lygocoris_pc2_plot <- ggplot(liocoris_lygocoris_df, 
                                      aes(x = species, y = PC2, color = species)) +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_boot", size = 1, linewidth = 1) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) + 
  ggtitle('Point range plot for PC2') +
  scale_color_manual(values=palette_W) +
  theme(legend.text = element_text(face = "italic", size = 12), 
        axis.text.x = element_text(face = "italic"), 
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        legend.title = element_text(face = "bold", size = 14)) +
  labs(color='Species')

### Analysis of variance of PC3 ----
mod3 <- lm(PC3 ~ species, data = liocoris_lygocoris_df)
anova(mod3) #no point, species are not different

liocoris_lygocoris_both_plots_pc<- plot_grid(liocoris_lygocoris_pc1_plot, liocoris_lygocoris_pc2_plot, rel_widths = c(0.40, 0.58))
ggsave('liocoris_lygocoris_PC_plot.tiff', plot = liocoris_lygocoris_both_plots_pc, path = './Final_tables', scale = 1.5, width = 20, height = 10, device='tiff', dpi=300,  units = c("cm"))


## PCA liocoris, lygus -----
lygocoris_lygus_data <- all_data[all_data$species !='Liocoris tripustulatus', ]
summary(lygocoris_lygus_data)

str(palette_W)
lygocoris_lygus_var_pca <- rda(lygocoris_lygus_data[, -c(1:3)], scale = TRUE)
summary(lygocoris_lygus_var_pca)
lygocoris_lygus_eig <- eigenvals(lygocoris_lygus_var_pca)[1:17]
lygocoris_lygus_eig*100/sum(lygocoris_lygus_eig)
ar <- arrow(length = unit(0.2, "cm")) #arrow for loading plot from grid package

# Estimating the number of significant PCs using broken-stick model
screeplot(lygocoris_lygus_var_pca, type = "lines", bstick = TRUE)

### Loadings and loading plot ----
lygocoris_lygus_sc <- scores(lygocoris_lygus_var_pca, display = "species", choices = c(1, 2, 3), scaling = 0)
write.table(lygocoris_lygus_sc, file="lygocoris_lygus_scores.tsv", sep = "\t")

lygocoris_lygus_df_load <- as.data.frame(scores(lygocoris_lygus_var_pca, display = "species", choices = c(1, 2, 3), scaling = "species"))

lygocoris_lygus_p_load <- ggplot(lygocoris_lygus_df_load) +
  geom_text_repel(aes(x = PC1, y = PC2, label = rownames(lygocoris_lygus_df_load)),
                  size = 2.7, max.overlaps = 30, force_pull=1, nudge_x = 0.125, nudge_y = 0, seed = 5, segment.colour = 'grey') +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               color = "coral2", arrow = ar, alpha = 0.8) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')) +
  #  coord_equal(xlim = c(-0.5, 2.5), ylim = c(-1.5, 1.5)) + 
  ggtitle('Loading plot')

lygocoris_lygus_p_load_3 <- ggplot(lygocoris_lygus_df_load) +
  geom_text_repel(aes(x = PC2, y = PC3, label = rownames(lygocoris_lygus_df_load)),
                  size = 2, max.overlaps = 50, force_pull=2, nudge_x = 0.2, seed = 256) +
  geom_segment(aes(x = 0, y = 0, xend = PC2, yend = PC3),
               color = "coral2", arrow = ar, alpha = 0.7) +
  #  coord_equal(xlim = c(-0.5, 2.5), ylim = c(-1.5, 1.5)) + 
  ggtitle('Loading plot')

# Data for score plots
lygocoris_lygus_df_scores <- data.frame(lygocoris_lygus_data$species,
                        scores(lygocoris_lygus_var_pca, display = "sites", 
                               choices = c(1, 2, 3), 
                               scaling = "sites"))
colnames(lygocoris_lygus_df_scores)[1] <- "species"
str(lygocoris_lygus_df_scores)

### Score plot ----
lygocoris_lygus_p_scores <- ggplot(lygocoris_lygus_df_scores, 
                                   aes(x = PC1, y = PC2, color = species)) + 
  geom_point(size = 1.5, alpha = 0.5,
             show.legend = FALSE) + 
  #  coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2)) + 
  ggtitle('Score plot') +
  scale_color_manual(values = palette_W) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        legend.text = element_text(face = "italic"))

lygocoris_lygus_p_scores_3 <- ggplot(lygocoris_lygus_df_scores, aes(x = PC2, y = PC3, color = species)) + 
  geom_point(size = 2.5, alpha = 0.5) + 
  #  coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2)) + 
  ggtitle('Score plot') +
  scale_color_manual(values = palette_W)+
  theme(legend.text = element_text(face = "italic"))

# Loading and score plots
lygocoris_lygus_both_plots<-plot_grid(lygocoris_lygus_p_load, lygocoris_lygus_p_scores, align = "h") 
lygocoris_lygus_both_plots_3<-plot_grid(lygocoris_lygus_p_load_3, lygocoris_lygus_p_scores_3, align = "h")
#rel_widths = c(0.45, 0.67)

ggsave('lygocoris_lygus_both_plots.tiff', plot = lygocoris_lygus_both_plots, path = './Final_tables', scale = 2, width = 20, height = 10, device='tiff', dpi=300,  units = c("cm"))
ggsave('lygocoris_lygus_both_plots3.tiff', plot = lygocoris_lygus_both_plots_3, path = './Final_tables', scale = 2, width = 20, height = 10, device='tiff', dpi=300,  units = c("cm"))

### Analysis of variance of PC1  ----
lygocoris_lygus_df <- data.frame(species = lygocoris_lygus_data$species,
                 scores(lygocoris_lygus_var_pca, display = "sites", choices = c(1, 2, 3), scaling = "sites"))
str(lygocoris_lygus_df)
mod1 <- lm(PC1 ~ species, data = lygocoris_lygus_df)
anova(mod1)

# Testing for conditions
mod_diag1 <- fortify(mod1)
res_p1 <- ggplot(data = mod_diag1, aes(x = .fitted, y = .stdresid)) + 
  geom_point(aes(size = .cooksd)) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(method="loess", se=FALSE)
mean_val1 <- mean(mod_diag1$.stdresid)
sd_val1 <- sd(mod_diag1$.stdresid)
norm_p1 <- ggplot(mod_diag1, aes(sample = .stdresid)) + 
  geom_point(stat = "qq") + 
  geom_abline(intercept = mean_val1, slope = sd_val1)
plot_grid(res_p1, norm_p1, ncol = 2, rel_widths = c(0.55, 0.45))

leveneTest(PC1 ~ species, data = lygocoris_lygus_df)
ggplot(mod_diag1, aes(x = .fitted, y = .stdresid)) + geom_point() + ggtitle("Residuals plot of model predictors") + geom_hline(yintercept = 0)
ggplot(mod_diag1, aes(x = species, y = .stdresid)) + geom_boxplot() + ggtitle("Residuals plot of model predictors") + geom_hline(yintercept = 0)

# Point range plot for PC1
lygocoris_lygus_df$species <- reorder(lygocoris_lygus_df$species, lygocoris_lygus_df$PC1, FUN=mean)

lygocoris_lygus_pc1_plot <- ggplot(lygocoris_lygus_df, aes(x = species, y = PC1, color = species)) +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_boot", size = 1, linewidth = 1) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) + 
  ggtitle('Point range plot for PC1') +
  scale_color_manual(values=palette_W) +
  theme(legend.position = 'none', axis.text.x = element_text(face = "italic"),
        plot.title = element_text(hjust = 0.5, face = 'bold'))

### Analysis of variance of PC2 ----
mod2 <- lm(PC2 ~ species, data = lygocoris_lygus_df)
anova(mod2)

# Testing for conditions
mod_diag2 <- fortify(mod2)
res_p2 <- ggplot(data = mod_diag2, aes(x = .fitted, y = .stdresid)) + 
  geom_point(aes(size = .cooksd)) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(method="loess", se=FALSE)
mean_val2 <- mean(mod_diag2$.stdresid)
sd_val2 <- sd(mod_diag2$.stdresid)
norm_p2 <- ggplot(mod_diag2, aes(sample = .stdresid)) + 
  geom_point(stat = "qq") + 
  geom_abline(intercept = mean_val2, slope = sd_val2)
plot_grid(res_p2, norm_p2, ncol = 2, rel_widths = c(0.55, 0.45))

leveneTest(PC2 ~ species, data = lygocoris_lygus_df)
ggplot(mod_diag2, aes(x = .fitted, y = .stdresid)) + geom_point() + ggtitle("Residuals plot of model predictors") + geom_hline(yintercept = 0)
ggplot(mod_diag2, aes(x = species, y = .stdresid)) + geom_boxplot() + ggtitle("Residuals plot of model predictors") + geom_hline(yintercept = 0)

# Point range plot for PC2
lygocoris_lygus_df$species <- reorder(lygocoris_lygus_df$species, lygocoris_lygus_df$PC2, FUN=mean)

lygocoris_lygus_pc2_plot <- ggplot(lygocoris_lygus_df, aes(x = species, y = PC2, color = species)) +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_boot", size = 1, linewidth = 1) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) + 
  ggtitle('Point range plot for PC2') +
  scale_color_manual(values=palette_W) +
  theme(legend.text = element_text(face = "italic", size = 12), 
        axis.text.x = element_text(face = "italic"), 
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        legend.title = element_text(face = "bold", size = 14)) +
  labs(color='Species')

### Analysis of variance of PC3 ----
mod3 <- lm(PC3 ~ species, data = lygocoris_lygus_df)
anova(mod3)

lygocoris_lygus_both_plots_pc<- plot_grid(lygocoris_lygus_pc1_plot, lygocoris_lygus_pc2_plot, rel_widths = c(0.40, 0.58))
ggsave('lygocoris_lygus_PC_plot.tiff', plot = lygocoris_lygus_both_plots_pc, path = './Final_tables', scale = 1.5, width = 20, height = 10, device='tiff', dpi=300,  units = c("cm"))


## PCA lygocoris, lygus -----
liocoris_lygus_data <- all_data[all_data$species !='Lygocoris pabulinus', ]
summary(liocoris_lygus_data)


liocoris_lygus_var_pca <- rda(liocoris_lygus_data[, -c(1:3)], scale = TRUE)
summary(liocoris_lygus_var_pca)
liocoris_lygus_eig <- eigenvals(liocoris_lygus_var_pca)[1:17]
liocoris_lygus_eig*100/sum(liocoris_lygus_eig)
ar <- arrow(length = unit(0.2, "cm")) #arrow for loading plot from grid package

# Estimating the number of significant PCs using broken-stick model
screeplot(liocoris_lygus_var_pca, type = "lines", bstick = TRUE)

### Loadings and loading plot ----
liocoris_lygus_sc <- scores(liocoris_lygus_var_pca, display = "species", choices = c(1, 2, 3), scaling = 0)
write.table(liocoris_lygus_sc, file="liocoris_lygus_scores.tsv", sep = "\t")

liocoris_lygus_df_load <- as.data.frame(scores(liocoris_lygus_var_pca, display = "species", choices = c(1, 2, 3), scaling = "species"))

liocoris_lygus_p_load <- ggplot(liocoris_lygus_df_load) +
  geom_text_repel(aes(x = PC1, y = PC2, label = rownames(liocoris_lygus_df_load)),
                  size = 2.7, max.overlaps = 30, force_pull=1, nudge_x = 0.125, nudge_y = 0, seed = 5, segment.colour = 'grey') +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2),
               color = "coral2", arrow = ar, alpha = 0.8) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')) +
  #  coord_equal(xlim = c(-0.5, 2.5), ylim = c(-1.5, 1.5)) + 
  ggtitle('Loading plot')

# Data for score plots
liocoris_lygus_df_scores <- data.frame(liocoris_lygus_data$species,
                        scores(liocoris_lygus_var_pca, display = "sites", 
                               choices = c(1, 2, 3), 
                               scaling = "sites"))
colnames(liocoris_lygus_df_scores)[1] <- "species"
str(liocoris_lygus_df_scores)

### Score plot ----
liocoris_lygus_p_scores <- ggplot(liocoris_lygus_df_scores, 
                                  aes(x = PC1, y = PC2, color = species)) + 
  geom_point(size = 1.5, alpha = 0.5,
             show.legend = FALSE) + 
  #  coord_equal(xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2)) + 
  ggtitle('Score plot') +
  scale_color_manual(values = palette_W) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        legend.text = element_text(face = "italic"))

# Loading and score plots
liocoris_lygus_both_plots<-plot_grid(liocoris_lygus_p_load, liocoris_lygus_p_scores, align = "h") 
#rel_widths = c(0.45, 0.67)

ggsave('liocoris_lygus_both_plots.tiff', plot = liocoris_lygus_both_plots, path = './Final_tables', scale = 2, width = 20, height = 10, device='tiff', dpi=300,  units = c("cm"))

### Analysis of variance of PC1  ----
liocoris_lygus_df <- data.frame(species = liocoris_lygus_data$species,
                 scores(liocoris_lygus_var_pca, display = "sites", choices = c(1, 2, 3), scaling = "sites"))
str(liocoris_lygus_df)
mod1 <- lm(PC1 ~ species, data = liocoris_lygus_df)
anova(mod1)

# Testing for conditions
mod_diag1 <- fortify(mod1)
res_p1 <- ggplot(data = mod_diag1, aes(x = .fitted, y = .stdresid)) + 
  geom_point(aes(size = .cooksd)) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(method="loess", se=FALSE)
mean_val1 <- mean(mod_diag1$.stdresid)
sd_val1 <- sd(mod_diag1$.stdresid)
norm_p1 <- ggplot(mod_diag1, aes(sample = .stdresid)) + 
  geom_point(stat = "qq") + 
  geom_abline(intercept = mean_val1, slope = sd_val1)
plot_grid(res_p1, norm_p1, ncol = 2, rel_widths = c(0.55, 0.45))

leveneTest(PC1 ~ species, data = liocoris_lygus_df)
ggplot(mod_diag1, aes(x = .fitted, y = .stdresid)) + geom_point() + ggtitle("Residuals plot of model predictors") + geom_hline(yintercept = 0)
ggplot(mod_diag1, aes(x = species, y = .stdresid)) + geom_boxplot() + ggtitle("Residuals plot of model predictors") + geom_hline(yintercept = 0)

# Point range plot for PC1
liocoris_lygus_df$species <- reorder(liocoris_lygus_df$species, liocoris_lygus_df$PC1, FUN=mean)

liocoris_lygus_pc1_plot <- ggplot(liocoris_lygus_df, aes(x = species, y = PC1, color = species)) +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_boot", size = 1, linewidth = 1) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) + 
  ggtitle('Point range plot for PC1') +
  scale_color_manual(values=palette_W) +
  theme(legend.position = 'none', axis.text.x = element_text(face = "italic"),
        plot.title = element_text(hjust = 0.5, face = 'bold'))

### Analysis of variance of PC2 ----
mod2 <- lm(PC2 ~ species, data = liocoris_lygus_df)
anova(mod2)

# Testing for conditions
mod_diag2 <- fortify(mod2)
res_p2 <- ggplot(data = mod_diag2, aes(x = .fitted, y = .stdresid)) + 
  geom_point(aes(size = .cooksd)) + 
  geom_hline(yintercept = 0) + 
  geom_smooth(method="loess", se=FALSE)
mean_val2 <- mean(mod_diag2$.stdresid)
sd_val2 <- sd(mod_diag2$.stdresid)
norm_p2 <- ggplot(mod_diag2, aes(sample = .stdresid)) + 
  geom_point(stat = "qq") + 
  geom_abline(intercept = mean_val2, slope = sd_val2)
plot_grid(res_p2, norm_p2, ncol = 2, rel_widths = c(0.55, 0.45))

leveneTest(PC2 ~ species, data = liocoris_lygus_df)
ggplot(mod_diag2, aes(x = .fitted, y = .stdresid)) + geom_point() + ggtitle("Residuals plot of model predictors") + geom_hline(yintercept = 0)
ggplot(mod_diag2, aes(x = species, y = .stdresid)) + geom_boxplot() + ggtitle("Residuals plot of model predictors") + geom_hline(yintercept = 0)

# Point range plot for PC2
liocoris_lygus_df$species <- reorder(liocoris_lygus_df$species, liocoris_lygus_df$PC2, FUN=mean)

liocoris_lygus_pc2_plot <- ggplot(liocoris_lygus_df, aes(x = species, y = PC2, color = species)) +
  stat_summary(geom = "pointrange", fun.data = "mean_cl_boot", size = 1, linewidth = 1) +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1)) + 
  ggtitle('Point range plot for PC2') +
  scale_color_manual(values=palette_W) +
  theme(legend.text = element_text(face = "italic", size = 12), 
        axis.text.x = element_text(face = "italic"), 
        plot.title = element_text(hjust = 0.5, face = 'bold'),
        legend.title = element_text(face = "bold", size = 14)) +
  labs(color='Species')

### Analysis of variance of PC3 ----
mod3 <- lm(PC3 ~ species, data = liocoris_lygus_df)
anova(mod3)

liocoris_lygus_both_plots_pc<- plot_grid(liocoris_lygus_pc1_plot, liocoris_lygus_pc2_plot, rel_widths = c(0.40, 0.58))
ggsave('liocoris_lygus_PC_plot.tiff', plot = liocoris_lygus_both_plots_pc, path = './Final_tables', scale = 1.5, width = 20, height = 10, device='tiff', dpi=300,  units = c("cm"))


## All pairs analyses ----
first_col_pairs <- plot_grid(liocoris_lygocoris_p_load, liocoris_lygocoris_p_scores, liocoris_lygocoris_pc1_plot, liocoris_lygocoris_pc2_plot, ncol = 4, rel_widths = c(0.92, 0.94, 0.84, 1.4))
second_col_pairs <- plot_grid(lygocoris_lygus_p_load, lygocoris_lygus_p_scores, lygocoris_lygus_pc1_plot, lygocoris_lygus_pc2_plot, ncol = 4, rel_widths = c(0.92, 0.94, 0.84, 1.4))
third_col_pairs <- plot_grid(liocoris_lygus_p_load, liocoris_lygus_p_scores, liocoris_lygus_pc1_plot, liocoris_lygus_pc2_plot, ncol = 4, rel_widths = c(0.92, 0.94, 0.84, 1.4))

first_title <- ggdraw() + 
  draw_label("Lygocoris pabulinus – Liocoris tripustulatus",
             fontface = 'bold.italic',
             x = 0,
             hjust = 0,
             size = 16) +
  theme(plot.margin = margin(0, 0, 0, 24))
second_title <- ggdraw() + 
  draw_label("Lygus punctatus – Lygocoris pabulinus",
             fontface = 'bold.italic',
             x = 0,
             hjust = 0,
             size = 16) +
  theme(plot.margin = margin(0, 0, 0, 24))
third_title <- ggdraw() + 
  draw_label("Lygus punctatus – Liocoris tripustulatus",
             fontface = 'bold.italic',
             x = 0,
             hjust = 0,
             size = 16) +
  theme(plot.margin = margin(0, 0, 0, 24))
all_title <- ggdraw() + 
  draw_label("Liocoris tripustulatus - Lygus punctatus – Lygocoris pabulinus",
             fontface = 'bold.italic',
             x = 0,
             hjust = 0,
             size = 16) +
  theme(plot.margin = margin(0, 0, 0, 24))

pairs_all <- plot_grid(all_title, all_three_all, first_title, first_col_pairs, second_title, second_col_pairs, third_title, third_col_pairs, ncol = 1, rel_heights = c(0.1, 1, 0.1, 1, 0.1, 1, 0.1, 1))

ggsave('All_PCA_plots.tiff', plot = pairs_all, path = './Final_tables', scale = 3.5, width = 119, height = 133, device='tiff', dpi=600,  units = c("mm"), bg = 'white')
