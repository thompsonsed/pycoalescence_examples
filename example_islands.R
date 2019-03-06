library(tidyverse)
library(viridis)
library(ggpubr)
# figure_directory <- "../../Reports/mee_pycoalescence/figures"
figure_directory <- "figures"
results_directory <- "results"
if(!dir.exists(figure_directory)) 
{
  dir.create(figure_directory)
}

# Generate some coordinates for our islands
island_coordinates <- data.frame(fragment=c("A", "B", "C", "D"),
                                 x=c(0.5, 1, 0.5, 1), y=c(0.5, 0.45, 1.1, 1))
# Read the island results csv (the simulation output)
island_biodiversity <- read.csv(file.path("results", "island_biodiversity.csv")) %>% select(-X) %>% 
  full_join(island_coordinates)

# Read the dispersal probabilities
dispersal_probabilities <- read.csv(file.path("dispersal_probabilities.csv")) %>% 
  rename(source=X) %>% 
  gather(key="destination", value="probability", A, B, C, D) %>% 
  full_join(island_coordinates, by=c(source="fragment")) %>% 
  rename(x_source = x, y_source = y) %>% 
  full_join(island_coordinates, by=c(destination="fragment")) %>% 
  rename(x_destination = x, y_destination =y) %>% 
  filter(!(x_source == x_destination & y_source == y_destination)) %>% 
  mutate(angle = acos((x_destination-x_source)/((x_destination-x_source)^2 + 
                                                  (y_destination-y_source)^2)^0.5),
         modifier_src = ifelse(source == "A", 0.3, ifelse(source %in% c("B", "D"), 0.25, 0.14)),
         modifier_dst = ifelse(destination == "A", 0.33, 
                               ifelse(destination %in% c("B", "D"), 0.27, 0.14)),
         x_destination_new = x_destination - modifier_dst*(x_destination-x_source),
         y_destination_new = y_destination - modifier_dst*(y_destination-y_source),
         x_source = x_source - modifier_src*(x_source-x_destination),
         y_source = y_source - modifier_src*(y_source-y_destination),
         angle_mod = mod(angle, pi),
         x_mod = ifelse(y_source == y_destination, 0, 
                        ifelse(x_source > x_destination, 0.01,
                               ifelse(x_source==x_destination & 
                                        y_source > y_destination, 0.02, -0.02))),
         y_mod = ifelse(x_source == x_destination, 0, 
                        ifelse(y_source < y_destination, 0.02,-0.02)),
         # x_mod = ifelse(x_source > x_destination, 0.02,
         #                ifelse(x_source == x_destination,ifelse(y_source > y_destination, -0.02, 0),
         #                       0)),
         # y_mod = ifelse(y_source < y_destination, 0.02, 
         #                ifelse(y_source == y_destination, 
         #                       ifelse(x_source > x_destination, -0.02, 0), 0)),
         x_destination_new = x_destination_new - x_mod,
         y_destination_new = y_destination_new - y_mod,
         x_source = x_source - x_mod,
         y_source = y_source - y_mod) %>% 
  select(-x_destination, -y_destination) %>% 
  rename(x_destination = x_destination_new, y_destination = y_destination_new) 

# Plot the species richness of each island
p_richness <- island_biodiversity %>% filter(speciation_rate==1e-3, fragment != "whole") %>% 
  ggplot()+
  geom_segment(data=dispersal_probabilities, 
               aes(x=x_source, y=y_source, xend=x_destination, yend=y_destination, 
                   alpha=probability),arrow=arrow(length=unit(0.3, "cm"), angle=20), lineend="butt")+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.key = element_blank(),
        aspect.ratio=1.0) +
  scale_colour_viridis("", option="inferno", begin=0.4, end=0.9)+
  scale_alpha_continuous(guide=FALSE)+
  scale_size_area(max_size=30, guide=FALSE)+
  geom_point(aes(x=x, y=y, colour=species_richness, size=number_individuals)) + 
  geom_text(aes(x=x, y=y, label=fragment), colour="black", size=7, fontface=2)+
  scale_y_reverse(limits=c(1.2, 0.3))+
  scale_x_continuous(limits=c(0.3, 1.1))

# Plot the endemics for each island
p_endemics <- island_biodiversity %>% filter(speciation_rate==1e-3, fragment != "whole") %>% 
  ggplot()+
  geom_segment(data=dispersal_probabilities, 
               aes(x=x_source, y=y_source, xend=x_destination, yend=y_destination, 
                   alpha=probability),arrow=arrow(length=unit(0.3, "cm"), angle=20), lineend="butt")+
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.key = element_blank(),
        aspect.ratio=1.0) +
  scale_colour_viridis("", option="viridis", begin=0.4, end=0.9)+
  scale_alpha_continuous(guide=FALSE)+
  scale_size_area(max_size=30, guide=FALSE)+
  geom_point(aes(x=x, y=y, colour=endemics, size=number_individuals)) + 
  geom_text(aes(x=x, y=y, label=fragment), colour="black", size=7, fontface=2)+
  scale_y_reverse(limits=c(1.2, 0.3))+
  scale_x_continuous(limits=c(0.3, 1.1))

gga1 <- ggarrange(p_richness, p_endemics, nrow=2, ncol=1, align = "v", 
                  labels=c("a) Species richness", "b) Endemic richness"))

pdf(file.path(figure_directory, "islands_diversity.pdf"), 3.8, 6)
print(gga1)
dev.off()
