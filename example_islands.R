library(tidyverse)
library(viridis)
library(ggpubr)
figure_directory <- "../../Reports/pycoalescence/figures"
# figure_directory <- "figures"
results_directory <- "results"
if(!dir.exists(figure_directory)) 
{
  dir.create(figure_directory)
}
# Read the island results csv
island_biodiversity <- read.csv(file.path("results", "island_biodiversity.csv")) %>% select(-X) %>%
  mutate(fragmentA = factor(fragmentA, levels = rev(levels(fragmentA))),
         number_dissimilar = ifelse(number_dissimilar == 0, 1, number_dissimilar))
  # gather(key = "scenario", value = "species_richness", number_similar, number_dissimilar)

p1 <- island_biodiversity %>% ggplot() + 
  geom_tile(aes(x=fragmentB, y=fragmentA, fill=number_dissimilar)) + 
  # facet_grid(.~scenario)+
  scale_x_discrete("", position="top")+
  scale_y_discrete("")+
  scale_fill_viridis("Number of \nspecies", option="magma", trans="log2") + 
  theme_classic() + theme(aspect.ratio=1)+
  ggtitle("Dissimilar species")
p2 <- island_biodiversity %>% ggplot() + 
  geom_tile(aes(x=fragmentB, y=fragmentA, fill=number_similar)) + 
  # facet_grid(.~scenario)+
  scale_x_discrete("", position="top")+
  scale_y_discrete("")+
  scale_fill_viridis("Number of \nspecies", option="magma", trans="log2") + 
  theme_classic() + theme(aspect.ratio=1)+
  ggtitle("Similar species")
out <- ggarrange(p2, p1, ncol=2, nrow=1, common.legend = TRUE, legend="right")
pdf(file.path(figure_directory, "islands_example.pdf"), 5, 2.5)
print(out)
dev.off()
