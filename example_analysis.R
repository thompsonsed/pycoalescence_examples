library(tidyverse)
library(ggthemr)
ggthemr_reset()
# Define the folder for outputting the figures
output_figure <- "../../Figures/MEE_paper/"

# Read in the data (the csvs should be in the same directory)
species_richness <- read.csv("richness.csv") %>% select(-X) %>% mutate(time = - time)
species_abundances <- read.csv("species_abundances.csv") %>% select(-X) %>% filter(abundance>0) %>%
  mutate(abun_class = floor(log2(abundance)),
         time=-time) %>% 
  group_by(time, abun_class, seed, speciation_rate) %>% 
  summarise(total = n()) %>% group_by(time, abun_class, speciation_rate) %>% 
  summarise(mean_total = mean(total))

# Plot the species richness values
ggthemr("dust")
p <- species_richness %>% ggplot(aes(x=time, y=species_richness, 
                                     colour=as.factor(speciation_rate), group=speciation_rate)) +
  # geom_point(alpha=0.4)+
  theme_classic() + xlab("Time before present (generations)") + ylab("Species richness") + 
  stat_summary(fun.y=mean, geom="line") + 
  scale_colour_discrete("Speciation rate", labels=c("0.0001", "0.0005", "0.001"))+
  theme(legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.box.margin=margin(c(30,10,10,10)),
        legend.background = element_rect(color = "black", size = 0.5,
                                         linetype = "solid")) + 
  geom_vline(aes(xintercept=-100), linetype="dotted",
             colour="black") +
  annotate("text", label="Real\nlandscape", x=-125, y=1800, colour="black")+
  annotate("text", label="Random\nlandscape", x=-75, y=1800, colour="black")

pdf(file.path(output_figure, "species_richness_time.pdf"), 7, 4)
print(p)
dev.off()

# Add in the 0 values for large size classes at time 0
species_abundances <- species_abundances %>% ungroup %>% 
  add_row(time=0, abun_class=7, mean_total=0.1, speciation_rate=c(0.0001, 0.0005, 0.001)) %>%
  add_row(time=0, abun_class=8, mean_total=0.1, speciation_rate=c(0.0001, 0.0005, 0.001)) %>%
  add_row(time=-50, abun_class=7, mean_total=0.1, speciation_rate=c(0.0001, 0.0005, 0.001)) %>%
  add_row(time=-50, abun_class=8, mean_total=0.1, speciation_rate=c(0.0001, 0.0005, 0.001))

# Plot the species abundance distributions
p <- species_abundances %>% ggplot(aes(x=2^abun_class, y=mean_total, fill=as.factor(speciation_rate), 
                                       group=speciation_rate)) +
  theme_classic() + 
  geom_bar(stat="identity", position=position_dodge()) +
  # geom_line(aes(colour=as.factor(time))) +
  scale_x_continuous("Abundance class", trans="log2", breaks=2^seq(0, 8, 2)) + 
  ylab("Mean number of species")+
  facet_grid(.~time, labeller = labeller(time = function(x) {return(paste("t =", x))}))+
  scale_fill_discrete("Speciation rate", labels=c("0.0001", "0.0005", "0.001"))+
  theme(legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.box.margin=margin(c(30,10,10,10)),
        legend.background = element_rect(color = "black", size = 0.5,
                                         linetype = "solid"))

pdf(file.path(output_figure, "species_abundances_time.pdf"), 7, 4)
print(p)
dev.off()
