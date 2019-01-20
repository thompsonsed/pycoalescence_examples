library(tidyverse)
library(ggthemr)
ggthemr_reset()
# Define the folder for outputting the figures
figure_directory <- "../../Reports/pycoalescence/figures"
# figure_directory <- "figures"
results_directory <- "results"
if(!dir.exists(figure_directory)) 
{
  dir.create(figure_directory)
}
# Read in the data (the csvs should be in the same directory)
species_richness <- read.csv(file.path(results_directory, "richness.csv")) %>% select(-X) %>% 
  mutate(time = - time)
species_abundances <- read.csv(file.path(results_directory, "species_abundances.csv")) %>%
  select(-X) %>% filter(abundance>0) %>%
  mutate(abun_class = floor(log2(abundance)),
         time=-time) %>% 
  group_by(time, abun_class, seed, speciation_rate) %>% 
  summarise(total = n()) %>% group_by(time, abun_class, speciation_rate) %>% 
  summarise(mean_total = mean(total),
            sd_total=sd(total),
            min_q = quantile(total)[2],
            max_q = quantile(total)[4]) %>% 
  replace_na(list(sd_total=0))

# Plot the species richness values
ggthemr("dust")
p <- species_richness %>% ggplot(aes(x=time, y=species_richness, 
                                     colour=as.factor(speciation_rate),
                                     fill=as.factor(speciation_rate),
                                     group=speciation_rate)) +
  theme_classic() + xlab("Time before present (generations)") + ylab("Species richness") + 
  stat_summary(fun.y=mean, geom="line") + 
  stat_summary(fun.data=mean_se, geom="ribbon", alpha=0.3) +
  scale_colour_discrete("Speciation\nrate", labels=c("0.0001", "0.0005", "0.001"))+
  scale_fill_discrete("Speciation\nrate", labels=c("0.0001", "0.0005", "0.001"))+
  theme(legend.background = element_rect(color = "black", size = 0.5,
                                         linetype = "solid")) + 
  geom_vline(aes(xintercept=-100), linetype="dotted", colour="black") +
  annotate("text", label="Real\nlandscape", x=-125, y=75, colour="black")+
  annotate("text", label="Random\nlandscape", x=-75, y=75, colour="black")

pdf(file.path(figure_directory, "species_richness_time.pdf"), 4.5, 3)
print(p)
dev.off()

# Add in the 0 values for large size classes at time 0
# species_abundances <- species_abundances %>% ungroup %>% 
  # add_row(time=0, abun_class=7, mean_total=0.1, speciation_rate=c(0.000001, 0.000005, 0.00001)) %>%
  # add_row(time=0, abun_class=8, mean_total=0.1, speciation_rate=c(0.000001, 0.000005, 0.00001)) %>%
  # add_row(time=-50, abun_class=7, mean_total=0.1, speciation_rate=c(0.000001, 0.000005, 0.00001)) %>%
  # add_row(time=-50, abun_class=8, mean_total=0.1, speciation_rate=c(0.000001, 0.000005, 0.00001))

# Plot the species abundance distributions
p <- species_abundances %>% filter(speciation_rate == 0.00001) %>% 
  ggplot(aes(x=2^abun_class, y=mean_total))+
             # colour=as.factor(speciation_rate), 
             # fill=as.factor(speciation_rate),
             # group=speciation_rate)) +
  theme_classic() + 
  geom_line()+
  geom_ribbon(aes(ymin=mean_total-sd_total-0.01, ymax=mean_total+sd_total+0.01), colour=NA, alpha=0.5)+
  # geom_bar(stat="identity", position=position_dodge()) +
  # geom_line(aes(colour=as.factor(time))) +
  scale_x_continuous("Abundance class", trans="log2", breaks=2^seq(0, 8, 2)) + 
  ylab("Mean number of species")+
  facet_grid(.~time, labeller = labeller(time = function(x) {return(paste("t =", x))}))+
  # scale_colour_discrete("Speciation\nrate", labels=c("0.000001", "0.000005", "0.00001"))+
  # scale_fill_discrete("Speciation\nrate", labels=c("0.000001", "0.000005", "0.00001"))+
  theme(legend.justification = c(1, 1), legend.position = c(1, 1),
        legend.box.margin=margin(c(1,0.5,10,10)),
        legend.background = element_rect(color = "black", size = 0.5,
                                         linetype = "solid"))

pdf(file.path(figure_directory, "species_abundances_time.pdf"), 4, 3)
print(p)
dev.off()

