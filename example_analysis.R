library(tidyverse)
library(ggthemr)
library(ggpubr)
ggthemr_reset()
# Define the folder for outputting the figures
# figure_directory <- "../../Reports/mee_pycoalescence/figures"
figure_directory <- "figures"
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
            se_total=var(total)/length(total),
            min_q = quantile(total)[2],
            max_q = quantile(total)[4]) %>% 
  replace_na(list(sd_total=0))

# Labelling speciation rates
x_times_ten_notation <- function(x) {
  b <- floor(log10(as.numeric(x)))
  c <- as.numeric(x)/10^b
  paste(c, " %*% 10^", b,sep="")
}
nu_times_ten_notation <- function(x)
{
  return(paste("nu == ", x_times_ten_notation(x)))
}
speciation_rates_label <-  as_labeller(nu_times_ten_notation,
                                       default=label_parsed)

# Plot the species richness values
ggthemr("dust")
p1 <- species_richness %>% 
  mutate(speciation_rate = factor(speciation_rate, levels=c(0.00001, 0.000005, 0.000001))) %>% 
  mutate(time = time + 150) %>% 
  ggplot(aes(x=time, y=species_richness, 
             colour=as.factor(speciation_rate),
             fill=as.factor(speciation_rate),
             group=speciation_rate)) +
  theme_classic() + 
  geom_rect(aes(xmin=-Inf, xmax=50, ymin=0, ymax=Inf), colour=NA,fill="grey90", alpha=0.7)+
  xlab("Time before present (generations)") + ylab("Species richness") + 
  stat_summary(fun.y=mean, geom="line") + 
  stat_summary(fun.data=mean_se, geom="ribbon", alpha=0.3, colour=NA) +
  scale_colour_discrete("Speciation\nrate", 
                        labels=function(x) scales::parse_format()(x_times_ten_notation(x)))+
  scale_fill_discrete("Speciation\nrate", 
                      labels=function(x) scales::parse_format()(x_times_ten_notation(x)))+
  # geom_vline(aes(xintercept=-150), linetype="dotted", colour="black") +
  # geom_vline(aes(xintercept=-100), linetype="dotted", colour="black") +
  # geom_vline(aes(xintercept=-50), linetype="dotted", colour="black") +
  # geom_vline(aes(xintercept=0), linetype="dotted", colour="black") +
  annotate("text", label="Landscape A", x=25, y=275, colour="black")+
  annotate("text", label="Landscape B", x=75, y=275, colour="black")

pdf(file.path(figure_directory, "species_richness_time.pdf"), 6, 4)
print(p1)
dev.off()

# Plot the species abundance distributions
p2 <- species_abundances %>%
  mutate(speciation_rate = factor(speciation_rate, levels=c(0.00001, 0.000005, 0.000001))) %>%
  ungroup() %>% 
  mutate(time=time+150) %>% 
  ggplot(aes(x=2^abun_class, y=mean_total,
             colour=as.factor(speciation_rate),
             fill=as.factor(speciation_rate))) +
  theme_classic() + 
  geom_line()+
  geom_ribbon(aes(ymin=mean_total-se_total-0.01, ymax=mean_total+se_total+0.01), alpha=0.5, 
              colour=NA)+
  scale_x_continuous(expression(paste("Abundance class (", log[2], ")")), trans=scales::log2_trans(),
                     breaks=scales::trans_breaks("log2", function(x) 2^x, n=6),
                     labels = scales::trans_format("log2", scales::math_format(.x))) + 
  ylab("Mean number of species")+
  facet_grid(speciation_rate~time, 
             labeller = labeller(time = function(x) {return(paste("t =", x))},
                                 speciation_rate=speciation_rates_label))+
  scale_colour_discrete("Speciation\nrate", labels=function(x) scales::parse_format()(x_times_ten_notation(x)))+
  scale_fill_discrete("Speciation\nrate", labels=function(x) scales::parse_format()(x_times_ten_notation(x)))
# print(p2)
pdf(file.path(figure_directory, "species_abundances_time.pdf"), 6, 3)
print(p2)
dev.off()

gga1 <- ggarrange(NULL, p2, NULL, p1, ncol = 1, nrow=4, legend = "right", common.legend = TRUE,
                  labels = c("a) SAD", NA, "b) Species richness"), heights=c(0.1, 1, 0.1, 0.75),
                  hjust = 0)
pdf(file.path(figure_directory, "sad_richness.pdf"), 6, 6)
print(gga1)
dev.off()

