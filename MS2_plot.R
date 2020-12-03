library(readxl)
library(tidyverse)
# use the file name of experimental data
exp_he <- read_excel("MAOA_experimental.xlsx", sheet = 1) %>% mutate(mass=round(mass, 6))
# use the file name of theoretical data
the_he <- read_excel("MAOA_theoretical.xlsx", sheet = 1) %>% mutate(mass=round(mass, 4))
# intensity max
max_he <- max(exp_he$Intensity)
# convert intensity to percentage
exp_he1 <- exp_he %>% mutate(intensity_percentage=(Intensity/max_he)*100) %>% select(mass, intensity_percentage)

exp_he_ms <- exp_he %>% select(mass) %>% data.matrix(.)
# read theoretical heavy data
the_he_ms <- the_he %>% select(mass) %>% data.matrix(.)
# get the difference for each two values of experimental and theoretical heavy data
he_mass_dif <- sapply(exp_he_ms, "-", the_he_ms)
# data frame
he_mass_dif1 <- data.frame(he_mass_dif)
# set row names and column names
rownames(he_mass_dif1) <- the_he$mass
colnames(he_mass_dif1) <- exp_he$mass
# change rownames to a column
he_mass_dif2 <- tibble::rownames_to_column(he_mass_dif1, "mass_tho") 
# if you need to use different cutoff, please change the number inside "filter(abs(DA) < 0.3)"
he_dif_fi <- he_mass_dif2 %>% gather(mass_exp, DA, 2:ncol(.)) %>% filter(abs(DA) < 0.3) %>% mutate_at(.,(1:3),as.double)
# get the matched data
he_dif_fi1 <- inner_join(the_he, he_dif_fi, by=c("mass" = "mass_tho"))
he_exp_match <- left_join(exp_he1, he_dif_fi1, by=c("mass"="mass_exp")) 
he_match_export <- left_join(the_he, he_dif_fi1, by=c("mass")) 
# if you need to export the match result, please delete "#" before write.csv
#write.csv(he_match_export, "heavy_match.csv", row.names = F)

# read experimental light data
exp_li <- read_excel("MAOA_experimental.xlsx", sheet = 2) %>% mutate(mass=round(mass, 6))
# read theoretical light data
the_li <- read_excel("MAOA_theoretical.xlsx", sheet = 2) %>% mutate(mass=round(mass, 4))
max_li <- max(exp_li$Intensity)

# did the same thing for light experiments
exp_li1 <- exp_li %>% mutate(intensity_percentage=(Intensity/max_li)*100) %>% select(mass, intensity_percentage)

exp_li_ms <- exp_li %>% select(mass) %>% data.matrix(.)
the_li_ms <- the_li %>% select(mass) %>% data.matrix(.)
#fa <- function(x,y) {(x-y)/y*10^6}
li_mass_dif <- sapply(exp_li_ms, "-", the_li_ms)
li_mass_dif1 <- data.frame(li_mass_dif)
rownames(li_mass_dif1) <- the_li$mass
colnames(li_mass_dif1) <- exp_li$mass

li_mass_dif2 <- tibble::rownames_to_column(li_mass_dif1, "mass_tho") 
li_dif_fi <- li_mass_dif2 %>% gather(mass_exp, DA, 2:ncol(.)) %>% filter(abs(DA) < 0.3) %>% mutate_at(.,(1:3),as.double)
li_dif_fi1 <-inner_join(the_li, li_dif_fi, by=c("mass" = "mass_tho"))

li_exp_match <- left_join(exp_li1,li_dif_fi1, by=c("mass"="mass_exp")) 
li_match_export <- left_join(the_li, li_dif_fi1, by=c("mass")) 
# if you need to export the match result, please delete "#" before write.csv
#write.csv(li_match_export, "light_match.csv", row.names = F)

# add color to the files
fb <- function(x) ifelse(x == "0" , "black", "red")
# match theoretical mass from heavy and light experiments
match_he_hi <- inner_join(he_exp_match, li_exp_match, by="mass.y", na_matches =  "never") %>% select(mass.x, mass.y, mass.y.y)
he_exp_match0 <- inner_join(he_exp_match, match_he_hi[,-3], by= c("mass" = "mass.x", "mass.y" = "mass.y")) %>% mutate(color = "blue") 
he_exp_match1 <- anti_join(he_exp_match, match_he_hi, by= c("mass" = "mass.x", "mass.y" = "mass.y")) %>% mutate(DA = replace_na(DA, 0)) %>% mutate(color=fb(DA)) 
he_exp_match_fi <- rbind(he_exp_match0, he_exp_match1)

li_exp_match0 <- inner_join(li_exp_match, match_he_hi, by= c("mass" = "mass.y.y", "mass.y" = "mass.y")) %>% mutate(color = "blue") %>% select(-c(mass.x))
li_exp_match1 <- anti_join(li_exp_match, match_he_hi, by= c("mass" = "mass.y.y", "mass.y" = "mass.y")) %>% mutate(DA = replace_na(DA, 0)) %>% mutate(color=fb(DA)) 
li_exp_match_fi <- rbind(li_exp_match0, li_exp_match1)

library(ggplot2)
library(ggrepel)
ggplot(he_exp_match_fi %>% filter(color == "black"))+
  geom_bar(aes(x=mass, y=intensity_percentage), stat = "identity", color = "#000000", width = 0.01)+
  geom_bar(data = he_exp_match_fi %>% filter(color == "red"), aes(x=mass, y=intensity_percentage), stat = "identity", color = "blue", width = 0.01)+
  geom_bar(data = he_exp_match_fi %>% filter(color == "blue"), aes(x=mass, y=intensity_percentage), stat = "identity", color = "#008000", width = 0.01)+
  geom_label_repel(data = he_exp_match_fi %>% filter(color != "black"), aes(x=mass, y=intensity_percentage, label = ion), na.rm = T, segment.color = "#C55A11", color = "#C55A11", segment.size = 0.1)+
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(200, max(he_exp_match_fi$mass), 100)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0, 100, 10))+
  coord_cartesian(xlim=c(0, max(he_exp_match_fi$mass)), ylim = c(0, 100))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.line = element_line(size = 0.1, colour = "black"))+
  xlab(NULL)+
  ylab(NULL)
ggsave("plot_heavy_v1.pdf") # change the name of saved pdf if needed

ggplot(li_exp_match_fi %>% filter(color == "black"))+
  geom_bar(aes(x=mass, y=intensity_percentage), stat = "identity", color = "#000000", width = 0.001)+
  geom_bar(data = li_exp_match_fi %>% filter(color == "red"), aes(x=mass, y=intensity_percentage), stat = "identity", color = "red", width = 0.001)+
  geom_bar(data = li_exp_match_fi %>% filter(color == "blue"), aes(x=mass, y=intensity_percentage), stat = "identity", color = "#008000", width = 0.001)+
  geom_label_repel(data = li_exp_match_fi %>% filter(color != "black"), aes(x=mass, y=intensity_percentage, label = ion), na.rm = T, segment.color = "#C55A11", segment.size = 0.1, color = "#C55A11")+
  scale_x_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(200, max(li_exp_match_fi$mass), 100))+ 
  scale_y_continuous(expand = expansion(mult = c(0, 0)), breaks = seq(0, 100, 10))+
  coord_cartesian(xlim=c(0, max(he_exp_match_fi$mass)), ylim = c(0, 100))+
  theme_classic()+
  theme(legend.position = "none")+
  theme(axis.line = element_line(size = 0.1, colour = "black"))+
  xlab(NULL)+
  ylab(NULL)
ggsave("plot_light_v2.pdf") # change the name of saved pdf if needed

