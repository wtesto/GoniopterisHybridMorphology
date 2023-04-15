library(dplyr)
library(magrittr)
library(ggplot2)
library(ggforce)

morphData <- read.csv("FieldData.csv") %>% 
  arrange(species) %>% 
  mutate(species=factor(species, levels=c("mollis", "hybrid", "nicaraguensis")))


## number of pinna pairs 
ggplot(morphData, aes(species, pinna_pairs, fill = species)) +
  geom_violin() +
  geom_sina() +
  stat_summary(fun = "mean",
               geom = "crossbar",
               width = 0.1,
               color = "black")+
  scale_fill_manual(values=c("#208cc0", "#f1af3a", "#cf5e4e"))+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())+
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "#ffffff"), 
        panel.border = element_rect(fill = NA))+
  guides(fill="none")+
  guides(shape="none")

ggsave(filename = "pinnaPairs.PDF", path = ".", width = 20, height = 10, 
       units = "cm", device = 'pdf', dpi=300, useDingbats=FALSE)

dev.off()

## pinna length
ggplot(morphData, aes(species, pinna_length, fill = species)) +
  geom_violin() +
  geom_sina() +
  stat_summary(fun = "mean",
               geom = "crossbar",
               width = 0.1,
               color = "black")+
  scale_fill_manual(values=c("#208cc0", "#f1af3a", "#cf5e4e"))+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())+
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "#ffffff"), 
        panel.border = element_rect(fill = NA))+
  guides(fill="none")+
  guides(shape="none")

ggsave(filename = "pinnaLength.PDF", path = ".", width = 20, height = 10, 
       units = "cm", device = 'pdf', dpi=300, useDingbats=FALSE)

dev.off()


## pinna width
ggplot(morphData, aes(species, pinna_width, fill = species)) +
  geom_violin() +
  geom_sina() +
  stat_summary(fun = "mean",
               geom = "crossbar",
               width = 0.1,
               color = "black")+
  scale_fill_manual(values=c("#208cc0", "#f1af3a", "#cf5e4e"))+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())+
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "#ffffff"), 
        panel.border = element_rect(fill = NA))+
  guides(fill="none")+
  guides(shape="none")

ggsave(filename = "pinnaWidth.PDF", path = ".", width = 20, height = 10, 
       units = "cm", device = 'pdf', dpi=300, useDingbats=FALSE)

dev.off()



## dissection
ggplot(morphData, aes(species, dissection, fill = species)) +
  geom_violin() +
  geom_sina() +
  stat_summary(fun = "mean",
               geom = "crossbar",
               width = 0.1,
               color = "black")+
  scale_fill_manual(values=c("#208cc0", "#f1af3a", "#cf5e4e"))+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())+
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "#ffffff"), 
        panel.border = element_rect(fill = NA))+
  guides(fill="none")+
  guides(shape="none")

ggsave(filename = "dissection.PDF", path = ".", width = 20, height = 10, 
       units = "cm", device = 'pdf', dpi=300, useDingbats=FALSE)

dev.off()


## vein pairs
ggplot(morphData, aes(species, vein_pairs, fill = species)) +
  geom_violin() +
  geom_sina() +
  stat_summary(fun = "mean",
               geom = "crossbar",
               width = 0.1,
               color = "black")+
  scale_fill_manual(values=c("#208cc0", "#f1af3a", "#cf5e4e"))+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())+
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "#ffffff"), 
        panel.border = element_rect(fill = NA))+
  guides(fill="none")+
  guides(shape="none")

ggsave(filename = "veinPairs.PDF", path = ".", width = 20, height = 10, 
       units = "cm", device = 'pdf', dpi=300, useDingbats=FALSE)

dev.off()



## pinna lw ratio
ggplot(morphData, aes(species, lw_ratio, fill = species)) +
  geom_violin() +
  geom_sina() +
  stat_summary(fun = "mean",
               geom = "crossbar",
               width = 0.1,
               color = "black")+
  scale_fill_manual(values=c("#208cc0", "#f1af3a", "#cf5e4e"))+
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())+
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "#ffffff"), 
        panel.border = element_rect(fill = NA))+
  guides(fill="none")+
  guides(shape="none")

ggsave(filename = "lw_ratio.PDF", path = ".", width = 20, height = 10, 
       units = "cm", device = 'pdf', dpi=300, useDingbats=FALSE)

dev.off()



##pinna pairs ANOVA
pinnaPairsANOVA <- aov(pinna_pairs ~ species, data = morphData)

summary(pinnaPairsANOVA)

pinnaPairsTukey<-TukeyHSD(pinnaPairsANOVA)

pinnaPairsTukey



##pinna length ANOVA
pinnaLengthANOVA <- aov(pinna_length ~ species, data = morphData)

summary(pinnaLengthANOVA)

pinnaLengthTukey<-TukeyHSD(pinnaLengthANOVA)

pinnaLengthTukey


##pinna width ANOVA
pinnaWidthANOVA <- aov(pinna_width ~ species, data = morphData)

summary(pinnaWidthANOVA)

pinnaWidthTukey<-TukeyHSD(pinnaWidthANOVA)

pinnaWidthTukey



##dissection ANOVA
dissectionANOVA <- aov(dissection ~ species, data = morphData)

summary(dissectionANOVA)

dissectionTukey<-TukeyHSD(dissectionANOVA)

dissectionTukey


##vein pairs ANOVA
veinsANOVA <- aov(vein_pairs ~ species, data = morphData)

summary(veinsANOVA)

vein_pairsTukey<-TukeyHSD(veinsANOVA)

vein_pairsTukey


##pinnae lw ratio ANOVA
pinnaLWANOVA <- aov(lw_ratio ~ species, data = morphData)

summary(pinnaLWANOVA)

pinnaLWTukey<-TukeyHSD(pinnaLWANOVA)

pinnaLWTukey
