# CassHS_manuscript script for  figures associated with Maloney et al., 2024
# Written by: ME Maloney, Auburn University, 2024



#####overview#####
#custom color sets:
loc <- c("#66d7cc", "#8559A5") #distingusihes location: teal is Atlantic, purple is Bay
appCol <- c("#0E81F5", "#BE8C63") #this feels kinda self explanatory, blue is blue, brown is brown
acc <- c(	"#B2B2B2", "#C50000") #accliamtion treatment, grey is Ambient, red is elevated
pd <- position_dodge(0.4)

#Almost every figure will follow this formatting style:
ggplot(DataFrameName, aes(x= , y= , fill= )) + #general beginning of ggplot, using DaraFrameName as my dataframe, #fill determines the factor that will have a color in the plot
  geom_boxplot(position=position_jitterdodge(.5), width=0.25, outlier.shape = NA) + #boxplots with jitter and set width,  removing outliers
  geom_point(size=2,shape=21, position = position_jitterdodge(0.5)) + #makes datapoints large enough to see in word without modifying
  scale_fill_manual(values = loc) + #scale_XXX must match what is listed in the ggplot aes
  geom_pwc(label = "p.format", hide.ns = TRUE) + #used to generate pairwise comparisons and include a p-value if significant
  xlab("") +
  ylab("") +
  theme(legend.position="none",
        text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title=element_blank(),
        axis.line = element_line(colour = "black"), #creates the black line on the axis
        panel.grid.major = element_blank(), #removes grids
        panel.grid.minor = element_blank(), #removes grids
        panel.border = element_blank(),
        panel.background = element_blank())





#####Figure 1#####
###Figure1A###
#using df fieldSites #see DataWrangling.R for details
ggplot(fieldSites, aes(x=location, y=temp, fill=location)) +
  geom_boxplot(position=position_dodge(.), width=0.5, outlier.shape = NA) +
  geom_point(size=2,shape=21, position = position_jitterdodge(0.5)) +
  scale_fill_manual(values = loc) +
  geom_pwc(label = "p.format", hide.ns = TRUE) +
  xlab("") +
  ylab("Daily temperature \n (mean °C)") +
  theme(legend.position="none",
        text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave("A_MeanTemp.svg", units = c("in"), device = "svg", width = 3, height = 3) #"final" figure edited and saved as .ai


#Figure1B
#using df fieldSites #see DataWrangling.R for details
fieldSites$monthDay <- as.Date(fieldSites$monthDay, format = "%m-%d")
ggplot(fieldSites, aes(x=monthDay, y=dailyRange, group=location, color=location)) +
  geom_line(aes(color=location), linewidth=.5) +
  geom_smooth(method=lm, linetype = 2) +
  geom_point(aes(color=location),size=2)+
  scale_color_manual(values=loc) +
  #scale_x_discrete(limits=c("March", "April", "May", "June", "July", "August", "September")) +
  theme(legend.position="none",
        text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title=element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  ylab("Daily range \n (maximum - minimum) (°C)") +
  xlab("Month")
ggsave("B_RangeTemp.svg", units = c("in"), device = "svg", width = 4.5, height = 3)


#Figure 1C
#using df: hs2021Diff
ggplot(hs2021Diff, aes(x=change, y=pulse, fill=location)) +
  geom_boxplot(outlier.shape = NA, width=0.5) +
  lemon::geom_pointline(aes(group=interaction(animal,location), colour=location, fill=location), size=2,shape=21, position = pd)+
  geom_hline(yintercept = 0) +
  scale_colour_manual(values=loc) +
  scale_fill_manual(values=c(loc)) +
  theme(legend.position="none",
        text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title=element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  geom_pwc(label = "p.format", hide.ns = TRUE) +
  scale_y_continuous()+
  scale_x_discrete(limits=c("b27", "b30", "b32", "b34", "b36", "b38", "b40")) +
  xlab("Temperature (°C)") +
  ylab("Change in pulse rate from start")
ggsave("C1_diff21_location.svg", units = c("in"), device = "svg", width = 4.5, height = 3)


#Figure 1D
#using df: TODall
ggplot(subset(TODall, DateStress == '2021'), aes(y=tempDeath, x=Location, group=Location, fill=Location)) +
  geom_boxplot(width = .1, fill = loc, size = 1.5, outlier.shape = NA) +
  ggdist::stat_halfeye(adjust = .43, width = .77, color = NA, position = position_nudge(x = .10)) +
  gghalves::geom_half_point(side = "l", range_scale = .3, alpha = .5, size = 3) +
  coord_flip() +
  scale_x_discrete(expand = c(.07, .07), position="top") +
  scale_y_continuous(breaks = 37:43) +
  scale_fill_manual(values = loc, guide = "none") +
  xlab("") +   
  ylab("") +
  geom_pwc(label = "p.format", hide.ns = TRUE) +
  theme(legend.position="none",
        text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        axis.text.y=element_blank(),
        legend.title=element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave("D1_Tod21Loc.svg", units = c("in"), device = "svg", width = 3, height = 3)



####Figure 2####
###Figure2A###
#using dataframe hs2022Diff
ggplot(subset(hs2022Diff, pulse < 40), aes(x=change, y=pulse, fill=acclimation)) +
  geom_boxplot(outlier.shape = NA, width=0.5, position=position_dodge(1), ) +
  lemon::geom_pointline(aes(group=interaction(animal,acclimation), colour=acclimation, fill=acclimation), size=1,shape=21, position = pd)+
  geom_hline(yintercept = 0) +
  scale_colour_manual(values=acc) +
  scale_fill_manual(values=c(acc)) +
  theme(legend.position="none",
        text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title=element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  geom_pwc(label = "p.format", hide.ns = TRUE) +
  scale_y_continuous()+
  scale_x_discrete(limits=c("b27", "b30", "b32", "b34", "b36", "b38", "b40", "b41")) +
  xlab("Temperature (°C)") +
  ylab("")
ggsave("A2_diff22_acclimation.svg", units = c("in"), device = "svg", width = 4.75, height = 3.5)

#Figure 2B
#using df: TODall
ggplot(subset(TODall, DateStress == '2022'), aes(y=tempDeath, x=temp, group=temp, fill=temp)) +
  geom_boxplot(width = .1, fill = acc, size = 1.5, outlier.shape = NA) +
  ggdist::stat_halfeye(adjust = .43, width = .77, color = NA, position = position_nudge(x = .10)) +
  gghalves::geom_half_point(side = "l", range_scale = .3, alpha = .5, size = 3) +
  coord_flip() +
  scale_x_discrete(expand = c(.07, .07), position="top") +
  scale_y_continuous(breaks = 37:43) +
  scale_fill_manual(values = acc, guide = "none") +
  xlab("") +   
  ylab("") +
  geom_pwc(label = "p.format", hide.ns = TRUE) +
  theme(legend.position="none",
        text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.text.y=element_blank(),
        legend.title=element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave("B2_Tod22Acc.svg", units = c("in"), device = "svg", width = 3, height = 3)





####Figure 3####
##A
ggplot(hs2021Diff, aes(x=change, y=pulse, fill=color)) +
  geom_boxplot(outlier.shape = NA, width=0.5) +
  lemon::geom_pointline(aes(group=interaction(animal,color), colour=color, fill=color), size=2,shape=21, distance = unit(1.5, "pt"), position = pd)+
  geom_hline(yintercept = 0) +
  scale_colour_manual(values=appCol) +
  scale_fill_manual(values=c(appCol)) +
  theme(legend.position="none",
        text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title=element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  geom_pwc(label = "p.format", hide.ns = TRUE) +
  scale_y_continuous()+
  scale_x_discrete(limits=c("b27", "b30", "b32", "b34", "b36", "b38", "b40")) +
  xlab("Temperature (°C)") +
  ylab("")
ggsave("a3_diff21_color.svg", units = c("in"), device = "svg", width = 3.5, height = 2.5)

ggplot(subset(hs2022Diff, pulse < 40), aes(x=change, y=pulse, fill=color)) +
  geom_boxplot(outlier.shape = NA, width=0.5) +
  lemon::geom_pointpath(aes(group=interaction(animal, color), colour=color), size=1,  distance = unit(1.5, "pt"), position = pd)+
  geom_hline(yintercept = 0) +
  scale_colour_manual(values=appCol) +
  scale_fill_manual(values=c(appCol)) +
  theme(legend.position="none",
        text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title=element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  geom_pwc(label = "p.format", hide.ns = TRUE) +
  facet_wrap(~acclimation, ncol=1)+
  scale_y_continuous()+
  scale_x_discrete(limits=c("b27", "b30", "b32", "b34", "b36", "b38", "b40", "b41")) +
  xlab("Temperature (°C)") +
  ylab("")
ggsave("B3_diff22_color.svg", units = c("in"), device = "svg", width = 3.5, height = 5.5)



ggplot(subset(TODall, DateStress == '2021'), aes(y=tempDeath, x=color, group=color, fill=color)) +
  geom_boxplot(width = .1, fill = appCol, size = 1.5, outlier.shape = NA) +
  ggdist::stat_halfeye(adjust = .43, width = .77, color = NA, position = position_nudge(x = .10)) +
  gghalves::geom_half_point(side = "l", range_scale = .3, alpha = .5, size = 3) +
  coord_flip() +
  scale_x_discrete(expand = c(.07, .07), position="top") +
  scale_y_continuous(limits = c(38, 43)) +
  scale_fill_manual(values = appCol, guide = "none") +
  xlab("") +   
  ylab("") +
  geom_pwc(label = "p.format", hide.ns = TRUE) +
  theme(legend.position="none",
        text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.text.y=element_blank(),
        legend.title=element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave("3_Tod21Col2.svg", units = c("in"), device = "svg", width = 3, height = 3.5)

ggplot(subset(TODall, DateStress == '2022'), aes(y=tempDeath, x=color, group=color, fill=color)) +
  geom_boxplot(width = .1, fill = appCol, size = 1.5, outlier.shape = NA) +
  ggdist::stat_halfeye(adjust = .43, width = .77, color = NA, position = position_nudge(x = .10)) +
  gghalves::geom_half_point(side = "l", range_scale = .3, alpha = .5, size = 3) +
  coord_flip() +
  scale_x_discrete(expand = c(.07, .07), position="top") +
  scale_y_continuous(limits = c(38, 43)) +
  scale_fill_manual(values = appCol, guide = "none") +
  xlab("") +   
  ylab("") +
  geom_pwc(label = "p.format", hide.ns = TRUE) +
  theme(legend.position="none",
        text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        axis.text.y=element_blank(),
        legend.title=element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave("3_Tod22Col2.svg", units = c("in"), device = "svg", width = 3, height = 3)

ggplot(subset(TODall, DateStress == '2021'), aes(y=tempDeath, x=Location, group=Location, fill=Location)) +
  geom_boxplot(outlier.shape = NA, width=0.5)+
  geom_jitter(size=2,shape=21, width = 0.25)+
  ggtitle("") +
  scale_fill_manual(values=loc) +
  ylab("") +
  #xlab("Appendage Color") +
  scale_y_continuous(limits = c(38, 43)) +
  facet_wrap(~color, ncol=2)+
  geom_pwc(label = "p.format", hide.ns = TRUE)+
  theme(legend.position="none",
        text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.title=element_blank(),
        axis.text.x=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave("hs21_todAccLocation.svg", units = c("in"), device = "svg", width = 3, height = 3)

ggplot(subset(TODall, DateStress == '2022'), aes(y=tempDeath, x=temp, group=temp, fill=temp)) +
  geom_boxplot(outlier.shape = NA, width=0.5)+
  geom_jitter(size=2,shape=21, width = 0.25)+
  ggtitle("") +
  scale_fill_manual(values=acc) +
  ylab("Lethal Temperature (°C)") +
  #xlab("Appendage Color") +
  facet_wrap(~color, ncol=2)+
  geom_pwc(label = "p.format", hide.ns = TRUE)+
  scale_y_continuous(limits = c(38, 43)) +
  theme(legend.position="bottom",
        text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.title=element_blank(),
        axis.text.x=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave("hs22_todAcc.svg", units = c("in"), device = "svg", width = 3, height = 3)


####Figure 4####
ggplot(percentColor1, aes(y = tempDead, x = percentBlue)) + 
  geom_point() +
  stat_smooth(method = "lm", size = 1, se = FALSE,)+
  scale_colour_manual(values=c(acc)) +
  theme(legend.position="none",
        text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.title=element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  #facet_wrap(~treatment, ncol = 1)+
  #stat_cor(aes(label=..rr.label..), size = 8)+
  ylab("") +
  xlab("Blue appendage coverage (%)") 
ggsave("percentBlue.svg", units = c("in"), device = "svg", width = 2, height = 2)

ggplot(percentColor1, aes(y = tempDead, x = percentBlue, colour=treatment)) + 
  geom_point() +
  stat_smooth(method = "lm", size = 1, se = FALSE,)+
  scale_colour_manual(values=c(acc)) +
  theme(legend.position="none",
        text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title=element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  #facet_wrap(~treatment, ncol = 1)+
  #stat_cor(aes(label=..rr.label..), size = 8)+
  ylab("") +
  xlab("Blue appendage coverage (%)") 
ggsave("percentBlueAccTreatment.svg", units = c("in"), device = "svg", width = 2, height = 2)


####Supplemental####
##SuppFig1##
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-90.9,-72), ylim = c(23.1,32.5), expand = FALSE) +
  annotate(geom = "text", x = -75, y = 30, label = "Atlantic \n Ocean", fontface = "italic", color = "grey22", size = 6) +
  annotate("rect", xmin = -82.5, xmax = -80, ymin = 24.3, ymax = 25.4, alpha = .6)
ggsave("insetMap.svg", units = c("px"), device = "svg", width = 2850, height = 2600)  

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-83, -79.25), ylim = c(24, 27)) +
  annotate(geom = "text", x = -81, y = 26, label = " Mainland Florida", color = "grey22", size = 6) +
  xlab("Longitude") + 
  ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) +
  scalebar(x.min = -80.6, x.max = -80.7, y.min = 24.5, y.max = 24.6, dist = 10, dist_unit = 'km', transform = TRUE, model = 'WGS84', location = 'bottomleft', st.dist = 0.49, height = 0.18)

##SuppFig2##
#B
ggplot(ITS2, aes(fill=type, y=proportion, x=sample)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(values=c(color2)) +
  coord_flip() +
  theme(legend.position="bottom",
        text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 9),
        legend.title=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ggtitle("" ) + 
  #geom_pwc(label = "p.format") +
  #facet_wrap(~acclimation, ncol=1) +
  scale_y_continuous()+
  #scale_x_discrete(limits=c("Atlantic 1", "Atlantic 2", "Atlantic 3", "Atlantic 4", "Atlantic 5")) +
  xlab("Sample") +
  ylab("Proportion")
ggsave("ITS2_barplot.svg", units = c("in"), device = "svg", width = 4, height = 3)

##SuppFig3##
ggplot(hs2021Diff, aes(x=change, y=pulse)) +
  geom_boxplot(outlier.shape = NA, width=0.5) +
  geom_line(aes(group=animal), position = position_dodge(0.5), alpha=0.1) +
  geom_point(aes(group=animal),size=.5, position = position_dodge(0.5)) +
  geom_hline(yintercept = 0)+
  #scale_fill_manual(values=c(loc)) +
  theme(legend.position="none",
        text = element_text(size = 20),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ggtitle("" ) + 
  #geom_pwc(label = "p.format") +
  scale_y_continuous()+
  #scale_x_discrete(limits=c("28", "34", "35", "36", "37", "38", "39", "40")) +
  xlab("Temperature (°C)") +
  ylab("Pulses / minute")
ggsave("supplemental3.svg", units = c("in"), device = "svg", width = 6, height = 3)


##Supp4##
##A
###Acclimation Temp in tanks- SUP####
#using data file accTemp.xlsx from dataFiles
write_xlsx(accTemp1,"C:/Users/mem0294/Box/Buckley Lab Data/Maloney/MaloneyEtAl2024/Analysis/DataFramesFigures/accTemp1.xlsx")


accTemp1$day <- factor(accTemp1$day, levels=c("-6", "-5", "-4","-3", "-2", "-1", "0.5", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10","11", "12", "13", "14", "15", "16", "17", "18", "19", "20","21", "22", "23", "24", "25", "26", "27", "28", "29", "30"))
ggplot(accTemp1, aes(x=day, y=mean, color=tank, group=tank)) +
  geom_line(linewidth=2) +
  geom_pointrange(aes(ymin = mean-sd, ymax = mean+sd),data = accTemp1) +
  geom_point(size=3,shape=21, color="black")+
  scale_color_manual(values=accCol2) +
  scale_x_discrete(breaks = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10","11", "12", "13", "14", "15", "16", "17", "18", "19", "20","21", "22", "23", "24", "25", "26", "27", "28", "29", "30"))+
  annotate('rect', xmin=0.75, xmax=3, ymin=25, ymax=33, alpha=.4, fill="#FAF5C6", color="black")+
  annotate('rect', xmin=3.25, xmax=7.85, ymin=25, ymax=33, alpha=.3, fill="#A7D1F3", color="black")+
  geom_vline(xintercept=2.1, linetype=2)+
  geom_vline(xintercept=38.5, linetype=2)+
  xlab("Time (days)") +
  ylab("Daily temperature (mean (°C)") +
  theme(legend.position="none",
        text = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.title=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave("Acclimation Temp in tanks.svg", units = c("in"), device = "svg", width = 6, height = 3)
#B
ggplot(size2022, aes(x=lengthTime, y=length, fill=temp)) +
  geom_boxplot(outlier.shape = NA, width=0.5, position = position_dodge2(preserve = "single")) +
  lemon::geom_pointpath(aes(group=interaction(sampleName, temp), colour=temp), size=1,  distance = unit(1.5, "pt"), position = pd)+
  scale_fill_manual(values=acc) +
  scale_color_manual(values=acc) +
  #geom_pwc(label = "p.format") +
  theme(legend.position="none",
        text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title=element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_x_discrete(limits=c("Baseline", "Post Acclimation")) +
  ggtitle("" ) + 
  xlab("") +
  ylab("Bell Diameter (mm)") 
ggsave("size22Acc.svg", units = c("in"), device = "svg", width = 3, height = 3) 

#C
ggplot(symbiont2022, aes(x=timepoint, y=density, fill=acclimation)) +
  geom_boxplot(outlier.shape = NA, width=0.25, position = position_dodge2(preserve = "single")) +
  lemon::geom_pointpath(aes(group=interaction(sampleName, acclimation), colour=acclimation), size=1,  distance = unit(1.5, "pt"), position = pd)+
  scale_fill_manual(values=acc) +
  scale_color_manual(values=acc) +
  #geom_pwc(label = "p.format") +
  theme(legend.position="none",
        text = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.title=element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_x_discrete(limits=c("before", "after")) +
  ggtitle("" ) + 
  scale_y_continuous(label=scientific) +
  xlab("") +
  ylab("Density Change")
ggsave("symb22Acc.svg", units = c("in"), device = "svg", width = 3, height = 3) 


##suppFigure5###
ggplot(subset(hs2022Diff, pulse < 45), aes(x=change, y=pulse)) +
  geom_boxplot(outlier.shape = NA, width=0.5) +
  geom_line(aes(group=animal), position = position_dodge(0.5), alpha=0.1) +
  geom_point(aes(group=animal),size=1, position = position_dodge(0.5)) +
  geom_hline(yintercept = 0)+
  theme(legend.position="none",
        text = element_text(size = 20),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ggtitle("" ) + 
  #geom_pwc(label = "p.format") +
  scale_y_continuous()+
  #scale_x_discrete(limits=c("28", "34", "35", "36", "37", "38", "39", "40")) +
  xlab("Temperature (°C)") +
  ylab("")
ggsave("supplemental5.svg", units = c("in"), device = "svg", width = 6, height = 3)

##suppFig 6##
#A
ggplot(size2022, aes(x=lengthTime, y=length, fill=color)) +
  geom_boxplot(outlier.shape = NA, width=0.5, position = position_dodge2(preserve = "single")) +
  lemon::geom_pointpath(aes(group=interaction(sampleName, color), colour=color), size=1,  distance = unit(1.5, "pt"), position = pd)+
  scale_fill_manual(values=appCol) +
  scale_color_manual(values=appCol) +
  #geom_pwc(label = "p.format") +
  theme(legend.position="none",
        text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title=element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_x_discrete(limits=c("Baseline", "Post Acclimation")) +
  ggtitle("" ) + 
  xlab("") +
  ylab("Bell Diameter (mm)") 
ggsave("size22Color.svg", units = c("in"), device = "svg", width = 3, height = 3) 

#B
ggplot(symbiont2022, aes(x=timepoint, y=density, fill=color)) +
  geom_boxplot(outlier.shape = NA, width=0.25, position = position_dodge2(preserve = "single")) +
  lemon::geom_pointpath(aes(group=interaction(sampleName, color), colour=color), size=1,  distance = unit(1.5, "pt"), position = pd)+
  scale_fill_manual(values=appCol) +
  scale_color_manual(values=appCol) +
  #geom_pwc(label = "p.format") +
  theme(legend.position="none",
        text = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.title=element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  scale_x_discrete(limits=c("before", "after")) +
  ggtitle("" ) + 
  scale_y_continuous(label=scientific) +
  xlab("") +
  ylab("Density Change")
ggsave("symb22Color.svg", units = c("in"), device = "svg", width = 3, height = 3) 
