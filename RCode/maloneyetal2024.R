# CassHS_manuscript script for stats and figures associated with Maloney et al., 2024
# Written by: ME Maloney, Auburn University, 2024



#working directory set to "Box/MaloneyEtAl2023_CassHeatStressManuscript"
setwd("C:/Users/mem0294/Box/MaloneyEtAl2023_CassHeatStressManuscript/dataFiles")
# data files associated with this script include:
#C:/Users/mem0294/Box/MaloneyEtAl2023_CassHeatStressManuscript/dataFiles/fieldtempStats.xlsx #sheet 2 which contains the mean, min, max, range1 and sd for collection sites in 2021.




######Location######
#Using file fieldtempStats.xlsx Sheet 2

fig1Ameanttest <- t.test(mean~location, fieldtempStats)
sjPlot::tab_model(fig1Ameanttest)
report::report(fig1Ameanttest)
fig1Brangettest <- t.test(range1~location, fieldtempStats)
sjPlot::tab_model(fig1Brangettest)
report::report(fig1Brangettest)

ggplot(fieldtempStats, aes(x=location, y=mean, fill=location)) +
  geom_boxplot(position=position_dodge(.), width=0.25, outlier.shape = NA) +
  geom_point(size=.5,shape=21, position = position_jitterdodge(0.5)) +
  scale_fill_manual(values = loc_color2) +
  geom_pwc(label = "p.format", hide.ns = TRUE) +
  xlab("") +
  ylab("Daily temperature \n (mean °C)") +
  theme(legend.position="none",
        text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave("A_MeanTemp.svg", units = c("in"), device = "svg", width = 2.5, height = 2.9) #"final" figure edited and saved as .ai



fieldtempStats$monthDay <- as.Date(fieldtempStats$monthDay, format = "%m-%d")
ggplot(fieldtempStats, aes(x=monthDay, y=range1, group=location, color=location)) +
  geom_line(aes(color=location), linewidth=.5) +
  geom_smooth(method=lm, linetype = 2) +
  geom_point(aes(color=location),size=.5)+
  scale_color_manual(values=loc_color2) +
  theme(legend.position="none",
        text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title=element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  annotate(geom="text",x=as.Date("3-20", format="%m-%d"), y=14.0, label="***p < 2.2e-16", size=3.5) +
  ylab("Daily range \n (maximum - minimum) (°C)") +
  xlab("Month")
ggsave("B_RangeTemp.svg", units = c("in"), device = "svg", width = 4, height = 2.9)

###PULSE####
#using file hs2021_SummaryPulse.xlsx which contains the aggrigated results of hs 2021 pulsation (average pulses per minute for each animal at each temperature)
#Full Model

#generalized linear mixed effect model with temperature, location and appendage color as fixed effect and animal/date as random effect. using poisson bc pulsation is a count.
stat_hs21_glmer_full <- glmer(pulse ~ temperature  + location + color + (1|animal) + (1|date), family = poisson(link=log), data=hs2021_summaryPulse)
summary(stat_hs21_glmer_full)
sjPlot::tab_model(stat_hs21_glmer_full)

#Getting df and F stat from full model
anova(stat_hs21_glmer_full)
Anova(stat_hs21_glmer_full)

#assumptions of glmer:
#Linear? 
plot(resid(stat_hs21_glmer_full), )
#normal dis
hist(hs2021_summaryPulse$pulse) #pretty bell curve lookin to me (:
#HOV
hs2021_summaryPulse$Model.F.Res<- residuals(stat_hs21_glmer_full) #extracts the residuals and places them in a new column in our original data table
hs2021_summaryPulse$Abs.Model.F.Res <-abs(hs2021_summaryPulse$Model.F.Res) #creates a new column with the absolute value of the residuals
hs2021_summaryPulse$Model.F.Res2 <- hs2021_summaryPulse$Abs.Model.F.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.Model.F <- lm(Model.F.Res2 ~ animal, data=hs2021_summaryPulse) #ANOVA of the squared residuals
anova(Levene.Model.F) #p was 0.191, meaning good to go!

#significant letters for plot
anova_hs21temp <- aov(pulse~temperature, data=hs2021_summaryPulse)
tukey_hs21temp <- (anova_hs21temp)

cld_hs21temp <- multcompLetters4(anova_hs21temp, tukey_hs21temp)
print(cld_hs21temp)




#table for stat_hs21_glmer_full
stat_hs21_tukey <- glht(stat_hs21_glmer_full, mcp(temperature="Tukey"))
tukey21 <- tidy(stat_hs21_tukey)

sjPlot::tab_model(tukey21)
r.squaredGLMM(stat_hs21_tukey)
report::report(stat_hs21_glmer_full)


#hs21_lateTemps_loc
ggplot(hs2021_summaryPulse0, aes(x=temperature, y=pulse, fill=location)) +
  geom_boxplot(outlier.shape = NA, width=0.25) +
  geom_line(aes(group=animal), position = position_dodge(0.5), alpha=0.2) +
  geom_point(aes(fill=location, group=animal),size=1,shape=21, position = position_dodge(0.5)) +
  scale_fill_manual(values=c(loc_color2)) +
  theme(legend.position="top",
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
  scale_x_discrete(limits=c("28", "34", "35", "36", "37", "38", "39", "40")) +
  xlab("Temperature (°C)") +
  ylab("Pulses / minute")
ggsave("hs21_lateTemps_loc_no0s.svg", units = c("px"), device = "svg", width = 1850, height = 1000)


##hs21_lateTemps_no0
ggplot(hs2021_summaryPulse0, aes(x=temperature, y=pulse)) +
  geom_boxplot(outlier.shape = NA, width=0.25) +
  geom_line(aes(group=animal), position = position_dodge(0.5), alpha=0.1) +
  geom_point(aes(group=animal),size=1, position = position_dodge(0.5)) +
  #scale_fill_manual(values=c(loc_color2)) +
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
  scale_x_discrete(limits=c("28", "34", "35", "36", "37", "38", "39", "40")) +
  xlab("Temperature (°C)") +
  ylab("Pulses / minute")
ggsave("hs21_lateTemps_no0.svg", units = c("px"), device = "svg", width = 2850, height = 2600)



###hs21_lateTemps_color_no0
hs2021_summaryPulse0 <- subset (hs2021_summaryPulse, pulse !=0)
ggplot(subset(hs2021_summaryPulse0, temperature !="ba"), aes(x=temperature, y=pulse, fill=color)) +
  geom_boxplot(outlier.shape = NA, width=0.5) +
  geom_line(aes(group=animal), position = position_dodge(0.5), alpha=0.1) +
  geom_point(aes(fill=color, group=animal),size=1,shape=21, position = position_dodge(0.5)) +
  scale_fill_manual(values=c(BlBrCol2)) +
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
  geom_pwc(label = "p.format", hide.ns = TRUE) +
  scale_y_continuous()+
  scale_x_discrete(limits=c("28", "34", "35", "36", "37", "38", "39", "40")) +
  xlab("Temperature (°C)") +
  ylab("Pulses / minute")
ggsave("hs21_lateTemps_color.svg", units = c("in"), device = "svg", width = 3.5, height = 2.75)

#######diff from baseline ########
#difference from baseline to temperature######
# substracting the basline rate minus the rate at differnt temperatures
pulseDiff21 <- reshape(hs2021_summaryPulse0, idvar="animal", timevar="temperature", v.names="pulse", direction="wide", sep="_")
pulseDiff21$b27 <- (pulseDiff21$pulse_27 - pulseDiff21$pulse_27)
pulseDiff21$b28 <- (pulseDiff21$pulse_28 - pulseDiff21$pulse_27)
pulseDiff21$b29 <- (pulseDiff21$pulse_29 - pulseDiff21$pulse_27)
pulseDiff21$b30 <- (pulseDiff21$pulse_30 - pulseDiff21$pulse_27)
pulseDiff21$b31 <- (pulseDiff21$pulse_31 - pulseDiff21$pulse_27)
pulseDiff21$b32 <- (pulseDiff21$pulse_32 - pulseDiff21$pulse_27)
pulseDiff21$b33 <- (pulseDiff21$pulse_33 - pulseDiff21$pulse_27)
pulseDiff21$b34 <- (pulseDiff21$pulse_34 - pulseDiff21$pulse_27)
pulseDiff21$b35 <- (pulseDiff21$pulse_35 - pulseDiff21$pulse_27)
pulseDiff21$b36 <- (pulseDiff21$pulse_36 - pulseDiff21$pulse_27)
pulseDiff21$b37 <- (pulseDiff21$pulse_37 - pulseDiff21$pulse_27)
pulseDiff21$b38 <- (pulseDiff21$pulse_38 - pulseDiff21$pulse_27)
pulseDiff21$b39 <- (pulseDiff21$pulse_39 - pulseDiff21$pulse_27)
pulseDiff21$b40 <- (pulseDiff21$pulse_40 - pulseDiff21$pulse_27)
pulseDiff21$b41 <- (pulseDiff21$pulse_41 - pulseDiff21$pulse_27)
pulseDiff1_21 <- subset(pulseDiff21, select=c(1:8, 27:40))
hs2021_pulseDiff <- gather(pulseDiff1_21, change, pulse, `b27`:`b40`, factor_key = TRUE)
hs2021_pulseDiff$change <- as.character.factor(hs2021_pulseDiff$change)
hs2021_pulseDiff0 <- subset(hs2021_pulseDiff, !is.na(hs2021_pulseDiff$change))

#significant letters for plot
#generalized linear mixed effect model with temperature, location and appendage color as fixed effect and animal/date as random effect. using poisson bc pulsation is a count.
stat_hs2021GLMER_diff_Full <- lmer(pulse ~ change  + location + color + (1|animal) + (1|date), gaussian(link = "identity"), data=hs2021_pulseDiff0)
summary(stat_hs2021GLMER_diff_Full)




stat_hs2021LMER_diff_Full <- lmer(pulse ~ change  + location + color + (1|animal) + (1|date), REML = FALSE, data=hs2021_pulseDiff0)
summary(stat_hs2021LMER_diff_Full)
stat_hs21_tukeychange <- glht(stat_hs2021LMER_diff_Full, mcp(change="Tukey"))
tukey21_change <- tidy(stat_hs21_tukeychange)

stat_hs2021LMER_diff_color <- lmer(pulse ~  color + (1|animal) + (1|date), REML = FALSE, data=hs2021_pulseDiff0)
summary(stat_hs2021LMER_diff_color)
sjPlot::tab_model(stat_hs2021LMER_diff_color)
report::report(stat_hs2021LMER_diff_color)

#Getting df and F stat from full model
anova(stat_hs2021GLMER_diff_Full)
Anova(stat_hs2021GLMER_diff_Full)

sjPlot::tab_model(stat_hs2021GLMER_diff_Full)
r.squaredGLMM(stat_hs2021GLMER_diff_Full)
report::report(stat_hs2021GLMER_diff_Full)


#hs21_diffPulse_location
ggplot(hs2021_pulseDiff, aes(x=change, y=pulse, fill=location)) +
  geom_boxplot(outlier.shape = NA, width=0.5) +
  geom_line(aes(group=animal), position = position_dodge(0.5), alpha=0.2) +
  geom_point(aes(fill=location,group=animal),size=0.5,shape=21, position = position_dodge(0.5)) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=c(loc_color2)) +
  theme(legend.position="none",
        text = element_text(size = 10),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ggtitle("" ) + 
  geom_pwc(label = "p.format", hide.ns = TRUE) +
  scale_y_continuous()+
  scale_x_discrete(limits=c("b27", "b30", "b32", "b34", "b36", "b38", "b40")) +
  xlab("Temperature (°C)") +
  ylab("Change in pulse rate from start")
ggsave("hs21_diffPulse_location.svg", units = c("in"), device = "svg", width = 4, height = 2.9)

ggplot(hs2021_pulseDiff, aes(x=change, y=pulse, fill=color)) +
  geom_boxplot(outlier.shape = NA, width=0.5) +
  geom_line(aes(group=animal), position = position_dodge(0.5), alpha=0.2) +
  geom_point(aes(fill=color,group=animal),size=0.5,shape=21, position = position_dodge(0.5)) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=c(BlBrCol2)) +
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
  ggtitle("" ) + 
  geom_pwc(label = "p.format", hide.ns = TRUE) +
  scale_y_continuous()+
  scale_x_discrete(limits=c("b27", "b30", "b32", "b34", "b36", "b38", "b40")) +
  xlab("Temperature (°C)") +
  ylab("Change in pulse rate from start")
ggsave("hs21_colorDiff.svg", units = c("in"), device = "svg", width = 4, height = 3.25)


#hs21_diffPulse
ggplot(hs2021_pulseDiff, aes(x=change, y=pulse)) +
  geom_boxplot(outlier.shape = NA, width=0.25) +
  geom_line(aes(group=animal), position = position_dodge(0.5), alpha=0.2) +
  geom_point(aes(group=animal),size=0.25,shape=21, position = position_dodge(0.5)) +
  geom_hline(yintercept = 0) +
  #scale_fill_manual(values=c(BlBrCol2)) +
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
  #geom_pwc(label = "p.format", hide.ns = TRUE) +
  scale_y_continuous()+
  #scale_x_discrete(limits=c("32", "34", "36", "38", "40")) +
  xlab("Temperature (°C)") +
  ylab("Change in pulse rate from start")
ggsave("hs21_diffPulse.svg", units = c("in"), device = "svg", width = 7, height = 3)

lmLOC2022 <- lm(tempDeath ~ temp, data=tod22)
tod22lmerTable <- tbl_regression(lmLOC2022, include = everything())

lmLOC2022color <- lm(tempDeath ~ color, data=tod22)
tod22lmerTablecolor <- tbl_regression(lmLOC2022color, include = everything())

lmLOC2021color <- lm(tempDeath ~ color, data=tod21)
tod21lmerTablecolor <- tbl_regression(lmLOC2021color, include = everything())

lmLOC2021colorloc <- lmer(tempDeath ~ color*Location + (1|sampleName), REML = FALSE, data=tod21)
tod21lmerTablecolorloc <- tbl_regression(lmLOC2021colorloc, include = everything())

lmLOC2022coloracc <- lm(tempDeath ~ color, data=tod22)
tod22lmerTablecoloracc <- tbl_regression(lmLOC2022coloracc, include = everything())

##########Acclimation##########
####Pulse####
#hs2022_raw <- subset(hs2022raw, temperature != 42)
hs2022_summaryPulse <- ddply(hs2022_raw, c("sample", "experimentDate", "temperature", "acclimation", "color"), summarise,
                             mean = mean(pulse),
                             sd = sd(pulse))
hs2022_summaryPulse <- na.omit(hs2022_summaryPulse)
hs2022_summaryPulse <- subset(hs2022_summaryPulse, temperature != 'Baseline')
names(hs2022_summaryPulse)[names(hs2022_summaryPulse ) == 'sample'] <- 'animal'
names(hs2022_summaryPulse)[names(hs2022_summaryPulse ) == 'experimentDate'] <- 'date'
hs2022_summaryPulse$color[hs2022_summaryPulse$color == "e"] <- 'BLUE'
hs2022_summaryPulse$color[hs2022_summaryPulse$color == "b"] <- 'BROWN'

stat_hs2022_GLMER <- glmer(pulse ~ temperature  + acclimation + color + (1|animal) + (1|date), family = poisson(link=log), data=hs2022_summaryPulse)
summary(stat_hs2022_GLMER)


sjPlot::tab_model(stat_hs2022_GLMER)
r.squaredGLMM(stat_hs2022_GLMER)
report::report(stat_hs2022_GLMER)


names(hs2022_summaryPulse)[names(hs2022_summaryPulse ) == 'mean'] <- 'pulse'
hs2022_summaryPulse$acclimation <- as.factor(hs2022_summaryPulse$acclimation)
hs2022_summaryPulse$temperature <- as.factor(hs2022_summaryPulse$temperature)
hs2022_summaryPulse$pulse <- as.integer(hs2022_summaryPulse$pulse)
write_xlsx(hs2022_summaryPulse,"C:/Users/mem0294/Box/MaloneyEtAl2023_CassHeatStressManuscript/dataFiles/hs2022_SummaryPulse.xlsx")
hs2022_SummaryPulse2 <- read_excel("dataFiles/hs2022_SummaryPulse.xlsx", sheet = "Sheet2") ####removed any 0s from the data set that were before temperature 38

#######diff from baseline ########
#difference from baseline to temperature######
# substracting the basline rate minus the rate at differnt temperatures
pulseDiff22 <- reshape(hs2022_summaryPulse, idvar="animal", timevar="temperature", v.names="pulse", direction="wide", sep="_")
pulseDiffAmb <- subset(pulseDiff22, acclimation == "Ambient (26°C)")
pulseDiffAmb$b27 <- (pulseDiffAmb$pulse_27 - pulseDiffAmb$pulse_27)
pulseDiffAmb$b28 <- (pulseDiffAmb$pulse_28 - pulseDiffAmb$pulse_27)
pulseDiffAmb$b29 <- (pulseDiffAmb$pulse_29 - pulseDiffAmb$pulse_27)
pulseDiffAmb$b30 <- (pulseDiffAmb$pulse_30 - pulseDiffAmb$pulse_27)
pulseDiffAmb$b31 <- (pulseDiffAmb$pulse_31 - pulseDiffAmb$pulse_27)
pulseDiffAmb$b32 <- (pulseDiffAmb$pulse_32 - pulseDiffAmb$pulse_27)
pulseDiffAmb$b33 <- (pulseDiffAmb$pulse_33 - pulseDiffAmb$pulse_27)
pulseDiffAmb$b34 <- (pulseDiffAmb$pulse_34 - pulseDiffAmb$pulse_27)
pulseDiffAmb$b35 <- (pulseDiffAmb$pulse_35 - pulseDiffAmb$pulse_27)
pulseDiffAmb$b36 <- (pulseDiffAmb$pulse_36 - pulseDiffAmb$pulse_27)
pulseDiffAmb$b37 <- (pulseDiffAmb$pulse_37 - pulseDiffAmb$pulse_27)
pulseDiffAmb$b38 <- (pulseDiffAmb$pulse_38 - pulseDiffAmb$pulse_27)
pulseDiffAmb$b39 <- (pulseDiffAmb$pulse_39 - pulseDiffAmb$pulse_27)
pulseDiffAmb$b40 <- (pulseDiffAmb$pulse_40 - pulseDiffAmb$pulse_27)
pulseDiffAmb$b41 <- (pulseDiffAmb$pulse_41 - pulseDiffAmb$pulse_27)

pulseDiffEle <- subset(pulseDiff22, acclimation == "Elevated (33°C)")
pulseDiffEle$b32 <- (pulseDiffEle$pulse_32 - pulseDiffEle$pulse_32)
pulseDiffEle$b33 <- (pulseDiffEle$pulse_33 - pulseDiffEle$pulse_32)
pulseDiffEle$b34 <- (pulseDiffEle$pulse_34 - pulseDiffEle$pulse_32)
pulseDiffEle$b35 <- (pulseDiffEle$pulse_35 - pulseDiffEle$pulse_32)
pulseDiffEle$b36 <- (pulseDiffEle$pulse_36 - pulseDiffEle$pulse_32)
pulseDiffEle$b37 <- (pulseDiffEle$pulse_37 - pulseDiffEle$pulse_32)
pulseDiffEle$b38 <- (pulseDiffEle$pulse_38 - pulseDiffEle$pulse_32)
pulseDiffEle$b39 <- (pulseDiffEle$pulse_39 - pulseDiffEle$pulse_32)
pulseDiffEle$b40 <- (pulseDiffEle$pulse_40 - pulseDiffEle$pulse_32)
pulseDiffEle$b41 <- (pulseDiffEle$pulse_41 - pulseDiffEle$pulse_32)

pulseDiffAmb22 <- subset(pulseDiffAmb, select=c(1:4, 21:34))
pulseDiff22Amb <- gather(pulseDiffAmb22, change, pulse, `b27`:`b40`, factor_key = TRUE)
pulseDiff22Amb$change <- as.character.factor(pulseDiffAmb22$change)
pulseDiffEle22 <- subset(pulseDiffEle, select=c(1:4, 21:29))
pulseDiff22Ele <- gather(pulseDiffEle22, change, pulse, `b32`:`b40`, factor_key = TRUE)
pulseDiff22Ele$change <- as.character.factor(pulseDiffEle22$change)
pulseDiff22_all$color[pulseDiff22_all$color == "b"] <- 'BROWN'
pulseDiff22_all <- rbind(pulseDiff22Amb, pulseDiff22Ele)



pulseDiff22_all0 <- subset(pulseDiff22_all, !is.na(pulseDiff22_all$change))
#stat_hs2022LME_diff <- lmer(pulse ~ change  + acclimation + color + (1|animal) + (1|date), gaussian(link = "identity"),  data=pulseDiff22_all0)
stat_hs2022GLMER_diff_Full <- glmer(pulse ~ change  + acclimation + color + (1|animal) + (1|date), gaussian(link = "identity"), data=pulseDiff22_all0)
summary(stat_hs2022GLMER_diff_Full)


sjPlot::tab_model(stat_hs2022GLM_diff)
r.squaredGLMM(stat_hs2022LMER_diff)
report::report(stat_hs2022LMER_diff)

stat_hs2022LMER_diff_acclimation <- lmer(pulse ~ acclimation  + (1|animal) + (1|date), REML = FALSE, data=pulseDiff22_all0)
summary(stat_hs2022LMER_diff_acclimation)


sjPlot::tab_model(stat_hs2022LMER_diff_acclimation)
r.squaredGLMM(stat_hs2022LMER_diff_acclimation)
report::report(stat_hs2022LMER_diff_acclimation)


stat_hs2022LMER_diff_temp <- lmer(pulse ~ change  + (1|animal) + (1|date), REML = FALSE, data=pulseDiff22_all0)
summary(stat_hs2022LMER_diff_temp)
tukey22Temps <- glht(stat_hs2022LMER_diff_temp, mcp(change="Tukey"))
tukey_hs22temps <-  tidy(tukey22Temps)

sjPlot::tab_model(stat_hs2022LMER_diff_acclimation)
r.squaredGLMM(stat_hs2022LMER_diff_acclimation)
report::report(stat_hs2022LMER_diff_acclimation)
ggplot(pulseDiff22_all, aes(x=change, y=pulse)) +
  geom_boxplot(outlier.shape = NA, width=0.5) +
  geom_line(aes(group=animal), position = position_dodge(0.5), alpha=0.2) +
  geom_point(aes(group=animal),size=0.25,shape=21, position = position_dodge(0.5)) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=c(BlBrCol2)) +
  theme(legend.position="top",
        text = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.title=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ggtitle("" ) + 
  geom_pwc(label = "p.format", hide.ns = TRUE) +
  scale_y_continuous()+
  #scale_x_discrete(limits=c("b27", "b30", "b32", "b34", "b36", "b38", "b40")) +
  xlab("Temperature (°C)") +
  ylab("Change in pulse rate from start")
ggsave("hs22_diffPulseAll.svg", units = c("in"), device = "svg", width = 7, height = 5)

ggplot(pulseDiff22_all, aes(x=change, y=pulse, fill=acclimation)) +
  geom_boxplot(outlier.shape = NA, width=0.5) +
  geom_line(aes(group=animal), position = position_dodge(0.5), alpha=0.2) +
  geom_point(aes(fill=acclimation,group=animal),size=0.25,shape=21, position = position_dodge(0.5)) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=c(accCol2)) +
  theme(legend.position="top",
        text = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.title=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ggtitle("" ) + 
  geom_pwc(label = "p.format") +
  scale_y_continuous()+
  facet_wrap(~color, ncol = 1) +
  xlab("Temperature (°C)") +
  ylab("Change in pulse rate from start")
ggsave("hs21_diffPulse_accColor.svg", units = c("in"), device = "svg", width = 7, height = 5)

ggplot(pulseDiff22_all, aes(x=change, y=pulse, fill=color)) +
    geom_boxplot(outlier.shape = NA, width=0.5) +
    geom_line(aes(group=animal), position = position_dodge(0.5), alpha=0.2) +
    geom_point(aes(fill=color,group=animal),size=0.25,shape=21, position = position_dodge(0.5)) +
    geom_hline(yintercept = 0) +
    scale_fill_manual(values=c(BlBrCol2)) +
    theme(legend.position="top",
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
    geom_pwc(label = "p.format") +
    scale_y_continuous()+
    facet_wrap(~acclimation, ncol = 1) +
    xlab("Temperature (°C)") +
    ylab("Change in pulse rate from start")  
ggsave("hs21_diffPulse_ColorAcc.svg", units = c("in"), device = "svg", width = 7, height = 5)  

ggplot(pulseDiff22_all, aes(x=change, y=pulse, fill=acclimation)) +
  geom_boxplot(outlier.shape = NA, width=1) +
  geom_line(aes(group=animal), position = position_dodge(0.5), alpha=0.2) +
  geom_point(aes(fill=acclimation,group=animal),size=0.5,shape=21, position = position_dodge(0.5)) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=c(accCol2)) +
  theme(legend.position="top",
        text = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.title=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  ggtitle("" ) + 
  #geom_pwc(label = "p.format") +
  scale_y_continuous()+
  scale_x_discrete(limits=c("b27", "b30", "b32", "b34", "b36", "b38", "b40")) +
  #facet_wrap(~color, ncol = 1) +
  xlab("Temperature (°C)") +
  ylab("Change in pulse rate from start")
ggsave("hs21_diffPulse_acc.svg", units = c("in"), device = "svg", width = 4, height = 3.5)

par(mar = c(0,0,0,0))
ggplot(pulseDiff22_all, aes(x=change, y=pulse, fill=color)) +
  geom_boxplot(outlier.shape = NA) +
  geom_line(aes(group=animal), position = position_dodge(0.5), alpha=0.2) +
  geom_point(aes(fill=color,group=animal),size=0.5,shape=21, position = position_dodge(0.5)) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values=c(BlBrCol2)) +
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
  ggtitle("" ) + 
  #geom_pwc(label = "p.format") +
  scale_y_continuous()+
  facet_wrap(~acclimation, ncol = 1) +
  scale_x_discrete(limits=c("b27", "b30", "b32", "b34", "b36", "b38", "b40")) +
  xlab("Temperature (°C)") +
  ylab("Change in pulse rate from start")  
ggsave("hs22_diffPulse_Color.svg", units = c("in"), device = "svg", width = 4, height = 4) 


###2022_singlePlots
ggplot(subset(hs2022_SummaryPulse2, pulse !=0), aes(x=temperature, y=pulse)) +
  geom_boxplot(outlier.shape = NA, width=0.25) +
  geom_line(aes(group=animal), position = position_dodge(0.5), alpha=0.2) +
  geom_point(aes(group=animal),size=0.25,shape=21, position = position_dodge(0.5)) +
  #scale_fill_manual(values=c(accCol2)) +
  theme(legend.position="top",
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
  geom_pwc(label = "p.format", hide.ns = TRUE) +
  scale_y_continuous()+
  scale_x_discrete(limits=c("32", "33", "34", "35", "36", "37", "38", "39", "40", "41")) +
  xlab("Temperature (°C)") +
  ylab("Pulses / minute")



#figure with all temperatures during hs22 with points colored by acclimationTreatment
#hs22_alltemps_acclimation0s
hs22_alltemps_acc <- ggplot(subset(hs2022_summaryPulse, temperature !="Baseline"), aes(x=temperature, y=pulse, fill=acclimation)) +
  geom_boxplot(outlier.shape = NA, width=0.25) +
  geom_line(aes(group=animal), position = position_dodge(0.5), alpha=0.2) +
  geom_point(aes(fill=acclimation,group=animal),size=0.25,shape=21, position = position_dodge(0.5)) +
  scale_fill_manual(values=c(accCol2)) +
  theme(legend.position="top",
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
  geom_pwc(label = "p.format", hide.ns = TRUE) +
  scale_y_continuous()+
  scale_x_discrete(limits=c("32", "33", "34", "35", "36", "37", "38", "39", "40", "41")) +
  xlab("Temperature (°C)") +
  ylab("Pulses / minute")

##removed all 0s including those signifying "death"
#hs22_alltemps_acclimation
ggplot(subset(hs2022_SummaryPulse2, pulse != 0), aes(x=temperature, y=pulse, fill=acclimation)) +
  geom_boxplot(outlier.shape = NA, width=0.25) +
  geom_line(aes(group=animal), position = position_dodge(0.5), alpha=0.2) +
  geom_point(aes(fill=acclimation,group=animal),size=0.25,shape=21, position = position_dodge(0.5)) +
  scale_fill_manual(values=c(accCol2)) +
  theme(legend.position="top",
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
  geom_pwc(label = "p.format", hide.ns = TRUE) +
  scale_y_continuous()+
  scale_x_discrete(limits=c("32", "33", "34", "35", "36", "37", "38", "39", "40", "41")) +
  xlab("Temperature (°C)") +
  ylab("Pulses / minute")


######Color########
#hs22_allTemps_color_facetAcc
ggplot(subset(hs2022_SummaryPulse2, pulse !=0), aes(x=temperature, y=pulse, fill=color)) +
  geom_boxplot(outlier.shape = NA, width=0.25) +
  geom_line(aes(group=animal), position = position_dodge(0.5), alpha=0.1) +
  geom_point(aes(fill=color,group=animal),size=0.35,shape=21, position = position_dodge(0.5)) +
  scale_fill_manual(values=c(BlBrCol2)) +
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
  geom_pwc(label = "p.format", hide.ns = TRUE) +
  facet_wrap(~acclimation, ncol=1) +
  scale_y_continuous()+
  xlab("Temperature (°C)") +
  ylab("Pulses / minute")

#hs22_allTemps_color
ggplot(subset(hs2022_SummaryPulse2, pulse !=0), aes(x=temperature, y=pulse, fill=color)) +
  geom_boxplot(outlier.shape = NA, width=0.25) +
  geom_line(aes(group=animal), position = position_dodge(0.5), alpha=0.1) +
  geom_point(aes(fill=color,group=animal),size=0.35,shape=21, position = position_dodge(0.5)) +
  scale_fill_manual(values=c(BlBrCol2)) +
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
  geom_pwc(label = "p.format", hide.ns = TRUE) +
  #facet_wrap(~acclimation, ncol=1) +
  scale_y_continuous()+
  xlab("Temperature (°C)") +
  ylab("Pulses / minute")

#hs22_allTemps_color_sub
ggplot(subset(hs2022_SummaryPulse2, pulse !=0), aes(x=temperature, y=pulse, fill=color)) +
  geom_boxplot(outlier.shape = NA, width=0.25) +
  geom_line(aes(group=animal), position = position_dodge(0.5), alpha=0.1) +
  geom_point(aes(fill=color,group=animal),size=0.35,shape=21, position = position_dodge(0.5)) +
  scale_fill_manual(values=c(BlBrCol2)) +
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
  geom_pwc(label = "p.format", hide.ns = TRUE) +
  #facet_wrap(~acclimation, ncol=1) +
  scale_y_continuous()+
  scale_x_discrete(limits=c("32", "34", "36", "38", "40")) +
  xlab("Temperature (°C)") +
  ylab("Pulses / minute")

#hs22_allTemps_color_sub_facAcc
ggplot(subset(hs2022_SummaryPulse2, pulse !=0), aes(x=temperature, y=pulse, fill=color)) +
  geom_boxplot(outlier.shape = NA, width=0.25) +
  geom_line(aes(group=animal), position = position_dodge(0.5), alpha=0.1) +
  geom_point(aes(fill=color,group=animal),size=0.35,shape=21, position = position_dodge(0.5)) +
  scale_fill_manual(values=c(BlBrCol2)) +
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
  facet_wrap(~acclimation, ncol=1) +
  scale_y_continuous()+
  scale_x_discrete(limits=c("32", "34", "36", "38", "40")) +
  xlab("Temperature (°C)") +
  ylab("Pulses / minute")

ggplot(subset(todAll, DateStress == '2021'), aes(y=tempDeath, x=Location, group=Location, fill=Location)) +
  geom_boxplot(width = .1, fill = "white", size = 1.5, outlier.shape = NA) +
  ggdist::stat_halfeye(adjust = .43, width = .77, color = NA, position = position_nudge(x = .10)) +
  gghalves::geom_half_point(side = "l", range_scale = .3, alpha = .5, size = 3) +
  coord_flip() +
  scale_x_discrete(expand = c(.07, .07), position="top") +
  scale_y_continuous(breaks = 37:43) +
  scale_color_manual(values = loc_color2, guide = "none") +
  scale_fill_manual(values = loc_color2, guide = "none") +
  xlab("") +   
  ylab("Lethal Temperature (°C)") +
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
ggsave("hs21_todLocation.svg", units = c("in"), device = "svg", width = 2.5, height = 2.9)




ggplot(subset(todAll, DateStress == '2022'), aes(y=tempDeath, x=color, group=color, fill=color)) +
  geom_boxplot(width = .1, fill = "white", size = 1.5, outlier.shape = NA) +
  ggdist::stat_halfeye(adjust = .43, width = .77, color = NA, position = position_nudge(x = .10)) +
  gghalves::geom_half_point(side = "l", range_scale = .3, alpha = .5, size = 3) +
  coord_flip() +
  scale_x_discrete(expand = c(.07, .07), position="top") +
  scale_y_continuous(breaks = 37:43) +
  scale_color_manual(values = BlBrCol2, guide = "none") +
  scale_fill_manual(values = BlBrCol2, guide = "none") +
  xlab("") +   
  ylab("Lethal Temperature (°C)") +
  geom_pwc(label = "p.format", hide.ns = TRUE) +
  theme(legend.position="none",
        text = element_text(size = 20),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text.y=element_blank(),
        legend.title=element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave("hs22_todColor.svg", units = c("in"), device = "svg", width = 3, height = 2.75)

ggplot(subset(todAll, DateStress == '2022'), aes(y=tempDeath, x=temp, group=temp, fill=temp)) +
  geom_boxplot(outlier.shape = NA, width=0.5)+
  geom_point(size=0.35,shape=21, position = position_dodge(0.5))+
  ggtitle("") +
  scale_fill_manual(values=accCol2) +
  ylab("Lethal Temperature (°C)") +
  #xlab("Appendage Color") +
  facet_wrap(~color, ncol=2)+
  geom_pwc(label = "p.format")+
  theme(legend.position="none",
        text = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.title=element_blank(),
        axis.text.x=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave("hs22_todAccColor.svg", units = c("in"), device = "svg", width = 3, height = 2.75)

ggplot(subset(todAll, DateStress == '2021'), aes(y=tempDeath, x=Location, group=Location, fill=Location)) +
  geom_boxplot(outlier.shape = NA, width=0.5)+
  geom_point(size=0.35,shape=21, position = position_dodge(0.5))+
  ggtitle("") +
  scale_fill_manual(values=loc_color2) +
  ylab("Lethal Temperature (°C)") +
  #xlab("Appendage Color") +
  facet_wrap(~color, ncol=2)+
  geom_pwc(label = "p.format")+
  theme(legend.position="none",
        text = element_text(size = 12),
        axis.text = element_text(size = 9),
        legend.title=element_blank(),
        axis.text.x=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave("hs21_todAccLocation.svg", units = c("in"), device = "svg", width = 3, height = 2.75)


#####Percent Color######

ggplot(data = percentColor2022, aes(y = tempDead, x = percentBlue, colour = treatment)) + 
  geom_point(aes(colour = factor(treatment)), size = 3) +
  geom_smooth(method = "lm", se = FALSE, size = 2) +
  scale_colour_manual(values=c(accCol2)) +
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
  facet_wrap(~treatment, ncol = 1)+
  stat_cor(aes(label=..rr.label..),label.x=40, label.y=41, size = 6)+
  ylab("Lethal Temperature (°C)") +
  xlab("Blue appendage coverage (%)") 

colorDeath2 <- subset(colorDeath, percentBlue < 60)

percentBlueCorr <-  cor.test(percentColor2022$percentBlue, percentColor2022$tempDead, method = "pearson")
summary(percentBlueCorr)

sjPlot::tab_model(percentBlueCorr)


ggplot(percentColor2022, aes(y = tempDead, x = percentBlue, colour=treatment)) + 
  geom_point() +
  stat_smooth(method = "lm", size = 1, se = FALSE,)+
  scale_colour_manual(values=c(accCol2)) +
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
  stat_cor(aes(label=..rr.label..), size = 8)+
  ylab("") +
  xlab("Blue appendage coverage (%)") 
ggsave("percentBlueAccTreatment.svg", units = c("in"), device = "svg", width = 4, height = 4)

ggplot(subset(percentColor2022, percentBlue < 60), aes(y = tempDead, x = percentBlue)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  #scale_colour_manual(values=c(BlBrCol2)) +
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
  stat_cor(aes(label=..rr.label..), size = 8)+
  ylab("") +
  xlab("Blue appendage coverage (%)") 

test <- t.test(tempDead~color, data=percentColor2022)


ggplot(data = percentColor2022, aes(y = tempDead, x = percentBlue)) + 
  geom_point(aes(colour = factor(treatment))) +
  geom_smooth(aes(colour = factor(treatment)), method = "lm", se = FALSE) +
  scale_colour_manual(values=c(accCol2)) +
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
  #facet_wrap(~colorByEye, ncol = 1)+
  ylab("Lethal Temperature (°C)") +
  xlab("Percent of jelly with blue") 

ggplot(data = percentColor2022, aes(y = tempDead, x = percentBlue, colour = colorByEye)) + 
  geom_point(aes(colour = factor(colorByEye))) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_colour_manual(values=c(BlBrCol2)) +
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
  facet_wrap(~treatment, ncol = 1)
  ylab("Lethal Temperature (°C)") +
  xlab("Percent of jelly with blue") 


######Symbiont and Size stuff #######
  size2022 %>%
    group_by(lengthTime) %>%
    shapiro_test(length)
  # lengthTime      variable statistic     p
  # <chr>           <chr>        <dbl> <dbl>
  #   1 baseline        length       0.979 0.650
  # 2 postAcclimation length       0.971 0.391
  # 3 postStress      length       0.985 0.861
  # 4 preStress       length       0.986 0.901
  
  
  ##Sphericity :)
  size2022NA <- na.omit(size2022)
  size2022NA <- subset(size2022NA, size2022NA$sampleName != "4E_4")
  size2022$lengthTime <- gsub("baseline", "Baseline", size2022$lengthTime)
  size2022$lengthTime <- gsub("postAcclimation", "Post Acclimation", size2022$lengthTime)
  size2022$lengthTime <- gsub("postStress", "Post Stress", size2022$lengthTime)
  size2022$lengthTime <- gsub("preStress", "Pre-Stress", size2022$lengthTime)
  size22.aov <- anova_test(data = size2022NA, dv = length, wid = sampleName, within = lengthTime)
  get_anova_table(size22.aov)
  # ANOVA Table (type III tests)
  # 
  # Effect  DFn   DFd      F        p p<.05   ges
  # 1 lengthTime 2.36 87.26 74.404 2.59e-21     * 0.151
  
  sizePWC <- size2022NA %>%
    pairwise_t_test(
      length ~ lengthTime, paired = TRUE,
      p.adjust.method = "bonferroni"
    )
  sizePWC
  sizePWC
  # A tibble: 6 × 10
  # .y.    group1          group2             n1    n2 statistic    df        p    p.adj p.adj.signif
  # * <chr>  <chr>           <chr>           <int> <int>     <dbl> <dbl>    <dbl>    <dbl> <chr>       
  #   1 length baseline        postAcclimation    38    38      3.47    37 1   e- 3 8   e- 3 **          
  #   2 length baseline        postStress         38    38     11.1     37 2.69e-13 1.61e-12 ****        
  #   3 length baseline        preStress          38    38      9.01    37 7.26e-11 4.36e-10 ****        
  #   4 length postAcclimation postStress         38    38      9.86    37 6.73e-12 4.04e-11 ****        
  #   5 length postAcclimation preStress          38    38      6.98    37 3.01e- 8 1.81e- 7 ****        
  #   6 length postStress      preStress          38    38     -6.23    37 3.06e- 7 1.84e- 6 ****
  
  sizePWC2 <- size2022NA %>%
    pairwise_t_test(
      length ~ temp,
      p.adjust.method = "bonferroni"
    )
  sizePWC2
  
  
  stats_rma_size22 <- aov(length~color+temp+lengthTime+Error(sampleName), data = size2022NA)
  summary(stats_rma_size22)
  
  library(multcompView)
  
  library(lsmeans)
  
  stats_ph_size22 <- lsmeans(stats_rma_size22, ~ temp:lengthTime)
  
  
  
  size2022$lengthTime <- as.character(size2022NA$lengthTime)
  size2022$lengthTime <- factor(size2022$lengthTime, levels=c("Baseline", "Post Acclimation", "Pre-Stress", "Post Stress"))
  cld(stats_ph_size22,
      alpha   = 0.05,
      Letters = letters,     ### Use lower-case letters for .group
      adjust  = "tukey")
  
  
ggplot(size2022, aes(x=lengthTime, y=length, fill=temp)) +
    geom_boxplot(outlier.shape = NA, width=0.5, position = position_dodge2(preserve = "single")) +
    geom_line(aes(group=sampleName), position = position_dodge(0.5), alpha=0.1) +
    geom_point(aes(fill=temp, group=sampleName),size=2,shape=21, position = position_dodge(0.5)) +
    scale_fill_manual(values=accCol2) +
    scale_color_manual(values=accCol2) +
    #geom_pwc(label = "p.format") +
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
    scale_x_discrete(limits=c("Baseline", "Post Acclimation")) +
    ggtitle("" ) + 
    xlab("") +
    ylab("Bell Diameter (mm)") 
ggsave("size22_AcclimationTreatmentDuringAcclimation.svg", units = c("in"), device = "svg", width = 3, height = 3) 

ggplot(size2022, aes(x=lengthTime, y=length, fill=temp)) +
  geom_boxplot(outlier.shape = NA, width=0.5, position = position_dodge2(preserve = "single")) +
  geom_line(aes(group=sampleName), position = position_dodge(0.5), alpha=0.1) +
  geom_point(aes(fill=temp, group=sampleName),size=2,shape=21, position = position_dodge(0.5)) +
  scale_fill_manual(values=accCol2) +
  scale_color_manual(values=accCol2) +
  #geom_pwc(label = "p.format") +
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
  scale_x_discrete(limits=c("Baseline", "Post Acclimation")) +
  facet_wrap(~color, ncol=1)+
  ggtitle("" ) + 
  xlab("") +
  ylab("Bell Diameter (mm)") 
ggsave("size22_AcclimationTreatmentColor.svg", units = c("in"), device = "svg", width = 3, height = 3)


 
ggplot(size2022, aes(x=lengthTime, y=length, fill=color)) +
    geom_boxplot(outlier.shape = NA, width=0.5, position = position_dodge2(preserve = "single")) +
    geom_line(aes(group=sampleName), position = position_dodge(0.5), alpha=0.1) +
    geom_point(aes(fill=color, group=sampleName),size=2,shape=21, position = position_dodge(0.5)) +
    scale_fill_manual(values=BlBrCol2) +
    scale_color_manual(values=BlBrCol2) +
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
    ggtitle("" ) + 
    facet_wrap(~temp, ncol=1)+
    scale_x_discrete(limits=c("Baseline", "Post Acclimation")) +
    xlab("") +
    ylab("Bell Diameter (mm)") 
ggsave("size22_colorDuringAcclimation.svg", units = c("in"), device = "svg", width = 3, height = 3)

ggplot(size2022, aes(x=lengthTime, y=length, fill=color)) +
  geom_boxplot(outlier.shape = NA, width=0.5, position = position_dodge2(preserve = "single")) +
  geom_line(aes(group=sampleName), position = position_dodge(0.5), alpha=0.1) +
  geom_point(aes(fill=color, group=sampleName),size=2,shape=21, position = position_dodge(0.5)) +
  scale_fill_manual(values=BlBrCol2) +
  scale_color_manual(values=BlBrCol2) +
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
  ggtitle("" ) + 
  scale_x_discrete(limits=c("Baseline", "Post Acclimation")) +
  xlab("") +
  ylab("Bell Diameter (mm)") 
ggsave("size22_color.svg", units = c("in"), device = "svg", width = 3, height = 3)

symb22$acclimation <- substr(symb22$sample,2,2)
symb22$acclimation[symb22$acclimation == "A"] <- 'Ambient (26°C)'
symb22$acclimation[symb22$acclimation == "E"] <- 'Elevated (33°C)'

ggplot(symb22, aes(x=acclimation, y=afterMbefore, fill=acclimation)) +
  geom_boxplot(outlier.shape = NA, width=0.25, position = position_dodge2(preserve = "single")) +
  #geom_line() +
  geom_point(aes(fill=acclimation),size=2,shape=21, position = position_dodge(0.5)) +
  scale_fill_manual(values=accCol2) +
  scale_color_manual(values=accCol2) +
  geom_pwc(label = "p.format") +
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
  ggtitle("" ) + 
  xlab("") +
  ylab("Density Change (number of cells / g tissue)  (Post Acclimation - Baseline)")

ggplot(SymbiontDensity2022CALCULATED, aes(x=color, y=afterMbefore, fill=color)) +
  geom_boxplot(outlier.shape = NA, width=0.25, position = position_dodge2(preserve = "single")) +
  #geom_line() +
  geom_point(aes(fill=color),size=2,shape=21, position = position_dodge(0.5)) +
  scale_fill_manual(values=BlBrCol2) +
  scale_color_manual(values=BlBrCol2) +
  geom_pwc(label = "p.format") +
  theme(legend.position="top",
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
  xlab("") +
  ylab("Density Change (number of cells / g tissue)  (Post Acclimation - Baseline)")

##symb before v after
scientific <- function(x){
  ifelse(x==0, "0", parse(text=gsub("[+]", "", gsub("e", " %*% 10^", scientific_format()(x)))))
}

symb22a$acclimation <- substr(symb22a$sample,2,2)
symb22a$acclimation[symb22a$acclimation == "A"] <- 'Ambient (26°C)'
symb22a$acclimation[symb22a$acclimation == "E"] <- 'Elevated (33°C)'

symb22Time <- melt(symb22a, id = c("sample", "acclimation", "color", "sex", "tempDeath"), variable.name = "time", value.name = "density")
symb22Time$time[symb22Time$time == "beforeAvg"] <- 'Before Acclimation'
symb22Time$time[symb22Time$time == "afterAvg"] <- 'Post Accliamtion'

stats_rma_symb22Time <- aov(density~time+color+acclimation+Error(sample), data = symb22Time)
summary(stats_rma_symb22Time)
sjPlot::tab_model(stats_rma_symb22Time)

symb22TimeNA <- na.omit(symb22Time)
symb22TimeNA2 <- subset(symb22TimeNA, sample != "4A_7")

pwcSym <- symb22TimeNA2 %>%
  group_by(time) %>%
  pairwise_t_test(
    density ~ color, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwcSym
get_anova_table(res.aovsymb22Time)

stats_rma_symb22Time2 <- aov(density~time+color+acclimation, data = symb22Time)
summary(stats_rma_symb22Time2)
sjPlot::tab_model(stats_rma_symb22Time2)
report::report(stats_rma_symb22Time2)

ggplot(symb22Time, aes(x=time, y=density, fill=acclimation)) +
  geom_boxplot(outlier.shape = NA, width=0.5, position = position_dodge2(preserve = "single")) +
  geom_line(aes(group=sample), position = position_dodge(0.5), alpha=0.1) +
  geom_point(aes(fill=acclimation, group=sample),size=2,shape=21, position = position_dodge(0.5)) +
  scale_fill_manual(values=accCol2) +
  scale_color_manual(values=accCol2) +
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
  ggtitle("" ) + 
  scale_y_continuous(label=scientific) +
  xlab("") +
  ylab("")
ggsave("sym22Time_acclimtion.svg", units = c("in"), device = "svg", width = 3, height = 3)


ggplot(symb22Time, aes(x=time, y=density, fill=color)) +
  geom_boxplot(outlier.shape = NA, width=0.5, position = position_dodge2(preserve = "single")) +
  geom_line(aes(group=sample), position = position_dodge(0.5), alpha=0.1) +
  geom_point(aes(fill=color, group=sample),size=2,shape=21, position = position_dodge(0.5)) +
  scale_fill_manual(values=BlBrCol2) +
  scale_color_manual(values=BlBrCol2) +
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
  ggtitle("" ) + 
  scale_y_continuous(label=scientific) +
  xlab("") +
  ylab("")
ggsave("sym22Time_color.svg", units = c("in"), device = "svg", width = 3, height = 3)








####BURST#####

BurstR$temperature <- substr(BurstR$videoName,3,4)
BurstR$acclimation <- substr(BurstR$animal,2,2)
BurstR$acclimation[BurstR$acclimation == "n"] <- 'E'
BurstR$color[BurstR$color == "e"] <- 'BLUE'
BurstR$color[BurstR$color == "l"] <- 'BLUE'
BurstR$color[BurstR$color == "r"] <- 'BROWN'
BurstR$shock[BurstR$shock == "s"] <- 'Shocked'
BurstR$shock[BurstR$shock == "n"] <- 'Not Shocked'
BurstR$temperature[BurstR$temperature == "ba"] <- 'Baseline'
BurstR$sex[BurstR$sex == "F"] <- 'f'

BurstR$acclimation[BurstR$acclimation == "A"] <- 'Ambient (26°C)'
BurstR$acclimation[BurstR$acclimation == "E"] <- 'Elevated (33°C)'
names(BurstR)[names(BurstR) == 'count'] <- 'pulse'
BurstR$temperature <- as.factor(BurstR$temperature)
BurstR$color <- as.factor(BurstR$color)
BurstR$acclimation <- as.factor(BurstR$acclimation)

BurstR <- subset(BurstR, experimentDate != 07202022)
BurstR_1 <- subset(BurstR, greater1 != 0)
BurstR_summary <- ddply(BurstR_1, c("animal", "experimentDate", "temperature", "acclimation", "color"), summarise,
                        mean = mean(less1),
                        sd = sd(less1))
BurstR_2 <- subset (BurstR_1, less1 !=0)
BurstR_2 <- subset (BurstR_2, less1 <=40)
BurstR_2 <- subset (BurstR_2, less1 >=0)
BurstR_summary2 <- ddply(BurstR_2, c("animal", "experimentDate", "temperature", "acclimation", "color"), summarise,
                             mean = mean(less1),
                             sd = sd(less1))
ggplot(BurstR_2, aes(x = less1)) +
  geom_density(alpha = 0.9) + 
  #scale_fill_manual(values = accCol2) +
  theme(legend.position="top",
        text = element_text(size = 20),
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 16),
        legend.title=element_blank()) +
  ggtitle("" ) + 
  scale_x_continuous(limits = c(0, 44)) +
  #annotate("text", x =37, y=0.7, hjust= 0.5, vjust=0.65,label= "T-Test, p < 0.00001 ")+
  xlab("Temperature of Death (°C)") +
  ylab("Density") 

ggplot(BurstR, aes(x = less1, y = temperature)) + 
  geom_density_ridges() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = "off") + 
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)

ggplot(BurstR_summary2, aes(x=temperature, y=mean, fill=color)) +
  geom_boxplot(outlier.shape = NA, width=0.25) +
  #geom_line(aes(group=animal), alpha=0.2) +
  geom_point(size=2,shape=21, position = position_dodge(0.5)) +
  scale_fill_manual(values=c(BlBrCol2)) +
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
  #facet_wrap(~location, ncol = 1) +
  #scale_x_discrete(limits=c("32", "34", "36", "38", "40")) +
  xlab("Temperature (°C)") +
  ylab("") 


#######Site Map#######
#pull data from the internet
library(geodata)
us <- getData("GADM",country="USA",level=1)

#keep only Florida
states <- c('Florida')
us.states <- us[us$NAME_1 %in% states,]

#make the themes
fte_theme_map_small <- function(){
  color.background = 'grey90'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill= 'white',color = 'white')) +
    theme(plot.background = element_rect(fill = color.background, color = color.background)) +
    theme(panel.border = element_rect(colour = 'black')) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.ticks = element_blank()) +
    theme(plot.title = element_text(color = color.title, size = 2, vjust = 1.25)) +
    theme(axis.text.x = element_blank()) + 
    theme(axis.text.y = element_blank()) + 
    theme(axis.title.x = element_blank()) +
    theme(axis.title.y = element_blank()) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.line.x = element_line(color="black", size = 0.15),
          axis.line.y = element_line(color="black", size = 0.15)) 
}
fte_theme_map_sites <- function(){
  color.background = 'white'
  color.grid.major = 'black'
  color.axis.text = 'black'
  color.axis.title = 'black'
  color.title = 'black'
  theme_bw(base_size = 9) + 
    theme(panel.background = element_rect(fill = 'white', color = 'white')) +
    theme(plot.background = element_rect(fill=color.background,color = color.background)) +
    theme(panel.border = element_rect(colour = 'black')) +
    theme(panel.grid.major = element_blank()) + 
    theme(panel.grid.minor = element_blank()) + 
    theme(axis.ticks = element_line(color="black", size = 0.15)) +
    theme(plot.title = element_text(color = color.title, size = 12, vjust = 1.25)) +
    theme(axis.text.x = element_text(size = 12, color = color.axis.text, angle = 90)) + 
    theme(axis.text.y = element_text(size = 12, color = color.axis.text)) + 
    theme(axis.title.x = element_text(size = 12, color = color.axis.title, vjust = 0)) +
    theme(axis.title.y = element_text(size = 12, color = color.axis.title, vjust = 1.25)) +
    theme(plot.title = element_blank()) +
    theme(axis.line.x = element_line(color="black", size = 0.15),
          axis.line.y = element_line(color="black", size = 0.15)) +
    theme(legend.position = c(0.88, 0.28),
          legend.background = element_rect(colour = 'black'),
          legend.text = element_text(size = 10),
          legend.title = element_text(size = 10))
}

#make larger map to put study area in context
continent = ggplot()+
  geom_polygon(data = us,aes(x=long,y=lat,group=group), colour = 'grey40', #put in the acutal shape file
               size = 0.01, fill = 'grey90')+
  geom_polygon(data = us.states,aes(x=long,y=lat,group=group), size = 0.01, #put in FL shapefile
               fill = 'grey70')+
  coord_cartesian(xlim = c(-93.9,-75.85), ylim = c(24.1,32.5)) + #delimit where we are
  fte_theme_map_small() + #bring in the map
  annotate("rect", xmin = -81, xmax = -80, ymin = 24.8, ymax = 25.2, alpha = .9)+
  annotate('text', x = -91, y = 25.8, label = 'Gulf of \n Mexico', size = 4)+
  annotate('text', x = -78, y = 31.2, label = 'Atlantic \n Ocean', size = 4)+
  scalebar(x.min = -93, x.max = -85, y.min = 24.5, y.max = 25.5, dist = 250, dist_unit = 'km', st.size = 3, #add scalebar
           transform = TRUE, model = 'WGS84', location = 'bottomleft', st.dist = 0.42, height = 0.18) #transform = TRUE assumes coordinates are in decimal degrees


sites_map = ggplot()+
  geom_polygon(data = us.states,aes(x=long,y=lat,group=group), colour = 'grey40', size = 0.01, 
               fill = 'grey50')+
  coord_cartesian(xlim = c(-81.75, -80.25), ylim = c(24.4, 25.3)) +
  fte_theme_map_sites() +
  labs(x = 'Longitude', y = 'Latitude')+
  north(location = 'topright', scale = 0.9, symbol = 12, #add north arrow
        x.min = -80.5, x.max = -80.25, y.min = 24.4, y.max = 24.5)+
  scalebar(x.min = -80.6, x.max = -80.7, y.min = 24.5, y.max = 24.6, dist = 10, dist_unit = 'km', #add scalebar
           transform = TRUE, model = 'WGS84', location = 'bottomleft', st.dist = 0.49, height = 0.18) #transform = TRUE assumes coordinates are in decimal degrees

#make the one plot inset with the other
insetmap = ggdraw()+
  draw_plot(sites_map) + 
  draw_plot(continent, x=0.102, y=0.62, width=0.38, height=0.35) 
insetmap
ggsave('study_map.png', plot = insetmap,
       width = 4, height = 3.5,
       dpi = 300)
ggsave("SUPP_Map2.svg", units = c("in"), device = "svg", width = 2, height = 2) 






###Map again
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




######ITS2#####
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



###Acclimation Temp in tanks- SUP####
#using data file accTemp.xlsx from dataFiles
accTemps <- read_excel("dataFiles/accTemp1.xlsx")
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
