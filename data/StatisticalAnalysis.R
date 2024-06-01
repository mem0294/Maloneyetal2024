#stats
#load data frames
#loggerSummary
#hs2021Summary
#hs2021Diff

####temperature loggers####
#only need CoCo Plum and Fitt sites for the purposes of this paper #using previously wrangeld dataframe fieldTempStats
fieldSites <- fieldtempStats[(fieldtempStats$location == "CoCo Plum") | (fieldtempStats$location == "FITT"), ]
fieldSites$location <- gsub("CoCo Plum", "Atlantic", fieldSites$location)
fieldSites$location <- gsub("FITT", "Bay", fieldSites$location)

#t-test measuring the difference between Atlantic and Bay overall mean temperatures between march and september 2022
field_meanttest <- t.test(temp~location, fieldSites)
summary(field_meanttest)
#t = -0.88289, df = 354.25, p-value = 0.3779
#95 percent confidence interval: -0.7446040  0.2832004
#mean in group Atlantic      mean in group Bay 
#29.91424               30.14495 

sjPlot::tab_model(field_meanttest)
report::report(field_meanttest)

field_rangettest <- t.test(dailyRange~location, fieldSites)
#t = 21.541, df = 206.6, p-value < 2.2e-16
#95 percent confidence interval: 2.404611 2.889121
#mean in group Atlantic      mean in group Bay 
#4.373380               1.726514 
sjPlot::tab_model(field_rangettest)
report::report(field_rangettest)

####HS 2021######
######ITS2####

#see ITS2_dada2_phyloseq.R for pipeline associated with this section.
#uses https://benjjneb.github.io/dada2/ITS_workflow.html

######pulse2021#####

#generalized linear mixed effect model with temperature, location and appendage color as fixed effect and animal/date as random effect. using poisson bc pulsation is a count.
hs21_pulseGLMER <- glmer(pulse ~ temperature  + location + color + (1|animal) + (1|date), family = poisson(link=log), data=hs2021Summary)
summary(hs21_pulseGLMER)
sjPlot::tab_model(hs21_pulseGLMER)

#Getting df and F stat from full model
anova(hs21_pulseGLMER)
# Analysis of Variance Table
# npar Sum Sq Mean Sq F value
# temperature   14 572.43  40.888 40.8880
# location       1   3.87   3.867  3.8671
# color          1   0.03   0.029  0.0293
Anova(hs21_pulseGLMER)
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: pulse
# Chisq Df Pr(>Chisq)    
# temperature 573.0211 14    < 2e-16 ***
#   location      3.5702  1    0.05882 .  
# color         0.0291  1    0.86461    


#assumptions of glmer:
#Linear? 
plot(resid(hs21_pulseGLMER), )
#normal dis
hist(hs2021Summary$pulse) #pretty bell curve lookin to me (:
#HOV
hs2021Summary$Model.F.Res<- residuals(hs21_pulseGLMER) #extracts the residuals and places them in a new column in our original data table
hs2021Summary$Abs.Model.F.Res <-abs(hs2021Summary$Model.F.Res) #creates a new column with the absolute value of the residuals
hs2021Summary$Model.F.Res2 <- hs2021Summary$Abs.Model.F.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
hs21pulse_Levene.Model.F <- lm(Model.F.Res2 ~ animal, data=hs2021Summary) #ANOVA of the squared residuals
anova(hs21pulse_Levene.Model.F) #p was 0.05, which is pretty close to not-significant. Callin it good to go (;

#post-hoc analysis determining which temperatures are significantly different
hs21_pulseTUKEY <- glht(hs21_pulseGLMER, mcp(temperature="Tukey"))

######changeInPulse2021#####
#linear mixed effect model with temperature (in this case its the difference between baseline and the temperature), location and appendage color as fixed effect and animal/date as random effect. No longer using "pulse" as a count, but as "change"
#origionally attempted to run a glmer with  normal distribution, but R directed me to just run the lmer. 
hs21_diffLMER <- lmer(pulse ~ change + location + color + (1|animal) + (1|date), REML = FALSE, data=hs2021Diff)
summary(hs21_diffLMER)
sjPlot::tab_model(hs21_diffLMER)


#assumptions of LMER:
#Linear? 
plot(resid(hs21_diffLMER), )
#HOV
hs2021Diff$Model.F.Res <- residuals(hs21_diffLMER) #extracts the residuals and places them in a new column in our original data table
hs2021Diff$Abs.Model.F.Res <-abs(hs2021Diff$Model.F.Res) #creates a new column with the absolute value of the residuals
hs2021Diff$Model.F.Res2 <- hs2021Diff$Abs.Model.F.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
hs21diff_Levene.Model.F <- lm(Model.F.Res2 ~ animal, data=hs2021Diff) #ANOVA of the squared residuals
anova(hs21diff_Levene.Model.F) #displays the results
#p = 7.862e-06 which is significant. 
plot(hs21_diffLMER) #plot seems okay though so could just be because the values are centered around 0
#normality
hist(hs2021Diff$pulse) #still bell looking

#linear mixed effect model assessing if there is an interaction between temperature and location. Temperature (in this case its the difference between baseline and the temperature), location and appendage color as fixed effect and animal/date as random effect. 
#origionally attempted to run a glmer with  normal disrtribution, but R directed me to just run the lmer. 
hs21_diffLMERint <- lmer(pulse ~ change*location + color + (1|animal) + (1|date), REML = FALSE, data=hs2021Diff)
summary(hs21_diffLMERint) #####fixed effect of location was not significant, therefore will not be using this statistical analysis.


######temperature at death 2021#####
#subset TODall to only contain samples from 2021
TOD21 <- subset(TODall, DateStress == 2021)
hs21_todLMER <- lmer(tempDeath ~ color+Location + (1|block), REML = FALSE, data=TOD21)
summary(hs21_todLMER)
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)  39.9899     0.1207 50.0000 331.241   <2e-16 ***
#   colorBrown   -0.4941     0.1571 50.0000  -3.145   0.0028 ** 
#   LocationBay  -0.1497     0.1571 50.0000  -0.952   0.3454   
sjPlot::tab_model(hs21_todLMER)


####HS 2022######
######pulse2022#####

#turn the mean pulse into an integer in order to use for stats
hs2022Summary$pulse <- as.integer(hs2022Summary$pulse)

#generalized linear mixed effect model with temperature, accliamtion treatment and appendage color as fixed effect and animal/date as random effect. using poisson bc pulsation is a count.
hs22_pulseGLMER <- glmer(pulse ~ temperature  + acclimation + color + (1|animal) + (1|date), family = poisson(link=log), data=hs2022Summary)
summary(hs22_pulseGLMER)
sjPlot::tab_model(hs22_pulseGLMER)

#Getting df and F stat from full model
anova(hs22_pulseGLMER)
# Analysis of Variance Table
# npar  Sum Sq Mean Sq F value
# temperature   14 154.905 11.0647 11.0647
# acclimation    1   0.084  0.0839  0.0839
# color          1   0.253  0.2528  0.2528
Anova(hs22_pulseGLMER)
# Analysis of Variance Table
# npar  Sum Sq Mean Sq F value
# temperature   14 154.905 11.0647 11.0647
# acclimation    1   0.084  0.0839  0.0839
# color          1   0.253  0.2528  0.2528  


#assumptions of glmer:
#Linear? 
plot(resid(hs22_pulseGLMER), )
#normal dis
hist(hs2022Summary$pulse) #pretty bell curve lookin to me (:
#HOV
hs2022Summary$Model.F.Res<- residuals(hs22_pulseGLMER) #extracts the residuals and places them in a new column in our original data table
hs2022Summary$Abs.Model.F.Res <-abs(hs2022Summary$Model.F.Res) #creates a new column with the absolute value of the residuals
hs2022Summary$Model.F.Res2 <- hs2022Summary$Abs.Model.F.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
hs22pulse_Levene.Model.F <- lm(Model.F.Res2 ~ animal, data=hs2022Summary) #ANOVA of the squared residuals
anova(hs22pulse_Levene.Model.F) #p significant
plot(hs22_pulseGLMER)



#post-hoc analysis determining which temperatures are significantly different
hs22_pulseTUKEY <- glht(hs22_pulseGLMER, mcp(temperature="Tukey"))
summary(hs22_pulseTUKEY)


######changeInPulse2022#####
#linear mixed effect model with temperature (in this case its the difference between baseline and the temperature), location and appendage color as fixed effect and animal/date as random effect. No longer using "pulse" as a count, but as "change"
#origionally attempted to run a glmer with  normal distribution, but R directed me to just run the lmer. 
hs22_diffLMER <- lmer(pulse ~ change + acclimation + color + (1|animal) + (1|date), REML = FALSE, data=hs2022Diff)
summary(hs22_diffLMER)
sjPlot::tab_model(hs22_diffLMER)


#assumptions of LMER:
#Linear? 
plot(resid(hs22_diffLMER), )
#HOV
hs2022Diff$Model.F.Res <- residuals(hs22_diffLMER) #extracts the residuals and places them in a new column in our original data table
hs2022Diff$Abs.Model.F.Res <-abs(hs2022Diff$Model.F.Res) #creates a new column with the absolute value of the residuals
hs2022Diff$Model.F.Res2 <- hs2022Diff$Abs.Model.F.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
hs22diff_Levene.Model.F <- lm(Model.F.Res2 ~ animal, data=hs2022Diff) #ANOVA of the squared residuals
anova(hs22diff_Levene.Model.F) #displays the results
#p = 2.427e-05 which is significant. 
plot(hs22_diffLMER) #plot seems okay though so could just be because the values are centered around 0
#normality
hist(hs2022Diff$pulse) #still bell looking

#linear mixed effect model assessing if there is an interaction between temperature and location. Temperature (in this case its the difference between baseline and the temperature), location and appendage color as fixed effect and animal/date as random effect. 
#origionally attempted to run a glmer with  normal disrtribution, but R directed me to just run the lmer. 
hs22_diffLMERint <- lmer(pulse ~ change*acclimation + color + (1|animal) + (1|date), REML = FALSE, data=hs2022Diff)
summary(hs22_diffLMERint) #####fixed effect of accliamtion was not significant, therefore will not be using this statistical analysis.


######temperature at death 2022#####
#subset TODall to only contain samples from 2022
TOD22 <- subset(TODall, DateStress == 2022)
hs22_todLMER <- lmer(tempDeath ~ color+temp + (1|block), REML = FALSE, data=TOD22)
summary(hs22_todLMER)
# Fixed effects:
#   Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)  39.9899     0.1207 50.0000 331.241   <2e-16 ***
#   colorBrown   -0.4941     0.1571 50.0000  -3.145   0.0028 ** 
#   LocationBay  -0.1497     0.1571 50.0000  -0.952   0.3454   
sjPlot::tab_model(hs22_todLMER)

######percent color####
hs2022_percentColor <- subset(hs2022_percentColor, percentBlue < 60)
hs2022_percentColor <- subset(hs2022_percentColor, sample != "4E_8")
hs22_ColorDeathcorr <-  cor.test(hs2022_percentColor$percentBlue, hs2022_percentColor$tempDead, method = "pearson")
hs22_ColorDeathcorr
sjPlot::tab_model(hs22_ColorDeathcorr)


####SUPPLEMENTAL MATERIAL#####
######size2022######
size2022 %>%
  group_by(lengthTime) %>%
  shapiro_test(length)

# lengthTime      variable statistic     p
# <chr>           <chr>        <dbl> <dbl>
#   1 baseline        length       0.979 0.650
# 2 postAcclimation length       0.971 0.391
# 3 postStress      length       0.985 0.861
# 4 preStress       length       0.986 0.901

##minor data wwrangling in order to run stats
size2022 <- na.omit(size2022)
size2022 <- subset(size2022, size2022$sampleName != "4E_4")
size2022$lengthTime <- gsub("baseline", "Baseline", size2022$lengthTime)
size2022$lengthTime <- gsub("postAcclimation", "Post Acclimation", size2022$lengthTime)
size2022$lengthTime <- gsub("postStress", "Post Stress", size2022$lengthTime)
size2022$lengthTime <- gsub("preStress", "Pre-Stress", size2022$lengthTime)
size2022$lengthTime <- factor(size2022$lengthTime, levels=c("Baseline", "Post Acclimation", "Pre-Stress", "Post Stress"))
##running a repeated measures ANOVA with dependent variable as length, within variation from the sample name (individuial), and within measurement (ie, time) is length time
size22.aov <- anova_test(data = size2022, dv = length, wid = sampleName, within =acclimation+color)
sizeAOV <- aov(length~temp+color, data=size2022)
summary(sizeAOV)

symAOV <- aov(length~acclimation+color, data=symbiont2022)
summary(symAOV)
get_anova_table(sizeAOV) #significant differnce between time points
# ANOVA Table (type III tests)
# Effect  DFn   DFd      F        p p<.05   ges
# 1 lengthTime 2.36 87.26 74.404 2.59e-21     * 0.151


##full model
size22_fullmodel <- aov(length~color+temp+lengthTime+Error(sampleName), data = size2022)
summary(size22_fullmodel)

summary(lmer(length~color+temp+lengthTime+(1|sampleName), REML = FALSE, data = size2022))
sjPlot::tab_model(lmer(length~color+temp+lengthTime+(1|sampleName), REML = FALSE, data = size2022))


######symbiont density 2022#####

##minor data wrangling
symbiont2022$acclimation <- substr(symbiont2022$sampleName,2,2)
symbiont2022$acclimation[symbiont2022$acclimation == "A"] <- 'Ambient (26°C)'
symbiont2022$acclimation[symbiont2022$acclimation == "E"] <- 'Elevated (33°C)'
symbiont2022$timepoint <- factor(symbiont2022$timepoint, levels=c("Before Acclimation", "Post Acclimation"))
symbiont2022$color <- factor(symbiont2022$color, levels=c("Blue", "Brown"))
symbiont2022$acclimation <- factor(symbiont2022$acclimation, levels=c("Ambient (26°C)", "Elevated (33°C)"))
symbiont2022$density <- as.numeric(symbiont2022$density)


symbiont2022$timepoint[symbiont2022$timepoint == "before"] <- 'Before Acclimation'
symbiont2022$timepoint[symbiont2022$timepoint == "after"] <- 'Post Accliamtion'

symbiont22_rma <- aov(density~timepoint+color+acclimation+Error(sampleName), data = symbiont2022)
summary(symbiont22_rma)
sjPlot::tab_model(symbiont22_rma)

lm(density~color+acclimation+timepoint, REML = FALSE, data = symbiont2022)
sjPlot::tab_model(lmer(length~color+temp+lengthTime+(1|sampleName), REML = FALSE, data = size2022))
