####Data manipulation for Cassiopea Heat Stress Manuscript, Maloney et al., 2024


######Temperature Loggers from March - Septemeber 2022#######
#file location
fieldLogger <- read_excel("~/Library/CloudStorage/Box-Box/Buckley Lab Data/Maloney/MaloneyEtAl2024/unmodified data/temperatureLoggers.xlsx")
colnames(fieldLogger) #"location" "date"     "temp"     "month"    "monthDay"


#get date in own column for later summerization
fieldLogger$monthDay <- substr(fieldLogger$date,6,10)
fieldLogger <- subset(fieldLogger, monthDay >= '03-13')
fieldLogger$monthDay <- as.Date(fieldLogger$monthDay, format = "%m-%d")

#summarize for daily mean by location
loggerSummary <- ddply(fieldLogger, c("location", "monthDay"), summarise,
               mean = mean(temp),
               min = min(temp),
               max = max(temp),
               sd = sd(temp))
names(loggerSummary)[names(loggerSummary) == 'mean'] <- 'temp'
loggerSummary$day <- gsub("2024-", "", loggerSummary$day)


#calculate the rage by subtracting the highest temp on a single day (max) from the lowest (min)
loggerSummary$dailyRange <- (loggerSummary$max - loggerSummary$min)
loggerSummary$month <- substr(loggerSummary$monthDay,6,7)
loggerSummary$day <- substr(loggerSummary$monthDay,6,10)
loggerSummary$day <- as.Date(loggerSummary$day, format = "%m-%d")
loggerSummary$month <- gsub("03", "March", loggerSummary$month)
loggerSummary$month <- gsub("04", "April", loggerSummary$month)
loggerSummary$month <- gsub("05", "May", loggerSummary$month)
loggerSummary$month <- gsub("06", "June", loggerSummary$month)
loggerSummary$month <- gsub("07", "July", loggerSummary$month)
loggerSummary$month <- gsub("08", "August", loggerSummary$month)
loggerSummary$month <- gsub("09", "September", loggerSummary$month)


###file saved as:
#/Users/meganmaloney/Library/CloudStorage/Box-Box/Buckley Lab Data/Maloney/MaloneyEtAl2024/R_Studio/Maloneyetal2024
write_xlsx(loggerSummary,"/Users/meganmaloney/Library/CloudStorage/Box-Box/Buckley Lab Data/Maloney/MaloneyEtAl2024/R_Studio/Maloneyetal2024/fieldtempStats.xlsx")


#####heat stress 2021######
hs2021 <- read_excel("~/Library/CloudStorage/Box-Box/Buckley Lab Data/Maloney/MaloneyEtAl2024/unmodified data/hs2021_transposedRAW.xlsx")
colnames(hs2021) #"animal"       "video"        "date"         "pulse"        "meanInt"      "sex"          "baseDiameter" "moveDiameter" "endDiameter"  "tempDead"     "baseEnd"     "baseMove"     "moveEnd"      

names(hs2021)[names(hs2021) == 'Animal'] <- 'animal'
names(hs2021)[names(hs2021) == 'VideoName'] <- 'video'
names(hs2021)[names(hs2021) == 'ExperimentDate'] <- 'date'
names(hs2021)[names(hs2021) == 'Sex'] <- 'sex'
names(hs2021)[names(hs2021) == 'startDiameter'] <- 'baseDiameter'
hs2021$baseEnd <- (hs2021$baseDiameter - hs2021$endDiameter)
hs2021$baseMove <- (hs2021$baseDiameter - hs2021$moveDiameter)
hs2021$moveEnd <- (hs2021$moveDiameter - hs2021$endDiameter)
hs2021$location <- substr(hs2021$animal,1,1)
hs2021$location <- gsub("A", "Atlantic", hs2021$location)
hs2021$location <- gsub("B", "Bay", hs2021$location)
hs2021$temperature <- substr(hs2021$video,3,4)

#remove anything with a pulse of 0
hs2021 <- subset(hs2021, pulse > 1)
hs2021$color <- NA
hs2021$color[hs2021$animal == "A2-2"] <- 'BLUE'
hs2021$color[hs2021$animal == "A2-3"] <- 'BLUE'
hs2021$color[hs2021$animal == "A2-4"] <- 'BROWN'
hs2021$color[hs2021$animal == "A2-5"] <- 'BLUE'
hs2021$color[hs2021$animal == "A2-7"] <- 'BLUE'
hs2021$color[hs2021$animal == "A2-8"] <- 'BROWN'
hs2021$color[hs2021$animal == "A2-9"] <- 'BROWN'
hs2021$color[hs2021$animal == "A2-11"] <- 'BROWN'
hs2021$color[hs2021$animal == "A2-14"] <- 'BROWN'
hs2021$color[hs2021$animal == "A4-2"] <- 'BROWN'
hs2021$color[hs2021$animal == "A4-5"] <- 'BLUE'
hs2021$color[hs2021$animal == "A4-9"] <- 'BLUE'
hs2021$color[hs2021$animal == "A4-10"] <- 'BROWN'
hs2021$color[hs2021$animal == "A4-12"] <- 'BLUE'
hs2021$color[hs2021$animal == "A4-14"] <- 'BLUE'
hs2021$color[hs2021$animal == "A6-1"] <- 'BROWN'
hs2021$color[hs2021$animal == "A6-2"] <- 'BLUE'
hs2021$color[hs2021$animal == "A6-3"] <- 'BROWN'
hs2021$color[hs2021$animal == "A6-7"] <- 'BLUE'
hs2021$color[hs2021$animal == "A6-11"] <- 'BLUE'
hs2021$color[hs2021$animal == "A6-12"] <- 'BLUE'
hs2021$color[hs2021$animal == "A8-6"] <- 'BROWN'
hs2021$color[hs2021$animal == "A8-7"] <- 'BLUE'
hs2021$color[hs2021$animal == "A8-13"] <- 'BLUE'
hs2021$color[hs2021$animal == "A8-15"] <- 'BLUE'
hs2021$color[hs2021$animal == "B1-1"] <- 'BLUE'
hs2021$color[hs2021$animal == "B1-3"] <- 'BLUE'
hs2021$color[hs2021$animal == "B1-5"] <- 'BLUE'
hs2021$color[hs2021$animal == "B1-6"] <- 'BLUE'
hs2021$color[hs2021$animal == "B1-10"] <- 'BROWN'
hs2021$color[hs2021$animal == "B1-11"] <- 'BLUE'
hs2021$color[hs2021$animal == "B3-2"] <- 'BLUE'
hs2021$color[hs2021$animal == "B3-4"] <- 'BROWN'
hs2021$color[hs2021$animal == "B3-6"] <- 'BROWN'
hs2021$color[hs2021$animal == "B3-9"] <- 'BROWN'
hs2021$color[hs2021$animal == "B3-10"] <- 'BLUE'
hs2021$color[hs2021$animal == "B3-11"] <- 'BROWN'
hs2021$color[hs2021$animal == "B3-12"] <- 'BROWN'
hs2021$color[hs2021$animal == "B5-2"] <- 'BROWN'
hs2021$color[hs2021$animal == "B5-3"] <- 'BROWN'
hs2021$color[hs2021$animal == "B5-4"] <- 'BLUE'
hs2021$color[hs2021$animal == "B5-6"] <- 'BROWN'
hs2021$color[hs2021$animal == "B5-7"] <- 'BLUE'
hs2021$color[hs2021$animal == "B5-8"] <- 'BROWN'
hs2021$color[hs2021$animal == "B5-11"] <- 'BROWN'
hs2021$color[hs2021$animal == "B7-2"] <- 'BROWN'
hs2021$color[hs2021$animal == "B7-3"] <- 'BROWN'
hs2021$color[hs2021$animal == "B7-10"] <- 'BROWN'
hs2021$color[hs2021$animal == "B7-11"] <- 'BLUE'
hs2021$color[hs2021$animal == "B7-12"] <- 'BROWN'  

#summarize pulse
hs2021Summary <- ddply(hs2021, c("animal","date", "temperature", "location", "moveDiameter", "color"), summarise,
                             mean = mean(pulse))
hs2021Summary <- na.omit(hs2021Summary)
names(hs2021Summary)[names(hs2021Summary ) == 'mean'] <- 'pulse'
hs2021Summary$location <- as.factor(hs2021Summary$location)
hs2021Summary$temperature <- as.factor(hs2021Summary$temperature)
hs2021Summary$pulse <- as.integer(hs2021Summary$pulse)
hs2021Summary$color <- gsub("BLUE", "Blue", hs2021Summary$color)
hs2021Summary$color <- gsub("BROWN", "Brown", hs2021Summary$color)

###file saved as:
#/Users/meganmaloney/Library/CloudStorage/Box-Box/Buckley Lab Data/Maloney/MaloneyEtAl2024/R_Studio/Maloneyetal2024
write_xlsx(hs2021Summary,"/Users/meganmaloney/Library/CloudStorage/Box-Box/Buckley Lab Data/Maloney/MaloneyEtAl2024/R_Studio/Maloneyetal2024/hs2021Summary.xlsx")


####hs2021 change in pulse rate#
# substracting the basline rate minus the rate at differnt temperatures
hs2021_difference <- reshape(hs2021Summary, idvar="animal", timevar="temperature", v.names="pulse", direction="wide", sep="_")
hs2021_difference$b27 <- (hs2021_difference$pulse_27 - hs2021_difference$pulse_ba)
hs2021_difference$b28 <- (hs2021_difference$pulse_28 - hs2021_difference$pulse_ba)
hs2021_difference$b29 <- (hs2021_difference$pulse_29 - hs2021_difference$pulse_ba)
hs2021_difference$b30 <- (hs2021_difference$pulse_30 - hs2021_difference$pulse_ba)
hs2021_difference$b31 <- (hs2021_difference$pulse_31 - hs2021_difference$pulse_ba)
hs2021_difference$b32 <- (hs2021_difference$pulse_32 - hs2021_difference$pulse_ba)
hs2021_difference$b33 <- (hs2021_difference$pulse_33 - hs2021_difference$pulse_ba)
hs2021_difference$b34 <- (hs2021_difference$pulse_34 - hs2021_difference$pulse_ba)
hs2021_difference$b35 <- (hs2021_difference$pulse_35 - hs2021_difference$pulse_ba)
hs2021_difference$b36 <- (hs2021_difference$pulse_36 - hs2021_difference$pulse_ba)
hs2021_difference$b37 <- (hs2021_difference$pulse_37 - hs2021_difference$pulse_ba)
hs2021_difference$b38 <- (hs2021_difference$pulse_38 - hs2021_difference$pulse_ba)
hs2021_difference$b39 <- (hs2021_difference$pulse_39 - hs2021_difference$pulse_ba)
hs2021_difference$b40 <- (hs2021_difference$pulse_40 - hs2021_difference$pulse_ba)
pulseDiff1_21 <- subset(hs2021_difference, select=c(1:5, 21:34))
hs2021PulseDiff <- gather(pulseDiff1_21, change, pulse, `b27`:`b40`, factor_key = TRUE)
hs2021PulseDiff$change <- as.character.factor(hs2021PulseDiff$change)
hs2021Diff <- subset(hs2021PulseDiff, !is.na(hs2021PulseDiff$pulse))

###file saved as:
write_xlsx(hs2021Diff,"/Users/meganmaloney/Library/CloudStorage/Box-Box/Buckley Lab Data/Maloney/MaloneyEtAl2024/R_Studio/Maloneyetal2024/hs2021Diff.xlsx")

#######heat stress 2022######
hs2022 <- read_excel("~/Library/CloudStorage/Box-Box/Buckley Lab Data/Maloney/MaloneyEtAl2024/unmodified data/hs2022_transposedRAW.xlsx")
colnames(hs2022) #"count"          "shock"          "sex"            "temp"           "color"          "experimentDate" "videoName"     "sample" 
hs2022 <- na.omit(hs2022)
hs2022$temperature <- substr(hs2022$videoName,3,4)
hs2022$acclimation <- substr(hs2022$sample,2,2)
hs2022$acclimation[hs2022$acclimation == "n"] <- 'E'
hs2022$color[hs2022$color == "l"] <- 'BLUE'
hs2022$color[hs2022$color == "r"] <- 'BROWN'
hs2022$color[hs2022$color == "e"] <- 'BLUE'
hs2022$color[hs2022$color == "b"] <- 'BROWN'
hs2022$shock[hs2022$shock == "s"] <- 'Shocked'
hs2022$shock[hs2022$shock == "n"] <- 'Not Shocked'
hs2022$temperature[hs2022$temperature == "ba"] <- 'Baseline'
hs2022$sex[hs2022$sex == "F"] <- 'f'

hs2022$acclimation[hs2022$acclimation == "A"] <- 'Ambient (26°C)'
hs2022$acclimation[hs2022$acclimation == "E"] <- 'Elevated (33°C)'
names(hs2022)[names(hs2022) == 'count'] <- 'pulse'
hs2022$temperature <- as.factor(hs2022$temperature)
hs2022$color <- as.factor(hs2022$color)
hs2022$acclimation <- as.factor(hs2022$acclimation)
hs2022$sample[hs2022$sample == "4A-9"] <- '4A_9'
hs2022 <- subset(hs2022, pulse > 1)
#hs2022 <- subset(hs2022, sample != "2A_6")
#hs2022 <- subset(hs2022, sample != "2A_3")
hs2022 <- subset(hs2022, experimentDate != 07202022)
hs2022 <- subset(hs2022, temperature != 'Baseline')
names(hs2022)[names(hs2022 ) == 'sample'] <- 'animal'
names(hs2022)[names(hs2022 ) == 'experimentDate'] <- 'date'

##summarize
hs2022Summary <- ddply(hs2022, c("animal", "date", "temperature", "acclimation", "color"), summarise,
                             mean = mean(pulse),
                             sd = sd(pulse))
names(hs2022Summary)[names(hs2022Summary) == 'mean'] <- 'pulse'

###file saved as:
write_xlsx(hs2022Summary,"/Users/meganmaloney/Library/CloudStorage/Box-Box/Buckley Lab Data/Maloney/MaloneyEtAl2024/R_Studio/Maloneyetal2024/hs2022Summary.xlsx")

##substracting the basline rate minus the rate at differnt temperatures
hs2022_difference <- reshape(hs2022Summary, idvar="animal", timevar="temperature", v.names="pulse", direction="wide", sep="_")

pulseDiffAmb <- subset(hs2022_difference, acclimation == "Ambient (26°C)")
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

pulseDiffEle <- subset(hs2022_difference, acclimation == "Elevated (33°C)")
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

pulseDiffAmb22 <- subset(pulseDiffAmb, select=c(1:4, 21:35))
pulseDiffAmb22 <- gather(pulseDiffAmb22, change, pulse, `b27`:`b41`, factor_key = TRUE)
pulseDiffAmb22$change <- as.character.factor(pulseDiffAmb22$change)
pulseDiffEle22 <- subset(pulseDiffEle, select=c(1:4, 21:30))
pulseDiffEle22 <- gather(pulseDiffEle22, change, pulse, `b32`:`b41`, factor_key = TRUE)
pulseDiffEle22$change <- as.character.factor(pulseDiffEle22$change)

hs2022Diff <- rbind(pulseDiffAmb22, pulseDiffEle22)
hs2022Diff <- subset(hs2022Diff, !is.na(hs2022Diff$pulse))


###file saved as:
write_xlsx(hs2022Diff,"/Users/meganmaloney/Library/CloudStorage/Box-Box/Buckley Lab Data/Maloney/MaloneyEtAl2024/R_Studio/Maloneyetal2024/hs2022Diff.xlsx")

####all temperature at death#####
TODall <- read_excel("~/Library/CloudStorage/Box-Box/Buckley Lab Data/Maloney/MaloneyEtAl2024/unmodified data/TOD_allRAW.xlsx")
write_xlsx(TODall,"/Users/meganmaloney/Library/CloudStorage/Box-Box/Buckley Lab Data/Maloney/MaloneyEtAl2024/R_Studio/Maloneyetal2024/TODall.xlsx")
