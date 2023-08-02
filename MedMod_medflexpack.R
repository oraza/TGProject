library(medflex)
data("UPBdata")
head(UPBdata)

# mediator: negaff 
# predictor: att
# outcome: UPB

# data
setwd("F:\\Megasync\\transgenderdata")
data.tg <- readxl::read_xlsx("Data_transgenderBSSImodification_dataUpdated.xlsx", 
                             sheet = "Sheet1")

data.tg2 <- read.csv("Data_transgenderBSSImodification_dataUpdatedfromspss.csv")

setwd("/Users/sulailfatima/Desktop/Megasync/transgenderdata")
tg.data <- read.spss("Data_transgenderBSSImodification_dataUpdated.sav",
                     to.data.frame = T,
                     use.value.labels = TRUE,
                     trim.factor.names = F,
                     trim_values = F)
# Scaling
## Standardized variables
## BSSI
ave.bssi <- mean(tg.data$BSSIScore2)
sd.bssi <- sd(tg.data$BSSIScore2)
tg.data$stdbssi <- ((tg.data$BSSIScore2 - ave.bssi)/sd.bssi)
## SC
ave.sc <- mean(tg.data$SCTotalScore)
sd.sc <- sd(tg.data$SCTotalScore)
tg.data$stdsc <- ((tg.data$SCTotalScore - ave.sc)/sd.sc)
## AKUADS
ave.akuads <- mean(tg.data$AKUADSScore2)
sd.akuads <- sd(tg.data$AKUADSScore2)
tg.data$stdakuads <- ((tg.data$AKUADSScore2 - ave.akuads)/sd.akuads)


medFit <- glm(AKUADSScore2 ~ SCTotalScore,
              family = gaussian, data = tg.data)
w <- weights(medFit)
neMod1 <- neModel(BSSIScore2 ~ SCTotalScore0  + SCTotalScore1,
                  expData = medFit)

# using this paper file:///C:/Users/OWAIS/Downloads/v76i11.pdf page 24
# on this paper https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7853644/pdf/nihms-1599745.pdf page 10

expData2 <-neImpute(stdbssi ~ stdsc*stdakuads, data= tg.data)
neMod2 <-neModel(stdbssi ~ stdsc0*stdsc1, expData = expData2,
                 se="bootstrap",
                 nBoot = 1000)
neMod2eff<-neEffdecomp(neMod2)
summary(neMod2eff)
confint(neMod2eff, level=.95, type="perc")

par(mfrow = c(1,2))
plot(neMod2)
str(neMod2)

