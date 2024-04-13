library(mediation)
#devtools::install_github("dustinfife/flexplot", ref = "development")
library(flexplot)

setwd("/Users/sulailfatima/Desktop/Megasync/transgenderdata")

tg.data <- read.spss("Data_transgenderBSSImodification_dataUpdated.sav",
                     to.data.frame = T,
                     use.value.labels = TRUE,
                     trim.factor.names = F,
                     trim_values = F)
# View(tg.data)
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

# tg.data <- scale(tg.data)
# tg.data <- as.data.frame(tg.data)

# https://mspeekenbrink.github.io/sdam-r-companion/moderation-and-mediation.html

# causal steps
mod1 <- lm(stdbssi ~ stdsc, data = tg.data)
mod2 <- lm(AKUADSScore2 ~ SCTotalScore, data = tg.data)
mod3 <- lm(BSSIScore2 ~ AKUADSScore2 + SCTotalScore, data = tg.data)
mod4 <- lm(stdbssi ~ stdsc*stdakuads, data = tg.data)

medmodel.tg <- mediation::mediate(model.m = mod2, model.y = mod3,
                       sims = 10000,
                       boot = T,
                       boot.ci.type = "bca",
                       treat = "SCTotalScore",
                       mediator = "AKUADSScore2")
summary(medmodel.tg)
plot(medmodel.tg, xlim = c(-.6, 0.6), col = heat.colors(10),
       cex = 1.5, lwd = 2.5, treatment = "both")
flexplot::visualize(mod4)
summary(mod4)
plot(mod4)

mediate_plot(BSSIScore2 ~  AKUADSScore2 + SCTotalScore, data = tg.data,
             jitter = 8, alpha = 0.6, se = T)


## Mediation Analysis from Tingley-2014-mediated.pdf
med.fit <- lm(AKUADSScore2 ~ SCTotalScore + SocCap45a_Rec + SocCap35aaBinned, data = tg.data)
out.fit <- lm(BSSIScore2 ~ AKUADSScore2 + SCTotalScore + 
                SocCap45a_Rec + SocCap35aaBinned + 
                AKUADSScore2*SCTotalScore, data = tg.data)

med.out <- mediate(med.fit, out.fit, 
                   treat = "SCTotalScore",
                   mediator = "AKUADSScore2",
                   robustSE = TRUE,
                   sims = 10)
summary(med.out)
test.TMint(med.out, conf.level = .95)
sens.out <- medsens(med.out, rho.by = .1, effect.type = "indirect", sims = 10)
summary(sens.out)
plot(sens.out, sens.par = "rho", main = "BSSIScore2",sign.prod = "negative")
plot(sens.out, sens.par = "R2", r.type = "total", sign.prod = "positive")

##########################
## adding observed confounders
# causal steps
mod1 <- lm(BSSIScore2 ~ SCTotalScore + SocCap36a + SocCap35a + SocCap44a_Rec +
             SocCap45a_Rec + SocCap42a, data = tg.data)
mod2 <- lm(AKUADSScore2 ~ SCTotalScore + SocCap36a + SocCap35a + SocCap44a_Rec +
             SocCap45a_Rec + SocCap41a_Rec + SocCap42a, 
           data = tg.data) # mediator model
mod3 <- lm(BSSIScore2 ~ SCTotalScore  + AKUADSScore2 + SCTotalScore*AKUADSScore2 + 
             SocCap36a + SocCap35a + SocCap44a_Rec +SocCap45a_Rec + 
             SocCap41a_Rec + SocCap42a, data = tg.data) # outcome model
mod4 <- lm(BSSIScore2 ~ SCTotalScore*AKUADSScore2 + SocCap36a + 
             SocCap35a + SocCap44a_Rec + SocCap45a_Rec + SocCap41a_Rec +
             SocCap42a, data = tg.data)

medmodel.tg <- mediate(model.m = mod2, model.y = mod3,
                       sims = 100000,
                       boot = T,
                       boot.ci.type = "bca",
                       treat = "SCTotalScore",
                       mediator = "AKUADSScore2")
summary(medmodel.tg)
plot(medmodel.tg, xlim = c(-1.2, 0.1))


tg.testint <- test.TMint(medmodel.tg, conf.level = .95)
tg.testint

# sensitivity analysis

tg.sensi <- medsens(medmodel.tg, rho.by = 0.1, 
                    eps = sqrt(.Machine$double.eps),  
                    effect.type = "both")
summary(tg.sensi)
plot(tg.sensi)

