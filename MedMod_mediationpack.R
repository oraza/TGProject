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
mod2 <- lm(stdakuads ~ stdsc, data = tg.data)
mod3 <- lm(stdbssi ~ stdakuads + stdsc, data = tg.data)
mod4 <- lm(stdbssi ~ stdsc*stdakuads, data = tg.data)

medmodel.tg <- mediate(model.m = mod2, model.y = mod3,
                       sims = 10000,
                       boot = T,
                       boot.ci.type = "bca",
                       treat = "stdsc",
                       mediator = "stdakuads")
summary(medmodel.tg)
plot(medmodel.tg, xlim = c(-.6, 0.6))
flexplot::visualize(mod4)
summary(mod4)
plot(mod4)

mediate_plot(stdbssi ~ stdakuads + stdsc, data = tg.data)
