library(stdmod)
setwd("/Users/sulailfatima/Desktop/Megasync/transgenderdata")
tg.data <- read.spss("Data_transgenderBSSImodification_dataUpdated.sav",
                     to.data.frame = T,
                     use.value.labels = TRUE,
                     trim.factor.names = F,
                     trim_values = F)
tg.interaction <- 
tg.model1 <- lm(BSSIScore2 ~ AKUADSScore2 + SCTotalScore + 
                  #SocCap45a_Rec + SocCap35aaBinned + 
                  ImputAge + dmyEmp + dmyEdu +
                  AKUADSScore2*SCTotalScore, data = tg.data)
tg.EMPlot <- plotmod(tg.model1, 
                     x = "SCTotalScore",
                     w = "AKUADSScore2",
                     w_method = "percentile",
                     w_percentiles = c(0.25, 0.75)
)
summary(tg.model1)
