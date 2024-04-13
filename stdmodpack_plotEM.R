library(stdmod)
setwd("/Users/sulailfatima/Desktop/Megasync/transgenderdata")
tg.data <- read.spss("Data_transgenderBSSImodification_dataUpdated.sav",
                     to.data.frame = T,
                     use.value.labels = TRUE,
                     trim.factor.names = F,
                     trim_values = F)

# imputing age 
tg.data$ImputAge <- tg.data$SocCap36a
tg.data$ImputAge[is.na(tg.data$ImputAge)] <- c(32, 35, 35, 34, 33, 36, 34)
tg.data$ImputAge[27] <- 34
tg.data$ImputAge[68] <- 32
tg.data$ImputAge[74] <- 35
tg.data$ImputAge[178] <- 34
tg.data$ImputAge[159] <- 33
mean(tg.data$ImputAge)
sd(tg.data$ImputAge)
# Dummy variables for 35a (employ), 42a (child < 18yo), 44a (source of income),
# 45a (monthly inc), 46a (edu level)
tg.data$dmyEmp <- ifelse(tg.data$SocCap35a == "Yes", 1, 0)
tg.data$dmyChld <- ifelse(tg.data$SocCap42a == "Yes", 1, 0)
tg.data$dmyInco <- ifelse(tg.data$SocCap44a_Rec == "Wages & salary", 1, 0)
tg.data$dmyEdu <- ifelse(tg.data$SocCap46a == "Less than or upto primary level",
                         0 ,1)
tg.data$dmyLivingAlone <- ifelse(tg.data$SocCap41a_Rec == "Alone",
                         1 ,0)

# imputing edu
mode_education <- as.numeric(names(sort(table(tg.data$dmyEdu), decreasing = TRUE)[1]))
tg.data$dmyEdu[is.na(tg.data$dmyEdu)] <- mode_education


## visualization of moderation effect of Age

tg.model.akuads <- lm(AKUADSScore2 ~ SCTotalScore + ImputAge + dmyEmp + 
                        dmyEdu  + ImputAge*SCTotalScore,
                      data = tg.data)
tg.EMPlot.akuads <- plotmod(tg.model.akuads,
                            x = "SCTotalScore",
                            w = "ImputAge",
                            #x_from_mean_in_sd = 0.5,
                            #w_from_mean_in_sd = 0.5,
                            #ylim = c(0, 30),
                            x_label = "Social Capital Score",
                            y_label = "AKUADSScore2",
                            w_label = "ImputAgeCat",
                            w_method = "sd",
                            #w_sd = "percentile",
                            no_title = T,
                            #w_percentiles = c(0.25, 0.75),
                            graph_type = "tumble"
                            ) + 
  scale_color_manual(values = c("grey60","green","darkgreen")) +
  theme_bw()
ggsave("EM_age.jpg", dpi = 600, 
       width = 15, height = 10, units = "cm",
       path = "~/Desktop/Megasync/transgenderdata")

tg.EMPlot.akuads + theme(plot.subtitle = element_blank())

  
summary(tg.model.akuads)


model.0 <- lm(BSSIScore2 ~ AKUADSScore2 + SCTotalScore,
              data = tg.data)

model.1 <- lm(BSSIScore2 ~ AKUADSScore2 + SCTotalScore + ImputAge + dmyEmp + 
                dmyEdu + dmyInco,
              data = tg.data)

model.2 <- lm(BSSIScore2 ~ AKUADSScore2 + SCTotalScore + ImputAge + dmyEmp + 
                dmyEdu + dmyInco + AKUADSScore2*SCTotalScore,
              data = tg.data)
summary(model.0)
summary(model.1)
summary(model.2)
