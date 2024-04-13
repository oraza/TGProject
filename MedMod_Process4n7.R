# Running mediation and moderator analysis in Process 
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
tg.data$dmyInco <- model.matrix(~ tg.data$SocCap44a_Rec - 1)
tg.data$dmyEdu <- ifelse(tg.data$SocCap46a == "Less than or upto primary level",
                         0 ,1)
# imputing edu
mode_education <- as.numeric(names(sort(table(tg.data$dmyEdu), decreasing = TRUE)[1]))
tg.data$dmyEdu[is.na(tg.data$dmyEdu)] <- mode_education

View(tg.data)
# model 1: where AKUADS is placed as a moderator. J-N region of significance 
# showed that score above the scale positive (score > 20 on akuads) effect 
# remains significant. Therefore, it doesn't make sense to be an effect modifier
# 
set.seed(8189)
proc1Mod <- process(data = tg.data,
                    y = "BSSIScore2",
                    x = "SCTotalScore",
                    #m = "AKUADSScore2",
                    w = "ImputAgeCat",
                    cov = c("dmyEmp","dmyEdu"),#c("dmyInco", "dmyEmp"),
                    cmatrix = c(1,1),
                    model = 1, # redo model 7, 4, 3 & 2 (results seems pretty cool),
                    modelbt = 1,
                    moments = 1,
                    plot = 1,
                    normal = 1,
                    total = 1,
                    describe = 1,
                    jn = 1,
                    decimals = 1.3,
                    #xmtest=1,
                    save = 2, 
                    boot = 100000)

# without moderator
set.seed(8189)
proc4NoMod <- process(data = tg.data,
                      y = "BSSIScore2",
                      x = "SCTotalScore",
                      m = "AKUADSScore2",
                      #w = "ImputAge",
                      cov = c("dmyEmp","dmyEdu"),#c("dmyInco", "dmyEmp"),
                      cmatrix = c(1,1,1,1),
                      model = 4, # redo model 7, 4, 3 & 2 (results seems pretty cool),
                      modelbt = 1,  bc = 1,
                      moments = 1,
                      plot = 1,
                      normal = 1,
                      total = 1,
                      describe = 1,
                      jn = 1,
                      decimals = 1.4, stand = 1,
                      xmtest=1, save = 1, 
                      boot = 10000)
# with moderator
set.seed(8189)
proc7Mod <- process(data = tg.data,
                    y = "BSSIScore2",
                    x = "SCTotalScore",
                    m = "AKUADSScore2",
                    w = "ImputAge",
                    cov = c("dmyEmp","dmyEdu"),#c("dmyInco", "dmyEmp"),
                    cmatrix = c(1,1,1,1),
                    model = 7, # redo model 7, 4, 3 & 2 (results seems pretty cool),
                    modelbt = 1, bc = 1,
                    moments = 1,
                    plot = 1,
                    normal = 1,
                    total = 1,
                    describe = 1,
                    jn = 1,
                    decimals = 1.4,
                    xmtest=1, save = 4, 
                    boot = 10000)
proc7Mod$ab <- proc7Mod$col2 * proc7Mod$col9
hist(proc7Mod$ab)


## graphing from model 7 Conditional effect of focal predictor at values of the moderator:
## With moderator
age<- proc7Mod[22:43,1]
effect<- proc7Mod[22:43,2]
plot(age,effect,type="l",pch=19,lwd=3,ylab="Conditional Effect of Social Capital on Depression/Anxiety (a-path)",
     xlab="Age (W)")
llci<-proc7Mod[22:43,6]
ulci<-proc7Mod[22:43,7]
points(age,llci,lwd=2,lty=2,type="l",col="red")
points(age,ulci,lwd=2,lty=2,type="l",col="red")
abline(h=0,untf=FALSE,lty=3,lwd=1)

# Above graph in ggplot2
df.CondEff <- data.frame(age, effect, llci, ulci)
library(ggplot2)
ggplot(df.CondEff, aes(x = age)) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey50") +
  geom_vline(aes(xintercept=42.96), linetype = 2, color = "grey50") +
  geom_ribbon(aes(ymin = llci, ymax = ulci), alpha = 0.2, fill = "#2c403b") +
  geom_line(aes(y = effect), size = 1.2, color = "darkblue") + 
  geom_text(aes(x = 42.96, y = -0.8, label = "42.96"),
            angle = 90, vjust = 1.2, hjust = 0.4) +  # Add this line
  labs(x = "Age (Years)", y = "Conditional Effect of Social Capital on Anxiety/Depression"
       #title = "Conditional Effect of Social Capital Score on AKU-ADS Score (a-path)"
       ) +
  theme_light(base_size = 14)
ggsave("JNplot.jpg", dpi = 1200, 
       width = 18, height = 15, units = "cm",
       path = "~/Desktop/Megasync/transgenderdata")
 

# DAGs
# dag for model 7
library(dagitty)
dag7 <- dagitty('dag {
"Age*SC" [pos="0.422,0.464"]
Age [pos="0.439,0.466"]
AxDep [pos="0.443,0.487"]
Edu [pos="0.422,0.498"]
Emp [pos="0.412,0.468"]
SC [exposure,pos="0.413,0.485"]
SI [outcome,pos="0.457,0.488"]
"Age*SC" -> AxDep
Age -> "Age*SC"
Age -> AxDep
Age -> SI [pos="0.447,0.476"]
AxDep -> SI
Edu -> AxDep [pos="0.426,0.499"]
Edu -> SC
Emp -> AxDep [pos="0.417,0.472"]
Emp -> SC
SC -> "Age*SC"
SC -> AxDep
SC -> SI [pos="0.438,0.505"]
}
'
)
plot(dag7)



