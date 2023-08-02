library(regmedint)

setwd("/Users/sulailfatima/Desktop/Megasync/transgenderdata")
tg.data <- read.spss("Data_transgenderBSSImodification_dataUpdated.sav",
                     to.data.frame = T,
                     use.value.labels = TRUE,
                     trim.factor.names = F,
                     trim_values = F)

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

# running mediator / moderator analysis after standardization of all three
# variable to scale them 
regmedint_obj1 <- regmedint(data = tg.data,
                            ## Variables
                            yvar = "stdbssi",
                            avar = "stdsc",
                            mvar = "stdakuads",
                            cvar = NULL,
                            #eventvar = "event",
                            ## Values at which effects are evaluated
                            a0 = 0,
                            a1 = 1,
                            m_cde = 0, # mean value of AKUADSscore 
                            # (specifies the level of the mediator at 
                            # which the four-way decomposition is computed)
                            c_cond = NULL,
                            ## Model types
                            mreg = "linear",
                            yreg = "linear",
                            ## Additional specification
                            interaction = TRUE,
                            casecontrol = FALSE)
summary(regmedint_obj1) # Note moderator value is fixed at mean (ie zero),
# following lines I will generate value of cde for various values of moderator
# by imputing values of moderator into "regmedint_obj1$myreg$est_fun()"

coef(regmedint_obj1)
## generating sequential values of moderator based on the range of moderator 
## values after standardization ie from -1.49 to 2.67 
## 
akuads_seq <- tg.data$stdakuads
# Create an empty vector to store the results (sep vectors for est and se)
seq.vector.est <- numeric(length(akuads_seq))
seq.vector.se <- numeric(length(akuads_seq))

# passing 'akuads_seq' vector in following loop to store series of CDEs est into
# seq.vector.est

for(i in seq_along(akuads_seq)) {
  result.est <- regmedint_obj1$myreg$est_fun(
    a0=0,
    a1=1,
    m_cde = akuads_seq[i],
    c_cond = NULL
  )
  seq.vector.est[i]<- result.est
}
# and into seq.vector.se
for(i in seq_along(akuads_seq)) {
  result.se <- regmedint_obj1$myreg$se_fun(
    a0=0,
    a1=1,
    m_cde = akuads_seq[i],
    c_cond = NULL
  )
  seq.vector.se[i]<- result.se
}
# at this point, we have two vectors of same length, seq.vector.est contains
# estimates of cde and seq.vector.se contains std errors of cde. we will put
# these into a dataframe along with standardized akuads values
vectors.cde <- data.frame(akuads = tg.data$AKUADSScore2, cde_est = seq.vector.est, cde_se = seq.vector.se)
library(dplyr)
vectors.cde <- vectors.cde %>%
  mutate(lower_95ci = seq.vector.est - 1.96 * seq.vector.se,
         upper_95ci = seq.vector.est + 1.96 * seq.vector.se)
#View(vectors.cde)
vectors.cde <-vectors.cde[order(vectors.cde$cde_est),]
library(ggplot2)
ggplot(data = vectors.cde, aes(x = akuads, y = cde_est)) +
  annotate("rect",xmin=20,xmax=45,ymin=-Inf,ymax=Inf,alpha=0.4,fill="red2") +
  #geom_hline(yintercept = 0.0, linetype = "dashed", color = "green", size = 1) +
  geom_pointrange(aes(ymin = lower_95ci, ymax = upper_95ci), 
                  position=position_jitter(width=0.5, height = 0.01), alpha = 0.3) +
  scale_x_continuous(limits=c(0, 50)) +
  labs(x = "AKUADS Score", y = "Conrtolled Direct Effects") +
  theme_light(base_size = 16)
