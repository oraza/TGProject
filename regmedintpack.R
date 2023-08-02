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
                            c_cond = NULL,
                            ## Model types
                            mreg = "linear",
                            yreg = "linear",
                            ## Additional specification
                            interaction = TRUE,
                            casecontrol = FALSE)
summary(regmedint_obj1) 

### analysis beyond this is following in regmedintpack_2.R script

# Note moderator value is fixed at mean (ie zero),
# following lines I will generate value of cde for various values of moderator
# by imputing values of moderator into "regmedint_obj1$myreg$est_fun()"


## generating sequential values of moderator based on the range of moderator 
## values after standardization ie from -1.49 to 2.67 
## 
akuads_seq <- seq(from = -1.49, to = 2.67, by = 0.01)
# Create an empty vector to store the results
seq.vector.se <- numeric(length(akuads_seq))
# passing 'akuads_seq' vector in following loop

for(i in seq_along(akuads_seq)) {
  result <- regmedint_obj1$myreg$est_fun(
    a0=0,
    a1=1,
    m_cde = akuads_seq[i],
    c_cond = NULL
    )
  seq.vector[i]<- result
  }
# Plotting values of CDE for various values of mediator/moderator
plot(akuads_seq, seq.vector, type = "l",
     xlab = "Mediator Value", ylab = "CDE Estimate")











# Loop through each mediator value
for(i in seq_along(akuads_seq)) {
  result <- regmedint(data = tg.data,
                      ## Variables
                      yvar = "BSSIScore2",
                      avar = "SCTotalScore",
                      mvar = "AKUADSScore2",
                      cvar = NULL,
                      #eventvar = "event",
                      ## Values at which effects are evaluated
                      a0 = 0,
                      a1 = 1,
                      m_cde = akuads_seq[i],
                      c_cond = NULL,
                      mreg = "linear",
                      yreg = "linear",
                      interaction = TRUE,
                      casecontrol = FALSE)
  seq.vector[i] <- result$estimates$CDE
  
  # Check if the result contains a valid CDE estimate
  if (!is.null(result$estimates$CDE)) {
    seq.vector[i] <- result$estimates$CDE
  } else {
    seq.vector[i] <- NA  # or any other value to indicate an error
  }
}
summary(result)

# sapply function

akuads_seq <- seq(from = 0, to = 44, by = 0.1)

# Using sapply to loop through each mediator value and calculate the CDE
seq.vector <- sapply(akuads_seq, function(m_val) {
  result <- regmedint(data = tg.data,
                      yvar = "BSSIScore2",
                      avar = "SCTotalScore",
                      mvar = "AKUADSScore2",
                      cvar = NULL,
                      a0 = 0,
                      a1 = 1,
                      m_cde = m_val,
                      c_cond = NULL,
                      mreg = "linear",
                      yreg = "linear",
                      interaction = TRUE,
                      casecontrol = FALSE)
  
  # Extract the CDE estimate from the result
  if (!is.null(result$estimates$CDE)) {
    return(result$estimates$CDE)
  } else {
    return(NA)  # or any other value to indicate an error
  }
})



