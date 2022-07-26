
# PCA Principal component analysis to estimate severity level of Long COVID
# Reference: https://stats.stackexchange.com/questions/222/what-are-principal-component-scores

## Load libraries
library(stats)
library(haven) # required to read data in xlsx
library(writexl) # required to write data in xlsx

## 1. Import dataset
DF <- data.frame(fatigue=c(80, 90, 95), dyspnoea=c(85, 85, 80),  
                 lsmell=c(60, 70, 40), cogdiff=c(55, 45, 50))

## 2. PCA
prcomp(DF, scale = FALSE)
# The first column here shows coefficients of linear combination that defines principal component #1, 
# and the second column shows coefficients for principal component #2.


## 3. PCA Scoring
### 3.1 centering: #center each column in the data frame y sybtracting the value for the column mean
DF <- sapply(DF, function(x) scale(x, scale=FALSE)) # scale=FALSE, we tell R not to divide by the standard deviation
DF <- data.frame(DF)
### 3.2 And then to form linear combinations to get PC1 and PC2 scores:
DF$x <- 0.28*DF$fatigue + -0.17*DF$dyspnoea + -0.94*DF$lsmell  + 0.07*DF$cogdiff   
DF$y <- 0.77*DF$fatigue + -0.08*DF$dyspnoea  + 0.19*DF$lsmell  + -0.60*DF$cogdiff


## 4. Save results
write_xlsx(DF,"tables/DF_PCA_scoring.xlsx")
