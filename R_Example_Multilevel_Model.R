# Simple data simulation


# Set random number generator
set.seed(1234)


# Sample size
N <- 100

# Variables in the model
soc.connection     <- round(abs(rnorm(N, 2, sd=5)), digits = 2)    # degree of perceived social connection
time               <- rep(1:4, N)                                # Assessment time (in years; year 1, year 2, year 3, year 4)
subjectno          <- rep(letters[1:N], each = 4)                # Subject variable
intercept.variance <- rep(rnorm(N, 2, sd = 2.5), each = 4)       # Random intercept variance for each individual



# Simulating the outcome variable (wellbeing) from assumed underlying model 
# considering the nested structure of the data as well as error variance 

# More wellbeing is linked to a higher degree of perceived social connection (0.4*soc.connection)
# This effect gets stronger over time (interaction: 0.9*soc.connection*time)
wellbeing <- (20 + intercept.variance)  + 0.2*time + 0.4*soc.connection + 0.9*soc.connection*time + rnorm(N, 0, sd = 2.5)


# Combine to data.frame
Data <- data.frame(wellbeing, soc.connection, time, subjectno)


# View simulated data
# View(Data)



# Test model using lme4 & lmerTest package in a sample with 100 participants
library(lme4) ; library(lmerTest)

model1 <- lmer(wellbeing ~ soc.connection*time + (1 | subjectno), data = Data)

summary(model1)
# Only the interaction effect is statistically detected (p < . 05)








