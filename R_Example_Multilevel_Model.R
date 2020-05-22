# Simple data simulation


# Sample size
N <- 100

# Variables in the model
social.contacts <- abs(rnorm(N, 2, sd=5))
time            <- rep(1:4, N)
subjectno       <- rep(letters[1:N], each = 4)



# Simulate outcome variable from assumed underlying model + error variance 
# (no multilevel structure considered yet)
# Individual wellbeing as a function of social contacts (no effect over time in this example)
wellbeing <- 20 + 0.8*social.contacts + 0*time + rnorm(N, 0, sd = 2.5)


# Combine to data.frame
Data <- data.frame(wellbeing, social.contacts, time, subjectno)


# View simulated data
# View(Data)


# Test model using lme4 package
library(lme4)

model1 <- lmer(wellbeing ~social.contacts + time + (1 | subjectno), data = Data)

summary(model1)
