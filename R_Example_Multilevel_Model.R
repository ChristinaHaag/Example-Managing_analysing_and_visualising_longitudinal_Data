####################################
### Example multilevel modelling ###
####################################


# Set random number generator
set.seed(1234)

# Simple simulation of data with nested structure

# Sample size
N <- 1000

# Variables in the model
soc.connection     <- round(abs(rnorm(N, 2, sd=5)), digits = 2)    # degree of perceived social connection
time               <- rep(1:4, N)                                  # Assessment time (in years; year 1, year 2, year 3, year 4)
subject            <- as.factor(rep(1:N, each = 4))                # Subject variable
intercept.variance <- rep(rnorm(N, 0, sd = 2.5), each = 4)         # Random intercept variance for each individual



# Simulating the outcome variable (wellbeing) from assumed underlying model 
# considering the nested structure of the data as well as error variance 

# More wellbeing is linked to a higher degree of perceived social connection (0.4*soc.connection)
# This effect gets stronger over time (interaction: 0.9*soc.connection*time)
wellbeing <- (20 + intercept.variance)  + 0.2*time + 0.4*soc.connection + 0.9*soc.connection*time + rnorm(N, 0, sd = 2.5)


# Combine to data.frame
Data <- data.frame(wellbeing, soc.connection, time, subject)


# View simulated data
# View(Data)


##############################################
# Data management
library(dplyr)

mean.Data <- Data  %>%
  select(subject, wellbeing) %>%  # Select variables
  group_by(subject) %>%           # Group by individual
  summarise(                      # Descriptives per individual
    #
    wellbeing.mean  = mean(wellbeing),
    wellbeing.sd    = sd(wellbeing))




##############################################
# Analysing the data

# Test model
library(lme4) ; library(lmerTest)

# Model
model1 <- lmer(wellbeing ~ soc.connection*time + (1 | subject), data = Data)

summary(model1)
# Only the interaction effect is statistically detected (p < . 05)



##############################################
# Visualising longitudinal data

# Select subset for individual-level plots
subset.data <- Data[1:80,]


# Display individual-level plots
library(ggplot2)

p <- ggplot2::ggplot(subset.data) + theme_bw() + facet_wrap("subject") +
  
  geom_point(aes(x = time, y = wellbeing), color="blue4", size=1.2) +
  geom_line(aes( x = time, y = wellbeing), color="blue4", size=1.2) +
  
  geom_point(aes(x = time, y = soc.connection), color="red", size=1.2)  +
  geom_line(aes( x = time, y = soc.connection), color="red", size=1.2)  +
  scale_x_discrete(name ="assessment time") +
  scale_y_discrete(name ="wellbeing")

# Static plot
p


# Interactive individual-level plot
library(plotly)
ggplotly(p)

