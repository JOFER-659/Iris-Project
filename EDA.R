######
# Iris Dataset EDA
######
# BY: Jacob Hofer
#####

# Load Packages
library(datasets)
library(tidyverse)
library(GGally)


# Load Dataset
IrisData <- iris
head(IrisData)
summary(IrisData)


#First lets look at a box and whisker of all the quantitative variables
IrisData %>%
  pivot_longer(cols = -Species, 
               names_to = "Variable", 
               values_to = "Value") %>%
  ggplot(aes(x = Variable, y = Value, fill = Variable)) + geom_violin() + 
  labs(title = "Iris Dataset Quantitative Variables Violin plot",
       y = 'Value', x = 'Variable') +
  theme_minimal() + scale_fill_brewer(palette = 'Spectral')

#Now lets look at a violin plot for each variable by species
IrisData %>% 
  pivot_longer(cols = -Species, 
                names_to = "Variable", 
                values_to = "Value") %>%
  ggplot(aes(x = Species, y = Value, fill = Species)) + 
  geom_violin() + theme_minimal() +
  facet_grid(.~Variable) + scale_fill_brewer(palette = 'Spectral')

#lets assess the covariance of our variables
ggpairs(IrisData, columns = 1:4, aes(color = Species),
        upper = list(continuous = "points"))
         