# Sensitivity analysis

library(readxl)
data <- read_excel("SensitivityAnalysis_Results.xlsx", skip = 2)

data <- data[,-c(1,3:24)]

df <- data.frame(data)

#Degree 

library(tidyverse)
df_degree <- df %>%
  select(CITY, min_degree, q1_degree, median_degree, q3_degree, max_degree)

#Reshape database

long_degree <- pivot_longer(df_degree, cols = c(min_degree, q1_degree, median_degree, q3_degree, max_degree), names_to = "variable", values_to = "value")

ggplot(long_degree, aes(x = value, y=CITY)) +
  geom_boxplot()






