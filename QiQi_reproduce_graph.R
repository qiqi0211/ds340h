# Install and load package 
> install.packages("readxl")

> library(readxl)
# load the dataset
my_data <- read_excel("C:/Users/liqiq/Downloads/MRMET normative data set.xlsx",skip = 3)

# view the dataset
View(my_data)

#install packages
install.packages("ggplot2")
install.packages("ggforce")
install.packages("dplyr")

library(ggplot2)
library(ggforce)
library(dplyr)

# subset only gender and MRMET data for the graph
df_columns <- select(my_data, gender, MRMET)
# filter out genderqueer for graph
df_columns <- df_columns[df_columns$gender != 'genderqueer',]
# view the filtered data
View(df_columns)

# use ggplot to create sinaplot
ggplot(df_columns, aes(x = gender, y = jitter(MRMET, amount = 0.9))) +
  # Add sinaplot points
  geom_sina(
    size = 0.7, alpha = 0.3, maxwidth = 0.6, color = "blue") +
  # Group means black line (calculated on original unjittered MRMET)
  stat_summary(aes(y = MRMET),fun = mean, fun.min = mean, fun.max = mean,
    geom = "crossbar", width = 0.7, linewidth = 0.7, color = "black") +
  theme_minimal() +
  labs(
    title = "Comparisons of MRMET by gender",
    x = "Gender",
    y = "MRMET"
  )
