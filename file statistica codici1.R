#PLOT TEMPLATES

library(tidyverse)

# 1. Bar Chart
#Purpose
#Compare the counts or sums of categories (e.g., how many items belong to each category).

#Data Preparation
#Below we use the built-in mpg dataset. 
#We want to count how many observations exist for each car class.

mpg_summary <- mpg %>%
  group_by(class) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(data = mpg_summary, aes(x = class, y = count)) +
  geom_col(fill = "steelblue") +
  labs(title = "Number of Vehicles by Class",
       x = "Vehicle Class",
       y = "Count of Vehicles") +
  theme_minimal()


# 2. Line Graph
#Purpose
#Display trends over time or over a continuous variable (e.g., monthly sales, economic changes).
#Data Preparation
#We use the built-in economics dataset, which contains US economic data over time. 
#Assume we want to look at trends in the unemployment rate (unemploy) over time.

econ_filtered <- economics %>%
  filter(date >= as.Date("2000-01-01"))

ggplot(data = econ_filtered, aes(x = date, y = unemploy)) +
  geom_line(color = "darkred") +
  labs(title = "Unemployment Trends Since 2000",
       x = "Year",
       y = "Number of Unemployed (in thousands)") +
  theme_minimal()


# 3. Scatterplot

#Purpose
#Show the relationship between two continuous variables 
#(e.g., engine displacement vs. fuel efficiency).
#Data Preparation
#We use mpg again to see how engine size (displ) relates to fuel efficiency (hwy).

mpg_clean <- mpg %>%
  filter(!is.na(displ), !is.na(hwy))

ggplot(data = mpg_clean, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  labs(title = "Fuel Efficiency by Engine Displacement",
       x = "Engine Displacement (liters)",
       y = "Highway Miles per Gallon",
       color = "Vehicle Class") +
  theme_minimal()


# 4. Histogram

#Purpose
#Visualize the distribution of a single continuous variable 
#(e.g., how highway mileage is distributed).
#Data Preparation
#We’ll again use the mpg dataset to look at the distribution of highway mileage (hwy).

mpg_hist_data <- mpg %>%
  filter(!is.na(hwy))

ggplot(data = mpg_hist_data, aes(x = hwy)) +
  geom_histogram(binwidth = 2, fill = "cornflowerblue", color = "white") +
  labs(title = "Distribution of Highway Mileage",
       x = "Highway MPG",
       y = "Count") +
  theme_minimal()


#Purpose
#Visualize the distribution, median, and outliers of a continuous variable across categories 
#(e.g., comparing highway MPG across car classes).
#Data Preparation
#We use mpg to compare highway mileage (hwy) across car classes (class).

# Data Preparation
mpg_box_data <- mpg %>%
  filter(!is.na(class), !is.na(hwy))

# Boxplot
ggplot(data = mpg_box_data, aes(x = class, y = hwy)) +
  geom_boxplot(fill = "lightgreen", outlier.color = "red") +
  labs(title = "Highway Mileage by Vehicle Class",
       x = "Vehicle Class",
       y = "Highway MPG") +
  theme_minimal()

# 6. Faceted Plot

#Purpose
#Split a single plot into multiple subplots (facets), usually by category, 
#to compare trends or distributions side by side.
#Data Preparation
#We use mpg once more, demonstrating faceting by 
#class while looking at the relationship between engine displacement and highway MPG.

# Data Preparation
mpg_facet_data <- mpg %>%
  filter(!is.na(class), !is.na(displ), !is.na(hwy))

# Faceted Scatterplot
ggplot(data = mpg_facet_data, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  facet_wrap(~ class) +
  labs(title = "Engine Displacement vs. Highway MPG by Vehicle Class",
       x = "Engine Displacement (liters)",
       y = "Highway MPG") +
  theme_minimal()
