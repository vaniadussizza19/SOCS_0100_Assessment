
#Set up

data <- read.csv("COMPDATASET.csv")


#Rename variables
library(dplyr)

data <- data %>%
  rename(
    electricity = Number.of.people.with.access.to.electricity,
    noelectricity = Number.of.people.without.access.to.electricity,
    cleanfuel = number_with_clean_fuels_cooking,
    nocleanfuel = number_without_clean_fuels_cooking
  )

#Remove the 2,661 rows with NAs to have equal sample size per variable, increasing consistency within the data set

# Remove rows with NAs using na.omit()
data <- na.omit(data)


#Delete all regions and organisations from entities to prevent double counting

values_to_remove <- c(
  "Africa Eastern and Southern", 
  "Africa Western and Central", 
  "Arab World",
  "Caribbean Small States",
  "Central Europe and the Baltics",
  "Early-demographic dividend",
  "East Asia & Pacific",
  "East Asia & Pacific (excluding high income)",
  "East Asia & Pacific (IDA & IBRD)",
  "Euro area",
  "Europe & Central Asia",
  "Europe & Central Asia (excluding high income)",
  "Europe & Central Asia (IDA & IBRD)",
  "European Union",
  "Fragile and conflict affected situations",
  "Heavily indebted poor countries (HIPC)",
  "High income",
  "IBRD only",
  "IDA & IBRD total",
  "IDA blend",
  "IDA only",
  "IDA total",
  "Late-demographic dividend",
  "Latin America & Caribbean",
  "Latin America & Caribbean (excluding high income)",
  "Latin America & Caribbean (IDA & IBRD)",
  "Least developed countries: UN classification",
  "Low & middle income",
  "Low income",
  "Lower middle income",
  "Middle East & North Africa",
  "Middle East & North Africa (excluding high income)",
  "Middle East & North Africa (IDA & IBRD)",
  "Middle income",
  "North America",
  "OECD members",
  "Other small states",
  "Pacific island small states",
  "Post-demographic dividend",
  "Pre-demographic dividend",
  "Small states",
  "South Asia",
  "South Asia (IDA & IBRD)",
  "Sub-Saharan Africa",
  "Sub-Saharan Africa (excluding high income)",
  "Sub-Saharan Africa (IDA & IBRD)",
  "Upper middle income",
  "World"
)

data <- data %>%
  filter(!Entity %in% values_to_remove)


#Round all numbers with decimals downwards to the nearest whole number

data <- data %>%
  mutate_at(vars(electricity, noelectricity, cleanfuel, nocleanfuel), floor)



#CALCULATING MEANS:

data %>%
  summarize(
    avg_electricity = mean(electricity, na.rm = TRUE),
    sd_electricity = sd(electricity, na.rm = TRUE),   
    avg_noelectricity = mean(noelectricity, na.rm = TRUE),
    sd_noelectricity = sd(noelectricity, na.rm = TRUE), 
    avg_cleanfuel = mean(cleanfuel, na.rm = TRUE),
    sd_cleanfuel = sd(cleanfuel, na.rm = TRUE),
    avg_nocleanfuel = mean(nocleanfuel, na.rm = TRUE),
    sd_nocleanfuel = sd(nocleanfuel, na.rm = TRUE))



#DATA-WRANGLING OPERATIONS:

#Creating variable

library(dplyr)

data_clean_fuels <- data %>%
  mutate(Clean_fuels_Halfpop_Access = 100*(cleanfuel/(cleanfuel+nocleanfuel)))

data_clean_fuels <- data_clean_fuels %>%
  mutate(Clean_fuels_Halfpop_Access = ifelse(Clean_fuels_Halfpop_Access >= 50, "Yes", "No"))

View(data_clean_fuels)

count_yes <- table(data_clean_fuels$Clean_fuels_Halfpop_Access)["Yes"]
count_yes
count_no <- table(data_clean_fuels$Clean_fuels_Halfpop_Access)["No"]
count_no 



#Subsetting based on a year (single condition subsetting)

library(dplyr)
data_2016 <- data %>%
  filter(Year == 2016)


#creating a function

x <- data$noelectricity
y <- data$electricity
simple_function <- function(x, y) {
  print(100 * x / (x + y))
}
simple_function(x = x, y = y)

data_function_noelectricity <- data %>%
  mutate(No_Electricity_Access_Percentage = simple_function(x = x, y = y))
View(data_function_noelectricity)
#dplyr::arrange(data_function_noelectricity, desc(No_Electricity_Access_Percentage))
View(data_function_noelectricity)


#PART 2: DATA VISUALISATION

#First visualisation

library(dplyr)
data_Philippines <- data %>%
  filter(Entity == "Philippines")

library(tidyverse)
ggplot(data = data_Philippines)
ggplot(
  data = data_Philippines,
  mapping = aes(x = electricity, y = noelectricity)
)
ggplot(
  data = data_Philippines,
  mapping = aes(x = electricity, y = noelectricity)
) +
  geom_point()

ggplot(
  data = data_Philippines,
  mapping = aes(x = electricity, y = noelectricity, color = Year)
) +
  geom_point()

ggplot(
  data = data_Philippines,
  mapping = aes(x = electricity, y = noelectricity, color = Year)
) +
  geom_point() +
  labs(x = "Electricity", y = "No Electricity", title = "Electricity vs No Elecectricity Access in Philippines by Year")+
  geom_smooth(method = "lm")+
  scale_y_continuous(labels = scales::number_format(scale = 1000)) +  
  scale_x_continuous(labels = scales::number_format(scale = 6000))

#Second visualisation

#My code which initially wasn’t working: 
library(dplyr)
COMPDATASET_barplots <- COMPDATASET %>%
  filter(Year == 2003, Entity == "Brazil", "United States", "Burkina Faso", "Georgia", "Italy", "Nauru", "Myanmar", "Namibia", "Norway", "Sri Lanka")


#Chat GPT’s corrected version (added pipe operator and c to symbolise vector)

# Filter COMPDATASET for specific countries and year 2010
data_barplots <- data %>%
  filter(Year == 2003 & 
           Entity %in% c("Brazil", "United States", "Burkina Faso", "Georgia", "Italy", "Nauru", "Myanmar", "Namibia", "Norway", "Sri Lanka"))

#Filtering countries code: 
#2003:
data_barplots_2003 <- data %>%
  filter(Year == 2003 & 
           Entity %in% c("Brazil", "United States", "Burkina Faso", "Georgia", "Italy", "Nauru", "Myanmar", "Namibia", "Norway", "Sri Lanka"))

library(dplyr)

data_barplots_2003 <- data_barplots_2003 %>%
  mutate(Entity = ifelse(Entity == "Burkina Faso", "Burkina", Entity))%>%
  mutate(Entity = ifelse(Entity == "United States", "US", Entity))


#2016:
data_barplots_2016 <- data %>%
  filter(Year == 2016 & 
           Entity %in% c("Brazil", "United States", "Burkina Faso", "Georgia", "Italy", "Nauru", "Myanmar", "Namibia", "Norway", "Sri Lanka"))

data_barplots_2016 <- data_barplots_2016 %>%
  mutate(Entity = ifelse(Entity == "Burkina Faso", "Burkina", Entity))%>%
  mutate(Entity = ifelse(Entity == "United States", "US", Entity))


#2003 bar plot code with the help of ggplot and the geom_bar function: 
ggplot(data_barplots_2003, aes(x = Entity)) +
  geom_bar(aes(y = cleanfuel, fill = "Clean Fuel"), stat="identity", position = "dodge", width = 0.4) +
  labs(x = "Entity", y = "Number of People", title = "Access to Clean Fuel (2003)") +
  scale_fill_manual(values = c("Clean Fuel" = "grey"))  


#2016 bar plot code:
ggplot(data_barplots_2016, aes(x = Entity)) +
  geom_bar(aes(y = cleanfuel, fill = "Clean Fuel"), stat="identity", position = "dodge", width = 0.4) +
  labs(x = "Entity", y = "Number of People", title = "Access to Clean Fuel (2016)") +
  scale_fill_manual(values = c("Clean Fuel" = "grey"))



# Combine the data frames
combined_data <- rbind(
  transform(data_barplots_2016, Year = 2016),
  transform(data_barplots_2003, Year = 2003)
)

# Load ggplot2 package
library(ggplot2)

# Create a merged bar plot
ggplot(combined_data, aes(x = Entity, y = cleanfuel, fill = factor(Year))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.4) +
  labs(x = "Entity", y = "Number of People", title = "Access to Clean Fuel") +
  scale_fill_manual(values = c("2016" = "blue", "2003" = "red"))



#Third visualisation using functions


# Create a subset and then new data frame of COMPDATASET where Year is 2019
data_low_high <- COMPDATASET %>%
  filter(Entity %in% c("Low income", "High income"))
data_low_high <- data_low_high %>% 
  rename(`electricity` = `Number of people with access to electricity`)
data_low_high <- data_low_high %>% 
  rename(`noelectricity` = `Number of people without access to electricity`)

# Remove rows with Year values between 1990 and 2000
data_low_high <- data_low_high %>%
  filter(Year < 1990 | Year > 2000)

# Create function for ratio and mutate this variable (No electricity ratio) : I tried using just the map function from purr and it didn’t work so put it in chat GPT and learned about the map2_dbl function which is targeted to performing this action:

library(purrr)
data_low_high <- data_low_high %>%
  mutate(No_Electricity_Ratio = map2_dbl(noelectricity, electricity, ~ .x / .y))
View(data_low_high)


# Create visualisation 

library(ggplot2)
# Create a dot plot
dot_plot <- ggplot(data = data_low_high, aes(x = Year, y = No_Electricity_Ratio, color = Entity)) +
  geom_point() +
  scale_color_manual(values = c("Low income" = "red", "High income" = "orange")) +
  labs(x = "Year", y = "No Electricity Ratio", title = "No Electricity Ratio by Year for Low vs High Income Country Groups")

# Print the dot plot
print(dot_plot)


