# Load required libraries
install.packages(c("dplyr", "ggplot2"))
library(dplyr)
library(ggplot2)

# Read the CSV file
d1 <- read.csv("covid_19_india.csv")

# Clean data (if needed)
# For example, you might want to convert the "Date" column to a Date object
d1$Date <- as.Date(d1$Date, format = "%d/%m/%Y")

# Calculate total cases state-wise
total_cases_state <- d1 %>%
  group_by(State.UnionTerritory) %>%
  summarize(total_cases = sum(Confirmed))

# View the result
print(total_cases_state)

# Visualize the data using a bar plot
ggplot(total_cases_state, aes(x = reorder(State.UnionTerritory, -total_cases), y = total_cases)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total COVID-19 Cases State-wise",
       x = "State/Union Territory",
       y = "Total Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#  cure 



# Assuming total_cases_over_time is your data frame with Date and total_cured columns
# Load the required library
library(readr)

# Read the data from the CSV file
covid_data <- read_csv("covid_19_india.csv")
# Load the required library
library(lubridate)

# Convert the "Date" column to a date format
covid_data$Date <- as.Date(covid_data$Date, format = "%m/%d/%Y")

# Load the required libraries
library(ggplot2)

# Create the plot
ggplot(covid_data, aes(x = Date, y = Cured)) +
  geom_line(color = "green") +
  labs(title = "COVID-19 Cured Cases Over Time",
       x = "Date",
       y = "Cured Cases") +
  theme_minimal()

# death rate



# Step 1: Read the CSV file
data <- read.csv("covid_19_india.csv")

# Convert the 'Date' column to the correct date format (assuming it's in character format)
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Step 2: Group the data by date and calculate death rate for each date
total_death_over_time <- data %>%
  group_by(Date) %>%
  summarize(total_deaths = sum(Deaths),
            total_confirmed = sum(Confirmed)) %>%
  mutate(death_rate = (total_deaths / total_confirmed) * 100)

# Step 3: Create the line plot to visualize the death rate over time
ggplot(total_death_over_time, aes(x = Date, y = death_rate)) +
  geom_line(color = "red") +
  labs(title = "COVID-19 Death Rate Over Time",
       x = "Date",
       y = "Death Rate (%)") +
  theme_minimal()

# top 5 stae with total confirmed &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


# Step 1: Read the CSV file (assuming your file is named 'data.csv')
data <- read.csv("covid_19_india.csv")

# Step 2: Group by State/UnionTerritory and calculate total confirmed cases for each
total_confirmed_by_state <- data %>%
  group_by(State.UnionTerritory) %>%
  summarize(total_confirmed = sum(Confirmed))

# Step 3: Sort the data in descending order based on total confirmed cases
total_confirmed_by_state <- total_confirmed_by_state %>%
  arrange(desc(total_confirmed))

# Step 4: Select the top 5 rows representing the states/union territories with highest total confirmed cases
top_5_states <- head(total_confirmed_by_state, 5)

# Step 5: Create a bar plot to visualize the top 5 states/union territories
ggplot(top_5_states, aes(x = reorder(State.UnionTerritory, -total_confirmed), y = total_confirmed)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Top 5 States/Union Territories with Highest Total Confirmed COVID-19 Cases",
       x = "State/Union Territory",
       y = "Total Confirmed Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


