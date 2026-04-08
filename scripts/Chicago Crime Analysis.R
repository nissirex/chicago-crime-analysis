# =========================================================
# CHICAGO CRIME ANALYSIS
# Data Mining & Predictive Modelling
# =========================================================

# -----------------------------
# 1. Load required libraries
# -----------------------------
library(tidyverse)
library(lubridate)
library(janitor)
library(leaflet)
library(sf)
library(here)
library(htmlwidgets)

# -----------------------------
# 2. Create output folders
# -----------------------------
# These folders will be created automatically if they do not already exist
dir.create(here("outputs"), showWarnings = FALSE)
dir.create(here("outputs", "plots"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("data", "processed"), recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 3. Load and clean the dataset
# -----------------------------
# Read the dataset from data/raw and clean column names
crime_full <- read_csv(here("data", "raw", "Crimes_-_2001_to_Present.csv")) |>
  clean_names()

# Remove duplicate records based on unique crime ID
crime_full <- crime_full |>
  distinct(id, .keep_all = TRUE)

# Check total number of rows after removing duplicates
nrow(crime_full)

# -----------------------------
# 4. Take a sample for analysis
# -----------------------------
# Set seed so results are reproducible
set.seed(123)

# Sample 1,000,000 rows to make analysis more manageable
crime <- crime_full |>
  slice_sample(n = 1000000)

# Inspect the dataset
glimpse(crime)
names(crime)
summary(crime)

# Check missing values in each column
colSums(is.na(crime)) |> sort(decreasing = TRUE)

# DATA QUALITY NOTES:
# - Most important variables such as date, primary_type, arrest, and domestic
#   have little or no missing data.
# - Location-related variables such as ward and community_area have some
#   missing values.
# - Latitude and longitude have a small amount of missing data.
# - Overall, the dataset is suitable for time, crime type, and location analysis.

# -----------------------------
# 5. Convert and prepare date variables
# -----------------------------
# Convert date column and create time-based features
crime2 <- crime |>
  mutate(
    date = mdy_hms(date),
    year = year(date),
    month = month(date),
    hour = hour(date)
  )

# Quick checks
head(crime2$date)
range(crime2$year, na.rm = TRUE)

# Save cleaned dataset for later use
write_csv(crime2, here("data", "processed", "crime_clean.csv"))

# -----------------------------
# 6. Crime trend over time
# -----------------------------
# Count number of crimes per year
crime_by_year <- crime2 %>%
  count(year, name = "crime_count")

# Create yearly crime trend plot
crime_trend_plot <- ggplot(crime_by_year, aes(x = year, y = crime_count)) +
  annotate(
    "rect",
    xmin = 2020, xmax = 2020.9,
    ymin = -Inf, ymax = Inf,
    alpha = 0.10, fill = "grey45"
  ) +
  geom_line(color = "#2C3E50", linewidth = 1) +
  geom_point(color = "#E74C3C", size = 2) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = "#3498DB",
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = 2020.45,
    y = max(crime_by_year$crime_count) * 0.88,
    label = "2020:\nCOVID lockdowns",
    size = 3.0,
    hjust = 0.5
  ) +
  labs(
    title = "Crime Trend in Chicago Over Time",
    subtitle = "Annual number of reported crimes (2001–2023)",
    x = "Year",
    y = "Number of Crimes"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Display plot
crime_trend_plot

# Save plot
ggsave(
  filename = here("outputs", "plots", "crime_trend_over_time.png"),
  plot = crime_trend_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# -----------------------------
# 7. Arrest rate over time
# -----------------------------
# Calculate yearly crime count and arrest rate
arrest_by_year <- crime2 %>%
  group_by(year) %>%
  summarise(
    crime_count = n(),
    arrest_rate = mean(arrest == TRUE, na.rm = TRUE),
    .groups = "drop"
  )

# Create arrest rate trend plot
arrest_trend_plot <- ggplot(arrest_by_year, aes(x = year, y = arrest_rate * 100)) +
  annotate(
    "rect",
    xmin = 2015, xmax = 2016,
    ymin = -Inf, ymax = Inf,
    alpha = 0.08, fill = "grey55"
  ) +
  annotate(
    "rect",
    xmin = 2020, xmax = 2020.9,
    ymin = -Inf, ymax = Inf,
    alpha = 0.10, fill = "grey45"
  ) +
  geom_line(color = "#2C3E50", linewidth = 1) +
  geom_point(color = "#E74C3C", size = 2) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = "#3498DB",
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = 2015.5, y = 12,
    label = "2015-2016: Post Laquan\nMcDonald case",
    size = 3.4, hjust = 0.5
  ) +
  annotate(
    "text",
    x = 2020.45, y = 29.5,
    label = "2020 disruption:\nCOVID lockdowns +\nGeorge Floyd protests",
    size = 3.4, hjust = 0.5
  ) +
  labs(
    title = "Arrest Rate in Chicago Over Time",
    subtitle = "Trend in proportion of crimes resulting in arrest",
    x = "Year",
    y = "Arrest Rate (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Display plot
arrest_trend_plot

# Save plot
ggsave(
  filename = here("outputs", "plots", "arrest_rate_over_time.png"),
  plot = arrest_trend_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# -----------------------------
# 8. Combined crime and arrest trend
# -----------------------------
# Create indexed values so both trends can be compared on the same scale
combined_data <- arrest_by_year %>%
  arrange(year) %>%
  mutate(
    crime_index = (crime_count / first(crime_count)) * 100,
    arrest_index = (arrest_rate / first(arrest_rate)) * 100
  )

# Convert to long format for plotting
combined_long <- combined_data %>%
  select(year, crime_index, arrest_index) %>%
  pivot_longer(
    cols = c(crime_index, arrest_index),
    names_to = "metric",
    values_to = "index"
  )

# Create combined trend plot
combined_trend_plot <- ggplot(combined_long, aes(x = year, y = index, color = metric)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("crime_index" = "#00BFC4", "arrest_index" = "#F8766D"),
    labels = c("Crime count", "Arrest rate")
  ) +
  labs(
    title = "Crime and Arrest Trends in Chicago",
    subtitle = "Both series indexed to first year = 100",
    x = "Year",
    y = "Index",
    color = "Metric"
  ) +
  theme_minimal()

# Display plot
combined_trend_plot

# Save plot
ggsave(
  filename = here("outputs", "plots", "crime_and_arrest_trends_indexed.png"),
  plot = combined_trend_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# -----------------------------
# 9. Crime by community area
# -----------------------------
# Count crimes by community area for recent years
crime_area <- crime2 %>%
  filter(year >= 2019) %>%
  filter(!is.na(community_area)) %>%
  count(community_area, name = "crime_count")

# Load Chicago community area boundaries from GeoJSON
chicago <- st_read(
  "https://data.cityofchicago.org/resource/igwz-8jzy.geojson",
  quiet = TRUE
)

# Optional: inspect column names if needed
names(chicago)

# Join crime counts with map boundaries
chicago_map <- chicago %>%
  mutate(area_num = as.numeric(area_numbe)) %>%
  left_join(crime_area, by = c("area_num" = "community_area"))

# Replace missing crime counts with 0
chicago_map$crime_count[is.na(chicago_map$crime_count)] <- 0

# Create colour palette
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = chicago_map$crime_count
)

# Create leaflet map
crime_map <- leaflet(chicago_map) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~pal(crime_count),
    weight = 1,
    color = "white",
    fillOpacity = 0.7,
    popup = ~paste0(
      "<strong>", community, "</strong><br>",
      "Crime count: ", crime_count
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ~crime_count,
    opacity = 0.7,
    title = "Crime count"
  )

# Display map
crime_map

# Save interactive map as HTML
saveWidget(
  widget = crime_map,
  file = here("outputs", "plots", "crime_map_community_area.html"),
  selfcontained = TRUE
)

# -----------------------------
# 10. Correlation between arrest rate and crime count
# -----------------------------
# Calculate correlation
correlation <- cor(
  arrest_by_year$arrest_rate,
  arrest_by_year$crime_count,
  use = "complete.obs"
)

# Create scatter plot with regression line
correlation_plot <- ggplot(arrest_by_year, aes(x = arrest_rate, y = crime_count)) +
  geom_point(color = "#E74C3C", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "#2C3E50", linewidth = 1) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Arrest Rate vs Crime Count",
    subtitle = paste("Correlation =", round(correlation, 2)),
    x = "Arrest Rate",
    y = "Number of Crimes"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Display plot
correlation_plot

# Save plot
ggsave(
  filename = here("outputs", "plots", "arrest_rate_vs_crime_count.png"),
  plot = correlation_plot,
  width = 9,
  height = 6,
  dpi = 300
)

# -----------------------------
# 11. Regression analysis
# -----------------------------
# Test whether arrest rate and year explain crime count
yearly_model_data <- arrest_by_year

model2 <- lm(crime_count ~ arrest_rate + year, data = yearly_model_data)

# View regression results
summary(model2)

# INTERPRETATION:
# - Arrest rate is not a statistically significant predictor of crime count.
# - Year is highly significant, which suggests a strong time trend.
# - This means crime reduction may be influenced more by broader long-term
#   changes than by policing levels alone.

# -----------------------------
# 12. Classification model for arrest outcome
# -----------------------------
# Prepare data for classification
class_data <- crime2 |>
  select(arrest, domestic, primary_type, district) |>
  drop_na()

# Fit logistic regression model
model_class <- glm(
  arrest ~ domestic + primary_type + district,
  data = class_data,
  family = "binomial"
)

# View classification model summary
summary(model_class)

# INTERPRETATION:
# - The probability of arrest changes depending on crime type, district,
#   and whether the incident is domestic.
# - Some crimes, such as narcotics and weapons violations, are more likely
#   to result in arrest than others.