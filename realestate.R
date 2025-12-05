library(tidyverse)
library(arrow)
library(janitor)
library(scales)

# 1. LOAD DATA
trans_df <- read_parquet("transactions.parquet") %>% clean_names()
gnaf_df  <- read_parquet("gnaf_prop.parquet") %>% clean_names()

# 2. CREATE A LOCATION MAPPING (Mesh Block -> Suburb)
# Since we can't link Property IDs directly, we link the "Area" (Mesh Block).
# We take one entry per Mesh Block to get its Suburb Name.
location_mapping <- gnaf_df %>%
  select(mb_2016_code, locality_name) %>%
  distinct(mb_2016_code, .keep_all = TRUE) %>%
  filter(!is.na(mb_2016_code))

# 3. JOIN TRANSACTIONS WITH LOCATION
data_joined <- trans_df %>%
  filter(price > 0) %>%
  # Join using the Mesh Block Code
  inner_join(location_mapping, by = "mb_2016_code")

# Check if it worked by printing the top rows
print(head(data_joined))

# 4. CONTINUE WITH CLUSTERING (Same as before)
suburb_features <- data_joined %>%
  group_by(locality_name) %>%
  summarise(
    Median_Price = median(price, na.rm = TRUE),
    Transaction_Volume = n(),
    Volatility = sd(price, na.rm = TRUE) / mean(price, na.rm = TRUE)
  ) %>%
  filter(Transaction_Volume > 50, !is.na(Volatility))

# Scale and Cluster
suburb_scaled <- suburb_features %>%
  select(Median_Price, Transaction_Volume, Volatility) %>%
  scale()

set.seed(123)
km_res <- kmeans(suburb_scaled, centers = 4, nstart = 25)

suburb_features <- suburb_features %>%
  mutate(Cluster = as.factor(km_res$cluster))

# Plot
ggplot(suburb_features, aes(x = Volatility, y = Median_Price, color = Cluster)) +
  geom_point(alpha = 0.6, size = 3) +
  scale_y_continuous(labels = dollar_format(), trans = "log10") +
  scale_x_continuous(labels = percent_format()) +
  labs(
    title = "Suburb Investment Personas",
    x = "Risk (Price Volatility)",
    y = "Entry Price (Log Scale)",
    color = "Market Type"
  ) +
  theme_minimal()