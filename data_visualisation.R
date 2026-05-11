library(tidyverse)
library(lubridate)
library(stringr)

logged_data <- read_csv(
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vTb4rR1s1h0Mr-yIQi2uQkB5QRxBCqUDA53fMHmO0kqXEurtaEqvVDd3xU0MIuYyQ0YPD0C9AAmGCRe/pub?output=csv"
)

latest_data <- logged_data %>%
  rename(
    type = `What type of ad is this?`,
    interest = `How interested are you in this ad? (1-10)`,
    click = `Did you click on the ad?`,
    time = `What time did you see this ad?`,
    related = `Was this ad related to something I recently searched for?`
  ) %>%
  mutate(
    interest = as.numeric(interest),
    Timestamp = ymd_hms(`时间戳记`),
    click = str_to_title(click),
    type = str_to_title(type),
    time = str_to_title(time)
  )

average_interest <- mean(latest_data$interest, na.rm = TRUE)

highest_interest <- max(latest_data$interest, na.rm = TRUE)

glimpse(latest_data)

# Plot 1

ad_count <- latest_data %>%
  group_by(type) %>%
  summarise(total = n()) %>%
  arrange(desc(total))

plot1 <- ggplot(
  ad_count,
  aes(
    x = reorder(type, total),
    y = total,
    fill = type
  )
) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "PuBu") +
  labs(
    title = "Most Common Instagram Advertisement Categories",
    x = "Advertisement Type",
    y = "Number of Advertisements",
    caption = "Data collected from Instagram browsing observations"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 18,
      face = "bold"
    ),
    axis.title = element_text(size = 13),
    plot.caption = element_text(
      size = 10,
      colour = "grey40"
    ),
    legend.position = "none"
  )

plot1

ggsave(
  "plot1.png",
  plot = plot1,
  width = 8,
  height = 5
)

# Plot 2

interest_time <- latest_data %>%
  group_by(time) %>%
  summarise(
    avg_interest = mean(interest, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_interest))

plot2 <- ggplot(
  interest_time,
  aes(
    x = reorder(time, avg_interest),
    y = avg_interest,
    fill = time
  )
) +
  geom_col() +
  scale_fill_brewer(palette = "PuBu") +
  labs(
    title = "Average Advertisement Interest by Time of Day",
    x = "Time of Day",
    y = "Average Interest Level",
    caption = "Average interest scores across observation periods"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 18,
      face = "bold"
    ),
    axis.title = element_text(size = 13),
    plot.caption = element_text(
      size = 10,
      colour = "grey40"
    ),
    legend.position = "none"
  )

plot2

ggsave(
  "plot2.png",
  plot = plot2,
  width = 8,
  height = 5
)

# Plot 3

plot3 <- ggplot(
  latest_data,
  aes(
    x = type,
    y = interest,
    fill = type
  )
) +
  geom_boxplot() +
  scale_fill_brewer(palette = "PuBu") +
  labs(
    title = "Interest Levels Across Advertisement Categories",
    x = "Advertisement Type",
    y = "Interest Level",
    caption = "Distribution of advertisement interest scores"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      size = 18,
      face = "bold"
    ),
    axis.title = element_text(size = 13),
    plot.caption = element_text(
      size = 10,
      colour = "grey40"
    ),
    legend.position = "none"
  )

plot3

ggsave(
  "plot3.png",
  plot = plot3,
  width = 8,
  height = 5
)
