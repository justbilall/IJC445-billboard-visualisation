# IJC445 Data Visualisation
# Script 02: Data Processing & Visualisations

library(tidyverse)
library(tidytext)
library(showtext)
library(viridis)


# Load raw data
df <- read.csv("data/billboard_24years_lyrics_spotify.csv")

# Minimal cleaning + features
df_clean <- df %>%
  filter(!is.na(lyrics)) %>%
  mutate(
    word_count = str_count(lyrics, "\\S+"),
    year = as.numeric(year)
  )

# Identify Top 8 Super-Artists
top_artists <- df_clean %>%
  count(band_singer, sort = TRUE) %>%
  slice_head(n = 8) %>%
  pull(band_singer)

# FIGURE 1: Market Share (Area Chart)
chart_a_data <- df_clean %>%
  mutate(group = if_else(
    band_singer %in% top_artists,
    "Top 8 Super-Artists",
    "All Other Artists"
  )) %>%
  group_by(year, group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(percentage = n / sum(n) * 100)

plot_a <- ggplot(chart_a_data, aes(x = year, y = percentage, fill = group)) +
  geom_area(alpha = 0.8, colour = "white") +
  scale_fill_manual(values = c("#2c3e50", "#cccccc")) +
  labs(
    title = "Figure 1: Market Share of Super-Artists (2000–2023)",
    subtitle = "Percentage of Billboard Hot-100 hits captured by the top 8 artists",
    x = "Year",
    y = "% of Chart Hits",
    fill = "Artist Group"
  ) +
  theme_minimal()

# FIGURE 2: Ranking Distributions (Boxplot)
plot_b <- df_clean %>%
  filter(band_singer %in% top_artists) %>%
  ggplot(aes(
    x = reorder(band_singer, ranking, FUN = median),
    y = ranking,
    fill = band_singer
  )) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.25, size = 1) +
  scale_y_reverse(breaks = c(1, 25, 50, 75, 100)) +
  coord_flip() +
  scale_fill_viridis_d(option = "mako", guide = "none") +
  labs(
    title = "Figure 2: Distribution of Song Rankings",
    subtitle = "Chart performance consistency of Super-Artists",
    x = "Artist",
    y = "Billboard Rank (1 = Best)"
  ) +
  theme_minimal()

# FIGURE 3: Lyrical Density vs Success (Scatter)
plot_c <- ggplot(df_clean, aes(x = word_count, y = ranking)) +
  geom_point(alpha = 0.3, colour = "#3498db") +
  geom_smooth(method = "lm", colour = "#e74c3c", se = TRUE) +
  scale_y_reverse() +
  coord_cartesian(xlim = c(0, 1500)) +
  labs(
    title = "Figure 3: Lyrical Word Count vs Chart Success",
    subtitle = "Examining the relationship between lyrical density and Billboard ranking",
    x = "Word Count (per song)",
    y = "Billboard Rank (1 = Best)"
  ) +
  theme_minimal()

# FIGURE 4: Career Longevity (Bubble Chart)
chart_d_data <- df_clean %>%
  group_by(band_singer) %>%
  summarise(
    first_year = min(year),
    avg_rank = mean(ranking, na.rm = TRUE),
    total_hits = n(),
    .groups = "drop"
  ) %>%
  filter(total_hits > 5)

plot_d <- ggplot(chart_d_data, aes(
  x = first_year,
  y = avg_rank,
  size = total_hits
)) +
  geom_point(alpha = 0.5, colour = "#27ae60") +
  scale_size(range = c(2, 12)) +
  scale_y_reverse() +
  labs(
    title = "Figure 4: The Longevity of Top-Tier Talent",
    subtitle = "Artist entry year vs average career ranking",
    x = "Year of First Hit",
    y = "Average Career Rank (1 = Best)",
    size = "Total Hits"
  ) +
  theme_minimal()

# Save figures
ggsave("figures/figure1_market_share.png", plot_a, width = 8, height = 5, dpi = 300)
ggsave("figures/figure2_rank_distribution.png", plot_b, width = 8, height = 5, dpi = 300)
ggsave("figures/figure3_lyrics_vs_rank.png", plot_c, width = 8, height = 5, dpi = 300)
ggsave("figures/figure4_career_longevity.png", plot_d, width = 8, height = 5, dpi = 300)

print("All figures saved successfully.")