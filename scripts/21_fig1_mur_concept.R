# =============================================================================
# 21_fig1_mur_concept.R
# Purpose: Generate Figure 1 — conceptual illustration of the MUR metric
# Shows stacked bars for selected conditions spanning the full MUR range,
# with "reported (underlying cause)" vs "hidden (contributing only)" portions
# =============================================================================

library(tidyverse)

dir.create("outputs/figures", showWarnings = FALSE)

# --- Data: selected conditions spanning the full MUR range ---
# Values from our analysis (ABS Cube 10, 2023)

fig_data <- tibble(
  condition = c(
    "Neoplasms\n(cancers)",
    "External causes\n(suicide, assault, accidents)",
    "Circulatory system\n(heart disease, stroke)",
    "Essential\nhypertension",
    "Respiratory failure\nNEC"
  ),
  mur = c(1.5, 1.9, 4.1, 27.7, 82.4),
  # Chapter-level counts from our data
  underlying = c(52819, 13307, 42298, NA, 92),
  multiple   = c(76987, 24664, 173121, NA, 7582)
)

# For hypertension (I10), compute from MUR and known underlying
# Underlying = 833 (2024 Cube 14) but Cube 10 (2023) figure is the basis for MUR 27.7
# Reverse-engineer: if MUR = 27.7 and we know it from Cube 10, use the ratio
fig_data <- fig_data %>%
  mutate(
    # Compute percentages from MUR (cleaner than raw counts for a conceptual figure)
    pct_underlying = 1 / mur * 100,
    pct_hidden     = (1 - 1 / mur) * 100,
    # Labels
    mur_label = paste0("MUR = ", format(mur, nsmall = 1)),
    hidden_label = paste0(round(pct_hidden), "% hidden"),
    condition = factor(condition, levels = condition)
  )

# Reshape for stacked bar
fig_long <- fig_data %>%
  select(condition, mur, mur_label, hidden_label, pct_underlying, pct_hidden) %>%
  pivot_longer(
    cols = c(pct_underlying, pct_hidden),
    names_to = "type",
    values_to = "pct"
  ) %>%
  mutate(
    type = factor(
      ifelse(type == "pct_underlying",
             "Reported as underlying cause",
             "Hidden (contributing only)"),
      levels = c("Hidden (contributing only)",
                 "Reported as underlying cause")
    )
  )

# --- Colour palette ---
col_reported <- "#2166AC"   # dark blue — visible
col_hidden   <- "#D6604D"   # muted red — hidden

# --- Plot ---
p <- ggplot(fig_long, aes(x = condition, y = pct, fill = type)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.3) +
  # MUR label above each bar
  geom_text(
    data = fig_data,
    aes(x = condition, y = 103, label = mur_label),
    inherit.aes = FALSE,
    size = 3.5, fontface = "bold", colour = "grey20"
  ) +
  # "X% hidden" label inside the hidden (red) portion
  geom_text(
    data = fig_data %>% filter(pct_hidden > 15),
    aes(x = condition, y = pct_hidden / 2 + pct_underlying, label = hidden_label),
    inherit.aes = FALSE,
    size = 3.2, colour = "white", fontface = "bold"
  ) +
  # For low-hidden conditions, put label outside
  geom_text(
    data = fig_data %>% filter(pct_hidden <= 15),
    aes(x = condition, y = pct_hidden + pct_underlying + 0.5,
        label = paste0(round(pct_hidden), "% hidden")),
    inherit.aes = FALSE,
    size = 3.0, colour = col_hidden, fontface = "bold",
    vjust = -0.3
  ) +
  scale_fill_manual(
    values = c(
      "Reported as underlying cause" = col_reported,
      "Hidden (contributing only)"   = col_hidden
    ),
    name = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 112),
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "The Multiple-to-Underlying Cause Ratio (MUR)",
    subtitle = "Proportion of death certificate mentions visible to standard mortality statistics",
    x = NULL,
    y = "Percentage of death certificate mentions",
    caption = "Data: ABS Causes of Death 2023 (Cube 10). Each bar represents 100% of death certificate\nmentions for that condition. Blue = counted in underlying cause statistics; red = hidden."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 11, colour = "grey40", hjust = 0),
    plot.caption = element_text(size = 8.5, colour = "grey50", hjust = 0),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 9.5, lineheight = 1.1),
    axis.text.y = element_text(size = 10),
    plot.margin = margin(10, 15, 10, 10)
  )

ggsave("outputs/figures/fig01_mur_concept.png", p,
       width = 10, height = 6, dpi = 300, bg = "white")

cat("Figure 1 saved: outputs/figures/fig01_mur_concept.png\n")
cat("\nConditions shown:\n")
fig_data %>%
  select(condition, mur, pct_underlying, pct_hidden) %>%
  mutate(across(where(is.numeric), ~ round(., 1))) %>%
  print(n = 5)
