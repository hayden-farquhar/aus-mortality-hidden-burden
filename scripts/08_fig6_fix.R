# ============================================================================
# Script 08: Figure 6 Fix — State-level Hospital vs Mortality Scatter
# 
# Problem: Mental health and Infectious diseases are dropped from the scatter 
# plot because proc_seps is NA, and ggplot silently removes rows with NA 
# in the size aesthetic.
#
# Fix: Use a two-layer approach — one for domains with procedure data (sized 
# points), one for domains without (fixed-size hollow points).
# ============================================================================

library(tidyverse)

# --- Load data ---------------------------------------------------------------
fig6_data <- read_csv("fig6_data.csv", show_col_types = FALSE)

# --- Inspect the NA cases ----------------------------------------------------
cat("Domains with missing procedure data:\n")
fig6_data %>% filter(is.na(proc_seps)) %>% select(clinical_domain, diag_seps, underlying_deaths) %>% print()

# --- Create the fixed plot ---------------------------------------------------

# Split data into domains with and without procedure data
has_proc   <- fig6_data %>% filter(!is.na(proc_seps))
no_proc    <- fig6_data %>% filter(is.na(proc_seps))

# Calculate axis medians for quadrant lines (using all domains with data)
med_diag  <- median(has_proc$diag_seps, na.rm = TRUE)
med_death <- median(has_proc$underlying_deaths, na.rm = TRUE)

p8 <- ggplot() +
  # Layer 1: Domains WITH procedure data — sized by procedure separations
  geom_point(
    data = has_proc,
    aes(x = diag_seps, y = underlying_deaths, size = proc_seps, colour = clinical_domain),
    alpha = 0.8
  ) +
  # Layer 2: Domains WITHOUT procedure data — fixed size, hollow triangle
  geom_point(
    data = no_proc,
    aes(x = diag_seps, y = underlying_deaths, colour = clinical_domain),
    shape = 17, size = 4, alpha = 0.8
  ) +
  # Labels for all domains
  geom_text(
    data = fig6_data,
    aes(x = diag_seps, y = underlying_deaths, label = clinical_domain),
    hjust = -0.15, vjust = -0.5, size = 3
  ) +
  # Quadrant reference lines
  geom_vline(xintercept = med_diag, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = med_death, linetype = "dashed", alpha = 0.3) +
  # Scales
  scale_size_continuous(
    name = "Procedure\nseparations",
    labels = scales::comma,
    range = c(3, 15)
  ) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  scale_colour_brewer(palette = "Set3", guide = "none") +
  # Labels
  labs(
    title = "Hospital Burden vs Mortality by Clinical Domain (2024)",
    subtitle = "Triangles = no mapped procedure chapter (Mental health, Infectious diseases)",
    x = "Diagnosis separations (hospital admissions)",
    y = "Underlying cause deaths",
    caption = "Data: AIHW Hospital Data Cubes; ABS Causes of Death 2023"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    legend.position = "right"
  )

# --- Save --------------------------------------------------------------------
ggsave("outputs/figures/fig8_hospital_vs_mortality_fixed.png", p8,
       width = 12, height = 8, dpi = 300, bg = "white")

cat("\n✓ Figure 8 saved: outputs/figures/fig8_hospital_vs_mortality_fixed.png\n")
cat("  Now includes Mental health and Infectious diseases as triangles.\n")
