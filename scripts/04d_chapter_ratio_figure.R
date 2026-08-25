# =============================================================================
# 04d_chapter_ratio_figure.R
# Purpose: Generate the chapter-level multiple-to-underlying ratio figure
#          (Supplementary Figure S1). The figure previously embedded at S1
#          (fig1_hidden_burden_ratio.png) plotted individual three-character
#          conditions, not ICD-10 chapters, contradicting its caption and the
#          "Chapter-level ratios" Results paragraph. This script produces the
#          chapter-level figure the text actually describes.
# INPUT:  outputs/exploratory/cube10_chapter_mur_summary.csv
# OUTPUT: outputs/figures/fig_chapter_ratio.png
# Run AFTER the chapter summary CSV has been generated.
# =============================================================================

library(tidyverse)
library(scales)

dir.create("outputs/figures", showWarnings = FALSE)

chapters <- read_csv("outputs/exploratory/cube10_chapter_mur_summary.csv",
                     show_col_types = FALSE) %>%
  filter(!is.na(mur_persons), is.finite(mur_persons)) %>%
  mutate(
    # "CHAPTER VII Diseases of the eye and adnexa (H00-H59)" -> "Diseases of the eye and adnexa (H00-H59)"
    chapter_label = str_remove(cause, "^CHAPTER\\s+[IVXL]+\\s+"),
    # small-denominator flag: chapters resting on < 20 underlying deaths are unstable
    unstable = underlying_persons < 20,
    label_txt = ifelse(unstable,
                       sprintf("%.1f*", mur_persons),
                       sprintf("%.1f", mur_persons))
  ) %>%
  arrange(desc(mur_persons))

any_unstable <- any(chapters$unstable)
cap <- paste0("Source: ABS Causes of Death 2023, Data Cube 10. ",
              "Ratio = total certificate mentions / underlying cause deaths, persons.",
              if (any_unstable) "\n* Chapter based on fewer than 20 underlying cause deaths; ratio unstable." else "")

p <- ggplot(chapters, aes(x = reorder(chapter_label, mur_persons), y = mur_persons)) +
  geom_col(fill = "#2166AC", alpha = 0.85) +
  geom_text(aes(label = label_txt), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Multiple-to-Underlying Ratio by ICD-10 Chapter",
    subtitle = "How many times more often a chapter's conditions appear on death certificates\nthan they are selected as the underlying cause, Australia 2023",
    x = NULL,
    y = "Multiple-to-Underlying Cause Ratio (persons)",
    caption = cap
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, colour = "grey40"),
    plot.caption = element_text(size = 7, colour = "grey50"),
    panel.grid.major.y = element_blank()
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

ggsave("outputs/figures/fig_chapter_ratio.png", p, width = 12, height = 8, dpi = 300)
cat("  Saved: outputs/figures/fig_chapter_ratio.png\n")
cat(sprintf("  Chapters plotted: %d (highest %.1f, lowest %.1f)\n",
            nrow(chapters), max(chapters$mur_persons), min(chapters$mur_persons)))
