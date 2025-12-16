# Load required libraries ------------------------------------------------------
# readxl      : read Excel files
# ggplot2     : plotting framework
# ggalluvial  : build alluvial/sankey-like plots
# dplyr       : data manipulation
# ggrepel     : repel overlapping text labels
# tidyverse   : broader data science toolkit
# ggpp/ggtext : enhanced text/annotation tools
library(readxl)
library(ggplot2)
library(ggalluvial)
library(dplyr)
library(ggrepel)
library(tidyverse)
library(ggpp)
library(ggtext)

# Define working directory
setwd("DEFINE_PATH")

# Example data structure -------------------------------------------------------
# This tibble mimics a 1-to-1 linkage dataset between:
#   Primary Activity → Grouped Code → Pressure Category → Pressure
# and aesthetic attributes controlling color/alpha/font. NOTE: The linkages are not meaningful and are completely random!
df1to1 <- tibble(
  `Primary Activity Type` = sample(c("Exogenous/Unmanaged (e.g. due to climate change)", "Fishing", "Tourism/ Recreation and Non-Commercial Harvesting"), 30, replace = TRUE),
  `Grouped Code` = sample(c("Emission", "Litter", "Fish_Bent", "Climate", "Cruise"), 30, replace = TRUE),
  `Pressure Category` = sample(c("Chemical changes, chemicals and other pollutants", "Biological disturbance", "Energy", "Physical change", "Exogenous/Unmanaged processes"), 30, replace = TRUE),
  Pressure = sample(c("Introduction of Non-synthetic compounds", "Introduction of Synthetic compounds", "N&P Enrichment", "Litter", "Extraction of flora and/or fauna"), 30, replace = TRUE),
  stratum_fill_color = sample(c("#2fc256", "#EACB2B", "#F5191C"), 30, replace = TRUE),
  transparency = sample(c(0.7), 30, replace = TRUE),
)

df1to1$font_format <- ifelse(df1to1$Pressure %in% c("Litter", "Extraction of flora and/or fauna"), "bold", "plain")

# Figure 4 — Sankey diagram representing the linkage framework of activities  and resulting pressures -------------------------------------------------
# Alluvial diagram showing connectivity from Primary Activity → Grouped Code → Pressure Category → Pressure.
ggplot(
  df1to1,
  aes(
    y     = 1,                         # constant height; flow thickness not used
    axis1 = `Primary Activity Type`,
    axis2 = `Grouped Code`,
    axis3 = `Pressure Category`,
    axis4 = Pressure
  )
) +
  # Flow ribbons ---------------------------------------------------------------
geom_alluvium(
  aes(fill = stratum_fill_color, alpha = transparency),
  aes.bind   = "alluvia",
  curve_type = "arctangent"
) +
  scale_fill_identity() +
  
  # Stratum blocks -------------------------------------------------------------
geom_stratum(fill = "white") +
  
  # Label: axis 1 (Primary Activity Type) --------------------------------------
geom_text_repel(
  aes(label = ifelse(after_stat(x) == 1,
                     stringr::str_wrap(after_stat(stratum), 15), NA)),
  stat             = "stratum",
  size             = 3.5,
  min.y            = -500,
  max.y            = 500,
  direction        = "y",
  position         = position_nudge_keep(x = -0.3),
  force            = 0.5,
  hjust            = "right",
  segment.size     = 0.2,
  segment.curvature= 0.1,
  color            = "grey30",
  bg.color         = "white",
  segment.color    = "black",
  bg.r             = 0.18
) +
  
  # Label: axis 2 (Grouped Code) ----------------------------------------------
geom_text_repel(
  aes(label = ifelse(after_stat(x) == 2,
                     stringr::str_wrap(after_stat(stratum), 25), NA)),
  stat         = "stratum",
  size         = 3.5,
  min.y        = -500,
  max.y        = 500,
  direction    = "y",
  position     = position_nudge_keep(y = 10, x = -0.3),
  force        = 0.5,
  hjust        = "right",
  segment.size = 0.2,
  segment.curvature = 0.1,
  color        = "grey30",
  bg.color     = "white",
  segment.color= "black",
  bg.r         = 0.18
) +
  
  # Label: axis 3 (Pressure Category) -----------------------------------------
# Replace "/" with line break for readability.
geom_text_repel(
  aes(label = ifelse(after_stat(x) == 3,
                     stringr::str_wrap(gsub("/", "/\n", after_stat(stratum)), 10), NA)),
  stat         = "stratum",
  size         = 3.5,
  min.y        = -500,
  max.y        = 500,
  direction    = "y",
  position     = position_nudge_keep(y = 5, x = -0.3),
  force        = 0.5,
  hjust        = "right",
  segment.size = 0.2,
  segment.curvature = 0.1,
  color        = "grey30",
  bg.color     = "white",
  segment.color= "black",
  bg.r         = 0.18
) +
  
  # Label: axis 4 (Pressure) ---------------------------------------------------
geom_text_repel(
  aes(label = ifelse(after_stat(x) == 4, 
                     as.character(after_stat(stratum)), NA), fontface = font_format),
  stat = "stratum", size = 3.5, 
  min.y =  -500, max.y = 500,
  direction = "y", 
  position = position_nudge_keep(y =10, x = .5),
  force             = 0.5,
  hjust = "left",
  segment.size      = 0.3,
  segment.curvature = -0.1,
  color = "grey30",     # text color
  bg.color = "white", # shadow color
  segment.color = "black",
  bg.r = 0.18         # shadow radius
) +
  
  # Axes and layout ------------------------------------------------------------
scale_x_continuous(
  breaks = 1:4,
  labels = c(
    "Primary\nActivity Type",
    "Grouped\nPrimary Activity",
    "Pressure\nCategory",
    "Pressure"
  ),
  position = "top",
  expand   = expansion(mult = 0.5)
) +
  scale_y_continuous(expand = expansion(mult = 0.05)) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title.y    = element_blank(),
    axis.text.y     = element_blank(),
    axis.ticks.y    = element_blank(),
    axis.line       = element_blank(),
    axis.ticks.x    = element_blank(),
    plot.margin     = margin(0.1, 0.5, 0.1, 0.1, "cm"),
    axis.text.x     = element_text(size = 13, face = "bold")
  )

# Export figure
ggsave("Figure4_LinkageFramework.png", width = 12, height = 7, dpi = 300)

