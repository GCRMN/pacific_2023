# 1. Required packages ----

require(extrafont) # For fonts

# 2. Set the default font family ----

windowsFonts("Open Sans" = windowsFont("Open Sans"))

font_choose_graph <- "Open Sans"
font_choose_map <- "Open Sans"

# 3. Set the colors ----

col_fill_map <- "#f2caae"
col_color_map <- "#888888"
col_background_map <- "#e4f1fe"

# Derived from scico package and lajolla palette
# scico(5, begin = 0, end = 0.9, palette = "lajolla", direction = -1)
palette_5cols <- c("#2c82c9", "#848ccf", "#E79652", "#C9504B", "#5B3023")

# 4. Define a common crs ----

crs_selected <- "+proj=eqearth"