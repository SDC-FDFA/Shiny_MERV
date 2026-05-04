
draw_plot_categories <- function(x, main_c, cat_labels, cat_min, cat_max, cat_color) {
  library(ggfittext)
  
  indicator_value <- x |> 
      filter(year == max(year)) |> 
      filter(code == main_c) |> 
      pull(round(value, digits = 2))
  
  if (length(indicator_value) == 0 || all(is.na(indicator_value))) {
    ggplot() +
      annotate("text", x = 0.5, y = 0.5,
               label = "No category data availble",
               size = 3.5, color = "grey50", hjust = 0.5) +
      theme_void()
  } else {
    categories <- data.frame(
    label = cat_labels,
    min   = cat_min,
    max   = cat_max,
    color = cat_color
  )
  
  min_lim <- min(cat_min)
  max_lim <- max(cat_max)
  
  # Identify which category the value falls in
  categories$active <- indicator_value >= categories$min & 
    indicator_value <  categories$max
  # Edge case: clamp to last bucket if value == max
  if (!any(categories$active)) categories$active[nrow(categories)] <- TRUE
  
  ggplot(categories) +
    # Colored segments
    geom_rect(aes(xmin = min, xmax = max, ymin = 0, ymax = 0.25, fill = color),
              color = "white", linewidth = 1.2) +
    # Dim non-active segments
    geom_rect(aes(xmin = min, xmax = max, ymin = 0, ymax = 0.25,
                  alpha = ifelse(active, 0, 0.55)),
              fill = "white") +
    # Category labels inside bars
    geom_fit_text(aes(xmin = min, xmax = max, ymin = 0, ymax = 0.25, label = label),
                  reflow = TRUE, fontface = "bold", color = "white", 
                  min.size = 3, max.size = 7) +
    # geom_text(aes(x = (min + max) / 2, y = 0.12, label = label),
    #           fontface = "bold", size = 3.5, color = "white") +
    # Value marker (triangle needle)
    annotate("point", x = indicator_value, y = 0.3,
             shape = 25, size = 0.8, fill = "black", color = "black") +
    # Value label above marker
    annotate("text", x = indicator_value, y = 0.4,
             label = paste0("Value: ", round(indicator_value, digits = 2)),
             fontface = "bold", size = 3, hjust = 0.5) +
    scale_fill_identity() +
    scale_alpha_identity() +
    scale_x_continuous(breaks = c(categories$min, max(categories$max)),
                       limits = c(min_lim, max_lim)) +
    coord_cartesian(ylim = c(0, 0.47), clip = "off") +
    theme_void() +
    theme(
      axis.text.x  = element_text(size = 8, color = "grey40"),
      axis.ticks.x = element_line(color = "grey60"),
      axis.ticks.length = unit(3, "pt"),
      plot.margin  = margin(t = 0, r = 10, b = 0, l = 10)
    )
  }
}

# Version without a value label
draw_plot_categories_noval <- function(x, main_c, cat_labels, cat_min, cat_max, cat_color) {
  library(ggfittext)
  
  indicator_value <- x |> 
    filter(year == max(year)) |> 
    filter(code == main_c) |> 
    pull(round(value, digits = 2))
  
  if (length(indicator_value) == 0 || all(is.na(indicator_value))) {
    ggplot() +
      annotate("text", x = 0.5, y = 0.5,
               label = "No category data availble",
               size = 3.5, color = "grey50", hjust = 0.5) +
      theme_void()
  } else {
    
  categories <- data.frame(
    label = cat_labels,
    min   = cat_min,
    max   = cat_max,
    color = cat_color
  )
  
  min_lim <- min(cat_min)
  max_lim <- max(cat_max)
  
  # Identify which category the value falls in
  categories$active <- indicator_value >= categories$min & 
    indicator_value <  categories$max
  # Edge case: clamp to last bucket if value == max
  if (!any(categories$active)) categories$active[nrow(categories)] <- TRUE
  
  ggplot(categories) +
    # Colored segments
    geom_rect(aes(xmin = min, xmax = max, ymin = 0, ymax = 0.25, fill = color),
              color = "white", linewidth = 1.2) +
    # Dim non-active segments
    geom_rect(aes(xmin = min, xmax = max, ymin = 0, ymax = 0.25,
                  alpha = ifelse(active, 0, 0.55)),
              fill = "white") +
    # Category labels inside bars
    geom_fit_text(aes(xmin = min, xmax = max, ymin = 0, ymax = 0.25, label = label),
                  reflow = TRUE, fontface = "bold", color = "white", 
                  min.size = 3, max.size = 7) +
    # geom_text(aes(x = (min + max) / 2, y = 0.12, label = label),
    #           fontface = "bold", size = 3.5, color = "white") +
    # Value marker (triangle needle)
    annotate("point", x = indicator_value, y = 0.3,
             shape = 25, size = 0.8, fill = "black", color = "black") +
    # Value label above marker
    # annotate("text", x = indicator_value, y = 0.4,
    #          label = paste0("Value: ", round(indicator_value, digits = 2)),
    #          fontface = "bold", size = 3, hjust = 0.5) +
    scale_fill_identity() +
    scale_alpha_identity() +
    scale_x_continuous(breaks = c(categories$min, max(categories$max)),
                       limits = c(min_lim, max_lim)) +
    coord_cartesian(ylim = c(0, 0.47), clip = "off") +
    theme_void() +
    theme(
      axis.text.x  = element_text(size = 8, color = "grey40"),
      axis.ticks.x = element_line(color = "grey60"),
      axis.ticks.length = unit(3, "pt"),
      plot.margin  = margin(t = 0, r = 10, b = 0, l = 10)
    )
  }
}