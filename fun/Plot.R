

draw_plot <- function(x, main_c, nyears, full_title, full_caption) {
    x <- x |>
      mutate(
        linewidth  = if_else(code == main_c, 1.2, 0.4),
        line_color = if_else(code == main_c, "#519796", "#cec8c8"),
        line_alpha = if_else(code == main_c, 1, 0.6),
        value = round(value, digits = 2)
      )
    
    country_name <- x |> 
      filter(year == max(year)) |> 
      filter(code == main_c) |> 
      pull(country)
    
    ggplot(x , aes(x = year, y = value, group = country)) +
      geom_line(aes(color = line_color, size = linewidth, alpha = line_alpha)) +
      scale_color_identity() +
      scale_size_identity() +
      scale_alpha_identity() +
      geom_text_repel(
        data = filter(x, year == max(year)),
        aes(
          label    = if_else(code == main_c,
                             paste0(country, ": ", value),
                             paste0(country, ": ", value)), 
          color    = line_color,
          fontface = if_else(code == main_c, "bold", "plain")
        ),
        size          = 9 / ggplot2::.pt,
        show.legend   = FALSE,
        direction     = "y",
        hjust         = 0,
        segment.color = "#cccccc",
        nudge_x       = 0.5
      ) +
      labs(
        title    = str_wrap(full_title, width = 60),
        subtitle = paste0(country_name, " highlighted"), 
        caption  = paste0("Source: ", full_caption)
      ) +
      scale_x_continuous(
        breaks = scales::breaks_pretty(n = nyears),
        limits = c(NA, max(x$year) + 2) ) +
      scale_y_continuous(
        expand = expansion(c(0.1, 0.1)),
        breaks = scales::breaks_pretty(n = 4),
        labels = scales::label_number(scale_cut = scales::cut_short_scale())
      ) +
      coord_cartesian(clip = "off") +
      theme(
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(colour = "white", fill = "white"),
        axis.text.x = element_text(size = 8, family = "arial"),
        axis.text.y = element_text(size = 8, family = "arial"),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size = 8, family = "arial"),
        strip.text = element_text(size = 10, face = "bold"),
        title = element_text(size = 9, family = "arial", face = "bold"),
        plot.caption = element_text(size = 7, 
                                    family = "arial", 
                                    face = "plain", 
                                    color = "#888888",
                                    margin = margin(t = 0, r = 0, b = 0, l = 0)),
        legend.position = "none",
        legend.title = element_blank(),
        legend.box = element_blank(),
        panel.grid.major.y = element_line(color = "#f0f0f0", linewidth = 0.3),
        plot.title         = element_text(size = 10, face = "bold"),
        plot.subtitle      = element_text(size = 8, color = "#555555"),
        plot.margin        = margin(5, 30, 0, 10)
      )
  }

draw_plot_girafe <- function(x, main_c, nyears, full_title, full_caption) {
  x <- x |>
    mutate(
      linewidth  = if_else(code == main_c, 1.2, 0.4),
      line_color = if_else(code == main_c, "#519796", "#cec8c8"),
      line_alpha = if_else(code == main_c, 1, 0.6),
      value      = round(value, digits = 2),
      # Build the tooltip string per row
      tooltip    = paste0("<b>", country, "</b><br>Year: ", year, "<br>Value: ", value)
    )
  
  country_name <- x |>
    filter(year == max(year), code == main_c) |>
    pull(country)
  
  p <- ggplot(x, aes(x = year, y = value, group = country)) +
    geom_line_interactive(
      aes(
        color   = line_color,
        size    = linewidth,
        alpha   = line_alpha,
        data_id = country       # still needed for the dim-others effect
      )
    ) +
    geom_point_interactive(
      aes(
        tooltip = paste0("<b>", country, "</b><br>Year: ", year, "<br>Value: ", value),
        data_id = country,      # same data_id links hover state to the line
        color   = line_color
      ),
      size  = 2,    # small but hittable
      alpha = 0     # invisible — only the tooltip matters
    ) +
    scale_color_identity() +
    scale_size_identity() +
    scale_alpha_identity() +
    # geom_text_repel is NOT interactive-capable; use plain geom_text or a
    # geom_label_interactive for the highlighted country, repel for others
    geom_text_repel(
      data = filter(x, year == max(year), code != main_c),
      aes(label = paste0(country, ": ", value), color = line_color),
      size          = 9 / ggplot2::.pt,
      show.legend   = FALSE,
      direction     = "y",
      hjust         = 0,
      segment.color = "#cccccc",
      nudge_x       = 0.5,
      fontface      = "plain"
    ) +
    # ▶ interactive label for the highlighted country
    geom_label_interactive(
      data = filter(x, year == max(year), code == main_c),
      aes(
        label   = paste0(country, ": ", value),
        tooltip = paste0("<b>", country, "</b><br>Value: ", value),
        data_id = country
      ),
      color    = "#519796",
      fontface = "bold",
      size     = 9 / ggplot2::.pt,
      hjust    = 0,
      nudge_x  = 0.5,
      fill     = NA,
      label.size = 0   # remove label box border
    ) +
    labs(
      title    = str_wrap(full_title, width = 60),
      subtitle = paste0(country_name, " highlighted"),
      caption  = paste0("Source: ", full_caption)
    ) +
    scale_x_continuous(
      breaks = scales::breaks_pretty(n = nyears),
      limits = c(NA, max(x$year) + 2)
    ) +
    scale_y_continuous(
      expand = expansion(c(0.1, 0.1)),
      breaks = scales::breaks_pretty(n = 4),
      labels = scales::label_number(scale_cut = scales::cut_short_scale())
    ) +
    coord_cartesian(clip = "off") +
    theme(
      panel.background   = element_rect(fill = "white"),
      panel.grid.major.x = element_blank(),
      axis.ticks         = element_blank(),
      strip.background   = element_rect(colour = "white", fill = "white"),
      axis.text.x        = element_text(size = 8, family = "arial"),
      axis.text.y        = element_text(size = 8, family = "arial"),
      axis.line.x        = element_line(colour = "black"),
      axis.line.y        = element_line(colour = "black"),
      axis.title.x       = element_blank(),
      axis.title.y       = element_blank(),
      text               = element_text(size = 8, family = "arial"),
      strip.text         = element_text(size = 10, face = "bold"),
      title              = element_text(size = 9, family = "arial", face = "bold"),
      plot.caption       = element_text(
        size = 7, family = "arial", face = "plain", color = "#888888",
        margin = margin(t = 0, r = 0, b = 0, l = 0)
      ),
      legend.position    = "none",
      legend.title       = element_blank(),
      legend.box         = element_blank(),
      panel.grid.major.y = element_line(color = "#f0f0f0", linewidth = 0.3),
      plot.title         = element_text(size = 10, face = "bold"),
      plot.subtitle      = element_text(size = 8, color = "#555555"),
      plot.margin        = margin(5, 30, 0, 10)
    )
  
  # Wrap in girafe for Shiny
  girafe(
    ggobj = p,
    width_svg = 9,
    height_svg = 3,
    options = list(
      opts_hover(css = "stroke-width: 2.5; opacity: 1;"),
      opts_hover_inv(css = "opacity: 0.15;"),   # dim non-hovered lines
      opts_tooltip(
        css = "background:#fff; border:1px solid #ccc; padding:6px 10px;
               border-radius:4px; font-family:arial; font-size:12px;",
        use_fill = TRUE
      ),
      opts_sizing(rescale = TRUE)
    )
  )
}


# draw_plot_backup <- function(x, main_c, nyears, full_title, full_caption) {
#   #message("main_c: '", main_c, "'; countries: ", paste(unique(x$country), collapse = ", "))
#   
#   x <- x |>
#     mutate(linewidth = if_else(code == main_c, 2, 0.5)
#         #   line_color = if_else(code == main_c, "#60b3b1", "#dad8d8") 
#     )  
#   
#   
#   p <- ggplot(
#     x,
#     aes(
#       x = year,
#       y = value,
#       color = country
#     )
#   ) +
#     geom_line(aes(size = linewidth)) +
#     scale_size_identity() +
#   #  scale_color_identity() +
#     geom_label_repel(
#       data = filter(x, year == max(year)),
#       aes(label = paste0(country, ": ", value)),
#       nudge_y = 0.03,
#       size = 9 / ggplot2::.pt,
#       show.legend = FALSE,
#       direction = "y"
#     ) +
#     labs(
#       title = str_wrap(full_title, width = 60),
#       caption = paste0("Source: ", full_caption)
#     ) +
#     scale_x_continuous(breaks = scales::breaks_pretty(n = nyears)) +
#     scale_y_continuous(
#       expand = expansion(c(0.1, 0.1)),
#       breaks = scales::breaks_pretty(n = 4),
#       labels = scales::label_number(scale_cut = scales::cut_short_scale()),
#      # limits = c(min(x$value) * 0.6, max(x$value) * 1.1)
#     ) +
#     theme(
#       panel.background = element_rect(fill = "white"),
#       #panel.grid.major.y = element_blank(),
#       panel.grid.major.x = element_blank(),
#       axis.ticks = element_blank(),
#       strip.background = element_rect(colour = "white", fill = "white"),
#       axis.text.x = element_text(size = 8, family = "sans"),
#       axis.text.y = element_text(size = 8, family = "sans"),
#       axis.line.x = element_line(colour = "black"),
#       axis.line.y = element_line(colour = "black"),
#       text = element_text(size = 8, family = "sans"),
#       strip.text = element_text(size = 10, face = "bold"),
#       title = element_text(size = 9, family = "sans", face = "bold"),
#       plot.caption = element_text(size = 8, family = "sans", face = "plain"),
#       legend.position = "none",
#       legend.title = element_blank(),
#       #axis.title = element_blank(),
#       legend.box = element_blank()
#     )
#   p
# }