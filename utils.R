theme_fsa <- function(frame_color = "#4C4C4C",
                      frame_font_color = "#4C4C4C",
                      grid_color = "#4C4C4C"){
  theme_bw() +
    theme(legend.position = "bottom",
          legend.spacing.x = unit(2, "pt"),
          strip.background=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major.x=element_blank(),
          panel.grid.major.y=element_blank(),
          panel.border     = element_blank(),
          # panel.border     = element_rect(color = frame_color),
          axis.line = element_line(color=frame_color),
          axis.text        = element_text(color = frame_font_color),
          axis.title       = element_text(color = frame_font_color),
          strip.text       = element_text(color = frame_font_color),
          legend.title     = element_text(color = frame_font_color),
          legend.text      = element_text(color = frame_font_color),
          axis.ticks = element_line(color = frame_color),
          axis.ticks.y = element_line(color = grid_color),
          plot.caption=element_text(size=7, color=frame_font_color)
    )
}

theme_set(theme_fsa())
