plot_map <- function (regions,
                      plot_name,
                      colname_regions_short,
                      size_short = 2,
                      size_line = 0.25,
                      size_text = 2,
                      color_land = "white",
                      color_ocean = "grey98",
                      color_region = "grey30",
                      fill_land = "white",
                      fill_ocean = "grey95",
                      fill_region = "grey85",
                      nudge_x = NULL,
                      nudge_y = NULL,
                      xmin = 169,
                      ymin = 31,
                      xmax = 241,
                      ymax = 65.5,
                      width = 90,
                      height = 60,
                      dpi = 300,
                      file_type = ".png") {

  # Define centroid ------------------------------------------------------------

  sf::sf_use_s2(FALSE)
  regions <- regions %>%
    cbind(sf::st_coordinates(suppressWarnings(sf::st_centroid(.$geometry))))

  # Define coastline -----------------------------------------------------------

  coastline <- rnaturalearth::ne_coastline(scale = 50, returnclass = "sf") %>%
    sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_recenter(clon = 180) %>%
    sf::st_make_valid()

  # Define land ----------------------------------------------------------------

  land <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf") %>%
    sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_recenter(clon = 180) %>%
    sf::st_make_valid()

  # Define outline -------------------------------------------------------------

  xbuf <- 3
  ybuf <- 2.5

  outline <- tibble::tibble(
    x = c(xmin - xbuf, xmin - xbuf, xmax + xbuf, xmax + xbuf, xmin - xbuf),
    y = c(ymin - ybuf, ymax + ybuf, ymax + ybuf, ymin - ybuf, ymin - ybuf)
  )

  # Define inset ---------------------------------------------------------------

  inset <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = land,
      color = color_land,
      fill = fill_land,
      lwd = 0.1
    ) +
    ggplot2::geom_sf(
      data = coastline,
      color = color_region,
      fill = NA,
      lwd = 0.25
    ) +
    ggplot2::coord_sf(
      xlim = c(130, 300),
      ylim = c(8, 80)
    ) +
    ggplot2::geom_polygon(
      data = outline,
      mapping = ggplot2::aes(x = x, y = y),
      fill = NA,
      color = "grey60",
      linewidth = 0.5
    ) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = fill_ocean, color = NA),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA)
    )

  # Define plot ----------------------------------------------------------------

  p1 <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = land,
      color = color_land,
      fill = fill_land,
      lwd = 0.1
    ) +
    ggplot2::geom_sf(
      data = coastline,
      color = color_region,
      fill = NA,
      lwd = 0.25
    ) +
    ggplot2::geom_sf(
      data = regions,
      col = color_region,
      fill = fill_region,
      linewidth = size_line
    ) +
    ggplot2::geom_label(
      data = regions,
      mapping = ggplot2::aes(X, Y, label = .data[[colname_regions_short]]),
      size = size_short,
      label.r = grid::unit(0.05, "lines"),
      label.size = 0.125,
      label.padding = grid::unit(0.15, "lines"),
      nudge_x = nudge_x,
      nudge_y = nudge_y
    ) +
    ggplot2::coord_sf(
      xlim = c(xmin, xmax),
      ylim = c(ymin, ymax)
    ) +
    ggspatial::annotation_north_arrow(
      height = grid::unit(0.25, "npc"),
      width = grid::unit(0.2, "npc"),
      pad_x = grid::unit(0.8, "npc"),
      pad_y = grid::unit(0.72, "npc"),
      style = ggspatial::north_arrow_fancy_orienteering(
        text_col = "grey60",
        line_col = "grey60",
        fill = c("white", "grey60")
      )
    ) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = size_text),
      panel.background = ggplot2::element_rect(fill = fill_ocean, color = NA),
      panel.grid.major = ggplot2::element_line(color = color_ocean),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(t = 2, r = 2, b = 2, l = 2)
    ) +
    ggplot2::annotation_custom(
      ggplot2::ggplotGrob(inset),
      xmin = xmin - 5.6,
      ymin = ymin - 4.5,
      xmax = 200,
      ymax = 45
    )


  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm",
    dpi = dpi
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}

plot_network <- function (regions,
                          rates,
                          plot_name,
                          colname_regions_short,
                          size_short = 2,
                          size_line = 0.25,
                          size_text = 2,
                          scale_edge_width_min = 1,
                          scale_edge_width_max = 6,
                          size_label = 5,
                          hjust_label = 0,
                          vjust_label = 0,
                          strength = 4,
                          color_land = "white",
                          color_ocean = "grey98",
                          color_region = "grey30",
                          fill_land = "white",
                          fill_ocean = "grey95",
                          fill_region = "grey85",
                          nudge_x = NULL,
                          nudge_y = NULL,
                          xmin = 169,
                          ymin = 31,
                          xmax = 241,
                          ymax = 65.5,
                          width = 90,
                          height = 60,
                          dpi = 300,
                          file_type = ".png") {

  # Check arguments ------------------------------------------------------------

  # Define centroid ------------------------------------------------------------

  sf::sf_use_s2(FALSE)
  regions <- regions %>%
    cbind(sf::st_coordinates(suppressWarnings(sf::st_centroid(.$geometry))))

  # Define network nodes -------------------------------------------------------

  nodes <- tibble::tibble(
    id = dplyr::pull(regions, 3),
    label = dplyr::pull(regions, 2)
  )

  # Define network edges -------------------------------------------------------

  edges <- rates %>%
    dplyr::select(
      from = x,
      to = y,
      weight = mean
    ) %>%
    dplyr::filter(abs(from - to) == 1)

  # Define network -------------------------------------------------------------

  network <- tidygraph::tbl_graph(
    nodes = nodes,
    edges = edges,
    directed = TRUE
  )

  # Define network layout ------------------------------------------------------

  layout <- ggraph::create_layout(
    graph = network,
    layout = "manual",
    circular = FALSE,
    x = dplyr::pull(regions, X), # Passed to ggraph:::layout_tbl_graph_manual()
    y = dplyr::pull(regions, Y)  # Passed to ggraph:::layout_tbl_graph_manual()
  )

  # Define coastline -----------------------------------------------------------

  coastline <- rnaturalearth::ne_coastline(scale = 50, returnclass = "sf") %>%
    sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_recenter(clon = 180) %>%
    sf::st_make_valid()

  # Define land ----------------------------------------------------------------

  land <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf") %>%
    sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
    st_recenter(clon = 180) %>%
    sf::st_make_valid()

  # Define outline -------------------------------------------------------------

  xbuf <- 3
  ybuf <- 2.5

  outline <- tibble::tibble(
    x = c(xmin - xbuf, xmin - xbuf, xmax + xbuf, xmax + xbuf, xmin - xbuf),
    y = c(ymin - ybuf, ymax + ybuf, ymax + ybuf, ymin - ybuf, ymin - ybuf)
  )

  # Define inset ---------------------------------------------------------------

  inset <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = land,
      color = color_land,
      fill = fill_land,
      lwd = 0.1
    ) +
    ggplot2::geom_sf(
      data = coastline,
      color = color_region,
      fill = NA,
      lwd = 0.25
    ) +
    ggplot2::coord_sf(
      xlim = c(130, 300),
      ylim = c(8, 80)
    ) +
    ggplot2::geom_polygon(
      data = outline,
      mapping = ggplot2::aes(x = x, y = y),
      fill = NA,
      color = "grey60",
      linewidth = 0.5
    ) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = fill_ocean, color = NA),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent", color = NA)
    )

  # Define plot ----------------------------------------------------------------

  p1 <- ggplot2::ggplot(data = layout) +
    ggplot2::geom_sf(
      data = land,
      color = color_land,
      fill = fill_land,
      lwd = 0.1
    ) +
    ggplot2::geom_sf(
      data = coastline,
      color = color_region,
      fill = NA,
      lwd = 0.25
    ) +
    ggplot2::geom_sf(
      data = regions,
      col = color_region,
      fill = fill_region,
      linewidth = size_line
    ) +
    ggraph::geom_edge_fan(
      mapping = ggplot2::aes(
        width = weight,
        label = paste0(as.character(round(weight, 2) * 100), "%"),
        hjust = hjust_label,
        vjust = vjust_label
      ),
      label_size = size_label,
      strength = strength,
      arrow = ggplot2::arrow(
        length = grid::unit(0.5 + 3 * edges$weight, "mm"),
        type = "closed"
      ),
      end_cap = ggraph::circle(2.5, "mm")
    ) +
    ggraph::scale_edge_width(
      range = c(scale_edge_width_min, scale_edge_width_max)
    ) +
    ggplot2::geom_label(
      data = regions,
      mapping = ggplot2::aes(X, Y, label = .data[[colname_regions_short]]),
      size = size_short,
      label.r = grid::unit(0.05, "lines"),
      label.size = 0.125,
      label.padding = grid::unit(0.06, "lines"),
      nudge_x = nudge_x,
      nudge_y = nudge_y
    ) +
    ggplot2::coord_sf(
      xlim = c(xmin, xmax),
      ylim = c(ymin, ymax)
    ) +
    ggspatial::annotation_north_arrow(
      height = grid::unit(0.25, "npc"),
      width = grid::unit(0.2, "npc"),
      pad_x = grid::unit(0.8, "npc"),
      pad_y = grid::unit(0.72, "npc"),
      style = ggspatial::north_arrow_fancy_orienteering(
        text_col = "grey60",
        line_col = "grey60",
        fill = c("white", "grey60")
      )
    ) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = size_text),
      legend.position = "none",
      panel.background = ggplot2::element_rect(fill = fill_ocean, color = NA),
      panel.grid.major = ggplot2::element_line(color = color_ocean),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(t = 2, r = 2, b = 2, l = 2)
    ) +
    ggplot2::annotation_custom(
      ggplot2::ggplotGrob(inset),
      xmin = xmin - 5.6,
      ymin = ymin - 4.5,
      xmax = 200,
      ymax = 45
    )

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm",
    dpi = dpi
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}

plot_cols <- function (data,
                       plot_name,
                       regions,
                       xvar,
                       xlab,
                       ylab,
                       x_text,
                       x_breaks,
                       y_text,
                       y_breaks,
                       x_angle = 0,
                       hjust = 0.5,
                       vjust = 0.5,
                       size_title = 8,
                       size_strip = 8,
                       size_text = 8,
                       size_error = 0.2,
                       panel_spacing = 1,
                       xmin = 1,
                       xmax = 20,
                       ymin = 0.0,
                       ymax = 1.0,
                       width = 90,
                       height = 90,
                       dpi = 300,
                       file_type = ".png") {

  # Augment data ---------------------------------------------------------------

  data <- data %>%
    dplyr::mutate(
      region_previous = factor(number_to_region(.data$x, regions), regions),
      region_current = factor(number_to_region(.data$y, regions), regions)
    )

  # Define plot ----------------------------------------------------------------

  p1 <- ggplot2::ggplot(
      data = data,
      mapping = ggplot2::aes(
        x = .data[[xvar]],
        y = .data$mean
      )
    ) +
    ggplot2::geom_col(
      color = "white",
      size = 0.1
    ) +
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        ymin = .data$q5,
        ymax = .data$q95
      ),
      size = size_error,
      width = 0
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$region_previous),
      cols = ggplot2::vars(.data$region_current),
      switch = "y"
    ) +
    ggplot2::scale_x_continuous(
      labels = x_text,
      breaks = x_breaks,
      limits = c(xmin, xmax)
    ) +
    ggplot2::scale_y_continuous(
      labels = y_text,
      breaks = y_breaks,
      position = "right"
    ) +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      panel.spacing = ggplot2::unit(panel_spacing, "mm"),
      strip.text.x = ggplot2::element_text(
        size = size_strip
      ),
      strip.text.y.left = ggplot2::element_text(
        size = size_strip,
        angle = 0
      ),
      axis.title = ggplot2::element_text(
        size = size_title
      ),
      axis.text.x = ggplot2::element_text(
        size = size_text,
        angle = x_angle,
        hjust = hjust,
        vjust = vjust
      ),
      axis.text.y = ggplot2::element_text(
        size = size_text
      )
    )


  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm",
    dpi = dpi
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}

plot_abundance <- function (data,
                            plot_name,
                            x_axis_label = "Year",
                            y_axis_label = "Abundance exchange (millions)",
                            toptop = "bcak",
                            topbottom = "akbc",
                            bottomtop = "ccbc",
                            bottombottom = "bccc",
                            toptop_annotation = "AK",
                            topbottom_annotation = "BC",
                            bottomtop_annotation = "BC",
                            bottombottom_annotation = "CC",
                            linewidth_hline = 0.5,
                            linewidth_error = 0.5,
                            x_limits = c(1979, 2020),
                            y_limits_top = c(-1, 1),
                            y_limits_bottom = c(-1, 1),
                            y_limits_top_breaks = c(-1, 0, 1),
                            y_limits_bottom_breaks = c(-1, 0, 1),
                            relative_panel_height = c(1, 1),
                            x_annotation = 2019,
                            y_annotation = 0.1,
                            width = 190,
                            height = 140,
                            dpi = 300,
                            file_type = ".png") {

  # Check arguments ------------------------------------------------------------

  # Create annotation data -----------------------------------------------------

  toptop_annotation_data <- tibble::tibble(
    x = x_annotation,
    y = y_annotation,
    label = toptop_annotation
  )

  topbottom_annotation_data <- tibble::tibble(
    x = x_annotation,
    y = -y_annotation,
    label = topbottom_annotation
  )

  bottomtop_annotation_data <- tibble::tibble(
    x = x_annotation,
    y = y_annotation,
    label = bottomtop_annotation
  )

  bottombottom_annotation_data <- tibble::tibble(
    x = x_annotation,
    y = -y_annotation,
    label = bottombottom_annotation
  )

  # Define plots ---------------------------------------------------------------

  # Top panel
  p1 <- ggplot2::ggplot() +
    ggplot2::geom_col(
      data = data %>% dplyr::filter(regions == toptop),
      ggplot2::aes(x = year, y = mean)
    ) +
    ggplot2::geom_col(
      data = data %>% dplyr::filter(regions == topbottom),
      ggplot2::aes(x = .data$year, y = -.data$mean)
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = linewidth_hline,
      col = "white"
    ) +
    # ggplot2::geom_line(
    #   data = line_1,
    #   ggplot2::aes(x = .data$year, y = .data$net_mean)
    # ) +
    ggplot2::geom_errorbar(
      data = data %>% dplyr::filter(regions == toptop),
      ggplot2::aes(
        x = .data$year,
        ymin = .data$q5,
        ymax = .data$q95
      ),
      linewidth = linewidth_error,
      width = 0
    ) +
    ggplot2::geom_errorbar(
      data = data %>% dplyr::filter(regions == topbottom),
      ggplot2::aes(
        x = .data$year,
        ymin = -.data$q5,
        ymax = -.data$q95
      ),
      linewidth = linewidth_error,
      width = 0
    ) +
    # ggplot2::geom_errorbar(
    #   data = line_1,
    #   ggplot2::aes(
    #     x = .data$year,
    #     ymin = .data$net_q5,
    #     ymax = .data$net_q95
    #   ),
    #   size = size_error,
    #   width = 0
    # ) +
    ggplot2::scale_x_continuous(
      limits = x_limits
    ) +
    ggplot2::scale_y_continuous(
      limits = y_limits_top,
      breaks = y_limits_top_breaks,
      labels = as.character(abs(y_limits_top_breaks))
    ) +
    # ggplot2::scale_y_continuous(
    #   limits = c(-22e6, 5e6),
    #   breaks = seq(-2e7, 5e6, 5e6),
    #   labels = c(20, 15, 10, 5, 0, 5)
    # ) +
    ggplot2::geom_text(
      data = toptop_annotation_data,
      ggplot2::aes(x = x, y = y, label = label),
      na.rm = TRUE
    ) +
    ggplot2::geom_text(
      data = topbottom_annotation_data,
      ggplot2::aes(x = x, y = y, label = label),
      na.rm = TRUE
    ) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 1, r = 1, b = 1, l = 1, "mm")
    )
  # Bottom
  p2 <- ggplot2::ggplot() +
    ggplot2::geom_col(
      data = data %>% dplyr::filter(regions == bottomtop),
      ggplot2::aes(x = .data$year, y = .data$mean)
    ) +
    ggplot2::geom_col(
      data = data %>% dplyr::filter(regions == bottombottom),
      ggplot2::aes(x = .data$year, y = -.data$mean)
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = linewidth_hline,
      col = "white"
    ) +
    # ggplot2::geom_line(
    #   data = line_2,
    #   ggplot2::aes(x = .data$year, y = .data$net_mean)
    # ) +
    ggplot2::geom_errorbar(
      data = data %>% dplyr::filter(regions == bottomtop),
      ggplot2::aes(
        x = .data$year,
        ymin = .data$q5,
        ymax = .data$q95
      ),
      linewidth = linewidth_error,
      width = 0
    ) +
    ggplot2::geom_errorbar(
      data = data %>% dplyr::filter(regions == bottombottom),
      ggplot2::aes(
        x = .data$year,
        ymin = -.data$q5,
        ymax = -.data$q95
      ),
      linewidth = linewidth_error,
      width = 0
    ) +
    # ggplot2::geom_errorbar(
    #   data = line_2,
    #   ggplot2::aes(
    #     x = .data$year,
    #     ymin = .data$net_q5,
    #     ymax = .data$net_q95
    #   ),
    #   size = size_error,
    #   width = 0
    # ) +
    ggplot2::scale_x_continuous(
      limits = x_limits
    ) +
    ggplot2::scale_y_continuous(
      limits = y_limits_bottom,
      breaks = y_limits_bottom_breaks,
      labels = as.character(abs(y_limits_bottom_breaks))
    ) +
    # ggplot2::scale_y_continuous(
    #   limits = c(-4e6, 15e6),
    #   breaks = seq(-5e6, 15e6, 5e6),
    #   labels = c(5, 0, 5, 10, 15)
    # ) +
    ggplot2::geom_text(
      data = bottomtop_annotation_data,
      ggplot2::aes(x = x, y = y, label = label),
      na.rm = TRUE
    ) +
    ggplot2::geom_text(
      data = bottombottom_annotation_data,
      ggplot2::aes(x = x, y = y, label = label),
      na.rm = TRUE
    ) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 1, r = 1, b = 1, l = 1, "mm")
    )


  # Assemble panel figure ------------------------------------------------------

  p0 <- ggpubr::ggarrange(
    p1, p2,
    heights = relative_panel_height,
    nrow = 2,
    align = "v"
  ) %>%
    ggpubr::annotate_figure(
      left = ggpubr::text_grob(
        label = y_axis_label,
        size = 10,
        rot = 90
      ),
      bottom = ggpubr::text_grob(
        label = x_axis_label,
        size = 10
      )
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )


  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm",
    dpi = dpi
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}

plot_exchange <- function (data,
                           plot_name,
                           size_hline = 0.5,
                           size_error = 0.5,
                           width = 90,
                           height = 100,
                           dpi = 300,
                           file_type = ".png") {

  # Check arguments ------------------------------------------------------------

  # Annotations ----------------------------------------------------------------

  north_to_ak <- tibble::tibble(
    x = 2010,
    y = 45e5,
    label = "North to Alaska"
  )
  south_to_bc <- tibble::tibble(
    x = 2010,
    y = -21e6,
    label = "South to British Columbia"
  )
  north_to_bc <- tibble::tibble(
    x = 2010,
    y = 14e6,
    label = "North to British Columbia"
  )
  south_to_cc <- tibble::tibble(
    x = 2010,
    y = -3e6,
    label = "South to California Current"
  )

  # Split data -----------------------------------------------------------------

  ak_s <- dplyr::filter(data, .data$direction == "ak s", .data$year %in% 1979:2017)
  bc_n <- dplyr::filter(data, .data$direction == "bc n", .data$year %in% 1979:2017)
  bc_s <- dplyr::filter(data, .data$direction == "bc s", .data$year %in% 1979:2017)
  cc_n <- dplyr::filter(data, .data$direction == "cc n", .data$year %in% 1979:2017)
  # Line data
  line_1 <- tibble::tibble(
    year = ak_s$year,
    net_mean = bc_n$mean - ak_s$mean,
    net_q5 = bc_n$q5 - ak_s$q5,
    net_q95 = bc_n$q95 - ak_s$q95
  )
  line_2 <- tibble::tibble(
    year = bc_s$year,
    net_mean = cc_n$mean - bc_s$mean,
    net_q5 = cc_n$q5 - bc_s$q5,
    net_q95 = cc_n$q95 - bc_s$q95
  )

  # Define plots ---------------------------------------------------------------

  # Ak and BC
  p1 <- ggplot2::ggplot() +
    ggplot2::geom_col(
      data = bc_n,
      ggplot2::aes(x = .data$year, y = .data$mean)
    ) +
    ggplot2::geom_col(
      data = ak_s,
      ggplot2::aes(x = .data$year, y = -.data$mean)
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      size = size_hline,
      col = "white"
    ) +
    ggplot2::geom_line(
      data = line_1,
      ggplot2::aes(x = .data$year, y = .data$net_mean)
    ) +
    ggplot2::geom_errorbar(
      data = bc_n,
      ggplot2::aes(
        x = .data$year,
        ymin = .data$q5,
        ymax = .data$q95
      ),
      size = size_error,
      width = 0
    ) +
    ggplot2::geom_errorbar(
      data = ak_s,
      ggplot2::aes(
        x = .data$year,
        ymin = -.data$q5,
        ymax = -.data$q95
      ),
      size = size_error,
      width = 0
    ) +
    ggplot2::geom_errorbar(
      data = line_1,
      ggplot2::aes(
        x = .data$year,
        ymin = .data$net_q5,
        ymax = .data$net_q95
      ),
      size = size_error,
      width = 0
    ) +
    ggplot2::scale_y_continuous(
      limits = c(-22e6, 5e6),
      breaks = seq(-2e7, 5e6, 5e6),
      labels = c(20, 15, 10, 5, 0, 5)
    ) +
    ggplot2::geom_text(
      data = north_to_ak,
      ggplot2::aes(x = x, y = y, label = label),
      na.rm = TRUE
    ) +
    ggplot2::geom_text(
      data = south_to_bc,
      ggplot2::aes(x = x, y = y, label = label),
      na.rm = TRUE
    ) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 1, r = 1, b = 1, l = 1, "mm")
    )
  # BC and CC
  p2 <- ggplot2::ggplot() +
    ggplot2::geom_col(
      data = cc_n,
      ggplot2::aes(x = .data$year, y = .data$mean)
    ) +
    ggplot2::geom_col(
      data = bc_s,
      ggplot2::aes(x = .data$year, y = -.data$mean)
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      size = size_hline,
      col = "white"
    ) +
    ggplot2::geom_line(
      data = line_2,
      ggplot2::aes(x = .data$year, y = .data$net_mean)
    ) +
    ggplot2::geom_errorbar(
      data = cc_n,
      ggplot2::aes(
        x = .data$year,
        ymin = .data$q5,
        ymax = .data$q95
      ),
      size = size_error,
      width = 0
    ) +
    ggplot2::geom_errorbar(
      data = bc_s,
      ggplot2::aes(
        x = .data$year,
        ymin = -.data$q5,
        ymax = -.data$q95
      ),
      size = size_error,
      width = 0
    ) +
    ggplot2::geom_errorbar(
      data = line_2,
      ggplot2::aes(
        x = .data$year,
        ymin = .data$net_q5,
        ymax = .data$net_q95
      ),
      size = size_error,
      width = 0
    ) +
    ggplot2::scale_y_continuous(
      limits = c(-4e6, 15e6),
      breaks = seq(-5e6, 15e6, 5e6),
      labels = c(5, 0, 5, 10, 15)
    ) +
    ggplot2::geom_text(
      data = north_to_bc,
      ggplot2::aes(x = x, y = y, label = label),
      na.rm = TRUE
    ) +
    ggplot2::geom_text(
      data = south_to_cc,
      ggplot2::aes(x = x, y = y, label = label),
      na.rm = TRUE
    ) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 1, r = 1, b = 1, l = 1, "mm")
    )

  # Assemble panel figure ------------------------------------------------------

  p0 <- ggpubr::ggarrange(
    p1, p2,
    heights = c(27, 19),
    nrow = 2
  ) %>%
    ggpubr::annotate_figure(
      left = ggpubr::text_grob(
        "Abundance exchange (millions)",
        size = 10,
        rot = 90
      ),
      bottom = ggpubr::text_grob(
        "Year",
        size = 10
      )
    ) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )


  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm",
    dpi = dpi
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}

plot_fishing_priors_posteriors <- function (plot_name,
                                            data_prior_means,
                                            data_prior_cv,
                                            data_posteriors,
                                            year_start,
                                            year_xmin,
                                            year_xmax,
                                            regions,
                                            bar_width = 0.75,
                                            position_dodge = 0.8,
                                            width = 190,
                                            height = 100,
                                            dpi = 300,
                                            file_type = ".png") {

  # Assemble prior data --------------------------------------------------------

  priors <- data_prior_means %>%
    tibble::as_tibble() %>%
    dplyr::mutate(year = dplyr::row_number() + year_start - 1) %>%
    tidyr::pivot_longer(
      cols = !tidyr::last_col(),
      names_to = "region",
      values_to = "mean"
    ) %>%
    dplyr::mutate(region = toupper(region)) %>%
    dplyr::mutate(type = "prior") %>%
    dplyr::mutate(median = mean) %>%
    dplyr::mutate(sd = data_prior_cv * mean) %>%
    dplyr::mutate(
      q5 = purrr::map2_dbl(
        .x = mean,
        .y = sd,
        .f = qnorm,
        p = 0.05
      ),
      q95 = purrr::map2_dbl(
        .x = mean,
        .y = sd,
        .f = qnorm,
        p = 0.95
      )
    ) %>%
    dplyr::select(year, type, region, mean, median, q5, q95)

  # Assemble posterior data ----------------------------------------------------

  posteriors <- data_posteriors %>%
    dplyr::mutate(year = t + year_start - 1) %>%
    dplyr::mutate(region = toupper(number_to_region(x, regions))) %>%
    dplyr::mutate(type = "posterior") %>%
    dplyr::select(year, type, region, mean, median, q5, q95)

  # Assemble data --------------------------------------------------------------

  data_plot <- dplyr::bind_rows(priors, posteriors) %>%
    dplyr::mutate(type = factor(type, levels = c("prior", "posterior")))

  # Assemble plot --------------------------------------------------------------

  p1 <- ggplot2::ggplot(
    data = data_plot,
    mapping = ggplot2::aes(x = year, y = mean)
  ) +
    ggplot2::geom_col(
      mapping = ggplot2::aes(fill = type),
      position = ggplot2::position_dodge(position_dodge),
      width = bar_width
    ) +
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        ymin = q5,
        ymax = q95,
        col = type
      ),
      width = 0,
      position = ggplot2::position_dodge(position_dodge)
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(region),
      switch = "y"
    ) +
    ggplot2::coord_cartesian(xlim = c(year_xmin, year_xmax)) +
    ggplot2::scale_x_continuous(
      name = "Year"
    ) +
    ggplot2::scale_y_continuous(
      name = "Annual fishing rate",
      position = "right"
    ) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 0)
    )

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm",
    dpi = dpi
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}

plot_mortality_priors_posteriors <- function (plot_name,
                                              data_prior_means,
                                              data_prior_sd,
                                              data_posteriors,
                                              regions,
                                              bar_width = 0.75,
                                              position_dodge = 0.8,
                                              width = 190,
                                              height = 100,
                                              dpi = 300,
                                              file_type = ".png") {

  # Assemble prior data --------------------------------------------------------

  priors <- data_prior_means %>%
    tibble::as_tibble() %>%
    dplyr::mutate(mean = value) %>%
    dplyr::mutate(region = toupper(regions)) %>%
    dplyr::mutate(type = "prior") %>%
    dplyr::mutate(median = mean) %>%
    dplyr::mutate(sd = data_prior_sd) %>%
    dplyr::mutate(
      q5 = purrr::map2_dbl(
        .x = mean,
        .y = sd,
        .f = qnorm,
        p = 0.05
      ),
      q95 = purrr::map2_dbl(
        .x = mean,
        .y = sd,
        .f = qnorm,
        p = 0.95
      )
    ) %>%
    dplyr::select(type, region, mean, median, q5, q95)

  # Assemble posterior data ----------------------------------------------------

  posteriors <- data_posteriors %>%
    dplyr::mutate(region = toupper(number_to_region(x, regions))) %>%
    dplyr::mutate(type = "posterior") %>%
    dplyr::select(type, region, mean, median, q5, q95)

  # Assemble data --------------------------------------------------------------

  data_plot <- dplyr::bind_rows(priors, posteriors) %>%
    dplyr::mutate(type = factor(type, levels = c("prior", "posterior")))

  # Assemble plot --------------------------------------------------------------

  p1 <- ggplot2::ggplot(
    data = data_plot,
    mapping = ggplot2::aes(x = region, y = mean)
  ) +
    ggplot2::geom_col(
      mapping = ggplot2::aes(fill = type),
      position = ggplot2::position_dodge(position_dodge),
      width = bar_width
    ) +
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        ymin = q5,
        ymax = q95,
        group = type
      ),
      width = 0,
      position = ggplot2::position_dodge(position_dodge)
    ) +
    ggplot2::scale_x_discrete(
      name = ""
    ) +
    ggplot2::scale_y_continuous(
      name = "Annual natural mortality rate"
    ) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank()
    )

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm",
    dpi = dpi
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}


plot_reporting_priors_posteriors <- function (plot_name,
                                              data_prior_means,
                                              data_prior_sd,
                                              data_posteriors,
                                              regions,
                                              bar_width = 0.75,
                                              position_dodge = 0.8,
                                              width = 190,
                                              height = 100,
                                              dpi = 300,
                                              file_type = ".png") {

  # Assemble prior data --------------------------------------------------------

  priors <- data_prior_means %>%
    tibble::as_tibble() %>%
    dplyr::mutate(mean = value) %>%
    dplyr::mutate(region = toupper(regions)) %>%
    dplyr::mutate(type = "prior") %>%
    dplyr::mutate(median = mean) %>%
    dplyr::mutate(sd = data_prior_sd) %>%
    dplyr::mutate(
      q5 = purrr::map2_dbl(
        .x = mean,
        .y = sd,
        .f = qnorm,
        p = 0.05
      ),
      q95 = purrr::map2_dbl(
        .x = mean,
        .y = sd,
        .f = qnorm,
        p = 0.95
      )
    ) %>%
    dplyr::select(type, region, mean, median, q5, q95)

  # Assemble posterior data ----------------------------------------------------

  posteriors <- data_posteriors %>%
    dplyr::mutate(region = toupper(number_to_region(x, regions))) %>%
    dplyr::mutate(type = "posterior") %>%
    dplyr::select(type, region, mean, median, q5, q95)

  # Assemble data --------------------------------------------------------------

  data_plot <- dplyr::bind_rows(priors, posteriors) %>%
    dplyr::mutate(type = factor(type, levels = c("prior", "posterior")))

  # Assemble plot --------------------------------------------------------------

  p1 <- ggplot2::ggplot(
    data = data_plot,
    mapping = ggplot2::aes(x = region, y = mean)
  ) +
    ggplot2::geom_col(
      mapping = ggplot2::aes(fill = type),
      position = ggplot2::position_dodge(position_dodge),
      width = bar_width
    ) +
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        ymin = q5,
        ymax = q95,
        group = type
      ),
      width = 0,
      position = ggplot2::position_dodge(position_dodge)
    ) +
    ggplot2::scale_x_discrete(
      name = ""
    ) +
    ggplot2::scale_y_continuous(
      name = "Annual reporting rate"
    ) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank()
    )

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm",
    dpi = dpi
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}

plot_released_by_year <- function (plot_name,
                                   data,
                                   size_range = 400:800,
                                   year_start,
                                   year_xmin,
                                   year_xmax,
                                   regions,
                                   bar_width = 0.75,
                                   width = 190,
                                   height = 100,
                                   dpi = 300,
                                   file_type = ".png") {

  # Assemble data --------------------------------------------------------------

  data <- data %>%
    dplyr::mutate(year_released = lubridate::year(date_released)) %>%
    dplyr::mutate(year_recovered = lubridate::year(date_recovered)) %>%
    dplyr::filter(size_released %in% size_range) %>%
    dplyr::filter(year_released %in% year_start:year_xmax) %>%
    dplyr::mutate(
      region_short_3 = number_to_region(region_released_3, toupper(regions))
    )

  # Assemble plot --------------------------------------------------------------

  p1 <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(x = year_released)
  ) +
    ggplot2::geom_bar(
      width = bar_width
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(region_short_3),
      switch = "y"
    ) +
    ggplot2::coord_cartesian(xlim = c(year_xmin, year_xmax)) +
    ggplot2::scale_x_continuous(
      name = "Year"
    ) +
    ggplot2::scale_y_continuous(
      name = "Tagged sablefish released",
      position = "right"
    ) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 0)
    )

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm",
    dpi = dpi
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}

plot_released_by_size <- function (plot_name,
                                   data,
                                   regions,
                                   size_range,
                                   year_range,
                                   binwidth = 10,
                                   width = 190,
                                   height = 100,
                                   dpi = 300,
                                   file_type = ".png") {

  # Assemble data --------------------------------------------------------------

  data <- data %>%
    dplyr::mutate(year_released = lubridate::year(date_released)) %>%
    dplyr::mutate(year_recovered = lubridate::year(date_recovered)) %>%
    dplyr::filter(size_released %in% size_range) %>%
    dplyr::filter(year_released %in% year_range) %>%
    dplyr::mutate(
      region_short_3 = number_to_region(region_released_3, toupper(regions))
    )

  # Assemble plot --------------------------------------------------------------

  p1 <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(x = size_released)
  ) +
    ggplot2::geom_histogram(
      binwidth = binwidth
    ) +
    ggplot2::geom_vline(
      xintercept = c(400, 800),
      colour = c("black"),
      linetype = 1,
      linewidth = 0.25
    ) +
    ggplot2::geom_vline(
      xintercept = c(550),
      colour = c("black"),
      linetype = 2,
      linewidth = 0.25
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(region_short_3),
      switch = "y"
    ) +
    ggplot2::scale_x_continuous(
      name = "Sablefish length (mm)"
    ) +
    ggplot2::scale_y_continuous(
      name = "Sablefish released",
      position = "right"
    ) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 0)
    )

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm",
    dpi = dpi
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}

plot_recovered_by_year <- function (plot_name,
                                    data,
                                    size_range, # Released size
                                    year_range,
                                    year_xmin,
                                    year_xmax,
                                    regions,
                                    bar_width = 0.75,
                                    width = 190,
                                    height = 100,
                                    dpi = 300,
                                    file_type = ".png") {

  # Assemble data --------------------------------------------------------------

  data <- data %>%
    dplyr::mutate(year_released = lubridate::year(date_released)) %>%
    dplyr::mutate(year_recovered = lubridate::year(date_recovered)) %>%
    dplyr::filter(size_released %in% size_range) %>%
    dplyr::filter(year_released %in% year_range) %>%
    dplyr::filter(year_recovered %in% year_range) %>%
    dplyr::mutate(
      region_rel_short_3 = number_to_region(region_released_3, toupper(regions)),
      region_rec_short_3 = number_to_region(region_recovered_3, toupper(regions)),
      max_liberty = factor(ifelse(days_liberty <= 1095, 0, 1), c(1, 0))
    )

  # Assemble plot --------------------------------------------------------------

  p1 <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(
      x = year_recovered,
      fill = max_liberty
    )
  ) +
    ggplot2::geom_bar(
      width = bar_width
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(region_rel_short_3),
      cols = ggplot2::vars(region_rec_short_3),
      switch = "y"
    ) +
    ggplot2::coord_cartesian(xlim = c(year_xmin, year_xmax)) +
    ggplot2::scale_x_continuous(
      name = "Year"
    ) +
    ggplot2::scale_y_continuous(
      name = "Tagged sablefish recovered",
      position = "right"
    ) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      legend.position = "none",
      legend.title = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 0)
    )

  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm",
    dpi = dpi
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}

plot_duration_at_liberty <- function (plot_name,
                                      data,
                                      size_range, # Released size
                                      year_range,
                                      regions,
                                      binwidth = 10,
                                      width = 190,
                                      height = 100,
                                      dpi = 300,
                                      file_type = ".png") {

  # Assemble data --------------------------------------------------------------

  data <- data %>%
    dplyr::mutate(year_released = lubridate::year(date_released)) %>%
    dplyr::mutate(year_recovered = lubridate::year(date_recovered)) %>%
    dplyr::filter(size_released %in% size_range) %>%
    dplyr::filter(year_released %in% year_range) %>%
    dplyr::filter(year_recovered %in% year_range) %>%
    dplyr::mutate(
      region_rel_short_3 = number_to_region(region_released_3, toupper(regions))
    )

  # Assemble plot --------------------------------------------------------------

  p1 <- ggplot2::ggplot(
    data = data,
    mapping = ggplot2::aes(x = days_liberty)
  ) +
    ggplot2::geom_histogram(
      binwidth = binwidth
    ) +
    ggplot2::geom_vline(
      xintercept = c(1095),
      colour = c("black"),
      linetype = 2,
      linewidth = 0.25
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(region_rel_short_3),
      switch = "y"
    ) +
    ggplot2::scale_x_continuous(
      name = "Duration at liberty (days)"
    ) +
    ggplot2::scale_y_continuous(
      name = "Sablefish recovered",
      position = "right"
    ) +
    ggsidekick::theme_sleek() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(angle = 0)
    )


  # Save ggplot ----------------------------------------------------------------

  ggplot2::ggsave(
    here::here("ms", "figs", paste0(plot_name, file_type)),
    width = width,
    height = height,
    units = "mm",
    dpi = dpi
  )

  # Return path
  return(paste0("ms/", "figs/", plot_name, file_type))
}

