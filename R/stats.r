# mean arrow
StatMeanCircular <- ggplot2::ggproto(
  "StatMeanCircular",
  ggplot2::Stat,
  compute_group = function(self,
                           data,
                           scales,
                           bandwith = 25,
                           na.rm = F) {
    if (!is.null(self$x_datacircularp))
      data$x = circular(
        data$x,
        units = self$x_datacircularp$units,
        type = self$x_datacircularp$type,
        template = self$x_datacircularp$template,
        modulo = self$x_datacircularp$modulo,
        zero = self$x_datacircularp$zero,
        rotation = self$x_datacircularp$rotation
      )

    mean_circ = mean(data$x, na.rm = na.rm)
    rho_circ = rho.circular(data$x, na.rm = na.rm)

    data.frame(x = mean_circ,
               y = 0,
               mean = mean_circ,
               rho = rho_circ)
  },

  required_aes = c("x"),
  default_aes = ggplot2::aes(
    x = ggplot2::after_stat(mean),
    y = 0,
    yend = ggplot2::after_stat(rho),
    xend = ggplot2::after_stat(mean)
  )
)

stat_mean_circular <-
  function(mapping = NULL,
           data = NULL,
           geom = "segment",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           bandwith = 25,
           ...) {

    arrow = ggplot2::arrow(length = unit(0.5, "cm"))

    ggplot2::layer(
      stat = StatMeanCircular,
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        bandwith = bandwith,
        na.rm = na.rm,
        arrow = arrow,
        ...
      ),
      layer_class = LayerCircular
    )
  }

# Custom Axis
StatAnnotateAxisCircular <-
  ggplot2::ggproto(
    "StatAnnotateAxisCircular",
    ggplot2::Stat,
    compute_group = function(self, data, scales, n = 4) {
      if (self$x_datacircularp$units == "degrees") {
        data.frame(
          x = seq(0, 270, by = 90),
          y = .7,
          label = seq(0, 270, by = 90)
        )
      }
      else if(self$x_datacircularp$units == "radians") {
        data.frame(
          x = seq(0, 1.5, by = 0.5)*pi,
          y = .7,
          label = c("2\u03C0", "½\u03C0", "1\u03C0", "1½\u03C0")
        )
      }
      else if(self$x_datacircularp$units == "hours") {
        data.frame(
          x = seq(0, 23, by = 1),
          y = .7,
          label = seq(0, 23, by = 1)
        )
      }
      else {
        rlang::abort("Unknown unit in circular data. Unit should be 'degrees', 'radians' or 'hours'.")
      }
    },
    default_aes = ggplot2::aes(
      x = x,
      y = .7,
      label = x,
      vjust = .5,
      hjust = .5
    )
  )

anootation_axis_circular = function(mapping = NULL, n = 4, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatAnnotateAxisCircular,
    position = "identity",
    geom = "text",
    inherit.aes = TRUE,
    params = list(...),
    layer_class = LayerCircular
  )
}




## StatDensityCircular

GeomPointCircular <- ggplot2::ggproto("GeomPointCircular", ggplot2::GeomPoint,
                                      draw_panel = function(self, data, panel_params, coord) {
                                        print(coord$limits)
                                        coord$limits$x = c(0,180)
                                        print(coord$limits)
                                        ggplot2::ggproto_parent(ggplot2::GeomPoint, self)$draw_panel(data, panel_params, coord)
                                      }

)

StatDensityCircular <- ggplot2::ggproto(
  "StatDensityCircular",
  ggplot2::Stat,
  compute_group = function(self, data, scales, bandwith = 25) {
    if (!is.null(self$x_datacircularp))
      data$x = circular(
        data$x,
        units = self$x_datacircularp$units,
        type = self$x_datacircularp$type,
        
        template = self$x_datacircularp$template,
        modulo = self$x_datacircularp$modulo,
        zero = self$x_datacircularp$zero,
        rotation = self$x_datacircularp$rotation
      )
    dens_dat = density(
      data$x,
      bw = bandwith,
      from = 0,
      to = 2 * pi,
      n = 1000
    )
    data.frame(x = dens_dat$x, density = dens_dat$y)
  },

  required_aes = c("x"),
  default_aes = ggplot2::aes(y = stat(density) + 1)
)

stat_density_circular <-
  function(mapping = NULL,
           data = NULL,
           geom = "line",
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           bandwith = 25,
           ...) {
    ggplot2::layer(
      stat = StatDensityCircular,
      data = data,
      mapping = mapping,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(bandwith = bandwith, na.rm = na.rm, ...),
      layer_class = LayerCircular
    )
  }

## StatPointCircular
StatPointCircular <- ggplot2::ggproto(
  "StatPointCircular",
  ggplot2::Stat,
  compute_group = function(self, data, scales) {
    data.frame(x = data$x, y = 0)
  },
  default_aes = ggplot2::aes(x = stat(x), y = stat(y) + 1),
  required_aes = c("x")
)

geom_point_circular <-
  function(mapping = NULL,
           data = NULL,
           stat = StatPointCircular,
           position = "identity",
           ...,
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE) {
    ggplot2::layer(
      stat = stat,
      data = data,
      mapping = mapping,
      geom = GeomPointCircular,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }
