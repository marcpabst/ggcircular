LayerCircular <- ggplot2::ggproto(
  "LayerCircular",
  ggplot2:::Layer,
  setup_layer = function(self, data, plot) {
    data
  },
  compute_aesthetics  = function(self, data, plot) {
    data = ggplot2::ggproto_parent(ggplot2:::Layer, self)$compute_aesthetics(data, plot)
    self$stat$x_datacircularp = NULL

    if (circular::is.circular(data$x))
      self$stat$x_datacircularp = circular::circularp(data$x)
    else
    {
      rlang::abort("'x' must be circular.")
    }
    return(data)
  }
)
