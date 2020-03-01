CoordCircular <- ggplot2::ggproto("CoordCircular", ggplot2::CoordPolar,
                                  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
                                    ret <- list(x = list(), y = list())
                                    for (n in c("x", "y")) {

                                      scale <- get(paste0("scale_", n))
                                      limits <- self$limits[[n]]

                                      if (self$theta == n) {
                                        expansion <- ggplot2:::default_expansion(scale, c(0, 0.5), c(0, 0))
                                      } else {
                                        expansion <- ggplot2:::default_expansion(scale, c(0, 0),   c(0, 0))
                                      }


                                      range <- ggplot2:::expand_limits_scale(scale, expansion, coord_limits = limits)
                                      #range[1] = 0
                                      #if(n == "x") range = c(0, 360)

                                      out <- scale$break_info(range)
                                      ret[[n]]$range <- out$range
                                      ret[[n]]$major <- out$major_source
                                      ret[[n]]$minor <- out$minor_source
                                      ret[[n]]$labels <- out$labels
                                      ret[[n]]$sec.range <- out$sec.range
                                      ret[[n]]$sec.major <- out$sec.major_source_user
                                      ret[[n]]$sec.minor <- out$sec.minor_source_user
                                      ret[[n]]$sec.labels <- out$sec.labels
                                    }


                                    details = list(
                                      x.range = ret$x$range, y.range = ret$y$range,
                                      x.major = ret$x$major, y.major = ret$y$major,
                                      x.minor = ret$x$minor, y.minor = ret$y$minor,
                                      x.labels = ret$x$labels, y.labels = ret$y$labels,
                                      x.sec.range = ret$x$sec.range, y.sec.range = ret$y$sec.range,
                                      x.sec.major = ret$x$sec.major, y.sec.major = ret$y$sec.major,
                                      x.sec.minor = ret$x$sec.minor, y.sec.minor = ret$y$sec.minor,
                                      x.sec.labels = ret$x$sec.labels, y.sec.labels = ret$y$sec.labels
                                    )

                                    if (self$theta == "y") {
                                      names(details) <- gsub("x\\.", "r.", names(details))
                                      names(details) <- gsub("y\\.", "theta.", names(details))
                                      details$r.arrange <- scale_x$axis_order()
                                    } else {
                                      names(details) <- gsub("x\\.", "theta.", names(details))
                                      names(details) <- gsub("y\\.", "r.", names(details))
                                      details$r.arrange <- scale_y$axis_order()
                                    }

                                    details
                                  })


coord_circular <- function(theta = "x", start = 0, direction = 1, clip = "on") {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"

  ggproto(NULL, CoordCircular,
          theta = theta,
          r = r,
          start = start,
          direction = sign(direction),
          clip = clip
  )
}

