# ggcircular
## Plotting circular data with ggplot2
For working with circular (or directional) data, the `circular` package provides a data type to hold values that represent positions across a circle. The `circular` package comes with basic plotting functions but compared to ggplot2 it lacks a lot of flexibility. GGplot2 supports circular coordinate systems (using `coord_polar()`). However, bringing those two together can quite complex, because ordinary stats like `stats_density()` will not work correctly with circular data.

GGcircular tries to make workig with such data easier while alowing you to use the flexibility of ggplot2. It introduces a number of new geoms and stats:

- `geom_point_circular()` for drawing circular data points
- `stats_density_circular()`: for drawing circular densities
- `stats_mean_circular()`: for drawing circular means and mean resultant lengths
- `annotation_axis_circular()` for drawing axis and decoration

## Example
```R
# Create a von-Mises distirbuted sample with M=0 and ϰ=2
data_example <- data.frame(x = circular::rvonmises(100,0,2, control.circular = list(units = "degrees")))

# Plot points, density and mean arrow
ggplot(data = data_example, mapping = aes(x = x)) + 
  coord_polar() +
  geom_point_circular() +
  stat_density_circular() +
  stat_mean_circular() +
  annotation_axis_circular(unit="degrees") +
  theme_circular()
 ```
