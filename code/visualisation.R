library(magrittr)

#### load data ####

load("output/tmp_data.RData")

#### create plot object ####

vis <- plotly::plot_ly() %>% 
  plotly::layout(
    showlegend = FALSE,
    scene = list(
      dragmode = "orbit",
      aspectratio = list(x=6, y=4, z=4),
      camera = list(
        eye = list(x = 7, y = -7, z = 2) 
      )
    )
  ) 

#### add trench corners #### 

edges <- data.frame(
  x = c(16, 20, 16, 20, 16, 20, 16, 20),
  y = c(100, 100, 100, 100, 106, 106, 106, 106),
  z = c(112.5, 112.5, 108, 108, 112.5, 112.5, 108, 108)
)

vis %<>%
  plotly::add_trace(
    data = edges, 
    x = ~x, y = ~y, z = ~z, 
    type = "scatter3d", 
    mode = "markers",
    marker = list(size = 3, color = "blue", symbol = 104)
  )

#### add measured points ####

vis %<>% plotly::add_trace(
  data = do.call(rbind, level_points), 
  x = ~x, y = ~y, z = ~z, 
  mode = "markers", 
  type = "scatter3d", 
  marker = list(size = 2, color = "red", symbol = 104)
)

#### add surfaces ####

# generic function
add_multiple_traces <- function(v, l, color = NA, ...) {
  i <- length(l)
  if (i == 1) { 
    plotly::add_trace(p = v, x = ~l[[i]]$x, y = ~l[[i]]$y, z = ~l[[i]]$z, color[i], ...)
  } else {
    add_multiple_traces(
      plotly::add_trace(p = v, x = ~l[[i]]$x, y = ~l[[i]]$y, z = ~l[[i]]$z, color[i], ...), 
      l[1:(i - 1)],
      color, 
      ...
    )
  }
}

# transform data to a struture suitable for plotly
maps_wide <- lapply(
  maps,
  function(x) recexcavAAR::spatialwide(x$x, x$y, x$pred, 3)
)

# add surfaces to plot
vis2 <- add_multiple_traces(
  v = vis, l = maps_wide, 
  type = "surface", showscale = FALSE
)

#### vis filled #### 

# split into list by horizons
all_points_list <- all_points %>%
  split(., .$pos)

# add points with different colors for each horizon
vis3 <- add_multiple_traces(
  v = vis2, l = all_points_list, 
  color = rainbow(length(all_points_list)), 
  mode = "markers", type = "scatter3d", 
  marker = list(size = 1, symbol = 104)
)

