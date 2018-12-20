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

maps_wide  <- lapply(
  maps,
  function(x) {
    recexcavAAR::spatialwide(x$x, x$y, x$pred, 3)
  }
)

add_layer <- function(vis, maps_wide) {
  i <- length(maps_wide)
  if (i == 1) { 
    plotly::add_trace(p = vis, x = ~maps_wide[[i]]$x, y = ~maps_wide[[i]]$y, z = ~maps_wide[[i]]$z, type = "surface", showscale = FALSE)
  } else {
    add_layer(
      plotly::add_trace(p = vis, x = ~maps_wide[[i]]$x, y = ~maps_wide[[i]]$y, z = ~maps_wide[[i]]$z, type = "surface", showscale = FALSE), 
      maps_wide[1:(i - 1)]
    )
  }
}

vis <- add_layer(vis, maps_wide)

#### vis filled #### 

all_points_list <- all_points %>%
  plyr::dlply("pos", identity)

vis2 <- vis %>% 
  plotly::add_trace(
    data = all_points_list$below_bottom, 
    x = ~x, y = ~y, z = ~z, 
    mode = "markers", type = "scatter3d", 
    marker = list(size = 1, color = "#e41a1c", symbol = 104)
  ) %>%
  plotly::add_trace(
    data = all_points_list$couche_rouge, 
    x = ~x, y = ~y, z = ~z, 
    mode = "markers", type = "scatter3d", 
    marker = list(size = 1, color = "#377eb8", symbol = 104)
  )  %>%
  plotly::add_trace(
    data = all_points_list$mixed_couche_rouge, 
    x = ~x, y = ~y, z = ~z, 
    mode = "markers", type = "scatter3d", 
    marker = list(size = 1, color = "#4daf4a", symbol = 104)
  ) %>%
  plotly::add_trace(
    data = all_points_list$mixed_escargotiere, 
    x = ~x, y = ~y, z = ~z, 
    mode = "markers", type = "scatter3d", 
    marker = list(size = 1, color = "#984ea3", symbol = 104)
  ) %>%
  plotly::add_trace(
    data = all_points_list$escargotiere, 
    x = ~x, y = ~y, z = ~z, 
    mode = "markers", type = "scatter3d", 
    marker = list(size = 1, color = "#ff7f00", symbol = 104)
  ) %>%
  plotly::add_trace(
    data = all_points_list$above_surface, 
    x = ~x, y = ~y, z = ~z, 
    mode = "markers", type = "scatter3d", 
    marker = list(size = 1, color = "#ffff33", symbol = 104)
  )

vis2
