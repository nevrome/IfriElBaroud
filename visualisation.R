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

a <- list()

for (mp in 1:length(maps)) {
  a[[mp]] <- recexcavAAR::spatialwide(maps[[mp]]$x, maps[[mp]]$y, maps[[mp]]$pred, 3)
}

vis <- vis %>% 
  plotly::add_trace(x = ~a[[1]]$x, y = ~a[[1]]$y, z = ~a[[1]]$z, type = "surface", showscale = FALSE
  ) %>%
  plotly::add_trace(x = ~a[[2]]$x, y = ~a[[2]]$y, z = ~a[[2]]$z, type = "surface", showscale = FALSE
  ) %>%
  plotly::add_trace(x = ~a[[3]]$x, y = ~a[[3]]$y, z = ~a[[3]]$z, type = "surface", showscale = FALSE
  ) %>%
  plotly::add_trace(x = ~a[[4]]$x, y = ~a[[4]]$y, z = ~a[[4]]$z, type = "surface", showscale = FALSE
  ) %>%
  plotly::add_trace(x = ~a[[5]]$x, y = ~a[[5]]$y, z = ~a[[5]]$z, type = "surface", showscale = FALSE
  ) 

add_layer <- function(vis, a, i) {
  if (i == 1) { 
    plotly::add_trace(p = vis, x = ~a[[i]]$x, y = ~a[[i]]$y, z = ~a[[i]]$z, type = "surface", showscale = FALSE) 
  } else {
    add_layer(
      plotly::add_trace(p = vis, x = ~a[[i]]$x, y = ~a[[i]]$y, z = ~a[[i]]$z, type = "surface", showscale = FALSE), 
      a, 
      i - 1
    )
  }
}

vis2 <- add_layer(vis, a, length(a))

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
