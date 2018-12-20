#### vis surfaces ####

edges <- data.frame(
  x = c(16, 20, 16, 20, 16, 20, 16, 20),
  y = c(100, 100, 100, 100, 106, 106, 106, 106),
  z = c(112.5, 112.5, 108, 108, 112.5, 112.5, 108, 108)
)

vis <- plot_ly(
  edges, 
  x = ~x, y = ~y, z = ~z, 
  type = "scatter3d", 
  mode = "markers",
  marker = list(size = 3, color = "blue", symbol = 104)
) %>% 
  layout(
    showlegend = FALSE,
    scene = list(
      dragmode = "orbit",
      aspectratio = list(x=6, y=4, z=4),
      camera = list(
        eye = list(x = 7, y = -7, z = 2) 
      )
    )
  )

for (pp in 1:length(level_points)) {
  vis <- vis %>% add_trace(
    data = level_points[[pp]], 
    x = ~x, y = ~y, z = ~z, 
    mode = "markers", 
    type = "scatter3d", 
    marker = list(size = 2, color = "red", symbol = 104)
  )
}

a <- list()

for (mp in 1:length(maps)) {
  a[[mp]] <- spatialwide(maps[[mp]]$x, maps[[mp]]$y, maps[[mp]]$pred, 3)
}

vis <- vis %>% 
  add_trace(x = ~a[[1]][[1]], y = ~a[[1]][[2]], z = ~a[[1]][[3]], type = "surface", showscale = FALSE
  ) %>%
  add_trace(x = ~a[[2]][[1]], y = ~a[[2]][[2]], z = ~a[[2]][[3]], type = "surface", showscale = FALSE
  ) %>%
  add_trace(x = ~a[[3]][[1]], y = ~a[[3]][[2]], z = ~a[[3]][[3]], type = "surface", showscale = FALSE
  ) %>%
  add_trace(x = ~a[[4]][[1]], y = ~a[[4]][[2]], z = ~a[[4]][[3]], type = "surface", showscale = FALSE
  ) %>%
  add_trace(x = ~a[[5]][[1]], y = ~a[[5]][[2]], z = ~a[[5]][[3]], type = "surface", showscale = FALSE
  ) 
#### vis filled #### 

all_points_list <- all_points %>%
  plyr::dlply("pos", identity)

vis2 <- vis %>% 
  add_trace(
    data = all_points_list$below_bottom, 
    x = ~x, y = ~y, z = ~z, 
    mode = "markers", type = "scatter3d", 
    marker = list(size = 1, color = "#e41a1c", symbol = 104)
  ) %>%
  add_trace(
    data = all_points_list$couche_rouge, 
    x = ~x, y = ~y, z = ~z, 
    mode = "markers", type = "scatter3d", 
    marker = list(size = 1, color = "#377eb8", symbol = 104)
  )  %>%
  add_trace(
    data = all_points_list$mixed_couche_rouge, 
    x = ~x, y = ~y, z = ~z, 
    mode = "markers", type = "scatter3d", 
    marker = list(size = 1, color = "#4daf4a", symbol = 104)
  ) %>%
  add_trace(
    data = all_points_list$mixed_escargotiere, 
    x = ~x, y = ~y, z = ~z, 
    mode = "markers", type = "scatter3d", 
    marker = list(size = 1, color = "#984ea3", symbol = 104)
  ) %>%
  add_trace(
    data = all_points_list$escargotiere, 
    x = ~x, y = ~y, z = ~z, 
    mode = "markers", type = "scatter3d", 
    marker = list(size = 1, color = "#ff7f00", symbol = 104)
  ) %>%
  add_trace(
    data = all_points_list$above_surface, 
    x = ~x, y = ~y, z = ~z, 
    mode = "markers", type = "scatter3d", 
    marker = list(size = 1, color = "#ffff33", symbol = 104)
  )

vis2