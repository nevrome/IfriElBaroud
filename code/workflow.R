#### load libraries ####

library(recexcavAAR)
library(dplyr)
library(kriging)
library(magrittr)
library(plotly)

#### load data ####

# from top to bottom
level_files <- c(
  "data/level_CaveSurface.csv",
  "data/border_MixedHorizonEscargotiere_Escargotiere.csv",
  "data/border_MixedHorizonCoucheRouge_MixedHorizonEscargotiere.csv",
  "data/border_CoucheRouge_MixedHorizonCoucheRouge.csv",
  "data/level_CaveBedrock_more_precise.csv"
)

level_points <- lapply(
  level_files,
  function(x) {
    read.csv(x, header = T)
  }
)

squares <- read.csv("data/corners_excavation_squares_CampaignIB2015.csv", header = TRUE)

#### create reconstructed surfaces ####

maps <- recexcavAAR::kriglist(level_points, lags = 5, model = "spherical")

#### cut the surfaces level to the trench outline ####

ggplot(maps[[1]]) +
  geom_point(aes(x, y, color = pred)) +
  scale_colour_continuous(low = "green", high = "red")

# trench outline polygon
c1 <- data.frame(
  x = c(16, 16.9, 16.9, 16),
  y = c(102.95, 102.95, 106, 106)
)
# cut
rem <- recexcavAAR::pnpmulti(c1$x, c1$y, maps[[1]]$x, maps[[1]]$y)
maps[[1]] <- maps[[1]][!rem, ]

ggplot(maps[[1]]) +
  geom_point(aes(x, y, color = pred)) +
  scale_colour_continuous(low = "green", high = "red")

#### fill and attribute squares ####

squares_pos <- squares %>% 
  dplyr::group_by(SID) %>%
  dplyr::do(
    fill_points = recexcavAAR::fillhexa(.[, -1], 0.2)
  ) %>% 
  dplyr::group_by(SID) %>%
  dplyr::do(
    points_with_pos = recexcavAAR::posdec(.$fill_points, maps)
  )

all_points <- squares_pos %>%
  tidyr::unnest() %>%
  dplyr::mutate(
    pos = dplyr::case_when(
      pos == 0 ~ "below_bottom",
      pos == 1 ~ "couche_rouge",
      pos == 2 ~ "mixed_couche_rouge",
      pos == 3 ~ "mixed_escargotiere",
      pos == 4 ~ "escargotiere",
      pos == 5 ~ "above_surface",
      TRUE ~ as.character(pos)
    )
  )

#### determine distribution ####

perc <- all_points %>%
  dplyr::group_by(SID) %>%
  dplyr::mutate(n_SID = n()) %>%
  dplyr::group_by(SID, pos) %>%
  dplyr::mutate(n_SID_POS = n()) %>%
  dplyr::mutate(part = (n_SID_POS/n_SID) * 100) %>%
  tidyr::spread(pos, part) %>%
  dplyr::group_by(SID) %>%
  dplyr::summarise_all(mean, na.rm = TRUE)

perc2 <- perc %>% 
  dplyr::select(
    SID,
    below_bottom, 
    couche_rouge, 
    mixed_couche_rouge, 
    mixed_escargotiere, 
    escargotiere, 
    above_surface
  ) %>%
  dplyr::mutate_all(
    round, 1
  )

write.csv(perc2, file = "report/attribution.csv")

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
