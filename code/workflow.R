library(magrittr)

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

# trench outline polygon
c1 <- data.frame(
  x = c(16, 16.9, 16.9, 16),
  y = c(102.95, 102.95, 106, 106)
)
# cut
rem <- recexcavAAR::pnpmulti(c1$x, c1$y, maps[[1]]$x, maps[[1]]$y)
maps[[1]] <- maps[[1]][!rem, ]

#### fill and attribute squares ####

squares_pos <- squares %>% 
  dplyr::group_by(SID) %>%
  dplyr::do(
    fill_points = recexcavAAR::fillhexa(.[, -1], 0.5)
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

#### save data ####

save(level_points, maps, all_points, file = "output/tmp_data.RData")
write.csv(perc2, file = "output/attribution.csv")
