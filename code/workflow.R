library(magrittr)

#### load data ####

# surface measurements from profiles and plana
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

# corner points of ecavation squares
squares <- read.csv("data/corners_excavation_squares_CampaignIB2015.csv", header = TRUE)

# trench outline polygon
trench_outline <- data.frame(
  x = c(16, 16.9, 16.9, 16),
  y = c(102.95, 102.95, 106, 106)
)

# horizon order from top to bottom
horizon_order <- c(
  "above_surface",
  "escargotiere",
  "mixed_escargotiere",
  "mixed_couche_rouge",
  "couche_rouge", 
  "below_bottom"
)

#### create reconstructed surfaces ####

maps <- recexcavAAR::kriglist(level_points, lags = 5, model = "spherical")

#### cut the surface level to the trench outline ####

<<<<<<< HEAD
# trench outline polygon
c1 <- data.frame(
  x = c(16, 16.9, 16.9, 16),
  y = c(102.95, 102.95, 106, 106)
)
# cut
rem <- recexcavAAR::pnpmulti(c1$x, c1$y, maps[[1]]$x, maps[[1]]$y)
=======
rem <- recexcavAAR::pnpmulti(trench_outline$x, trench_outline$y, maps[[1]]$x, maps[[1]]$y)
>>>>>>> f41efd20c9a56b14021a474a6068fd69a0e13803
maps[[1]] <- maps[[1]][!rem, ]

#### fill and attribute squares ####

<<<<<<< HEAD
squares_pos <- squares %>% 
  dplyr::group_by(SID) %>%
  dplyr::do(
    fill_points = recexcavAAR::fillhexa(.[, -1], 0.5)
  ) %>% 
=======
all_points <- squares %>% 
>>>>>>> f41efd20c9a56b14021a474a6068fd69a0e13803
  dplyr::group_by(SID) %>%
  dplyr::do(
    dplyr::select(., -SID) %>%
      # fill each square with a regular point grid
      recexcavAAR::fillhexa(., 0.2) %>%
      # decide for each point in which excavation 
      # horizon it belongs
      recexcavAAR::posdec(., maps) %>%
      # rename horizons
      dplyr::mutate(
        pos = dplyr::case_when(
          pos == 0 ~ "below_bottom",
          pos == 1 ~ "couche_rouge",
          pos == 2 ~ "mixed_couche_rouge",
          pos == 3 ~ "mixed_escargotiere",
          pos == 4 ~ "escargotiere",
          pos == 5 ~ "above_surface",
          TRUE ~ as.character(pos)
        ) %>% factor(levels = horizon_order)
      )
  ) %>%
  dplyr::ungroup()

#### determine percental attribution of squares to horizons ####

perc <- all_points %>%
  # count number of points per square
  dplyr::group_by(SID) %>%
  dplyr::mutate(n_SID = n()) %>%
  # summarise to percentage of each square per horizon 
  dplyr::group_by(SID, pos) %>%
<<<<<<< HEAD
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
=======
  dplyr::summarise(
    part = round(unique(n()/n_SID * 100), 1)
  ) %>%
  dplyr::ungroup() %>%
  # transform long to wide data structure
  tidyr::spread(pos, part)

#### write result to .csv file

write.csv(perc, file = "output/attribution.csv", row.names = FALSE)
>>>>>>> f41efd20c9a56b14021a474a6068fd69a0e13803
