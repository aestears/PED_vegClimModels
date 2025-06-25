# # this script runs the '02_ModelFitting.Rmd" file for each cover variable and saves the output
# 
# Shrub/grass ecoregion ; yes trim anomalies---------------------------------------------------
# total herbaceous cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "shrubGrass",
                                  response = "TotalHerbaceousCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_TotalHerbaceousCover_trimAnoms_LASSO_gammaLogLink.html")

#total tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "shrubGrass",
                                  response = "TotalTreeCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_TotalTreeCover_trimAnoms_LASSO_gammaLogLink.html")

#total shrub cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "shrubGrass",
                                  response = "ShrubCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_ShrubCover_trimAnoms_LASSO_gammaLogLink.html")

# #CAM cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd",
#                   params = list(  run= TRUE,
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = TRUE,
#                                   ecoregion = "shrubGrass",
#                                   response = "CAMCover",
#                                   removeTexasLouisianaPlain = FALSE),
#                   output_format = "html_document",
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "GrassShrub_CAMCover_trimAnoms_LASSO_gammaLogLink.html")

#bare ground cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "shrubGrass",
                                  response = "BareGroundCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_BareGroundCover_trimAnoms_LASSO_gammaLogLink.html")

#C4 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "shrubGrass",
                                  response = "C4GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_c4GramCover_trimAnoms_LASSO_gammaLogLink.html")
#C3 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "shrubGrass",
                                  response = "C3GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_c3GramCover_trimAnoms_LASSO_gammaLogLink.html")
#broad leaved tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "shrubGrass",
                                  response = "AngioTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_AngioTreeCover_trimAnoms_LASSO_gammaLogLink.html")
#conifer tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "shrubGrass",
                                  response = "ConifTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_ConifTreeCover_trimAnoms_LASSO_gammaLogLink.html")
#forb cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "shrubGrass",
                                  response = "ForbCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_ForbCover_trimAnoms_LASSO_gammaLogLink.html")
# forest ecoregion; yes trim anomalies- --------------------------------------------------------

# total herbaceous cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "forest",
                                  response = "TotalHerbaceousCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "forest_TotalHerbaceousCover_trimAnoms_LASSO_gammaLogLink.html")

#total tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "forest",
                                  response = "TotalTreeCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "forest_TotalTreeCover_trimAnoms_LASSO_gammaLogLink.html")

#total shrub cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "forest",
                                  response = "ShrubCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "forest_ShrubCover_trimAnoms_LASSO_gammaLogLink.html")

# #CAM cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd",
#                   params = list(  run= TRUE,
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = TRUE,
#                                   ecoregion = "forest",
#                                   response = "CAMCover",
#                                   removeTexasLouisianaPlain = FALSE),
#                   output_format = "html_document",
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "forest_CAMCover_trimAnoms_LASSO_gammaLogLink.html")

#bare ground cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run = TRUE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = TRUE,
#                                   ecoregion = "forest",
#                                   response = "BareGroundCover",
#                                   removeTexasLouisianaPlain = FALSE), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "forest_BareGroundCover_trimAnoms_LASSO_gammaLogLink.html")

#C4 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "forest",
                                  response = "C4GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "forest_c4GramCover_trimAnoms_LASSO_gammaLogLink.html")
#C3 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "forest",
                                  response = "C3GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "forest_c3GramCover_trimAnoms_LASSO_gammaLogLink.html")
#broad leaved tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "forest",
                                  response = "AngioTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "forest_AngioTreeCover_trimAnoms_LASSO_gammaLogLink.html")
#conifer tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "forest",
                                  response = "ConifTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "forest_ConifTreeCover_trimAnoms_LASSO_gammaLogLink.html")
#forb cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "forest",
                                  response = "ForbCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "forest_ForbCover_trimAnoms_LASSO_gammaLogLink.html")

# CONUS-wide models; yes trim anomalies- ------------------------------------------------

# #CAM cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run = TRUE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = TRUE,
#                                   ecoregion = "CONUS",
#                                   response = "CAMCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "CONUS_CAMCover_trimAnoms_LASSO_gammaLogLink.html")

# # total herbaceous cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run = TRUE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = TRUE,
#                                   ecoregion = "CONUS",
#                                   response = "TotalHerbaceousCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "CONUS_TotalHerbaceousCover_trimAnoms_LASSO_gammaLogLink.html")
# 
# # total tree cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run = TRUE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = TRUE,
#                                   ecoregion = "CONUS",
#                                   response = "TotalTreeCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "CONUS_TotalTreeCover_trimAnoms_LASSO_gammaLogLink.html")
# 
# 
# 
# total shrub cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd",
                  params = list(  run = TRUE,
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "ShrubCover"),
                  output_format = "html_document",
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "CONUS_ShrubCover_trimAnoms_LASSO_gammaLogLink.html")
# 
# 
# total bare ground cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd",
                  params = list(  run = TRUE,
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "BareGroundCover"),
                  output_format = "html_document",
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "CONUS_BareGroundCover_trimAnoms_LASSO_gammaLogLink.html")

#C4 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "C4GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "CONUS_c4GramCover_trimAnoms_LASSO_gammaLogLink.html")
#C3 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "C3GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "CONUS_c3GramCover_trimAnoms_LASSO_gammaLogLink.html")
#broad leaved tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "AngioTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "CONUS_AngioTreeCover_trimAnoms_LASSO_gammaLogLink.html")
#conifer tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "ConifTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "CONUS_ConifTreeCover_trimAnoms_LASSO_gammaLogLink.html")
#forb cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "ForbCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "CONUS_ForbCover_trimAnoms_LASSO_gammaLogLink.html")
# shrub models in forests - east forest and west forest yes AND no trim anomalies -----------------------
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run = TRUE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = TRUE,
#                                   ecoregion = "eastForest",
#                                   response = "ShrubCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "eastForest_ShrubCover_trimAnoms_LASSO_gammaLogLink.html")
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run = TRUE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = FALSE,
#                                   ecoregion = "eastForest",
#                                   response = "ShrubCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "eastForest_ShrubCover_NoTrimAnoms_LASSO_gammaLogLink.html")
# 
# 
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run = TRUE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = TRUE,
#                                   ecoregion = "westForest",
#                                   response = "ShrubCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "westForest_ShrubCover_trimAnoms_LASSO_gammaLogLink.html")
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run = TRUE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = FALSE,
#                                   ecoregion = "westForest",
#                                   response = "ShrubCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "westForest_ShrubCover_NoTrimAnoms_LASSO_gammaLogLink.html")
# 
# # Shrub/grass ecoregion ; NO trim anomalies---------------------------------------------------
# total herbaceous cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = FALSE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "shrubGrass",
                                  response = "TotalHerbaceousCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_TotalHerbaceousCover_NoTrimAnoms_LASSO_gammaLogLink.html")

#total tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = FALSE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "shrubGrass",
                                  response = "TotalTreeCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_TotalTreeCover_NoTrimAnoms_LASSO_gammaLogLink.html")

#total shrub cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = FALSE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "shrubGrass",
                                  response = "ShrubCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_ShrubCover_NoTrimAnoms_LASSO_gammaLogLink.html")

# #CAM cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd",
#                   params = list(  run = FALSE,
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = FALSE,
#                                   ecoregion = "shrubGrass",
#                                   response = "CAMCover",
#                                   removeTexasLouisianaPlain = FALSE),
#                   output_format = "html_document",
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "GrassShrub_CAMCover_NoTrimAnoms_LASSO_gammaLogLink.html")

#bare ground cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = FALSE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "shrubGrass",
                                  response = "BareGroundCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_BareGroundCover_NoTrimAnoms_LASSO_gammaLogLink.html")

#C4 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "shrubGrass",
                                  response = "C4GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_c4GramCover_NoTrimAnoms_LASSO_gammaLogLink.html")
#C3 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "shrubGrass",
                                  response = "C3GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_c3GramCover_NoTrimAnoms_LASSO_gammaLogLink.html")
#broad leaved tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "shrubGrass",
                                  response = "AngioTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_AngioTreeCover_NoTrimAnoms_LASSO_gammaLogLink.html")
#conifer tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "shrubGrass",
                                  response = "ConifTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_ConifTreeCover_NoTrimAnoms_LASSO_gammaLogLink.html")
#forb cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "shrubGrass",
                                  response = "ForbCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_ForbCover_NoTrimAnoms_LASSO_gammaLogLink.html")
# forest ecoregion; no trim anomalies- --------------------------------------------------------
# total herbaceous cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = FALSE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "forest",
                                  response = "TotalHerbaceousCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "forest_TotalHerbaceousCover_NoTrimAnoms_LASSO_gammaLogLink.html")

#total tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = FALSE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "forest",
                                  response = "TotalTreeCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "forest_TotalTreeCover_NoTrimAnoms_LASSO_gammaLogLink.html")

#total shrub cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = FALSE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "forest",
                                  response = "ShrubCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "forest_ShrubCover_NoTrimAnoms_LASSO_gammaLogLink.html")

# #CAM cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd",
#                   params = list(  run = FALSE,
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = FALSE,
#                                   ecoregion = "forest",
#                                   response = "CAMCover",
#                                   removeTexasLouisianaPlain = FALSE),
#                   output_format = "html_document",
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "forest_CAMCover_NoTrimAnoms_LASSO_gammaLogLink.html")

# #bare ground cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run = FALSE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = FALSE,
#                                   ecoregion = "forest",
#                                   response = "BareGroundCover",
#                                   removeTexasLouisianaPlain = FALSE), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "forest_BareGroundCover_NoTrimAnoms_LASSO_gammaLogLink.html")
# 
#C4 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "forest",
                                  response = "C4GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "Forest_c4GramCover_NoTrimAnoms_LASSO_gammaLogLink.html")
#C3 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "forest",
                                  response = "C3GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "Forest_c3GramCover_NoTrimAnoms_LASSO_gammaLogLink.html")
#broad leaved tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "forest",
                                  response = "AngioTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "Forest_AngioTreeCover_NoTrimAnoms_LASSO_gammaLogLink.html")
#conifer tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "forest",
                                  response = "ConifTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "Forest_ConifTreeCover_NoTrimAnoms_LASSO_gammaLogLink.html")
#forb cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "forest",
                                  response = "ForbCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "Forest_ForbCover_NoTrimAnoms_LASSO_gammaLogLink.html")

# CONUS-wide models; no trim anomalies- ------------------------------------------------


# #CAM cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run = FALSE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = FALSE,
#                                   ecoregion = "CONUS",
#                                   response = "CAMCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "CONUS_CAMCover_NoTrimAnoms_LASSO_gammaLogLink.html")

# # total herbaceous cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run = FALSE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = FALSE,
#                                   ecoregion = "CONUS",
#                                   response = "TotalHerbaceousCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "CONUS_TotalHerbaceousCover_NoTrimAnoms_LASSO_gammaLogLink.html")

# # total tree cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run = FALSE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = FALSE,
#                                   ecoregion = "CONUS",
#                                   response = "TotalTreeCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "CONUS_TotalTreeCover_NoTrimAnoms_LASSO_gammaLogLink.html")



# total shrub cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = FALSE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "CONUS",
                                  response = "ShrubCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "CONUS_ShrubCover_NoTrimAnoms_LASSO_gammaLogLink.html")

# total bare ground cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = FALSE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "CONUS",
                                  response = "BareGroundCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "CONUS_BareGroundCover_NoTrimAnoms_LASSO_gammaLogLink.html")

#C4 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "CONUS",
                                  response = "C4GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "CONUS_c4GramCover_NoTrimAnoms_LASSO_gammaLogLink.html")
#C3 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "CONUS",
                                  response = "C3GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "CONUS_c3GramCover_NoTrimAnoms_LASSO_gammaLogLink.html")
#broad leaved tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "CONUS",
                                  response = "AngioTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "CONUS_AngioTreeCover_NoTrimAnoms_LASSO_gammaLogLink.html")
#conifer tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "CONUS",
                                  response = "ConifTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "CONUS_ConifTreeCover_NoTrimAnoms_LASSO_gammaLogLink.html")
#forb cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "CONUS",
                                  response = "ForbCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "CONUS_ForbCover_NoTrimAnoms_LASSO_gammaLogLink.html")

