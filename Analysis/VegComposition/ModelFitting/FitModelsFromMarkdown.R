# # this script runs the '02_ModelFitting.Rmd" file for each cover variable and saves the output
# 
# # Shrub/grass ecoregion ; no trim anomalies ---------------------------------------------------
# # total herbaceous cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run= TRUE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = FALSE,
#                                   ecoregion = "shrubGrass",
#                                   response = "TotalHerbaceousCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "GrassShrub_TotalHerbaceousCover_LASSO_gammaLogLink.html")
# 
# #total tree cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run= TRUE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = FALSE,
#                                   ecoregion = "shrubGrass",
#                                   response = "TotalTreeCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "GrassShrub_TotalTreeCover_LASSO_gammaLogLink.html")
# 
# #total shrub cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run= TRUE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = FALSE,
#                                   ecoregion = "shrubGrass",
#                                   response = "ShrubCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "GrassShrub_ShrubCover_LASSO_gammaLogLink.html")
# 
# # #CAM cover
# # rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
# #                   params = list(  run= TRUE, 
# #                                   test_run = FALSE,
# #                                   save_figs = FALSE,
# #                                   ecoregion = "shrubGrass",
# #                                   response = "CAMCover"), 
# #                   output_format = "html_document", 
# #                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
# #                   output_file = "GrassShrub_CAMCover_LASSO_gammaLogLink.html")
# 
# #bare ground cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run= TRUE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = FALSE,
#                                   ecoregion = "shrubGrass",
#                                   response = "BareGroundCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "GrassShrub_BareGroundCover_LASSO_gammaLogLink.html")
# 
# 
# # forest ecoregion; no trim anomalies --------------------------------------------------------
# 
# # total herbaceous cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run= TRUE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = FALSE,
#                                   ecoregion = "forest",
#                                   response = "TotalHerbaceousCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "forest_TotalHerbaceousCover_LASSO_gammaLogLink.html")
# 
# #total tree cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run= TRUE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = FALSE,
#                                   ecoregion = "forest",
#                                   response = "TotalTreeCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "forest_TotalTreeCover_LASSO_gammaLogLink.html")
# 
# #total shrub cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run= TRUE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = FALSE,
#                                   ecoregion = "forest",
#                                   response = "ShrubCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "forest_ShrubCover_LASSO_gammaLogLink.html")
# 
# #bare ground cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run= TRUE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = FALSE,
#                                   ecoregion = "forest",
#                                   response = "BareGroundCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "forest_BareGroundCover_LASSO_gammaLogLink.html")
# 
# 
# 
# # CONUS-wide modes; no trim anomalies ------------------------------------------------
# 
# 
# #CAM cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run= TRUE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = FALSE,
#                                   ecoregion = "CONUS",
#                                   response = "CAMCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "CONUS_CAMCover_LASSO_gammaLogLink.html")
# 
# 
# # total herbaceous cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run = TRUE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = FALSE,
#                                   ecoregion = "CONUS",
#                                   response = "TotalHerbaceousCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "CONUS_TotalHerbaceousCover_LASSO_gammaLogLink.html")
# 
# # total tree cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run = TRUE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = FALSE,
#                                   ecoregion = "CONUS",
#                                   response = "TotalTreeCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "CONUS_TotalTreeCover_LASSO_gammaLogLink.html")
# 
# # total shrub cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run = TRUE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = FALSE,
#                                   ecoregion = "CONUS",
#                                   response = "ShrubCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "CONUS_ShrubCover_LASSO_gammaLogLink.html")
# 
# # bare ground cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run = TRUE, 
#                                   test_run = FALSE,
#                                   save_figs = FALSE,
#                                   trimAnomalies = FALSE,
#                                   ecoregion = "CONUS",
#                                   response = "BareGroundCover"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "CONUS_BareGroundCover_LASSO_gammaLogLink.html")

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

#CAM cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd",
                  params = list(  run= TRUE,
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  ecoregion = "shrubGrass",
                                  response = "CAMCover",
                                  removeTexasLouisianaPlain = FALSE),
                  output_format = "html_document",
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_CAMCover_trimAnoms_LASSO_gammaLogLink.html")

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

#CAM cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd",
                  params = list(  run= TRUE,
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  ecoregion = "forest",
                                  response = "CAMCover",
                                  removeTexasLouisianaPlain = FALSE),
                  output_format = "html_document",
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "forest_CAMCover_trimAnoms_LASSO_gammaLogLink.html")

#bare ground cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "forest",
                                  response = "BareGroundCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "forest_BareGroundCover_trimAnoms_LASSO_gammaLogLink.html")



# CONUS-wide model for CAM; yes trim anomalies- ------------------------------------------------


#CAM cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "CAMCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "CONUS_CAMCover_trimAnoms_LASSO_gammaLogLink.html")

# total herbaceous cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "TotalHerbaceousCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "CONUS_TotalHerbaceousCover_trimAnoms_LASSO_gammaLogLink.html")

# total tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "TotalTreeCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "CONUS_TotalTreeCover_trimAnoms_LASSO_gammaLogLink.html")



# total shrub cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "CONUS",
                                  response = "ShrubCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "CONUS_ShrubCover_trimAnoms_LASSO_gammaLogLink.html")


# total bare ground cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "CONUS",
                                  response = "BareGroundCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "CONUS_BareGroundCover_trimAnoms_LASSO_gammaLogLink.html")

# CONUS total trees - remove Texas Louisiana Coastal Plain ----------------
# total tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "TotalTreeCover",
                                  removeTexasLouisianaPlain =TRUE
                                  ), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "CONUS_TotalTreeCover_trimAnoms_REMOVETLP_LASSO_gammaLogLink.html")

# grassShrub total trees - remove Texas Louisiana Coastal Plain ----------------
# total tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "shrubGrass",
                                  response = "TotalTreeCover",
                                  removeTexasLouisianaPlain =TRUE
                  ), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_TotalTreeCover_trimAnoms_REMOVETLP_LASSO_gammaLogLink.html")

# grassShrub total trees - remove Texas Louisiana Coastal Plain ----------------
# total tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "shrubGrass",
                                  response = "ShrubCover",
                                  removeTexasLouisianaPlain =TRUE
                  ), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_ShrubCover_trimAnoms_REMOVETLP_LASSO_gammaLogLink.html")

