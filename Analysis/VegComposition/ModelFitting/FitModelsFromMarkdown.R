# # this script runs the '02_ModelFitting.Rmd" file for each cover variable and saves the output
# 
# Shrub/grass ecoregion ; yes trim anomalies---------------------------------------------------
# total herbaceous cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = FALSE, 
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
                  params = list(  run = FALSE, 
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
                  params = list(  run = FALSE, 
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
                  params = list(  run = FALSE, 
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
                  params = list(  run = FALSE, 
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
                  params = list(  run = FALSE, 
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
                  params = list(  run = FALSE, 
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
                  params = list(  run = FALSE, 
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
                  params = list(  run = FALSE, 
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
                  params = list(  run = FALSE, 
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
rmarkdown::render(input = "Documents/Dropbox_static/Work/NAU_USGS_postdoc/PED_vegClimModels/Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = TRUE, 
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "forest",
                                  response = "TotalTreeCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                    output_dir = "Documents/Dropbox_static/Work/NAU_USGS_postdoc/PED_vegClimModels/Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "gammaLASSO_test_forest_totalTree.html"
                    #"forest_TotalTreeCover_trimAnoms_LASSO_gammaLogLink.html"
                    )

#total shrub cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run = FALSE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "forest",
                                  response = "ShrubCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "forest_ShrubCover_trimAnoms_LASSO_gammaLogLink.html")

#bare ground cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
#                   params = list(  run = FALSE, 
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
                  params = list(  run = FALSE, 
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
                  params = list(  run = FALSE, 
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
                  params = list(  run = FALSE, 
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
                  params = list(  run = FALSE, 
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
                  params = list(  run = FALSE, 
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
# total shrub cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd",
                  params = list(  run = FALSE,
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
                  params = list(  run = FALSE,
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
                  params = list(  run = FALSE, 
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
                  params = list(  run = FALSE, 
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
                  params = list(  run = FALSE, 
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
                  params = list(  run = FALSE, 
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
                  params = list(  run = FALSE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "ForbCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "CONUS_ForbCover_trimAnoms_LASSO_gammaLogLink.html")


# Beta-version: Shrub/grass ecoregion ; yes trim anomalies---------------------------------------------------
# total herbaceous cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "shrubGrass",
                                  response = "TotalHerbaceousCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_GrassShrub_TotalHerbaceousCover.html")

#total tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "shrubGrass",
                                  response = "TotalTreeCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_GrassShrub_TotalTreeCover.html")

#total shrub cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "shrubGrass",
                                  response = "ShrubCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_GrassShrub_ShrubCover.html")

#bare ground cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "shrubGrass",
                                  response = "BareGroundCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_GrassShrub_BareGroundCover.html")

#C4 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "shrubGrass",
                                  response = "C4GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_GrassShrub_C4GramCover_prop.html")
#C3 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "shrubGrass",
                                  response = "C3GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_GrassShrub_C3GramCover_prop.html")
#broad leaved tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "shrubGrass",
                                  response = "AngioTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_GrassShrub_AngioTreeCover_prop.html")
#conifer tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "shrubGrass",
                                  response = "ConifTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_GrassShrub_ConifTreeCover_prop.html")
#forb cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "shrubGrass",
                                  response = "ForbCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_GrassShrub_ForbCover_prop.html")
#Beta-version:  forest ecoregion; yes trim anomalies- --------------------------------------------------------

# total herbaceous cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "forest",
                                  response = "TotalHerbaceousCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_forest_TotalHerbaceousCover.html")

#total tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "forest",
                                  response = "TotalTreeCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "Documents/Dropbox_static/Work/NAU_USGS_postdoc/PED_vegClimModels/Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_forest_TotalTreeCover.html"
)

#total shrub cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "forest",
                                  response = "ShrubCover",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_forest_ShrubCover.html")


#C4 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "forest",
                                  response = "C4GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_forest_C4GramCover_prop.html")
#C3 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "forest",
                                  response = "C3GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_forest_C3GramCover_prop.html")
#broad leaved tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "forest",
                                  response = "AngioTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_forest_AngioTreeCover_prop.html")
#conifer tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "forest",
                                  response = "ConifTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_forest_ConifTreeCover_prop.html")
#forb cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "forest",
                                  response = "ForbCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_forest_ForbCover_prop.html")

#Beta-version:  CONUS-wide models; yes trim anomalies- ------------------------------------------------

# total shrub cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd",
                  params = list(  run = TRUE,
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "ShrubCover"),
                  output_format = "html_document",
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_CONUS_ShrubCover.html")
# 
# 
# total bare ground cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd",
                  params = list(  run = TRUE,
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "BareGroundCover"),
                  output_format = "html_document",
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_CONUS_BareGroundCover.html")

#C4 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "C4GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_CONUS_C4GramCover_prop.html")
#C3 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "C3GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_CONUS_C3GramCover_prop.html")
#broad leaved tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "AngioTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_CONUS_AngioTreeCover_prop.html")
#conifer tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "ConifTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_CONUS_ConifTreeCover_prop.html")
#forb cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "ForbCover_prop",
                                  removeTexasLouisianaPlain = FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_test_CONUS_ForbCover_prop.html")

