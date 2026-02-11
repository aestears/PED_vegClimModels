# # this script runs the '02_ModelFitting.Rmd" file for each cover variable and saves the output
# 
setwd("/Users/astears/Documents/Dropbox_static/Work/NAU_USGS_postdoc/cleanPED/PED_vegClimModels/")

# Beta-version: noTree ; yes trim anomalies---------------------------------------------------
# total herbaceous cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = FALSE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "noTrees",
                                  response = "TotalHerbaceousCover",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = FALSE,
                                  whichSecondBestMod= "auto"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_NoTrees_TotalHerbaceousCover_trimAnom.html")
# 
# #total tree cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
#                   params = list(  run = TRUE, 
#                                   
#                                   save_figs = FALSE,
#                                   trimAnomalies = TRUE,
#                                   ecoregion = "noTrees",
#                                   response = "TotalTreeCover",
#                                   removeTexasLouisianaPlain = FALSE,
#                                   removeAllAnoms = FALSE,
#                                   whichSecondBestMod= "auto"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "betaLASSO__NoTrees_TotalTreeCover.html")

# #total shrub cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
#                   params = list(  run = TRUE, 
#                                   
#                                   save_figs = FALSE,
#                                   trimAnomalies = TRUE,
#                                   ecoregion = "noTrees",
#                                   response = "ShrubCover",
#                                   removeTexasLouisianaPlain = FALSE,
#                                   removeAllAnoms = FALSE,
#                                   whichSecondBestMod= "auto"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "betaLASSO_NoTrees_ShrubCover_trimAnoms.html")

# #bare ground cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
#                   params = list(  run = TRUE, 
#                                   
#                                   save_figs = FALSE,
#                                   trimAnomalies = TRUE,
#                                   ecoregion = "noTrees",
#                                   response = "BareGroundCover",
#                                   removeTexasLouisianaPlain = FALSE,
#                                   removeAllAnoms = FALSE,
#                                   whichSecondBestMod= "auto"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "betaLASSO_NoTrees_BareGroundCover_trimAnoms.html")

#C4 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "noTrees",
                                  response = "C4GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = FALSE,
                                  whichSecondBestMod= "auto"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_NoTrees_C4GramCover_trimAnoms.html")
#C3 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "noTrees",
                                  response = "C3GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = FALSE,
                                  whichSecondBestMod= "auto"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_NoTrees_C3GramCover_trimAnoms.html")
# #broad leaved tree cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
#                   params = list(  run = TRUE, 
#                                   
#                                   save_figs = FALSE,
#                                   trimAnomalies = TRUE,
#                                   ecoregion = "shrubGrass",
#                                   response = "AngioTreeCover_prop",
#                                   removeTexasLouisianaPlain = FALSE,
#                                   removeAllAnoms = FALSE,
#                                   whichSecondBestMod= "auto"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "betaLASSO_GrassShrub_AngioTreeCover_prop.html")
# #conifer tree cover
# rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
#                   params = list(  run = TRUE, 
#                                   
#                                   save_figs = FALSE,
#                                   trimAnomalies = TRUE,
#                                   ecoregion = "shrubGrass",
#                                   response = "ConifTreeCover_prop",
#                                   removeTexasLouisianaPlain = FALSE,
#                                   removeAllAnoms = FALSE,
#                                   whichSecondBestMod= "auto"), 
#                   output_format = "html_document", 
#                   output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
#                   output_file = "betaLASSO_GrassShrub_ConifTreeCover_prop.html")
#forb cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "noTrees",
                                  response = "ForbCover_prop",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = FALSE,
                                  whichSecondBestMod= "auto"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_NoTrees_ForbCover_trimAnoms.html")
#Beta-version:  yes trees ecoregion; yes trim anomalies- --------------------------------------------------------

# total herbaceous cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "trees",
                                  response = "TotalHerbaceousCover",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = FALSE,
                                  whichSecondBestMod= "auto"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_YesTrees_TotalHerbaceousCover_trimAnom.html")

#total tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "trees",
                                  response = "TotalTreeCover",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = FALSE,
                                  whichSecondBestMod= "auto"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_YesTrees_TotalTreeCover_trimAnom.html"
)

#total shrub cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "trees",
                                  response = "ShrubCover",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = FALSE,
                                  whichSecondBestMod= "auto"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_YesTrees_ShrubCover_trimAnom.html")


#C4 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "trees",
                                  response = "C4GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = FALSE,
                                  whichSecondBestMod= "auto"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_YesTrees_C4GramCover_prop_trimAnom.html")
#C3 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "trees",
                                  response = "C3GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = FALSE,
                                  whichSecondBestMod= "auto"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_YesTrees_C3GramCover_prop_trimAnom.html")
#broad leaved tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "trees",
                                  response = "AngioTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = FALSE,
                                  whichSecondBestMod= "auto"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_YesTrees_AngioTreeCover_prop_trimAnom.html")
#conifer tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "trees",
                                  response = "ConifTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = FALSE,
                                  whichSecondBestMod= "auto"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_YesTrees_ConifTreeCover_prop_trimAnom.html")
#forb cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "trees",
                                  response = "ForbCover_prop",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = FALSE,
                                  whichSecondBestMod= "auto"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_YesTrees_ForbCover_prop_trimAnom.html")

#Beta-version:  CONUS-wide models; yes trim anomalies- ------------------------------------------------
# total herbaceous cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "TotalHerbaceousCover",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = FALSE,
                                  whichSecondBestMod= "auto"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_CONUS_TotalHerbaceousCover_trimAnom.html")
# total shrub cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd",
                  params = list(  run = TRUE,
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "ShrubCover",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = FALSE,
                                  whichSecondBestMod= "auto"),
                  output_format = "html_document",
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_CONUS_ShrubCover_trimAnom.html")
# 
# 
# total bare ground cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd",
                  params = list(  run = TRUE,
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "BareGroundCover",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = FALSE,
                                  whichSecondBestMod= "auto"),
                  output_format = "html_document",
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_CONUS_BareGroundCover_trimAnom.html")

#C4 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "C4GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = FALSE,
                                  whichSecondBestMod= "auto"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_CONUS_C4GramCover_prop_trimAnom.html")
#C3 graminoid cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "C3GramCover_prop",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = FALSE,
                                  whichSecondBestMod= "auto"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_CONUS_C3GramCover_prop_trimAnom.html")
#broad leaved tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "AngioTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = FALSE,
                                  whichSecondBestMod= "auto"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_CONUS_AngioTreeCover_prop_trimAnom.html")
#conifer tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "ConifTreeCover_prop",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = FALSE,
                                  whichSecondBestMod= "auto"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_CONUS_ConifTreeCover_prop_trimAnom.html")
#forb cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = TRUE, 
                                  
                                  save_figs = FALSE,
                                  trimAnomalies = TRUE,
                                  ecoregion = "CONUS",
                                  response = "ForbCover_prop",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = FALSE,
                                  whichSecondBestMod= "auto"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_CONUS_ForbCover_prop_trimAnom.html")


# Beta-version: total tree models that exclude anomalies ----------------------
#grass/shrub
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = FALSE, 
                                  save_figs = TRUE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "shrubGrass",
                                  response = "TotalTreeCover",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = TRUE,
                                  whichSecondBestMod= "auto"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_GrassShrub_TotalTreeCover_removeAllAnoms.html")
#forest
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run = FALSE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "forest",
                                  response = "TotalTreeCover",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = TRUE,
                                  whichSecondBestMod = "halfse"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_YesTrees_TotalTreeCover_removeAllAnoms.html")
#CONUS
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/03_modelFitting_testingBetaLASSO.Rmd", 
                  params = list(  run =  TRUE,
                                  save_figs = FALSE,
                                  trimAnomalies = FALSE,
                                  ecoregion = "CONUS",
                                  response = "TotalTreeCover",
                                  removeTexasLouisianaPlain = FALSE,
                                  removeAllAnoms = TRUE,
                                  whichSecondBestMod= "auto"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "betaLASSO_CONUS_TotalTreeCover_removeAllAnoms.html")


