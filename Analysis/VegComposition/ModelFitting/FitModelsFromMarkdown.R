# this script runs the '02_ModelFitting.Rmd" file for each cover variable and saves the output

# Shrub/grass ecoregion ---------------------------------------------------
# total herbaceous cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run= TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  ecoregion = "shrubGrass",
                                  response = "TotalHerbaceousCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_TotalHerbaceousCover_LASSO_gammaLogLink.html")

#total tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run= TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  ecoregion = "shrubGrass",
                                  response = "TotalTreeCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_TotalTreeCover_LASSO_gammaLogLink.html")

#total shrub cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run= TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  ecoregion = "shrubGrass",
                                  response = "ShrubCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_ShrubCover_LASSO_gammaLogLink.html")

#CAM cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run= TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  ecoregion = "shrubGrass",
                                  response = "CAMCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_CAMCover_LASSO_gammaLogLink.html")

#bare ground cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run= TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  ecoregion = "shrubGrass",
                                  response = "BareGroundCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "GrassShrub_BareGroundCover_LASSO_gammaLogLink.html")


# forest ecoregion --------------------------------------------------------

# total herbaceous cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run= TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  ecoregion = "forest",
                                  response = "TotalHerbaceousCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "forest_TotalHerbaceousCover_LASSO_gammaLogLink.html")

#total tree cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run= TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  ecoregion = "forest",
                                  response = "TotalTreeCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "forest_TotalTreeCover_LASSO_gammaLogLink.html")

#total shrub cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run= TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  ecoregion = "forest",
                                  response = "ShrubCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "forest_ShrubCover_LASSO_gammaLogLink.html")

#CAM cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run= TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  ecoregion = "forest",
                                  response = "CAMCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "forest_CAMCover_LASSO_gammaLogLink.html")

#bare ground cover
rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(  run= TRUE, 
                                  test_run = FALSE,
                                  save_figs = FALSE,
                                  ecoregion = "forest",
                                  response = "BareGroundCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "forest_BareGroundCover_LASSO_gammaLogLink.html")
