# Running nonlinear biomass model tests for different combinations of functional groups and ecoregions

# needleLeavedTree - trimmed predictors at 90% and trimmed response ---------------
# forest
rmarkdown::render(input = "./Analysis/BiomassQuantity/Analysis/NonlinearModelTesting.Rmd", 
                  params = list(  readParams = TRUE,
                                  coverType = "needleLeavedTreeCover",
                                  ecoregion = "forest", # dryShrubGrass
                                  trimPreds= TRUE,
                                  trimThreshold= 0.95,
                                  curtailResponse=FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/BiomassQuantity/Analysis/modelRunMarkdowns/",
                  output_file = "NonlinearModelTesting_needleLeavedTree_forest_trimmedPreds5_noTrimmedResponse.html")
# grassShrub
rmarkdown::render(input = "./Analysis/BiomassQuantity/Analysis/NonlinearModelTesting.Rmd", 
                  params = list(  readParams = TRUE,
                                  coverType = "needleLeavedTreeCover",
                                  ecoregion = "dryShrubGrass", # dryShrubGrass
                                  trimPreds= TRUE,
                                  trimThreshold= 0.95,
                                  curtailResponse=FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/BiomassQuantity/Analysis/modelRunMarkdowns/",
                  output_file = "NonlinearModelTesting_needleLeavedTree_dryShrubGrass_trimmedPreds5_noTrimmedResponse.html")
# CONUS
rmarkdown::render(input = "./Analysis/BiomassQuantity/Analysis/NonlinearModelTesting.Rmd", 
                  params = list(  readParams = TRUE,
                                  coverType = "needleLeavedTreeCover",
                                  ecoregion = "CONUS",
                                  trimPreds= TRUE,
                                  trimThreshold= 0.95,
                                  curtailResponse=FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/BiomassQuantity/Analysis/modelRunMarkdowns/",
                  output_file = "NonlinearModelTesting_needleLeavedTree_CONUS_trimmedPreds5_noTrimmedResponse.html")

# broadLeavedTree - trimmed predictors at 90% and trimmed response ---------------
# forest
rmarkdown::render(input = "./Analysis/BiomassQuantity/Analysis/NonlinearModelTesting.Rmd", 
                  params = list(  readParams = TRUE,
                                  coverType = "broadLeavedTreeCover",
                                  ecoregion = "forest", # dryShrubGrass
                                  trimPreds= TRUE,
                                  trimThreshold= 0.95,
                                  curtailResponse=FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/BiomassQuantity/Analysis/modelRunMarkdowns/",
                  output_file = "NonlinearModelTesting_BroadLeavedTree_forest_trimmedPreds5_noTrimmedResponse.html")
# grassShrub
rmarkdown::render(input = "./Analysis/BiomassQuantity/Analysis/NonlinearModelTesting.Rmd", 
                  params = list(  readParams = TRUE,
                                  coverType = "broadLeavedTreeCover",
                                  ecoregion = "dryShrubGrass", # dryShrubGrass
                                  trimPreds= TRUE,
                                  trimThreshold= 0.95,
                                  curtailResponse=FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/BiomassQuantity/Analysis/modelRunMarkdowns/",
                  output_file = "NonlinearModelTesting_BroadLeavedTree_dryShrubGrass_trimmedPreds5_noTrimmedResponse.html")
# CONUS
rmarkdown::render(input = "./Analysis/BiomassQuantity/Analysis/NonlinearModelTesting.Rmd", 
                  params = list(  readParams = TRUE,
                                  coverType = "broadLeavedTreeCover",
                                  ecoregion = "CONUS",
                                  trimPreds= TRUE,
                                  trimThreshold= 0.95,
                                  curtailResponse=FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/BiomassQuantity/Analysis/modelRunMarkdowns/",
                  output_file = "NonlinearModelTesting_BroadLeavedTree_CONUS_trimmedPreds5_noTrimmedResponse.html")

# C3GramCover - trimmed predictors at 90% and trimmed response ---------------
# forest
rmarkdown::render(input = "./Analysis/BiomassQuantity/Analysis/NonlinearModelTesting.Rmd", 
                  params = list(  readParams = TRUE,
                                  coverType = "C3GramCover",
                                  ecoregion = "forest", # dryShrubGrass
                                  trimPreds= TRUE,
                                  trimThreshold= 0.95,
                                  curtailResponse=FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/BiomassQuantity/Analysis/modelRunMarkdowns/",
                  output_file = "NonlinearModelTesting_C3GramCover_forest_trimmedPreds5_noTrimmedResponse.html")
# grassShrub
rmarkdown::render(input = "./Analysis/BiomassQuantity/Analysis/NonlinearModelTesting.Rmd", 
                  params = list(  readParams = TRUE,
                                  coverType = "C3GramCover",
                                  ecoregion = "dryShrubGrass", # dryShrubGrass
                                  trimPreds= TRUE,
                                  trimThreshold= 0.95,
                                  curtailResponse=FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/BiomassQuantity/Analysis/modelRunMarkdowns/",
                  output_file = "NonlinearModelTesting_C3GramCover_dryShrubGrass_trimmedPreds5_noTrimmedResponse.html")
# CONUS
rmarkdown::render(input = "./Analysis/BiomassQuantity/Analysis/NonlinearModelTesting.Rmd", 
                  params = list(  readParams = TRUE,
                                  coverType = "C3GramCover",
                                  ecoregion = "CONUS",
                                  trimPreds= TRUE,
                                  trimThreshold= 0.90,
                                  curtailResponse=FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/BiomassQuantity/Analysis/modelRunMarkdowns/",
                  output_file = "NonlinearModelTesting_C3GramCover_CONUS_trimmedPreds10_noTrimmedResponse.html")

# C4GramCover - trimmed predictors at 90% and trimmed response ---------------
# forest
rmarkdown::render(input = "./Analysis/BiomassQuantity/Analysis/NonlinearModelTesting.Rmd", 
                  params = list(  readParams = TRUE,
                                  coverType = "C4GramCover",
                                  ecoregion = "forest", # dryShrubGrass
                                  trimPreds= TRUE,
                                  trimThreshold= 0.95,
                                  curtailResponse=FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/BiomassQuantity/Analysis/modelRunMarkdowns/",
                  output_file = "NonlinearModelTesting_C4GramCover_forest_trimmedPreds5_noTrimmedResponse.html")
# grassShrub
rmarkdown::render(input = "./Analysis/BiomassQuantity/Analysis/NonlinearModelTesting.Rmd", 
                  params = list(  readParams = TRUE,
                                  coverType = "C4GramCover",
                                  ecoregion = "dryShrubGrass", # dryShrubGrass
                                  trimPreds= TRUE,
                                  trimThreshold= 0.95,
                                  curtailResponse=FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/BiomassQuantity/Analysis/modelRunMarkdowns/",
                  output_file = "NonlinearModelTesting_C4GramCover_dryShrubGrass_trimmedPreds5_noTrimmedResponse.html")
# CONUS
rmarkdown::render(input = "./Analysis/BiomassQuantity/Analysis/NonlinearModelTesting.Rmd", 
                  params = list(  readParams = TRUE,
                                  coverType = "C4GramCover",
                                  ecoregion = "CONUS",
                                  trimPreds= TRUE,
                                  trimThreshold= 0.95,
                                  curtailResponse=FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/BiomassQuantity/Analysis/modelRunMarkdowns/",
                  output_file = "NonlinearModelTesting_C4GramCover_CONUS_trimmedPreds5_noTrimmedResponse.html")

# forbCover - trimmed predictors at 90% and trimmed response ---------------
# forest
rmarkdown::render(input = "./Analysis/BiomassQuantity/Analysis/NonlinearModelTesting.Rmd", 
                  params = list(  readParams = TRUE,
                                  coverType = "forbCover",
                                  ecoregion = "forest", # dryShrubGrass
                                  trimPreds= TRUE,
                                  trimThreshold= 0.95,
                                  curtailResponse=FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/BiomassQuantity/Analysis/modelRunMarkdowns/",
                  output_file = "NonlinearModelTesting_forbCover_forest_trimmedPreds5_noTrimmedResponse.html")
# grassShrub
rmarkdown::render(input = "./Analysis/BiomassQuantity/Analysis/NonlinearModelTesting.Rmd", 
                  params = list(  readParams = TRUE,
                                  coverType = "forbCover",
                                  ecoregion = "dryShrubGrass", # dryShrubGrass
                                  trimPreds= TRUE,
                                  trimThreshold= 0.95,
                                  curtailResponse=FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/BiomassQuantity/Analysis/modelRunMarkdowns/",
                  output_file = "NonlinearModelTesting_forbCover_dryShrubGrass_trimmedPreds5_noTrimmedResponse.html")
# CONUS
rmarkdown::render(input = "./Analysis/BiomassQuantity/Analysis/NonlinearModelTesting.Rmd", 
                  params = list(  readParams = TRUE,
                                  coverType = "forbCover",
                                  ecoregion = "CONUS",
                                  trimPreds= TRUE,
                                  trimThreshold= 0.95,
                                  curtailResponse=FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/BiomassQuantity/Analysis/modelRunMarkdowns/",
                  output_file = "NonlinearModelTesting_forbCover_CONUS_trimmedPreds5_noTrimmedResponse.html")


# shrubCover - trimmed predictors at 90% and trimmed response ---------------
# forest
rmarkdown::render(input = "./Analysis/BiomassQuantity/Analysis/NonlinearModelTesting.Rmd", 
                  params = list(  readParams = TRUE,
                                  coverType = "shrubCover",
                                  ecoregion = "forest", # dryShrubGrass
                                  trimPreds= TRUE,
                                  trimThreshold= 0.95,
                                  curtailResponse=FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/BiomassQuantity/Analysis/modelRunMarkdowns/",
                  output_file = "NonlinearModelTesting_shrubCover_forest_trimmedPreds5_noTrimmedResponse.html")
# grassShrub
rmarkdown::render(input = "./Analysis/BiomassQuantity/Analysis/NonlinearModelTesting.Rmd", 
                  params = list(  readParams = TRUE,
                                  coverType = "shrubCover",
                                  ecoregion = "dryShrubGrass", # dryShrubGrass
                                  trimPreds= TRUE,
                                  trimThreshold= 0.95,
                                  curtailResponse=FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/BiomassQuantity/Analysis/modelRunMarkdowns/",
                  output_file = "NonlinearModelTesting_shrubCover_dryShrubGrass_trimmedPreds5_noTrimmedResponse.html")
# CONUS
rmarkdown::render(input = "./Analysis/BiomassQuantity/Analysis/NonlinearModelTesting.Rmd", 
                  params = list(  readParams = TRUE,
                                  coverType = "shrubCover",
                                  ecoregion = "CONUS",
                                  trimPreds= TRUE,
                                  trimThreshold= 0.95,
                                  curtailResponse=FALSE), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/BiomassQuantity/Analysis/modelRunMarkdowns/",
                  output_file = "NonlinearModelTesting_shrubCover_CONUS_trimmedPreds5_noTrimmedResponse.html")
