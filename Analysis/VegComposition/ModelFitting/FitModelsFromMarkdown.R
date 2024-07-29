# this script runs the '02_ModelFitting.Rmd" file for each cover variable and saves the output

rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(response = "TotalTreeCover_dec" ,
                                s = "TotalTreeCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "glm_beta_model_FirstPass_TotalTreeCover.html")

rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(response = "ShrubCover_dec" ,
                                s = "ShrubCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "glm_beta_model_FirstPass_ShrubCover.html")

rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(response = "HerbCover_dec" ,
                                s = "HerbCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "glm_beta_model_FirstPass_HerbCover.html")

rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(response = "TotalGramCover_dec" ,
                                s = "TotalGramCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "glm_beta_model_FirstPass_TotalGramCover.html")

rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(response = "C3GramCover_dec" ,
                                s = "C3GramCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "glm_beta_model_FirstPass_C3GramCover.html")

rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(response = "C4GramCover_dec" ,
                                s = "C4GramCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "glm_beta_model_FirstPass_C4GramCover.html")

rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(response = "ConifTreeCover_dec" ,
                                s = "ConifTreeCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "glm_beta_model_FirstPass_ConifTreeCover.html")

rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(response = "AngioTreeCover_dec" ,
                                s = "AngioTreeCover"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "glm_beta_model_FirstPass_AngioTreeCover.html")

rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(response = "AnnualHerbGram_dec" ,
                                s = "AnnualHerbGram"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "glm_beta_model_FirstPass_AnnualHerbGram.html")

rmarkdown::render(input = "./Analysis/VegComposition/ModelFitting/02_ModelFitting.Rmd", 
                  params = list(response = "PerennialHerbGram_dec" ,
                                s = "PerennialHerbGram"), 
                  output_format = "html_document", 
                  output_dir = "./Analysis/VegComposition/ModelFitting/outputHtmls/",
                  output_file = "glm_beta_model_FirstPass_PerennialHerbGram.html")