# This script runs R Markdown documentation docs. and saves them in the shared oneDrive folder /VegClim_modelOutputs/Documentation/


# Methods overview document -----------------------------------------------
rmarkdown::render(input = "./Documentation/CoverMethodsOverview.Rmd", 
                  output_format = "html_document", 
                  output_dir = "/Users/astears/Library/CloudStorage/OneDrive-DOI/VegClim_modelOutputs/Documentation/",
                  output_file = paste0("./CoverMethodsOverview_",Sys.Date(),".html"))
# save in additional local documentation folder
file.copy(from = 
            paste0("/Users/astears/Library/CloudStorage/OneDrive-DOI/VegClim_modelOutputs/Documentation/CoverMethodsOverview_",Sys.Date(),".html"),
              to = paste0("./Documentation/CoverMethodsOverview_",Sys.Date(),".html"))
