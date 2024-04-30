# set working directory to current script file
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# select all .tex documents 

tex_files <- paste0("Latex Tables/",
                    list.files("Latex Tables/",
                    pattern = "\\.tex"))

# compile all .tex documents

for(i in 1:length(tex_files))
   {
   tinytex::xelatex(tex_files[i])
   }

# convert all .pdf tables to .png

pdf_files <- gsub("\\.tex", "\\.pdf", tex_files)


tab_names <- gsub("Latex Tables\\/", "", tex_files) %>% 
   gsub("\\.tex", ".png", .) 


for(i in 1:length(pdf_files))
   {
pdftools::pdf_convert(pdf_files[i],
            filenames = tab_names[i],
            dpi = 300)
   }



