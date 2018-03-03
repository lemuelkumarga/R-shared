# Load Global Variables
source("shared/variables.R");

knitter <- function(inputFile, 
                    encoding, 
                    prepend_mds = c("shared/md/website.md","shared/md/requirements.md","shared/md/cloning.md")) {
  
  # First collect all the csss that are available
  css = c('shared/css/defaults.css');
  if (file.exists(paste0(website_css_dir,'definitions.css'))) { css = c(css, css,paste0(website_css_dir,'definitions.css')); } 
  if (file.exists(paste0(website_css_dir,'general.css'))) { css = c(css, css,paste0(website_css_dir,'general.css')); } 
  css = c(css, 'shared/css/Rmd.css'); 
  
  # html output
  html_output <- rmarkdown::html_document(css=css, 
                                          code_folding='hide',
                                          self_contained=FALSE,
                                          theme='cosmo', 
                                          toc=TRUE, 
                                          toc_depth=4)
  html_file <- file.path(dirname(inputFile), 'index.html')
  rmarkdown::render(inputFile, 
                    output_format=html_output, 
                    encoding = encoding, 
                    output_file = html_file); 
  
  # Create a responsive table container for all tables
  system(paste0("sed 's/<table/<div class=",'"',"table-responsive",'"',"><table/g' index.html | sed 's/<\\/table>/<\\/table><\\/div>/g' > tmp.html"))
  system('mv tmp.html index.html')
  
  # md output
  md_output <- rmarkdown::github_document(toc = TRUE,
                                          toc_depth = 4,
                                          html_preview = FALSE)
  md_file <- file.path(dirname(inputFile), 'README.md')
  rmarkdown::render(inputFile,
                    output_format=md_output,
                    encoding = encoding,
                    output_file = md_file);


  if (length(prepend_mds)) {
    # Draw a Horizontal Line to indicate the end of prepend
    system('echo --- | cat - README.md > tmp.md')
    system('mv tmp.md README.md')
    
    # Prepend specified markdowns 
    for (md_file in rev(prepend_mds)) {
      if (file.exists(md_file)) {
        system(paste0('cat ',md_file,' | cat - README.md > tmp.md'))
        system('mv tmp.md README.md')
      }
    }
    
    # Take special care for inputFile of the form "./main.Rmd"
    if (grepl('^\\.\\/',inputFile)) {
      project_name <- getwd()
    } else {
      project_name <- dirname(inputFile)
    }
    project_name <- basename(project_name)
    
    # Replace projects with actual name of the project
    system(paste0("sed 's/<project>/",project_name,"/g' README.md > tmp.md"))
    system('mv tmp.md README.md')
  }
  
  system('rm -rf README_files');
}
 