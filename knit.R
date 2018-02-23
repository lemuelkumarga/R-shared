
knitter <- function(inputFile, 
                    encoding, 
                    prepend_mds = c("shared/md/requirements.md","shared/md/cloning.md")) {
  
  # First collect all the csss that are available
  css = c('shared/css/defaults.css');
  if (file.exists('../../../shared/css/definitions.css')) { css = c(css, css,'../../../shared/css/definitions.css'); } 
  if (file.exists('../../../shared/css/general.css')) { css = c(css, css,'../../../shared/css/general.css'); } 
  css = c(css, 'shared/css/Rmd.css'); 
  
  # html output
  html_output <- rmarkdown::html_document(css=css, 
                                          code_folding='hide',
                                          self_contained=TRUE,
                                          theme='cosmo', 
                                          toc=TRUE, 
                                          toc_depth=4)
  html_file <- file.path(dirname(inputFile), 'index.html')
  rmarkdown::render(inputFile, 
                    output_format=html_output, 
                    encoding = encoding, 
                    output_file = html_file); 
  
  # md output
  md_output <- rmarkdown::github_document(toc = TRUE,
                                          toc_depth = 4,
                                          html_preview = TRUE)
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
      system(paste0('cat ',md_file,' | cat - README.md > tmp.md'))
      system('mv tmp.md README.md')
    }
    # Replace projects with actual name of the project
    project_name <- basename(dirname(inputFile))
    system(paste0("sed -i 's/<project>/",project_name,"/g' README.md"))
  }
  
  system('rm -rf README_files');
}
 