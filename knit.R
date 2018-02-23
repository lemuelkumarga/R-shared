
knitter <- function(inputFile, encoding) {
  
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
  
  system('rm -rf README_files');
}
 