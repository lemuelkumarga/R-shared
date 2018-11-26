
website_css_dir <- "../../shared/css/"
`%|%` <- function(s1, s2) { paste0(s1,s2) }

# Hacks to Ensure Consistency Between What is User-Loaded, and Automate-Loaded
resolveHTML <- function() {
  # Issue 1: Fix issues where highcharts load an older version of jquery
  system("grep -v 'jquery-1.11.1' index.html > tmp.html")
  system("mv tmp.html index.html")
}

knitRMD <- function(inputFile, 
                    encoding, 
                    prepend_mds = c("shared/md/website.md","shared/md/requirements.md","shared/md/cloning.md"),
                    css = c('shared/css/defaults.css', website_css_dir %|% 'definitions.css',
                            website_css_dir %|% "general.css",'shared/css/Rmd.css')) {
  
  # Then collects all the additional JS plugins
  js = rmarkdown::includes(in_header= "shared/js/js.html")
  
  # html output
  html_output <- rmarkdown::html_document(css=css, 
                                          code_folding='hide',
                                          self_contained=FALSE,
                                          includes=js,
                                          theme='cosmo', 
                                          toc=FALSE)
  html_file <- file.path(dirname(inputFile), 'index.html')
  rmarkdown::render(inputFile, 
                    output_format=html_output, 
                    encoding = encoding, 
                    output_file = html_file); 
  
  # Create a responsive table container for all tables
  system("sed 's/<table/<div class=" %|% '"' %|% "table-responsive" %|% '"' %|% "><table/g' index.html | sed 's/<\\/table>/<\\/table><\\/div>/g' > tmp.html")
  system('mv tmp.html index.html')
  
  resolveHTML()
  
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
        system('cat ' %|% md_file %|% ' | cat - README.md > tmp.md')
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
    system("sed 's/<project>/" %|% project_name %|% "/g' README.md > tmp.md")
    system('mv tmp.md README.md')
  }
  
  system('rm -rf README_files');
}

knitPPT <- function(inputFile, encoding,
                    css=c('shared/css/defaults.css', website_css_dir %|% 'definitions.css',
                          website_css_dir %|% "general.css",'shared/css/ppt.css')) { 
  
  # Then collects all the additional JS plugins
  js = rmarkdown::includes(in_header= "shared/js/ppt.html")
  
  html_output <- rmarkdown::ioslides_presentation(
                    css=css,
                    self_contained=FALSE,
                    includes=js
                  )
  html_file <- file.path(dirname(inputFile), 'index.html')
  rmarkdown::render(inputFile, 
                    output_format=html_output,
                    encoding = encoding, 
                    output_file=html_file)
  
  # Disable scaling
  system("sed '/<meta name=\"viewport\"/s/\">/,user-scalable=no\">/g' index.html > tmp.html")
  system("mv tmp.html index.html")
  
  # Convert dark slides to invert
  system("sed '/<slide/s/dark/invert/g' index.html > tmp.html")
  system("mv tmp.html index.html")
  
  # Use to transfer any css styles specified in Rmd from the article to slide element 
  from_article_to_slide <- function(cls) {
    system("sed '/slide-" %|% cls %|% "/s/<slide class=\"/<slide class=\"" %|% cls %|% " /g' index.html > tmp.html")
    system("mv tmp.html index.html")
  }
  
  from_article_to_slide("invert")
  from_article_to_slide("null")
  
  resolveHTML()
}
