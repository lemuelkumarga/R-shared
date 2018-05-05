# Default setup for R markdown
# Copyright Lemuel Kumarga

# Load Global Variables
source("shared/variables.R");

# Load Packages or Install if Necessary ----
## ---- load_packages

# Courtesy of https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
load_or_install.packages <- function(list_of_packages) {
  # Install packages if any
  new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  if (length(new_packages)) {
    install.packages(new_packages, dependencies = TRUE, repos = "http://cran.us.r-project.org")
  }
  
  # Load packages
  tmp <- suppressMessages(lapply(list_of_packages,library, character.only=TRUE))
}

## ---- end-of-load_packages

# Load CSS Styles ----
## ---- load_css
load_or_install.packages("dplyr")
css_files = c(paste0(website_css_dir,"definitions.css"),"shared/css/defaults.css")

load_css_variables <- function(css_files) {
  # Perform a cascading load, we find the first available file
  # that exists and load that css.
  css_to_load = ""
  for (i in css_files) {
    if (file.exists(i)) {
      css_to_load = i;
      break;
    }
  }
  
  if (css_to_load == "") {
    stop("Failed to load any css design file. Please use the default css file in shared.")
  }
  
  # Load the css
  contents <- readChar(css_to_load, file.info(css_to_load)$size) %>% 
              { gsub('[\r\n\t]','',.) } %>%
              # Remove Comments %>%
              { gsub('/\\*[^\\*]*\\*/','',.) } %>%
              # Find Root Blocks
              { regmatches(.,gregexpr(':root[ ]*\\{([^\\{\\}]*)\\}',.))} %>%
              { sapply(.,function(s) { gsub(':root[ ]*\\{|\\}','',s) })} %>%
              # Get all Variables
              { strsplit(paste(.,collapse=' '),";") } %>%
              # Trim Each Variable String
              { c(sapply(.,trimws, which="both"))} %>%
              # Split Variable With Value
              { sapply(.,function(s) { strsplit(s,"[ ]*:[ ']*|[' ]*,[' ]*")})}
  
  # Create css variables
  css_variables <- sapply(contents, function (x) { x[2:length(x)] })
  names(css_variables) <- sapply(contents, function (x) { x[1]})
  
  return(css_variables)
}

css_variables <- load_css_variables(css_files)

## ---- end-of-load_css

# Font Styles ----
## ---- init_font_styles

def_font <- css_variables[["--font-family"]][1]

## ---- end-of-init_font_styles

# Colors ----
## ---- init_colors
load_or_install.packages(c("grDevices","ggplot2"))

bg_color <- css_variables[["--pri"]]
sec_color <- css_variables[["--sec"]]
heading_color <- css_variables[["--heading-color"]]
txt_color <- css_variables[["--font-color"]]
ltxt_color <- alpha(txt_color,0.75)

color_palette <- unlist(css_variables[grepl("--color-", names(css_variables))]) %>%
                 { .[sort.list(names(.))] }

hue_palette <- c("yellow",
                 "orange",
                 "red",
                 "purple",
                 "blue",
                 "cyan",
                 "green")
names(hue_palette) <- hue_palette
hue_palette <- sapply(hue_palette, function(col) { css_variables[[paste0("--",col)]]})

pollute_color <- function(original_col, pollute_col, ratio) {
  c_palette <- colorRamp(c(original_col, pollute_col))
  return(rgb(c_palette(ratio),maxColorValue=255))
}

fade_color <- function(color, fadingFactor) {
  return(pollute_color(bg_color, color, fadingFactor))
}

get_color <- function(inp = "", fadingFactor = 1.) { 
  
  tmp_color_palette <- sapply(color_palette, function(x) {fade_color(x, fadingFactor)})
  tmp_hue_palette <- sapply(hue_palette, function(x) {fade_color(x, fadingFactor)})
  
  if (inp == "") {
    # If nothing is specified, return the list of color palettes
    return (as.character(tmp_color_palette))
  } else if (is.numeric(inp)) {
    # If index is specified, return the index of the color palette
    return(tmp_color_palette[[((inp - 1 + length(tmp_color_palette)) %% length(tmp_color_palette)) + 1]])
  } else if (inp %in% names(tmp_hue_palette)) {
    # If palette is requested, return the palette
    return(tmp_hue_palette[[inp]])
  } else if (inp == "palette") {
    # If palette is requested, return the palette
    return(colorRampPalette(as.character(tmp_hue_palette)))
  } else {
    return(bg_color)
  }
}

## ---- end-of-init_colors

# Pander Tables ----
# ---- init_pander
load_or_install.packages("pander")

# Fix cell text alignment to left
panderOptions('table.alignment.default',
              function(df){ ifelse(sapply(df, is.numeric), 'center', 'left') })

# Fix when to split table
panderOptions('table.split.table',Inf)

# Fix style
panderOptions('table.style','multiline')

## ---- end-of-init_pander

# GGPlot ----
## ---- init_ggplot

theme_lk <- function(fmt_plot = TRUE,
                     fmt_legend = TRUE,
                     fmt_x = TRUE,
                     fmt_y = TRUE) {
  
  bg_n_plots <- theme(
    text = element_text(family=def_font, colour=txt_color),
    # Background Color
    plot.background = element_rect(fill=bg_color, colour=bg_color),
    
    # Plot
    panel.background = element_rect(fill=alpha("#FFFFFF",0.0),colour=NA),
    panel.border = element_rect(colour=NA, fill=NA),
    plot.margin = unit(c(20,20,20,20),'pt')
  )
  
  legends <-  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill=NA),
    legend.title = element_text(size = 10),
    legend.key = element_rect(fill=bg_color, colour=NA),
    legend.text = element_text(size = 10),
    legend.direction ="horizontal",
    legend.box = "horizontal",
    legend.justification = c(1, 0)
  )
  
  x_axis <- theme(
    axis.line.x = element_line(colour=ltxt_color),
    axis.ticks.x = element_line(colour=ltxt_color),
    axis.title.x = element_text(colour=ltxt_color, size = 15),
    axis.text.x = element_text(colour=ltxt_color, size = 12),
    panel.grid.major.x = element_line(colour=NA),
    panel.grid.minor.x = element_line(colour=NA)
  )
  
  y_axis <- theme(
    axis.line.y = element_line(colour=ltxt_color),
    axis.ticks.y = element_line(colour=ltxt_color),
    axis.title.y = element_text(colour=ltxt_color, size = 15),
    axis.text.y = element_text(colour=ltxt_color, size = 12),
    panel.grid.major.y = element_line(colour=NA),
    panel.grid.minor.y = element_line(colour=NA)
  )
  
  output <- theme()
  if (fmt_plot) { output <- output + bg_n_plots }
  if (fmt_legend) { output <- output + legends }
  if (fmt_x) { output <- output + x_axis }
  if (fmt_y) { output <- output + y_axis }
  output
}

## ---- end-of-init_ggplot
