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
              # Split Blocks
              { unlist(strsplit(.,"}")) } %>%
              # Find the ones that contain ":root"
              { .[grepl(":root",.)]} %>%
              # Remove any letters before root 
              { unlist(strsplit(.,":root")) } %>%  
              # Only select the body inside root 
              { .[grepl("\\{",.)] } %>%
              # Split the variables within root
              { unlist(strsplit(.,"\\{|;")) } %>%
              # Remove comments embedded within the body
              { unlist(strsplit(.,"\\*/")) } %>%
              { .[!grepl("/\\*",.)] } %>%
              # Trim spaces
              trimws(.) %>%
              # Remove empty strings
              .[. != ""] %>%
              # Find Key-Value Pairs 
              {
                lapply(., function(x) {
                  unlist(strsplit(x,":|,")) %>%
                  # Remove any string quotations
                  gsub('"','',.) %>%
                  gsub("'","",.) %>%
                  # Trim Spacing
                  trimws()
                })
              }
  
  # Create css variables
  css_variables <- sapply(contents, function (x) { x[2:length(x)] })
  names(css_variables) <- sapply(contents, function (x) { x[1]})
  
  return(css_variables)
}

css_variables <- load_css_variables(c(paste0(website_css_dir,"definitions.css"),"shared/css/defaults.css"))

## ---- end-of-load_css

# Colors ----
## ---- init_colors
load_or_install.packages(c("grDevices","ggplot2"))

bg_color <- css_variables[["--pri"]]
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

get_color <- function(inp = "") { 
  
  if (inp == "") {
    # If nothing is specified, return the list of color palettes
    return(as.character(color_palette))
  } else if (is.numeric(inp)) {
    # If index is specified, return the index of the color palette
    return(color_palette[[(inp + length(color_palette)) %% length(color_palette)]])
  } else if (inp %in% names(hue_palette)) {
    # If palette is requested, return the palette
    return(hue_palette[[inp]])
  } else if (inp == "palette") {
    # If palette is requested, return the palette
    return(colorRampPalette(as.character(color_palette)))
  } else {
    return(bg_color)
  }
  
}

## ---- end-of-init_colors

# Font Styles ----
## ---- init_font_styles

def_font <- css_variables[["--font-family"]][1]

## ---- end-of-init_font_styles

# GGPlot ----
## ---- init_ggplot

theme_lk <- function() {
  
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
  
  bg_n_plots + legends + x_axis + y_axis
}

## ---- end-of-init_ggplot

# Pander Tables ----
# ---- init_pander
load_or_install.packages("pander")

# Fix cell text alignment to left
panderOptions('table.alignment.default',
              function(df){ ifelse(sapply(df, is.numeric), 'center', 'left') })

# Fix when to split table
panderOptions('table.split.table',80)

# Fix style
panderOptions('table.style','multiline')

## ---- end-of-init_pander