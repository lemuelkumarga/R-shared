# Default setup for R markdown
# Copyright Lemuel Kumarga

# Load Packages or Install if Necessary ----
## ---- load_packages

# Courtesy of https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
load_or_install.packages <- function(...) {
  list_of_packages <- unlist(list(...))
  # Install packages if any
  new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  if (length(new_packages)) {
    install.packages(new_packages, dependencies = TRUE, repos = "http://cran.us.r-project.org")
  }
  
  # Load packages
  tmp <- suppressMessages(lapply(list_of_packages,library, character.only=TRUE))
}

## ---- end-of-load_packages

# Load or install any necessary packages for this script
load_or_install.packages("dplyr", "rlang","grDevices","ggplot2","pander")

# Short Hand Syntaxes for Certain Use Cases ----
## ---- fn_definition

# @input def an expression of the form f(args)
#        For binary operators trap with dots
#        Example: for binary lhs %^% rhs, use `.^.`(lhs,rhs)
#        For anonymous functions, use ..(args)
# @input body the body of the expression
# @output There are two cases:
# [1] if ..(args) is specified, this factory returns an anonymous function
# [2] if f(args) is specified, this factory creates a function with name f
# on the parent environment
`%:=%` <- function(def, body) { 
  
  def <- expr_text(substitute(def))
  body <- expr_text(substitute(body))
  
  # Split Variables Into Function Name And Variable Name
  spt_idx <- gregexpr("\\(", def)[[1]][1]
  # Case 0: def does not follow the format f(args)
  if (spt_idx < 2) {
    stop(sprintf("Definition %s is not of the form f(args).",def))
  } 
  
  # Convert binary functions into appropriate format
  fname <- gsub("(^`\\.)|(\\.`$)","%",substring(def,1,spt_idx - 1))
  params <- substring(def,spt_idx)
  
  # Create Function String
  f_str <- sprintf("function %s { %s }",params,body)
    
  
  output_f <- eval(parse(text=f_str))
  # We want this function to behave similarly to function() {...}
  # Hence, the environment of this output_f must be set to where
  # the function is called
  environment(output_f) <- parent.frame()
  # Case 1: No function name is specified
  # In that case we return an anonymous function
  if (fname == "..") {
    return(output_f)
  } else {
    assign(fname, output_f, env=parent.frame())
  }
}
## ---- end-of-fn_definition

## ---- str_concatenate

# Create a binary function to concatenate strings
`.|.`(e1,e2) %:=% paste0(e1,e2)

## ---- end-of-str_concatenate

## ---- cond_operator

# Create a conditional operator similar to that in C++
`.?.`(cond,true_val) %:=%  { 
  ..(false_val) %:=% 
    sapply(cond, ..(c) %:=% {
      if (c) { true_val } else { false_val }
    })
}

`.:.`(eval_fn, false_val) %:=% { eval_fn(false_val) }

## ---- end-of-cond_operator

# Load CSS Styles ----
# Encapsulate to hide unnecessary functions
{..() %:=% {
  
  ## ---- load_css
  
  # Load Specifications
  #Specify which css files are to be used, in descending priority
  css_files = c("../../shared/css/definitions.css",
                "shared/css/defaults.css")
  
  
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
              { sapply(., ..(s) %:=% { gsub(':root[ ]*\\{|\\}','',s) })} %>%
              # Get all Variables
              { strsplit(paste(.,collapse=' '),";") } %>%
              # Trim Each Variable String
              { c(sapply(.,trimws, which="both"))} %>%
              # Split Variable With Value
              { sapply(.,..(s) %:=% { strsplit(s,"[ ]*:[ ']*|[' ]*,[' ]*")})}
  
  # Create css variables
  css_variables <- sapply(contents, ..(x) %:=% { x[2:length(x)] })
  names(css_variables) <- sapply(contents, ..(x) %:=% { x[1] })

  ## ---- end-of-load_css

  # Font Styles ----
  ## ---- init_font_styles
  
  assign("@f",css_variables[["--font-family"]][1], envir = parent.frame())
  
  ## ---- end-of-init_font_styles
  
  # Colors ----
  ## ---- init_colors
  
  pollute_color(original_col, pollute_col, ratio) %:=% {
    c_palette <- colorRamp(c(original_col, pollute_col))
    return(rgb(c_palette(ratio),maxColorValue=255))
  }

  base_palette <- c( "bg" = css_variables[["--pri"]],
                     "sec" = css_variables[["--sec"]],
                     "heading" = css_variables[["--heading-color"]],
                     "txt" = css_variables[["--font-color"]],
                     "ltxt" = pollute_color(css_variables[["--pri"]],css_variables[["--font-color"]],0.75))
  
  color_palette <- unlist(css_variables[grepl("--color-", names(css_variables))]) %>%
                   { .[sort.list(names(.))] }
  
  hue_palette <- c("yellow","orange","red","purple","blue","cyan","green")
  names(hue_palette) <- hue_palette
  hue_palette <- sapply(hue_palette, ..(col) %:=% { css_variables[[paste0("--",col)]]})
  
  fade_color(color, fadingFactor) %:=% {
    return(pollute_color(base_palette[["bg"]], color, fadingFactor))
  }
  
  get_color(id = "NA", fadingFactor = 1.) %:=% { 
    
    inp <- id
    
    tmp_base_palette <- sapply(base_palette, ..(x) %:=% { fade_color(x, fadingFactor) })
    tmp_color_palette <- sapply(color_palette, ..(x) %:=% { fade_color(x, fadingFactor) })
    tmp_hue_palette <- sapply(hue_palette, ..(x) %:=% { fade_color(x, fadingFactor) })
    
    if (inp == "NA") {
      # If nothing is specified, return the list of color palettes
      return (as.character(tmp_color_palette))
    } else if (inp == "palette") {
      # If palette is requested, return the palette
      return(colorRampPalette(as.character(tmp_hue_palette)))
    } else if (grepl("^[0-9]+$",inp)) {
      # If index is specified, return the index of the color palette
      return(tmp_color_palette[[((as.numeric(inp) - 1 + length(tmp_color_palette)) %% length(tmp_color_palette)) + 1]])
    } else if (inp %in% names(tmp_hue_palette)) {
      # If palette is requested, return the palette
      return(tmp_hue_palette[[inp]])
    } else if (inp %in% names(tmp_base_palette)) {
      # If base colors is requested
      return(tmp_base_palette[[inp]])
    } else {
      stop("Invalid input for function @c is specified.")
    }
  }
  
  assign("@c_",get_color, envir = parent.frame())
  assign("@c", ..(id = NA, fadingFactor = 1.) %:=% get_color(expr_text(substitute(id)), fadingFactor), envir = parent.frame())
  
  ## ---- end-of-init_colors
}}()

# Pander Tables ----
# ---- init_pander

# Fix cell text alignment to left
panderOptions('table.alignment.default',
              ..(df) %:=% { ifelse(sapply(df, is.numeric), 'center', 'left') })

# Fix when to split table
panderOptions('table.split.table',Inf)

# Fix style
panderOptions('table.style','multiline')

## ---- end-of-init_pander

# GGPlot ----
## ---- init_ggplot

theme_lk(fmt_plot = TRUE,
         fmt_legend = TRUE,
         fmt_x = TRUE,
         fmt_y = TRUE,
         scale = 1.0) %:=% {
  
  title_size <- 15 * scale
  subtitle_size <- 12 * scale
  font_size <- 10 * scale
  
  bg_n_plots <- theme(
    text = element_text(family=`@f`, colour=`@c`(txt),size=font_size),
    # Background Color
    plot.background = element_rect(fill=`@c`(bg), colour=`@c`(bg)),
    
    # Plot
    panel.background = element_rect(fill=alpha(`@c`(bg),0.0),colour=NA),
    panel.border = element_rect(colour=NA, fill=NA),
    plot.margin = unit(c(20,20,20,20),'pt'),
    
    # Title
    plot.title = element_text(size = title_size, hjust=0.5),
    
    # Strips
    strip.background = element_rect(fill=`@c`(ltxt,0.5), 
                                    color=`@c`(ltxt,0.5)),
    strip.text = element_text(color=`@c`(bg))
  )
  
  legends <-  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill=NA),
    legend.title = element_text(size = font_size),
    legend.key = element_rect(fill=`@c`(bg), colour=NA),
    legend.text = element_text(size = font_size),
    legend.direction ="horizontal",
    legend.box = "horizontal",
    legend.justification = c(1, 0)
  )
  
  x_axis <- theme(
    axis.line.x = element_line(colour=`@c`(ltxt)),
    axis.ticks.x = element_line(colour=`@c`(ltxt)),
    axis.title.x = element_text(colour=`@c`(ltxt), size = subtitle_size),
    axis.text.x = element_text(colour=`@c`(ltxt), size = font_size),
    panel.grid.major.x = element_line(colour=NA),
    panel.grid.minor.x = element_line(colour=NA)
  )
  
  y_axis <- theme(
    axis.line.y = element_line(colour=`@c`(ltxt)),
    axis.ticks.y = element_line(colour=`@c`(ltxt)),
    axis.title.y = element_text(colour=`@c`(ltxt), size = subtitle_size),
    axis.text.y = element_text(colour=`@c`(ltxt), size = font_size),
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

theme_ppt(...) %:=% {
  theme_lk(..., scale=1.7)
} 

## ---- end-of-init_ggplot

# Highcharts ----
## ---- init_highcharts

hcharts_lk(scale = 1.0) %:=% {
  
  title_size <- paste0(2.0 * scale,'em')
  subtitle_size <- paste0(1.5 * scale,'em')
  font_size <- paste0(1.25 * scale,'em')
  
  axis_config <- list(
    # Text
    title=list(style=list(fontSize = subtitle_size, color=`@c`(ltxt,0.6)),
               text=NA),
    labels=list(style=list(fontSize = font_size, color=`@c`(ltxt,0.6))),
    # Lines
    lineWidth=2,
    lineColor=`@c`(ltxt,0.4),
    # Ticks
    tickWidth = 0,
    # Grids
    gridLineWidth=0
  )
  
  def_opts <- list(
    # Overall Definition
    chart = list(style=list(fontFamily= `@f`)),
    credits = list(enabled=FALSE),
    exporting = list(enabled=FALSE),
    # Title
    title = list(style=list(fontFamily=`@f`, fontSize = title_size, color=`@c`(txt)), text=NA),
    # Axises
    xAxis = axis_config,
    yAxis = axis_config,
    zAxis = axis_config,
    # Legend
    legend = list(
      align="right",
      # items
      itemStyle = list(
        fontFamily = `@f`,
        fontSize = font_size,
        fontWeight = 'normal',
        color = `@c`(txt)
      )
    ),

    # Tooltips
    tooltip=list(
      # Text Format
      headerFormat = "",
      style = list(
        fontFamily = `@f`,
        fontSize = font_size,
        fontWeight = 'normal',
        color = `@c`(txt)
      ),
      # Animation
      delayForDisplay=10
    ),
    #Annotations
    # Remove Annotations
    annotationsOptions=list(
      enabledButtons=FALSE
    )
  )

  def_opts
}

hcharts_ppt() %:=% {
  ppt_opts <- hcharts_lk(scale=1.5)
  ppt_opts$chart$style$marginBottom <- 45
  
  # Legend Formatting
  ppt_opts$legend$layout <- "vertical"
  ppt_opts$legend$verticalAlign <- "middle"
  
  #Tooltip Formatting
  ppt_opts$tooltip$style$fontSize <- paste0(
    as.numeric(gsub("em","",ppt_opts$tooltip$style$fontSize)) * 0.9,"em")
  ppt_opts
}


## ---- end-of-init_highcharts
