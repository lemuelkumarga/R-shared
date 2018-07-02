
## ---- cache

# After running the function, save the output into a cache file
# @input name the name of the cache (exclude extensions)
# @input inputs a list of inputs for the function
# @input f the function to be run
cache_dir <- "cache/"
cache <- function(name, inputs, f) {
  cache_file <- paste0(cache_dir,name,".RDS")
  if (file.exists(cache_file)) { readRDS(cache_file) }
  else { output <- do.call(f,inputs); saveRDS(output, cache_file); output }
}

## ---- end-of-cache

## ---- tictoc

# Create a tic toc function to detect changes made by the code
# @input tic the function to run when tic is executed. Results are stored and released later
# @input toc the function to run when toc is executed. Takes in two values, the old value of tic
# and the new value of tic
# @input tic_on_toc TRUE if we automatically execute tic on the completion of toc.
# @output a list of 3 items:
#   storage: the value being stored when tic is run
#   tic: the tic function to store existing value
#   toc: the toc function to process existing value with current value
tictoc <- function(tic, toc, tic_on_toc = FALSE) {
  
  tt_output <- list(storage=NA, tic=NA, toc=NA)
  
  tt_output$tic <- function() {
    tt_output$storage <<- tic()
  }
  
  tt_output$toc <- function() {
    
    old_val <- tt_output$storage
    new_val <- tic()
    
    if (tic_on_toc) { tt_output$tic() }
    
    toc(old_val, new_val)
  }
  
  tt_output
}

## ---- end-of-tictoc

## ---- data-overview

# Provides high level information about a particular data frame
# @input data: the data frame
# @input null_fn: a function that takes in a value (like those in the cells)
# and outputs true if value corresponds to the equivalent null value, false
# otherwise. For example, we may want to consider 0 as nulls on some scenarios
# but not on others.
# @output A data frame showing
# - Name of the Columns
# - Type of Variables in Each Column
# - Some Non-Null Examples of Each Column
# - % of Values that are Non-Null in Each Column (% Filled)
data_overview <- function(data,
                          null_fn = function(cname) { paste0(cname," == '' | is.na(",cname,")")}) {
  
  cols_summary <- data.frame(ColumnNames = colnames(data))
  cols_summary$Type <- lapply(data, class) %>%
    toupper()
  cols_summary$Examples <- lapply(cols_summary$ColumnNames,
                                  function(cname) {
                                    data %>%
                                      filter_(paste0("!(",null_fn(cname),")")) %>%
                                      `[[`(cname) %>%
                                      unique() -> filtered_set
                                    filtered_set[1:min(5, length(filtered_set))] %>%
                                      paste(collapse=' // ')
                                  })
  cols_summary$EmptyValues <- lapply(cols_summary$ColumnNames,
                                     function(cname) {
                                       data %>%
                                         filter_(null_fn(cname)) %>%
                                         nrow()
                                     })
  cols_summary$PctFilled <- lapply(cols_summary$EmptyValues,
                                   function(x) {
                                     ((nrow(data) - x) / nrow(data)) %>%
                                       `*`(100) %>% floor() %>%
                                       paste0("%")
                                   })
  
  select(cols_summary, ColumnNames, Type, Examples, PctFilled)
}

## ---- end-of-data-overview

## ---- plot-overview

# @prereq: assumes default.R has been loaded. If not, run the default.R source.

# Load dependent packages
load_or_install.packages(c("ggplot2","dplyr","rms","GGally"))

# Provides a relationship snapshot amongst features and the class
# @input dataset: the dataset
# @input response: the column name containing the observed class
# @input rank_method: Choose different methods to determine variable ranking
# -- independent: run linear regression for each feature independently and rank based on r^2
# -- multi: run multiple linear regression for all features and rank based on p-values
# @output A plot showing
# - Diagonal: The density of varibale for each class
# - Lower Triangle: A contour density plot
# - Upper Triangle: Correlation plot
class_snapshot <- function(dataset, response, rank_method = "multi") {
  
  
  # ################################ 
  # RANKING ALGORITHM ##############
  
  if (rank_method == "independent") {
    
    # Rank Each Feature Based On Their R^2 using logit
    rank_name <- "R-Square"
    rankings <- sapply(setdiff(colnames(dataset),response), function(f) {
      eqn <- as.formula(paste(response,"~",f))
      model <- lrm(eqn, data = dataset)
      return(model$stats[["R2"]])
    }) %>% sort(decreasing = TRUE) %>% round(3)
    
  } else if (rank_method == "multi") {
    
    # Rank Each Feature Based on Their R^2 using multilogit
    rank_name <- "P-Values"
    # Add Tags to discrete variables so that 
    # we can detect p-values for these variables later on
    discrete_vars <- sapply(dataset %>% select_(.dots=c(paste0("-",response))), is.factor)
    rank_dataset <- dataset
    colnames(rank_dataset)[discrete_vars] <- sprintf("--%s--",colnames(rank_dataset)[discrete_vars])
    
    # Perform logistic regression on the whole dataset
    logit.fit <- glm(as.formula(paste0(response," ~ .")), family=binomial, data=rank_dataset)
    
    # Get the p_values for each feature
    p_vals <- summary(logit.fit)$coefficients[,4]
    feature_names <- colnames(dataset)
    rankings <- sapply(feature_names[feature_names != response], function (fn) {
      # For discrete, we first find all the p-values associated with
      # the feature and get the min
      if (discrete_vars[fn]) {
        return(min(p_vals[grepl(sprintf("--%s--",fn),names(p_vals))]))
      } else {
        return(p_vals[[fn]])
      }
    }) %>%
    sort() %>%
    round(3)
    
  }
  
  # #################################### 
  # DEFAULT PLOT SETTINGS ##############
  # Create Default Settings Across Plots
  r_levels <- levels(dataset[,response])
  # Colors
  if (length(r_levels) > length(get_color())) { 
    r_cols <- get_color("palette")(length(r_levels)) 
  } else { 
    r_cols <- get_color()[1:length(r_levels)] 
  }
  names(r_cols) <- r_levels
  
  # Legends
  legend_p <- (ggplot(dataset, aes_string(x=response, fill=response)) + 
                 geom_histogram(stat="count") +
                 theme_lk() + 
                 theme(legend.position = "top",
                       legend.justification = c(0.5,0)) +
                 scale_fill_manual(name=response, values=r_cols,
                                   guide=guide_legend())) %>%
    grab_legend()
  
  # ############################# 
  # DIAGONAL PLOTS ##############
  diag_p <- function(data, mapping, ...) {
    
    # Get Feature and Response Variable
    feature <- quo_name(mapping$x)
    p_output <- ggplot(data)
    
    f_factor <- is.factor(data[,feature])
    # If feature is discrete, compare conditional distributions on feature
    if (f_factor) {
      p_output <- p_output + 
        geom_histogram(aes_string(x=feature, fill=response, alpha="..count.."), 
                       stat="count",
                       colour = NA, position="fill") + 
        scale_x_discrete(expand=c(0,0))
      # If feature is continuous, plot a density plot
    } else {
      p_output <- p_output + 
        geom_density(aes_string(x=feature, fill=response), 
                     alpha=0.5, colour = NA) + 
        scale_x_continuous(expand=c(0,0))
    }
    
    # Output plot
    p_output +
      geom_text(data = data.frame(x=Inf, y=Inf, label = scales::percent(rankings[[feature]])),
                aes(x=x, y=y, label=label),
                family=def_font, hjust=1.3, vjust=2.0) + 
      scale_y_continuous(expand=c(0,0)) +
      scale_fill_manual(values=r_cols)
  }
  
  # ############################# 
  # LOWER PLOTS #################
  lower_p <- function(data, mapping, ...) {
    
    # Get Feature1 and Feature2 Variable
    x <- quo_name(mapping$x)
    y <- quo_name(mapping$y)
    
    # Determine if x and y is discrete or continuous
    x_factor <- is.factor(data[,x])
    y_factor <- is.factor(data[,y])
    
    # Case 1: Both x and y are continuous
    if (!x_factor & !y_factor) {
      p_out <- ggplot(data) +
        geom_density_2d(aes_string(x=x, y=y,  colour=response), alpha=0.3) +
        scale_x_continuous(expand=c(0,0)) + 
        scale_y_continuous(expand=c(0,0)) + 
        scale_color_manual(values=r_cols)
      # Case 2: x is continuous, y is discrete
    } else if (!x_factor & y_factor) {
      p_out <- ggplot(data) +
        geom_density(aes_string(x=x, fill=response),
                     colour=NA, alpha=0.3) + 
        scale_x_continuous(expand=c(0,0)) + 
        scale_y_continuous(expand=c(0,0)) +
        scale_fill_manual(values=r_cols) +
        facet_wrap(as.formula(paste0("~",y)), ncol=1) + 
        theme(strip.background = element_blank(),
              strip.text.x = element_blank())
      
    } else {
      p_out <- ggplot(data)
      # Case 3: x is discrete and y is continuous
      if (!y_factor) { 
        p_out <- p_out + 
          geom_histogram(aes_string(x=y, fill=response, alpha="..count.."),
                         position="fill", bins = 10) + 
          scale_x_continuous(expand=c(0,0)) 
        # Case 4: x is discrete and y is discrete
      } else { 
        p_out <- p_out + 
          geom_histogram(aes_string(x=y, fill=response, alpha="..count.."),
                         stat="count",
                         position="fill") + 
          scale_x_discrete(expand=c(0,0))
      }
      p_out <- p_out +
        scale_y_continuous(expand=c(0,0)) +
        scale_fill_manual(values=r_cols) + 
        coord_flip() +
        facet_wrap(as.formula(paste0("~",x))) + 
        theme(strip.background = element_blank(),
              strip.text.x = element_blank())
    }
    
  }
  
  # ############################# 
  # UPPER PLOTS #################
  upper_p <- function(data, mapping, ...) {
    
    # Get Feature1 and Feature2 Variable
    x <- quo_name(mapping$x)
    y <- quo_name(mapping$y)
    
    # Determine if y is discrete or continuous
    y_factor <- is.factor(data[,y])
    eqn <- as.formula(paste(y,"~",x))
    relation <- 0
    # if discrete, run logistic regression
    if (y_factor) {
      model <- lrm(eqn, data)
      relation <- round(sqrt(model$stats[["R2"]]),2)
      # else if continuous, run linear regression
    } else {
      model <- lm(eqn, data)
      relation <- round(sqrt(summary(model)$r.squared),2)
    }
    
    # Output plot
    ggplot(data=data.frame(1)) + 
      theme_void() + 
      scale_x_continuous(limits=c(0,1)) + 
      scale_y_continuous(limits=c(0,1)) +
      geom_label(x=0.5, y=0.5, 
                 label=scales::percent(relation), 
                 family=def_font, color=bg_color, fill=fade_color(txt_color,0.2), 
                 size=relation*10)
  }
  
  # Output Plot
  ggpairs(dataset, aes_string(colour=response),columns=names(rankings),
          lower=list(continuous=GGally::wrap(lower_p),
                     combo=GGally::wrap(lower_p),
                     discrete=GGally::wrap(lower_p)),
          diag=list(continuous=GGally::wrap(diag_p),
                    discrete=GGally::wrap(diag_p)),
          upper=list(continuous=GGally::wrap(upper_p),
                     combo=GGally::wrap(upper_p),
                     discrete=GGally::wrap(upper_p)),
          axisLabels="none",
          switch="y",
          legend=legend_p) + 
    theme_lk() + 
    theme(strip.background = element_rect(fill=NA),
          strip.text = element_text(colour=ltxt_color, size=12),
          panel.spacing = unit(3,"pt"),
          panel.border = element_rect(color = fade_color(txt_color,0.3), fill = NA, size = 0.1),
          axis.title.x = element_text(margin=margin(15,0,0,0))) + 
    xlab(paste0("LT = 2D Density View     D = Density (With ",rank_name,")    UT = Correlation"))
}

## ---- end-of-plot-overview
