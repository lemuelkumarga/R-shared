
# Prerequisites: Must have ran defaults.R previously
load_or_install.packages("tidyr")

## ---- cache

# After running the function, save the output into a cache file
# @input name the name of the cache (exclude extensions)
# @input inputs a list of inputs for the function
# @input f the function to be run
cache_dir <- "cache/"
cache(name, inputs, f) %:=% {
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
tictoc(input_tic, input_toc, tic_on_toc = FALSE) %:=% {
  
  cur_env <- environment()
  
  tic() %:=% { assign("storage",input_tic(), envir=cur_env) }
  
  toc() %:=% {
    old_val <- storage
    new_val <- tic()
    
    if (tic_on_toc) { tic() }
    
    input_toc(old_val, new_val)
  }
  
  cur_env
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
data_overview(data,
              null_fn = ..(cname) %:=% { paste0(cname," == '' | is.na(",cname,")")}) %:=%  {
  
  cols_summary <- data.frame(ColumnNames = colnames(data))
  cols_summary$Type <- lapply(data, class) %>%
    toupper()
  cols_summary$Examples <- lapply(cols_summary$ColumnNames,
                                  function(cname) {
                                    data %>%
                                      filter(!!parse_expr(paste0("!(",null_fn(cname),")"))) %>%
                                      { .[[cname]] } %>%
                                      unique() -> filtered_set
                                    filtered_set[1:min(5, length(filtered_set))] %>%
                                      paste(collapse=' // ')
                                  })
  cols_summary$EmptyValues <- lapply(cols_summary$ColumnNames,
                                     function(cname) {
                                       data %>%
                                         filter(!!parse_expr(null_fn(cname))) %>%
                                         nrow()
                                     })
  cols_summary$PctFilled <- lapply(cols_summary$EmptyValues,
                                   function(x) {
                                      { ((nrow(data) - x) / nrow(data)) * 100 } %>% 
                                       floor %>% paste0("%")
                                   })
  
  select(cols_summary, ColumnNames, Type, Examples, PctFilled)
}

## ---- end-of-data-overview

## ---- data-snapshot

# Provides graphical output on the relationship between features and response
# @input data the data frame
# @input r_col the column name corresponding to the response variable
# @input cont_geom Custom geom input for continuous plots
# @input disc_geom Custom geom input for discrete plots
# @input misc_layers Custom layers for both continuous and discrete plots
# @output A list showing 2 plots
#   - cont_plot: ggplot output between response and continuous features
#   - disc_plot: ggplot output between response and discrete features
data_snapshot(data, r_col, cont_geom = NULL, disc_geom = NULL, misc_layers = geom_blank()) %:=% {
  
  r_col <- expr_text(substitute(r_col))
  
  # Check if response is continuous (regression) or discrete (classification)
  is_r_cont <- is.numeric(data[,r_col])
  if (!is_r_cont) { n_classes <- data[,r_col] %>% unique %>% length}
  
  # Set Geom Defaults
  if (is_r_cont) {
    # For Regression Problems
    if (is.null(cont_geom)) { cont_geom <- geom_smooth(method="loess", color=`@c`(1), fill=`@c`(txt,0.5)) + 
                                         geom_point(alpha=0.02, color=`@c`(txt)) }
    if (is.null(disc_geom)) { disc_geom <- geom_violin(aes(alpha="NA"), scale="count", fill=`@c`(txt,0.25), color=NA) }
  } else {
    # For classification problems
    if (is.null(cont_geom)) { cont_geom <- geom_density(alpha=0.5, color=NA) }
    if (is.null(disc_geom)) { disc_geom <- geom_bar(aes(alpha="NA"), stat="identity", position="stack") }
    
  }
  
  # Split data into three components, response, cont_features, disc_features
  response <- data %>% select(r_col)
  features <- data %>% select_("-" %|% r_col)
  cont_features <- features %>% select_if(is.numeric)
  disc_features <- features %>% select_if(~!is.numeric(.))
  
  # Reformat both features for ggplot input
  to_snapshot_input(feature_data) %:=% {
    feature_data %>% 
      cbind(response) %>%
      gather(key="var",value="val",colnames(.)[colnames(.) != r_col])
  }
  cont_features <- cont_features %>% to_snapshot_input
  disc_features <- disc_features %>% to_snapshot_input
  
  # Plot Response vs Continuous Features
  cont_plot <- NA
  if (length(cont_features) > 1) {
    if (is_r_cont) {
      cont_plot <- ggplot(cont_features, aes_string(x="val",y=r_col)) +
                   cont_geom +
                   scale_x_continuous(name="Variable Value")
    } else {
      cont_plot <- ggplot(cont_features, aes_string(x="val",fill=r_col)) +
                   cont_geom + 
                   scale_x_continuous(name="Variable Value", expand=c(0,0)) + 
                   scale_y_continuous(name="Density", expand=c(0,0)) + 
                   scale_fill_manual(values=`@c`())
    }
    cont_plot <- cont_plot + 
                 theme_lk() +
                 facet_wrap(.~toupper(var), scales="free", strip.position="bottom") +
                 misc_layers
  }
  
  # Plot Response vs Discrete Features
  disc_plot <- NA
  if (length(disc_features) > 1) {
    if (is_r_cont) {
      # Order disc_features by Median
      disc_order <- disc_features %>%
                    group_by(var, val) %>%
                    summarise_(.dots=list("med"="median(" %|% r_col %|%")")) %>%
                    arrange(desc(med)) %>%
                    `[[`('val')
      disc_features$val <- factor(disc_features$val, levels = disc_order)
      disc_plot <- ggplot(disc_features, aes_string(x="val", y=r_col)) + 
                   disc_geom + 
                   geom_boxplot(fill=NA, 
                                color=`@c`(1), 
                                outlier.color=`@c`(txt),
                                outlier.alpha=0.02,
                                width=0.1) +
                   scale_x_discrete(name="Variable Levels") + 
                   scale_alpha_manual(name="Violin Width", labels=c("NA"="Sample Size of Levels"), values=c("NA"=0.8)) 
    } else {
      # Find proportion across different levels
      disc_prop <- disc_features %>%
                   group_by_("var", "val", r_col) %>%
                   summarise(count=n()) %>%
                   ungroup() %>%
                   group_by_("var","val") %>%
                   mutate(val_count = sum(count)) %>%
                   ungroup() %>%
                   group_by_("var") %>%
                   mutate(val_freq = val_count/sum(count)) %>%
                   ungroup() %>%
                    # Find Density Ratio
                   group_by_("var",r_col) %>%
                   mutate(count_freq = count / sum(count)) %>%
                   ungroup() %>%
                   group_by_("var","val") %>%
                   mutate(freq_ratio = count_freq / sum(count_freq))
                    
      disc_plot <- ggplot(disc_prop,
                          aes_string(x="val", y="freq_ratio", fill=r_col, width="val_freq * 1.5")) + 
                    disc_geom +
                    scale_x_discrete(name="Variable Levels") + 
                    scale_y_continuous(name="Density Proportion", expand=c(0,0), labels=scales::percent) + 
                    scale_fill_manual(values=`@c`()) +
                    scale_alpha_manual(name="Bar Width", 
                                       guide=guide_legend(order=1),
                                       labels=c("NA"="Sample Size of Levels"), 
                                       values=c("NA"=0.8))
    }
    disc_plot <- disc_plot + 
                  theme_lk() +
                  facet_wrap(.~toupper(var), scales="free", strip.position="bottom") +
                  misc_layers
  }
 
  return(list("cont_plot" = cont_plot,
              "disc_plot" = disc_plot))
}

## ---- end-of-data-snapshot
