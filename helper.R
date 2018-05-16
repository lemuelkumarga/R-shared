
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