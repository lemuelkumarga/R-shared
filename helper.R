
## ---- data_overview

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
                  cols_summary$Type <- lapply(training_set, class) %>%
                    toupper()
                  cols_summary$Examples <- lapply(cols_summary$ColumnNames,
                                                  function(cname) {
                                                    training_set %>%
                                                      filter_(paste0("!(",null_fn(cname),")")) %>%
                                                      `[[`(cname) %>%
                                                      unique() -> filtered_set
                                                    filtered_set[1:min(5, length(filtered_set))] %>%
                                                      paste(collapse=' // ')
                                                  })
                  cols_summary$EmptyValues <- lapply(cols_summary$ColumnNames,
                                                     function(cname) {
                                                       training_set %>%
                                                         filter_(null_fn(cname)) %>%
                                                         nrow()
                                                     })
                  cols_summary$PctFilled <- lapply(cols_summary$EmptyValues,
                                                   function(x) {
                                                     ((nrow(training_set) - x) / nrow(training_set)) %>%
                                                       `*`(100) %>% round(0) %>%
                                                       paste0("%")
                                                   })
                  
                  select(cols_summary, ColumnNames, Type, Examples, PctFilled)
}

## ---- end-of-data_overview