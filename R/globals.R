## Suppress R CMD check NOTEs about undefined global variables.
## - `ui` and `server` are created by source(..., local = TRUE) inside
##   launchCerebroV1.x functions.
## - `group` is used as a column name in dplyr pipelines (tibble).
## - `.` is used in magrittr/dplyr pipe expressions.
## - `Cerebro.options` is assigned with <<- intentionally (global option store).
utils::globalVariables(c("ui", "server", "group", ".", "Cerebro.options"))

## Initialize Cerebro.options in the package namespace so that <<- assignment
## in launchCerebroV1.x has a visible binding and R CMD check does not NOTE it.
Cerebro.options <- NULL
