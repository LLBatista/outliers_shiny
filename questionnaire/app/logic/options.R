box::use(
  utils[read.csv],
)

#' @export
read_options <- function(path, column) {
  if(!file.exists(path)){
    stop("File not found: ", path)
  } 
  data <- read.csv(path, 
                   stringsAsFactors = FALSE, 
                   strip.white = TRUE)
  if(!column %in% names(data)) {
    stop("The file ", path, 
         "must have a column called '", column, "'.")
  }
  sort(unique(data[[column]]))
}