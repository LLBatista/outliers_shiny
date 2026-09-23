# Plain R code (no Shiny): reading the product options from a CSV file.
box::use(
  utils[read.csv],
)

#' Read the list of products from a CSV file.
#'
#' The file must have a column called `product`, with one product per row.
#' Returns a sorted character vector without blanks or duplicates.
#' @export
read_products <- function(path) {
  if (!file.exists(path)) {
    stop("Products file not found: ", path)
  }
  products <- read.csv(path, stringsAsFactors = FALSE)
  if (!"product" %in% names(products)) {
    stop("The products file must have a column called 'product'.")
  }
  values <- trimws(products$product)
  sort(unique(values[!is.na(values) & values != ""]))
}
