# Plain R code (no Shiny): reading the product options from a CSV file.
box::use(
  utils[read.csv],
)

#' Read the products and their lots from a CSV file.
#' @export
read_products <- function(path) {
  if (!file.exists(path)) {
    stop("Products file not found: ", path)
  }
  product_id <- read.csv(path, stringsAsFactors = FALSE, strip.white = TRUE)
  if (!all(c("product", "lot") %in% names(product_id))) {
    stop("The products file must have the columns 'product' and 'lot'.")
  }
  product_id
}

#' Lots available for one product
#' @export
lots_for_product <- function(product_id, assay) {
  sort(unique(product_id$lot[product_id$product == assay]))
}
