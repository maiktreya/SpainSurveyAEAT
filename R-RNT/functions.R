# Load necessary packages
library(pdftools)
library(data.table)
library(magrittr)
library(stringr)
library(tcltk)

# Define function to extract information from a single PDF page
extract_info <- function(page_text) {
  # Find the row numbers of the delimiters
  delim1 <- which(page_text == "0111")
  delim2 <- which(page_text == "empresario")

  # Extract the required information
  if (length(delim1) > 0 && length(delim2) > 0) {
    cuenta_cot <- page_text[delim1 + 1]
    nif <- page_text[delim2 + 2]

    return(data.table(cuenta_cot = cuenta_cot, nif = nif))
  } else {
    return(data.table())
  }
}

# Define function to split a PDF into individual pages
split_pdf <- function(file_path) {
  # Get the number of pages in the PDF
  pdf_length <- pdf_info(file_path)$pages

  # Split PDF into individual pages
  pdf_pages <- lapply(seq_len(pdf_length), function(i) {
    pdf_subset(file_path, pages = i)
  })

  # List created individual pdf files
  pdf_files <- list.files(pattern = "$.pdf")

  return(pdf_files)
}

# Define function to merge individual PDF pages by NIF
merge_pdfs <- function(table_fin) {
  # Get an ordered list of unique NIFs
  unique_nifs <- unique(table_fin$nif)

  # Merge individual pdfs by NIF according to table_fin description
  for (i in seq_along(unique_nifs)) {
    pages <- table_fin[nif == unique_nifs[i], pagina]

    # Create output file name
    output_file <- file.path("R/output", paste0(unique_nifs[i], ".pdf"))

    # Merge PDF pages and save to output file
    pdf_subset(file_path,
               pages = pages,
               output = output_file)
  }
}
