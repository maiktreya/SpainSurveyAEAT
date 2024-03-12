# Clean environment
rm(list = ls())
source("R-RNT/functions.R")

# Define global variables
file_path <- tcltk::tk_choose.files()  # Interactive file selection
table_fin <- data.table()              # Matrix objects
pdf_pages <- listed_pdfs <- list()    # Nested lists

# Validate input file
if (!file.exists(file_path)) {
  stop("Error: File not found.")
}

# Split PDF into individual pages
pdf_files <- split_pdf(file_path)

# Loop through each page
for (i in seq_along(pdf_files)) {
  # Extract text from current page
  text <- pdf_text(pdf_files[i]) %>%
    strsplit(split = "\n") %>%
    unlist() %>%
    as.data.table()

  # Add page number to text data table
  text[, pagina := i]

  # Extract information from current page
  info <- extract_info(text$V1)

  # Add extracted information to table_fin
  table_fin <- rbind(table_fin, info, fill = TRUE)
}

# Order table by nif groups
table_fin <- table_fin[order(nif)]

# Show included NIFs and CCC for verification
print(table_fin)

# Merge individual PDF pages by NIF
merge_pdfs(table_fin)