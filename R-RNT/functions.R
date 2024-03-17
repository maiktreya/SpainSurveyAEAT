# Load necessary libraries magrittr and data.table

# Function to create specified directories if they do not already exist
create_directories <- function(trans_dir, ind_dir, out_dir) {
  # Define the list of directories
  dirs <- list(trans_dir, ind_dir, out_dir)
  # Iterate through the list to create each directory if it doesn't exist
  lapply(dirs, function(dir) {
    if (!dir.exists(dir)) dir.create(dir)
  })
}

# Function to split a PDF file into individual pages, saving them in specified directories
split_pdfs_into_pages <- function(file_path, trans_dir, ind_dir, pdf_range) {
  for (i in seq_along(pdf_range)) {
    # Define file paths for intermediate and final PDFs
    intermediate_file_path <- paste0(trans_dir, "/out", i, ".pdf")
    final_file_path <- paste0(ind_dir, "/out", i, ".pdf")

    # Use pdf_subset from pdftools to create subsets of the PDF
    # If it's the last item in range, use the single page. Otherwise, use a range of pages
    if (i == length(pdf_range)) {
      pdftools::pdf_subset(file_path, pages = pdf_range[i], output = intermediate_file_path)
    } else {
      pdftools::pdf_subset(file_path, pages = pdf_range[i]:(pdf_range[i + 1] - 1), output = intermediate_file_path)
    }

    # Split the intermediate PDF into individual pages
    pdftools::pdf_split(intermediate_file_path, output = final_file_path)
  }
}

# Function to extract and process text from PDF files located in a specified directory
extract_and_process_text <- function(ind_dir) {
  # Get a list of PDF files in the directory
  pdf_files <- list.files(path = ind_dir, pattern = "\\.pdf$")
  # Initialize an empty data.table to store results
  table_fin <- data.table::data.table()

  # Loop through each file and extract text
  for (i in seq_along(pdf_files)) {
    # Extract text from the current PDF file
    text <- pdftools::pdf_data(paste0(ind_dir, "/", pdf_files[i])) %>% data.table::as.data.table()
    text[, index := .I] # Add a column with the row index

    # Find the row numbers for specific delimiter texts
    delim1 <- text[text == "0111", index]
    delim2 <- text[text == "9", index][1]

    # If delimiters are found, extract the required information and add it to the result table
    if (!is.na(delim1) & !is.na(delim2)) {
      cuenta_cot <- text[index == delim1 + 1]$text
      nif <- text[index == delim2 + 1]$text

      row_to_add <- data.table::data.table(page = i, cuenta_cot = cuenta_cot, nif = nif)
      table_fin <- rbind(table_fin, row_to_add)
    }
  }

  # Order the result table by NIF and return it
  table_fin <- table_fin[order(nif)]
  return(table_fin)
}

# Function to merge PDFs based on NIF, creating a single PDF for each unique NIF
merge_pdfs_by_nif <- function(table_fin, file_path, out_dir) {
  # Get a list of unique NIFs
  unique_nifs <- unique(table_fin$nif)

  # Loop through each unique NIF to merge PDF pages associated with it
  for (i in seq_along(unique_nifs)) {
    listed_pdfs <- table_fin[nif == unique_nifs[i], page]

    # Merge pages into a single PDF for the current NIF
    pdftools::pdf_subset(file_path,
                         pages = listed_pdfs,
                         output = paste0(out_dir, "/", unique_nifs[i], ".pdf"))
  }
}
