# Clean environment to ensure a fresh start
rm(list = ls())

# Load necessary packages for PDF manipulation, data handling, and piping
library(pdftools) # For PDF file manipulation
library(data.table) # For efficient data manipulation
library(magrittr) # For the pipe operator (%>%)
library(stringr) # For string operations, though not explicitly used in this script

# Source external functions from the functions.R file
# Note: This requires the magrittr and data.table packages
source("R-RNT/functions.R")

# Define global variables and directories for processing
file_path <- tcltk::tk_choose.files() # Interactive file selection dialog
pdf_length <- pdf_length(file_path) # Determine the number of pages in the PDF
pdf_range <- c(seq(1, pdf_length, 49), pdf_length) # Define a range for processing, to avoid memory issues

# Directories for storing intermediate and final processing files
trans_dir <- "R-RNT/trans" # Directory for storing intermediate PDFs
ind_dir <- "R-RNT/ind" # Directory for individual PDF pages
out_dir <- "R-RNT/output" # Output directory for the final merged PDFs

# Workflow execution
# Step 1: Create necessary directories
create_directories(trans_dir, ind_dir, out_dir)

# Step 2: Split the main PDF into smaller chunks and individual pages
split_pdfs_into_pages(file_path, trans_dir, ind_dir, pdf_range)

# Step 3: Extract and process text from individual PDF pages
# This step involves reading the text from each PDF, identifying specific patterns,
# and extracting relevant information based on these patterns.
table_fin <- extract_and_process_text(ind_dir)

# Step 4: Print the extracted and processed information for verification
print(table_fin)

# Step 5: Merge individual PDF pages into documents based on NIF (a unique identifier)
merge_pdfs_by_nif(table_fin, file_path, out_dir)

# Cleanup: Remove intermediate directories to clean up the workspace
unlink(trans_dir, recursive = TRUE)
unlink(ind_dir, recursive = TRUE)
