# Social contributions classification by group companies

# Load necessary packages
library(pdftools)
library(data.table)
library(magrittr)
library(stringr)

rm(list = ls()) # clean env

# define global vars
file_path <- tcltk::tk_choose.files() # interactive file selection
table_fin <- data.table() # matrix objects
pdf_pages <- listed_pdfs <- list() # nested lists
pdf_length <- pdf_length(file_path) # number of pdf pages
pdf_range <- c(seq(1, pdf_length, 55), pdf_length) # chunks of 50 to avoid crashes

# Define directories
trans_dir <- "R/trans"
ind_dir <- "R/ind"
out_dir <- "R/output"

# Create the directories if they don't exist
if (!dir.exists(trans_dir)) dir.create(trans_dir)
if (!dir.exists(ind_dir)) dir.create(ind_dir)
if (!dir.exists(out_dir)) dir.create(out_dir)

# Split PDF into individual pages, saving intermediate files in the "intermediate" directory
for (i in seq_along(pdf_range)) {
    # Define paths for intermediate and final files
    intermediate_file_path <- paste0(trans_dir, "/out", i, ".pdf")
    final_file_path <- paste0(ind_dir, "/out", i, ".pdf") # Placeholder %d for page numbers

    # Create intermediate subset PDF
    if (i == length(pdf_range)) {
        # For the last iteration, use the current index i
        pdf_pages <- pdf_subset(file_path,
            pages = pdf_range[i],
            output = intermediate_file_path
        )
    } else {
        # For all other iterations, use a range that ends at the current index i
        pdf_pages <- pdf_subset(file_path,
            pages = pdf_range[i]:(pdf_range[i + 1] - 1),
            output = intermediate_file_path
        )
    }

    # Split the intermediate PDF into individual pages, saving them in the "final" directory
    pdf_split(pdf_pages, output = final_file_path)
}


# list created individual pdf files
pdf_files <- list.files(path = ind_dir, pattern = "\\.pdf$")

# Loop through each page
for (i in seq_along(pdf_files)) {
    # Extract text from current page
    text <- pdf_data(paste0(ind_dir, "/", pdf_files[i])) %>% as.data.table()
    text[, index := .I]

    # Find the row numbers of the delimiters
    delim1 <- text[text == "0111", index]
    delim2 <- text[text == "9", index][1]

    # Extract the required information
    if (!is.na(delim1) & !is.na(delim2)) {
        cuenta_cot <- text[index == delim1 + 1]$text
        nif <- text[index == delim2 + 1]$text

        # Add the extracted information to table_fin
        row_to_add <- data.table(
            page = i,
            cuenta_cot = cuenta_cot,
            nif = nif
        )
        table_fin <- rbind(table_fin, row_to_add)
    }
}

# order table by nif groups
table_fin <- table_fin[order(nif)]

# get an ordered list of included NIFs
unique_nifs <- unique(table_fin$nif)

# show included NIFs and CCC for verification
print(table_fin)

# merge individual pdfs by NIF according to table_fin description
for (i in seq_along(unique_nifs)) {
    listed_pdfs[[i]] <- table_fin[nif == unique_nifs[i], page]

    pdf_subset(file_path,
        pages = listed_pdfs[[i]],
        output = paste0(out_dir, "/", unique_nifs[i], ".pdf")
    )
}

unlink(trans_dir, recursive = TRUE)
unlink(ind_dir, recursive = TRUE)
