# Load necessary packages
library(pdftools)
library(data.table)
library(magrittr)
library(tesseract)
library(stringr)
rm(list = ls()) # clean env

# define global vars
(spa <- tesseract("spa"))
table_fin <- data.table()
pdf_pages <- listed_pdfs <- list()
pdf_length <- pdf_length("R/input/RNT_202402.pdf")
pdf_range <- seq(1, pdf_length, 49)

# Split PDF into individual pages
for (i in seq_along(pdf_range)) {
    pdf_pages[[i]] <- pdf_subset("R/input/RNT_202402.pdf",
        pages = pdf_range[i]:pdf_range[i + 1]
    )
    pdf_split(pdf_pages[[i]])
}

# Loop through each page
for (i in seq_along(pdf_length)) {
    # Extract text from current page
    text <- pdf_data(pdf_pages[[i]]) %>% as.data.table()
    text[, index := .I]

    # Find the row numbers of the delimiters
    delim1 <- text[text == "0111", index]
    delim2 <- text[text == "empresario", index]

    # Extract the required information
    if (!is.na(delim1) & !is.na(delim2)) {
        cuenta_cot <- text[index == delim1 + 1]$text
        nif <- text[index == delim2 + 2]$text

        # Add the extracted information to table_fin
        row_to_add <- data.table(
            page = i,
            cuenta_cot = cuenta_cot, nif = nif
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
    pdf_subset("R/input/RNT_202402.pdf",
        pages = listed_pdfs[[i]],
        output = paste0("R/output/", unique_nifs[i], ".pdf")
    )
}
