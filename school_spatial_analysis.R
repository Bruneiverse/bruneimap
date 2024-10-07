library(tidyverse)
# remotes::install_github("propertypricebn/bruneimap")
library(bruneimap)
library(ggrepel)
library(kernlab)
library(osrm)
library(osmdata)
library(readxl)


# 1. DATA COLLECTION/CLEANING
#   A. GIS - School-----------------------------------------------------

#   B. Study variable - MOE 2018 ---------------------------------------------------
# Trimmed pdf using pdftools: pdf_subset("moe2018.pdf", pages = c(127:145), output = "moe2018_extracted.pdf")
# Split landscape (2 pages per page) to portrait using https://deftpdf.com/split-pdf-down-the-middle
# Converted to excel using online ilovepdf.com
# Clean and tidy on MS Excel

# 2. DATA CLEANING --------------------------------------------------------
#   A. GIS - School ---------------------------------------------------------------------
# Filter out schools not in Brunei
# Match to MOE school listing


#   B. Study Variable - MOE 2018 --------------------------------------------

