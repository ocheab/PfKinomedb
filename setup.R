# Load necessary libraries
library(RSQLite)
library(DBI)
library(readr)
library(dplyr)
library(ggplot2)
library(clusterProfiler)
library(org.Pf.plasmo.db)

# Define the path to the CSV file (use relative path)
file_path <- "KINOME.csv"

# Read the CSV file
kinome_data <- read_csv(file_path)

# Print the column names to check the current names
print(colnames(kinome_data))

# Clean and rename columns to match the new database schema
kinome_data <- kinome_data %>%
  rename_with(~ gsub(" ", "_", .), everything()) %>%
  rename_with(~ gsub("[^[:alnum:]_]", "", .), everything())

# Check the cleaned and renamed column names
print(colnames(kinome_data))

# Connect to the database
con <- dbConnect(SQLite(), "plasmodium_falciparum_kinome.db")

# Drop the existing table if it exists
dbExecute(con, "DROP TABLE IF EXISTS kinases")

# Define the new schema to match the CSV columns
dbExecute(con, "
CREATE TABLE IF NOT EXISTS kinases (
  UNIPROT_ID TEXT,
  Entry_Name TEXT,
  Protein_names TEXT,
  Gene_Names TEXT,
  Organism TEXT,
  Length INTEGER,
  Sequence TEXT,
  EC_number TEXT,
  Biological_process TEXT,
  Cellular_component TEXT,
  GO TEXT,
  Molecular_function TEXT,
  GIDs TEXT
)")

# Function to populate the database
populate_database <- function(con, data) {
  dbWriteTable(con, "kinases", data, append = TRUE, row.names = FALSE)
}

# Populate the database with CSV data
populate_database(con, kinome_data)

# Basic Analysis and Visualization Functions

# 1. Distribution of Protein Lengths
plot_protein_length_distribution <- function(data) {
  ggplot(data, aes(x = Length)) +
    geom_histogram(binwidth = 50, fill = "blue", color = "black") +
    theme_minimal() +
    labs(title = "Distribution of Protein Lengths", x = "Protein Length", y = "Frequency")
}
