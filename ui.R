library(shiny)
library(plotly)

shinyUI(fluidPage(
  titlePanel("PfKinomedb: Plasmodium Falciparum Kinome Database"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("gene_name", "Enter Gene Names for Lookup (comma-separated):", ""),
      actionButton("lookup", "Lookup Genes"),
      numericInput("top_go", "Top GO Categories:", value = 10, min = 1),
      actionButton("go_enrichment", "Perform GO Enrichment"),
      numericInput("top_kegg", "Top KEGG Pathways:", value = 10, min = 1),
      actionButton("kegg_enrichment", "Perform KEGG Enrichment"),
      downloadButton("downloadData", "Download Lookup Results"),
      verbatimTextOutput("summary"),
      verbatimTextOutput("gene_data"),
      verbatimTextOutput("go_results"),
      verbatimTextOutput("kegg_results")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("General Plots",
                 plotlyOutput("length_distribution"),
                 plotlyOutput("organism_frequency")
        ),
        tabPanel("GO Enrichment",
                 plotlyOutput("go_plot")
        ),
        tabPanel("KEGG Enrichment",
                 plotlyOutput("kegg_plot")
        )
      )
    )
  )
))
