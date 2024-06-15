library(shiny)
library(RSQLite)
library(DBI)
library(dplyr)
library(ggplot2)
library(clusterProfiler)
library(org.Pf.plasmo.db)
library(writexl)
library(plotly)

shinyServer(function(input, output, session) {
  # Connect to the database
  con <- dbConnect(SQLite(), "plasmodium_falciparum_kinome.db")
  
  # Function to summarize the kinases data
  summarize_kinases <- function(con) {
    query <- "SELECT Organism, COUNT(*) AS kinase_count, AVG(Length) AS avg_length FROM kinases GROUP BY Organism"
    result <- dbGetQuery(con, query)
    return(result)
  }
  
  # Function to retrieve kinase data by gene names
  get_kinase_by_genes <- function(con, gene_names) {
    gene_names <- sapply(gene_names, function(x) paste0("'", x, "'"))
    gene_names <- paste(gene_names, collapse = ",")
    query <- sprintf("SELECT * FROM kinases WHERE Gene_Names IN (%s)", gene_names)
    result <- dbGetQuery(con, query)
    return(result)
  }
  
  # Function to perform GO enrichment analysis for Plasmodium falciparum
  perform_go_enrichment_pf <- function(gene_vector) {
    go_results <- enrichGO(gene = gene_vector,
                           OrgDb = org.Pf.plasmo.db,
                           keyType = "ORF",
                           ont = "ALL",
                           pAdjustMethod = "BH",
                           pvalueCutoff = 0.05,
                           qvalueCutoff = 0.05,
                           readable = TRUE)
    return(go_results)
  }
  
  # Function to perform KEGG pathway enrichment analysis for Plasmodium falciparum
  perform_kegg_enrichment_pf <- function(gene_vector) {
    kegg_results <- enrichKEGG(gene = gene_vector,
                               organism = 'pfa',
                               keyType = 'kegg',
                               pAdjustMethod = "BH",
                               pvalueCutoff = 0.05,
                               qvalueCutoff = 0.05)
    return(kegg_results)
  }
  
  # Summarize kinases
  output$summary <- renderPrint({
    summary <- summarize_kinases(con)
    print("Summary of Kinases by Organism:")
    print(summary)
  })
  
  observeEvent(input$lookup, {
    req(input$gene_name)
    gene_names <- strsplit(input$gene_name, ",")[[1]]
    gene_names <- trimws(gene_names)
    
    kinase_data <- get_kinase_by_genes(con, gene_names)
    
    output$gene_data <- renderPrint({
      print(paste("Kinase Data for Genes:", paste(gene_names, collapse = ", ")))
      print(kinase_data)
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("kinase_data_", Sys.Date(), ".xlsx", sep = "")
      },
      content = function(file) {
        writexl::write_xlsx(kinase_data, file)
      }
    )
  })
  
  output$length_distribution <- renderPlotly({
    query <- "SELECT Length FROM kinases"
    lengths <- dbGetQuery(con, query)
    
    p <- ggplot(lengths, aes(x = Length)) +
      geom_histogram(binwidth = 50, fill = "blue", color = "black") +
      theme_minimal() +
      labs(title = "Distribution of Kinase Lengths", x = "Length", y = "Frequency")
    ggplotly(p)
  })
  
  output$organism_frequency <- renderPlotly({
    query <- "SELECT Organism FROM kinases"
    organisms <- dbGetQuery(con, query) %>%
      count(Organism)
    
    p <- ggplot(organisms, aes(x = reorder(Organism, -n), y = n)) +
      geom_bar(stat = "identity", fill = "green", color = "black") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Frequency of Different Organisms", x = "Organism", y = "Count")
    ggplotly(p)
  })
  
  observeEvent(input$go_enrichment, {
    selected_genes <- dbGetQuery(con, "SELECT Gene_Names FROM kinases")$Gene_Names
    go_enrichment_results_pf <- perform_go_enrichment_pf(selected_genes)
    
    output$go_results <- renderPrint({
      print(as.data.frame(go_enrichment_results_pf))
    })
    
    output$go_plot <- renderPlotly({
      if (!is.null(go_enrichment_results_pf) && nrow(as.data.frame(go_enrichment_results_pf)) > 0) {
        top_n <- input$top_go
        p <- barplot(go_enrichment_results_pf, showCategory = top_n, title = "GO Enrichment Results")
        ggplotly(p) %>%
          layout(margin = list(l = 200, r = 50, b = 100, t = 50),
                 yaxis = list(tickangle = 0, tickfont = list(size = 15)))
      } else {
        NULL
      }
    })
  })
  
  observeEvent(input$kegg_enrichment, {
    selected_genes <- dbGetQuery(con, "SELECT Gene_Names FROM kinases")$Gene_Names
    kegg_enrichment_results_pf <- perform_kegg_enrichment_pf(selected_genes)
    
    output$kegg_results <- renderPrint({
      print(as.data.frame(kegg_enrichment_results_pf))
    })
    
    output$kegg_plot <- renderPlotly({
      if (!is.null(kegg_enrichment_results_pf) && nrow(as.data.frame(kegg_enrichment_results_pf)) > 0) {
        top_n <- input$top_kegg
        p <- barplot(kegg_enrichment_results_pf, showCategory = top_n, title = "KEGG Enrichment Results")
        ggplotly(p) %>%
          layout(margin = list(l = 200, r = 50, b = 100, t = 50),
                 yaxis = list(tickangle = 0, tickfont = list(size = 15)))
      } else {
        NULL
      }
    })
  })
  
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
})
