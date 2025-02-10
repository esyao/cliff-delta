######################### CLIFF DELTA CALCULATOR ##########################

library(shiny)
library(dplyr)
library(effsize) # for cliff.delta and cohen.d 

# Define UI ---------------------------------------------------------------

ui <- fluidPage(
  
  # Title ----
  h1("Cliff's Delta Effect Size Calculator"), 

  # Background ----
  h2("Background"), # heading 2
  p("This web-based Shiny app provides an accessible way to calculate Cliff's delta 
    effect size. Cliff's delta is also converted to Cohen's", em("d"), "metric to 
    facilitate interpretation."),
  p("Cliff's delta is a robust and intuitive effect size measure which indicates
    the magnitude of difference between two groups or time points. Unlike common 
    more effect size measures such as Cohen's", em("d,"), "it does not assume normally 
    distributed data. As a result, Cliff's delta is more accurate and appropriate 
    than Cohen's", em("d"), "when data are non-normal and/or are ordinal and therefore have
    reduced variance. There is no down side to using Cliff's delta even when 
    normality assumptions are met."),
  p("Cliff's delta is calculated by comparing each value in one group to each value 
    in the other group, and thus requires the raw dataset (rather than just means 
    and standard deviations). The statistic ranges from -1 to +1, with the extremes 
    indicating no overlap between the groups, and 0 indicating complete overlap. 
    The direction of the sign indicates which group is larger than the other. 
    In other words, the choice of reference group (e.g., which group is placed in 
    Column 1 of the dataset) affects whether the statistic is positive or negative, 
    but will not affect the absolute value of the result."),
  p("Cliff's delta can be converted to the more common Cohen's", em("d"), "metric, 
    since Cohen's", em("d"), "is also a measure of distributional overlap. 
    The table below shows the rule-of-thumb for interpreting Cohen's", em("d"), 
    "alongside its equivalent Cliff's delta value. For example, a Cliff's delta result 
    of 0.15 is equivalent to a Cohen's", em("d"), "of 0.20, and can be interpreted 
    as a small effect."),
  tableOutput("interpretation_table"),

  # Instructions ----
  
  fluidRow(
    # Column 1: Text
    column(width = 10,
           h2("Instructions"), 
           p("To calculate Cliff's delta effect size difference between two groups or 
              time points, upload a CSV (comma-separated values) file by selecting", 
              em("Browse..."), "in the input panel", HTML(paste0("below.", tags$sup("1"))), 
              "The file should have two 
              columns of raw data, and  contain a header row specifying the name for 
              each column. The table on the right provides an example."),
           p("Please ensure that:"),
           tags$ul( 
             tags$li("Names in the header row begin with a letter, and contain",
                     strong("no spaces or special characters."), 
                     "Numbers are allowed (e.g., 'School1', 'School2'). If needed, you 
                      can rename the columns in the input panel below (letters, numbers, 
                      spaces, and special characters are all allowed there)."),
             tags$li("Cells with missing values are left", strong("blank.")), 
             tags$li("The file does not exceed ", strong("5000 rows.")), 
             tags$li("If you are analysing data with two time points and wish to exclude 
                      participants with missing data, please remove the", strong("entire row"), 
                     "of data corresponding to the relevant participants.")
           ),
           p(tags$sup("1"), "To",
             tags$a(href="https://support.microsoft.com/en-us/office/save-a-workbook-to-text-format-txt-or-csv-3e9a9d6c-70da-4255-aa28-fcacf1f081e6", 
                    "save a Microsoft Excel file in CSV format,"), 
             "open the relevant Excel file, then click", em("File"), ">", em("Save As."), 
             "In the popup window, click the dropdown list next to", 
             em("Save as type,"), "then select", em("CSV UTF-8 (Comma-delimited) (.csv).")),
    ),
    
    # Column 2: Example data - first six rows
    column(width = 2,
           br(), br(), 
           tableOutput("sample_data"),
    )
  ),
  
  # Data input panel 
  wellPanel(
    
    # File input: Upload CSV file
    fileInput("upload", label = "Upload CSV file:", accept = ".csv", width = "500px"),
    
    # Text 
    p(strong("Or tick to use example data:")), 
    
    # Checkbox input: Option to use example data (default = FALSE) 
    checkboxInput("example", label = "Use example data", value = FALSE),
    hr(), 

    # Text input: Group names (default = not specified) 
    textInput("group1", label = ("Rename Column 1:"), value = "(optional)", width = "500px"),
    textInput("group2", label = ("Rename Column 2:"), value = "(optional)", width = "500px"),
  ),
  
  # Results ----
  
  # Text 
  h2("Results"), 
  p(em("Results will be shown when a CSV file is uploaded.")),
  
  # Display results table (generated in server)
  tableOutput("results"),
  
  # Display note for table (generated in server)
  htmlOutput("note"), 
  
  # Download button 
  downloadButton("downloadData", label = "Download results (.csv)"),
  
  # Display text interpretation (generated in server)
  htmlOutput("interpretation"), 
  
  # End matter ----
  hr(), 
  p("See Meissel and Yao (2024) for a more detailed guide on Cliff's delta and this Shiny app:"),
  p("Meissel, K., & Yao, E. S. (2024). Using Cliff's delta as a non-parametric 
    effect size measure: An accessible web app and R tutorial.", 
    em("Practical Assessment, Research, and Evaluation, 29,"),
    "Article 2.",
    tags$a(href="https://doi.org/10.7275/pare.1977", "https://doi.org/10.7275/pare.1977")),
  p("") 
)

# Define server -----------------------------------------------------------

server <- function(input, output) {
  
  # Interpretation table
  output$interpretation_table <- renderTable({
    interpretation_table <- data.frame(Interpretation = c("Small", "Medium", "Large"),
                                       Cohen = c(0.20, 0.50, 0.80),
                                       Cliff = c(0.15, 0.33, 0.47)) %>% 
      rename("Cohen's d" = Cohen,
             "Cliff's delta" = Cliff)
  })
  
  # Example data - first 6 rows
  output$sample_data <- renderTable({
    sample_data <- data.frame(School1 = c(3, 1, 4, "", 3,  3),
                              School2 = c(4, 2, 4,  4, 3, ""))
  })
  
  # df (reactive)
  df <- reactive({
    # IF user uploaded a CSV file, import their CSV file
    if(input$example == FALSE){
      req(input$upload)
      df <- read.csv(input$upload$datapath,
                     na.strings = "NA",
                     header = TRUE,
                     fileEncoding = "UTF-8-BOM")
    }
    # ELSE use example data
    else{
      df <- read.csv("data/example-data.csv",
                     na.strings = "NA",
                     header = TRUE,
                     fileEncoding = "UTF-8-BOM")
    }
  })
  
  # Group 1 name (reactive)
  group1 <- reactive({
    # IF user did NOT specify a name for group 1, use column 1 name in df
    if(input$group1 == "(optional)"){
      group1 <- colnames(df()[1])
    }
    # ELSE use user's chosen name for group 1
    else{
      group1 <- input$group1  
    }
  })
  
  # Group 2 name (reactive) 
  group2 <- reactive({
    # IF user did NOT specify a name for group 2, use column 1 name in df
    if(input$group2 == "(optional)"){
      group2 <- colnames(df()[2])
    }
    # ELSE use user's chosen name for group 1
    else{
      group2 <- input$group2
    }
  })
  
  # Cliff's delta results (calculated from raw data; reactive)
  cliff <- reactive({
    cliff <- cliff.delta(df()[[2]], df()[[1]], return.dm = TRUE) # Compare Group 2 to Group 1
  })
  
  # Cohen's d results (with Hedges' correction; calculated from raw data; reactive) 
  cohen <- reactive({
    cohen <- cohen.d(df()[[2]], df()[[1]], hedges.correction = TRUE, na.rm = TRUE, # Compare Group 2 to Group 1
                     pooled = TRUE, paired = FALSE) 
  })
  
  # Collated results table (reactive)
  results <- reactive({
    # Collate desired statistics into a matrix
    results <- data.frame(
      n_1       <- length(na.omit(df()[[1]])), # n in Group 1
      n_2       <- length(na.omit(df()[[2]])), # n in Group 2
      ps_1      <- sum(cliff()$dm <  0)/length(cliff()$dm), # probability group 1 > group 2
      ps_equal  <- sum(cliff()$dm == 0)/length(cliff()$dm), # probability group 1 = group 2
      ps_2      <- sum(cliff()$dm >  0)/length(cliff()$dm), # probability group 1 < group 2
      delta_raw <- cliff()$estimate, # Cliff's delta calculated from raw data
      cohen_converted <- 2*qnorm(-1/(cliff()$estimate-2)), # Cohen's d converted from Cliff's delta
      cohen_raw <- cohen()$estimate # Cohen's d (with Hedges' correction) calculated from raw data
    ) %>% 
      t() %>% 
      as.data.frame() %>% 
      rename(Value = V1) %>% # rename V1 (result) column to "Value"
      # Add descriptive labels for each statistic in a new column labelled "Statistic"
      mutate(Statistic = c(
        paste0("Number of cases in ", group1()),
        paste0("Number of cases in ", group2()),
        paste0("Probability that ", group1(), " > ", group2()),
        paste0("Probability that ", group1(), " = ", group2()),
        paste0("Probability that ", group1(), " < ", group2()),
        "Cliff's delta (calculated from raw data)",
        "Cohen's d (converted from Cliff's delta)",
        "Cohen's d (with Hedges' correction, calculated from raw data)"
      )) %>% 
      # Add 95% CIs for Cliff's (raw), Cohen's (converted from Cliff's), and Cohen's (raw) in new columns
      mutate("Lower 95% CI" = c(NA, NA, NA, NA, NA, cliff()$conf.int[[1]], 2*qnorm(-1/(cliff()$conf.int[[1]]-2)), cohen()$conf.int[[1]])) %>%
      mutate("Upper 95% CI" = c(NA, NA, NA, NA, NA, cliff()$conf.int[[2]], 2*qnorm(-1/(cliff()$conf.int[[2]]-2)), cohen()$conf.int[[2]])) %>% 
      select(Statistic, everything()) %>% 
      mutate(across(-Statistic, ~round(., digits = 5))) 
  })
  
  # Direction of Cliff's delta (reactive)
  direction <- reactive({
    direction <- ifelse(
      # If delta is *positive* >> Column 2's distribution is *larger*
      round(results()[6,2],2) > 0, "larger", 
      # If delta is *negative* >> Column 2's distribution is *smaller*,
      # Otherwise, the distributions are *equal* 
      ifelse(round(results()[6,2],2) < 0, "smaller", "equal to")
    )
  })
  
  # Render collated results table 
  output$results <- renderTable({
    results()
  }, digits = 2)
  
  # Table note
  output$note <- renderText({
    HTML(paste(
      "<p><i>Note.</i> Effect sizes are <i>positive</i> when the distribution of ", 
      group2(), " is <i>larger</i> than ", group1(), 
      ", and <i>negative</i> when the distribution of ", group2(), 
      " is <i>smaller</i> than ", group1(), ". CI = confidence interval.</p>",
      sep = ""
    ))
  })
  
  # Download collated results table
  output$downloadData <- downloadHandler(
    filename = "cliff-delta-results.csv",
    content = function(file) {
      write.csv(results(), file, row.names = FALSE)
    })
  
  # Generate written interpretation 
  output$interpretation <- renderText({
    HTML(paste(
      # % distributional overlap
      "<h2>Interpretation</h2>",
      "<p>The distribution of <b>", group2(), # Column 2 name
      "</b> is <b>", direction(), # larger, smaller, or equal to
      "</b> than <b>", group1(), # Column 1 name
      "</b>. There is a <b>", round(100-abs(results()[6,2])*100, 1), # absolute % overlap
      "%</b> overlap between the two distributions.</p>",
      
      # Cliff's delta & robust Cohen's d
      "<p>The Cliff's delta effect size is <b>", round(results()[6,2], 2), # Cliff's delta value
      "</b>. This is equivalent to a Cohen's <i>d</i> of <b>", 
      round(results()[7,2], 2), # Cohen's d converted from Cliff's delta
      "</b>, corresponding to a <b>", 
      as.character(cliff()$magnitude), # Cliff's delta interpretation
      "</b> effect size.</p>", 
      
      # Cohen's d from raw data
      "<p>For reference, Cohen's <i>d</i> (with Hedges' correction) as calculated from raw data is <b>", 
      round(results()[8,2], 2), # raw Cohen's d value
      "</b>, corresponding to a <b>", 
      as.character(cohen()$magnitude), # raw Cohen's d interpretation
      "</b> effect size. ",
      "The discrepancy between the two Cohen's <i>d</i> values (<b>", 
      round(results()[8,2], 2) - round(results()[7,2], 2), # raw d - converted d
      "</b>) indicates the degree of bias attributable to using raw Cohen's <i>d</i> for non-normal data. </p>", 
      sep = ""
    ))
  })
}

# Run app -----------------------------------------------------------------

shinyApp(ui, server)
