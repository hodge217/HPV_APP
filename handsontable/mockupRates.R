
library(shiny)
library(rhandsontable)
library(RSQLite)
rm(list=ls())

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$style(
    HTML(
      ".box.box-solid.box-success>.box-header {
                                      background:#fca500
                                      }
                                      .box.box-solid.box-success{
                                      border-bottom-color:#fca500;
                                      border-left-color:#fca500;
                                      border-right-color:#fca500;
                                      border-top-color:#fca500
                                      }"
    )
  ),
  # this is to change the 'status=success' box color to yellow instead of green
  # Make the error messages red when we use the validate function
  tags$style(HTML(
    ".shiny-output-error-red_warnings {
                          color: red;
                          }
                          "
  )),
  tags$style(
    HTML(
      ".shiny-input-container:not(.shiny-input-container-inline) {
              width: 100%;
              }"
    )
  ),
  
  # Show a plot of the generated distribution
  #mainPanel(
    h2("Table demo"),
    uiOutput("Q28"),
    uiOutput("Q29"),
    HTML("<br><br>"),
    uiOutput("brates_9_10"),
    HTML("<br>"),
    uiOutput("brates_11_12"),
    HTML("<br>"),
    uiOutput("brates_13")

)




# Define server logic required to draw a histogram
server <- function(input, output) {
 load("SavedRates.Rdata")
 
  
  # Define the input datasets
  datasets <- list(
    FemAge1 = dplyr::select(savedRates, FemAge1_total, FemAge1_dose1, FemAge1_dose2) %>%
      t(),
    MenAge1 = dplyr::select(savedRates, MenAge1_total, MenAge1_dose1, MenAge1_dose2) %>%
      t(),
    BothAge1 = dplyr::select(savedRates, BothAge1_total, BothAge1_dose1, BothAge1_dose2) %>%
      t(),
    
    FemAge2 = dplyr::select(savedRates, FemAge2_total, FemAge2_dose1, FemAge2_dose2, FemAge2_mening, FemAge2_tdap) %>%
      t(),
    MenAge2 = dplyr::select(savedRates, MenAge2_total, MenAge2_dose1, MenAge2_dose2, MenAge2_mening, MenAge2_tdap) %>%
      t(),
    BothAge2 = dplyr::select(savedRates, BothAge2_total, BothAge2_dose1, BothAge2_dose2, BothAge2_mening, BothAge2_tdap) %>%
      t(),
    
    FemAge3 = dplyr::select(savedRates, FemAge3_total, FemAge3_dose1, FemAge3_dose2, FemAge3_mening, FemAge3_tdap) %>%
      t(),
    MenAge3 = dplyr::select(savedRates, MenAge3_total, MenAge3_dose1, MenAge3_dose2, MenAge3_mening, MenAge3_tdap) %>%
      t(),
    BothAge3 = dplyr::select(savedRates, BothAge3_total, BothAge3_dose1, BothAge3_dose2, BothAge3_mening, BothAge3_tdap) %>%
      t()
  )
  
  # Make the datasets look right
  datasets <- lapply(datasets, function(df) {
    df <- df %>%
      data.frame()
    names(df) <- "N"
    if (nrow(df) == 3) {
      df$Lab <- c("Total number of patients seen in 2020",
                  "Number of active patients who received at least one (1) HPV dose",
                  "Number of active patients who received both (2) HPV doses")
    } else {
      df$Lab <- c("Total number of patients seen in 2020",
                  "Number of active patients who received at least one (1) HPV dose",
                  "Number of active patients who received both (2) HPV doses",
                  "Meningococcal",
                  "Tdap")
    }
    df <- df[,c("Lab", "N")]
    df$Rate <- ""
    df[,names(df)] <- lapply(df[,names(df)], as.character)
    return(df)
  })
  
  
  FemAge1 <- reactive(datasets$FemAge1)
  FemAge2 <- reactive(datasets$FemAge2)
  FemAge3 <- reactive(datasets$FemAge3)

  MenAge1 <- reactive(datasets$MenAge1)
  MenAge2 <- reactive(datasets$MenAge2)
  MenAge3 <- reactive(datasets$MenAge3)

  BothAge1 <- reactive(datasets$BothAge1)
  BothAge2 <- reactive(datasets$BothAge2)
  BothAge3 <- reactive(datasets$BothAge3)

  
  output$Q28 <- renderUI({
    selectInput(
      inputId = "Q28",
      label = "28. For what ages are you reporting (select all that apply)?",
      choices = c("",
                  "9-10",
                  "11-12",
                  "13"),
      selected = "",
      multiple = T)
  })
  output$Q29 <- renderUI({
    selectInput(
      inputId = "Q29",
      label = "29.  Can you pull rate data by sex?",
      choices = c("",
                  "Yes, we'll report separate data for male and female patients",
                  "No, we will have combined rate data"),
      selected = "")
  })
  
  
  # Custom renderer function for the table
  color_renderer <- "
  function(instance, td) {
    Handsontable.renderers.TextRenderer.apply(this, arguments);
    td.style.color = 'red';
  }
"
  
  
  
  # For boys and girls separately
  # Create the ages 9-10 boxes
  observeEvent(list(input$Q28, input$Q29), {
    validate(need(input$Q28, ""))
    validate(need(input$Q29, ""))
    
    # Boys and girls - ages 9-10
    if ("9-10" %in% input$Q28 &
        input$Q29 == "Yes, we'll report separate data for male and female patients") {
      
      output$brates_9_10 <- renderUI({
        splitLayout(
                 verticalLayout(
                   h4("Vaccination Rates for Female Patients, Ages 9-10", align = "center"),
                   uiOutput("uiFemAge1")),
                 verticalLayout(
                   h4("Vaccination Rates for Male Patients, Ages 9-10", align = "center"),
                   uiOutput("uiMenAge1"))
        )

      })
      output$uiFemAge1 <- renderUI({
        rHandsontableOutput("tblFemAge1", width = "100%")
      })
      output$uiMenAge1 <- renderUI({
        rHandsontableOutput("tblMenAge1", width = "100%")
      })
      
     # Function to track changes
      change.FemAge1 <- reactive({
        if (is.null(input$tblFemAge1)) {
          FemAge1()
        } else if (!identical(FemAge1(), input$tblFemAge1)) {
          mytable <- as.data.frame(hot_to_r(input$tblFemAge1))
          total <- as.numeric(mytable$N[1])
          rest <- as.numeric(mytable$N[-1])
          prop <- ((rest / total) * 100)
          
          total <- mytable[1,]
          total$Error <- ifelse(is.na(total$N), "This number is required", "")
          
          rest <- data.frame(mytable[-1,c("Lab", "N")], Rate = prop)
          rest$Error <- with(rest, ifelse(
            is.na(N), "This number is required", ifelse(
              Rate > 100, "This number can't be greater than the total number of patients seen",
              paste0(format(round(Rate, 2), nsmall = 2),
                     "% vaccination rate in this age group"))))
          
          if(!is.na(rest$Rate[1]) & !is.na(rest$Rate[2]) & rest$Rate[2] > rest$Rate[1]) {
            condition <- 1
          } else {
            condition <- 0
          }
          rest$Error <- ifelse(rest$Lab == "Number of active patients who received both (2) HPV doses" &
                               rest$Error != "This number is required" &
                               condition == 1, "This number can't be greater than those patients getting 1+ doses",
                               rest$Error)
          
          final <- rbind(total, rest)
          final$Rate <- final$Error
          dplyr::select(final, Lab, N, Rate)
        }
      })
      output$tblFemAge1 <- renderRHandsontable({
          rhandsontable(change.FemAge1(), rowHeaders = NULL, stretchH = "all", height = 200) %>%
          hot_cols(colWidths = c(100, 25, 100),
                   manualColumnMove = FALSE,
                   manualColumnResize = TRUE
                   ##, wordWrap = "yes please"
          ) %>%
          hot_col("N", halign = "htCenter", renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }")  %>%
          hot_col("Rate", readOnly = T, renderer = color_renderer)
      })
      change.MenAge1 <- reactive({
        if (is.null(input$tblMenAge1)) {
          MenAge1()
        } else if (!identical(MenAge1(), input$tblMenAge1)) {
          mytable <- as.data.frame(hot_to_r(input$tblMenAge1))
          total <- as.numeric(mytable$N[1])
          rest <- as.numeric(mytable$N[-1])
          prop <- ((rest / total) * 100)
          
          total <- mytable[1,]
          total$Error <- ifelse(is.na(total$N), "This number is required", "")
          
          rest <- data.frame(mytable[-1,c("Lab", "N")], Rate = prop)
          rest$Error <- with(rest, ifelse(
            is.na(N), "This number is required", ifelse(
              Rate > 100, "This number can't be greater than the total number of patients seen",
              paste0(format(round(Rate, 2), nsmall = 2),
                     "% vaccination rate in this age group"))))
          if(!is.na(rest$Rate[1]) & !is.na(rest$Rate[2]) & rest$Rate[2] > rest$Rate[1]) {
            condition <- 1
          } else {
            condition <- 0
          }
          rest$Error <- ifelse(rest$Lab == "Number of active patients who received both (2) HPV doses" &
                                 rest$Error != "This number is required" &
                                 condition == 1, "This number can't be greater than those patients getting 1+ doses",
                               rest$Error)
          final <- rbind(total, rest)
          final$Rate <- final$Error
          dplyr::select(final, Lab, N, Rate)
        }
      })
      output$tblMenAge1 <- renderRHandsontable({
        rhandsontable(change.MenAge1(), rowHeaders = NULL, stretchH = "all", height = 200) %>%
          hot_cols(colWidths = c(100, 25, 100),
                   manualColumnMove = FALSE,
                   manualColumnResize = TRUE
                   ##, wordWrap = "yes please"
          ) %>%
          hot_col("N", halign = "htCenter", renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }")  %>%
          hot_col("Rate", readOnly = T, renderer = color_renderer)
      })
    }  # end boys and girls age 9-10
    
    
    # Boys and girls - ages 9-10
    if ("11-12" %in% input$Q28 &
        input$Q29 == "Yes, we'll report separate data for male and female patients") {
      
      output$brates_11_12 <- renderUI({
        splitLayout(
                 verticalLayout(
                   h4("Vaccination Rates for Female Patients, Age 11-12", align = "center"),
                   uiOutput("uiFemAge2")),
                 verticalLayout(
                 h4("Vaccination Rates for male Patients, Ages 11-12"),
                 uiOutput("uiMenAge2"))
        )
      })
      output$uiFemAge2 <- renderUI({
        rHandsontableOutput("tblFemAge2", width = "100%")
      })
      output$uiMenAge2 <- renderUI({
        rHandsontableOutput("tblMenAge2", width = "100%")
      })
      
      # Function to track changes
      change.FemAge2 <- reactive({
        if (is.null(input$tblFemAge2)) {
          FemAge2()
        } else if (!identical(FemAge2(), input$tblFemAge2)) {
          mytable <- as.data.frame(hot_to_r(input$tblFemAge2))
          total <- as.numeric(mytable$N[1])
          rest <- as.numeric(mytable$N[-1])
          prop <- ((rest / total) * 100)
          
          total <- mytable[1,]
          total$Error <- ifelse(is.na(total$N), "This number is required", "")
          
          rest <- data.frame(mytable[-1,c("Lab", "N")], Rate = prop)
          rest$Error <- with(rest, ifelse(
            is.na(N), "This number is required", ifelse(
              Rate > 100, "This number can't be greater than the total number of patients seen",
              paste0(format(round(Rate, 2), nsmall = 2),
                     "% vaccination rate in this age group"))))
          
          if(!is.na(rest$Rate[1]) & !is.na(rest$Rate[2]) & rest$Rate[2] > rest$Rate[1]) {
            condition <- 1
          } else {
            condition <- 0
          }
          rest$Error <- ifelse(rest$Lab == "Number of active patients who received both (2) HPV doses" &
                                 rest$Error != "This number is required" &
                                 condition == 1, "This number can't be greater than those patients getting 1+ doses",
                               rest$Error)
          
          final <- rbind(total, rest)
          final$Rate <- final$Error
          dplyr::select(final, Lab, N, Rate)
        }
      })
      output$tblFemAge2 <- renderRHandsontable({
        rhandsontable(change.FemAge2(), rowHeaders = NULL, stretchH = "all", height = 200) %>%
          hot_cols(colWidths = c(100, 25, 100),
                   manualColumnMove = FALSE,
                   manualColumnResize = TRUE
                   ##, wordWrap = "yes please"
          ) %>%
          hot_col("N", halign = "htCenter", renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }")  %>%
          hot_col("Rate", readOnly = T, renderer = color_renderer)
      })
      change.MenAge2 <- reactive({
        if (is.null(input$tblMenAge2)) {
          MenAge2()
        } else if (!identical(MenAge2(), input$tblMenAge2)) {
          mytable <- as.data.frame(hot_to_r(input$tblMenAge2))
          total <- as.numeric(mytable$N[1])
          rest <- as.numeric(mytable$N[-1])
          prop <- ((rest / total) * 100)
          
          total <- mytable[1,]
          total$Error <- ifelse(is.na(total$N), "This number is required", "")
          
          rest <- data.frame(mytable[-1,c("Lab", "N")], Rate = prop)
          rest$Error <- with(rest, ifelse(
            is.na(N), "This number is required", ifelse(
              Rate > 100, "This number can't be greater than the total number of patients seen",
              paste0(format(round(Rate, 2), nsmall = 2),
                     "% vaccination rate in this age group"))))
          if(!is.na(rest$Rate[1]) & !is.na(rest$Rate[2]) & rest$Rate[2] > rest$Rate[1]) {
            condition <- 1
          } else {
            condition <- 0
          }
          rest$Error <- ifelse(rest$Lab == "Number of active patients who received both (2) HPV doses" &
                                 rest$Error != "This number is required" &
                                 condition == 1, "This number can't be greater than those patients getting 1+ doses",
                               rest$Error)
          final <- rbind(total, rest)
          final$Rate <- final$Error
          dplyr::select(final, Lab, N, Rate)
        }
      })
      output$tblMenAge2 <- renderRHandsontable({
        rhandsontable(change.MenAge2(), rowHeaders = NULL, stretchH = "all", height = 200) %>%
          hot_cols(colWidths = c(100, 25, 100),
                   manualColumnMove = FALSE,
                   manualColumnResize = TRUE
                   ##, wordWrap = "yes please"
          ) %>%
          hot_col("N", halign = "htCenter", renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }")  %>%
          hot_col("Rate", readOnly = T, renderer = color_renderer)
      })
    }  # end boys and girls age 11-12
    
  
    # Boys and girls - ages 13
    if ("13" %in% input$Q28 &
        input$Q29 == "Yes, we'll report separate data for male and female patients") {
      
      output$brates_13 <- renderUI({
        splitLayout(
                 verticalLayout(
                 h4("Vaccination Rates for Female Patients, Age 13", align = "center"),
                 uiOutput("uiFemAge3")),
                 verticalLayout(
                 h4("Vaccination Rates for Male Patients, Age 13", align = "center"),
                 uiOutput("uiMenAge3")))
      })
      output$uiFemAge3 <- renderUI({
        rHandsontableOutput("tblFemAge3", width = "100%")
      })
      output$uiMenAge3 <- renderUI({
        rHandsontableOutput("tblMenAge3", width = "100%")
      })
      
      # Function to track changes
      change.FemAge3 <- reactive({
        if (is.null(input$tblFemAge3)) {
          FemAge3()
        } else if (!identical(FemAge3(), input$tblFemAge3)) {
          mytable <- as.data.frame(hot_to_r(input$tblFemAge3))
          total <- as.numeric(mytable$N[1])
          rest <- as.numeric(mytable$N[-1])
          prop <- ((rest / total) * 100)
          
          total <- mytable[1,]
          total$Error <- ifelse(is.na(total$N), "This number is required", "")
          
          rest <- data.frame(mytable[-1,c("Lab", "N")], Rate = prop)
          rest$Error <- with(rest, ifelse(
            is.na(N), "This number is required", ifelse(
              Rate > 100, "This number can't be greater than the total number of patients seen",
              paste0(format(round(Rate, 2), nsmall = 2),
                     "% vaccination rate in this age group"))))
          if(!is.na(rest$Rate[1]) & !is.na(rest$Rate[2]) & rest$Rate[2] > rest$Rate[1]) {
            condition <- 1
          } else {
            condition <- 0
          }
          rest$Error <- ifelse(rest$Lab == "Number of active patients who received both (2) HPV doses" &
                                 rest$Error != "This number is required" &
                                 condition == 1, "This number can't be greater than those patients getting 1+ doses",
                               rest$Error)
          final <- rbind(total, rest)
          final$Rate <- final$Error
          dplyr::select(final, Lab, N, Rate)
        }
      })
      output$tblFemAge3 <- renderRHandsontable({
        rhandsontable(change.FemAge3(), rowHeaders = NULL, stretchH = "all", height = 200) %>%
          hot_cols(colWidths = c(100, 25, 100),
                   manualColumnMove = FALSE,
                   manualColumnResize = TRUE
                   ##, wordWrap = "yes please"
          ) %>%
          hot_col("N", halign = "htCenter", renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }")  %>%
          hot_col("Rate", readOnly = T, renderer = color_renderer)
      })
      change.MenAge3 <- reactive({
        if (is.null(input$tblMenAge3)) {
          MenAge3()
        } else if (!identical(MenAge3(), input$tblMenAge3)) {
          mytable <- as.data.frame(hot_to_r(input$tblMenAge3))
          total <- as.numeric(mytable$N[1])
          rest <- as.numeric(mytable$N[-1])
          prop <- ((rest / total) * 100)
          
          total <- mytable[1,]
          total$Error <- ifelse(is.na(total$N), "This number is required", "")
          
          rest <- data.frame(mytable[-1,c("Lab", "N")], Rate = prop)
          rest$Error <- with(rest, ifelse(
            is.na(N), "This number is required", ifelse(
              Rate > 100, "This number can't be greater than the total number of patients seen",
              paste0(format(round(Rate, 2), nsmall = 2),
                     "% vaccination rate in this age group"))))
          if(!is.na(rest$Rate[1]) & !is.na(rest$Rate[2]) & rest$Rate[2] > rest$Rate[1]) {
            condition <- 1
          } else {
            condition <- 0
          }
          rest$Error <- ifelse(rest$Lab == "Number of active patients who received both (2) HPV doses" &
                                 rest$Error != "This number is required" &
                                 condition == 1, "This number can't be greater than those patients getting 1+ doses",
                               rest$Error)
          final <- rbind(total, rest)
          final$Rate <- final$Error
          dplyr::select(final, Lab, N, Rate)
        }
      })
      output$tblMenAge3 <- renderRHandsontable({
        rhandsontable(change.MenAge3(), rowHeaders = NULL, stretchH = "all", height = 200) %>%
          hot_cols(colWidths = c(100, 25, 100),
                   manualColumnMove = FALSE,
                   manualColumnResize = TRUE
                   ##, wordWrap = "yes please"
          ) %>%
          hot_col("N", halign = "htCenter", renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }") %>%
          hot_col("Rate", readOnly = T, renderer = color_renderer)
      })
    }  # end boys and girls age 113
    
    # Combined - ages 9-10
    if ("9-10" %in% input$Q28 &
        input$Q29 == "No, we will have combined rate data") {
      
      output$brates_9_10 <- renderUI({
                 verticalLayout(
                   h4("Vaccination Rates for Male and Female Patients, Ages 9-10", align = "center"),
                   uiOutput("uiBothAge1"))
      })
      output$uiBothAge1 <- renderUI({
        rHandsontableOutput("tblBothAge1", width = "100%")
      })
      # Function to track changes
      change.BothAge1 <- reactive({
        if (is.null(input$tblBothAge1)) {
          BothAge1()
        } else if (!identical(BothAge1(), input$tblBothAge1)) {
          mytable <- as.data.frame(hot_to_r(input$tblBothAge1))
          total <- as.numeric(mytable$N[1])
          rest <- as.numeric(mytable$N[-1])
          prop <- ((rest / total) * 100)
          
          total <- mytable[1,]
          total$Error <- ifelse(is.na(total$N), "This number is required", "")
          
          rest <- data.frame(mytable[-1,c("Lab", "N")], Rate = prop)
          rest$Error <- with(rest, ifelse(
            is.na(N), "This number is required", ifelse(
              Rate > 100, "This number can't be greater than the total number of patients seen",
              paste0(format(round(Rate, 2), nsmall = 2),
                     "% vaccination rate in this age group"))))
          if(!is.na(rest$Rate[1]) & !is.na(rest$Rate[2]) & rest$Rate[2] > rest$Rate[1]) {
            condition <- 1
          } else {
            condition <- 0
          }
          rest$Error <- ifelse(rest$Lab == "Number of active patients who received both (2) HPV doses" &
                                 rest$Error != "This number is required" &
                                 condition == 1, "This number can't be greater than those patients getting 1+ doses",
                               rest$Error)
          final <- rbind(total, rest)
          final$Rate <- final$Error
          dplyr::select(final, Lab, N, Rate)
        }
      })
      output$tblBothAge1 <- renderRHandsontable({
        rhandsontable(change.BothAge1(), rowHeaders = NULL, stretchH = "all", height = 200) %>%
          hot_cols(colWidths = c(100, 25, 100),
                   manualColumnMove = FALSE,
                   manualColumnResize = TRUE
                   ##, wordWrap = "yes please"
          ) %>%
          hot_col("N", halign = "htCenter", renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }") %>%
          hot_col("Rate", readOnly = T, renderer = color_renderer)
      })
      } # End combined ages 9-10
    
    
    if ("11-12" %in% input$Q28 &
        input$Q29 == "No, we will have combined rate data") {
      
      output$brates_11_12 <- renderUI({
        verticalLayout(
          h4("Vaccination Rates for Male and Female Patients, Ages 11-12", align = "center"),
          uiOutput("uiBothAge2"))
      })
      output$uiBothAge2 <- renderUI({
        rHandsontableOutput("tblBothAge2", width = "100%")
      })
      # Function to track changes
      change.BothAge2 <- reactive({
        if (is.null(input$tblBothAge2)) {
          BothAge2()
        } else if (!identical(BothAge2(), input$tblBothAge2)) {
          mytable <- as.data.frame(hot_to_r(input$tblBothAge2))
          total <- as.numeric(mytable$N[1])
          rest <- as.numeric(mytable$N[-1])
          prop <- ((rest / total) * 100)
          
          total <- mytable[1,]
          total$Error <- ifelse(is.na(total$N), "This number is required", "")
          
          rest <- data.frame(mytable[-1,c("Lab", "N")], Rate = prop)
          rest$Error <- with(rest, ifelse(
            is.na(N), "This number is required", ifelse(
              Rate > 100, "This number can't be greater than the total number of patients seen",
              paste0(format(round(Rate, 2), nsmall = 2),
                     "% vaccination rate in this age group"))))
          if(!is.na(rest$Rate[1]) & !is.na(rest$Rate[2]) & rest$Rate[2] > rest$Rate[1]) {
            condition <- 1
          } else {
            condition <- 0
          }
          rest$Error <- ifelse(rest$Lab == "Number of active patients who received both (2) HPV doses" &
                                 rest$Error != "This number is required" &
                                 condition == 1, "This number can't be greater than those patients getting 1+ doses",
                               rest$Error)
          final <- rbind(total, rest)
          final$Rate <- final$Error
          dplyr::select(final, Lab, N, Rate)
        }
      })
      output$tblBothAge2 <- renderRHandsontable({
        rhandsontable(change.BothAge2(), rowHeaders = NULL, stretchH = "all", height = 200) %>%
          hot_cols(colWidths = c(100, 25, 100),
                   manualColumnMove = FALSE,
                   manualColumnResize = TRUE
                   ##, wordWrap = "yes please"
          ) %>%
          hot_col("N", halign = "htCenter", renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }") %>%
          hot_col("Rate", readOnly = T, renderer = color_renderer)
      s})
    } # End combined ages 11-12
    
    if ("13" %in% input$Q28 &
        input$Q29 == "No, we will have combined rate data") {
      
      output$brates_13 <- renderUI({
        verticalLayout(
          h4("Vaccination Rates for Male and Female Patients, Age 13", align = "center"),
          uiOutput("uiBothAge3"))
      })
      output$uiBothAge3 <- renderUI({
        rHandsontableOutput("tblBothAge3", width = "100%")
      })
      # Function to track changes
      change.BothAge3 <- reactive({
        if (is.null(input$tblBothAge3)) {
          BothAge3()
        } else if (!identical(BothAge3(), input$tblBothAge3)) {
          mytable <- as.data.frame(hot_to_r(input$tblBothAge3))
          total <- as.numeric(mytable$N[1])
          rest <- as.numeric(mytable$N[-1])
          prop <- ((rest / total) * 100)
          
          total <- mytable[1,]
          total$Error <- ifelse(is.na(total$N), "This number is required", "")
          
          rest <- data.frame(mytable[-1,c("Lab", "N")], Rate = prop)
          rest$Error <- with(rest, ifelse(
            is.na(N), "This number is required", ifelse(
              Rate > 100, "This number can't be greater than the total number of patients seen",
              paste0(format(round(Rate, 2), nsmall = 2),
                     "% vaccination rate in this age group"))))
          if(!is.na(rest$Rate[1]) & !is.na(rest$Rate[2]) & rest$Rate[2] > rest$Rate[1]) {
            condition <- 1
          } else {
            condition <- 0
          }
          rest$Error <- ifelse(rest$Lab == "Number of active patients who received both (2) HPV doses" &
                                 rest$Error != "This number is required" &
                                 condition == 1, "This number can't be greater than those patients getting 1+ doses",
                               rest$Error)
          final <- rbind(total, rest)
          final$Rate <- final$Error
          dplyr::select(final, Lab, N, Rate)
        }
      })
      output$tblBothAge3 <- renderRHandsontable({
        rhandsontable(change.BothAge3(), rowHeaders = NULL, stretchH = "all", height = 200) %>%
          hot_cols(colWidths = c(100, 25, 100),
                   manualColumnMove = FALSE,
                   manualColumnResize = TRUE
                   ##, wordWrap = "yes please"
          ) %>%
          hot_col("N", halign = "htCenter", renderer = "function(instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               td.style.background = 'lightblue';
               }") %>%
          hot_col("Rate", renderer = color_renderer)
      })
    } # End combined ages 13
    })


  
}     


# Run the application 
shinyApp(ui = ui, server = server)
