### Shiny app
library(shiny)
library(dplyr)
library(shinythemes)
library(DT)
library(shinydashboard)
library(rsconnect)
library(AzureStor)
library(stringr)
library(ggplot2)
library(gridExtra)
library(rdrop2)
library(shinyjs)
library(shinyalert)
library(RSQLite)
library(rhandsontable)
library(dbplyr)
library(grid)
rm(list=ls())


# Production URLs
logout <- "https://interventionsandimplementation.shinyapps.io/HPV_Monitoring/__logout__/"

# Source files for my functions
source("Functions.R")



# Ui ----------------------------------------------------------------------

ui = dashboardPage(
  title = "HPV Vaccination Systems and Strategies Inventory 2021",
  header = dashboardHeader(title = textOutput("username")),
  
  
  # Sidebar layout ----------------------------------------------------------
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      type = "hidden",
      menuItem("Instructions", tabName = "instructions", icon = icon("home")),
      menuItem(
        "Baseline Submission",
        tabName = "baseline",
        icon = icon("hourglass-start")
      ),
      menuItem("Final Submission", tabName = "finalupdate", icon = icon("hourglass-end")),
      menuItem("Log out",
               href = logout,
               icon=icon("user-cog"))
    ),
    tags$head(tags$style(".mybutton{background-color:black;} .skin-black .sidebar .mybutton{color: white;}"))
  ),
  
  body = dashboardBody(
    shinyjs::useShinyjs(),
    # need this so we can do the session$reload() thing
    shinyalert::useShinyalert(),
    # need this for those lovely pop-ups
    
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
    tabItems(
      
      # Instructions page  -------------------------------------------
      
      
      tabItem(
        "instructions",
        div(
          imageOutput(
            'HPVlogo',
            inline = TRUE,
            height = '75px',
            width = '300px'
          ),
          h1("HPV Vaccination Systems and Strategies Inventory 2020", align =
               "center"),
        ),
        # end box with logos and title
        HTML("<br><br>"),
        h3("Instructions and Dates", align = "center"),
        HTML("<br>"),
        helpText(
          "The Systems and Strategies Inventory (SASI) is designed to be completed by American Cancer Society (ACS) staff and the health
                                         system partner quality improvement team for each year of HPV vaccination partnership. This tool will guide you as you assemble
                                         a team, make a plan based on your data and previous activities, engage your system, track your progress, and measure its impact."
        ),
        HTML("<br>"),
        helpText("The Inventory contains ", strong("two sections:")),
        tags$ol(
          tags$li(strong("Baseline: "), "due March 31, 2020"),
          tags$li(strong("Final Project Update: "), "due March 31, 2021")
        ),
        HTML("<br>"),
        helpText(
          "If your HPV vaccination project with ACS continues for more than one year, ",
          strong("you will complete a new inventory for each project year.")
        ),
        HTML("<br>", "<br>"),
        
        
        splitLayout(
          box(width = '12',
              align = 'center',
              verticalLayout(
                h4(strong("Baseline Data Entry")),
                helpText("Complete to start your project."),
                actionButton("click_baseline", "Baseline Health System Information",
                             class = "btn-inverse btn-lg"),
                HTML("<br>"),
                h4(strong("Final Data Entry")),
                helpText("Complete this section ", strong("after "), "your project is completed"),
                actionButton("click_finalupdate", "Final Project Update", class = "btn-inverse btn-lg")
              )),
          
          
          box(width = '12',
              align = 'center',
              h4(strong("Data Download")),
              verticalLayout(
                downloadLink("download_final", "Download Final Data"),
                #actionButton("download_final", "Download Final Data", class =
                #               "btn-inverse btn-lg"),
                helpText("Click here to download all final submitted data."),
                HTML("<br>")
              )
          )),
        
      ),    # end instructions page
      
      
      # Baseline information page -----------------------------------------------
      
      tabItem(
        "baseline",
        verticalLayout(splitLayout(
          h1("Baseline Information", align = "center")
          #valueBox(textOutput("uniqueID"), "Health System ID", icon=icon("building")) # Display unique health system identifier once the necessary information has been input
        )),
        
        
        # Demographic section -----------------------------------------------------
        
        tabBox(
          width = NULL,
          id = "tabbox_baseline",
          
          tabPanel(
            h4("Demographics"),
            value = "p1_demo",
            h3("Demographics"),
            helpText(
              "Please select the account name associated with each health system to review their data"
            ),
            uiOutput("SelectDemoUser"),
            uiOutput("date_demo"),
            HTML("<br>"),
            verticalLayout(
              # --- Lines 1-2 of the pdf ---- #
              uiOutput("HealthSystem"),
              uiOutput("Q1_DBA"),
              splitLayout(
                uiOutput("Q2"), uiOutput("Q2_other")
              ),
              uiOutput("Q3"),
              textOutput("error_Q3"),
              HTML("<br>"),
              # ---- Line 3 in the pdf- address info  ----- #
              splitLayout(
                uiOutput("Q4a"), uiOutput("Q4b"), uiOutput("Q4c")
              ),
              HTML("<br>"),
              splitLayout(
                uiOutput("Q5"), uiOutput("Q5_Email")
              ),
              uiOutput("Q6"),
              uiOutput("Q7"),
              uiOutput("Q8"),
              uiOutput("Q8_Email"),
              uiOutput("Q9"),
              fluidRow(
                column(width = 6,
                       uiOutput("Q10"),),
                column(
                  width = 4,
                  offset = 1,
                  uiOutput("Q10_other"),
                  uiOutput("error_Q10_other"),
                  uiOutput("error_Q10")
                  
                )
              ),
              verticalLayout(
                uiOutput("Q11"),
                uiOutput("Q11_other")
              ),
              verticalLayout(
                uiOutput("Q12"),
                uiOutput("Q12_other")
              ),
              uiOutput("Q13"),
              splitLayout(
                uiOutput("Q13_amount"), uiOutput("Q13_source"),  uiOutput("Q13_date"),  uiOutput("Q13_length")
              ),
              splitLayout(
                textOutput("error_Q13_amount"), textOutput("error_Q13_source"), textOutput("error_Q13_date"), textOutput("error_Q13_length")
              ),
              splitLayout(uiOutput("Q14"),
                          uiOutput("Q14_other")),
              HTML("<br>"),
              
              p("Please hit the save button below in order to save your responses and have them repopulate should you exit from this browser and return at a later date. Otherwise, your entered responses will disappear when you exit this browser.", style = "color:green"),
              
              actionButton("button_demo", "Save and finish later", class =
                             "btn-primary")
            ) #end vertical layout
            
          ),
          # end demographics tab panel,
          
          
          
          # Systems background page -------------------------------------------------------
          
          
          tabPanel(
            h4("System Background"),
            value = "p1_data",
            h3("System Background"),
            helpText("Please select the username associated with a health system to review the data."),
            uiOutput("SelectSystemsUser"),
            uiOutput("date_systems"),
            HTML("<br>"),
            verticalLayout(
              uiOutput("Q15"),
              uiOutput("Q15_other"),
              uiOutput("Q15_EHRversion"), 
              uiOutput("Q16"),
              HTML("<br>"),
              uiOutput("Q17"),
              HTML("<br>"),
              uiOutput("Q18"),
              uiOutput("Q19"),
              HTML("<br>"),
              uiOutput("Q20"),
              uiOutput("Q20_notes", inline = TRUE),
              uiOutput("Q20_orders"),
              uiOutput("Q20_other"),
              uiOutput("Q21"),
              uiOutput("Q21_text"),
              uiOutput("Q22"),
              uiOutput("Q22_notes"),
              uiOutput("Q23"),
              uiOutput("Q23_notes", inline = TRUE),
              uiOutput("Q24"),
              uiOutput("Q24_years", inline = TRUE),
              uiOutput("Q25"),
              uiOutput("Q25_other"),
              HTML("<br>"),
              uiOutput("Q26"),
              uiOutput("Q26_other"),
              HTML("<br>"),
              p("Please hit the save button below in order to save your responses and have them repopulate should you exit from this browser and return at a later date. Otherwise, your entered responses will disappear when you exit this browser.", style = "color:green"),
              actionButton("button_systems", "Save and finish later", class = "btn-primary"),
              HTML("<br>"),
              uiOutput("ReviewData"),
              uiOutput("saveBaseline"),
              uiOutput("saveError")
            )
          ),
          
          
          # Vaccination rates tab ---------------------------------------------------
          
          # TEXT STUFF
          tabPanel(
            h4("Vaccination Rates"),
            value = "p1_rates",
            h3("Vaccination Rates"),
            helpText("Please select the username associated with a health system to review the data."),
            uiOutput("SelectRatesUser"),
            uiOutput("date_brates"),
            HTML("<br>"),
            h4("INSTRUCTIONS:"),
            helpText("This section collects baseline vaccination rates for your health system. Vaccination rates 
                     should be calculated for active medical patients ages 9-13 at participating clinic sites who were up-to-date 
                     with HPV, Tdap, and Meningococcal vaccines in 2020. The definitions and tips listed below will help you
                      calculate your vaccination rates. Data entry tables will appear after you answer the prompts to the questions
                      below."),
            HTML("<br>"),
            h4("DEFINITIONS:"),
            tags$ul(
              tags$li(
                em("HPV vaccine initiation"), "number should include patients who have ",
                strong("ever"), " received 1 dose of the HPV vaccine. (This number will include patients who received both their 1st and 2nd dose.)"
              ),
              tags$li(
                em("HPV vaccination series completion "), "includes patients who have received 2 doses of HPV vaccine separated by at least 5 months"
              ),
              tags$li(
                em("Active medical patients"),
                " are defined as those who were seen at least once during ",
                strong("the time period you specify below"),
                " (12 months, 18 months, 24 months). Medical visits do not include dental or other non-medical visits. Medical visits",
                em(" do "), "include well-child visits and sick visits. Please note that a longer time period allows more patients to be 
                included in the rate calculation to account for the infrequenty medical viits for patients ages 9-13."
              ),
              tags$li(
                "The ", em("reporting period "), "is used to define patient age to determine which active patients were, for example,
                 age 13 during the baseline year. The reporting period for baseline is January 1, 2020 to December 31, 2020. Patients included in the age 13 group turned 13 during the reporting period"
              ),
              tags$li(
                em("Up-to-date "), "is defined as active medical patients in the relevant age and sex categories who have ",
                strong("ever "), "received the specified vaccine (or dose). They may have received the vaccine prior to the reporting period
                and should still be counted. This means that the date of service is not relevant to the calculation of vaccination rates for this project.
                Use the following table to identify ", 
                strong("up-to-date "), "patients in 2020."
              )
            ),
            
            HTML("<br>"),
            tableOutput("static_table"),
            
            h4("TIPS ON DATA QUALITY:"),
            helpText(p("You will see ", strong("red notes ", style="color:red"), "appear if entered data includes the following data quality flags:")),
            tags$ul(
              tags$li(
                "The number of patients receiving a vaccination exceeds the number of active patients"
              ),
              tags$li(
                "Completion rates exceed initiation rates (every child that completed the series must have been initiated)"
              )
            ),
            helpText("Other quality tips you will not be notified for but you should pay attention to include:"),
            tags$ul(
              tags$li(
                "Be sure to enter data for the correct age into each table"
              ),
              tags$li(
                "The active patient population should not be the same for different age groups or sex (this would be highly unlikely)"
              ),
              tags$li(
                "Do any of the numbers seem really high or really low? Is the number of patients who received one or two doses for HPV a lot higher than Tdap and Meningococcal?
                Are more patients age 9-10 vaccinated than those age 13? These questions could highlight potential issues in data quality."
              )
            ),
            HTML("<br>"),
            h4("CUSTOMIZING YOUR VACCINATION RATE TABLES:"),
            helpText("We strongly encourage you to report vaccination rates separated by sex and specific age groupings (9-10, 11-12, and 13).
                      We recognize this may not always be possible. If you can only report on specific ages or data for males and females combined, use the following
                      menu to specify which data entry tables are visible."
            ),
            HTML("<br>"),
            
            # These go AFTER the instructions but before the rates
            splitLayout(
              uiOutput("Q27"), uiOutput("Q27_other")
            ),
            splitLayout(
              uiOutput("Q28"), uiOutput("Q28_other")
            ),
            uiOutput("Q29"),
            HTML("<br>"),
            
            # Rates tables
            uiOutput("brates_9_10"),
            uiOutput("brates_11_12"),
            uiOutput("brates_13"),
            
            verticalLayout(
              HTML("<br><br>"),
              
              box(
                width = '100%',
                span(
                  uiOutput("Q30"),
                ),
                textOutput("error_rates"),
                uiOutput("Q31"),
                uiOutput("Q31_other"),
                uiOutput("Q32"),
                splitLayout(uiOutput("Q32_details"),
                            uiOutput("Q32_other"))
              ),
              
              fluidRow(
                column(6,  uiOutput("Q33")),
                column(6, uiOutput("Q33_other"))),
              uiOutput("Q34")
            ), # End vertical arrangement
            
            HTML("<br>"),
            p("Please hit the save button below in order to save your responses and have them repopulate should you exit from this browser and return at a later date. Otherwise, your entered responses will disappear when you exit this browser.", style = "color:green"),
            
            actionButton("button_rates", "Save and finish later", class =
                           "btn-primary")
          ),
          # end rates tabbox
          
          
          # Project activity plan ---------------------------------------------------
          
          tabPanel(
            h4("Project Activity Plan"),
            value = "p1_plan",
            helpText("Please select the username associated with a health system to review the data."),
            uiOutput("SelectActivitiesUser"),
            uiOutput("date_activity"),
            HTML("<br>"),
            verticalLayout(
              h4(strong("Goal Setting"), align = "center"),
              fluidRow(
                column(8,
                       uiOutput("Q35")),
                column(4,
                       uiOutput("Q35_other"))
              ),
              HTML("<br>"),
              box(
                width = 12,
                uiOutput("Q36a"),
                uiOutput("Q36b"),
                uiOutput("Q36c"),
                uiOutput("Q36d"),
                uiOutput("Q36e"),
                HTML("<br>"),
                box(
                  title = h4(
                    "36f.  Now, combine the above into a single ",
                    strong("Aim"),
                    " statement:"
                  ),
                  solidHeader = TRUE,
                  width = 12,
                  status = "success",
                  uiOutput("Q36f")
                ) # end this mini box
              ),  #end the other box
              HTML("<br>"),
              uiOutput("Q37"),
              uiOutput("Q38"),
              uiOutput("Q38_other"),
              uiOutput("Q39"),
              uiOutput("Q39_other"),
              uiOutput("Q40"),
              uiOutput("Q41"),
              uiOutput("Q42"),
              
              HTML("<br>"),
              h5(
                strong(
                  "18.  What specific actions will we take to implement the above interventions and trainings to meet our goal?"
                )
              ),
              helpText(em("This question can be used as a QI action plan")),
              HTML("<br>"),
              box(
                width = 12,
                splitLayout(
                  uiOutput("act1"), uiOutput("time1"), uiOutput("ppl1")
                ),
                splitLayout(
                  uiOutput("act2"), uiOutput("time2"), uiOutput("ppl2")
                ),
                splitLayout(
                  uiOutput("act3"), uiOutput("time3"), uiOutput("ppl3")
                ),
                splitLayout(
                  uiOutput("act4"), uiOutput("time4"), uiOutput("ppl4")
                ),
                splitLayout(
                  uiOutput("act5"), uiOutput("time5"), uiOutput("ppl5")
                ),
                splitLayout(
                  uiOutput("act6"), uiOutput("time6"), uiOutput("ppl6")
                ),
                splitLayout(
                  uiOutput("act7"), uiOutput("time7"), uiOutput("ppl7")
                ),
                splitLayout(
                  uiOutput("act8"), uiOutput("time8"), uiOutput("ppl8")
                ),
                splitLayout(
                  uiOutput("act9"), uiOutput("time9"), uiOutput("ppl9")
                ),
                splitLayout(
                  uiOutput("act10"), uiOutput("time10"), uiOutput("ppl10")
                )
              ),# end box
              HTML("<br>"),
              p("Please hit the save button below in order to save your responses and have them repopulate should you exit from this browser and return at a later date. Otherwise, your entered responses will disappear when you exit this browser.", style = "color:green"),
              
              actionButton("button_activities", "Save and finish later", class =
                             "btn-primary")
              
            ) # end vertical layout
            
          ) # end activity plan tabitem
           #end baseline submission tab
        ) # end baseline tabbox
      ), # end baseline's page (with all the nested tabboxes)
      
      
      # Final Rates section ----------------------------------------------------
      
      
      # Note:  They have several "Progress Updates" that are otherwise identical in the page.
      # As I understand, this project only has the single update, which is why that's all that's programmed here
      # We can expand if necessary.
      
      tabItem(
        "finalupdate",
        h1("Final Project Update", align = "center"),
        
        
        
        # All text stuff
        tabBox(
          width = NULL,
          id = "tabbox_final",
          tabPanel(
            h4("Updated Rates"),
            value = "p2_rates",
            h3("Final Project Updates"),
            HTML("<br>"),
            helpText("Please select the username associated with a health system to review the data."),
            uiOutput("SelectFURatesUser"),
            uiOutput("date_fwup_rates"),
            HTML("<br>"),
            h4("INSTRUCTIONS:"),
            helpText("This section collects final vaccination rates for your health system. Vaccination rates 
                     should be calculated for active medical patients ages 9-13 at participating clinic sites who were up-to-date 
                     with HPV, Tdap, and Meningococcal vaccines in 2021. The definitions and tips listed below will help you
                      calculate your vaccination rates. Data entry tables will appear after you answer the prompts to the questions
                      below."),
            HTML("<br>"),
            h4("DEFINITIONS:"),
            tags$ul(
              tags$li(
                em("HPV vaccine initiation"), "number should include patients who have ",
                strong("ever"), " received 1 dose of the HPV vaccine. (This number will include patients who received both their 1st and 2nd dose.)"
              ),
              tags$li(
                em("HPV vaccination series completion "), "includes patients who have received 2 doses of HPV vaccine separated by at least 5 months"
              ),
              tags$li(
                em("Active medical patients"),
                " are defined as those who were seen at least once during ",
                strong("the time period you specify below"),
                " (12 months, 18 months, 24 months). Medical visits do not include dental or other non-medical visits. Medical visits",
                em(" do "), "include well-child visits and sick visits. Please note that a longer time period allows more patients to be 
                included in the rate calculation to account for the infrequenty medical viits for patients ages 9-13."
              ),
              tags$li(
                "The ", em("reporting period "), "is used to define patient age to determine which active patients were, for example,
                 age 13 during the baseline year. The reporting period for the final update is January 1, 2021 to December 31, 2021. Patients included in the age 13 group turned 13 during the reporting period"
              ),
              tags$li(
                em("Up-to-date "), "is defined as active medical patients in the relevant age and sex categories who have ",
                strong("ever "), "received the specified vaccine (or dose). They may have received the vaccine prior to the reporting period
                and should still be counted. This means that the date of service is not relevant to the calculation of vaccination rates for this project.
                Use the following table to identify ", 
                strong("up-to-date "), "patients in 2021."
              )
            ),
            
            HTML("<br>"),
            tableOutput("static_table_fwup"),
            
            h4("TIPS ON DATA QUALITY:"),
            helpText(p("You will see ", strong("red notes ", style="color:red"), "appear if entered data includes the following data quality flags:")),
            tags$ul(
              tags$li(
                "The number of patients receiving a vaccination exceeds the number of active patients"
              ),
              tags$li(
                "Completion rates exceed initiation rates (every child that completed the series must have been initiated)"
              )
            ),
            helpText("Other quality tips you will not be notified for but you should pay attention to include:"),
            tags$ul(
              tags$li(
                "Be sure to enter data for the correct age into each table"
              ),
              tags$li(
                "The active patient population should not be the same for different age groups or sex (this would be highly unlikely)"
              ),
              tags$li(
                "Do any of the numbers seem really high or really low? Is the number of patients who received one or two doses for HPV a lot higher than Tdap and Meningococcal?
                Are more patients age 9-10 vaccinated than those age 13? These questions could highlight potential issues in data quality."
              )
            ),
            HTML("<br>"),
            h4("CUSTOMIZING YOUR VACCINATION RATE TABLES:"),
            helpText("We strongly encourage you to report vaccination rates separated by sex and specific age groupings (9-10, 11-12, and 13).
                      We recognize this may not always be possible. If you can only report on specific ages or data for males and females combined, use the following
                      menu to specify which data entry tables are visible."
            ),
            HTML("<br>"),
            # These go AFTER the instructions but before the rates
            splitLayout(
              uiOutput("Q1FU"), uiOutput("Q1FU_other")
            ),
            splitLayout(
              uiOutput("Q2FU"), uiOutput("Q2FU_other")
            ),
            uiOutput("Q3FU"),
            HTML("<br>"),
            
            uiOutput("furates_9_10"),
            uiOutput("furates_11_12"),
            uiOutput("furates_13"),
            
            # ages 9-10
            verticalLayout(
              HTML("<br>"),
              HTML("<br>"),
              uiOutput("Q4FU"),
              HTML("<br>"),
              p("Please hit the save button below in order to save your responses and have them repopulate should you exit from this browser and return at a later date. Otherwise, your entered responses will disappear when you exit this browser.", style = "color:green"),
              
              actionButton("button_fwup_rates", "Save and finish later", class =
                             "btn-primary"),
              textOutput("error_rates_fwup")
            )),
          # end tab on rates
          
       # Additional information page ---------------------------------------------
          
          tabPanel(
            h4("Additional info"),
            value = "p2_additional",
            h3("Additional info"),
            HTML("<br>"),
            helpText("Please select the username associated with a health system to review the data."),
            uiOutput("SelectAddInfoUser"),
            uiOutput("date_fwup_additional"),
            HTML("<br>"),
            verticalLayout(
              uiOutput("Q5FU"),
              uiOutput("Q5FU_text"),
              uiOutput("Q6FU"),
              uiOutput("Q6FU_text"),
              HTML("<br>"),
              uiOutput("Q7FU"),
              uiOutput("Q7FU_other"),
              uiOutput("Q8FU"),
              uiOutput("Q8FU_other"),
              uiOutput("Q9FU"),
              uiOutput("Q9FU_other"),
              HTML("<br>"),
              verticalLayout(
                uiOutput("Q10FU"),
                uiOutput("Q11FU"),
                uiOutput("Q12FU")
              ),
              uiOutput("Q13FU"),
              uiOutput("Q13FU_other"),
              uiOutput("Q14FU"),
              uiOutput("Q14FU_other"),
              uiOutput("Q15FU"),
              uiOutput("Q16FU"),
              uiOutput("Q17FU"),
              uiOutput("Q18FU"),
              uiOutput("Q18FU_more")
            ),
            HTML("<br>"),
            p("Please hit the save button below in order to save your responses and have them repopulate should you exit from this browser and return at a later date. Otherwise, your entered responses will disappear when you exit this browser.", style = "color:green"),
            
            actionButton("save_followup", "Save and finish later", class =
                           "btn-primary")
            
          ) # end tab on other
        )# end tabbox
        
        
        
      )
      # end final update tab
      
      
      
    ) #end all tabItems listing
    
  ),# end dashboard Body
  skin = "black"
) # End UI section





# Server ------------------------------------------------------------------



server <- function(input, output, session) {
  
  
  # File paths to Azure: ----------------------------------------------------
  
  
  # Pull my inputData
  inputData <- dbConnect(SQLite(), "inputData.DB")
  

  # Define the endpoint and container
  # One primary containers: DataSrc
  endpoint <- storage_endpoint(
    dbReadTable(inputData, "azureDat")$myFileshare,
    dbReadTable(inputData, "azureDat")$mykey
  )
  DataSrc <- storage_container(endpoint, "hpv/DataSrc")
  
  # list of files on the server that I need to choose from
  files <- list_azure_files(DataSrc) %>%
    filter(isdir == "FALSE") %>%
    filter(name != ".DB")
  
  lapply(files$name, function(x) {
    storage_download(DataSrc, x, overwrite = T)
  })
  
  # Merge the data
  tbls <- c("demographics", "systems", "savedRates", "savedActivities", "followupRates", "additionalInfo")
  myData <- lapply(tbls, function(x) {
    foo <- lapply(files$name, function(y) {
      conn <- dbConnect(SQLite(), y)
      df <- dbReadTable(conn, x)
      dateVar <- names(df)[2]
      df <- df[order(df[[dateVar]], decreasing = T),]
      df[[dateVar]] <- as.POSIXct(df[[dateVar]], origin = "1970-01-01")
      dbDisconnect(conn)
      return(df[1,])
    }) %>%
      do.call("rbind",.)
    return(foo)
  })
  names(myData) <- tbls
  
  # Clean up shinyapps.io
  lapply(files$name, file.remove)
  rm(tbls)
  
  mergedData <- Reduce(function(x,y) full_join(x,y, "Username"), myData)
  
  
  
  # Download buttons-----------------------------------------------
  
  
  # I will need to update the functions here
  output$download_final <- downloadHandler(
    filename = function() {
      paste0("HPV Survey Final Data - ", Sys.Date(),".CSV")
    },
    content = function(con) {
      write.csv(mergedData, con)
    }
  )
  
  
  
  
  # Home page ---------------------------------------------------------------
  
  # Get the mergeID
  output$username <- renderText({
    session$user
  })
  
  # Display the logos
  output$HPVlogo <- renderImage({
    list(src = "Logos/ACS-Mission HPV_RGB.png",
         width = 300,
         alt = "ACS Interventions & Implementations team")
  }, deleteFile = FALSE)
  
  
  # When click the buttons on the first page, get taken to the relevant tabs
  observeEvent(input$click_baseline, {
    updateTabsetPanel(session, "tabs", selected = "baseline")
  })
  observeEvent(input$click_finalupdate, {
    updateTabsetPanel(session, "tabs", selected = "finalupdate")
  })
  observeEvent(input$click_dashboard, {
    updateTabsetPanel(session, "tabs", selected = "dashboard")
  })
  observeEvent(input$click_fuDashboard, {
    updateTabsetPanel(session, "tabs", selected = "fuDashboard")
  })
  
  # ------------------------------- Baseline information section ---------------------------------------- #
  # Demographics ------------------------------------------------------------
  
  
  demographics <- myData$demographics
  
  
  # SelectDemouser() selelcts those files - I want to input it into the data subset
  output$SelectDemoUser <- renderUI({
    selectInput(inputId = "SelectDemoUser",
                label = "Select health system account",
                choices = demographics$Username,
                selected = demographics$Username[1])
  })
  
  
  
  observeEvent(input$SelectDemoUser, {
    validate(need(input$SelectDemoUser, ""))
    demographics <- filter(myData$demographics, Username == input$SelectDemoUser)
    
    output$date_demo <- renderUI({
      h4(paste("This health system's data was most recently updated on ", (demographics$demographicsDate)))
    })
    
  
  foo11 <- c(demographics$Q11_1, demographics$Q11_2, demographics$Q11_3, demographics$Q11_4, demographics$Q11_5)
  foo11 <- foo11[!is.na(foo11)]
  
  foo12 <- with(demographics, c(Q12_1, Q12_2, Q12_3, Q12_4,  Q12_5, Q12_6))
  foo12 <- foo12[!is.na(foo12)]  
  
  foo14 <- with(demographics, c(Q14_1, Q14_2, Q14_3))
  foo14 <- foo14[!is.na(foo14)]
  
  
  output$HealthSystem <- renderUI({
    span(
      textInput(
        inputId = "HealthSystem",
        label = "1.  System Name  (Please write out your full system name, no abbreviations)",
        value = demographics$HealthSystem,
        width =  '100%',
      ),
      style = "color:red"
    )
  })
  output$Q1_DBA <- renderUI({
    textInput(
      inputId = "Q1_DBA",
      label = "1a. Doing Business as (DBA)",
      value = demographics$Q1_DBA,
      width = '100%')
  })
  output$Q2 <- renderUI({
    selectInput(
      inputId = "Q2",
      label = "2.  Health System type",
      choices = c("", "FQHC", "IDS", "Other (specify)"),
      width = '100%',
      selected <- demographics$Q2,
      selectize = FALSE)
  })
  output$Q2_other <- renderUI({
    validate(need(input$Q2, ""))
    if ("Other (specify)" %in% input$Q2) {
      textInput("Q2_other",
                label = "Other:",
                value = demographics$Q2_other,
                width = "100%")
    }
  })
  output$Q3 <- renderUI({
    numericInput(
      inputId = "Q3",
      label = "3.  Total number of clinic sites in system",
      value = demographics$Q3,
      min = 0,
      max = 100,
      step = 1,
      width = '100%'
    )
  })
  output$Q4a <- renderUI({
    textInput(
      inputId = "Q4a",
      label = "4a. City",
      value = demographics$Q4a,
      placeholder = c("ex: Atlanta")
    )
  })
  output$Q4b <- renderUI({
    selectInput(
      inputId = "Q4b",
      label = "4b. State",
      choices = c(
        "","AL","AK","AZ","AR","CA","CO","CT","DE",
        "FL","GA","HI", "ID","IL","IN","IA","KS","KY",
        "LA","ME","MD","MA","MI","MN","MS","MO","MT",
        "NE","NV","NH","NJ","NM","NY","NC","ND","OH",
        "OK","OR","PA","RI","SC","SD","TN","TX","UT",
        "VT","VA","WA","WV","WI", "WY"),
      selected = demographics$Q4b,
      selectize = F
    )
  })
  output$Q4c <- renderUI({
    textInput(
      inputId  = "Q4c",
      label = "4c. Zip code",
      value = demographics$Q4c,
      placeholder = "ex: 90210"
    )
  })   
  output$Q5 <- renderUI({
    textInput(
      inputId = "Q5",
      label = "5.  Name and title of Health System project lead",
      value = demographics$Q5,
      width = '100%'
    )
  })
  output$Q5_Email <- renderUI({
    validate(need(input$Q5, ""))
    span(
      textInput(
        inputId = "Q5_Email",
        label = "Health System project lead e-mail address",
        value = demographics$Q5_Email,
        width = "100%",
        placeholder = "name@domain.com"
      ),
      style = "color:red"
    )
  })
  output$Q6 <- renderUI({
    textInput(
      inputId = "Q6",
      label = "6.  Name and title of Health System QI lead",
      value = demographics$Q6,
      width = '100%'
    )
  })
  output$Q7 <- renderUI({
    textInput(
      inputId = "Q7",
      label = "7.  Name and title of project clinical champion",
      value = demographics$Q7,
      width = '100%'
    )
  })
  output$Q8 <- renderUI({
    selectInput(
      inputId = "Q8",
      label = "8. Select or enter the name of an ACS lead",
      choices = c("", dbReadTable(inputData, "roster")$Name),
      selected = demographics$Q8,
      selectize = F,
      width = "100%"
    )
  })
  
  
  output$Q8_Email <- renderUI({
    validate(need(input$Q8, ""))
    # Read in the staff roster data
    roster <- dbReadTable(inputData, "roster") %>%
      filter(Name == input$Q8)
    span(
      textInput(
        inputId = "Q8_Email",
        label = "8a. ACS Lead Email address",
        width = "100%",
        value = roster$Email,
      ),
      style = "color:red"
    )
  })
  output$Q9 <- renderUI({
    textInput(
      inputId = "Q9",
      label = "9.  Other ACS staff involved in project",
      value = demographics$Q9,
      width = '100%'
    )
  })
  output$Q10 <- renderUI({
    selectInput(
      inputId = "Q10",
      label = "10.  Are all clinic sites in the system participating in this project?",
      choices = c("", "No", "Yes"),
      selected = demographics$Q10,
      selectize = T
    )
  })
  output$Q10_other <- renderUI({
    validate(need(input$Q10, ""))
    if (input$Q10 == "No") {
      numericInput(
        inputId = "Q10_other",
        label = "10a. How many clinic sites are participating in this project?",
        min = 0,
        max = 100,
        value = demographics$Q10_other,
        step = 1
      )
    }
  })
  
  # Q11 - checkbox group
  output$Q11 <- renderUI({
    checkboxGroupInput(
      inputId = "Q11",
      label = p("11.  Which service lines are participating in this HPV vaccination project? ",
                em("(check all that apply)")
      ),
      choices = c(
        "Pediatric",
        "Internal Medicine",
        "Family Medicine",
        "Dental",
        "Other (specify)"
      ),
      selected = foo11,
      inline =  TRUE
    )
  })
  output$Q11_other <- renderUI({
    validate(need(input$Q11, ""))
    if ("Other (specify)" %in% input$Q11) {
      textInput("Q11_other",
                "Please specify",
                value = demographics$Q11_other,
                width = "100%")
    }
  })
  
  # Q12 - checkbox group
  output$Q12 <- renderUI({
    checkboxGroupInput(
      inputId = "Q12",
      label = p("12.  Are any of the following types of clinics participating in this HPV vaccination project? ",
                em("(check all that apply)")
      ),
      choices = c(
        "Urban clinics",
        "Suburban clinics",
        "Rural clinics",
        "School-based clinics",
        "Mobile clinics",
        "Other (specify)"
      ),
      selected =  foo12,
      inline =  TRUE)
  })
  output$Q12_other <- renderUI({
    validate(need(input$Q12, ""))
    if ("Other (specify)" %in% input$Q12) {
      textInput(inputId = "Q12_other", 
                label = "Please specify", 
                value = demographics$Q12_other)
    }
  })
  output$Q13 <- renderUI({
    selectInput(
      inputId = "Q13",
      label = "13.  Does this project have any project-specific funding?",
      choices = c("", "No", "Yes"),
      selected = demographics$Q13,
      selectize = T)
  })
  output$Q13_amount <- renderUI({
    validate(need(input$Q13, ""))
    if (input$Q13 != "Yes")
      return("")
    else{
      textInput("Q13_amount",
                "13a. Amount",
                value = demographics$Q13_amount,
                width = "100%")
    }
  })
  output$Q13_source <- renderUI({
    validate(need(input$Q13, ""))
    if (input$Q13 != "Yes")
      return("")
    else{
      textInput(
        "Q13_source",
        "13b. Please describe the funding source",
        value = demographics$Q13_source,
        width = "100%"
      )
    }
  })
  output$Q13_date <- renderUI({
    validate(need(input$Q13, ""))
    if (input$Q13 != "Yes")
      return()
    else{
      dateInput(
        "Q13_date",
        "13c. When did you first receive this funding?",
        width = "100%",
        format = "mm/dd/yy",
        value = as.Date(demographics$Q13_date, origin = "1970-01-01")
      )
    }
  })
  output$Q13_length <- renderUI({
    validate(need(input$Q13, ""))
    if (input$Q13 != "Yes")
      return("")
    else{
      numericInput(
        "Q13_length",
        "13d. For how many months was this funding?",
        value = demographics$Q13_length,
        min = 0,
        max = 36,
        step = 1,
        width = "100%"
      )
    }
  })
  
  # Q14 - checkbox group
  output$Q14 <- renderUI({
    checkboxGroupInput(
      inputId = "Q14",
      label = p("14.  Is this project part of a", em("(check all that apply)")),
      choices = c("ECHO", "Learning collaborative", "Other (specify)"),
      selected = foo14,
      inline = TRUE
    )
  })
  output$Q14_other <- renderUI({
    validate(need(input$Q14, ""))
    if (input$Q14 != "Other (specify)")
      return("")
    else{
      textInput(inputId = "Q14_other",
                label = "Please specify",
                value = demographics$Q14_other)
    }
  })
  
  
  
  # Demographics - error sections
  # These sections will work regardless of the previous entered data
  # So they exist outside the control structure.
  observe({
    output$error_Q3 <- renderText({
      validate(need(input$Q3,
                    "Please enter a numeric value for the total number of clinics in your health system"
      ), errorClass = "red_warnings"
      )
    })
  })
  
  
  output$error_Q10 <- renderUI({
    validate(need(input$Q10, ""))
    validate(need(input$Q10_other, ""))
    if (input$Q10 == "No" & input$Q10_other == 0) {
      helpText(strong("You must enter a numeric value.  If you don't know, please enter 1", style = ("color:red")))
    }
  })
  
  output$error_Q10_other <- renderUI({
    validate(need(input$Q10_other, ""))
    validate(need(input$Q10, ""))
    validate(need(input$Q3, ""))
    Q3 <- ifelse(is.na(input$Q3), 0, input$Q3)
    Q10_other <- ifelse(is.na(input$Q10_other), 0, input$Q10_other)
    condition <- ifelse(input$Q10 == "No" & Q10_other > Q3, 1, 0)
    if (condition == 1) {
      helpText(strong("The number of participating sites must be fewer than the total number of sites in the health system (Question 3)",
                      style = ("color:red")))
    }
  })
  
  observe({
    validate(need(input$Q13, ""))
    
    output$error_Q13_amount <- renderText({
      if (input$Q13=="Yes"){
        validate(need(input$Q13_amount,
                      "Please answer this question"
        ), errorClass = "red_warnings"
        )} else { return("")}
    })
    
    output$error_Q13_source <- renderText({
      if (input$Q13=="Yes"){
        validate(need(input$Q13_source,
                      "Please answer this question"
        ), errorClass = "red_warnings"
        ) } else { return("")}
    })
    
    output$error_Q13_date <- renderText({
      if (input$Q13=="Yes"){
        validate(need(input$Q13_date,
                      "Please answer this question"
        ), errorClass = "red_warnings"
        ) } else { return("")}
    })
    
    output$error_Q13_length <- renderText({
      if (input$Q13 == "Yes") {
        validate(need(input$Q13_length,
                      "Please answer this question"
        ), errorClass = "red_warnings"
        )
      } else { return("")}
    })
  })
  
  })
  # Gather the data and make the temporary save before
  # Moving to the systems background section
  observeEvent(input$button_demo, {
      updateTabsetPanel(session, "tabbox_baseline", selected = "p1_data")
    })

  
  
  # Systems background ------------------------------------------------------------
  
  systems <- myData$systems
  
  output$SelectSystemsUser <- renderUI({
    selectInput(inputId = "SelectSystemsUser",
                label = "Select health system account",
                choices = systems$Username,
                selected = systems$Username[1])
  })
  
  observe({
    validate(need(input$SelectSystemsUser, ""))
    
    systems <- filter(myData$systems, Username == input$SelectSystemsUser)

  # Multi group input vectors
  foo17 <- with(systems, c(Q17_1, Q17_2, Q17_3, Q17_4, Q17_5))
  foo17 <- foo17[!is.na(foo17)]
  
  foo19 <- with(systems, c(Q19_1, Q19_2, Q19_3, Q19_4, Q19_5))
  foo19 <- foo19[!is.na(foo19)]
  
  foo25 <- with(systems, c(Q25_1, Q25_2, Q25_3, Q25_4, Q25_5))
  foo25 <- foo25[!is.na(foo25)]
  
  foo26 <- with(systems, c(Q26_1, Q26_2, Q26_3, Q26_4, Q26_5, Q26_6, Q26_7, Q26_8, Q26_9))
  foo26 <- foo26[!is.na(foo26)]
  
  output$Q15 <- renderUI({
    selectInput(
      inputId = "Q15",
      label = "15.  EHR System",
      choices =c("",
                 "AIIScripts",
                 "Athena",
                 "Cerner",
                 "eClinicalWorks",
                 "Epic",
                 "GE Centricity",
                 "Greenway",
                 "Indian Health Services",
                 "MEDITECH",
                 "NextGen",
                 "Other (specify)"),
      selected = systems$Q15,
      selectize =  FALSE,
      width = '100%')
  })
  output$Q15_other <- renderUI({
    if ("Other (specify)" %in% input$Q15) {
      textInput("Q15_other",
                "Please specify",
                value = systems$Q15_other,
                width = '100%')
    }
  })
  output$Q15_EHRversion <- renderUI({
    textInput(inputId = "Q15_EHRversion",
              label = "15a. EHR version",
              value = systems$Q15_EHRversion,
              width = '100%')
  })
  output$Q16 <- renderUI({
    textInput(
      inputId = "Q16",
      label = "16.  Population Management system and version",
      value = systems$Q16,
      width = '100%')
  })
  
  # Q17 - multiCheckBox
  
  output$Q17 <- renderUI({
    checkboxGroupInput(
      inputId = "Q17",
      label = p("17.  Our system is currently set up to: ",
                em("(check all that apply)")),
      choices = c("Automatically determine which immunizations are due for each patient at every visit",
                  "Alert providers that a patient is due for HPV vaccination",
                  "Provide a report of patients who are not up-to-date on HPV vaccination or have not completed all doses",
                  "Provide a report with provider-specific HPV vaccination rates",
                  "Provide a missed opportunity report that identifies patients who had an appointment, were due for HPV vaccination, but did not receive a vaccine dose"),
      selected = foo17,
      width = "100%")
  })
  output$Q18 <- renderUI({
    selectInput(
      inputId = "Q18",
      label = "18.  At this moment, our EHR system has:",
      choices = c("",
                  "Bidirectional interface with the State Immunization Registry",
                  "Unidirectional interface with the State Immunization Registry",
                  "No interface with the State Immunization Registry"),
      selected = systems$Q18,
      width = "100%")
  })
  
  # Q19 - multiCheckBox
  output$Q19 <- renderUI({
    checkboxGroupInput(
      inputId = "Q19",
      label = p("19.  At this moment, our State Immunization Registry: ",
                em("(check all that apply)")),
      choices = c("Has current and accurate data",
                  "Is used daily to verify patient vaccination status",
                  "Provides data we use to track HPV vaccination rates",
                  "Is not useful to our HPV vaccination work",
                  "May have functions we could use, but have not explored"),
      selected = foo19,
      width = '100%')
  })
  output$Q20 <- renderUI({
    selectInput(
      inputId = "Q20",
      label = "20.  Does your system have standing orders for HPV vaccination?",
      choices = c("", "Yes", "No"),
      selected = systems$Q20)
  })
  output$Q20_notes <- renderUI({
    validate(need(input$Q20, ""))
    if (input$Q20 == "No")
      return(NULL)
    else{
      textAreaInput(inputId =  "Q20_notes",
                    label =   "20a. Please indicate who vaccinates besides a provider, how often, and whether a 
                                copy of standing orders is available for review",
                    placeholder = "",
                    value = systems$Q20_notes,
                    rows = 4
      )
    }
  })
  output$Q20_orders <- renderUI({
    validate(need(input$Q20, ""))
    if (input$Q20 == "No")
      return(NULL)
    else{
      selectInput(inputId = "Q20_orders",
                  label =  "20b. Are standing orders implemented fully and systematically?",
                  choices = c("", "Yes", "No"),
                  selected = systems$Q20_orders)
    }
  })
  output$Q20_other <- renderUI({
    validate(need(input$Q20_orders, ""))
    if (input$Q20_orders == "No") {
      textAreaInput(inputId = "Q20_other",
                    label = "20c. Why haven't standing orders been implemented fully and systematically?",
                    rows = 4,
                    value = systems$Q20_other)
    } else
      return(NULL)
  })
  
  output$Q21 <-   renderUI({
    selectInput(inputId = "Q21",
                label = "21.  Are patients scheduled for the next HPV vaccination visit before they leave the office?",
                choices = c("", "Yes", "No"),
                selected = systems$Q21)
  })
  output$Q21_text <- renderUI({
    validate(need(input$Q21, ""))
    if (input$Q21 != "No")
      return(NULL)
    else{
      textAreaInput(inputId = "Q21_text",
                    label = "21a. Why not?",
                    value = systems$Q21_text,
                    rows = 4)
    }
  })
  
  output$Q22 <- renderUI({
    selectInput(inputId = "Q22",
                label = "22.  If the HPV vaccine is declined, is it system policy to recommend it again at future visits?",
                choices = c("", "Yes", "No"),
                selected = systems$Q22)
  })
  output$Q22_notes <- renderUI({
    validate(need(input$Q22, ""))
    if (input$Q22 != "No") {
      return(NULL)
    } else {
      textAreaInput(inputId = "Q22_notes", 
                    label = "22a. Why isn't this the current policy?",
                    value = systems$Q22_notes,
                    rows = 4)
    }
  })
  output$Q23 <- renderUI({
    selectInput(inputId = "Q23",
                label = "23.  Has your system used quality improvement to increase HPV vaccination rates in the past?",
                choices = c("", "Yes", "No"),
                selected = systems$Q23)
  })
  output$Q23_notes <- renderUI({
    validate(need(input$Q23, ""))
    if (input$Q23 == "No") {
      return(NULL)
    } else {
      textAreaInput(inputId =  "Q23_notes",
                    label = "23a. Please describe your system's quality improvement efforts",
                    value = systems$Q23_notes,
                    rows = 4)
    }
  })
  
  output$Q24 <- renderUI({
    selectInput(inputId = "Q24",
                label = "24.  Has your system completed a HPV VACs Systems and Strategies Inventory (SASI) with ACS in the past?",
                choices = c("", "Yes", "No"),
                selected = systems$Q24)
  })
  output$Q24_years <- renderUI({
    validate(need(input$Q24, ""))
    if (input$Q24 == "No") {
      return(NULL)
    } else {
      numericInput( inputId = "Q24_years",
                    label = p("24a. If yes, how many years has", strong("your system"), "previously participated in a VACs QI project and submitted a SASI?"),
                    value = systems$Q24_years,
                    min = 1,
                    max = 5)
    }
  })
  
  # Q25 checkboxGroup
  output$Q25 <- renderUI({
    checkboxGroupInput(inputId = "Q25",
                       label = p("25.  Which of the following ",
                                 strong("training and education activities ", style = "color:blue"),
                                 "has your system already conducted to increase HPV vaccination rates? ",
                                 em("(With or without support from ACS)."), " Select all that apply"),
                       choices = c("Educated staff on HPV vaccination as cancer prevention",
                                   "Educated staff on strategies to improve HPV vaccination rates",
                                   "Identified HPV vaccination champions",
                                   "Trained providers on making an effective HPV vaccine recommendation",
                                   "Other (specify)"),
                       selected = foo25,
                       width = "100%")
  })
  output$Q25_other <- renderUI({
    validate(need(input$Q25, ""))
    if ("Other (specify)" %in% input$Q25) {
      textInput(inputId = "Q25_other",
                label = "Please specify",
                value = systems$Q25_other,
                width = "100%")
    }
  })
  
  # Q26 checkboxGroup
  output$Q26 <- renderUI({
    checkboxGroupInput(inputId = "Q26",
                       label = p( "26.  Which of the following ",
                                  strong("interventions ", style = "color:blue"),
                                  "to increase HPV vaccination has your system already implemented? ",
                                  em("(With or without support from ACS)."), " Select all that apply"),
                       choices = c("Client reminders",
                                   "Extended hours",
                                   "Modified EHR",
                                   "Offered in alternative settings like schools or mobile units",
                                   "Parent/patient education",
                                   "Provider assessment & feedback",
                                   "Provider prompts/reminders",
                                   "Standing orders",
                                   "Other (specify)"),
                       selected =  foo26,
                       width = '100%')
  })
  output$Q26_other <- renderUI({
    validate(need(input$Q26, ""))
    if ("Other (specify)" %in% input$Q26) {
      textInput(inputId = "Q26_other",
                label = "Please specify",
                value = systems$Q26_other,
                width = "100%")
    }
  })
  })
  
  observeEvent(input$button_systems, {
    updateTabsetPanel(session, "tabbox_baseline", selected = "p1_rates")
  })
  
  
  # Rates page  -------------------------------------------------------------
  
  savedRates <- myData$savedRates
  
  output$SelectRatesUser <- renderUI({
    selectInput(inputId = "SelectRatesUser",
                label = "Select health system account",
                choices = unique(savedRates$Username),
                selected = savedRates$Username[1])
  })
  
  observe({
    
    validate(need(input$SelectRatesUser, ""))
    savedRates <- filter(myData$savedRates, Username == input$SelectRatesUser)
    
    # Show the last updated date
    output$date_brates <- renderUI({
      h4(paste("This health system's data was most recently updated on ", (savedRates$baseratesDate)))
    })
  
  # Checkbox groups
  foo28 <- with(savedRates, c(Q28_1, Q28_2, Q28_3, Q28_4))
  foo28 <- foo28[!is.na(foo28)]
  
  foo32 <- with(savedRates, c(Q32_details1, Q32_details2, Q32_details3, Q32_details4))
  foo32 <- foo32[!is.na(foo32)]
  
  output$Q27 <- renderUI({
    selectInput(
      inputId = "Q27",
      label = "27.  How are you defining your active patient populations?",
      choices = c("",
                  "12 months",
                  "18 months",
                  "24 months",
                  "Other (please specify)"),
      selected = savedRates$Q27,
      selectize = F
    )
  })
  output$Q27_other <- renderUI({
    validate(need(input$Q27, ""))
    if (input$Q27 != "Other (please specify)") {
      return(NULL)
    } else {
      textInput(
        inputId = "Q27_other",
        label = "Please specify",
        value = savedRates$Q27_other,
        placeholder = "",
        width = "150%")
    }
  })
  
  
  # Q28 - CheckBoxGroup
  output$Q28 <- renderUI({
    checkboxGroupInput(
      inputId = "Q28",
      label = "28. For what ages are you reporting?",
      choices = c("9-10",
                  "11-12",
                  "13",
                  "We can't report on these ages (other, please specify)?"),
      selected = foo28)
  })
  output$Q28_other <- renderUI({
    validate(need(input$Q28, ""))
    if ("We can't report on these ages (other, please specify)?" %in% input$Q28) {
      textInput(
        inputId = "Q28_other",
        label = "Please specify",
        value = savedRates$Q28_other,
        placeholder = "",
        width = "150%"
      )
    } else {
      return(NULL)
    }
  })
  
  output$Q29 <- renderUI({
    selectInput(
      inputId = "Q29",
      label = "29.  Can you pull rate data by sex?",
      choices = c("",
                  "Yes, we'll report separate data for male and female patients",
                  "No, we will have combined rate data"),
      selected = savedRates$Q29)
  })
  
  
  # Create the ages 9-10 boxes
  observeEvent(list(input$Q28, input$Q29), {
    validate(need(input$Q28, ""))
    validate(need(input$Q29, ""))
    
    # Boys and girls - ages 9-10
    if ("9-10" %in% input$Q28 &
        input$Q29 == "Yes, we'll report separate data for male and female patients") {
      
      # Girls ages 9-10 rates 
      output$brates_9_10 <- renderUI({
        verticalLayout(
          splitLayout(
            box(
              h3("Vaccination Rates for Female Patients, Ages 9-10"),
              title = "Vaccination Rates for Female Patients, Ages 9-10",
              collapsible = T,
              solidHeader = T,
              status = "primary",
              collapsed = T,
              width = '100%',
              HTML("<br>"),
              h5(
                strong(
                  "Total number of active ",
                  strong("female ", style = "color:blue"),
                  "medical patients ",
                  em("ages 9-10 ", style = "color:red"),
                  "your clinic saw in 2020?"
                )
              ),
              numericInput(
                "FemAge1_total",
                label = NULL,
                value = savedRates$FemAge1_total,
                min = 0,
                step = 1,
                width = '50%'
              ),
              textOutput("error_f9_total"),
              h5(
                strong(
                  "Number of active patients who received at least one (1) HPV dose"
                )
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemAge1_dose1",
                  label = NULL,
                  min = 0,
                  value = savedRates$FemAge1_dose1,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemAge1_dose1_rate")
              ),
              textOutput("error_f9_gt1"),
              h5(
                strong("Number of active patients who received both (2) HPV doses")
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemAge1_dose2",
                  label = NULL,
                  min = 0,
                  value = savedRates$FemAge1_dose2,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemAge1_dose2_rate")
              ),
              textOutput("error_f9_2plus"),
              textOutput("error_f9a_2plus")
            ),
            box( # Men aged 9-10
              h3("Males, ages 9-10"),
              title = "Males, ages 9-10",
              collapsible = T,
              solidHeader = T,
              status = "danger",
              collapsed = T,
              width = '100%',
              HTML("<br>"),
              h5(
                strong(
                  "Total number of active ",
                  strong("male ", style = "color:blue"),
                  "medical patients ",
                  em("ages 9-10 ", style = "color:red"),
                  "your clinic saw in 2020?"
                )
              ),
              numericInput(
                "MenAge1_total",
                label = NULL,
                value = savedRates$MenAge1_total,
                min = 0,
                step = 1,
                width = '50%'
              ),
              textOutput("error_m9_total"),
              h5(
                strong(
                  "Number of active patients who received at least one (1) HPV dose"
                )
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenAge1_dose1",
                  label = NULL,
                  min = 0,
                  value =  savedRates$MenAge1_dose1,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenAge1_dose1_rate")
              ),
              textOutput("error_m9_gt1"),
              h5(
                strong("Number of active patients who received both (2) HPV doses")
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenAge1_dose2",
                  label = NULL,
                  min = 0,
                  value =  savedRates$MenAge1_dose2,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenAge1_dose2_rate")
              ),
              textOutput("error_m9_2plus"),
              textOutput("error_m9a_2plus")
            )
          )) # end split and veritical layout
      })
      output$FemAge1_dose1_rate <- renderText({
        validate(need(input$FemAge1_total, ""))
        if (is.na(input$FemAge1_dose1)) {
          return(NULL)
        }
        if (!is.na(input$FemAge1_dose1)) {
          rate <- input$FemAge1_dose1 / input$FemAge1_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group.")
          return(rate)
        }
      })
      output$FemAge1_dose2_rate <- renderText({
        validate(need(input$FemAge1_total, ""))
        if (is.na(input$FemAge1_dose2)) {
          return(NULL)
        }
        if (!is.na(input$FemAge1_dose2)) {
          rate <- input$FemAge1_dose2 / input$FemAge1_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group.")
          return(rate)
        }
      })
      observe({
        output$error_f9_total <- renderText({
          validate(need(input$FemAge1_total, "This number is required"),
                   errorClass = "red_warnings")
        })
      })
      
      
      output$MenAge1_dose1_rate <- renderText({
        validate(need(input$MenAge1_total, ""))
        if (is.na(input$MenAge1_dose1)) {
          return(NULL)
        }
        if (!is.na(input$MenAge1_dose1)) {
          rate <- input$MenAge1_dose1 / input$MenAge1_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group.")
          return(rate)
        }
      })
      output$MenAge1_dose2_rate <- renderText({
        validate(need(input$MenAge1_total, ""))
        if (is.na(input$MenAge1_dose2)) {
          return(NULL)
        }
        if (!is.na(input$MenAge1_dose2)) {
          rate <- input$MenAge1_dose2 / input$MenAge1_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group.")
          return(rate)
        }
      })
    }
    
    # Ages 9-10, combined
    if ("9-10" %in% input$Q28 &
        input$Q29 == "No, we will have combined rate data") {
      # Create the 'male and females combined' box
      output$brates_9_10 <- renderUI({
        box(
          h3("Vaccination Rates for Male and Female Patients Combined, Ages 9-10"),
          title = "Vaccination Rates for Male and Female Patients Combined, Ages 9-10",
          collapsible = T,
          solidHeader = T,
          status = "success",
          collapsed = T,
          width = '100%',
          HTML("<br>"),
          h5(
            strong(
              "Total number of active medical patients ",
              em("ages 9-10 ", style = "color:red"),
              "your clinic saw in 2020?"
            )
          ),
          numericInput(
            "BothAge1_total",
            label = NULL,
            value = savedRates$BothAge1_total,
            min = 0,
            step = 1,
            width = '50%'
          ),
          textOutput("error_9_total"),
          h5(
            strong(
              "Number of active patients who received at least one (1) HPV dose"
            )
          ),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothAge1_dose1",
              label = NULL,
              min = 0,
              value = savedRates$BothAge1_dose1,
              step = 1,
              width = '50%'
            ),
            textOutput("BothAge1_dose1_rate")
          ),
          textOutput("error_9_gt1"),
          h5(
            strong("Number of active patients who received both (2) HPV doses")
          ),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(inputId = "BothAge1_dose2",
                         label = NULL,
                         min = 0,
                         value = savedRates$BothAge1_dose2,
                         step = 1,
                         width = '50%'
            ),
            textOutput("BothAge1_dose2_rate")
          ),
          textOutput("error_9_2plus"),
          textOutput("error_9a_2plus")
        )
      })
      output$BothAge1_dose1_rate <- renderText({
        validate(need(input$BothAge1_total, ""))
        if (is.na(input$BothAge1_dose1)) {
          return(NULL)
        }
        if (!is.na(input$BothAge1_dose1)) {
          rate <- input$BothAge1_dose1 / input$BothAge1_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group.")
          return(rate)
        }
      })
      output$BothAge1_dose2_rate <- renderText({
        validate(need(input$BothAge1_total, ""))
        if (is.na(input$BothAge1_dose2)) {
          return(NULL)
        }
        if (!is.na(input$BothAge1_dose2)) {
          rate <- input$BothAge1_dose2 / input$BothAge1_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group.")
          return(rate)
        }
      })
    }
    
  }) # end the 9-10 observation
  
  # Create the ages 11-12 boxes
  observeEvent(list(input$Q28, input$Q29), {
    validate(need(input$Q28, ""))
    validate(need(input$Q29, ""))
    
    if ("11-12" %in% input$Q28 &
        input$Q29 == "Yes, we'll report separate data for male and female patients") {
      
      # Girls ages 11-12
      output$brates_11_12 <- renderUI({
        verticalLayout(
          splitLayout(
            box(
              h3("Vaccination Rates for Females Patients, Ages 11-12"),
              title = "Vaccination Rates for Female Patients, Ages 11-12",
              collapsible = T,
              solidHeader = T,
              status = "primary",
              collapsed = T,
              width = '100%',
              HTML("<br>"),
              h5(
                strong(
                  "Total number of active ",
                  strong("female ", style = "color:blue"),
                  "medical patients ",
                  em("ages 11-12 ", style = "color:red"),
                  "your clinic saw in 2020?"
                )
              ),
              numericInput(
                "FemAge2_total",
                label = NULL,
                value = savedRates$FemAge2_total,
                min = 0,
                step = 1,
                width = '100%'
              ),
              textOutput("error_f11_total"),
              h5(
                strong(
                  "Number of active patients who received at least one (1) HPV dose"
                )
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemAge2_dose1",
                  label = NULL,
                  min = 0,
                  value = savedRates$FemAge2_dose1,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemAge2_dose1_rate")
              ),
              textOutput("error_f11_gt1"),
              h5(
                strong("Number of active patients who received both (2) HPV doses")
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemAge2_dose2",
                  label = NULL,
                  min = 0,
                  value = savedRates$FemAge2_dose1,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemAge2_dose2_rate")
              ),
              textOutput("error_f11_2plus"),
              textOutput("error_f11a_2plus"),
              h5(strong("Meningococcal")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemAge2_mening",
                  label = NULL,
                  min = 0,
                  value = savedRates$FemAge2_mening,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemAge2_mening_rate")
              ),
              textOutput("error_f11_mening"),
              h5(strong("Tdap")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemAge2_tdap",
                  label = NULL,
                  min = 0,
                  value = savedRates$FemAge2_tdap,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemAge2_tdap_rate")
              ),
              textOutput("error_f11_Tdap"),
            ),
            box( # males aged 11-12
              h3("Males, ages 11-12"),
              title = "Males, ages 11-12",
              collapsible = T,
              solidHeader = T,
              status = "danger",
              collapsed = T,
              width = '100%',
              HTML("<br>"),
              h5(
                strong(
                  "Total number of active ",
                  strong("male ", style = "color:blue"),
                  "medical patients ",
                  em("ages 11-12 ", style = "color:red"),
                  "your clinic saw in 2020?"
                )
              ),
              numericInput(
                "MenAge2_total",
                label = NULL,
                value = savedRates$MenAge2_total,
                min = 0,
                step = 1,
                width = '100%'
              ),
              textOutput("error_m11_total"),
              h5(
                strong(
                  "Number of active patients who received at least one (1) HPV dose"
                )
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenAge2_dose1",
                  label = NULL,
                  min = 0,
                  value = savedRates$MenAge2_dose1,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenAge2_dose1_rate")
              ),
              textOutput("error_m11_gt1"),
              h5(
                strong("Number of active patients who received both (2) HPV doses")
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenAge2_dose2",
                  label = NULL,
                  min = 0,
                  value = savedRates$MenAge2_dose2,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenAge2_dose2_rate")
              ),
              textOutput("error_m11_2plus"),
              textOutput("error_m11a_2plus"),
              h5(strong("Meningococcal")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenAge2_mening",
                  label = NULL,
                  min = 0,
                  value = savedRates$MenAge2_mening,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenAge2_mening_rate")
              ),
              textOutput("error_m11_mening"),
              h5(strong("Tdap")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenAge2_tdap",
                  label = NULL,
                  min = 0,
                  value = savedRates$MenAge2_tdap,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenAge2_tdap_rate")
              ),
              textOutput("error_m11_Tdap"),
            )
          )) # end split and vertical layout
      })
      output$FemAge2_dose1_rate <- renderText({
        validate(need(input$FemAge2_total, ""))
        if (is.na(input$FemAge2_dose1)) {
          return(NULL)
        }
        if (!is.na(input$FemAge2_dose1)) {
          rate <- input$FemAge2_dose1 / input$FemAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$FemAge2_dose2_rate <- renderText({
        validate(need(input$FemAge2_total, ""))
        if (is.na(input$FemAge2_dose2)) {
          return(NULL)
        }
        if (!is.na(input$FemAge2_dose2)) {
          rate <- input$FemAge2_dose2 / input$FemAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$FemAge2_mening_rate <- renderText({
        validate(need(input$FemAge2_total, ""))
        if (is.na(input$FemAge2_mening)) {
          return(NULL)
        }
        if (!is.na(input$FemAge2_mening)) {
          rate <- input$FemAge2_mening / input$FemAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$FemAge2_tdap_rate <- renderText({
        validate(need(input$FemAge2_total, ""))
        if (is.na(input$FemAge2_tdap)) {
          return(NULL)
        }
        if (!is.na(input$FemAge2_tdap)) {
          rate <- input$FemAge2_tdap / input$FemAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      
      
      
      output$MenAge2_dose1_rate <- renderText({
        validate(need(input$MenAge2_total, ""))
        if (is.na(input$MenAge2_dose1)) {
          return(NULL)
        }
        if (!is.na(input$MenAge2_dose1)) {
          rate <- input$MenAge2_dose1 / input$MenAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$MenAge2_dose2_rate <- renderText({
        validate(need(input$MenAge2_total, ""))
        if (is.na(input$MenAge2_dose2)) {
          return(NULL)
        }
        if (!is.na(input$MenAge2_dose2)) {
          rate <- input$MenAge2_dose2 / input$MenAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$MenAge2_mening_rate <- renderText({
        validate(need(input$MenAge2_total, ""))
        if (is.na(input$MenAge2_mening)) {
          return(NULL)
        }
        if (!is.na(input$MenAge2_mening)) {
          rate <- input$MenAge2_mening / input$MenAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$MenAge2_tdap_rate <- renderText({
        validate(need(input$MenAge2_total, ""))
        if (is.na(input$MenAge2_tdap)) {
          return(NULL)
        }
        if (!is.na(input$MenAge2_tdap)) {
          rate <- input$MenAge2_tdap / input$MenAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      
    }
    # Ages 11-12 - boys and girls combined
    if ("11-12" %in% input$Q28 &
        input$Q29 == "No, we will have combined rate data") {
      output$brates_11_12 <- renderUI({
        box(
          h3("Vaccination Rates for Male and Female Patients Combined, Ages 11-12"),
          title = "Vaccination Rates for Male and Female Patients Combined, Ages 11-12",
          collapsible = T,
          solidHeader = T,
          status = "success",
          collapsed = T,
          width = '100%',
          HTML("<br>"),
          h5(
            strong(
              "Total number of active medical patients ",
              em("ages 11-12 ", style = "color:red"),
              "your clinic saw in 2020?"
            )
          ),
          numericInput(
            "BothAge2_total",
            label = NULL,
            value = savedRates$BothAge2_total,
            min = 0,
            step = 1,
            width = '100%'
          ),
          textOutput("error_11_total"),
          h5(
            strong(
              "Number of active patients who received at least one (1) HPV dose"
            )
          ),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothAge2_dose1",
              label = NULL,
              min = 0,
              value = savedRates$BothAge2_dose1,
              step = 1,
              width = '50%'
            ),
            textOutput("BothAge2_dose1_rate")
          ),
          textOutput("error_11_gt1"),
          h5(
            strong("Number of active patients who received both (2) HPV doses")
          ),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothAge2_dose2",
              label = NULL,
              min = 0,
              value = savedRates$BothAge2_dose2,
              step = 1,
              width = '50%'
            ),
            textOutput("BothAge2_dose2_rate")
          ),
          textOutput("error_11_2plus"),
          textOutput("error_11a_2plus"),
          h5(strong("Meningococcal")),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothAge2_mening",
              label = NULL,
              min = 0,
              value = savedRates$BothAge2_mening,
              step = 1,
              width = '50%'
            ),
            textOutput("BothAge2_mening_rate")
          ),
          textOutput("error_11_mening"),
          h5(strong("Tdap")),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothAge2_tdap",
              label = NULL,
              min = 0,
              value = savedRates$BothAge2_tdap,
              step = 1,
              width = '50%'
            ),
            textOutput("BothAge2_tdap_rate")
          ),
          textOutput("error_11_Tdap"),
        )
      })
      output$BothAge2_dose1_rate <- renderText({
        validate(need(input$BothAge2_total, ""))
        if (is.na(input$BothAge2_dose1)) {
          return(NULL)
        }
        if (!is.na(input$BothAge2_dose1)) {
          rate <- input$BothAge2_dose1 / input$BothAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$BothAge2_dose2_rate <- renderText({
        validate(need(input$BothAge2_total, ""))
        if (is.na(input$BothAge2_dose2)) {
          return(NULL)
        }
        if (!is.na(input$BothAge2_dose2)) {
          rate <- input$BothAge2_dose2 / input$BothAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$BothAge2_mening_rate <- renderText({
        validate(need(input$BothAge2_total, ""))
        if (is.na(input$BothAge2_mening)) {
          return(NULL)
        }
        if (!is.na(input$BothAge2_mening)) {
          rate <- input$BothAge2_mening / input$BothAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$BothAge2_tdap_rate <- renderText({
        validate(need(input$BothAge2_total, ""))
        if (is.na(input$BothAge2_tdap)) {
          return(NULL)
        }
        if (!is.na(input$BothAge2_tdap)) {
          rate <- input$BothAge2_tdap / input$BothAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
    }
  }) # end the 11-12 observation
  
  
  # Age 13 boxes
  observeEvent(list(input$Q28, input$Q29), {
    validate(need(input$Q28, ""))
    validate(need(input$Q29, ""))
    
    if ("13" %in% input$Q28 &
        input$Q29 == "Yes, we'll report separate data for male and female patients") {
      
      # Girls age 13
      output$brates_13 <- renderUI({
        verticalLayout(
          splitLayout(
            box(
              h3("Vaccination Rates for Female Patients, Age 13"),
              title = "Vaccination Rates for Female Patients, Age 13",
              collapsible = T,
              solidHeader = T,
              status = "primary",
              collapsed = T,
              width = '100%',
              HTML("<br>"),
              h5(
                strong(
                  "Total number of active ",
                  strong("female ", style = "color:blue"),
                  "medical patients ",
                  em("age 13 ", style = "color:red"),
                  "your clinic saw in 2020?"
                )
              ),
              numericInput(
                "FemAge3_total",
                label = NULL,
                value = savedRates$FemAge3_total,
                min = 0,
                step = 1,
                width = '100%'
              ),
              textOutput("error_f13_total"),
              h5(
                strong(
                  "Number of active patients who received at least one (1) HPV dose"
                )
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemAge3_dose1",
                  label = NULL,
                  min = 0,
                  value = savedRates$FemAge3_dose1,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemAge3_dose1_rate")
              ),
              textOutput("error_f13_gt1"),
              h5(
                strong("Number of active patients who received both (2) HPV doses")
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemAge3_dose2",
                  label = NULL,
                  min = 0,
                  value = savedRates$FemAge3_dose2,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemAge3_dose2_rate")
              ),
              textOutput("error_f13_2plus"),
              textOutput("error_f13a_2plus"),
              h5(strong("Meningococcal")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemAge3_mening",
                  label = NULL,
                  min = 0,
                  value = savedRates$FemAge3_mening,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemAge3_mening_rate")
              ),
              textOutput("error_f13_mening"),
              h5(strong("Tdap")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemAge3_tdap",
                  label = NULL,
                  min = 0,
                  value = savedRates$FemAge3_tdap,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemAge3_tdap_rate")
              ),
              textOutput("error_f13_Tdap"),
            ),
            box( # boys aged 13
              h3("Males, age 13"),
              title = "Males, age 13",
              collapsible = T,
              solidHeader = T,
              status = "danger",
              collapsed = T,
              width = '100%',
              HTML("<br>"),
              h5(
                strong(
                  "Total number of active ",
                  strong("male ", style = "color:blue"),
                  "medical patients ",
                  em("age 13 ", style = "color:red"),
                  "your clinic saw in 2020?"
                )
              ),
              numericInput(
                "MenAge3_total",
                label = NULL,
                value = savedRates$MenAge3_total,
                min = 0,
                step = 1,
                width = '100%'
              ),
              textOutput("error_m13_total"),
              h5(
                strong(
                  "Number of active patients who received at least one (1) HPV dose"
                )
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenAge3_dose1",
                  label = NULL,
                  min = 0,
                  value = savedRates$MenAge3_total,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenAge3_dose1_rate")
              ),
              textOutput("error_m13_gt1"),
              h5(
                strong("Number of active patients who received both (2) HPV doses")
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenAge3_dose2",
                  label = NULL,
                  min = 0,
                  value = savedRates$MenAge3_dose2,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenAge3_dose2_rate")
              ),
              textOutput("error_m13_2plus"),
              textOutput("error_m13a_2plus"),
              h5(strong("Meningococcal")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenAge3_mening",
                  label = NULL,
                  min = 0,
                  value = savedRates$MenAge3_mening,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenAge3_mening_rate")
              ),
              textOutput("error_m13_mening"),
              h5(strong("Tdap")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenAge3_tdap",
                  label = NULL,
                  min = 0,
                  value = savedRates$MenAge3_tdap,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenAge3_tdap_rate")
              ),
              textOutput("error_m13_Tdap"),
            )
          ))
      })
      output$FemAge3_dose1_rate <- renderText({
        validate(need(input$FemAge3_total, ""))
        if (is.na(input$FemAge3_dose1)) {
          return(NULL)
        }
        if (!is.na(input$FemAge3_dose1)) {
          rate <- input$FemAge3_dose1 / input$FemAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$FemAge3_dose2_rate <- renderText({
        validate(need(input$FemAge3_total, ""))
        if (is.na(input$FemAge3_dose2)) {
          return(NULL)
        }
        if (!is.na(input$FemAge3_dose2)) {
          rate <- input$FemAge3_dose2 / input$FemAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$FemAge3_mening_rate <- renderText({
        validate(need(input$FemAge3_total, ""))
        if (is.na(input$FemAge3_mening)) {
          return(NULL)
        }
        if (!is.na(input$FemAge3_mening)) {
          rate <- input$FemAge3_mening / input$FemAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$FemAge3_tdap_rate <- renderText({
        validate(need(input$FemAge3_total, ""))
        if (is.na(input$FemAge3_tdap)) {
          return(NULL)
        }
        if (!is.na(input$FemAge3_tdap)) {
          rate <- input$FemAge3_tdap / input$FemAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      
      
      output$MenAge3_dose1_rate <- renderText({
        validate(need(input$MenAge3_total, ""))
        if (is.na(input$MenAge3_dose1)) {
          return(NULL)
        }
        if (!is.na(input$MenAge3_dose1)) {
          rate <- input$MenAge3_dose1 / input$MenAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$MenAge3_dose2_rate <- renderText({
        validate(need(input$MenAge3_total, ""))
        if (is.na(input$MenAge3_dose2)) {
          return(NULL)
        }
        if (!is.na(input$MenAge3_dose2)) {
          rate <- input$MenAge3_dose2 / input$MenAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$MenAge3_mening_rate <- renderText({
        validate(need(input$MenAge3_total, ""))
        if (is.na(input$MenAge3_mening)) {
          return(NULL)
        }
        if (!is.na(input$MenAge3_mening)) {
          rate <- input$MenAge3_mening / input$MenAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$MenAge3_tdap_rate <- renderText({
        validate(need(input$MenAge3_total, ""))
        if (is.na(input$MenAge3_tdap)) {
          return(NULL)
        }
        if (!is.na(input$MenAge3_tdap)) {
          rate <- input$MenAge3_tdap / input$MenAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
    }
    # Boys and girls combined - age 13
    if ("13" %in% input$Q28 &
        input$Q29 == "No, we will have combined rate data") {
      output$brates_13 <- renderUI({
        box(
          h3("Vaccination Rates for Male and Female Patients Combined, Age 13"),
          title = "Vaccination Rates for Male and Female Patients Combined, Age 13",
          collapsible = T,
          solidHeader = T,
          status = "success",
          collapsed = T,
          width = '100%',
          HTML("<br>"),
          h5(
            strong(
              "Total number of active medical patients ",
              em("age 13 ", style = "color:red"),
              "your clinic saw in 2020?"
            )
          ),
          numericInput(
            "BothAge3_total",
            label = NULL,
            value = savedRates$BothAge3_total,
            min = 0,
            step = 1,
            width = '100%'
          ),
          textOutput("error_13_total"),
          h5(
            strong(
              "Number of active patients who received at least one (1) HPV dose"
            )
          ),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothAge3_dose1",
              label = NULL,
              min = 0,
              value = savedRates$BothAge3_dose1,
              step = 1,
              width = '50%'
            ),
            textOutput("BothAge3_dose1_rate")
          ),
          textOutput("error_13_gt1"),
          h5(
            strong("Number of active patients who received both (2) HPV doses")
          ),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothAge3_dose2",
              label = NULL,
              min = 0,
              value = savedRates$BothAge3_dose2,
              step = 1,
              width = '50%'
            ),
            textOutput("BothAge3_dose2_rate")
          ),
          textOutput("error_13_2plus"),
          textOutput("error_13a_2plus"),
          h5(strong("Meningococcal")),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothAge3_mening",
              label = NULL,
              min = 0,
              value = savedRates$BothAge3_mening,
              step = 1,
              width = '50%'
            ),
            textOutput("BothAge3_mening_rate")
          ),
          textOutput("error_13_mening"),
          h5(strong("Tdap")),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothAge3_tdap",
              label = NULL,
              min = 0,
              value = savedRates$BothAge3_tdap,
              step = 1,
              width = '50%'
            ),
            textOutput("BothAge3_tdap_rate")
          ),
          textOutput("error_13_Tdap"),
        )
      })
      output$BothAge3_dose1_rate <- renderText({
        validate(need(input$BothAge3_total, ""))
        if (is.na(input$BothAge3_dose1)) {
          return(NULL)
        }
        if (!is.na(input$BothAge3_dose1)) {
          rate <- input$BothAge3_dose1 / input$BothAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$BothAge3_dose2_rate <- renderText({
        validate(need(input$BothAge3_total, ""))
        if (is.na(input$BothAge3_dose2)) {
          return(NULL)
        }
        if (!is.na(input$BothAge3_dose2)) {
          rate <- input$BothAge3_dose2 / input$BothAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$BothAge3_mening_rate <- renderText({
        validate(need(input$BothAge3_total, ""))
        if (is.na(input$BothAge3_mening)) {
          return(NULL)
        }
        if (!is.na(input$BothAge3_mening)) {
          rate <- input$BothAge3_mening / input$BothAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$BothAge3_tdap_rate <- renderText({
        validate(need(input$BothAge3_total, ""))
        if (is.na(input$BothAge3_tdap)) {
          return(NULL)
        }
        if (!is.na(input$BothAge3_tdap)) {
          rate <- input$BothAge3_tdap / input$BothAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
    }
    
  }) # end the 13 observation
  
  output$Q30 <- renderUI({
    textAreaInput(
      inputId = "Q30",
      label = "30.  Please share anything else about your baseline data you'd like us to know.",
      value =  savedRates$Q30,
      rows = 4)
  })
  
  output$Q31 <- renderUI({
    selectInput(inputId = "Q31",
                label = "31.  What was the primary data source used to calculate your vaccination rates?",
                choices =c("",
                           "EHR (preferred)",
                           "Chart Audit",
                           "State Immunization Registry",
                           "Other (specify)"),
                selectize = F,
                selected = savedRates$Q31,
                width = "100%")
  })
  output$Q31_other <- renderUI({
    validate(need(input$Q31, ""))
    if ("Other (specify)" %in% input$Q31) {
      textInput("Q31_other",
                label = "Please specify",
                value = savedRates$Q31_other,
                width = "100%")
    }
  })
  
  output$Q32 <- renderUI({
    selectInput(
      inputId = "Q32",
      label = "32.  Did you use a secondary data source?",
      choices = c("", "Yes", "No"),
      selected = savedRates$Q32,
      selectize = T)
  })
  
  # Q32 - group textbox
  output$Q32_details <- renderUI({
    validate(need(input$Q32, ""))
    if (input$Q32 == "Yes") {
      checkboxGroupInput(
        inputId = "Q32_details",
        label = "32a. What secondary sources were used (select all that apply)?",
        choices = c( "EHR",
                     "Chart Audit",
                     "State Immunization Registry",
                     "Other (specify)"),
        selected = foo32)
    }
  })
  output$Q32_other <- renderUI({
    validate(need(input$Q32_details, ""))
    if ("Other (specify)" %in% input$Q32_details) {
      textInput("Q32_other",
                label = "Please specify",
                value = savedRates$Q32_other,
                width = "100%")
    }
  })
  
  output$Q33 <- renderUI({
    selectInput(
      inputId = "Q33",
      label = "33.  Were you unable to report any of the requested data?",
      choices = c("", "Yes", "No"),
      selected = savedRates$Q33,
      selectize = T)
  })
  output$Q33_other <- renderUI({
    validate(need(input$Q33, ""))
    if (input$Q33 == "Yes") {
      textAreaInput("Q33_other",
                    label = "33a. Describe what you were unable to report and why.",
                    value = savedRates$Q33_other,
                    rows = 4)
    }
  })
  
  output$Q34 <- renderUI({
    selectInput(
      inputId = "Q34",
      label = "34.  Are you working on your reporting capacity and anticipate submitting updated data in the next few months?",
      choices = c("", "Yes", "No"),
      selected = savedRates$Q34,
      selectize = T,
      width = "100%")
  })
  
  
  
  
  # ---- RATES ----- #
  # Create the static table that people will use as reference
  output$static_table <- renderTable({
    q <-
      data.frame(rbind(
        c("AGES", "BORN", "EVER RECEIVED THE FOLLOWING VACCINES"),
        c("9-10", "2010-2011", "At least 1 dose of HPV; 2 doses of HPV"),
        c("11-12","2008-2009", "At least 1 dose of HPV; 2 doses of HPV; Meningococcal; Tdap"),
        c("13", "2007",  "At least 1 dose of HPV; 2 doses of HPV; Meningococcal; Tdap")
      ))
  }, striped = FALSE, align = "l", colnames = FALSE, rownames = FALSE, bordered =
    TRUE, spacing = "m", width = "70%")
  
  
  
  # Error warnings for the rate tables
  
  # Females (age 9)
  observe({
    output$error_f9_total <- renderText({
      validate(need(input$FemAge1_total, "This number is required"),
               errorClass = "red_warnings")
    })
  })
  observe({
    output$error_f9_gt1 <- renderText({
      if (is.na(input$FemAge1_dose1) | is.na(input$FemAge1_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemAge1_dose1 <= input$FemAge1_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_f9_2plus <- renderText({
      if (is.na(input$FemAge1_dose2) | is.na(input$FemAge1_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemAge1_dose2 <= input$FemAge1_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_f9a_2plus <- renderText({
      if (is.na(input$FemAge1_dose2) | is.na(input$FemAge1_dose1)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemAge1_dose2 <= input$FemAge1_dose1,
            "This number can't be greater than the total number of patients who received the first dose"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  
  # Males
  observe({
    output$error_m9_total <- renderText({
      validate(need(input$MenAge1_total, "This number is required"),
               errorClass = "red_warnings") # require responses for the male rates
    })
  })
  observe({
    output$error_m9_gt1 <- renderText({
      if (is.na(input$MenAge1_dose1) | is.na(input$MenAge1_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenAge1_dose1 <= input$MenAge1_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_m9_2plus <- renderText({
      if (is.na(input$MenAge1_dose2) | is.na(input$MenAge1_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenAge1_dose2 <= input$MenAge1_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_m9a_2plus <- renderText({
      if (is.na(input$MenAge1_dose2) | is.na(input$MenAge1_dose1)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenAge1_dose2 <= input$MenAge1_dose1,
            "This number can't be greater than the total number of patients who received the first dose"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  
  # Combined (males and females)
  observe({
    output$error_9_total <- renderText({
      validate(need(input$BothAge1_total, "This number is required"),
               errorClass = "red_warnings") # require responses for the male rates
    })
  })
  observe({
    output$error_9_gt1 <- renderText({
      if (is.na(input$BothAge1_dose1) | is.na(input$BothAge1_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothAge1_dose1 <= input$BothAge1_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_9_2plus <- renderText({
      if (is.na(input$BothAge1_dose2) | is.na(input$BothAge1_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothAge1_dose2 <= input$BothAge1_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_9a_2plus <- renderText({
      if (is.na(input$BothAge1_dose2) | is.na(input$BothAge1_dose1)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothAge1_dose2 <= input$BothAge1_dose1,
            "This number can't be greater than the total number of patients who received the first dose"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  
  
  # Aged 11-12 warnings ------------------------------- #
  
  # Females
  observe({
    output$error_f11_total <- renderText({
      validate(need(input$FemAge2_total, "This number is required"),
               errorClass = "red_warnings") # require responses for the female rates
    })
  })
  observe({
    output$error_f11_gt1 <- renderText({
      if (is.na(input$FemAge2_dose1) | is.na(input$FemAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemAge2_dose1 <= input$FemAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_f11_2plus <- renderText({
      if (is.na(input$FemAge2_dose2) | is.na(input$FemAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemAge2_dose2 <= input$FemAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_f11a_2plus <- renderText({
      if (is.na(input$FemAge2_dose2) | is.na(input$FemAge2_dose1)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemAge2_dose2 <= input$FemAge2_dose1,
            "This number can't be greater than the total number of patients who received the first dose"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_f11_mening <- renderText({
      if (is.na(input$FemAge2_mening) | is.na(input$FemAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemAge2_mening <= input$FemAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_f11_Tdap <- renderText({
      if (is.na(input$FemAge2_tdap) | is.na(input$FemAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemAge2_tdap <= input$FemAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  
  # Males
  observe({
    output$error_m11_total <- renderText({
      validate(need(input$MenAge2_total, "This number is required"),
               errorClass = "red_warnings") # require responses for the male rates
    })
  })
  observe({
    output$error_m11_gt1 <- renderText({
      if (is.na(input$MenAge2_dose1) | is.na(input$MenAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenAge2_dose1 <= input$MenAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_m11_2plus <- renderText({
      if (is.na(input$MenAge2_dose2) | is.na(input$MenAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenAge2_dose2 <= input$MenAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_m11a_2plus <- renderText({
      if (is.na(input$MenAge2_dose2) | is.na(input$MenAge2_dose1)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenAge2_dose2 <= input$MenAge2_dose1,
            "This number can't be greater than the total number of patients who received the first dose"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_m11_mening <- renderText({
      if (is.na(input$MenAge2_mening) | is.na(input$MenAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenAge2_mening <= input$MenAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_m11_Tdap <- renderText({
      if (is.na(input$MenAge2_tdap) | is.na(input$MenAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenAge2_tdap <= input$MenAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  
  # Combined (males and females)
  observe({
    output$error_11_total <- renderText({
      validate(need(input$BothAge2_total, "This number is required"),
               errorClass = "red_warnings") # require responses for the male rates
    })
  })
  observe({
    output$error_11_gt1 <- renderText({
      if (is.na(input$BothAge2_dose1) | is.na(input$BothAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothAge2_dose1 <= input$BothAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_11_2plus <- renderText({
      if (is.na(input$BothAge2_dose2) | is.na(input$BothAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothAge2_dose2 <= input$BothAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_11a_2plus <- renderText({
      if (is.na(input$BothAge2_dose2) | is.na(input$BothAge2_dose1)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothAge2_dose2 <= input$BothAge2_dose1,
            "This number can't be greater than the total number of patients who received the first dose"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_11_mening <- renderText({
      if (is.na(input$BothAge2_mening) | is.na(input$BothAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothAge2_mening <= input$BothAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_11_Tdap <- renderText({
      if (is.na(input$BothAge2_tdap) | is.na(input$BothAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothAge2_tdap <= input$BothAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  
  
  # Aged 13 warnings ------------------------------- #
  
  # Females
  observe({
    output$error_f13_total <- renderText({
      validate(need(input$FemAge3_total, "This number is required"),
               errorClass = "red_warnings") # require responses for the female rates
    })
  })
  observe({
    output$error_f13_gt1 <- renderText({
      if (is.na(input$FemAge3_dose1) | is.na(input$FemAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemAge3_dose1 <= input$FemAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_f13_2plus <- renderText({
      if (is.na(input$FemAge3_dose2) | is.na(input$FemAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemAge3_dose2 <= input$FemAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_f13a_2plus <- renderText({
      if (is.na(input$FemAge3_dose2) | is.na(input$FemAge3_dose1)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemAge3_dose2 <= input$FemAge3_dose1,
            "This number can't be greater than the total number of patients who received the first dose"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_f13_mening <- renderText({
      if (is.na(input$FemAge3_mening) | is.na(input$FemAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemAge3_mening <= input$FemAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_f13_Tdap <- renderText({
      if (is.na(input$FemAge3_tdap) | is.na(input$FemAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemAge3_tdap <= input$FemAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  
  # Males
  observe({
    output$error_m13_total <- renderText({
      validate(need(input$MenAge3_total, "This number is required"),
               errorClass = "red_warnings") # require responses for the male rates
    })
  })
  observe({
    output$error_m13_gt1 <- renderText({
      if (is.na(input$MenAge3_dose1) | is.na(input$MenAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenAge3_dose1 <= input$MenAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_m13_2plus <- renderText({
      if (is.na(input$MenAge3_dose2) | is.na(input$MenAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenAge3_dose2 <= input$MenAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_m13a_2plus <- renderText({
      if (is.na(input$MenAge3_dose2) | is.na(input$MenAge3_dose1)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenAge3_dose2 <= input$MenAge3_dose1,
            "This number can't be greater than the total number of patients who received the first dose"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_m13_mening <- renderText({
      if (is.na(input$MenAge3_mening) | is.na(input$MenAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenAge3_mening <= input$MenAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_m13_Tdap <- renderText({
      if (is.na(input$MenAge3_tdap) | is.na(input$MenAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenAge3_tdap <= input$MenAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  
  # Combined (males and females)
  observe({
    output$error_13_total <- renderText({
      validate(need(input$BothAge3_total, "This number is required"),
               errorClass = "red_warnings") # require responses for the male rates
    })
  })
  observe({
    output$error_13_gt1 <- renderText({
      if (is.na(input$BothAge3_dose1) | is.na(input$BothAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothAge3_dose1 <= input$BothAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_13_2plus <- renderText({
      if (is.na(input$BothAge3_dose2) | is.na(input$BothAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothAge3_dose2 <= input$BothAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_13a_2plus <- renderText({
      if (is.na(input$BothAge3_dose2) | is.na(input$BothAge3_dose1)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothAge3_dose2 <= input$BothAge3_dose1,
            "This number can't be greater than the total number of patients who received the first dose"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_13_mening <- renderText({
      if (is.na(input$BothAge3_mening) | is.na(input$BothAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothAge3_mening <= input$BothAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_13_Tdap <- renderText({
      if (is.na(input$BothAge3_tdap) | is.na(input$BothAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothAge3_tdap <= input$BothAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  
  })
  
  # WHEN HIT BUTTON, SAVE DATA AND MOVE TO FINAL PAGE
  observeEvent(input$button_rates, {
    
    updateTabsetPanel(session, "tabbox_baseline", selected = "p1_plan")
  })
  

  
  
  
  # Project activity plan ---------------------------------------------------
  savedActivities <- myData$savedActivities
  
  output$SelectActivitiesUser <- renderUI({
    selectInput(inputId = "SelectActivitiesUser",
                label = "Select health system account",
                choices = unique(savedActivities$Username),
                selected = savedActivities$Username[1])
  })
  
  observe({
    validate(need(input$SelectActivitiesUser, ""))
    savedActivities <- filter(myData$savedActivities, Username == input$SelectActivitiesUser)
    
    
    output$date_activity <- renderUI({
      h4(paste("This health system's data was most recently updated on ", (savedActivities$activitiesDate)))
    })  
  
  
  foo35 <- with(savedActivities, c(Q35_1, Q35_2, Q35_3, Q35_4))
  foo35 <- foo35[!is.na(foo35)]  
  
  foo37 <- with(savedActivities, c(Q37_1, Q37_2, Q37_3, Q37_4))
  foo37 <- foo37[!is.na(foo37)]
  
  
  foo38 <- with(savedActivities, c(Q38_1, Q38_2, Q38_3, Q38_4, Q38_5, Q38_6, Q38_7, Q38_8, Q38_9))
  foo38 <- foo38[!is.na(foo38)]
  
  foo39 <- with(savedActivities, c(Q39_1, Q39_2, Q39_3, Q39_4, Q39_5))
  foo39 <- foo39[!is.na(foo39)]
  
  
  # Q35 - checkbox group input
  output$Q35 <- renderUI({
    checkboxGroupInput(
      inputId = "Q35",
      label = "35.  What other data do we need to obtain before establishing our goal?",
      choices = c("Provider-level rates", "Clinic-level rates", "We have all the data we need", "Other (specify)"),
      selected = foo35,
      width = '100%'
    )
  })
  output$Q35_other <- renderUI({
    validate(need(input$Q35, ""))
    if ("Other (specify)" %in% input$Q35) {
      textInput("Q35_other",
                label = "Please specify",
                value = savedActivities$Q35_other,
                width = "100%")
    }
  })
  
  output$Q36a <- renderUI({
    numericInput(
      inputId = "Q36a",
      label = "36a.  What is our specific numerical goal?",
      value =  savedActivities$Q36a,
      width = '100%')
  })
  output$Q36b <- renderUI({
    textInput(
      inputId = "Q36b",
      label = "36b.  What is our target population?",
      value = savedActivities$Q36b,
      width = '100%')
  })
  output$Q36c <- renderUI({
    textInput(
      inputId = "Q36c",
      label = "36c.  Where will we do it?",
      value = savedActivities$Q36c,
      width = '100%')
  })
  output$Q36d <- renderUI({
    textInput(inputId = "Q36d",
              label = "36d.  What is the time period to achieve this aim?",
              value = savedActivities$Q36d,
              width = '100%')
  })
  output$Q36e <- renderUI({
    textAreaInput(inputId = "Q36e",
                  label = "36e.  Why is it important to do this now?",
                  value = savedActivities$Q36e,
                  rows = 4)
  })
  output$Q36f <- renderUI({
    textAreaInput(inputId = "Q36f",
                  label = "We aim to...",
                  value = savedActivities$Q36f,
                  rows = 5)
  })
  
  # Q37 - checkbox group
  output$Q37 <- renderUI({
    checkboxGroupInput(inputId = "Q37",
                       label = p("37.  Which of the following HPV vaccination training and education activities will we conduct?",
                                 em(" (check all that apply)")),
                       choices = c("Educate staff on HPV vaccination as cancer prevention",
                                   "Educate staff on strategies to improve HPV vaccination rates",
                                   "Identify HPV vaccination champions",
                                   "Train providers to make an effective HPV vaccine recommendation"),
                       selected = foo37,
                       width = "250%")
  })
  
  # Q38 - checkbox group
  output$Q38 <- renderUI({
    checkboxGroupInput(
      inputId = "Q38",
      label = p("38.  Which of the following interventions to increase HPV vaccination will we implement?",
                em(" (check all that apply)")),
      choices = c("Client reminders",
                  "Extended hours",
                  "Modified EHR",
                  "Offer in alternative settings, like schools or mobile units",
                  "Parent/patient education",
                  "Provider assessment & feedback",
                  "Provider prompts/reminders",
                  "Standing orders",
                  "Other (specify)"),
      selected = foo38,
      width = "250%")
  })
  output$Q38_other <- renderUI({
    validate(need(input$Q38, ""))
    if ("Other (specify)" %in% input$Q38) {
      textInput("Q38_other",
                label = "Please specify",
                value = savedActivities$Q38_other,
                width = "100%")
    }
  })
  
  # Q39 - checkbox group
  output$Q39 <- renderUI({
    checkboxGroupInput(
      inputId = "Q39",
      label = p("39.  What forms of support would the health system like from the ACS staff partner?",
                em(" (check all that apply)")),
      choices = c("Member of HPV QI project team",
                  "Provide input on strategy",
                  "Provide materials",
                  "Provide technical assistance",
                  "Other (specify)"),
      width =  '250%',
      selected = foo39)
    
  })
  output$Q39_other <- renderUI({
    validate(need(input$Q39, ""))
    if ("Other (specify)" %in% input$Q39) {
      textInput("Q39_other",
                label = "Please specify",
                value = savedActivities$Q39_other,
                width = "100%")
    }
  })
  
  output$Q40 <- renderUI({
    textAreaInput(
      inputId = "Q40",
      label = "40.  How will your system communicate with providers, staff, and leadership about project kickoff, goal, and updates?",
      rows = 4,
      value <- savedActivities$Q40)
  })
  output$Q41 <- renderUI({
    textAreaInput(
      inputId = "Q41",
      label = "41.  How will your system build and maintain excitement and momentum for the project?",
      rows = 4,
      value = savedActivities$Q41)
  })
  output$Q42 <- renderUI({
    textAreaInput(
      inputId = "Q42",
      label = "42.  How will your system celebrate and share success?",
      rows = 4,
      value = savedActivities$Q42)
  })
  
  # Activity matrix
  output$act1 <- renderUI({
    textInput("act1", strong("Activity"), value = savedActivities$act1)
  })
  output$time1 <- renderUI({
    textInput("time1", strong("Timeline"), value = savedActivities$time1)
  })
  output$ppl1 <- renderUI({
    textInput("ppl1", strong("Person/People Responsible"), value = savedActivities$ppl1)
  })
  
  output$act2 <- renderUI({
    textInput("act2", label = NULL, value = savedActivities$act2)
  })
  output$time2 <- renderUI({
    textInput("time2", label = NULL, value = savedActivities$time2)
  })
  output$ppl2 <- renderUI({
    textInput("ppl2", label = NULL, value = savedActivities$ppl2)
  })
  
  output$act3 <- renderUI({
    textInput("act3", label = NULL, value = savedActivities$act3)
  })
  output$time3 <- renderUI({
    textInput("time3", label = NULL, value = savedActivities$time3)
  })
  output$ppl3 <- renderUI({
    textInput("ppl3", label = NULL, value = savedActivities$ppl3)
  })
  
  output$act4 <- renderUI({
    textInput("act4", label = NULL, value = savedActivities$act4)
  })
  output$time4 <- renderUI({
    textInput("time4", label = NULL, value = savedActivities$time4)
  })
  output$ppl4 <- renderUI({
    textInput("ppl4", label = NULL, value = savedActivities$ppl4)
  })
  
  output$act5 <- renderUI({
    textInput("act5", label = NULL, value = savedActivities$act5)
  })
  output$time5 <- renderUI({
    textInput("time5", label = NULL, value = savedActivities$time5)
  })
  output$ppl5 <- renderUI({
    textInput("ppl5", label = NULL, value = savedActivities$ppl5)
  })
  
  output$act6 <- renderUI({
    textInput("act6", label = NULL, value = savedActivities$act6)
  })
  output$time6 <- renderUI({
    textInput("time6", label = NULL, value = savedActivities$time6)
  })
  output$ppl6 <- renderUI({
    textInput("ppl6", label = NULL, value = savedActivities$ppl6)
  })
  
  output$act7 <- renderUI({
    textInput("act7", label = NULL, value = savedActivities$act7)
  })
  output$time7 <- renderUI({
    textInput("time7", label = NULL, value = savedActivities$time7)
  })
  output$ppl7 <- renderUI({
    textInput("ppl7", label = NULL, value = savedActivities$ppl7)
  })
  
  output$act8 <- renderUI({
    textInput("act8", label = NULL, value = savedActivities$act8)
  })
  output$time8 <- renderUI({
    textInput("time8", label = NULL, value = savedActivities$time8)
  })
  output$ppl8 <- renderUI({
    textInput("ppl8", label = NULL, value = savedActivities$ppl8)
  })
  
  output$act9 <- renderUI({
    textInput("act9", label = NULL, value = savedActivities$act9)
  })
  output$time9 <- renderUI({
    textInput("time9", label = NULL, value = savedActivities$time9)
  })
  output$ppl9 <- renderUI({
    textInput("ppl9", label = NULL, value = savedActivities$ppl9)
  })
  
  output$act10 <- renderUI({
    textInput("act10", label = NULL, value = savedActivities$act10)
  })
  output$time10 <- renderUI({
    textInput("time10", label = NULL, value = savedActivities$time10)
  })
  output$ppl10 <- renderUI({
    textInput("ppl10", label = NULL, value = savedActivities$ppl10)
  })
  
  })  
  
  # Gather and save the data
  observeEvent(input$button_activities, {
    updateTabsetPanel(session, "tabbox_baseline", selected = "p1_submission")
  })
 
  
  ###### ------------------------------ FINAL UPDATES TABS ----------------------------- #######
  
  
  followupRates <- myData$followupRates
  output$SelectFURatesUser <- renderUI({
    selectInput(inputId = "SelectFURatesUser",
                label = "Select health system account",
                choices = unique(followupRates$Username),
                selected = followupRates$Username[1])
  })
  
  observe({
    validate(need(input$SelectFURatesUser, ""))
    
    followupRates <- filter(myData$followupRates, Username == input$SelectFURatesUser)
    
    
    output$date_fwup_rates <- renderUI({
      h4(paste("This health system's data was most recently updated on ", (followupRates$fwupratesDate)))
    })
    
  
  foo2FU <- with(followupRates, c(Q2FU_1, Q2FU_2, Q2FU_3, Q2FU_4))
  foo2FU <- foo2FU[!is.na(foo2FU)]
  
  
  output$Q1FU <- renderUI({
    selectInput(
      inputId = "Q1FU",
      label = "1.  How are you defining your active patient populations?",
      choices = c("",
                  "12 months",
                  "18 months",
                  "24 months",
                  "Other (please specify)"),
      selected = followupRates$Q1FU,
      selectize = F)
  })
  output$Q1FU_other <- renderUI({
    validate(need(input$Q1FU, ""))
    if (input$Q1FU != "Other (please specify)") {
      return(NULL)
    } else {
      textInput(
        inputId = "Q1FU_other",
        label = "Please specify",
        value = followupRates$Q1FU_other,
        placeholder = "",
        width = "150%"
      )
    }
  })
  output$Q2FU <- renderUI({
    checkboxGroupInput(
      inputId = "Q2FU",
      label = "2. For what ages are you reporting?",
      choices = c("9-10",
                  "11-12",
                  "13",
                  "We can't report on these ages (other, please specify)?"),
      selected = foo2FU)
  })
  output$Q2FU_other <- renderUI({
    validate(need(input$Q2FU, ""))
    if (!"We can't report on these ages (other, please specify)?" %in% input$Q2FU) {
      return(NULL)
    } else {
      textInput(
        inputId = "Q2FU_other",
        label = "Please specify",
        value = followupRates$Q2FU_other,
        placeholder = "",
        width = "150%"
      )
    }
  })    
  output$Q3FU <- renderUI({
    selectInput(
      inputId = "Q3FU",
      label = "3.  Can you pull rate data by sex?",
      choices = c("",
                  "Yes, we'll report separate data for male and female patients",
                  "No, we will have combined rate data"),
      selected = followupRates$Q3FU)
  })
  output$Q4FU <- renderUI({
    textAreaInput(
      inputId = "Q4FU",
      label = "4.  Please share anything else about your final data you'd like us to know.",
      value = followupRates$Q4FU,
      rows = 4)
  })
  
  # Create the ages 9-10 boxes
  observeEvent(list(input$Q2FU, input$Q3FU), {
    validate(need(input$Q2FU, ""))
    validate(need(input$Q3FU, ""))
    
    # Boys and girls - ages 9-10
    if ("9-10" %in% input$Q2FU &
        input$Q3FU == "Yes, we'll report separate data for male and female patients") {
      
      # Girls ages 9-10 rates 
      output$furates_9_10 <- renderUI({
        verticalLayout(
          splitLayout(
            box(
              h3("Vaccination Rates for Female Patients, Ages 9-10"),
              title = "Vaccination Rates for Female Patients, Ages 9-10",
              collapsible = T,
              solidHeader = T,
              status = "primary",
              collapsed = T,
              width = '100%',
              HTML("<br>"),
              h5(
                strong(
                  "Total number of active ",
                  strong("female ", style = "color:blue"),
                  "medical patients ",
                  em("ages 9-10 ", style = "color:red"),
                  "your clinic saw in 2021?"
                )
              ),
              numericInput(
                "FemFUAge1_total",
                label = NULL,
                value = followupRates$FemFUAge1_total,
                min = 0,
                step = 1,
                width = '50%'
              ),
              textOutput("error_f9_total_fwup"),
              h5(
                strong(
                  "Number of active patients who received at least one (1) HPV dose"
                )
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemFUAge1_dose1",
                  label = NULL,
                  min = 0,
                  value = followupRates$FemFUAge1_dose1,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemFUAge1_dose1_rate")
              ),
              textOutput("error_f9_gt1_fwup"),
              h5(
                strong("Number of active patients who received both (2) HPV doses")
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemFUAge1_dose2",
                  label = NULL,
                  min = 0,
                  value = followupRates$FemFUAge1_dose2,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemFUAge1_dose2_rate")
              ),
              textOutput("error_f9_2plus_fwup_fwup"),
              textOutput("error_f9a_2plus_fwup_fwup")
            ),
            box( #boys aged 9-10
              h3("Males, ages 9-10"),
              title = "Males, ages 9-10",
              collapsible = T,
              solidHeader = T,
              status = "danger",
              collapsed = T,
              width = '100%',
              HTML("<br>"),
              h5(
                strong(
                  "Total number of active ",
                  strong("male ", style = "color:blue"),
                  "medical patients ",
                  em("ages 9-10 ", style = "color:red"),
                  "your clinic saw in 2021?"
                )
              ),
              numericInput(
                "MenFUAge1_total",
                label = NULL,
                value = followupRates$MenFUAge1_total,
                min = 0,
                step = 1,
                width = '50%'
              ),
              textOutput("error_m9_total_fwup"),
              h5(
                strong(
                  "Number of active patients who received at least one (1) HPV dose"
                )
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenFUAge1_dose1",
                  label = NULL,
                  min = 0,
                  value =  followupRates$MenFUAge1_dose1,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenFUAge1_dose1_rate")
              ),
              textOutput("error_m9_gt1_fwup"),
              h5(
                strong("Number of active patients who received both (2) HPV doses")
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenFUAge1_dose2",
                  label = NULL,
                  min = 0,
                  value =  followupRates$MenFUAge1_dose2,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenFUAge1_dose2_rate")
              ),
              textOutput("error_m9_2plus_fwup"),
              textOutput("error_m9a_2plus_fwup")
            )
          )) # end split and vertical layout
      })
      output$FemFUAge1_dose1_rate <- renderText({
        validate(need(input$FemFUAge1_total, ""))
        if (is.na(input$FemFUAge1_dose1)) {
          return(NULL)
        }
        if (!is.na(input$FemFUAge1_dose1)) {
          rate <- input$FemFUAge1_dose1 / input$FemFUAge1_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group.")
          return(rate)
        }
      })
      output$FemFUAge1_dose2_rate <- renderText({
        validate(need(input$FemFUAge1_total, ""))
        if (is.na(input$FemFUAge1_dose2)) {
          return(NULL)
        }
        if (!is.na(input$FemFUAge1_dose2)) {
          rate <- input$FemFUAge1_dose2 / input$FemFUAge1_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group.")
          return(rate)
        }
      })
      observe({
        output$error_f9_total_fwup <- renderText({
          validate(need(input$FemFUAge1_total, "This number is required"),
                   errorClass = "red_warnings")
        })
      })
      
      
      output$MenFUAge1_dose1_rate <- renderText({
        validate(need(input$MenFUAge1_total, ""))
        if (is.na(input$MenFUAge1_dose1)) {
          return(NULL)
        }
        if (!is.na(input$MenFUAge1_dose1)) {
          rate <- input$MenFUAge1_dose1 / input$MenFUAge1_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group.")
          return(rate)
        }
      })
      output$MenFUAge1_dose2_rate <- renderText({
        validate(need(input$MenFUAge1_total, ""))
        if (is.na(input$MenFUAge1_dose2)) {
          return(NULL)
        }
        if (!is.na(input$MenFUAge1_dose2)) {
          rate <- input$MenFUAge1_dose2 / input$MenFUAge1_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group.")
          return(rate)
        }
      })
    }
    
    # Ages 9-10, combined
    if ("9-10" %in% input$Q2FU &
        input$Q3FU == "No, we will have combined rate data") {
      # Create the 'male and females combined' box
      output$furates_9_10 <- renderUI({
        box(
          h3("Vaccination Rates for Male and Female Patients Combined, Ages 9-10"),
          title = "Vaccination Rates for Male and Female Patients Combined, Ages 9-10",
          collapsible = T,
          solidHeader = T,
          status = "success",
          collapsed = T,
          width = '100%',
          HTML("<br>"),
          h5(
            strong(
              "Total number of active medical patients ",
              em("ages 9-10 ", style = "color:red"),
              "your clinic saw in 2021?"
            )
          ),
          numericInput(
            "BothFUAge1_total",
            label = NULL,
            value = followupRates$BothFUAge1_total,
            min = 0,
            step = 1,
            width = '50%'
          ),
          textOutput("error_9_total_fwup"),
          h5(
            strong(
              "Number of active patients who received at least one (1) HPV dose"
            )
          ),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothFUAge1_dose1",
              label = NULL,
              min = 0,
              value = followupRates$BothFUAge1_dose1,
              step = 1,
              width = '50%'
            ),
            textOutput("BothFUAge1_dose1_rate")
          ),
          textOutput("error_9_gt1_fwup"),
          h5(
            strong("Number of active patients who received both (2) HPV doses")
          ),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(inputId = "BothFUAge1_dose2",
                         label = NULL,
                         min = 0,
                         value = followupRates$BothFUAge1_dose2,
                         step = 1,
                         width = '50%'
            ),
            textOutput("BothFUAge1_dose2_rate")
          ),
          textOutput("error_9_2plus_fwup"),
          textOutput("error_9a_2plus_fwup")
        )
      })
      output$BothFUAge1_dose1_rate <- renderText({
        validate(need(input$BothFUAge1_total, ""))
        if (is.na(input$BothFUAge1_dose1)) {
          return(NULL)
        }
        if (!is.na(input$BothFUAge1_dose1)) {
          rate <- input$BothFUAge1_dose1 / input$BothFUAge1_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group.")
          return(rate)
        }
      })
      output$BothFUAge1_dose2_rate <- renderText({
        validate(need(input$BothFUAge1_total, ""))
        if (is.na(input$BothFUAge1_dose2)) {
          return(NULL)
        }
        if (!is.na(input$BothFUAge1_dose2)) {
          rate <- input$BothFUAge1_dose2 / input$BothFUAge1_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group.")
          return(rate)
        }
      })
    }
    
  }) # end the 9-10 observation
  
  # Create the ages 11-12 boxes
  observeEvent(list(input$Q2FU, input$Q3FU), {
    validate(need(input$Q2FU, ""))
    validate(need(input$Q3FU, ""))
    
    if ("11-12" %in% input$Q2FU &
        input$Q3FU == "Yes, we'll report separate data for male and female patients") {
      
      # Girls ages 11-12
      output$furates_11_12 <- renderUI({
        verticalLayout(
          splitLayout(
            box(
              h3("Vaccination Rates for Female Patients, Ages 11-12"),
              title = "Vaccination Rates for Female Patients, Ages 11-12",
              collapsible = T,
              solidHeader = T,
              status = "primary",
              collapsed = T,
              width = '100%',
              HTML("<br>"),
              h5(
                strong(
                  "Total number of active ",
                  strong("female ", style = "color:blue"),
                  "medical patients ",
                  em("ages 11-12 ", style = "color:red"),
                  "your clinic saw in 2021?"
                )
              ),
              numericInput(
                "FemFUAge2_total",
                label = NULL,
                value = followupRates$FemFUAge2_total,
                min = 0,
                step = 1,
                width = '100%'
              ),
              textOutput("error_f11_total_fwup"),
              h5(
                strong(
                  "Number of active patients who received at least one (1) HPV dose"
                )
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemFUAge2_dose1",
                  label = NULL,
                  min = 0,
                  value = followupRates$FemFUAge2_dose1,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemFUAge2_dose1_rate")
              ),
              textOutput("error_f11_gt1_fwup"),
              h5(
                strong("Number of active patients who received both (2) HPV doses")
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemFUAge2_dose2",
                  label = NULL,
                  min = 0,
                  value = followupRates$FemFUAge2_dose1,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemFUAge2_dose2_rate")
              ),
              textOutput("error_f11_2plus_fwup"),
              textOutput("error_f11a_2plus_fwup"),
              h5(strong("Meningococcal")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemFUAge2_mening",
                  label = NULL,
                  min = 0,
                  value = followupRates$FemFUAge2_mening,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemFUAge2_mening_rate")
              ),
              textOutput("error_f11_mening_fwup"),
              h5(strong("Tdap")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemFUAge2_tdap",
                  label = NULL,
                  min = 0,
                  value = followupRates$FemFUAge2_tdap,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemFUAge2_tdap_rate")
              ),
              textOutput("error_f11_Tdap_fwup"),
            ),
            box( # boys aged 11-12
              h3("Males, ages 11-12"),
              title = "Males, ages 11-12",
              collapsible = T,
              solidHeader = T,
              status = "danger",
              collapsed = T,
              width = '100%',
              HTML("<br>"),
              h5(
                strong(
                  "Total number of active ",
                  strong("male ", style = "color:blue"),
                  "medical patients ",
                  em("ages 11-12 ", style = "color:red"),
                  "your clinic saw in 2021?"
                )
              ),
              numericInput(
                "MenFUAge2_total",
                label = NULL,
                value = followupRates$MenFUAge2_total,
                min = 0,
                step = 1,
                width = '100%'
              ),
              textOutput("error_m11_total_fwup"),
              h5(
                strong(
                  "Number of active patients who received at least one (1) HPV dose"
                )
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenFUAge2_dose1",
                  label = NULL,
                  min = 0,
                  value = followupRates$MenFUAge2_dose1,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenFUAge2_dose1_rate")
              ),
              textOutput("error_m11_gt1_fwup"),
              h5(
                strong("Number of active patients who received both (2) HPV doses")
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenFUAge2_dose2",
                  label = NULL,
                  min = 0,
                  value = followupRates$MenFUAge2_dose2,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenFUAge2_dose2_rate")
              ),
              textOutput("error_m11_2plus_fwup"),
              textOutput("error_m11a_2plus_fwup"),
              h5(strong("Meningococcal")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenFUAge2_mening",
                  label = NULL,
                  min = 0,
                  value = followupRates$MenFUAge2_mening,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenFUAge2_mening_rate")
              ),
              textOutput("error_m11_mening_fwup"),
              h5(strong("Tdap")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenFUAge2_tdap",
                  label = NULL,
                  min = 0,
                  value = followupRates$MenFUAge2_tdap,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenFUAge2_tdap_rate")
              ),
              textOutput("error_m11_Tdap_fwup"),
            )
          )) # end split and vertical layout
      })
      output$FemFUAge2_dose1_rate <- renderText({
        validate(need(input$FemFUAge2_total, ""))
        if (is.na(input$FemFUAge2_dose1)) {
          return(NULL)
        }
        if (!is.na(input$FemFUAge2_dose1)) {
          rate <- input$FemFUAge2_dose1 / input$FemFUAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$FemFUAge2_dose2_rate <- renderText({
        validate(need(input$FemFUAge2_total, ""))
        if (is.na(input$FemFUAge2_dose2)) {
          return(NULL)
        }
        if (!is.na(input$FemFUAge2_dose2)) {
          rate <- input$FemFUAge2_dose2 / input$FemFUAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$FemFUAge2_mening_rate <- renderText({
        validate(need(input$FemFUAge2_total, ""))
        if (is.na(input$FemFUAge2_mening)) {
          return(NULL)
        }
        if (!is.na(input$FemFUAge2_mening)) {
          rate <- input$FemFUAge2_mening / input$FemFUAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$FemFUAge2_tdap_rate <- renderText({
        validate(need(input$FemFUAge2_total, ""))
        if (is.na(input$FemFUAge2_tdap)) {
          return(NULL)
        }
        if (!is.na(input$FemFUAge2_tdap)) {
          rate <- input$FemFUAge2_tdap / input$FemFUAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      
      
      
      output$MenFUAge2_dose1_rate <- renderText({
        validate(need(input$MenFUAge2_total, ""))
        if (is.na(input$MenFUAge2_dose1)) {
          return(NULL)
        }
        if (!is.na(input$MenFUAge2_dose1)) {
          rate <- input$MenFUAge2_dose1 / input$MenFUAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$MenFUAge2_dose2_rate <- renderText({
        validate(need(input$MenFUAge2_total, ""))
        if (is.na(input$MenFUAge2_dose2)) {
          return(NULL)
        }
        if (!is.na(input$MenFUAge2_dose2)) {
          rate <- input$MenFUAge2_dose2 / input$MenFUAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$MenFUAge2_mening_rate <- renderText({
        validate(need(input$MenFUAge2_total, ""))
        if (is.na(input$MenFUAge2_mening)) {
          return(NULL)
        }
        if (!is.na(input$MenFUAge2_mening)) {
          rate <- input$MenFUAge2_mening / input$MenFUAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$MenFUAge2_tdap_rate <- renderText({
        validate(need(input$MenFUAge2_total, ""))
        if (is.na(input$MenFUAge2_tdap)) {
          return(NULL)
        }
        if (!is.na(input$MenFUAge2_tdap)) {
          rate <- input$MenFUAge2_tdap / input$MenFUAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      
    }
    # Ages 11-12 - boys and girls combined
    if ("11-12" %in% input$Q2FU &
        input$Q3FU == "No, we will have combined rate data") {
      output$furates_11_12 <- renderUI({
        box(
          h3("Vaccination Rates for Male and Female Patients Combined, Ages 11-12"),
          title = "Vaccination Rates for Male and Female Patients Combined, Ages 11-12",
          collapsible = T,
          solidHeader = T,
          status = "success",
          collapsed = T,
          width = '100%',
          HTML("<br>"),
          h5(
            strong(
              "Total number of active medical patients ",
              em("ages 11-12 ", style = "color:red"),
              "your clinic saw in 2021?"
            )
          ),
          numericInput(
            "BothFUAge2_total",
            label = NULL,
            value = followupRates$BothFUAge2_total,
            min = 0,
            step = 1,
            width = '100%'
          ),
          textOutput("error_11_total_fwup"),
          h5(
            strong(
              "Number of active patients who received at least one (1) HPV dose"
            )
          ),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothFUAge2_dose1",
              label = NULL,
              min = 0,
              value = followupRates$BothFUAge2_dose1,
              step = 1,
              width = '50%'
            ),
            textOutput("BothFUAge2_dose1_rate")
          ),
          textOutput("error_11_gt1_fwup"),
          h5(
            strong("Number of active patients who received both (2) HPV doses")
          ),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothFUAge2_dose2",
              label = NULL,
              min = 0,
              value = followupRates$BothFUAge2_dose2,
              step = 1,
              width = '50%'
            ),
            textOutput("BothFUAge2_dose2_rate")
          ),
          textOutput("error_11_2plus_fwup"),
          textOutput("error_11a_2plus_fwup"),
          h5(strong("Meningococcal")),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothFUAge2_mening",
              label = NULL,
              min = 0,
              value = followupRates$BothFUAge2_mening,
              step = 1,
              width = '50%'
            ),
            textOutput("BothFUAge2_mening_rate")
          ),
          textOutput("error_11_mening_fwup"),
          h5(strong("Tdap")),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothFUAge2_tdap",
              label = NULL,
              min = 0,
              value = followupRates$BothFUAge2_tdap,
              step = 1,
              width = '50%'
            ),
            textOutput("BothFUAge2_tdap_rate")
          ),
          textOutput("error_11_Tdap_fwup"),
        )
      })
      output$BothFUAge2_dose1_rate <- renderText({
        validate(need(input$BothFUAge2_total, ""))
        if (is.na(input$BothFUAge2_dose1)) {
          return(NULL)
        }
        if (!is.na(input$BothFUAge2_dose1)) {
          rate <- input$BothFUAge2_dose1 / input$BothFUAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$BothFUAge2_dose2_rate <- renderText({
        validate(need(input$BothFUAge2_total, ""))
        if (is.na(input$BothFUAge2_dose2)) {
          return(NULL)
        }
        if (!is.na(input$BothFUAge2_dose2)) {
          rate <- input$BothFUAge2_dose2 / input$BothFUAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$BothFUAge2_mening_rate <- renderText({
        validate(need(input$BothFUAge2_total, ""))
        if (is.na(input$BothFUAge2_mening)) {
          return(NULL)
        }
        if (!is.na(input$BothFUAge2_mening)) {
          rate <- input$BothFUAge2_mening / input$BothFUAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$BothFUAge2_tdap_rate <- renderText({
        validate(need(input$BothFUAge2_total, ""))
        if (is.na(input$BothFUAge2_tdap)) {
          return(NULL)
        }
        if (!is.na(input$BothFUAge2_tdap)) {
          rate <- input$BothFUAge2_tdap / input$BothFUAge2_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
    }
  }) # end the 11-12 observation
  
  
  # Age 13 boxes
  observeEvent(list(input$Q2FU, input$Q3FU), {
    validate(need(input$Q2FU, ""))
    validate(need(input$Q3FU, ""))
    
    if ("13" %in% input$Q2FU &
        input$Q3FU == "Yes, we'll report separate data for male and female patients") {
      
      # Girls age 13
      output$furates_13 <- renderUI({
        verticalLayout(
          splitLayout(
            box(
              h3("Vaccination Rates for Female Patients, Age 13"),
              title = "Vaccination Rates for Female Patients, Age 13",
              collapsible = T,
              solidHeader = T,
              status = "primary",
              collapsed = T,
              width = '100%',
              HTML("<br>"),
              h5(
                strong(
                  "Total number of active ",
                  strong("female ", style = "color:blue"),
                  "medical patients ",
                  em("age 13 ", style = "color:red"),
                  "your clinic saw in 2021?"
                )
              ),
              numericInput(
                "FemFUAge3_total",
                label = NULL,
                value = followupRates$FemFUAge3_total,
                min = 0,
                step = 1,
                width = '100%'
              ),
              textOutput("error_f13_total_fwup"),
              h5(
                strong(
                  "Number of active patients who received at least one (1) HPV dose"
                )
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemFUAge3_dose1",
                  label = NULL,
                  min = 0,
                  value = followupRates$FemFUAge3_dose1,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemFUAge3_dose1_rate")
              ),
              textOutput("error_f13_gt1_fwup"),
              h5(
                strong("Number of active patients who received both (2) HPV doses")
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemFUAge3_dose2",
                  label = NULL,
                  min = 0,
                  value = followupRates$FemFUAge3_dose2,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemFUAge3_dose2_rate")
              ),
              textOutput("error_f13_2plus_fwup"),
              textOutput("error_f13a_2plus_fwup"),
              h5(strong("Meningococcal")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemFUAge3_mening",
                  label = NULL,
                  min = 0,
                  value = followupRates$FemFUAge3_mening,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemFUAge3_mening_rate")
              ),
              textOutput("error_f13_mening_fwup"),
              h5(strong("Tdap")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "FemFUAge3_tdap",
                  label = NULL,
                  min = 0,
                  value = followupRates$FemFUAge3_tdap,
                  step = 1,
                  width = '50%'
                ),
                textOutput("FemFUAge3_tdap_rate")
              ),
              textOutput("error_f13_Tdap_fwup"),
            ),
            box( # boys aged 13
              h3("Males, age 13"),
              title = "Males, age 13",
              collapsible = T,
              solidHeader = T,
              status = "danger",
              collapsed = T,
              width = '100%',
              HTML("<br>"),
              h5(
                strong(
                  "Total number of active ",
                  strong("male ", style = "color:blue"),
                  "medical patients ",
                  em("age 13 ", style = "color:red"),
                  "your clinic saw in 2021?"
                )
              ),
              numericInput(
                "MenFUAge3_total",
                label = NULL,
                value = followupRates$MenFUAge3_total,
                min = 0,
                step = 1,
                width = '100%'
              ),
              textOutput("error_m13_total_fwup"),
              h5(
                strong(
                  "Number of active patients who received at least one (1) HPV dose"
                )
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenFUAge3_dose1",
                  label = NULL,
                  min = 0,
                  value = followupRates$MenFUAge3_total,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenFUAge3_dose1_rate")
              ),
              textOutput("error_m13_gt1_fwup"),
              h5(
                strong("Number of active patients who received both (2) HPV doses")
              ),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenFUAge3_dose2",
                  label = NULL,
                  min = 0,
                  value = followupRates$MenFUAge3_dose2,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenFUAge3_dose2_rate")
              ),
              textOutput("error_m13_2plus_fwup"),
              textOutput("error_m13a_2plus_fwup"),
              h5(strong("Meningococcal")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenFUAge3_mening",
                  label = NULL,
                  min = 0,
                  value = followupRates$MenFUAge3_mening,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenFUAge3_mening_rate")
              ),
              textOutput("error_m13_mening_fwup"),
              h5(strong("Tdap")),
              splitLayout(
                cellWidths = c("30%", "70%"),
                numericInput(
                  "MenFUAge3_tdap",
                  label = NULL,
                  min = 0,
                  value = followupRates$MenFUAge3_tdap,
                  step = 1,
                  width = '50%'
                ),
                textOutput("MenFUAge3_tdap_rate")
              ),
              textOutput("error_m13_Tdap_fwup"),
            )
          )) # end split and vertical layout
      })
      output$FemFUAge3_dose1_rate <- renderText({
        validate(need(input$FemFUAge3_total, ""))
        if (is.na(input$FemFUAge3_dose1)) {
          return(NULL)
        }
        if (!is.na(input$FemFUAge3_dose1)) {
          rate <- input$FemFUAge3_dose1 / input$FemFUAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$FemFUAge3_dose2_rate <- renderText({
        validate(need(input$FemFUAge3_total, ""))
        if (is.na(input$FemFUAge3_dose2)) {
          return(NULL)
        }
        if (!is.na(input$FemFUAge3_dose2)) {
          rate <- input$FemFUAge3_dose2 / input$FemFUAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$FemFUAge3_mening_rate <- renderText({
        validate(need(input$FemFUAge3_total, ""))
        if (is.na(input$FemFUAge3_mening)) {
          return(NULL)
        }
        if (!is.na(input$FemFUAge3_mening)) {
          rate <- input$FemFUAge3_mening / input$FemFUAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$FemFUAge3_tdap_rate <- renderText({
        validate(need(input$FemFUAge3_total, ""))
        if (is.na(input$FemFUAge3_tdap)) {
          return(NULL)
        }
        if (!is.na(input$FemFUAge3_tdap)) {
          rate <- input$FemFUAge3_tdap / input$FemFUAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      
      
      output$MenFUAge3_dose1_rate <- renderText({
        validate(need(input$MenFUAge3_total, ""))
        if (is.na(input$MenFUAge3_dose1)) {
          return(NULL)
        }
        if (!is.na(input$MenFUAge3_dose1)) {
          rate <- input$MenFUAge3_dose1 / input$MenFUAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$MenFUAge3_dose2_rate <- renderText({
        validate(need(input$MenFUAge3_total, ""))
        if (is.na(input$MenFUAge3_dose2)) {
          return(NULL)
        }
        if (!is.na(input$MenFUAge3_dose2)) {
          rate <- input$MenFUAge3_dose2 / input$MenFUAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$MenFUAge3_mening_rate <- renderText({
        validate(need(input$MenFUAge3_total, ""))
        if (is.na(input$MenFUAge3_mening)) {
          return(NULL)
        }
        if (!is.na(input$MenFUAge3_mening)) {
          rate <- input$MenFUAge3_mening / input$MenFUAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$MenFUAge3_tdap_rate <- renderText({
        validate(need(input$MenFUAge3_total, ""))
        if (is.na(input$MenFUAge3_tdap)) {
          return(NULL)
        }
        if (!is.na(input$MenFUAge3_tdap)) {
          rate <- input$MenFUAge3_tdap / input$MenFUAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
    }
    # Boys and girls combined - age 13
    if ("13" %in% input$Q2FU &
        input$Q3FU == "No, we will have combined rate data") {
      output$furates_13 <- renderUI({
        box(
          h3("Vaccination Rates for Male and Female Patients Combined, Age 13"),
          title = "Vaccination Rates for Male and Female Patients Combined, Age 13",
          collapsible = T,
          solidHeader = T,
          status = "success",
          collapsed = T,
          width = '100%',
          HTML("<br>"),
          h5(
            strong(
              "Total number of active medical patients ",
              em("age 13 ", style = "color:red"),
              "your clinic saw in 2021?"
            )
          ),
          numericInput(
            "BothFUAge3_total",
            label = NULL,
            value = followupRates$BothFUAge3_total,
            min = 0,
            step = 1,
            width = '100%'
          ),
          textOutput("error_13_total_fwup"),
          h5(
            strong(
              "Number of active patients who received at least one (1) HPV dose"
            )
          ),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothFUAge3_dose1",
              label = NULL,
              min = 0,
              value = followupRates$BothFUAge3_dose1,
              step = 1,
              width = '50%'
            ),
            textOutput("BothFUAge3_dose1_rate")
          ),
          textOutput("error_13_gt1_fwup"),
          h5(
            strong("Number of active patients who received both (2) HPV doses")
          ),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothFUAge3_dose2",
              label = NULL,
              min = 0,
              value = followupRates$BothFUAge3_dose2,
              step = 1,
              width = '50%'
            ),
            textOutput("BothFUAge3_dose2_rate")
          ),
          textOutput("error_13_2plus_fwup"),
          textOutput("error_13a_2plus_fwup"),
          h5(strong("Meningococcal")),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothFUAge3_mening",
              label = NULL,
              min = 0,
              value = followupRates$BothFUAge3_mening,
              step = 1,
              width = '50%'
            ),
            textOutput("BothFUAge3_mening_rate")
          ),
          textOutput("error_13_mening_fwup"),
          h5(strong("Tdap")),
          splitLayout(
            cellWidths = c("30%", "70%"),
            numericInput(
              "BothFUAge3_tdap",
              label = NULL,
              min = 0,
              value = followupRates$BothFUAge3_tdap,
              step = 1,
              width = '50%'
            ),
            textOutput("BothFUAge3_tdap_rate")
          ),
          textOutput("error_13_Tdap_fwup"),
        )
      })
      output$BothFUAge3_dose1_rate <- renderText({
        validate(need(input$BothFUAge3_total, ""))
        if (is.na(input$BothFUAge3_dose1)) {
          return(NULL)
        }
        if (!is.na(input$BothFUAge3_dose1)) {
          rate <- input$BothFUAge3_dose1 / input$BothFUAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$BothFUAge3_dose2_rate <- renderText({
        validate(need(input$BothFUAge3_total, ""))
        if (is.na(input$BothFUAge3_dose2)) {
          return(NULL)
        }
        if (!is.na(input$BothFUAge3_dose2)) {
          rate <- input$BothFUAge3_dose2 / input$BothFUAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$BothFUAge3_mening_rate <- renderText({
        validate(need(input$BothFUAge3_total, ""))
        if (is.na(input$BothFUAge3_mening)) {
          return(NULL)
        }
        if (!is.na(input$BothFUAge3_mening)) {
          rate <- input$BothFUAge3_mening / input$BothFUAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
      output$BothFUAge3_tdap_rate <- renderText({
        validate(need(input$BothFUAge3_total, ""))
        if (is.na(input$BothFUAge3_tdap)) {
          return(NULL)
        }
        if (!is.na(input$BothFUAge3_tdap)) {
          rate <- input$BothFUAge3_tdap / input$BothFUAge3_total
          rate <- rate * 100
          rate <- format(round(rate, 1), nsmall = 1)
          rate <-
            paste0(rate, "% vaccination rate in this age group")
        }
      })
    }
    
  }) # end the 13 observation
  
  
  output$static_table_fwup <- renderTable({
    q <-
      data.frame(rbind(
        c("AGES", "BORN", "EVER RECEIVED THE FOLLOWING VACCINES"),
        c("9-10", "2011-2012", "At least 1 dose of HPV; 2 doses of HPV"),
        c("11-12",  "2009-2010", "At least 1 dose of HPV; 2 doses of HPV; Meningococcal; Tdap"),
        c("13", "2008","At least 1 dose of HPV; 2 doses of HPV; Meningococcal; Tdap")
      ))
    
  }, striped = FALSE, align = "l", colnames = FALSE, rownames = FALSE, bordered =
    TRUE, spacing = "m", width = "70%")
  
  
  
  # -------------- WARNINGS ---------------- #
  observe({
    output$error_f9_total_fwup <- renderText({
      validate(need(input$FemFUAge1_total, "This number is required"),
               errorClass = "red_warnings")
    })
  })
  observe({
    output$error_f9_gt1_fwup <- renderText({
      if (is.na(input$FemFUAge1_dose1) | is.na(input$FemFUAge1_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemFUAge1_dose1 <= input$FemFUAge1_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_f9_2plus_fwup <- renderText({
      if (is.na(input$FemFUAge1_dose2) | is.na(input$FemFUAge1_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemFUAge1_dose2 <= input$FemFUAge1_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_f9a_2plus_fwup <- renderText({
      if (is.na(input$FemFUAge1_dose2) | is.na(input$FemFUAge1_dose1)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemFUAge1_dose2 <= input$FemFUAge1_dose1,
            "This number can't be greater than the total number of patients who received the first dose"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  
  
  
  # Males
  observe({
    output$error_m9_total_fwup <- renderText({
      validate(need(input$MenFUAge1_total, "This number is required"),
               errorClass = "red_warnings") # require responses for the male rates
    })
  })
  observe({
    output$error_m9_gt1_fwup <- renderText({
      if (is.na(input$MenFUAge1_dose1) | is.na(input$MenFUAge1_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenFUAge1_dose1 <= input$MenFUAge1_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_m9_2plus_fwup <- renderText({
      if (is.na(input$MenFUAge1_dose2) | is.na(input$MenFUAge1_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenFUAge1_dose2 <= input$MenFUAge1_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_m9a_2plus_fwup <- renderText({
      if (is.na(input$MenFUAge1_dose2) | is.na(input$MenFUAge1_dose1)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenFUAge1_dose2 <= input$MenFUAge1_dose1,
            "This number can't be greater than the total number of patients who received the first dose"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  
  # Combined (males and females)
  observe({
    output$error_9_total_fwup <- renderText({
      validate(need(input$BothFUAge1_total, "This number is required"),
               errorClass = "red_warnings") # require responses for the male rates
    })
  })
  observe({
    output$error_9_gt1_fwup <- renderText({
      if (is.na(input$BothFUAge1_dose1) | is.na(input$BothFUAge1_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothFUAge1_dose1 <= input$BothFUAge1_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_9_2plus_fwup <- renderText({
      if (is.na(input$BothFUAge1_dose2) | is.na(input$BothFUAge1_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothFUAge1_dose2 <= input$BothFUAge1_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_9a_2plus_fwup <- renderText({
      if (is.na(input$BothFUAge1_dose2) | is.na(input$BothFUAge1_dose1)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothFUAge1_dose2 <= input$BothFUAge1_dose1,
            "This number can't be greater than the total number of patients who received the first dose"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  
  # Aged 11-12 warnings ------------------------------- #
  
  # Females
  observe({
    output$error_f11_total_fwup <- renderText({
      validate(need(input$FemFUAge2_total, "This number is required"),
               errorClass = "red_warnings") # require responses for the female rates
    })
  })
  observe({
    output$error_f11_gt1_fwup <- renderText({
      if (is.na(input$FemFUAge2_dose1) | is.na(input$FemFUAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemFUAge2_dose1 <= input$FemFUAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_f11_2plus_fwup <- renderText({
      if (is.na(input$FemFUAge2_dose2) | is.na(input$FemFUAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemFUAge2_dose2 <= input$FemFUAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_f11a_2plus_fwup <- renderText({
      if (is.na(input$FemFUAge2_dose2) | is.na(input$FemFUAge2_dose1)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemFUAge2_dose2 <= input$FemFUAge2_dose1,
            "This number can't be greater than the total number of patients who received the first dose"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_f11_mening_fwup <- renderText({
      if (is.na(input$FemFUAge2_mening) | is.na(input$FemFUAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemFUAge2_mening <= input$FemFUAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_f11_Tdap_fwup <- renderText({
      if (is.na(input$FemFUAge2_tdap) | is.na(input$FemFUAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemFUAge2_tdap <= input$FemFUAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  
  # Males
  observe({
    output$error_m11_total_fwup <- renderText({
      validate(need(input$MenFUAge2_total, "This number is required"),
               errorClass = "red_warnings") # require responses for the male rates
    })
  })
  observe({
    output$error_m11_gt1_fwup <- renderText({
      if (is.na(input$MenFUAge2_dose1) | is.na(input$MenFUAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenFUAge2_dose1 <= input$MenFUAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_m11_2plus_fwup <- renderText({
      if (is.na(input$MenFUAge2_dose2) | is.na(input$MenFUAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenFUAge2_dose2 <= input$MenFUAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_m11a_2plus_fwup <- renderText({
      if (is.na(input$MenFUAge2_dose2) | is.na(input$MenFUAge2_dose1)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenFUAge2_dose2 <= input$MenFUAge2_dose1,
            "This number can't be greater than the total number of patients who received the first dose"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_m11_mening_fwup <- renderText({
      if (is.na(input$MenFUAge2_mening) | is.na(input$MenFUAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenFUAge2_mening <= input$MenFUAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_m11_Tdap_fwup <- renderText({
      if (is.na(input$MenFUAge2_tdap) | is.na(input$MenFUAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenFUAge2_tdap <= input$MenFUAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  
  
  
  
  
  # Combined (males and females)
  observe({
    output$error_11_total_fwup <- renderText({
      validate(need(input$BothFUAge2_total, "This number is required"),
               errorClass = "red_warnings") # require responses for the male rates
    })
  })
  observe({
    output$error_11_gt1_fwup <- renderText({
      if (is.na(input$BothFUAge2_dose1) | is.na(input$BothFUAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothFUAge2_dose1 <= input$BothFUAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_11_2plus_fwup <- renderText({
      if (is.na(input$BothFUAge2_dose2) | is.na(input$BothFUAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothFUAge2_dose2 <= input$BothFUAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_11a_2plus_fwup <- renderText({
      if (is.na(input$BothFUAge2_dose2) | is.na(input$BothFUAge2_dose1)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothFUAge2_dose2 <= input$BothFUAge2_dose1,
            "This number can't be greater than the total number of patients who received the first dose"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_11_mening_fwup <- renderText({
      if (is.na(input$BothFUAge2_mening) |
          is.na(input$BothFUAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothFUAge2_mening <= input$BothFUAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_11_Tdap_fwup <- renderText({
      if (is.na(input$BothFUAge2_tdap) | is.na(input$BothFUAge2_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothFUAge2_tdap <= input$BothFUAge2_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  
  
  # Aged 13 warnings ------------------------------- #
  
  # Females
  observe({
    output$error_f13_total_fwup <- renderText({
      validate(need(input$FemFUAge3_total, "This number is required"),
               errorClass = "red_warnings") # require responses for the female rates
    })
  })
  observe({
    output$error_f13_gt1_fwup <- renderText({
      if (is.na(input$FemFUAge3_dose1) | is.na(input$FemFUAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemFUAge3_dose1 <= input$FemFUAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_f13_2plus_fwup <- renderText({
      if (is.na(input$FemFUAge3_dose2) | is.na(input$FemFUAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemFUAge3_dose2 <= input$FemFUAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_f13a_2plus_fwup <- renderText({
      if (is.na(input$FemFUAge3_dose2) | is.na(input$FemFUAge3_dose1)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemFUAge3_dose2 <= input$FemFUAge3_dose1,
            "This number can't be greater than the total number of patients who received the first dose"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_f13_mening_fwup <- renderText({
      if (is.na(input$FemFUAge3_mening) | is.na(input$FemFUAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemFUAge3_mening <= input$FemFUAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_f13_Tdap_fwup <- renderText({
      if (is.na(input$FemFUAge3_tdap) | is.na(input$FemFUAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$FemFUAge3_tdap <= input$FemFUAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  
  # Males
  observe({
    output$error_m13_total_fwup <- renderText({
      validate(need(input$MenFUAge3_total, "This number is required"),
               errorClass = "red_warnings") # require responses for the male rates
    })
  })
  observe({
    output$error_m13_gt1_fwup <- renderText({
      if (is.na(input$MenFUAge3_dose1) | is.na(input$MenFUAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenFUAge3_dose1 <= input$MenFUAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_m13_2plus_fwup <- renderText({
      if (is.na(input$MenFUAge3_dose2) | is.na(input$MenFUAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenFUAge3_dose2 <= input$MenFUAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_m13a_2plus_fwup <- renderText({
      if (is.na(input$MenFUAge3_dose2) | is.na(input$MenFUAge3_dose1)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenFUAge3_dose2 <= input$MenFUAge3_dose1,
            "This number can't be greater than the total number of patients who received the first dose"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_m13_mening_fwup <- renderText({
      if (is.na(input$MenFUAge3_mening) | is.na(input$MenFUAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenFUAge3_mening <= input$MenFUAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_m13_Tdap_fwup <- renderText({
      if (is.na(input$MenFUAge3_tdap) | is.na(input$MenFUAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$MenFUAge3_tdap <= input$MenFUAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  
  # Combined (males and females)
  observe({
    output$error_13_total_fwup <- renderText({
      validate(need(input$BothFUAge3_total, "This number is required"),
               errorClass = "red_warnings") # require responses for the male rates
    })
  })
  observe({
    output$error_13_gt1_fwup <- renderText({
      if (is.na(input$BothFUAge3_dose1) | is.na(input$BothFUAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothFUAge3_dose1 <= input$BothFUAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_13_2plus_fwup <- renderText({
      if (is.na(input$BothFUAge3_dose2) | is.na(input$BothFUAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothFUAge3_dose2 <= input$BothFUAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_13a_2plus_fwup <- renderText({
      if (is.na(input$BothFUAge3_dose2) | is.na(input$BothFUAge3_dose1)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothFUAge3_dose2 <= input$BothFUAge3_dose1,
            "This number can't be greater than the total number of patients who received the first dose"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_13_mening_fwup <- renderText({
      if (is.na(input$BothFUAge3_mening) |
          is.na(input$BothFUAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothFUAge3_mening <= input$BothFUAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  observe({
    output$error_13_Tdap_fwup <- renderText({
      if (is.na(input$BothFUAge3_tdap) | is.na(input$BothFUAge3_total)) {
        return(NULL)
      } else {
        validate(
          need(
            input$BothFUAge3_tdap <= input$BothFUAge3_total,
            "This number can't be greater than the total number of patients seen"
          ),
          errorClass = "red_warnings"
        )
      }
    })
  })
  })
  
  
  observeEvent(input$button_fwup_rates, {
    updateTabsetPanel(session, "tabbox_final", selected = "p2_additional")
  })
  

  
  
  
  
  # Additional information tab ----------------------------------------------
  
  
  
  
  additionalInfo <- myData$additionalInfo
  output$SelectAddInfoUser <- renderUI({
    selectInput(inputId = "SelectAddInfoUser",
                label = "Select health system account",
                choices = unique(additionalInfo$Username),
                selected = additionalInfo$Username[1])
  })
  
  observe({
    validate(need(input$SelectAddInfoUser, ""))
    additionalInfo <- filter(myData$additionalInfo, Username == input$SelectAddInfoUser)
    
    
    
    output$date_fwup_additional <- renderUI({
      h4(paste("This health system's data was most recently updated on ", (additionalInfo$additionalInfoDate)))
    })
    
    

  foo7FU <- with(additionalInfo, c(Q7FU_1, Q7FU_2, Q7FU_3, Q7FU_4, Q7FU_5))
  foo7FU <- foo7FU[!is.na(foo7FU)]
  
  foo8FU <- with(additionalInfo, c(Q8FU_1, Q8FU_2, Q8FU_3))
  foo8FU <- foo8FU[!is.na(foo8FU)]
  
  foo9FU <- with(additionalInfo, c(Q9FU_1, Q9FU_2, Q9FU_3, Q9FU_4,
                                   Q9FU_5, Q9FU_6, Q9FU_7, Q9FU_8, Q9FU_9))
  foo9FU <- foo9FU[!is.na(foo9FU)]
  
  foo13FU <- with(additionalInfo, c(Q13FU_1, Q13FU_2, Q13FU_3, Q13FU_4,
                                    Q13FU_5, Q13FU_6, Q13FU_7, Q13FU_8))
  foo13FU <- foo13FU[!is.na(foo13FU)]
  
  foo14FU <- with(additionalInfo, c(Q14FU_1, Q14FU_2, Q14FU_3, Q14FU_4, Q14FU_5))
  foo14FU <- foo14FU[!is.na(foo14FU)]
  
  
  
  output$Q5FU <- renderUI({
    selectInput(
      inputId = "Q5FU",
      label = "5.  Did you use the same primary data source for all pulls?",
      choices = c("", "Yes", "No"),
      selected = additionalInfo$Q5FU)
  })
  
  
  output$Q5FU_text <- renderUI({
    validate(need(input$Q5FU, ""))
    if (input$Q5FU != "No")
      return(NULL)
    else{
      textInput("Q5FU_text",
                label = "5a. Please describe what changed",
                value = additionalInfo$Q5FU_text,
                width = "100%")
    }
  })
  output$Q6FU <- renderUI({
    selectInput(
      inputId = "Q6FU",
      label = "6.  Did you meet your project goals?",
      choices = c("", "Yes", "Some, but not all",  "No"),
      selected = additionalInfo$Q6FU)
  })
  
  output$Q6FU_text <- renderUI({
    validate(need(input$Q6FU, ""))
    if (c("Yes", "") %in% input$Q6FU)
      return(NULL)
    else{
      textInput("Q6FU_text",
                label = "6a. Tell us more about this",
                value = additionalInfo$Q6FU_text,
                width = "100%")
    }
  })
  
  # Q7FU - checkbox group
  output$Q7FU <- renderUI({
    checkboxGroupInput(
      inputId = "Q7FU",
      label = p(
        "7.  We conducted or participated in the following HPV vaccination training and education activities in 2021: ",
        em(" (check all that apply)")),
      choices = c(
        "Educated staff on HPV vaccination as cancer prevention",
        "Educated staff on strategies to improve HPV vaccination rates",
        "Identified HPV vaccination champions",
        "Trained providers to make an effective HPV vaccine recommendation",
        "Other (specify)"),
      selected =  foo7FU,
      width = '100%')
  })
  output$Q7FU_other <- renderUI({
    validate(need(input$Q7FU, ""))
    if ("Other (specify)" %in% input$Q7FU) {
      textInput(
        "Q7FU_other",
        label = "Please specify",
        value = additionalInfo$Q7FU_other,
        width = "100%"
      )
    }
  })
  
  # Q8FU - checkbox group
  output$Q8FU <- renderUI({
    checkboxGroupInput(
      inputId = "Q8FU",
      label = p("8.  Did you participate in any of the following in 2021 (select all that apply)?"),
      choices = c("ECHO",
                  "Learning collaborative",
                  "Other (specify)"),
      width = "100%",
      selected = foo8FU)
  })
  output$Q8FU_other <- renderUI({
    validate(need(input$Q8FU, ""))
    if ("Other (specify)" %in% input$Q8FU) {
      textInput(
        "Q8FU_other",
        label = "Please specify",
        value = additionalInfo$Q8FU,
        width = "100%"
      )
    }
  })
  
  # Q9FU - checkbox group
  output$Q9FU <- renderUI({
    checkboxGroupInput(
      inputId = "Q9FU",
      label = p(
        "9.  We implemented the following interventions to increase HPV vaccination in 2021: ",
        em("(check all that apply)")),
      choices = c(
        "Extended hours",
        "Modified EHR",
        "Offered in alternative settings like schools or mobile units",
        "Parent/patient education",
        "Patient reminders",
        "Provider assessment and feedback",
        "Provider prompts/reminders",
        "Standing orders",
        "Other (specify)"),
      width = '100%',
      selected =  foo9FU)
  })
  output$Q9FU_other <- renderUI({
    validate(need(input$Q9FU, ""))
    if ("Other (specify)" %in% input$Q9FU) {
      textInput(
        "Q9FU_other",
        label = "Please specify",
        value = additionalInfo$Q9FU_other,
        width = "100%"
      )
    }
  })
  
  output$Q10FU <- renderUI({
    numericInput(inputId = "Q10FU",
                 label = "10.  How many providers did you train?",
                 width = '100%',
                 min = 0,
                 step = 1,
                 value = additionalInfo$Q10FU)
  })
  output$Q11FU <- renderUI({              
    numericInput( inputId = "Q11FU",
                  label = "11.  What was the average number of training hours per provider?",
                  min = 0,
                  width = '100%',
                  value = additionalInfo$Q11FU)
  })
  output$Q12FU <- renderUI({
    numericInput(
      inputId = "Q12FU",
      label = "12  How many other staff did we educate?",
      min = 0,
      step = 1,
      width = '100%',
      value = additionalInfo$Q12FU)
  })
  
  
  
  # Q13FU - checkbox group
  output$Q13FU <- renderUI({
    checkboxGroupInput(inputId = "Q13FU",
                       label = p("13.  We used the following tools and resources: ",
                                 em("(check all that apply)")),
                       choices = c("ACS 'Steps' Guide",
                                   "ACS 'HPV 101' slides",
                                   "ACS Reminder Cards",
                                   "ACS and/or Merck EHR Guides",
                                   "ACS and/or CDC parent handout",
                                   "CDC 'You Are The Key' slides",
                                   "Other ACS Key Resources Roundtable Action Guides",
                                   "Other (specify)"),
                       width = '100%',
                       selected =  foo13FU)
  })
  output$Q13FU_other <- renderUI({
    validate(need(input$Q13FU, ""))
    if ("Other (specify)" %in%  input$Q13FU) {
      textInput(
        "Q13FU_other",
        label = "Please specify",
        value = additionalInfo$Q13FU_other,
        width = "100%"
      )
    }
  })
  
  
  # Q14FU checkbox group
  output$Q14FU <- renderUI({
    checkboxGroupInput(inputId = "Q14FU",
                       label = "14.  What roles did ACS play in this project?",
                       choices = c(
                         "Member of HPV QI project team",
                         "Provided input on strategy",
                         "Provided materials",
                         "Provided technical assistance",
                         "Other (specify)"),
                       width = '100%',
                       selected =  foo14FU)
  })
  output$Q14FU_other <- renderUI({
    validate(need(input$Q14FU, ""))
    if ("Other (specify)" %in%  input$Q14FU) {
      textInput(
        "Q14FU_other",
        label = "Please specify",
        value = additionalInfo$Q14FU_other,
        width = "100%"
      )
    }
  })
  
  output$Q15FU <- renderUI({
    textAreaInput(
      inputId = "Q15FU",
      label = "15. What were the successes and challenges of this project?",
      value = additionalInfo$Q15FU,
      width = '100%',
      rows = 3
    )
  })
  output$Q16FU <- renderUI({
    textAreaInput(
      inputId = "Q16FU",
      label = "16.  What is your plan to sustain the changes you made in this project?",
      value = additionalInfo$Q16FU,
      width = '100%',
      rows = 3)
  })
  output$Q17FU <- renderUI({
    textAreaInput(
      inputId = "Q17FU",
      label = "17.  Is there anything else to add about this project?",
      value = additionalInfo$Q17FU,
      width = '100%',
      rows = 3)
  })
  output$Q18FU <- renderUI({
    selectInput(
      inputId = "Q18FU",
      label = "18.  Will your system continue to work with ACS to increase our HPV vaccination rates?",
      choices = c("", "Yes", "No"),
      selected =  additionalInfo$Q18FU)
  })
  output$Q18FU_more <- renderUI({
    if (input$Q18FU =="No"){
      textInput(
        inputId = "Q18FU_more",
        label = "18a. Tell us more about why not",
        value= additionalInfo$Q18FU_more,
        width='100%')
    }
  })
  })
  
  
  observeEvent(input$save_followup, {
    
    updateTabsetPanel(session, "tabbox_final", selected = "p2_submission")
    
    
  })
 } # end the server


shiny::shinyApp(ui=ui, server=server)


