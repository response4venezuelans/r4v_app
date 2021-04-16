#add upload capacity to shiny to 30MB

options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output, session) {
  
  ## Declaring Variables
  Data <- reactiveVal()
  Consolidated <- reactiveVal()
  Error_Download <- reactiveVal()
  # PreCleaned_Errors_Download <- reactiveVal()
  Cleaned_Errors_Download <- reactiveVal()
  
  # To go back on folder before.
  # setwd("..")

  
  imported <- import_copypaste_server("myid")
  
  
  output$status <- renderPrint({
    imported$status() })
  
    output$data <- renderPrint({
      source("R/1b_read_data_rmrp.R")
      # if (imported$data[1:1] = 'Appealing Organization')
        
    Data(r4v_pull_xlsdata(imported$data()))
      showNotification("Data Processing Complete",duration = 10, type = "error")
      
    updateSelectInput(session,"country_name",choices = unique(Data()$Country))
    updateSelectInput(session,"country_name_agg",choices = unique(Data()$Country))
    
    
    })
    
    
  # output$data <- renderPrint({
  #   imported$data()
  #   Data(r4v_pull_xlsdata(imported$data()))
  #     updateSelectInput(session,"country_name",choices = unique(Data()$Country))
  #     updateSelectInput(session,"country_name_agg",choices = unique(Data()$Country))
  #   
  #     
  # })
  # 
  
  
  
  # output$data <- renderPrint({
  #   source("R/1b_read_data_rmrp.R")
  #   Data(r4v_pull_xlsdata(imported$data()))
  #   
  #   updateSelectInput(session,"country_name",choices = unique(Data()$Country))
  #   updateSelectInput(session,"country_name_agg",choices = unique(Data()$Country))
  #   
  #   
  #    })
  
  
  
  
  ## Observe file Input
  observeEvent(
    
    input$data_upload,{
    filename <- tolower(input$data_upload$name)
    
    ## Condition to check file type
    if(!(sub('.*\\.','',filename)) %in% 'xlsx'){
      showNotification('Only XLSX are supported',duration = 5)
      req(F)
    }
    # Skip=2 get rid of the 2 toplines as per the template
    Data(read_excel(input$data_upload$datapath, skip = 2))
    source("R/1b_read_data_rmrp.R")
    Data(r4v_pull_xlsdata(Data()))
    showNotification("Data Processing Complete",duration = 10, type = "error")
    
    # Update the drop down button with Countries
    
    updateSelectInput(session,"country_name",choices = unique(Data()$Country))
    updateSelectInput(session,"country_name_agg",choices = unique(Data()$Country))
  })
  
  ## Data Preview
  output$Preview_Data <- DT::renderDataTable({Data()},extensions = c("Buttons"), options = list(
    dom = 'lfrtip', 
    # add B for button
    paging = TRUE,
    ordering = TRUE,
    lengthChange = TRUE,
    pageLength = 10,
    scrollX = TRUE,
    autowidth = TRUE,
    rownames = TRUE
    # buttons = c('copy', 'csv', 'excel', 'pdf')
  ))
  
  observeEvent(input$Run_Script,{
    source("R/1_read_data_rmrp.R")
    Data(r4v_pull_aidata())
    showNotification("Data Processing Complete",duration = 10, type = "error")
    updateSelectInput(session,"country_name",choices = c("All",unique(Data()$Country)))
    updateSelectInput(session,"country_name_agg",choices = c("All",unique(Data()$Country)))
  })
  
  ## run script Check error
  
  observeEvent(input$run_err_report,{
    source("R/2_quality_check_report_generator.R")
    Error_report <- r4v_error_report(data = Data(),
                                     countryname = input$country_name,
                                     covid = input$covid_selection_err_report,
                                     supportspace = input$supportspace_selection_err_report,
                                     rmrp = input$rmrp_err_report,
                                     cvachange = input$cvachange_err_report,
                                     pin_benef_fill = input$err_pin_benef_fill,
                                     total_lower_new = input$total_err_new,
                                     assign_in_destination = input$assign_in_destination_err_report,
                                     assign_all_destination = input$err_assign_all_destination,
                                     pin_auto_apportioning = input$err_pin_auto_apportioning, 
                                     complete_pin_apportioning = input$err_complete_pin_apportioning,
                                     rounding_diff1 = input$err_rounding_diff1, 
                                     pin_erase_quantity = input$err_pin_erase_quantity, 
                                     people_fill = input$err_people_fill,
                                     people_erase_extradata = input$err_people_erase_extradata,
                                     other_fill = input$err_other_fill, 
                                     other_erase_extradata = input$err_other_erase_extradata)
    
    
    Error_Download(Error_report$ErrorReportclean)
    # PreCleaned_Errors_Download(Error_report$ErrorReportActivitiesClean)
    Cleaned_Errors_Download(Error_report$cleaned_data)
    
    # To output number of activities and error 
    
    output$Number_of_Activities <- renderText({nrow(Error_report$cleaned_data)})
    output$Number_of_Errors_Pre <- renderText({sum(!is.na(Error_report$ErrorReportclean$ERROR))})
    output$Number_of_Errors <- renderText({sum(!is.na(Error_report$cleaned_data$ERROR))})
    output$Percentage_of_Errors <- renderText(round({sum(!is.na(Error_report$cleaned_data$ERROR))}/{nrow(Error_report$cleaned_data)}*100, digits = 1))
    
    # PLOTLY section
    
    output$plot <- renderPlotly({
      Error_report$cleaned_data %>%
        filter(!is.na(ERROR)) %>%
        ggplot() +
        aes(x = Appealing.Organization.Name, size = ERROR) +
        geom_bar(fill = "#0c4c8a") +
        coord_flip() +
        theme_minimal()})
    
    output$plot2 <- renderPlotly({
      Error_report$cleaned_data %>%
        filter(!is.na(ERROR)) %>%
        ggplot() +
        aes(x = Country, size = ERROR) +
        geom_bar(fill = "#0c4c8a") +
        coord_flip() +
        theme_minimal()})
    
    showNotification("Successful",duration = 10, type = "error")
  })
  
  observeEvent(input$run_aggregation,{
    source("R/6_consolidated_report.R")
    Consolidated(r4v_consolidated(data = Data(),countryname = input$country_name_agg,totalmodel = input$totalmodel_agg))
    showNotification("Successful",duration = 10, type = "error")
  })
  
  #Run the RMD report, still need to make by the country selected and need to move the output in Vignettes folder
  
  observeEvent(input$run_rmd_report,{
    rmarkdown::render("./vignettes/Quality check RMRP 2021 shiny.Rmd", output_dir = "./out")
    showNotification("Successful",duration = 10, type = "error")
  })
  
  ## Preview of Error Report
  output$Preview_Error_Report <- DT::renderDataTable({Error_Download()},extensions = c("Buttons"), options = list(
    dom = 'lfrtip',
    paging = TRUE,
    ordering = TRUE,
    lengthChange = TRUE,
    pageLength = 10,
    scrollX = TRUE,
    autowidth = TRUE,
    rownames = TRUE
    # buttons = c('copy', 'csv', 'excel', 'pdf')
  ))
  
  ## Preview of Consolidated
  
  output$Preview_conslidated <- DT::renderDataTable({Consolidated()},extensions = c("Buttons"), options = list(
    dom = 'lfrtip',
    paging = TRUE,
    ordering = TRUE,
    lengthChange = TRUE,
    pageLength = 10,
    scrollX = TRUE,
    autowidth = TRUE,
    rownames = TRUE
    # buttons = c('copy', 'csv', 'excel', 'pdf')
  ))
  
  
  # 
  # ## Download Error Report
  # output$downloaderror <- downloadHandler(
  #   filename = function() {
  #     paste("Error_Report", ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(Error_Download(), file, row.names = FALSE)
  #   }
  # )
  
  ## Download Pre-Cleaned
  output$downloadprecleaned <- downloadHandler(
    filename = function() {
      paste("Pre_Cleaned_Data", ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(Error_Download(), file)
    }
  )
  
  ## Download Cleaned
  output$downloadcleaned <- downloadHandler(
    filename = function() {
      paste("Cleaned_Data", ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(Cleaned_Errors_Download(), file)
    }
  )
  
  ## Download Consolidated
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Consolidated_Report", ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(Consolidated(), file)
    }
  )
  
  ## Download RMD Quality report
  output$report <- downloadHandler(
    filename <- function() {
      paste("Quality-check-RMRP-2021", "html", sep=".")
    },
    content <- function(file) {
      file.copy("./out/Quality-check-RMRP-2021-shiny.html", file)
    }
  )
  
  
  # Stop app when closing the browser
  # session$onSessionEnded(stopApp)
  
  
  
  
})





