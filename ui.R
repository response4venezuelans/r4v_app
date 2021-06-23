dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  skin = "black",
  dashboardBody(
    img(src = "r4v.png", height = 80),
    tabsetPanel(
      tabPanel(title = "1.Data Upload",br(),
               p("V2.1 Updated 22/06/2021, please send any comments to the Regional platform IM team", style="color: #fff; background-color: #672D53"),
               
                 
                 column(8,shinydashboard::box(id="box_2", title = "Please copy paste the data with the header or upload you regional ENG 5W import table or load all the records from the API", solidHeader = T,collapsible = T,collapsed = F,
                                              width = 12,status = "primary",
                                              
                                              
                                              fluidRow(
                                                column(
                                                  width = 6,
                                                  import_copypaste_ui("myid", title = "Paste with the header row."),
                                                
                                                ),

                                              
                                              # h3(p("Please upload you regional ENG 5W import table or load all the record from the API")),
                                              fluidRow(
                                                column(width = 5,
                                                       br(),
                                                       br(),
                                                       fileInput(inputId = "data_upload",label = "or Browse and Upload (xlsx Regional format)",multiple = F),
                                                ),
                                                column(width = 5,
                                                       br(),
                                                       p("or Load all the data from Activity Info (API) regional database (2min)"),
                                                       actionButton(inputId = "Run_Script",label = "Load from Regional Database", icon = icon("bone"), width = "300px", style="color: #fff; background-color: #00AAAD"),
                                                ))))),
               br(),
               fluidRow(column(12,shinydashboard::box(id="box_9", title = "Preview data", solidHeader = T,collapsible = T,collapsed = F,
                                                      width = 12,status = "primary",
                                                      DT::dataTableOutput("Preview_Data")
               ))),
               
               
               fluidRow(column(1,shinydashboard::box(id="box_15", title = "Control", solidHeader = T,collapsible = T,collapsed = F,
                                                      width = 12,status = "primary",
               tags$b("Imported data:"),
               verbatimTextOutput(outputId = "status"),
               verbatimTextOutput(outputId = "data")
               )))
               
               
               
      ),
      tabPanel(title = "2.Error and cleaning script",br(),
               p("V2.1 Updated 22/06/2021 please send any comments to the Regional platform IM team", style="color: #fff; background-color: #672D53"),
               
               fluidRow(
                 
                 column(8,shinydashboard::box(id="box_2", title = "Error Report and cleaning scripts", solidHeader = T,collapsible = T,collapsed = F,
                                              width = 12,status = "primary",
                                              p("Please select your country and the automatic cleaning script you want to apply and RUN SCRIPT"),
                                              fluidRow(
                                                column(6,
                                                       selectInput("country_name",label = "Country Name",choices = c("NULL")),
                                                       radioButtons(inline = TRUE, "covid_selection_err_report",label = "COVID: If the COVID 19 field is empty, automatically registers as “No”",choices = c("TRUE","FALSE")),
                                                       radioButtons(inline = TRUE, "supportspace_selection_err_report",label = "SUPPORTSPACE: If the Support Space field is empty, automatically registers as “No”. ", choices = c("TRUE","FALSE")),
                                                       radioButtons(inline = TRUE, "rmrp_err_report",label = "RMRP: If the RMRP field is empty, automatically registers as “Yes”",choices = c("TRUE","FALSE")),
                                                       radioButtons(inline = TRUE, "cvachange_err_report",label = "CVACHANGE: If CVA is registered as No, but that there is a Value of transfer that has been registered, automatically changes the CVA to Yes", choices = c("TRUE","FALSE")),
                                                       radioButtons(inline = TRUE, "err_pin_benef_fill",label = "PIN BENEF FILL: If New beneficiaries and Total are equal 0, copy values from breakdowns (only if they are equal) or from quantity column, if breakdowns are not equals.",choices = c("TRUE","FALSE")),
                                                       radioButtons(inline = TRUE, "total_err_new",label = "TOTAL LOWER NEW: If the New Beneficiaries value is superior to the Total of Beneficiaries, automatically changes the value of Total to equal the value of New Beneficiaries",choices = c("TRUE","FALSE")),
                                                       radioButtons(inline = TRUE, "assign_in_destination_err_report",label = "ASSIGN IN DESTINATION: For PiN indicators, is the Population Type breakdown is empty, assign the New Beneficiaries values to the Refugees and Migrants category, as recommended in the Guidance.", choices = c("TRUE","FALSE")),
                                                       radioButtons(inline = TRUE, "err_assign_all_destination",label = "ASSIGN ALL DESTINATION:If New beneficiaries value is not equal to the population type breakdown, assign the difference to the MAX category of the breakdown",choices = c("TRUE","FALSE"))
                                                       
                                                ),
                                                column(6,
                                                       radioButtons(inline = TRUE, "err_pin_auto_apportioning",label = "PIN AUTO APPORTIONING: For Pin indicators, if the Age and Gender breakdown is empty, applies an automatic breakdown extracted from the Pin 2021 Country and Sector information based on new beneficiaries values",choices = c("TRUE","FALSE")),
                                                       radioButtons(inline = TRUE, "err_complete_pin_apportioning",label = "COMPLETE PIN APPORTIONING: For Pin indicators, if the Age and Gender breakdown is empty, applies an automatic breakdown extracted from the Country and Sector information based on new beneficiaries values. USE WITH CARE AS IT OVERWRITES DATA !!",choices = c("TRUE","FALSE")),
                                                       radioButtons(inline = TRUE, "err_rounding_diff1",label = "ROUNDING DIFF: Rounding differences: due to roundings, differences in the breakdowns can be +1 or -1, sum or rest this 1 difference to the max age and gender category",choices = c("TRUE","FALSE")),
                                                       radioButtons(inline = TRUE, "err_pin_erase_quantity",label = "PIN ERASE QUANTITY: For Pin indicators: as Pin indicators should not contain data in Quantity, change all values for this field to 0. USE WITH CARE AS IT ERASES DATA !!",choices = c("TRUE","FALSE")),
                                                       radioButtons(inline = TRUE, "err_people_fill",label = "PEOPLE FILL: People indicators: if Quantity of unit equal 0, copy values from Total of beneficiaries or if total empty, from age and gender breakdown sum", choices = c("TRUE","FALSE")),
                                                       radioButtons(inline = TRUE, "err_people_erase_extradata",label = "PEOPLE ERASE EXTRA DATA: For People indicators: as People indicators should not contain data in population type breakdown, change all values for this field to 0. USE WITH CARE AS IT ERASES DATA !!", choices = c("TRUE","FALSE")),
                                                       radioButtons(inline = TRUE, "err_other_fill",label = "OTHER FILL:People indicators: if Quantity of uni equal 0, copy values from Total of beneficiaries",choices = c("TRUE","FALSE")),
                                                       radioButtons(inline = TRUE, "err_other_erase_extradata",label = "OTHER ERASE EXTRA DATA: For Other indicators: as Other indicators should not contain data in age gender and population type breakdown, change all values for this field to 0. USE WITH CARE AS IT ERASES DATA !!",choices = c("TRUE","FALSE"))
                                                )
                                                
                                                
                                                
                                              ),
                                              actionButton("run_err_report",label = "Run Script",icon = icon("black-tie"), style="color: #fff; background-color: #00AAAD"), 
                                              # downloadButton("downloaderror", "Download Precleaned data", style="color: #fff; background-color: #672D53"),
                                              downloadButton("downloadprecleaned", "Download Precleaned data", style="color: #fff; background-color: #672D53"),
                                              downloadButton("downloadcleaned", "Download Cleaned data", style="color: #fff; background-color: #672D53"),
                                              
                                              
                 )),
                 
                 # column(4,shinydashboard::box(id="box_3", title = "Data Quality report (HTML)", solidHeader = T,collapsible = T,collapsed = F,
                 #                              width = 12,status = "primary",
                 #                              p("Run DQC report before downloading (2min)"),
                 #                              br(),
                 #                              actionButton("run_rmd_report",label = "Run DQC report",icon = icon("paper-plane"), style="color: #fff; background-color: #00AAAD"),
                 #                              downloadButton("report", "Download Data Quality report", style="color: #fff; background-color: #672D53")
                 # )) ,
                 
                 column(4,shinydashboard::box(id="box_3", title = "Summary", solidHeader = T,collapsible = T,collapsed = F,
                                              width = 12,status = "warning",
                                              p("Number or Activities"),
                                              h2(textOutput("Number_of_Activities")),
                                              p("Number of errors Pre cleaning"),
                                              h2(textOutput("Number_of_Errors_Pre")),
                                              p("Number of errors Post cleaning"),
                                              h2(textOutput("Number_of_Errors")),
                                              p("Percentage of errors"),
                                              h2(textOutput("Percentage_of_Errors")),
                 )) ,
                 
               ),
               # GRAPH
               
               fluidRow(
                 column(12,shinydashboard::box(id="box_14", title = "Error per org and Country", solidHeader = T,collapsible = T,collapsed = F,
                                               width = 12,status = "primary",
                                               fluidRow(  column(6,plotlyOutput("plot")),
                                                          column(6,plotlyOutput("plot2"))
                                                          
                                               ))) ,
                 # 
                 
                 
                 
                 column(12,shinydashboard::box(id="box_4", title = "Preview error report table", solidHeader = T,collapsible = T,collapsed = F,
                                               width = 12,status = "primary",
                                               DT::dataTableOutput("Preview_Error_Report")
                 ))) 
               
               
      ),
      
      tabPanel(title = "3.Consolidated report",br(),
               p("V2.1 Updated 22/06/2021 please send any comments to the Regional platform IM team", style="color: #fff; background-color: #672D53"),
               fluidRow(column(8,shinydashboard::box(id="box_5", title = "Consolidated Report creation", solidHeader = T,collapsible = T,collapsed = F,
                                                     width = 12,status = "primary",
                                                     p("Please select your country and the aggregation method you want to apply and RUN SCRIPT"),
                                                     selectInput("country_name_agg",label = "Country Name",choices = c("NULL")),
                                                     radioButtons(inline = TRUE, "totalmodel_agg",label = "Aggregation Model",choices = c("sum","maxagegender","maxsector", "conosurmodel")),
                                                     actionButton("run_aggregation",label = "Run Script",icon = icon("battle-net"), style="color: #fff; background-color: #00AAAD"),
                                                     downloadButton("downloadData", "Download consolidated report", style="color: #fff; background-color: #672D53")))),
               
               fluidRow(column(12,shinydashboard::box(id="box_6", title = "Preview consolidated report", solidHeader = T,collapsible = T,collapsed = F,
                                                      width = 12,status = "primary",
                                                      DT::dataTableOutput("Preview_conslidated")
               ))) )
      
    )))

