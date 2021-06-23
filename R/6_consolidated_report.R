#' @name consolidated
#' @rdname consolidated
#' @title Produce consolidated report
#' @description 
#'
#' @param query
#'
#' @return frame
#'
#' @examples
#'\dontrun{
#' consolidated ()
#' }
#'
#' @export
#'

r4v_consolidated <- function(data,countryname = NULL, totalmodel = "sum")
  
{# The agregation and calculation processed in this file are base on the information 
  # provided by the country in the RMRP 2021 Activty Info databasse. 
  
  # Packages
  library(tidyverse)
  library(readxl)
  library(dplyr)
  library(writexl)
  
  # Download of Activity Info tables from local file
  
  # Paths
  df <-  read_excel('./data-raw/RMRP_2021_AI_activities.xlsx')
  dfindicator <- read_excel( './data-raw/RMRP_2021_AI_indicators.xlsx')
  
  # Download consolidated template to merge the final files
  dftemplate <-  read_excel('./data/Consolidated_Template.xlsx')
  
  
  # Filter data and template by Country
  
  if (is.null(countryname) || (countryname=="All")) {
    dfindicator2 <- df%>%
      left_join(dfindicator, by = c("Sector...Subsector...WG", "Indicator")) 
  } else {
    dfindicator2 <- df%>%
      left_join(dfindicator, by = c("Sector...Subsector...WG", "Indicator"))%>%
      filter(Country == countryname )   
  }
  
  
  if (is.null(countryname) || (countryname=="All")) {
    dftemplate <- dftemplate
  } else {
    dftemplate <- dftemplate%>%
      filter(Country == countryname) 
  }
  
  
  # Get the TOTAL MONTHLY number of persons per sector of activity
  # PiN Indicators grouped by sectors
  
  monthlysectors <- dfindicator2%>%
    filter(Indicator_Type == "Pin")%>%
    group_by(Country, Reporting.Month, Sector...Subsector...WG)%>%
    summarise('Monthly Total Beneficiaries' = sum(Total.monthly.beneficiaries))
  
  monthlysectorstotal <- monthlysectors%>%
    group_by(Country, Reporting.Month)%>%
    summarise(Sector...Subsector...WG = "All", 'Monthly Total Beneficiaries' = sum(`Monthly Total Beneficiaries`))
  
  monthly_total_benef<- rbind(monthlysectors, monthlysectorstotal)
  
  # Get the TOTAL CVA MONTHLY number of persons per sector of activity
  # PiN Indicators grouped by sectors
  
  monthlysectorCVA <- dfindicator2%>%
    filter(Indicator_Type == "Pin", CVA == "Yes")%>%
    group_by(Country, Reporting.Month, Sector...Subsector...WG)%>%
    summarise('Monthly CVA Beneficiaries' = sum(Total.monthly.beneficiaries))
  
  monthlyCVAtotal <- monthlysectorCVA%>%
    group_by(Country, Reporting.Month)%>%
    summarise(Sector...Subsector...WG = "All", `Monthly CVA Beneficiaries` = sum(`Monthly CVA Beneficiaries`))
  
  monthly_CVA<- rbind(monthlysectorCVA, monthlyCVAtotal)
  
  Monthly_benef<-monthly_total_benef%>%
    full_join(monthly_CVA, by = c("Country", "Reporting.Month", "Sector...Subsector...WG"))%>%
    mutate(`Monthly CVA Beneficiaries` = ifelse(is.na(`Monthly CVA Beneficiaries`), 0, `Monthly CVA Beneficiaries`))
  
  #Consolidated method for total
  #Consolidated data are extracted from the new beneficiaries as well as the 
  # AGD and Population type breakdowns
  
  # First: generate the table of new beneficiaries with the breakdowns
  
  New_beneficairies <-  dfindicator2%>%
    filter(Indicator_Type == "Pin")%>%
    group_by(Country, Admin1, Reporting.Month, Sector...Subsector...WG)%>%
    summarise(Monthly_Consolidated = sum(New.beneficiaries.of.the.month),
              Consolidated.RMindestination = sum(Refugees.and.Migrants.IN.DESTINATION), 
              Consolidated.RM.in.transit = sum(Refugees.and.Migrants.IN.TRANSIT),
              Consolidated.Host.Community = sum(Host.Communities.Beneficiaries),
              Consolidated.RM.Pendulars = sum(Refugees.and.Migrants.PENDULARS),
              Consolidated.Colombian.Returnees = sum(Colombian.Returnees),
              Consolidated.Women.under.18 = sum(Women.under.18), 
              Consolidated.Men.under.18 = sum(Men.under.18),
              Consolidated.Women.above.18 = sum(Women.above.18),
              Consolidated.Men.above.18 = sum(Men.above.18)
    )
  
  New_CVA_beneficairies <-  dfindicator2%>%
    filter(Indicator_Type == "Pin", CVA == "Yes")%>%
    group_by(Country, Admin1, Reporting.Month, Sector...Subsector...WG)%>%
    summarise(Monthly_CVA_Consolidated = sum(New.beneficiaries.of.the.month))
  
  New_beneficairies_table<- New_beneficairies%>%
    full_join(New_CVA_beneficairies, by = c("Country", "Admin1", "Reporting.Month", "Sector...Subsector...WG"))%>%
    mutate(Monthly_CVA_Consolidated = ifelse(is.na(Monthly_CVA_Consolidated), 0, Monthly_CVA_Consolidated))
  
  New_beneficiaries_sectors <- New_beneficairies_table%>%
    group_by(Country, Reporting.Month, Sector...Subsector...WG)%>%
    summarise(Monthly_Consolidated = sum(Monthly_Consolidated),
              Consolidated.RMindestination = sum(Consolidated.RMindestination),
              Consolidated.RM.in.transit = sum(Consolidated.RM.in.transit),
              Consolidated.Host.Community = sum(Consolidated.Host.Community),
              Consolidated.RM.Pendulars = sum(Consolidated.RM.Pendulars),
              Consolidated.Colombian.Returnees = sum(Consolidated.Colombian.Returnees),
              Consolidated.Women.under.18 = sum(Consolidated.Women.under.18), 
              Consolidated.Men.under.18 = sum(Consolidated.Men.under.18), 
              Consolidated.Women.above.18 = sum(Consolidated.Women.above.18), 
              Consolidated.Men.above.18 = sum( Consolidated.Men.above.18),
              Monthly_CVA_Consolidated = sum(Monthly_CVA_Consolidated))
  
  # Consolidated model to be chosen
  # Max AGE and GENDER
  # Takes the MAX of all each age and gender breakdown to define the national number of beneficiaries
  if (totalmodel == "maxagegender")
    monthly_cons_total <- New_beneficairies_table%>%
    group_by(Country, Admin1, Reporting.Month)%>%
    summarise(Sector...Subsector...WG = "All",
              Consolidated.Women.under.18 = max(Consolidated.Women.under.18), 
              Consolidated.Men.under.18 = max(Consolidated.Men.under.18), 
              Consolidated.Women.above.18 = max(Consolidated.Women.above.18), 
              Consolidated.Men.above.18 = max( Consolidated.Men.above.18),
              Monthly_CVA_Consolidated = max(Monthly_CVA_Consolidated))%>%
    rowwise()%>%
    mutate(Monthly_Consolidated = sum(Consolidated.Women.under.18,
                                      Consolidated.Men.under.18,
                                      Consolidated.Women.above.18,
                                      Consolidated.Men.above.18, na.rm = TRUE))%>%
    ungroup()%>%
    group_by(Country, Reporting.Month)%>%
    summarise(Sector...Subsector...WG = "All",
              Monthly_Consolidated = sum(Monthly_Consolidated),
              Consolidated.Women.under.18 = sum(Consolidated.Women.under.18), 
              Consolidated.Men.under.18 = sum(Consolidated.Men.under.18), 
              Consolidated.Women.above.18 = sum(Consolidated.Women.above.18), 
              Consolidated.Men.above.18 = sum( Consolidated.Men.above.18),
              Monthly_CVA_Consolidated = sum(Monthly_CVA_Consolidated))
  # SUM: sums every category and every sectors to define national figure
  else if (totalmodel == "sum")
    monthly_cons_total <- New_beneficairies_table%>%
    group_by(Country, Reporting.Month)%>%
    summarise(Sector...Subsector...WG = "All",
              Consolidated.Women.under.18 = sum(Consolidated.Women.under.18), 
              Consolidated.Men.under.18 = sum(Consolidated.Men.under.18), 
              Consolidated.Women.above.18 = sum(Consolidated.Women.above.18), 
              Consolidated.Men.above.18 = sum( Consolidated.Men.above.18),
              Consolidated.RMindestination = sum(Consolidated.RMindestination), 
              Consolidated.RM.in.transit = sum(Consolidated.RM.in.transit),
              Consolidated.Host.Community = sum(Consolidated.Host.Community),
              Consolidated.RM.Pendulars = sum(Consolidated.RM.Pendulars),
              Consolidated.Colombian.Returnees = sum(Consolidated.Colombian.Returnees),
              Monthly_CVA_Consolidated = sum(Monthly_CVA_Consolidated),
              Monthly_Consolidated = sum(Monthly_Consolidated))
  # Max Sector: takes the values of the sector with the highest total number of beneficiaries
  else if (totalmodel == "maxsector")
  {
    max_sector <- New_beneficairies_table%>%
      group_by(Country, Admin1, Reporting.Month)%>%
      slice(which.max(Monthly_Consolidated))%>%
      summarise(Reporting.Month, Sector...Subsector...WG = "All", 
                Monthly_Consolidated, 
                Consolidated.Women.under.18, 
                Consolidated.Men.under.18,
                Consolidated.Women.above.18, 
                Consolidated.Men.above.18,
                Consolidated.RMindestination, 
                Consolidated.RM.in.transit, 
                Consolidated.Host.Community,
                Consolidated.RM.Pendulars,
                Consolidated.Colombian.Returnees)%>%
      group_by(Country,  Sector...Subsector...WG, Reporting.Month)%>%
      summarise(Monthly_Consolidated = sum(Monthly_Consolidated),
                Consolidated.Women.under.18 = sum(Consolidated.Women.under.18),
                Consolidated.Men.under.18 = sum(Consolidated.Men.under.18),
                Consolidated.Women.above.18 = sum(Consolidated.Women.above.18),
                Consolidated.Men.above.18 = sum (Consolidated.Men.above.18),
                Consolidated.RMindestination = sum(Consolidated.RMindestination), 
                Consolidated.RM.in.transit = sum(Consolidated.RM.in.transit), 
                Consolidated.Host.Community = sum(Consolidated.Host.Community),
                Consolidated.RM.Pendulars = sum(Consolidated.RM.Pendulars),
                Consolidated.Colombian.Returnees = sum(Consolidated.Colombian.Returnees))
    max_cva <-  New_beneficairies_table%>%
      group_by(Country, Admin1,Reporting.Month)%>%
      slice(which.max(Monthly_CVA_Consolidated))%>%
      summarise(Reporting.Month, 
                Sector...Subsector...WG = "All",
                Monthly_CVA_Consolidated)%>%
      group_by(Country,  Sector...Subsector...WG, Reporting.Month)%>%
      summarise(Monthly_CVA_Consolidated = sum(Monthly_CVA_Consolidated))
    monthly_cons_total<- max_sector%>%
      full_join(max_cva, by = c("Country", "Reporting.Month", "Sector...Subsector...WG"))
  }
  else if(totalmodel == "conosurmodel")
    # Cono Sur model: Max across Shelter, Food security, Humanitarian transport and WASH, 
    # then protection (General) then sum Integration, Multipurporse CBI, Health, Education
  {
    total1_1 <- New_beneficairies_table%>%
      # Max across Shelter, Food security, Humanitarian transport and WASH
      filter(Sector...Subsector...WG %in% c("Shelter", "Humanitarian Transportation", "WASH", "Food Security"))%>%
      group_by(Country, Admin1, Reporting.Month)%>%
      slice(which.max(Monthly_Consolidated))%>%
      summarise(Reporting.Month, 
                Sector...Subsector...WG = "All", 
                Monthly_Consolidated, 
                Consolidated.RMindestination, 
                Consolidated.RM.in.transit,
                Consolidated.Host.Community,
                Consolidated.RM.Pendulars,
                Consolidated.Colombian.Returnees,
                Consolidated.Women.under.18, 
                Consolidated.Men.under.18,
                Consolidated.Women.above.18, 
                Consolidated.Men.above.18)%>%
      group_by(Country, Sector...Subsector...WG, Reporting.Month)%>%
      summarise(Monthly_Consolidated = sum(Monthly_Consolidated),
                Consolidated.Women.under.18 = sum(Consolidated.Women.under.18),
                Consolidated.Men.under.18 = sum(Consolidated.Men.under.18),
                Consolidated.Women.above.18 = sum(Consolidated.Women.above.18),
                Consolidated.Men.above.18 = sum (Consolidated.Men.above.18),
                Consolidated.RMindestination = sum(Consolidated.RMindestination), 
                Consolidated.RM.in.transit = sum(Consolidated.RM.in.transit), 
                Consolidated.Host.Community = sum(Consolidated.Host.Community),
                Consolidated.RM.Pendulars = sum(Consolidated.RM.Pendulars),
                Consolidated.Colombian.Returnees = sum(Consolidated.Colombian.Returnees))
    total1_2 <- New_beneficairies_table%>%
      # Max across Shelter, Food security, Humanitarian transport and WASH
      filter(Sector...Subsector...WG %in% c("Shelter", "Humanitarian Transportation", "WASH", "Food Security"))%>%
      group_by(Country, Admin1, Reporting.Month)%>%
      slice(which.max(Monthly_CVA_Consolidated))%>%
      summarise(Reporting.Month, 
                Sector...Subsector...WG = "All",
                Monthly_CVA_Consolidated)%>%
      group_by(Country, Sector...Subsector...WG, Reporting.Month)%>%
      summarise(Monthly_CVA_Consolidated = sum(Monthly_CVA_Consolidated))
    total1 <- total1_1 %>%
      full_join(total1_2, by = c("Country", "Reporting.Month", "Sector...Subsector...WG"))
    # Take Protection (General) data
    total2 <- New_beneficairies_table%>%
      filter(Sector...Subsector...WG == "Protection (General)")%>%
      group_by(Country, Reporting.Month)%>%
      summarise(Sector...Subsector...WG = "All", 
                Monthly_Consolidated = sum(Monthly_Consolidated),
                Consolidated.Women.under.18 = sum(Consolidated.Women.under.18),
                Consolidated.Men.under.18 = sum(Consolidated.Men.under.18),
                Consolidated.Women.above.18 = sum(Consolidated.Women.above.18),
                Consolidated.Men.above.18 = sum (Consolidated.Men.above.18),
                Consolidated.RMindestination = sum(Consolidated.RMindestination), 
                Consolidated.RM.in.transit = sum(Consolidated.RM.in.transit), 
                Consolidated.Host.Community = sum(Consolidated.Host.Community),
                Consolidated.RM.Pendulars = sum(Consolidated.RM.Pendulars),
                Consolidated.Colombian.Returnees = sum(Consolidated.Colombian.Returnees))
    # Sum Other sectors mentionned: Integration, Multipurporse CBI, Health, Education
    total3 <- New_beneficairies_table%>%
      filter(Sector...Subsector...WG %in% c("Integration", "Multipurpose CBI", "Health", "Education"))%>%
      group_by(Country, Reporting.Month)%>%
      summarise(Sector...Subsector...WG = "All",
                Monthly_Consolidated = sum(Monthly_Consolidated), 
                Consolidated.RMindestination = sum(Consolidated.RMindestination), 
                Consolidated.RM.in.transit = sum(Consolidated.RM.in.transit),
                Consolidated.Host.Community = sum(Consolidated.Host.Community),
                Consolidated.RM.Pendulars = sum(Consolidated.RM.Pendulars),
                Consolidated.Colombian.Returnees = sum(Consolidated.Colombian.Returnees),
                Consolidated.Women.under.18 = sum(Consolidated.Women.under.18), 
                Consolidated.Men.under.18 = sum(Consolidated.Men.under.18),
                Consolidated.Women.above.18 = sum(Consolidated.Women.above.18),
                Consolidated.Men.above.18 = sum(Consolidated.Men.above.18),
                Monthly_CVA_Consolidated = sum(Monthly_CVA_Consolidated))
    # Merge the 3 tables to get the national total
    monthly_cons_total <- rbind(total1, total2, total3)%>%
      group_by(Country, Reporting.Month)%>%
      summarise(Sector...Subsector...WG = "All",
                Monthly_Consolidated= sum(Monthly_Consolidated), 
                Consolidated.RMindestination = sum(Consolidated.RMindestination), 
                Consolidated.RM.in.transit = sum(Consolidated.RM.in.transit),
                Consolidated.Host.Community = sum(Consolidated.Host.Community),
                Consolidated.RM.Pendulars = sum(Consolidated.RM.Pendulars),
                Consolidated.Colombian.Returnees = sum(Consolidated.Colombian.Returnees),
                Consolidated.Women.under.18 = sum(Consolidated.Women.under.18), 
                Consolidated.Men.under.18 = sum(Consolidated.Men.under.18),
                Consolidated.Women.above.18 = sum(Consolidated.Women.above.18),
                Consolidated.Men.above.18 = sum(Consolidated.Men.above.18),
                Monthly_CVA_Consolidated = sum(Monthly_CVA_Consolidated))
  }
  
  # Bind between the monthly total and consolidated figures
  
  Consolidated0 <- rbind(New_beneficiaries_sectors, monthly_cons_total)
  
  Consolidated <- dftemplate %>%
    full_join(Consolidated0, by = c("Country", "Month" = "Reporting.Month", "Subsector"= "Sector...Subsector...WG"))%>%
    full_join(Monthly_benef,by = c("Country", "Month" = "Reporting.Month", "Subsector"= "Sector...Subsector...WG"))%>%
    arrange(Country, Month, desc(Subsector))%>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
  FinalConsolidated <- Consolidated%>%
    group_by(Country, Subsector)%>%
    arrange(Country, Month)%>%
    mutate('Consolidated Total' = cumsum(Monthly_Consolidated),
           'Consolidated In Destination' = cumsum(Consolidated.RMindestination),
           'Consolidated In Transit' = cumsum(Consolidated.RM.in.transit),
           'Consolidated Host Communities' = cumsum(Consolidated.Host.Community),
           'Consolidated Pendular' = cumsum(Consolidated.RM.Pendulars),
           'Consolidated Returnees' = cumsum(Consolidated.Colombian.Returnees),
           'Consolidated Girls' = cumsum(Consolidated.Women.under.18),
           'Consolidated Boys' = cumsum(Consolidated.Men.under.18),
           'Consolidated Women' = cumsum(Consolidated.Women.above.18),
           'Consolidated Men' = cumsum(Consolidated.Men.above.18),
           'Consolidated CVA Beneficiaries' = cumsum(Monthly_CVA_Consolidated))%>%
    rowwise()%>%
    # Add 2 columns to check if the breakdowns are correct. 
    mutate('Check PopType Breakdown' = ifelse(`Consolidated Total` == 0 | (`Consolidated Total` > 0 & 
                                                                             `Consolidated Total` == sum(`Consolidated In Destination`,
                                                                                                         `Consolidated In Transit`,
                                                                                                         `Consolidated Host Communities`,
                                                                                                         `Consolidated Pendular`,
                                                                                                         `Consolidated Returnees`,
                                                                                                         na.rm = TRUE)), "Ok", "Check Population Type breakdown"),
           'Check AGD Breakdown' = ifelse(`Consolidated Total` == 0 | (`Consolidated Total` > 0 & 
                                                                         `Consolidated Total`== sum(`Consolidated Girls`,
                                                                                                    `Consolidated Boys`,
                                                                                                    `Consolidated Women`,
                                                                                                    `Consolidated Men`,
                                                                                                    na.rm = TRUE)), "Ok", "Check AGD breakdown"))%>%
    ungroup()%>%
    select(Platform,
           Country, 
           Month,
           Sector,
           Subsector,
           `Monthly Total Beneficiaries`,
           `Monthly CVA Beneficiaries`,
           `Consolidated Total`,
           `Consolidated In Destination`,
           `Consolidated In Transit`,
           `Consolidated Host Communities`,
           `Consolidated Pendular`,
           `Consolidated Returnees`,
           `Consolidated Girls`,
           `Consolidated Boys`,
           `Consolidated Women`,
           `Consolidated Men`,
           `Consolidated CVA Beneficiaries`,
           `Check PopType Breakdown`, 
           `Check AGD Breakdown`)
  
  
  # Print file
  write_xlsx(FinalConsolidated, './out/RMRP_2021_AI_consolidated.xlsx')
  
  
  
  ## SHINY Please do not remove, necessary to fetch data from the function.
  return(FinalConsolidated)
  
  
  
}
