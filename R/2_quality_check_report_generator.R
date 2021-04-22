### RMRP 2021 Cleaning script and error report generator. To access the R Markdown and narrative Quality Check report, please refer to the "Quality Check RMRP 2021.Rmd" file. 

r4v_error_report <- function(data,countryname = NULL, 
                             covid = "TRUE", 
                             supportspace = "TRUE",  
                             rmrp = "TRUE", 
                             cvachange = "TRUE", 
                             pin_benef_fill = "TRUE",
                             total_lower_new = "TRUE",
                             assign_in_destination = "TRUE", 
                             assign_all_destination = "TRUE", 
                             pin_auto_apportioning = "TRUE", 
                             complete_pin_apportioning = "TRUE",
                             rounding_diff1 = "TRUE", 
                             pin_erase_quantity = "TRUE", 
                             people_fill = "TRUE",
                             total_lower_new_people = "TRUE",
                             people_erase_extradata = "TRUE",
                             other_fill = "TRUE", 
                             other_erase_extradata = "TRUE")

#r4v_error_report <- function(countryname = NULL)
{
  ## Necessary packages
  library(tidyverse)
  library(readxl)
  library(dplyr)
  library(writexl)
  
  
  # SHINY Creating a list which will return all the dataframes
  return_data <- list()
  
  ## Activity database
  
  df2021 <- read_excel('./data-raw/RMRP_2021_AI_activities.xlsx')
  
  ## Indicator database
  
  dfindicator <- read_excel( './data-raw/RMRP_2021_AI_indicators.xlsx')
  
  ## Country and Admin1 locations database
  
  dfGIS <- read_excel('./data-raw/RMRP_2021_AI_GIS.xlsx')
  
  ## Age and gender proportions per Country and Sector
  
  dfapportioning <- read_excel( './data/RMRP_2021_Apportioning.xlsx')
  
  ## Partners and IP lists
  
  dfpartner <- read_excel( './data-raw/RMRP_2021_AI_partners.xlsx')
  dfIP <- read_excel( './data-raw/RMRP_2021_AI_IP.xlsx')
  
  ##  Vector creation for categorical data check
  
  indicatorlist <- as.vector(dfindicator["Indicator"])
  sectorlist <- as.vector(unique(dfindicator["Sector...Subsector...WG"]))
  countrylist <- as.vector(unique(dfGIS["Country"]))
  admin1list <- as.vector(dfGIS["Admin1"])
  partnerlist <- as.vector(dfpartner["Name"])
  iplist <- as.vector(dfIP["Name"])
  
  ## Filter the data by the country you are interested in reviewing.
  #Shiny
  
  if (is.null(countryname) || (countryname=="All")) {
    MyCountrydf2021 <- df2021   
  } else {
    MyCountrydf2021 <- df2021 %>% filter(Country == countryname)    
  }
  
  #Some of the data quality check are linked to indicators parameters, thus we join our databases to be able to filter by those. 
  
  dfindicator2 <- MyCountrydf2021%>%
    left_join(dfindicator, by = c("Sector...Subsector...WG", "Indicator"))
  
  dfindicator2 <- dfindicator2%>%
    mutate_at(c("Quantity.of.unit.measured",'Refugees.and.Migrants.IN.DESTINATION', "Refugees.and.Migrants.IN.TRANSIT","Refugees.and.Migrants.PENDULARS", "Host.Communities.Beneficiaries", "Colombian.Returnees"), as.numeric)
  
  dfindicator2[, 17:28][is.na(dfindicator2[, 17:28])] <- 0 
  
  # The list of the errors that are checked by the RMRP Quality check report are listed in the "Data Quality Check points" file in the docs folder
  
  dferrors <- dfindicator2 %>%
    rowwise()%>%
    mutate(Missing.Appealing.org.name = ifelse(!is.na(Appealing.Organization.Name) & 
                                                 any(Appealing.Organization.Name == partnerlist), "", "ERROR"),
           Missing.Implementation.Setup = ifelse(is.na(Implementation.Set.up), "ERROR", ""),
           Missing.ImplementingPartner = ifelse(Implementation.Set.up == "Yes" & 
                                                  (is.na(Implementing.partner.Name) | 
                                                     !any(Implementing.partner.Name == iplist)), "ERROR", ""),
           Missing.Sector = ifelse(!is.na(Sector...Subsector...WG) & any(Sector...Subsector...WG == sectorlist), "", "ERROR"),
           Missing.Month = ifelse(is.na(Reporting.Month), "ERROR", ""),
           Missing.Indicator = ifelse(!is.na(Indicator) & any(Indicator == indicatorlist), "", "ERROR"),
           Missing.Activity.Name = ifelse(is.na(Activity.name), "ERROR", ""),
           Missing.covid19 = ifelse(is.na(COVID.19.situation), "ERROR", ""),
           Missing.RMRP.Activity = ifelse(is.na(RMRP.Activity), "ERROR", ""),
           Missing.CVA = ifelse(is.na(CVA), "ERROR", ""),
           Non.CVA.Indicators.with.Value = ifelse((CVA == "No" & (!is.na(Value.of.Transfer.in.USD)| Value.of.Transfer.in.USD != 0) & 
                                                     !is.na(Delivery.Mechanism)), "ERROR", ""),
           CVA.Indicator.with.Missing.Value = ifelse((CVA == "Yes" & (is.na(Value.of.Transfer.in.USD)| is.na(Delivery.Mechanism))), "ERROR", ""),
           Missing.CountryAdmin1 = ifelse((!is.na(Country) & any(Country == countrylist))&(!is.na(Admin1) & any(Admin1 == admin1list)), "", "ERROR"),
           Pin.Beneficiaries.vs.AGbreakdown = ifelse(Indicator_Type =="Pin" & (New.beneficiaries.of.the.month != 
                                                                                 (sum(Women.under.18 + Men.under.18 + Women.above.18 + 
                                                                                        Men.above.18,na.rm=TRUE))), "ERROR", ""),
           Pin.Beneficiaries.vs.pop.type = ifelse( Indicator_Type =="Pin" & (New.beneficiaries.of.the.month != sum(Refugees.and.Migrants.IN.DESTINATION + 
                                                                                                                     Refugees.and.Migrants.IN.TRANSIT + 
                                                                                                                     Host.Communities.Beneficiaries + 
                                                                                                                     Refugees.and.Migrants.PENDULARS + 
                                                                                                                     Colombian.Returnees, na.rm = TRUE)), "ERROR", ""),
           New.beneficiaries.superior.to.total = ifelse(Indicator_Type =="Pin" & New.beneficiaries.of.the.month > Total.monthly.beneficiaries, "ERROR", ""),
           People.Beneficiaries.vs.AGbreakdown = ifelse(Indicator_Type =="People" & (New.beneficiaries.of.the.month != 
                                                                                       sum(Women.under.18 + Men.under.18 + Women.above.18 + 
                                                                                             Men.above.18, na.rm = TRUE)), "ERROR", ""),
           PinPeople.Beneficaries.equal.0 = ifelse(Indicator_Type !="Other" & Total.monthly.beneficiaries == 0 & New.beneficiaries.of.the.month == 0, "ERROR", ""),
           Other.quantity.equal.0 = ifelse(Indicator_Type =="Other" & Quantity.of.unit.measured == 0, "ERROR", ""),
           Empty.data = ifelse(sum(Quantity.of.unit.measured + Total.monthly.beneficiaries +  New.beneficiaries.of.the.month +
                                     Refugees.and.Migrants.IN.DESTINATION + Refugees.and.Migrants.IN.TRANSIT + Host.Communities.Beneficiaries + 
                                     Refugees.and.Migrants.PENDULARS + Colombian.Returnees + Women.under.18 + 
                                     Men.under.18 + Women.above.18 + Men.above.18,na.rm =TRUE) == 0, "ERROR", ""))%>%
    ungroup()
  
  
  # We drop the indicator table columns to keep the error report with only the columns from the Activity Info database and the error flags
  
  dferrorslight <- dferrors%>%
    select(-(29:34))%>%
    mutate(ERROR = NA)
  
  dferrorslight$ERROR[apply(dferrorslight, 1, function(r) any(r %in% c("ERROR"))) == TRUE] <- "ERROR FOUND"
  ErrorReportActivities <- dferrorslight
  
  # the following errors check are called with anti_join functions as they correspond to mismatch between categorical values that have validation rules obeying to foreign keys in other tables
  
  ErrorSectorIndicator <-  df2021%>%
    anti_join(dfindicator, by = c("Sector...Subsector...WG", "Indicator"))%>%
    summarise(Sector...Subsector...WG, Indicator, "Number of errors Sector Indicator ", "Count" = n())
  
  ErrorCountryAdmin1 <- df2021%>%
    anti_join(dfGIS, by = c("Country", "Admin1"))%>%
    summarise(Country, Admin1, "Number of errors Country Admin1",  "Count" = n())
  
  
  writexl::write_xlsx(ErrorReportActivities, './out/ErrorReportPrecleaning.xlsx')
  
  # SHINY
  return_data$ErrorReportclean <- ErrorReportActivities
  
  if(nrow(ErrorCountryAdmin1) != 0){
    writexl::write_xlsx(ErrorCountryAdmin1, "./out/ErrorReportCountryAdmin1.xlsx")
  } 
  
  if(nrow(ErrorSectorIndicator) != 0){
    writexl::write_xlsx(ErrorSectorIndicator, './out/ErrorReportSectorIndicator.xlsx')
  }
  
  ## The following steps constitute an automatic cleaning process of the Error report.
  # By applying certain criteria, you can choose to skip or not certain of those step
  # The clean file will the go through the error detection process and be printed so you can compare both files
  
  # Fill NA cells automatically
  # COVID is NA, change to No
  
  dferrors <- dferrors%>%
    left_join(dfapportioning, by = c("Country","Sector...Subsector...WG" = "Sector"))
  
  if(covid == "TRUE") 
    dferrors <- dferrors %>% 
    mutate(
      COVID.19.situation = replace_na(COVID.19.situation, "No")
    ) 
  else if (covid == "FALSE")
    dferrors <- dferrors 
  
  # Support space is NA, change to No
  
  if(supportspace == "TRUE") 
    dferrors <- dferrors %>% 
    mutate(
      Support.Space.activity = replace_na(Support.Space.activity, 'No')
    ) 
  else if (supportspace == "FALSE")
    dferrors <- dferrors 
  
  # RMRP is NA, change to No
  if(rmrp == "TRUE")
    dferrors <- dferrors %>% 
    mutate(
      RMRP.Activity = replace_na(RMRP.Activity, 'Yes')
    )
  else if (rmrp == "FALSE")
    dferrors <- dferrors 
  
  # CVA Indicator error change: No to Yes when there is an amount in CVA
  
  if(cvachange == "TRUE")
    dferrors <- dferrors %>%
    mutate(
      CVA = ifelse(CVA == "No" & CBI == "Yes" & !is.na(Value.of.Transfer.in.USD) & Value.of.Transfer.in.USD != 0, "Yes", CVA),
      CVA = ifelse(is.na(CVA) & Value.of.Transfer.in.USD == 0 & is.na(Delivery.Mechanism), "No", CVA)
    ) 
  else if (cvachange == "FALSE")
    dferrors <- dferrors 
  
  #--------------------PIN RELATED INDICATORS CHANGES ------------------------------
  
  # Pin indicators: If New beneficiaries equals 0, copy values from breakdowns 
  # (only if they are equal) or from quantity column, if breakdowns are not equals.
  if(pin_benef_fill == "TRUE")
    dferrors <- dferrors %>%
    rowwise()%>%
    mutate(New.beneficiaries.of.the.month =  ifelse(Indicator_Type == "Pin" & New.beneficiaries.of.the.month == 0 &
                                                      sum(Women.under.18 + Men.under.18 + Women.above.18 + Men.above.18, na.rm = TRUE) == 
                                                      sum(Refugees.and.Migrants.IN.DESTINATION + Refugees.and.Migrants.IN.TRANSIT + 
                                                            Host.Communities.Beneficiaries + Refugees.and.Migrants.PENDULARS + Colombian.Returnees, na.rm = TRUE), 
                                                    sum(Women.under.18 + Men.under.18 + Women.above.18 + Men.above.18, na.rm = TRUE), New.beneficiaries.of.the.month),
           New.beneficiaries.of.the.month = ifelse(Quantity.of.unit.measured != 0 & Indicator_Type == "Pin" &
                                                     New.beneficiaries.of.the.month == 0, 
                                                   Quantity.of.unit.measured, New.beneficiaries.of.the.month)
    )%>%
    ungroup()
  else if (pin_benef_fill == "FALSE")
    dferrors <- dferrors 
  
  # For Pin Indicators, when Total is lower than New beneficiaries, copy the New beneficiaries 
  # value to total
  
  if(total_lower_new == "TRUE")
    dferrors <- dferrors %>%
    mutate( Total.monthly.beneficiaries =  ifelse(Indicator_Type == "Pin" & 
                                                    Total.monthly.beneficiaries < New.beneficiaries.of.the.month, 
                                                  New.beneficiaries.of.the.month, Total.monthly.beneficiaries)
    ) 
  else if (total_lower_new == "FALSE")
    dferrors <- dferrors 
  
  #  For PiN indicators, when the breakdown for population type is empty, assign 
  # all new beneficiaries to "Refugees and Migrants in DESTINATION" (as per guidance document)
  if(assign_in_destination == "TRUE")
    dferrors <- dferrors%>%
    rowwise()%>%
    mutate(Refugees.and.Migrants.IN.DESTINATION = ifelse(Indicator_Type == "Pin"  & New.beneficiaries.of.the.month != 0 & 
                                                           sum(Refugees.and.Migrants.IN.DESTINATION + Refugees.and.Migrants.IN.TRANSIT + 
                                                                 Host.Communities.Beneficiaries + Refugees.and.Migrants.PENDULARS + 
                                                                 Colombian.Returnees, na.rm = TRUE) == 0, 
                                                         New.beneficiaries.of.the.month, Refugees.and.Migrants.IN.DESTINATION)
    )%>%
    ungroup()
  else if (assign_in_destination == "FALSE")
    dferrors <- dferrors 
  
  # If New beneficiaries value is not equal to the population type breakdown, assign the difference 
  # to the MAX category of the breakdown
  
  var_list2 = c("Refugees.and.Migrants.IN.DESTINATION", "Refugees.and.Migrants.IN.TRANSIT", "Host.Communities.Beneficiaries", 
                "Refugees.and.Migrants.PENDULARS" , "Colombian.Returnees")
  
  if(assign_all_destination == "TRUE")
    dferrors <- dferrors %>%
    rowwise()%>%
    mutate(Refugees.and.Migrants.IN.DESTINATION = replace_na(Refugees.and.Migrants.IN.DESTINATION, 0),
           Refugees.and.Migrants.IN.TRANSIT = replace_na(Refugees.and.Migrants.IN.TRANSIT, 0),
           Refugees.and.Migrants.PENDULARS = replace_na(Refugees.and.Migrants.PENDULARS, 0),
           Host.Communities.Beneficiaries = replace_na(Host.Communities.Beneficiaries, 0),
           Colombian.Returnees = replace_na(Colombian.Returnees, 0),
           diff = New.beneficiaries.of.the.month - sum(c(Refugees.and.Migrants.IN.DESTINATION , Refugees.and.Migrants.IN.TRANSIT , 
                                                         Host.Communities.Beneficiaries , Refugees.and.Migrants.PENDULARS , 
                                                         Colombian.Returnees), na.rm = TRUE),
           row_max = var_list2[which.max(across(var_list2))])%>%
    ungroup%>%
    mutate( Refugees.and.Migrants.IN.DESTINATION = case_when((Indicator_Type == "Pin")&(row_max == "Refugees.and.Migrants.IN.DESTINATION")&(diff > 0) ~ 
                                                               (Refugees.and.Migrants.IN.DESTINATION + diff), TRUE ~ Refugees.and.Migrants.IN.DESTINATION),
            Refugees.and.Migrants.IN.TRANSIT = case_when((Indicator_Type == "Pin")&(row_max == "Refugees.and.Migrants.IN.TRANSIT")&(diff > 0) ~ 
                                                           (Refugees.and.Migrants.IN.TRANSIT + diff), TRUE ~ Refugees.and.Migrants.IN.TRANSIT),
            Refugees.and.Migrants.PENDULARS = case_when((Indicator_Type == "Pin")&(row_max == "Refugees.and.Migrants.PENDULARS")&(diff > 0) ~ 
                                                          (Refugees.and.Migrants.PENDULARS + diff), TRUE ~ Refugees.and.Migrants.PENDULARS),
            Host.Communities.Beneficiaries = case_when((Indicator_Type == "Pin")&(row_max == "Host.Communities.Beneficiaries")&(diff > 0) ~ 
                                                         (Host.Communities.Beneficiaries + diff), TRUE ~ Host.Communities.Beneficiaries),
            Colombian.Returnees = case_when((Indicator_Type == "Pin")&(row_max == "Colombian.Returnees")&(diff > 0) ~ 
                                              (Colombian.Returnees + diff), TRUE ~ Colombian.Returnees)
    )%>%
    ungroup()%>%
    select(-59, -60)
  else if (assign_all_destination == "FALSE")
    dferrors <- dferrors
  
  #  For Pin indicators, if the Age and Gender breakdown is empty, applies an automatic 
  # breakdown extracted from the Pin 2021 Country and Sector information based on new 
  # beneficiaries values
  if(pin_auto_apportioning == "TRUE")
    dferrors <- dferrors %>%
    mutate( auto_clean = ifelse(Indicator_Type == "Pin" & (Women.under.18 + Men.under.18 + 
                                                             Women.above.18 + Men.above.18) == 0, "Yes", "No"),
            Women.under.18 = ifelse(auto_clean == "Yes" & Indicator_Type == "Pin", 
                                    round((New.beneficiaries.of.the.month * Girls),digits = 0), Women.under.18),
            Men.under.18 =ifelse(auto_clean == "Yes"& Indicator_Type == "Pin",
                                 round((New.beneficiaries.of.the.month * Boys),digits =  0), Men.under.18),
            Women.above.18 = ifelse(auto_clean == "Yes"& Indicator_Type == "Pin", 
                                    round((New.beneficiaries.of.the.month * Women),digits =  0), Women.above.18),
            Men.above.18 = ifelse(auto_clean == "Yes"& Indicator_Type == "Pin", 
                                  round((New.beneficiaries.of.the.month * dferrors$Men),digits =  0), Men.above.18))%>%
    select(-59)
  else if (pin_auto_apportioning == "FALSE")
    dferrors <- dferrors 
  
  #  For Pin indicators, if the Age and Gender breakdown is empty, applies an automatic breakdown extracted from the Country and Sector information
  # based on new beneficiaries values. USE WITH CARE AS IT OVERWRITES DATA !!
  
  if(complete_pin_apportioning == "TRUE")
    dferrors <- dferrors %>%
    mutate(full_clean = ifelse(Indicator_Type == "Pin" & New.beneficiaries.of.the.month != 0 &
                                 (Women.under.18 + Men.under.18 + Women.above.18 + Men.above.18) != New.beneficiaries.of.the.month, "Yes", "No"),
           Women.under.18 = ifelse(Indicator_Type == "Pin"  & full_clean == "Yes", round((New.beneficiaries.of.the.month * Girls),digits = 0), Women.under.18),
           Men.under.18 = ifelse(Indicator_Type == "Pin"  & full_clean == "Yes",round((New.beneficiaries.of.the.month * dferrors$Boys),digits =  0), Men.under.18),
           Women.above.18 = ifelse(Indicator_Type == "Pin"  & full_clean == "Yes", round((New.beneficiaries.of.the.month * Women),digits =  0), Women.above.18),
           Men.above.18 = ifelse(Indicator_Type == "Pin"  & full_clean == "Yes", round((New.beneficiaries.of.the.month * dferrors$Men),digits =  0), Men.above.18))%>%
    select(-59)
  else if (complete_pin_apportioning == "FALSE")
    dferrors <- dferrors 
  
  # Rounding differences: due to roundings, differences in the breakdowns can be +1 or -1,
  # sum or rest this 1 difference to the max age and gender category
  
  var_list = c("Women.under.18", "Men.under.18", "Women.above.18", "Men.above.18")
  
  if(rounding_diff1 == "TRUE")
    dferrors <- dferrors %>%
    rowwise()%>%
    mutate(Women.under.18 = replace_na(Women.under.18, 0),
           Men.under.18 = replace_na(Men.under.18, 0),
           Women.above.18 = replace_na(Women.above.18, 0),
           Men.above.18 = replace_na(Men.above.18, 0),
           diff = New.beneficiaries.of.the.month - sum(c(Women.under.18, Men.under.18, Women.above.18, Men.above.18), na.rm = TRUE),
           row_max = var_list[which.max(across(var_list))])%>%
    ungroup%>%
    mutate( Women.under.18 = case_when((Indicator_Type == "Pin")&(row_max == "Women.under.18")&(abs(diff)<=2) ~ (Women.under.18 + diff), TRUE ~ Women.under.18),
            Men.under.18 = case_when((Indicator_Type == "Pin")&(row_max == "Men.under.18")&(abs(diff)<=2) ~ (Men.under.18 + diff), TRUE ~ Men.under.18),
            Women.above.18 = case_when((Indicator_Type == "Pin")&(row_max == "Women.above.18")&(abs(diff)<=2) ~ (Women.above.18 + diff), TRUE ~ Women.above.18),
            Men.above.18 = case_when((Indicator_Type == "Pin")&(row_max == "Men.above.18")&(abs(diff)<=2) ~ (Men.above.18 + diff), TRUE ~ Men.above.18)
    )%>%
    ungroup()%>%
    select(-59, -60)
  else if (rounding_diff1 == "FALSE")
    dferrors <- dferrors
  
  # For Pin indicators: as Pin indicators should not contain data in Quantity, 
  # change all values for this field to 0. USE WITH CARE AS IT ERASES DATA !!
  
  if(pin_erase_quantity == "TRUE")
    dferrors <- dferrors%>%
    mutate(Quantity.of.unit.measured = ifelse(Indicator_Type == "Pin", 0, Quantity.of.unit.measured)
    )
  else if (pin_erase_quantity == "FALSE")
    dferrors <- dferrors 
  
  ##--------------------PEOPLE RELATED INDICATORS CHANGES ------------------------------
  
  # People indicators: if New beneficaries of unit equal 0, copy values from age and gender breakdown sum 
  # or if total empty,  copy values from Quantity of unit measured
  
  if(people_fill == "TRUE")
    dferrors <- dferrors %>%
    rowwise()%>%
    mutate(New.beneficiaries.of.the.month =  ifelse(Indicator_Type == "People" & New.beneficiaries.of.the.month == 0, 
                                                    sum(Women.under.18 + Men.under.18 + 
                                                          Women.above.18 + Men.above.18, na.rm = TRUE) , New.beneficiaries.of.the.month),
           New.beneficiaries.of.the.month = ifelse(Indicator_Type == "People" & New.beneficiaries.of.the.month == 0,
                                                   Quantity.of.unit.measured, New.beneficiaries.of.the.month)
    )%>%
    ungroup()
  else if (people_fill == "FALSE")
    dferrors <- dferrors
  
  # For People Indicators, when Total is lower than New beneficiaries, copy the New beneficiaries 
  # value to total
  
  if(total_lower_new_people == "TRUE")
    dferrors <- dferrors %>%
    mutate( Total.monthly.beneficiaries =  ifelse(Indicator_Type == "People" & 
                                                    Total.monthly.beneficiaries < New.beneficiaries.of.the.month, 
                                                  New.beneficiaries.of.the.month, Total.monthly.beneficiaries)
    ) 
  else if (total_lower_new_people == "FALSE")
    dferrors <- dferrors
  
  # For People indicators: as People indicators should not contain data in population type breakdown, 
  # change all values for this field to 0. USE WITH CARE AS IT ERASES DATA !!
  
  if(people_erase_extradata == "TRUE")
    dferrors <- dferrors%>%
    mutate(Quantity.of.unit.measured = ifelse(Indicator_Type == "People", 0, Quantity.of.unit.measured),
           Refugees.and.Migrants.IN.DESTINATION = ifelse(Indicator_Type == "People", 0, Refugees.and.Migrants.IN.DESTINATION),
           Refugees.and.Migrants.IN.TRANSIT = ifelse(Indicator_Type == "People", 0, Refugees.and.Migrants.IN.TRANSIT),
           Refugees.and.Migrants.PENDULARS = ifelse(Indicator_Type == "People", 0, Refugees.and.Migrants.PENDULARS),
           Host.Communities.Beneficiaries = ifelse(Indicator_Type == "People", 0, Host.Communities.Beneficiaries),
           Colombian.Returnees = ifelse(Indicator_Type == "People", 0, Colombian.Returnees)
    )
  else if (people_erase_extradata == "FALSE")
    dferrors <- dferrors 
  
  ##--------------------OTHER INDICATORS CHANGES -------------------------------
  
  # People indicators: if Quantity of uni equal 0, copy values from Total of beneficiaries 
  
  if(other_fill == "TRUE")
    dferrors <- dferrors %>%
    mutate(Quantity.of.unit.measured =  ifelse(Indicator_Type == "Other" & Quantity.of.unit.measured == 0, 
                                               Total.monthly.beneficiaries, Quantity.of.unit.measured)
    ) 
  else if (other_fill == "FALSE")
    dferrors <- dferrors
  
  # For Other indicators: as Other indicators should not contain data in age gender and population type 
  # breakdown, change all values for this field to 0. USE WITH CARE AS IT ERASES DATA !!
  
  if(other_erase_extradata == "TRUE")
    dferrors <- dferrors%>%
    mutate(Total.monthly.beneficiaries = ifelse(Indicator_Type == "Other", 0, Total.monthly.beneficiaries),
           New.beneficiaries.of.the.month = ifelse(Indicator_Type == "Other", 0, New.beneficiaries.of.the.month),
           Refugees.and.Migrants.IN.DESTINATION = ifelse(Indicator_Type == "Other", 0, Refugees.and.Migrants.IN.DESTINATION),
           Refugees.and.Migrants.IN.TRANSIT = ifelse(Indicator_Type == "Other", 0, Refugees.and.Migrants.IN.TRANSIT),
           Refugees.and.Migrants.PENDULARS = ifelse(Indicator_Type == "Other", 0, Refugees.and.Migrants.PENDULARS),
           Host.Communities.Beneficiaries = ifelse(Indicator_Type == "Other", 0, Host.Communities.Beneficiaries),
           Colombian.Returnees = ifelse(Indicator_Type == "Other", 0, Colombian.Returnees),
           Women.under.18 = ifelse(Indicator_Type == "Other", 0, Women.under.18),
           Men.under.18 = ifelse(Indicator_Type == "Other", 0, Men.under.18),
           Women.above.18 = ifelse(Indicator_Type == "Other", 0, Women.above.18),
           Men.above.18 = ifelse(Indicator_Type == "Other", 0, Men.above.18),
    )
  else if (other_erase_extradata == "FALSE")
    dferrors <- dferrors
  
  
  ##---------------------Second Data quality check for clean data report--------
  
  
  dferrorsclean <- dferrors %>%
    rowwise()%>%
    mutate(Missing.Appealing.org.name = ifelse(!is.na(Appealing.Organization.Name) & 
                                                 any(Appealing.Organization.Name == partnerlist), "", "ERROR"),
           Missing.Implementation.Setup = ifelse(is.na(Implementation.Set.up), "ERROR", ""),
           Missing.ImplementingPartner = ifelse(Implementation.Set.up == "Yes" & 
                                                  (is.na(Implementing.partner.Name) | 
                                                     !any(Implementing.partner.Name == iplist)), "ERROR", ""),
           Missing.Sector = ifelse(!is.na(Sector...Subsector...WG) & any(Sector...Subsector...WG == sectorlist), "", "ERROR"),
           Missing.Month = ifelse(is.na(Reporting.Month), "ERROR", ""),
           Missing.Indicator = ifelse(!is.na(Indicator) & any(Indicator == indicatorlist), "", "ERROR"),
           Missing.Activity.Name = ifelse(is.na(Activity.name), "ERROR", ""),
           Missing.covid19 = ifelse(is.na(COVID.19.situation), "ERROR", ""),
           Missing.RMRP.Activity = ifelse(is.na(RMRP.Activity), "ERROR", ""),
           Missing.CVA = ifelse(is.na(CVA), "ERROR", ""),
           Non.CVA.Indicators.with.Value = ifelse((CVA == "No" & (!is.na(Value.of.Transfer.in.USD)| Value.of.Transfer.in.USD != 0) & 
                                                     !is.na(Delivery.Mechanism)), "ERROR", ""),
           CVA.Indicator.with.Missing.Value = ifelse((CVA == "Yes" & (is.na(Value.of.Transfer.in.USD)| is.na(Delivery.Mechanism))), "ERROR", ""),
           Missing.CountryAdmin1 = ifelse((!is.na(Country) & any(Country == countrylist))&(!is.na(Admin1) & any(Admin1 == admin1list)), "", "ERROR"),
           Pin.Beneficiaries.vs.AGbreakdown = ifelse(Indicator_Type =="Pin" & (New.beneficiaries.of.the.month != 
                                                                                 (sum(Women.under.18 + Men.under.18 + Women.above.18 + 
                                                                                        Men.above.18,na.rm=TRUE))), "ERROR", ""),
           Pin.Beneficiaries.vs.pop.type = ifelse( Indicator_Type =="Pin" & (New.beneficiaries.of.the.month != sum(Refugees.and.Migrants.IN.DESTINATION + 
                                                                                                                     Refugees.and.Migrants.IN.TRANSIT + 
                                                                                                                     Host.Communities.Beneficiaries + 
                                                                                                                     Refugees.and.Migrants.PENDULARS + 
                                                                                                                     Colombian.Returnees, na.rm = TRUE)), "ERROR", ""),
           New.beneficiaries.superior.to.total = ifelse(Indicator_Type =="Pin" & New.beneficiaries.of.the.month > Total.monthly.beneficiaries, "ERROR", ""),
           People.Beneficiaries.vs.AGbreakdown = ifelse(Indicator_Type =="People" & (New.beneficiaries.of.the.month != 
                                                                                       sum(Women.under.18 + Men.under.18 + Women.above.18 + 
                                                                                             Men.above.18, na.rm = TRUE)), "ERROR", ""),
           PinPeople.Beneficaries.equal.0 = ifelse(Indicator_Type !="Other" & Total.monthly.beneficiaries == 0 & New.beneficiaries.of.the.month == 0, "ERROR", ""),
           Other.quantity.equal.0 = ifelse(Indicator_Type =="Other" & Quantity.of.unit.measured == 0, "ERROR", ""),
           Empty.data = ifelse(sum(Quantity.of.unit.measured + Total.monthly.beneficiaries +  New.beneficiaries.of.the.month +
                                     Refugees.and.Migrants.IN.DESTINATION + Refugees.and.Migrants.IN.TRANSIT + Host.Communities.Beneficiaries + 
                                     Refugees.and.Migrants.PENDULARS + Colombian.Returnees + Women.under.18 + 
                                     Men.under.18 + Women.above.18 + Men.above.18,na.rm =TRUE) == 0, "ERROR", ""))%>%
    ungroup()%>%
    select(-(29:34), -(55:58))%>%
    mutate(ERROR = NA)
  
  dferrorsclean$ERROR[apply(dferrorsclean, 1, function(r) any(r %in% c("ERROR"))) == TRUE] <- "ERROR FOUND"
  
  writexl::write_xlsx(dferrorsclean, './out/ErrorReportClean.xlsx')
  
  # SHINY
  return_data$cleaned_data <- dferrorsclean
  return(return_data)
  
} 
