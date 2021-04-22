#' @name pull_aidata
#' @rdname pull_aidata
#' @title Pull AI data from API
#' @description 
#'
#' @param query
#'
#' @return frame
#'
#' @examples
#'\dontrun{
#' pull_aidata( )
#' }
#'
#' @export
#'

r4v_pull_aidata <- function()
  
{
  # install.packages("devtools")
  #devtools::install_github("bedatadriven/activityinfo-R")
  library(activityinfo)
  library(tidyverse)
  
  ## Credentials in an external file
  source("R/activityInfoLogin.R")
  activityinfo::activityInfoLogin("fayolle@unhcr.org", "126c199a4206a96f62a3d4f88e996c33")
  
  
  ################################################
  
  # This import the 2021 database and export a simple xlsx file
  # The structure can be easily extracted when looking at the database in AI, under Export via the API > Query using R and copy
  
  
  df <- queryTable("c5ky0rxkh4wkkqc4k",
                   "Appealing Organization Name" = "c2vptrokh4xwqrzbr.name",
                   "Implementation Set up" = "cuzqigvkl2c3mps7",
                   "Implementing partner Name" = "cpfztf8kkibwjy73.name",
                   "Reporting Month" = "cqryqsjkhgnj7oo2",
                   "Sector - Subsector - WG" = "cinwvyskh4y1cd0cd.cx5m4e8kh4w1qg71n.c4vvj7fkh4vl37xz",
                   "Indicator" = "cinwvyskh4y1cd0cd.cgq71wzkh4w2u851p",
                   "Activity name" = "cex8v6hkh4y2vl1cf",
                   "Activity description" = "cfkwng0kh4y3bf4cg",
                   "COVID 19 situation" = "c8wnxr7kh4y3qwycm",
                   "RMRP Activity" = "caxtujnkh4y47ndct",
                   "Support Space activity" = "caswybqkhdxppti7",
                   "CVA" = "ctgtxuvkh4y53npd0",
                   "Value of Transfer in USD" = "cbrctvqkh4y5v15d8",
                   "Delivery Mechanism" = "chodkuekh4y7bq9de",
                   "Country" = "csuwhurkh4yus04e4.ciuuiz6kh4x0syb5g.culsq4ykh4wyojs4w",
                   "Admin1" = "csuwhurkh4yus04e4.cfb24tnkh4x1tdu5i",
                   "Quantity of unit measured" = "cqqylo5kh4z20kqe6",
                   "Total monthly beneficiaries" = "ckfpgekh4z42zve7",
                   "New beneficiaries of the month" = "cgqd7hekh4z5mjbe8",
                   "Refugees and Migrants IN DESTINATION" = "c5h04fzkh4z6yyde9",
                   "Refugees and Migrants IN TRANSIT" = "cv60fvgkh4z7qncea",
                   "Host Communities Beneficiaries" = "cyhtychkh4z9xg7ed",
                   "Refugees and Migrants PENDULARS" = "c7mqdqnkh4z88y1eb",
                   "Colombian Returnees" = "crbzvgvkh4z8zknec",
                   "Women under 18" = "clq8atikh4zasddee",
                   "Men under 18" = "catu3wckh4zbiewef",
                   "Women above 18" = "cwvbcp4kh4zc4aneg",
                   "Men above 18" = "cxh7c03kh4zcmhneh", truncate.strings = FALSE)
  
  
  # load dataframe indicator
  
  dfindicator<- queryTable("crumc80kh4w0hsw1k",
                           "CODE" = "clddcd8kh4w0npj1l",
                           "Sector - Subsector - WG" = "cx5m4e8kh4w1qg71n.c4vvj7fkh4vl37xz",
                           "Sector Objective" = "ca08bcjkh4w2eat1o",
                           "Indicator" = "cgq71wzkh4w2u851p",
                           "Indicator description /rationale" = "cwr1j9pkh4w4sy91s",
                           "CBI" = "cxk27oakh4w566l1y",
                           "PiN/Target Oriented" = "cd1xa7mkh4w5lbj25",
                           "Unit of measurement" = "cyxpbmfkh4w61jt27",
                           "People" = "co43wybkh4w6ifl2e",
                           "Collection method" = "cp18yyzkh4w704d2g",
                           "Definitions" = "cbz431wkh4w77ax2h",
                           "Means of Verification (Data source)" = "cogxg1xkh4w7gsd2i",
                           "Disaggregation" = "cipbrm1kh4wg7nr2l",
                           "Data limitations" = "cdilnv1kh4w7p8x2j",
                           "Time (Frequency)" = "cetzw1kh4w852i2k",
                           "Coverage" = "ccadnd7kljj03618",
                           "Reporting in AI" = "c18xwwokljj1z4am", truncate.strings = FALSE)
  
  # Load Country and Admin1 data for further analysis
  
  dfGIS <- queryTable("ckjn8z5kh4x09f95f",
                      "Country" = "ciuuiz6kh4x0syb5g.culsq4ykh4wyojs4w",
                      "PCODE" = "csgnygnkh4x1k8x5h",
                      "Admin1" = "cfb24tnkh4x1tdu5i",
                      "ISOCODE" = "cz0uashkh4x21e25j", truncate.strings = FALSE)
  
  # Load the Appealing organisation and Implementing partners table
  
  dforg <- queryTable("c4ba6mbkh4v6p442",
                      "IDORG" = "c5k7qoukh4v7yhi3",
                      "Name" = "name",
                      "Nombre" = "crtefhakh4v8oug4",
                      "Nome" = "cqfgoaukh4v9ejg5",
                      "Acronym/Short Name" = "cvzkt1zkh4v9snu6",
                      "Type" = "cxt8hhakh4vao1oc",
                      "Regional" = "cnivg3okkgxfji7d",
                      "Platform" = "ca4trickh4vcfy5o",
                      "Changes contrl" = "cn9r7uokm5g230i7")
  
  dfIP <- queryTable("c8j53vukkgxdcva2e",
                     "IDIP" = "ca9gplmklh387ez2",
                     "IDORG" = "c5k7qoukh4v7yhi3",
                     "Name" = "name",
                     "Nombre" = "crtefhakh4v8oug4",
                     "Nome" = "cqfgoaukh4v9ejg5",
                     "Acronym/Short Name" = "cvzkt1zkh4v9snu6",
                     "Type" = "cxt8hhakh4vao1oc",
                     "Platform" = "ca4trickh4vcfy5o",
                     "Changes Control" = "c2cpycykm5fut5z7")
  
  
  # Change months name for more readability
  
  df$Reporting.Month [df$Reporting.Month == "2021-01"] <- "January"
  df$Reporting.Month [df$Reporting.Month == "2021-02"] <- "February"
  df$Reporting.Month [df$Reporting.Month == "2021-03"] <- "March"
  df$Reporting.Month [df$Reporting.Month == "2021-04"] <- "April" 
  df$Reporting.Month [df$Reporting.Month == "2021-05"] <- "May"
  df$Reporting.Month [df$Reporting.Month == "2021-06"] <- "June"
  df$Reporting.Month [df$Reporting.Month == "2021-07"] <- "July"
  df$Reporting.Month [df$Reporting.Month == "2021-08"] <- "August"
  df$Reporting.Month [df$Reporting.Month == "2021-09"] <- "September"
  df$Reporting.Month [df$Reporting.Month == "2021-10"] <- "October"
  df$Reporting.Month [df$Reporting.Month == "2021-11"] <- "November"
  df$Reporting.Month [df$Reporting.Month == "2021-12"] <- "December"
  
  # Convert the beneficiaries related columns to numeric and change NA to 0s to ensure that the calculations are correct
  
  df <- df%>%
    mutate_at(c("Quantity.of.unit.measured",'Refugees.and.Migrants.IN.DESTINATION', "Refugees.and.Migrants.IN.TRANSIT","Refugees.and.Migrants.PENDULARS", "Host.Communities.Beneficiaries", "Colombian.Returnees"), as.numeric)
  
  df[, 17:28][is.na(df[, 17:28])] <- 0 
  
  # Drop unused columns in both indicators table
  
  dfindicator <- dfindicator %>%
    select(CODE, Sector...Subsector...WG, Indicator, CBI, PiN.Target.Oriented, People, Unit.of.measurement)%>%
    mutate(Indicator_Type = ifelse(PiN.Target.Oriented == "Yes", "Pin", NA),
           Indicator_Type = ifelse(PiN.Target.Oriented == "No" & People == "Yes", "People", Indicator_Type),
           Indicator_Type = ifelse(PiN.Target.Oriented == "No" & People == "No", "Other", Indicator_Type))
  
  # Write an excel from the API data
  
  writexl::write_xlsx(df, './data-raw/RMRP_2021_AI_activities.xlsx')
  writexl::write_xlsx(dfindicator, './data-raw/RMRP_2021_AI_indicators.xlsx')
  writexl::write_xlsx(dfGIS, './data-raw/RMRP_2021_AI_GIS.xlsx')
  writexl::write_xlsx(dforg, './data-raw/RMRP_2021_AI_partners.xlsx')
  writexl::write_xlsx(dfIP, './data-raw/RMRP_2021_AI_IP.xlsx')
  
  return(df)
  
}

