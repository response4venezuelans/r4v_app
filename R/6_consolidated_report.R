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
  
{
  # Aggregation model for Costa Rica - RMRP 2021
  # The agregation and calculation processed in this file are base on the information provided by the country in the RMRP 2021 Activty Info databasse. 
  

  # Paths
  df <-  read_excel('./data-raw/RMRP_2021_AI_activities.xlsx')
  dfindicator <- read_excel( './data-raw/RMRP_2021_AI_indicators.xlsx')
  

  # Table join between activities and indicator to filter by Pin oriented indicators and filter by country
  
  
  
  
  if (is.null(countryname))
    dfindicator2 <- df%>%
    left_join(dfindicator, by = c("Sector...Subsector...WG", "Indicator"))
  else
    dfindicator2 <- df%>%
    left_join(dfindicator, by = c("Sector...Subsector...WG", "Indicator"))%>%
    filter(Country == countryname)
  
  
  
  # Get the number of persons per sector of activity
  # SUM at Admin1 level
  #The aggregation model takes the SUM of all PIN oriented indicators per sector at Admin1 level
  
  
  
  monthlybreakdowntable <- dfindicator2%>%
    filter(PiN.Target.Oriented == "Yes")%>%
    group_by(Reporting.Month, Sector...Subsector...WG)%>%
    summarise(  Total = sum(New.beneficiaries.of.the.month), 
                Total.Monthly.RMindestination = sum(Refugees.and.Migrants.IN.DESTINATION), 
                Total.Monthly.RM.in.transit = sum(Refugees.and.Migrants.IN.TRANSIT),
                Total.Monthly.Host.Community = sum(Host.Communities.Beneficiaries),
                Total.Monthly.RM.Pendulars = sum(Refugees.and.Migrants.PENDULARS),
                Total.Monthly.Colombian.Returnees = sum(Colombian.Returnees),
                Total.Monthly.Women.under.18 = sum(Women.under.18), 
                Total.Monthly.Men.under.18 = sum(Men.under.18),
                Total.Monthly.Women.above.18 = sum(Women.above.18),
                Total.Monthly.Men.above.18 = sum(Men.above.18))
  
  
  
  # Table for CVA Benficiaries total
  
  
  
  CVAtable <- dfindicator2%>%
    filter(PiN.Target.Oriented == "Yes", CVA == "Yes")%>%
    mutate(Total.CVA.Beneficiaries = Women.under.18 + 
             Men.under.18 + Women.above.18 +  
             Men.above.18)%>%
    group_by(Reporting.Month, Sector...Subsector...WG)%>%
    select(Reporting.Month, Sector...Subsector...WG, Country, Total.CVA.Beneficiaries)%>%
    summarise(Total.CVA.beneficiaries = sum(Total.CVA.Beneficiaries))
  
  
  # Join the tables per month and sector
  
  
  
  beneficiariestable <-monthlybreakdowntable%>%
    full_join(CVAtable, by = c("Reporting.Month", "Sector...Subsector...WG"))
  
  
  
  #Consolidated method for total
  # Then it takes the MAX of all each age and gender breakdown to define the national number of beneficiaries
  if (totalmodel == "maxagegender")
    totalcountry <- monthlybreakdowntable%>%
    group_by(Reporting.Month)%>%
    summarise(Sector...Subsector...WG = "All",
              Total.Monthly.Women.under.18 = max(Total.Monthly.Women.under.18), 
              Total.Monthly.Men.under.18 = max(Total.Monthly.Men.under.18), 
              Total.Monthly.Women.above.18 = max(Total.Monthly.Women.above.18), 
              Total.Monthly.Men.above.18 = max( Total.Monthly.Men.above.18))%>%
    mutate(Total = Total.Monthly.Women.under.18 + Total.Monthly.Men.under.18 + 
             Total.Monthly.Women.above.18+Total.Monthly.Men.above.18)
  else if (totalmodel == "sum")
    # Then it takes the SUM of all each age and gender breakdown to define the national number of beneficiaries
    totalcountry <- monthlybreakdowntable%>%
    group_by(Reporting.Month)%>%
    summarise(  Sector...Subsector...WG = "All",
                Total.Monthly.Women.under.18 = sum(Total.Monthly.Women.under.18), 
                Total.Monthly.Men.under.18 = sum(Total.Monthly.Men.under.18), 
                Total.Monthly.Women.above.18 = sum(Total.Monthly.Women.above.18), 
                Total.Monthly.Men.above.18 = sum( Total.Monthly.Men.above.18),
                Total.Monthly.RMindestination = sum(Total.Monthly.RMindestination), 
                Total.Monthly.RM.in.transit = sum(Total.Monthly.RM.in.transit),
                Total.Monthly.Host.Community = sum(Total.Monthly.Host.Community),
                Total.Monthly.RM.Pendulars = sum(Total.Monthly.RM.Pendulars),
                Total.Monthly.Colombian.Returnees = sum(Total.Monthly.Colombian.Returnees))%>%
    mutate(Total = Total.Monthly.Women.under.18 + Total.Monthly.Men.under.18 + 
             Total.Monthly.Women.above.18+Total.Monthly.Men.above.18)
  else if (totalmodel == "maxsector")
    totalcountry <- monthlybreakdowntable%>%
    group_by(Reporting.Month)%>%
    slice(which.max(Total))%>%
    summarise(Reporting.Month, Sector...Subsector...WG = "All", Total, Total.Monthly.Women.under.18, Total.Monthly.Men.under.18,
              Total.Monthly.Women.above.18, Total.Monthly.Men.above.18)
  
  
  
  # Bind between the two tables
  
  
  
  FinalConsolidated <- rbind(beneficiariestable, totalcountry)%>%
    arrange(factor(Reporting.Month, levels = c("January", "February", "March", "April", "May", "August", "September", "October", "November", "December")))
  
  
  
  write_xlsx(FinalConsolidated, './out/RMRP_2021_AI_consolidated.xlsx')
     
     return(FinalConsolidated)
}

