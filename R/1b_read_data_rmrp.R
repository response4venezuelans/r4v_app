#' @name pull_xlsdata
#' @rdname pull_xlsdata
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

r4v_pull_xlsdata <- function(data)
  
{
  dfxls <- data
  
  df <- dfxls %>%
    transmute(
      `Appealing.Organization.Name` = `Appealing Organization`,
      `Implementation.Set.up` = `Implementation Set-up*`,
      `Implementing.partner.Name` = `Implementing partner`,
      `Reporting.Month` = `Reporting month`,
      `Sector...Subsector...WG` = `Sector - Subsector -WG`,
      `Indicator` = `Indicator`,
      `Activity.name` = `Activity name`,
      `Activity.description` = `Activity description`,
      `RMRP.Activity` = `RMRP Activity`,
      `COVID.19.situation` = `COVID-19 situation?*`,
      `Support.Space.activity` = `Support Space activity?`,
      `CVA` = `CVA`,
      `Value.of.Transfer.in.USD` = `Value of Transfer in USD*`,
      `Delivery.Mechanism` = `Delivery Mechanism*`,
      `Country` = `Country`,
      `Admin1` = `Admin1`,
      `Quantity.of.unit.measured` = `Quantity of unit measured`,
      `Total.monthly.beneficiaries` = `Total monthly beneficiaries`,
      `New.beneficiaries.of.the.month`  = `New beneficiaries of the month`,
      `Refugees.and.Migrants.IN.DESTINATION` = `Refugees and Migrants IN DESTINATION`,
      `Refugees.and.Migrants.IN.TRANSIT` = `Refugees and Migrants IN TRANSIT`,
      `Host.Communities.Beneficiaries` =  `Host Communities beneficiaries`,
      `Refugees.and.Migrants.PENDULARS` = `Refugees and Migrants PENDULARS`,
      `Colombian.Returnees` = `Colombian returnees`,
      `Women.under.18` = `Women under 18 years old`,
      `Men.under.18` = `Men under 18 years old`,
      `Women.above.18` = `Women above 18`,
      `Men.above.18` = `Men above 18`
    )
  
  # Convert the beneficiaries related columns to numeric and change NA to 0s to ensure that the calculations are correct
  df <- df%>%
    mutate_at(c("Quantity.of.unit.measured",'Refugees.and.Migrants.IN.DESTINATION', "Refugees.and.Migrants.IN.TRANSIT","Refugees.and.Migrants.PENDULARS", "Host.Communities.Beneficiaries", "Colombian.Returnees"), as.numeric)%>%
    mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
  # Write an excel from the API data
  writexl::write_xlsx(df, './data-raw/RMRP_2021_AI_activities.xlsx')
  
  ## Please do not remove, necessary to fetch data from the function.
  return(df)
}

