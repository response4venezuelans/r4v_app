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
  df <- data
  
  colnames(df) <- c("Appealing.Organization.Name",
                    "Implementation.Set.up",
                    "Implementing.partner.Name",
                    "Reporting.Month",
                    "Sector...Subsector...WG",
                    "Indicator",
                    "Activity.name",
                    "Activity.description",
                    "RMRP.Activity",
                    "COVID.19.situation",
                    "Support.Space.activity",
                    "CVA",
                    "Value.of.Transfer.in.USD",
                    "Delivery.Mechanism",
                    "Country",
                    "Admin1",
                    "Quantity.of.unit.measured",
                    "Total.monthly.beneficiaries",
                    "New.beneficiaries.of.the.month",
                    "Refugees.and.Migrants.IN.DESTINATION",
                    "Refugees.and.Migrants.IN.TRANSIT",
                    "Host.Communities.Beneficiaries",
                    "Refugees.and.Migrants.PENDULARS",
                    "Colombian.Returnees",
                    "Women.under.18",
                    "Men.under.18",
                    "Women.above.18",
                    "Men.above.18"
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

