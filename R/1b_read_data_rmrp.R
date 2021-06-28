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
    mutate_at(c("Value.of.Transfer.in.USD", 
                "Quantity.of.unit.measured",
                'Refugees.and.Migrants.IN.DESTINATION', 
                "Refugees.and.Migrants.IN.TRANSIT",
                "Refugees.and.Migrants.PENDULARS", 
                "Host.Communities.Beneficiaries", 
                "Colombian.Returnees"), as.numeric)
  
  df[, 17:28][is.na(df[, 17:28])] <- 0 
  
  #Download partner data for update
  
  dforg <- queryTable("c4ba6mbkh4v6p442",
                      "IDORG" = "c5k7qoukh4v7yhi3",
                      "Name" = "name",
                      "Nombre" = "crtefhakh4v8oug4",
                      "Nome" = "cqfgoaukh4v9ejg5",
                      "Acronym/Short Name" = "cvzkt1zkh4v9snu6",
                      "Type" = "cxt8hhakh4vao1oc",
                      "Regional" = "cnivg3okkgxfji7d",
                      "Platform" = "ca4trickh4vcfy5o",
                      "Changes contrl" = "cn9r7uokm5g230i7", truncate.strings = FALSE)
  
  dfIP <- queryTable("c8j53vukkgxdcva2e",
                     "IDIP" = "ca9gplmklh387ez2",
                     "IDORG" = "c5k7qoukh4v7yhi3",
                     "Name" = "name",
                     "Nombre" = "crtefhakh4v8oug4",
                     "Nome" = "cqfgoaukh4v9ejg5",
                     "Acronym/Short Name" = "cvzkt1zkh4v9snu6",
                     "Type" = "cxt8hhakh4vao1oc",
                     "Platform" = "ca4trickh4vcfy5o",
                     "Changes Control" = "c2cpycykm5fut5z7", truncate.strings = FALSE)
  
  # Write an excel from the API data
  writexl::write_xlsx(df, './data-raw/RMRP_2021_AI_activities.xlsx')
  writexl::write_xlsx(dforg, './data-raw/RMRP_2021_AI_partners.xlsx')
  writexl::write_xlsx(dfIP, './data-raw/RMRP_2021_AI_IP.xlsx')
  
  ## Please do not remove, necessary to fetch data from the function.
  return(df)
}


