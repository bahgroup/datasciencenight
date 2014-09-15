## Scrape Grants.Gov Data using the API Query Parameters
## See http://docs.fbopen.apiary.io/ for details

## Specify keywords, whether you want to see noncompeted and/or closed grants,
## your API key, and whether or not you want to peek at the results first (see the first 10).

library(jsonlite, warn.conflicts=F)
library(RCurl, warn.conflicts=F)

getGrantsGovData = function(keywords=c(), showNonCompeted=TRUE, showClosed=TRUE, my.key="", peek=FALSE) {

  # Perform an initial call to the FBOpen API to find out the total number of results
  # and test initial connectivity to the API server
  my.api.key = paste0("&api_key=",my.key)
  api.url = "https://api.data.gov/gsa/fbopen/v0/opps?"
  query= paste0("&data_source=grants.gov&show_noncompeted=", tolower(as.character(showNonCompeted)), 
                 "&show_closed=", tolower(as.character(showClosed)),
                 "&q=", gsub(" ","+",paste0(keywords, collapse="&"))
                )



  callback = getURL(paste0(api.url,query,my.api.key), ssl.verifypeer=F)
  num.results = fromJSON(callback)$numFound
  results = fromJSON(callback)$docs

  print(paste("Your query found", num.results, "results."))
  print("Depending on the number of results, this might take a few minutes.")

  # Definte relevant fields because the number of fields returned from each query are apparently quite 
  # variable for some reason.
  rel.fields = c("data_type","data_source","posted_dt","grants.gov_FundingInstrumentType_t",
                 "grants.gov_FundingActivityCategory_t","grants.gov_OtherCategoryExplanation_t",
                 "grants.gov_NumberOfAwards_t","grants.gov_EstimatedFunding_t","AwardCeiling_i",
                 "grants.gov_AgencyMailingAddress_t","title","solnbr","solnbr_ci","id","close_dt",
                 "ArchiveDate_dt","office","agency","description","grants.gov_CFDANumber_t",
                 "grants.gov_EligibilityCategory_t","grants.gov_AdditionalEligibilityInfo_t",
                 "grants.gov_CostSharing_t","grants.gov_ObtainFundingOppText_t","AgencyContact_t",
                 "listing_url","_version_","score","notice_type","contact")

  results = results[,rel.fields]

  # If user just wants a peek, return the first ten results.
  if(peek){ return(results) }  


  # Iterate through each page of results and concatenate the results as a dataframe.
   for(i in 1:floor(num.results/10)){
    temp.call = getURL(paste0(api.url,query,"&start=",i*10, my.api.key), ssl.verifypeer=F)
    temp.data = fromJSON(temp.call)$docs
    results = rbind(results, temp.data[,rel.fields])
  }

  results = unique(results)
  return(results)
}
