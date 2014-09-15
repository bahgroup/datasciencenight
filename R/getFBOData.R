## Scrape FBO.Gov Data using the API Query Parameters
## See http://docs.fbopen.apiary.io/ for details

## Specify keywords, whether you want to see noncompeted and/or closed grants,
## your API key, and whether or not you want to peek at the results first (see the first 10).

library(jsonlite, warn.conflicts=F)
library(RCurl, warn.conflicts=F)
library(gtools)

getFBOGovData = function(keywords=c(), showNonCompeted=TRUE, showClosed=TRUE, my.key="", peek=FALSE) {

  # Perform an initial call to the FBOpen API to find out the total number of results
  # and test initial connectivity to the API server
  my.api.key = paste0("&api_key=",my.key)
  api.url = "https://api.data.gov/gsa/fbopen/v0/opps?"
  query= paste0("&data_source=FBO&show_noncompeted=", tolower(as.character(showNonCompeted)), 
                 "&show_closed=", tolower(as.character(showClosed)),
                 "&q=", gsub(" ","+",paste0(keywords, collapse="&"))
                )


  callback = getURL(paste0(api.url,query,my.api.key), ssl.verifypeer=F)
  num.results = fromJSON(callback)$numFound
  results = fromJSON(callback)$docs
  
  # Remove links and unlist the text content so it saves to csv cleanly
  results$links = NULL
  results$highlights = NULL
  if("content" %in% names(results)){
    results$content[unlist(lapply(results$content, is.null))] = "<NA>"
    results$content = unlist(results$content)
  }

  print(paste("Your query found", num.results, "results."))
  print("Depending on the number of results, this might take a few minutes.")


  # If user just wants a peek, return the first ten results.
  if(peek){ return(results) }  

  # Iterate through each page of results and concatenate the results as a dataframe.
  for(i in 1:floor(num.results/10)){
    if(num.results==10){ break }
    temp.call = getURL(paste0(api.url,query,"&start=",i*10, my.api.key), ssl.verifypeer=F)
    temp.data = fromJSON(temp.call)$docs

    temp.data=temp.data[,names(temp.data) %in% names(results)]
	if("content" %in% names(temp.data)){
      temp.data$content[unlist(lapply(temp.data$content, is.null))] = "<NA>"
      temp.data$content = unlist(temp.data$content)
	}

    results = smartbind(results, temp.data)
  }
  results = unique(results)
  return(results)
}
