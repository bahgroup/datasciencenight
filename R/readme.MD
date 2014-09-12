## Quick Overview
These are a collection of R scripts related to BAH Data Science Night. If you would like to use them in your analysis, you'll need download [R](http://cran.r-project.org/) (and possibly even [RStudio](http://www.rstudio.com/products/RStudio/)) and install a few webscraping oriented packages. To do so, just run the following code in your R environment...

`install.packages(c("jsonlite","RCurl"))`

After that, all you need to do to is drop one of these functions in your working directory and call the function at the beginning of your analysis file at the beginning like this...

`source("getGrantsGovData.R")`

After you've sourced a function, you can easily call it in your analysis file.
