# Power-Bill-Review
Review 3 years of power bills. Shows effect of adding solar panels.


# Power Review of the last 3 years

8/2018  
by matt2001   

Tools: RStudio, R markdown  
libraries used:  
library(tidyverse)  
library(lubridate)  


* go to GRU download file and import dataset  
* [GRU for Download](https://www.gru.com/)  
* read file to smaller name:  
* dfgru <- `gru.billing.history.(3)` or other df desination  
* write the imported file to csv folder  
* write_csv(dfgru ,lookup)  
