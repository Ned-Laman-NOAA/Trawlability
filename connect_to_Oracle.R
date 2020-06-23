oracle.connect.Rstudio <- function(){
# building block script

# connects to AFSC Oracle database from Rstudio
# doesn't work in R-gui but could be written to detect RStudio or Rgui

## uses window prompts for user ID (uid) and password (pwd)
#_______________________________________________________________
require(RODBC)

channel <- odbcConnect(dsn = "AFSC", 
       uid = rstudioapi::showPrompt(title = "Username", message = "Enter Oracle Username", default = ""), 
       pwd = rstudioapi::askForPassword("Enter Oracle Password"), believeNRows = FALSE)

## checks to see if connection has been established
# odbcGetInfo(channel)
return(channel)

}
