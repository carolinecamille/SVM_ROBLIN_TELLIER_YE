  
library(rsconnect)

setAccountInfo(name   = Sys.getenv("shinyapps_name"),
               token  = Sys.getenv("shinyapps_token"),
               secret = Sys.getenv("shinyapps_secret"))

deployApp('C:/Users/Caroline/Documents/M2/SVM/projet/projet_final')


