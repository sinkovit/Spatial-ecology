loadDataframeFromMB <- function(studyid, username, password) {

      library(move)
      
      login <- movebankLogin(username=username, password=password)
      gpsdata <- getMovebankLocationData(study=study, login=login)

      return(gpsdata)
}
