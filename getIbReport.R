
#function to get specific Ib report
getIbReport <- function(id= reportId, tok= ibToken){
    
    # current IB default
    baseUrl     <- "https://gdcdyn.interactivebrokers.com/Universal/servlet/FlexStatementService.SendRequest?t="
    version     <- "v=2"
    
    # url to query file
    queryUrl     <- paste0(baseUrl, tok, "&q=", id, "&", version)
    
    # get data and parse xml 
    xmlData <- xmlParse(GET(queryUrl))
    xmlData <- xmlRoot(xmlData) 
    xmlData <- xmlSApply(xmlData, function(x) xmlSApply(x, xmlValue)) 
    
    # url of requested report
    queryUrl <- paste0(xmlData[2], "?q=", xmlData[1], "&t=", ibToken, "&", version)
    
    # try until resulting not empty
    options(stringsAsFactors = FALSE)
    
    for (try in 1:30) {
        
        db <- read.csv(queryUrl, sep=",", header= TRUE)

        if (!grepl("FlexStatementResponse", colnames(db)[1])) break
        
        Sys.sleep(5)
    }
    
    setDT(db)
}
