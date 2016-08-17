pacman::p_load(RSelenium)
checkForServer()
startServer()


startServer(args = c("-Dwebdriver.chrome.driver=C:/Program Files (x86)/Google/Chrome/Application/chromedriver.exe")
            , log = FALSE, invisible = FALSE)
mybrowser <- remoteDriver(browser = "chrome", port = 4444)
mybrowser$open()
mybrowser$navigate("http://www.weather.gov")
mybrowser$findElement(using = 'css selector', "#inputstring")
wxbox <- mybrowser$findElement(using = 'css selector', "#inputstring")
wxbox$sendKeysToElement(list("01701"))
wxbutton <- mybrowser$findElement(using = 'css selector', "#btnSearch")
wxbutton$clickElement()
mybrowser$goBack()
wxbox <- mybrowser$findElement(using = 'css selector', "#inputstring")
wxbox$sendKeysToElement(list("01701", "\uE007"))



library(RSelenium)
pJS <- phantom()
remDr <- remoteDriver(browserName = "phantom")
remDr$open()
result <- remDr$phantomExecute("var page = this;
                                page.onLoadFinished = function(status) {
                                var url = page.url;
                                console.log(\"Status:  \" + status);
                                console.log(\"Loaded:  \" + url);
                               };")
# > remDr$navigate("http://www.google



remDr <- remoteDriver(browserName = "phantomjs")

url = "https://www.amazon.com/Jaybird-Freedom-Ear-Wireless-Headphones/dp/B01EHIQAUE"

page <- htmlParse(getURL(url), asText = T) 



title <- getNodeSet(page, '//*[@id="productTitle"]')[[1]] %>% xmlValue() %>% gsub("\n", "", .) %>% trim()

title