library(tidyverse)
library(rvest)

lookup_abn <- function(RTO_code) {
  tryCatch(
    {url <- paste0("https://training.gov.au/Organisation/Details/", RTO_code)
    page_html <- read_html(url)},
    error = function(e){
      return(NA)
    }
  )
  
  legal_xpath <- "/html/body/div[@id='page']/div[@id='layoutWrapper']/div[@id='oneColLayout']/div[@id='layoutContentWrapper']/div[@id='rtoDetails']/div[@id='rtoDetails-1']/div[@class='outer']/div[@class='fieldset']/div[@class='display-row'][2]/div[@class='display-field-unblocked-narrowest']"
  trading_name_xpath <- "/html/body/div[@id='page']/div[@id='layoutWrapper']/div[@id='oneColLayout']/div[@id='layoutContentWrapper']/div[@id='rtoDetails']/div[@id='rtoDetails-1']/div[@class='outer']/div[@class='fieldset']/div[@class='display-row'][3]/div[@class='display-field-unblocked-narrowest']"
  status_xpath <- "/html/body/div[@id='page']/div[@id='layoutWrapper']/div[@id='oneColLayout']/div[@id='layoutContentWrapper']/div[@id='rtoDetails']/div[@id='rtoDetails-1']/div[@class='outer']/div[@class='fieldset']/div[@class='display-row'][4]/div[@class='display-field-no-width']"
  ABN_xpath <- "/html/body/div[@id='page']/div[@id='layoutWrapper']/div[@id='oneColLayout']/div[@id='layoutContentWrapper']/div[@id='rtoDetails']/div[@id='rtoDetails-1']/div[@class='outer']/div[@class='fieldset']/div[@class='display-row'][5]/div[@class='display-field-no-width']/a"
  ACN_xpath <- "/html/body/div[@id='page']/div[@id='layoutWrapper']/div[@id='oneColLayout']/div[@id='layoutContentWrapper']/div[@id='rtoDetails']/div[@id='rtoDetails-1']/div[@class='outer']/div[@class='fieldset']/div[@class='display-row'][6]/div[@class='display-field-no-width']"
  RTO_Type_xpath <- "/html/body/div[@id='page']/div[@id='layoutWrapper']/div[@id='oneColLayout']/div[@id='layoutContentWrapper']/div[@id='rtoDetails']/div[@id='rtoDetails-1']/div[@class='outer']/div[@class='fieldset']/div[@class='display-row'][7]/div[@class='display-field-unblocked-narrowest']"
  web_address_xpath <- "/html/body/div[@id='page']/div[@id='layoutWrapper']/div[@id='oneColLayout']/div[@id='layoutContentWrapper']/div[@id='rtoDetails']/div[@id='rtoDetails-1']/div[@class='outer']/div[@class='fieldset']/div[@class='display-row'][8]/div[@class='display-field-no-width']/a"
  
  tryCatch(
    {
      legal_name <- page_html %>%
        html_node(xpath = legal_xpath) %>%
        html_text() %>%
        str_trim() 
      
      trading_name <- page_html %>%
        html_node(xpath = trading_name_xpath) %>%
        html_text() %>%
        str_trim()
      
      status <- page_html %>%
        html_node(xpath = status_xpath) %>%
        html_text() %>%
        str_trim() 
      
      ABN <- page_html %>%
        html_node(xpath = ABN_xpath) %>%
        html_text() %>%
        str_extract("[0-9\\s]+[0-9]") %>%
        str_trim() 
      
      ACN <- page_html %>%
        html_node(xpath = ACN_xpath) %>%
        html_text() %>%
        str_trim() 
      
      RTO_type <- page_html %>%
        html_node(xpath = RTO_Type_xpath) %>%
        html_text() %>%
        str_trim()
      
      Web_address <- page_html %>%
        html_node(xpath = web_address_xpath) %>%
        html_text %>%
        str_trim()
      
    },
    error = function(e) {
      NA
    }
  )
  return(data.frame(RTO_code = RTO_code, legal_name = legal_name , trading_name = trading_name, status = status, ABN = ABN, ACN = ACN, RTO_type = RTO_type, Web_address = Web_address,
                    stringsAsFactors = FALSE))
}

library(furrr)
plan(multiprocess)
library(tidyverse)

input_names <- read_csv("~/lnd/web_scraping/RTO_codes.csv", col_types = cols(.default = "c"))[[1]]
input_names = input_names[1:(length(input_names)-8)]



output <- future_map_dfr(input_names, lookup_abn, .progress = TRUE)

write_csv(output, "RTO_ABN.csv")

lookup_abn <- function(RTO_code){
  tryCatch(
    {url <- paste0("https://training.gov.au/Organisation/Details/", RTO_code)
    page_html <- read_html(url)},
    error = function(e){
      return(NA)
    })
  
  legal_xpath <- "//div[contains(., 'Legal name')]/following-sibling::div[1]"
  trading_name_xpath <- "//div[contains(., 'Trading name(s):')]/following-sibling::div[1]"
  status_xpath <- "//div[contains(., 'Status:')]/following-sibling::div[1]"
  ABN_xpath <- "//div[contains(., 'ABN:')]/following-sibling::div[1]"
  ACN_xpath <- "//div[contains(., 'ACN:')]/following-sibling::div[1]"
  RTO_Type_xpath <- "//div[contains(., 'RTO type:')]/following-sibling::div[1]"
  web_address_xpath <- "//div[contains(., 'Web address:')]/following-sibling::div[1]"
  
  tryCatch(
    {
      legal_name <- page_html %>%
        html_node(xpath = legal_xpath) %>%
        html_text() %>%
        str_trim() 
      
      trading_name <- page_html %>%
        html_node(xpath = trading_name_xpath) %>%
        html_text() %>%
        str_trim()
      
      status <- page_html %>%
        html_node(xpath = status_xpath) %>%
        html_text() %>%
        str_trim() 
      
      ABN <- page_html %>%
        html_node(xpath = ABN_xpath) %>%
        html_text() %>%
        str_extract("[0-9\\s]+[0-9]") %>%
        str_trim() 
      
      ACN <- page_html %>%
        html_node(xpath = ACN_xpath) %>%
        html_text() %>%
        str_trim() 
      
      RTO_type <- page_html %>%
        html_node(xpath = RTO_Type_xpath) %>%
        html_text() %>%
        str_trim()
      
      Web_address <- page_html %>%
        html_node(xpath = web_address_xpath) %>%
        html_text %>%
        str_trim()
      
    },
    error = function(e){NA})
  return(data.frame(RTO_code, legal_name, 
                    trading_name, status, 
                    ABN, ACN, RTO_type, 
                    Web_address))
}

lookup_abn("90304")

lookup_abn("70018")