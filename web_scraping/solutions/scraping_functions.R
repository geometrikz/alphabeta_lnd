get_curriculum = function(id){
  link = paste0("https://training.gov.au/Training/Details/", id)
  page = read_html(link)
  curriculum = page %>% html_node(xpath = "//h2[contains(., 'Elements and Performance Criteria')]/following-sibling::table[1]")
  if(is.na(curriculum)){
    return(NULL)
  }else{
    curriculum = curriculum %>% html_table()
    curriculum = curriculum[-c(1,2),]
    colnames(curriculum) = c("elements", "criteria")
    output = list()
    output$subject_id = id
    output$curriculum = curriculum
    return(output)
  }
}

get_program_units = function(pid){
  link = glue::glue("https://training.gov.au/Training/Details/{pid}?tableUnits-page=1&pageSizeKey=Training_Details_tableUnits&pageSize=1000&setFocus=tableUnits")
  page = read_html(link)
  
  output = page %>% html_node(xpath = "//table[@title = 'Table listing Units of Competency']")
  if(is.na(output)){
    return(NULL)
  }else{
    output = output %>% html_table()
    
    output = output[-nrow(output),]
    colnames(output) = c("Code", "Title", "Essential")
    output$Code = str_extract(output$Code, "[^ ]+")
    output$Program = pid
    return(output)
  }
}

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
  output = list()
  # return(data.frame(RTO_code, legal_name, 
  #                   trading_name, status, 
  #                   ABN, ACN, RTO_type, 
  #                   Web_address))
  output$RTO_code = RTO_code
  output$legal_name = legal_name
  output$trading_name = trading_name
  output$status = status
  output$ABN = ABN
  output$ACN = ACN
  output$RTO_type = RTO_type
  output$Web_address = Web_address
  return(output)
}