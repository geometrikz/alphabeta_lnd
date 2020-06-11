setwd("~/documents/streamlining/program_scraper/")
library(rvest)
library(stringr)

programs = readr::read_csv("products.csv")
program_ids = programs[programs$product_type == "Qualification", "code"][[1]] %>% unique()

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

https://training.gov.au/Training/Details/AHC32419?tableUnits-page=1&pageSizeKey=Training_Details_tableUnits&pageSize=1000&setFocus=tableUnits

library(furrr)
plan(multiprocess)

safe_scrape = purrr::possibly(get_program_units, otherwise = NA)

safe_scrape(program_ids[105])                            

scraped_data = future_map_dfr(program_ids, get_program_units, .progress = TRUE)

scraped_data %>% write.csv("electives.csv", row.names = FALSE)