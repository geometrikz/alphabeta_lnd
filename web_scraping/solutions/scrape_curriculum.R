library(rvest)
library(stringr)
setwd("~/streamlining/")
products = readr::read_csv("products.csv")
subjects = products[products$product_type == "Unit of Competency", "code"][[1]] %>% unique()

download_page = function(id, path = "/webpages"){
  link = paste0("https://training.gov.au/Training/Details/", id)
  dest = paste0("data/subject_pages/", id, ".html")
  if(!file.exists(dest)){
  tryCatch(download.file(link, dest, method = "auto"),
           error = function(e) "download error")
  }else{
    return(paste0(dest, " already exists."))
  }
}

library(furrr)
plan(multiprocess)

check = future_map(subjects, download_page, .progress = TRUE)
check



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

library(furrr)
plan(multiprocess)

loop_timer = function(i){
  scraped_data_loop <- vector("list", length = i)
  for(i in 1:20){
    scraped_data_loop[[i]] <- get_curriculum(subjects[i])
  }
}

system.time(scraped_data <- future_map(subjects[200:250], get_curriculum, .progress = TRUE))
system.time(scraped_data_single_core <- purrr::map(subjects[1:20], get_curriculum))
system.time(loop_timer(20))

