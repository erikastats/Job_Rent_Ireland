################# Trabalho Irlanda ###################
# Bibliotecas -------------------------------------------------------------

library(tidyverse)
library(rvest)


# Empregos que nos interessam ---------------------------------------------

links = c("https://www.jobs.ie/big_data_business_analysis_jobs.aspx",
          "https://www.jobs.ie/business-analyst-jobs",
          "https://www.jobs.ie/Jobs.aspx?hd_searchbutton=true&Keywords=statistician&Regions=0&Categories=0&job-search=true",
          "https://www.jobs.ie/Jobs.aspx?hd_searchbutton=true&Keywords=Data+analysis&Regions=0&Categories=0&job-search=true",
          "https://www.jobs.ie/hotels_jobs.aspx",
          "https://www.jobs.ie/pubs_bars_clubs_jobs.aspx",
          "https://www.jobs.ie/restaurants_catering_jobs.aspx",
          "https://www.jobs.ie/travel_tourism_jobs.aspx",
          "https://www.jobs.ie/biology-jobs")

# Importando informações --------------------------------------------------



info <- function(link){
pagina <- link %>% read_html()

print(link)
# pagina <- "https://www.jobs.ie/biology-jobs" %>% read_html()
ids <- pagina %>% html_nodes("section.serp-list div") %>% 
  html_attr("id") %>% 
  discard(is.na)

pagina %>% html_element("section")
# Job title
jobtitle <- map_chr(.x = ids, .f = ~pagina %>% 
      html_nodes(xpath = paste0("//*[@id='", .x,"']/div/div[2]/div[1]/a/h2" )) %>% 
      html_text)

# Company name
company_name <- map_chr(.x = ids, .f = ~pagina %>% 
          html_nodes(xpath = paste0("//*[@id='", .x,"']/div/div[2]/div[1]/span/text/a" )) %>% 
          html_text)

# Local
local <- map_chr(.x = ids, .f = ~pagina %>% 
          html_nodes(xpath = paste0("//*[@id='", .x,"']/div/div[2]/div[2]/dl/dd[1]/text()" )) %>% 
          html_text)

# Salario
salario <- map_chr(.x = ids, .f = ~pagina %>% 
          html_nodes(xpath = paste0("//*[@id='", .x,"']/div/div[2]/div[2]/dl/dd[2]/text()" )) %>% 
          html_text)

# Tipo Contrato
tipo_contrato <- map_chr(.x = ids, .f = ~pagina %>% 
          html_nodes(xpath = paste0("//*[@id='", .x,"']/div/div[2]/div[3]/dl/dd[2]/text()" )) %>% 
          html_text)

data.frame(JobTitle = jobtitle, CompanyName = company_name, Local = local,
           Salary = salario, TipoContrato = tipo_contrato, Link = link)
}

dados <- map_dfr(links, .f = info)
info("https://www.jobs.ie/business-analyst-jobs")
link = "https://www.jobs.ie/big_data_business_analysis_jobs.aspx"
