# Bibliotecas -------------------------------------------------------------

library(rvest)
library(tidyverse)
library(tm)
library(lubridate)


# Cidades -----------------------------------------------------------------

cities = c( "cork", "limerick", 'galway', "waterford", 'athlone-and-surrounds-westmeath')


# Rent --------------------------------------------------------------------

rent = "https://www.daft.ie/property-for-rent/"


# Sharing -----------------------------------------------------------------

sharing = "https://www.daft.ie/sharing/"



# Teste -------------------------------------------------------------------

# Links das páginas principais
linksPrincipais <- function(linkbase, condado){
  
  link <- paste0(linkbase, condado)
  pagina <- link %>% read_html()
  
  numero_casas <- pagina %>% 
    html_nodes('h1') %>% 
    html_text() %>% 
    removePunctuation() %>% 
    str_extract_all("[:digit:]+") %>% 
    .[[1]] %>% 
    head(1) %>% 
    as.numeric()
  
  numero_paginas <- round(numero_casas/20)
  data.frame(LinkGeral = paste0(link, "?pageSize=20&from=", (0:numero_paginas)*20), Cond = condado)
}

linksGerais <- map2_dfr(.x = rent, .y = cities, .f = ~linksPrincipais(.x, .y))
linksGeraisS <- map2_dfr(.x = sharing, .y = cities, .f = ~linksPrincipais(.x, .y))
# Links dos anúncios de cada página
LinkPaginas <- function(links, cond){
  
  paginateste <- links %>% read_html()
  
  # paginas Anuncio
  paginasanuncio <- paginateste %>% 
    html_nodes("li a") %>%
    html_attr("href") %>% replace_na("") %>% 
    # keep(~str_detect(.x, "/for-rent/")) %>% 
    keep(~str_detect(.x, "/share/")) %>% 
    paste0("https://www.daft.ie/", .)
  
  data.frame(PagAnuncio = paginasanuncio, Cond = cond)
}

linksAnuncios <- map2_dfr(.x =  linksGeraisS %>% pull(LinkGeral),
                          .y = linksGeraisS %>% pull(Cond),
                          .f = ~LinkPaginas(.x, .y))

linksAnuncios_m <- linksAnuncios %>% filter(row_number() %in% c(213,226))

infoPaginasInd <- function(link, cond){
  # Sys.sleep(3)
  paginaTeste2 <- link %>% read_html()  
  
  # print(link)
  # Price
  price = paginaTeste2 %>% 
    html_nodes(xpath = "//div[@data-testid='price']") %>% 
    html_text()
  if(length(price) == 0){
    price = NA
  }
  
  # Address
  address <- paginaTeste2 %>% 
    html_nodes(xpath = "//h1[@data-testid='address']") %>% 
    html_text()
  if(length(address) == 0){ address = NA }
  # Beds
  beds <- paginaTeste2 %>% 
    html_nodes(xpath = "//p[@data-testid='beds']") %>% 
    html_text()
  if(length(beds) == 0){ beds = NA }
  # Baths
  baths <- paginaTeste2 %>% 
    html_nodes(xpath = "//p[@data-testid='baths']") %>% 
    html_text()
  if(length(baths) == 0){ baths = NA }
  # Property Type 
  prop_type <- paginaTeste2 %>% 
    html_nodes(xpath = "//p[@data-testid='property-type']") %>% 
    html_text()
  if(length(prop_type) == 0){ prop_type = NA }
  
  df <- data.frame(Price = price, Address =  address, Beds = beds,
                   Baths = baths, Prop_type = prop_type, Cond = cond)
  df
}


infototal <- data.frame(Price = character(), Address =  character(), Beds = character(),
                 Baths = character(), Prop_type = character(), Cond = character())

for(i in 1:nrow(linksAnuncios)){
  print(i)
  df <- linksAnuncios %>% filter(row_number() == i)
  infodf <- infoPaginasInd(df %>% pull(PagAnuncio),
                           df %>% pull(Cond))
  infototal <- infototal %>% bind_rows(infodf)
}

infototal <- map2(.x = linksAnuncios %>% pull(PagAnuncio),
                  .y= linksAnuncios %>% pull(Cond),
                  .f = ~infoPaginasInd(.x,.y))


# Limpeza dos dados -------------------------------------------------------

write_rds(infototal, "dadosrent.r")
infototal_arr_S <- read_rds("dadosrent.r")

infototal_arr_S <- infototal %>% 
  separate(col = Price, into = c("Price", "Tipopag"),
                       sep = "per", remove = F) %>% 
  mutate(Price = Price %>% str_replace_all(",", "") %>% 
           str_replace_all("€", "") %>% as.numeric(),
         # Beds = Beds %>% str_extract("[:digit:]"),
         # Baths = Baths %>% str_extract("[:digit:]"),
         Tipopag = Tipopag %>% str_trim()) %>% 
  filter(!is.na(Address))

infototal_arr_S$Price[12] <- 449
infototal_arr_S$Price[c(110,112,140,154,174,180,183,190,194,199,210,212,211,226,240)] <- c(400,750,450,200,400,450,600,426,700,850,450,150,500,400,500)
# Análise exploratória ----------------------------------------------------
write_rds(infototal_arr, "dadosrent.r")

infototal_arr %>% group_by(Cond) %>% summarise(Quant = n())

infototal_arr %>% 
  group_by(Cond, Tipopag) %>% 
  summarise(Quant = n(),
            PrecoRent = mean(Price))  

library(plotly)

infototal_arr %>% filter(Tipopag == "month") %>% 
  plot_ly(x = ~Price, type = "histogram", split = ~Cond)
  
# Análise Shared

write_rds(infototal_arr_S, "dadosshared.r")

infototal_arr_S %>% filter(str_detect(Beds, "Double"), Price >1) %>% 
  group_by(Cond, Tipopag) %>% 
  summarise(Quant = n(),
            PrecoRent = mean(Price))

plots = map(.x = infototal_arr_S %>% pull(Cond) %>% unique,
            .f = ~infototal_arr_S %>% filter(Cond == .x, str_detect(Beds, "Double"), Price >1) %>% 
              plot_ly(x= ~Price, type = "histogram", split = ~Tipopag))
subplot(plots, nrows = 3)


# Média de aluguel --------------------------------------------------------


Media_S <- infototal_arr_S %>% filter(str_detect(Beds, "Double"), Price >1) %>% 
  group_by(Cond, Tipopag) %>% 
  summarise(Quant = n(),
            PrecoRent = median(Price),
            Min = min(Price),
            Max = max(Price)) #%>% view
Media_R <- infototal_arr %>% 
  group_by(Cond, Tipopag) %>% 
  summarise(Quant = n(),
            PrecoRent = median(Price),
            Min = min(Price),
            Max = max(Price))  #%>% view
