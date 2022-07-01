
# Bibliotecas -------------------------------------------------------------

library(rvest)
library(tidyverse)
library(tm)
library(lubridate)

# County ------------------------------------------------------------------

# link_county <- "https://en.wikipedia.org/wiki/Counties_of_Ireland"
# 
# pagina_count <- link_county %>% read_html()
# 
# county_list <- pagina_count %>% html_table(fill = TRUE) %>% 
  # .[[4]] %>% select(-c(1, 9))

# Daft --------------------------------------------------------------------



rent = "https://www.daft.ie/property-for-rent/"
sharing = "https://www.daft.ie/sharing/"
condados = c("dublin", "cork", "limerick", 'galway', "waterford", 'athlone-and-surrounds-westmeath')
links = map(.x = c(rent, sharing), .f = ~paste0(.x, condados))
# Final Paginação

final_paginacao_daft <- function(link, cond){
  
  pagina_inicial_daft <- link %>% read_html()
  
  numero_casas <- pagina_inicial_daft %>% 
    html_nodes('h1') %>% 
    html_text() %>% 
    removePunctuation() %>% 
    str_extract_all("[:digit:]+") %>% 
    .[[1]] %>% 
    head(1) %>% 
    as.numeric()
  
  numero_paginas <- round(numero_casas/20)
  data.frame(Link = link, NumeroPaginas = 20*(1:numero_paginas - 1), Condado = cond)
}

teste = map2(.x = links, .y = condados, .f = ~final_paginacao_daft(.x, .y))
fpd <- list(final_paginacao_daft(link_inicial_daft_rent), 
            final_paginacao_daft(link_inicial_daft_shared))
# PEgando os links dos anúncios

links_daft <- lapply(1:2, function(i){
  
  if (i == 1){
    link_paginas <- paste0("https://www.daft.ie/property-for-rent/ireland?location=dublin&location=cork&location=limerick&location=galway&location=waterford&location=bray-wicklow&location=navan-and-surrounds-meath&pageSize=20&from=", fpd[[i]])
  } else {
    link_paginas <- paste0("https://www.daft.ie/sharing/ireland?showMap=false&location=dublin&location=cork&location=limerick&location=galway&location=waterford&location=bray-wicklow&location=navan-and-surrounds-meath&pageSize=20&from=", fpd[[i]])
  }

  
  # importando links
  # 1 == rent
  # 2 == shared
  
  lapply(link_paginas, function(li){
    
    print(li)
    # importando página com todos anúncios
    pagina_main <- li %>% read_html()
    
    # Escolhendo se o filtro é para rent ou share
    filtro_link <- ifelse(i == 1, "/for-rent/", "/share/")
    # Verificando os links que tem o filtro
    id_rent <- pagina_main %>% 
      html_nodes("li a") %>%
      html_attr("href") %>% 
      str_detect(filtro_link)
      
    
    # Selecionando os links que tem o filtro
    link_p <- pagina_main %>% 
      html_nodes("li a") %>%
      html_attr("href") %>% 
      .[id_rent] %>% na.exclude()
    if (length(link_p)==0){
      link_p = NA
    }
    
    # Endereço
    ende <- pagina_main %>% 
      html_nodes("p.TitleBlock_Address-sc-1avkvav-7.eARcqq") %>% 
      html_text()
    if (length(ende) == 0){
      ende = NA
    }
    
    
    
    
    data.frame(link_p, Ren_Sha = ifelse(i == 1, "Rent", "Shared"),
               End = ende)
  }) %>% 
    do.call(rbind,.)
}) # Aqui


# transformando os links em data.frame
links_daft_df <- links_daft %>% 
  do.call(rbind,.)


i# Pegando Informações -----------------------------------------------------

pegandoinfo <- function(i){
  
  print(i)
  link = links_daft_df[i,1]
  
  url <- paste0("https://www.daft.ie", link)
  
  pagina_interna <-  url %>% read_html()
  
  info <- pagina_interna %>% html_nodes("div p") %>% 
    html_text()
  
  condicao <- info %>% str_detect("per ") %>% .[1]
  
  if (!condicao){
    valor <- NA
    frequencia_pagamento <- NA
    quarto <- NA
    banheiro <- NA
    tipo_construcao <- NA
    adress <- NA
    
  } else {
    valor <- info %>% 
      .[1] %>% 
      str_trim() %>% 
      removePunctuation() %>% 
      str_extract("[:digit:]+") %>% 
      as.numeric()
    
    frequencia_pagamento <- info %>% 
      .[1] %>% str_trim() %>% 
      str_replace_all("per", "") %>% 
      removePunctuation() %>% 
      str_extract("[:alpha:]+")
    
    quarto <- info %>% .[2] %>% 
      str_trim() %>% 
      str_extract("[:digit:]+") %>% 
      as.numeric()
    
    banheiro <- info %>% .[3] %>% 
      str_trim() %>% 
      str_extract("[:digit:]+") %>% 
      as.numeric()
    
    tipo_construcao <- info %>% .[4] %>% 
      str_trim() 
    
    adress <- info %>% .[5] %>% 
      str_trim()
  }
  
  data.frame(  Valor = valor, 
               Frequencia_pagamento = frequencia_pagamento,
               Quarto = quarto,
               Banheiro = banheiro,
               Tipo_construcao = tipo_construcao,
               Adress = adress,
               Tipo_aluguel = links_daft_df[i,2],
               stringsAsFactors = FALSE)
}

seq(1, nrow(links_daft_df),length.out = 5)

tabela_interm1 <- lapply(1:943, pegandoinfo)
tabela_interm2 <- lapply(944:1886, pegandoinfo)
tabela_interm3 <- lapply(1887:2829, pegandoinfo)
tabela_interm4 <- lapply(2830:3301,pegandoinfo )
tabela_interm5 <- lapply(3302:nrow(links_daft_df), pegandoinfo)


# Tranformando todos em um único data frame -------------------------------
tabela_daft <- map_dfr(.x = list(tabela_interm1, tabela_interm2,
                  tabela_interm2, tabela_interm3,
                  tabela_interm5), .f = rbind) #%>% 



# Dados completos ---------------------------------------------------------

dados_rent_df <- do.call(rbind, dados_rent) %>% na.exclude()


# Salvando dados ----------------------------------------------------------

dados <- readRDS("dados_rent.rds")
dados_rent_df  <- dados %>% bind_rows(dados_rent_df) %>% unique()
saveRDS(dados_rent_df, file = "dados_rent.rds")
