#### pacotes necessários

library(httr)
library(XML)
library(jsonlite)
library(tidyverse)
library(bRasilLegis)
library(lubridate)
library(plotly)
library(ggalluvial)
library(scales)

### obtendo todos os deputados da legislatura 55

deputados55<-obterDeputados()

### vetor com os ids dos deputados

tit_55 <- deputados55 %>%
  filter(condicao !="Suplente")

ids_55 <- tit_55$ideCadastro

### incluindo valores de titulares

ids_55 <- c(ids_55,
            178843,
            141522,
            178847,
            74141,
            74213,
            178875,
            74210,
            178900,
            141509,
            74159,
            73788,
            178935,
            72442,
            178921,
            178918,
            178924,
            178925,
            74254,
            73692,
            179001,
            178996,
            132056)

ids_55M <- unique(ids_55)

### funcao para obter dados camara

obterDetalhesDeputado1 <- function (ideCadastro,
                                    numLegislatura = "",
                                    atuacao = "bio"){
  parsedOutput <- xmlParse(GET('http://www.camara.gov.br/SitCamaraWS/Deputados.asmx/ObterDetalhesDeputado?',
                               query = list(ideCadastro = ideCadastro,
                                            numLegislatura = numLegislatura)))
  infoBasica <- xmlToDataFrame(parsedOutput)[,1:12]
  
  if (atuacao == "bio") {
    partidoAtual <- xmlToDataFrame(getNodeSet(parsedOutput, "//partidoAtual"))
    gabinete <- xmlToDataFrame(getNodeSet(parsedOutput, "//gabinete"))
    output <- cbind(infoBasica, partidoAtual, gabinete)
  }
  
  if (atuacao == "comissoes") {
    infoBasica <- infoBasica[,c(1, 8, 10)]
    output <- data.frame()
    for (i in nrow(infoBasica)) {
      comissoes <- xmlToDataFrame(getNodeSet(parsedOutput,
                                             paste("//Deputado[.//numLegislatura/text() = '",
                                                   infoBasica$numLegislatura[i],
                                                   "']//comissao",
                                                   sep = "")), stringsAsFactors = F)
      output <- bind_rows(output, merge(infoBasica, comissoes))
    }
  }
  
  if (atuacao == "cargos") {
    infoBasica <- infoBasica[,c(1, 8, 10)]
    output <- data.frame()
    for (i in nrow(infoBasica)) {
      cargos <- xmlToDataFrame(getNodeSet(parsedOutput,
                                          paste("//Deputado[.//numLegislatura/text() = '",
                                                infoBasica$numLegislatura[i],
                                                "']//cargosComissoes",
                                                sep = "")), stringsAsFactors = F)
      output <- bind_rows(output, merge(infoBasica, cargos))
    }
  }
  
  if (atuacao == "exercicios") {
    infoBasica <- infoBasica[,c(1, 8, 10)]
    output <- data.frame()
    for (i in nrow(infoBasica)) {
      periodos <- xmlToDataFrame(getNodeSet(parsedOutput,
                                            paste("//Deputado[.//numLegislatura/text() = '",
                                                  infoBasica$numLegislatura[i],
                                                  "']//periodosExercicio",
                                                  sep = "")), stringsAsFactors = F)
      output <- bind_rows(output, merge(infoBasica, periodos))
    }
  }
  
  if (atuacao == "filiacoes") {
    infoBasica <- infoBasica[,c(1, 8, 10)]
    output <- data.frame()
    for (i in nrow(infoBasica)) {
      PartidoAnterior <- xmlToDataFrame(getNodeSet(parsedOutput,
                                                   paste("//Deputado[.//numLegislatura/text() = '",
                                                         infoBasica$numLegislatura[i],
                                                         "']//siglaPartidoAnterior",
                                                         sep = "")), stringsAsFactors = F)
      
      PartidoPosterior <- xmlToDataFrame(getNodeSet(parsedOutput,
                                                    paste("//Deputado[.//numLegislatura/text() = '",
                                                          infoBasica$numLegislatura[i],
                                                          "']//siglaPartidoPosterior",
                                                          sep = "")), stringsAsFactors = F)
      
      data <- xmlToDataFrame(getNodeSet(parsedOutput,
                                        paste("//Deputado[.//numLegislatura/text() = '",
                                              infoBasica$numLegislatura[i],
                                              "']//dataFiliacaoPartidoPosterior",
                                              sep = "")), stringsAsFactors = F)
      
      
      
      output <-  cbind(infoBasica, PartidoAnterior, PartidoPosterior, data)
    }
  }
  
  if (atuacao == "lideranca") {
    infoBasica <- infoBasica[,c(1, 8, 10)]
    output <- data.frame()
    for (i in nrow(infoBasica)) {
      lideranca <- xmlToDataFrame(getNodeSet(parsedOutput,
                                             paste("//Deputado[.//numLegislatura/text() = '",
                                                   infoBasica$numLegislatura[i],
                                                   "']//historicoLider",
                                                   sep = "")), stringsAsFactors = F)
      output <- bind_rows(output, merge(infoBasica, lideranca))
    }
  }
  return(output)
}

###lop para retirar os dados

datamin<- ids_55M[1:50]
datalist=list()

safe_get <- safely(obterDetalhesDeputado1)

for (i in seq_along(ids_55M)){    
  ex<- safe_get(ids_55M[i], numLegislatura = 55, atuacao = "bio") 
  datalist[[i]]<- ex
}

dt <- datalist %>% transpose %>% .$result
base_part = do.call(rbind, dt)
write.csv(base_part, "base_part.csv")

### manipulacoes base part

n_base_part <- base_part %>% 
  group_by(sigla) %>% 
  summarise(n_titulares = n())

write.csv(n_base_part, "n_base_part.csv")
####

datalist2=list()

safe_get <- safely(obterDetalhesDeputado1)

for (i in seq_along(ids_55M)){    
  ex<- safe_get(ids_55M[i], numLegislatura = 55, atuacao = "filiacoes") 
  datalist2[[i]]<- ex
}

dt <- datalist2 %>% transpose %>% .$result
mudancas_part = do.call(rbind, dt)

### mudancas de nome

names(mudancas_part)[4:6] <- c("PartidoAnterior", "PartidoFinal", "Data")

### limpando mudancas de nome de partido

mud_part <- mudancas_part %>% 
  filter(!(PartidoAnterior=="PTN" & PartidoFinal=="PODE") & !(PartidoAnterior=="PTdoB" & PartidoFinal=="AVANTE"))

### mudando nomes dos partidos

mud_part <- mud_part%>%
  mutate(PartidoAnterior = replace(PartidoAnterior, PartidoAnterior="PTN", "PODE"))

mud_part <- mud_part%>%
  mutate(PartidoFinal = replace(PartidoFinal, PartidoFinal=="PTN", "PODE"))


mud_part <- mud_part%>%
  mutate(PartidoAnterior = replace(PartidoAnterior, PartidoAnterior=="PTdoB", "AVANTE"))

mud_part <- mud_part%>%
  mutate(PartidoFinal = replace(PartidoFinal, PartidoFinal=="PTdoB", "AVANTE"))

### comeco e fim da legislatura

mud_part$Data<-dmy(mud_part$Data)

mud_part_periodo <- mud_part %>%
  arrange(Data)%>% 
  group_by(ideCadastro, nomeParlamentarAtual) %>% 
  summarise(first_part = first(PartidoAnterior), last_part = last(PartidoFinal), n_mud=n())%>% 
  rename(`Partido Inicial`= first_part, `Partido Final`=last_part)

### deputados que comecaram e terminaram no mesmo partido

arrependidos  <- mud_part_periodo %>% 
  filter(`Partido Inicial`== `Partido Final`)

### analise mudancas de partidos no msm periodo

ana_mud_part_periodo <-  mud_part_periodo %>%
  filter(!(`Partido Inicial`== `Partido Final`))
freq_in <- mud_part_periodo %>% 
  group_by(`Partido Inicial`) %>% 
  summarise(`Nº Deputados`=n())

mud_part_periodo <- left_join(mud_part_periodo, freq_in, by=c("Partido Inicial"))

### matrix

mtx_mud_part_periodo <- ana_mud_part_periodo %>% 
  select (`Partido Inicial`, `Partido Final`) %>%
  group_by(`Partido Inicial`, `Partido Final`) %>%
  summarise(n_part=n())%>%
  spread(`Partido Final`, n_part)

n_mud_part_periodo <- ana_mud_part_periodo %>% 
  select (`Partido Inicial`, `Partido Final`) %>%
  group_by(`Partido Inicial`, `Partido Final`) %>%
  summarise(n_part=n())

### grafico

ggplot(as.data.frame(mud_part_periodo),
       aes(weight = `Nº Deputados`, axis1 = `Partido Inicial`, axis2 =`Partido Final`)) +
  geom_alluvium(aes(fill = `Partido Inicial`), width = 1/15) +
  geom_stratum(width = 1/12, fill = "grey", color = "white") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_continuous(breaks = 1:2, labels = c("Inicio", "Final")) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("Mudanças Partidárias Legislatura 55")