# Importando os pacotes que não estão instalados

install.packages(c('textdata', 'tidytext', 'zoo',
                   'Hmisc', 'sentimentr', 'flextable'))
# O pacote klippy prepara os pedaços de código e os copia para o board
install.packages("remotes")
remotes::install_github("rlesur/klippy")

# Carregando os pacotes 
# setando o options
options(stringsAsFactors = F)          # sem tranformação automática de dados
options("scipen" = 100, "digits" = 12) # suppress notações matemáticas
# activate packages
library(tidyverse)
library(readr)
library(tidytext)
library(textdata)
library(zoo)
library(Hmisc)
library(sentimentr)
library(flextable)
# activando a função copy-to-clipbpard do pacote klippy
klippy::klippy()

# Agora vou carregar a URL dos livros que serão analisados

darwin <- base::readRDS(url("https://slcladal.github.io/data/origindarwin.rda", "rb"))
twain <- base::readRDS(url("https://slcladal.github.io/data/twainhuckfinn.rda", "rb"))
orwell <- base::readRDS(url("https://slcladal.github.io/data/orwell.rda", "rb"))
lovecraft <- base::readRDS(url("https://slcladal.github.io/data/lovecraftcolor.rda", "rb"))

# Função que limpa os dados: Separa as palavras e organiza em formato df
txtclean <- function(x, title){
  require(dplyr)
  x <- x %>%
    tolower() %>%
    paste0(collapse = " ") %>%
    stringr::str_squish()%>%
    stringr::str_split(" ") %>%
    unlist() %>%
    tibble() %>%
    dplyr::select(word = 1, everything()) %>%
    dplyr::mutate(novel = title) %>%
    dplyr::anti_join(stop_words) %>%
    dplyr::mutate(word = str_remove_all(word, "\\W")) %>%
    dplyr::filter(word != "")
}

# Agora posso processar os arquivos rda
darwin_clean <- txtclean(darwin, "darwin")
lovecraft_clean <- txtclean(lovecraft, "lovecraft")
orwell_clean <- txtclean(orwell, "orwell")
twain_clean <- txtclean(twain, "twain")

## Agora para a parte da análise de sentimento

textdata:::download_functions$nrc(file.path(rappdirs::user_cache_dir("textdata"), "nrc"))

novels_anno <- rbind(darwin_clean, twain_clean, orwell_clean, lovecraft_clean) %>%
  dplyr::group_by(novel) %>%
  dplyr::mutate(words = n()) %>%
  dplyr::left_join(textdata::lexicon_nrc(manual_download = TRUE)) %>%
  dplyr::mutate(novel = factor(novel),
                sentiment = factor(sentiment))

## Agora é possível sumarizar os sentimentos e olhar a porcentagem pelos livros

novels <- novels_anno %>%
  dplyr::group_by(novel) %>%
  dplyr::group_by(novel, sentiment) %>%
  dplyr::summarise(sentiment = unique(sentiment),
                   sentiment_freq = n(),
                   words = unique(words)) %>%
  dplyr::filter(is.na(sentiment) == F) %>%
  dplyr::mutate(percentage = round(sentiment_freq/words*100, 1))

# Agora a parte mais legal: visualizando

novels %>%
  dplyr::filter(sentiment != "positive",
                sentiment != "negative") %>%
  ggplot(aes(sentiment, percentage, fill = novel)) +    
  geom_bar(stat="identity",   
           position=position_dodge()) + 
  scale_fill_manual(name = "", values=c("orange", "gray70", "red", "grey30")) +
  theme_bw() +
  theme(legend.position = "top")

# Agora visualizando com os sentimentos de forma ordenada e com cores mais intuitivas

novels %>%
  dplyr::filter(sentiment != "positive",
                sentiment != "negative") %>%
  dplyr::mutate(sentiment = factor(sentiment, 
                                   levels = c("anger", "fear", "disgust", "sadness",
                                              "surprise", "anticipation", "trust", "joy"))) %>%
  ggplot(aes(novel, percentage, fill = sentiment)) +    
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_brewer(palette = "RdBu") +
  theme_bw() +
  theme(legend.position = "right") +
  coord_flip()

# Agora identificando as principais emoções
novels_impw <- novels_anno %>%
  dplyr::filter(!is.na(sentiment),
                sentiment != "anticipation",
                sentiment != "surprise",
                sentiment != "disgust",
                sentiment != "negative",
                sentiment != "sadness",
                sentiment != "positive") %>%
  dplyr::mutate(sentiment = factor(sentiment, levels = c("anger", "fear",  "trust", "joy"))) %>%
  dplyr::group_by(novel) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  dplyr::group_by(novel, sentiment) %>%
  dplyr::top_n(3) %>%
  dplyr::mutate(score = n/sum(n))

# Agora fica mais fácil analisar as principais palavras dentro dessas emoções
novels_impw %>%
  dplyr::group_by(novel) %>%
  slice_max(score, n = 20) %>%
  dplyr::arrange(desc(score)) %>%
  dplyr::ungroup() %>%
  ggplot(aes(x = reorder(word, score), y = score, fill = word)) +
  facet_wrap(novel~sentiment, ncol = 4, scales = "free_y") +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = "Words")

# Calculando a polarização dos sentimentos
novels %>%
  dplyr::filter(sentiment == "positive" | sentiment == "negative") %>%
  dplyr::select(-percentage, -words) %>%
  dplyr::mutate(sentiment_sum = sum(sentiment_freq),
                positive = sentiment_sum-sentiment_freq) %>%
  dplyr::filter(sentiment != "positive") %>%
  dplyr::rename(negative = sentiment_freq) %>%
  dplyr::select(novel, positive, negative) %>%
  dplyr::group_by(novel) %>%
  dplyr::summarise(polarity = positive/negative) %>%
  ggplot(aes(reorder(novel, polarity, mean), polarity)) +    
  geom_point(size = 3) + 
  theme_bw() +
  labs(y = "polarity\n\nmore negative                                more positive\n",
       x = "novel")
      

# Bin
novels_bin <- novels_anno %>%
  dplyr::group_by(novel) %>%
  dplyr::filter(is.na(sentiment) | sentiment == "negative" | sentiment == "positive") %>%
  dplyr::mutate(sentiment = as.character(sentiment),
                sentiment = case_when(is.na(sentiment) ~ "0", 
                                      TRUE ~ sentiment),
                sentiment= case_when(sentiment == "0" ~ 0,
                                     sentiment == "positive" ~ 1,
                                     TRUE ~ -1),
                id = 1:n(),
                index = as.numeric(cut2(id, m=100))) %>%
  dplyr::group_by(novel, index) %>%
  dplyr::summarize(index = unique(index),
                   polarity = mean(sentiment))

# Agora um ggplot
ggplot(novels_bin, aes(index, polarity)) + 
  facet_wrap(vars(novel), scales="free_x") +
  geom_smooth(se = F, col = "black") + 
  theme_bw() +
  labs(y = "polarity ratio (mean by bin)",
       x = "index (bin)")

# media movel
novels_change <- novels_anno %>%
  dplyr::filter(is.na(sentiment) | sentiment == "negative" | sentiment == "positive") %>%
  dplyr::group_by(novel) %>%
  dplyr::mutate(sentiment = as.character(sentiment),
                sentiment = case_when(is.na(sentiment) ~ "0", 
                                      TRUE ~ sentiment),
                sentiment= case_when(sentiment == "0" ~ 0,
                                     sentiment == "positive" ~ 1,
                                     TRUE ~ -1),
                id = 1:n()) %>%
  dplyr::summarise(id = id,
                   rmean=rollapply(sentiment, 100, mean, align='right', fill=NA)) %>%
  na.omit()
# plotando as medias moveis
ggplot(novels_change, aes(id, rmean)) +    
  facet_wrap(vars(novel), scales="free_x") +
  geom_smooth(se = F, col = "black") + 
  theme_bw() +
  labs(y = "polarity ratio (rolling mean, k = 100)",
       x = "index (word in monograph)")

# Agora dividindo em sentenças e calculando a polarização de algumas delas
orwell_sent <- orwell %>%
  iconv(to = "latin1") %>%
  paste0(collapse = " ") %>%
  stringr::str_replace_all(., "([a-z])- ([a-z])", "\\1\\2") %>%
  stringr::str_squish() %>%
  tibble() %>%
  dplyr::select(text = 1, everything()) %>%
  tidytext::unnest_tokens(sentence, text, token = "sentences") %>%
  dplyr::top_n(50)
# Agora olhando os coeficientes
orwell_sent_class <- orwell_sent %>%
  dplyr::mutate(ressent = sentiment(sentence)$sentiment)

# Esse é um ótimo framewrok para análise de arquivos rda! 
# Também é util para processar arquivos txt extraidos de diversas fontes