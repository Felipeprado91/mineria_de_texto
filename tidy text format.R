library(dplyr)
library(tidytext) # Para limpiar datos tipo texto
library(janeaustenr) # bbdd ejemplo
library(stringr)
library(ggplot2)
library(tibble)
library(tidyr)

## Data Frame original
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text
text_df <- tibble(line = 1:4, text = text)
text_df

## Tranformar dataframe a una fila por palabra

text_df %>%
  unnest_tokens(word, # word = nombre columna nueva
                text) #text = nombre columna original

## Data de Jane Austen
austen_books() %>% View()
## Agrupar por libros y eli
original_books <- austen_books() %>%
  group_by(book) %>% # Agrupar
  mutate(line_number = row_number(), # agregar indice a cada fila
         chapter = cumsum(
           str_detect( # busca un patron dentro de un string
              text,   # String sobre el que se cumple la condicion
              regex("^chapter [\\divxlc]") ## Patron que busca
              ))) %>%
  ungroup()


original_books

## Data tidy
tidy_books <-original_books %>% 
  unnest_tokens(word, text)

tidy_books

data("stop_words") # BBDD con palabras comunmente repetidas
stop_words


tidy_books <- tidy_books %>%
  anti_join(stop_words)  # Elimina de tidy books las palabras comunes

## Ver palabras mas repetidas
tidy_books %>% 
  count(word, sort = TRUE)


## Grafico de palabras más repetidas

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  labs(x = NULL)+
  coord_flip()



### Paquete gutemberg
install.packages("gutenbergr")
library(gutenbergr)




