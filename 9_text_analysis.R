### Item response theory
library(tidyverse)
library(tidytext)


## Creating custom stopwords dictionary
        
        # Create a custom library
                custom_stop_words <- tribble(
                 ~ word, ~lexicon, 
                 "badword1", "CUSTOM",
                 "useless", "CUSTOM"
                )
                
        # Combine with stopwords
                stop_words2 <- stop_words %>%
                        bind_rows(custom_stop_words)