library(ggplot2)
library(tidyverse)
library(lubridate)
library(scales)
library(ggridges)
library(hrbrthemes)

load("books_df")
authors_df <- read_csv("final_dataset.csv")

books_df <- books_df |>
  mutate(author= str_extract(author, "^[^,]+")) |> # only the first author
  mutate(genre = factor(genre, levels=c("high-fantasy", "romantasy",
                                      "science-fiction", "young-adult",
                                      "literary-fiction", "thriller",
                                      "classics"))) |>
  drop_na() # removing NA rows

## create the plot ##

fill_colors <- c("classics" = "#007F5F",
                 "romantasy" = "#F4A1A1",
                 "high-fantasy" = "#FF7F11",
                 "science-fiction" = "#4FD1C5",
                 "literary-fiction" = "#F3C677",
                 "thriller" = "#2A3B58",
                 "young-adult" = "#8F6AA3")

p <- ggplot(books_df, aes(avg_rating, genre,
                                fill=genre)) +
  geom_density_ridges(alpha = 0.9, linewidth=0.3) +
  theme(legend.position = "none",
        plot.background = element_rect("#F8F5EC"),
        panel.background = element_rect("#F8F5EC"))+
  scale_fill_manual(values=fill_colors)+
  labs(x = NULL,
       y = NULL)
p


## doing a one-way ANOVA to see if group differences are significant ###

# checking assumptions #
books_anova <- aov(avg_rating ~ genre, data=books_df) #creating ANOVA

# diagnostic plots #
plot(books_anova)

# assumption of hom/variances violated #
# use Welch's test instead #
welch <- oneway.test(avg_rating ~ genre, data = books_authors)

# post hoc tests to see which groups differ significantly #
library(rstatix)
ghtest <- games_howell_test(books_authors, ratings ~ genre)

