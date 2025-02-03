library(tidyverse)
library(rvest)
library(stringr)
library(purrr)
library(ggplot2)

genres <- c("classics")
most_read <- "https://www.goodreads.com/genres/most_read/"
top_50 <- "https://www.goodreads.com/shelf/show/"

top_50_urls <- tibble(genre = genres,
                        base_url = paste0(top_50, genres))

urls_to_crawl_df <- top_50_urls |>
  # create a column containing the html for each genre
  mutate(html = map(base_url, ~ read_html(.x))) |>
  # create another column containing lists of the book URLs
  mutate(book_urls = map(html, ~ {
    html_elements(.x, ".left") |>
      html_element("a") |>
      html_attr("href")})) |>
  # prepend the URLs with the base (goodreads.com)
  mutate(book_urls = map(book_urls, ~ str_c("https://www.goodreads.com", .x))) |>
  #remove the columns no longer needed (base URL and html)
  select(genre, book_urls)
  
### creating a tibble of books for each genre ###
books_df <- urls_to_crawl_df |>
  mutate(books = map(book_urls, ~ {
    tibble(url = .x,
           details = map(url, possibly(~ {
      Sys.sleep(sample(10:30, 1)) # prevents overloading the server 
                                 # (pauses 10 to 30 seconds between iterations)
      html <- read_html(.x, user_agent="Mozilla/5.0")
      list(title = html |> html_element(".Text__title1") |> html_text2(),
        author = html |> html_element(".ContributorLinksList") |> html_text2(),
        avg_rating = html |> html_element(".RatingStatistics__rating") |> html_text2(),
        ratings = html |> html_element(".RatingStatistics__centerAlign .RatingStatistics__meta span:nth-child(1)") |> html_text2(),
        reviews = html |> html_element(".RatingStatistics__centerAlign .u-dot-before") |> html_text2(),
        pages = html %>% html_element("p:nth-child(1)") %>% html_text2(),
        publication_year = html %>% html_element("p+ p") %>% html_text2())
      }, otherwise = NULL))) |>
      unnest_wider(details)
  })) |>
  select(genre, books) |>  # removing the lists of book urls
  unnest(cols=books) |>  # unnesting to finish with one data frame
  select(!url) # removing the URL column

### tidying the columns ###
books_df <- books_df |>
  mutate(pages = str_extract(pages, "^\\d+(?=\\s)"),
         ratings = str_extract(ratings, "^\\S+"),
         reviews = str_extract(reviews, "^\\S+"),
         publication_year = str_extract(publication_year, 
         "(?<=First published\\s).*")) |>
  mutate(ratings = as.numeric(gsub(",", "", ratings)),
         reviews = as.numeric(gsub(",", "", reviews)),
         avg_rating = as.numeric(avg_rating),
         pages = as.numeric(pages),
         publication_year = as.Date(publication_year, format = "%B %d, %Y"))

save(books_df, file="books_df")


