# Načte klasifikované dotazy ze spreadsheetu a dotazy za poslední týden ze
# Search Console. Oklasifikuje ty dotazy, které ještě klasifikované nejsou a vše
# zapíše zpátky do spreadsheetu.


# Parametry ---------------------------------------------------------------

spreadsheet_id <- "1oEGm7bAmJfC_efGu-unXdhHw9AGxMTmWULslLiNWrCY"
sheet_name <- "queries"
sc_site <- "https://www.cajtydne.cz/"


# Balíčky -----------------------------------------------------------------

library(tidyverse)
library(searchConsoleR)
library(openai)
library(janitor)
library(googlesheets4)


# Prompt template ---------------------------------------------------------

prompt_template <- readr::read_lines("
Klasifikuj následující seznam vyhledávacích dotazů:

{queries}

U každého dotazu uveď, zda označuje čaj (ano/ne), a pokud ano, napiš typ čaje (černý, zelený, bílý, oolong, puerh) a zemi původu. Výsledky uspořádej do tabulky ve formátu CSV s hodnotami oddělenými čárkou.

Příklad:
dotaz,označuje čaj,typ čaje, země původu
dračí studna,ano,zelený,Čína
gaiwan,ne,,
zelený čaj,ano,zelený,
darjeeling,ano,černý,Indie
", skip = 1) |>
  paste(collapse = "\n")


# Funkce ------------------------------------------------------------------

split_vector <- function(x, n) {
  split(x, ceiling(seq_along(x) / n)) |>
    map(paste, collapse = "\n")
}

build_prompt <- function(queries, template) {
  str_glue(prompt_template, queries = queries)
}

call_ai <- function(prompt) {
  response <- openai::create_chat_completion(
    model = "gpt-3.5-turbo",
    max_tokens = 2000,
    temperature = 0,
    messages = list(list(
      "role" = "user",
      "content" = prompt
    ))
  )
  response$choices$message.content
}


# Načtení vstupních dat ---------------------------------------------------

scr_auth(email = Sys.getenv("MY_GOOGLE_ACCOUNT"))
gs4_auth(email = Sys.getenv("MY_GOOGLE_ACCOUNT"))

classified_queries <- read_sheet(spreadsheet_id, sheet_name)

new_queries <- search_analytics(
  siteURL = sc_site,
  startDate = today() - 9,
  endDate = today() - 3,
  dimensions = "query",
  dimensionFilterExp = "country==cze"
) |>
  anti_join(classified_queries, by = join_by(query)) |>
  pull(query)


# Klasifikace -------------------------------------------------------------

# Trvá dost dlouho, 278 dotazů 267 sekund

tictoc::tic()

ai_answer <- new_queries |>
  split_vector(20) |>
  map(build_prompt, template = prompt_template) |>
  map(call_ai)

tictoc::toc()


# Zápis do spreadsheetu ---------------------------------------------------

classified_queries |>
  bind_rows(
    ai_answer |>
      map(read_csv) |>
      list_rbind() |>
      janitor::clean_names() |>
      mutate(across(typ_caje:zeme_puvodu, \(x) na_if(x, ","))) |>
      rename(query = dotaz)
  ) |>
  distinct() |>
  write_sheet(spreadsheet_id, sheet_name)

