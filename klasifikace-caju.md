Klasifikace dotazů ze Search Console pomocí OpenAI API
================
Marek Prokop
7. 5. 2023

## Zadání úlohy

1.  Stáhnu data dotazů ze Search Console.
2.  Pomocí OpenAI dotazy zjednodušeně oklasifikuju a nasegmentuju.
3.  Metriky za segmenty dotazů nějak vizualizuju.
4.  Klasifikaci dotazů uložím do [Google
    Sheets](https://docs.google.com/spreadsheets/d/1oEGm7bAmJfC_efGu-unXdhHw9AGxMTmWULslLiNWrCY/edit#gid=1853344797)
    pro [Looker
    Studio](https://lookerstudio.google.com/u/0/reporting/6ecd0996-1f8e-4b4a-b26d-742285b25abb/page/p_yuweto9s5c).

## Příprava

Musíte mít přístup k [Search
Consoli](https://search.google.com/search-console) alespoň jednoho webu.
Dále musíte mít přístup k [OpenAI API](https://platform.openai.com/) a
[vygenerovaný API klíč](https://platform.openai.com/account/api-keys).

Skript počítá s tím, že máte v proměnné prostředí `MY_GOOGLE_ACCOUNT`
uloženou e-mailovou adresu svého Google účtu a v proměnné prostředí
`OPENAI_API_KEY` klíč k API OpenAI. Tyto proměnné vytvoříte např. takto:

1.  Nainstalujte si balíček *usethis*.

2.  Z konsole zadejte příkaz `usethis::edit_r_environ()`. Tím se vám v
    editoru otevře soubor *.Renviron*.

3.  Do souboru *.Renviron* přidejte tyto dva řádky s upravenými
    hodnotami:

        MY_GOOGLE_ACCOUNT = "vas.email@gmail.com"
        OPENAI_API_KEY = "váš_api_key"

4.  Soubor uložte a restartujte R (v RStudiu příkazem *Restart R* z menu
    *Session*).

## Balíčky

Připojím potřebné balíčky. Pokud je nemáte nainstalované, nainstalujte.

``` r
library(tidyverse)
library(searchConsoleR)
library(openai)
library(janitor)
library(googlesheets4)
```

## Načtení dat ze Search Console

Autorizuju Search Console API.

``` r
scr_auth(email = Sys.getenv("MY_GOOGLE_ACCOUNT"))
```

Vyberu si účet, se kterým budu pracovat. Vy si vyberte nějaký svůj.

``` r
list_websites() |> 
  filter(str_detect(siteUrl, "cajtydne"))
```

Načtu dotazy ze Search Console.

``` r
queries <- search_analytics(
  siteURL = "https://www.cajtydne.cz/",
  startDate = today() - 16,
  endDate = today() - 3,
  dimensions = "query",
  dimensionFilterExp = "country==cze"
)

queries |> 
  slice_max(order_by = impressions, n = 10)
```

                query clicks impressions         ctr position
    1             cha      0         644 0.000000000 8.613354
    2      darjeeling      2         249 0.008032129 8.413655
    3          gaiwan      5         107 0.046728972 4.915888
    4    dračí studna      0          76 0.000000000 4.697368
    5         kukicha      9          69 0.130434783 3.275362
    6         hojicha      8          52 0.153846154 1.576923
    7   gruzínský čaj      0          44 0.000000000 7.840909
    8      vodní víla      1          41 0.024390244 2.878049
    9    pivoňka bílá      0          38 0.000000000 5.526316
    10 čaj darjeeling      1          36 0.027777778 2.666667

## Klasifikace pomoci OpenAI API

Definuju šablonu promptu. Vy si definujte svou šablonu, kterou si předem
otestujte přímo v ChatGPT nebo v ještě lépe v
[Playougroundu](https://platform.openai.com/playground).

``` r
prompt_template <- read_lines("
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

cat(prompt_template)
```

    Klasifikuj následující seznam vyhledávacích dotazů:

    {queries}

    U každého dotazu uveď, zda označuje čaj (ano/ne), a pokud ano, napiš typ čaje (černý, zelený, bílý, oolong, puerh) a zemi původu. Výsledky uspořádej do tabulky ve formátu CSV s hodnotami oddělenými čárkou.

    Příklad:
    dotaz,označuje čaj,typ čaje, země původu
    dračí studna,ano,zelený,Čína
    gaiwan,ne,,
    zelený čaj,ano,zelený,
    darjeeling,ano,černý,Indie

Připravím funkci, která rozdělí dotazy do skupin po n položkách.
Výsledek bude seznam (list) znakových vektorů. Na vzorku 45 dotazů
vyzkouším, zda funkce funguje.

``` r
split_vector <- function(x, n) {
  split(x, ceiling(seq_along(x) / n)) |> 
    map(paste, collapse = "\n")
}

queries$query |> 
  head(45) |> 
  split_vector(20)
```

    $`1`
    [1] "kukicha\nhojicha\nčaj týdne\ngaiwan\nkukicha čaj\nčaj gaba\ngaba oolong\ndarjeeling\ngaba čaj\ngaba čaj účinky\nhojicha čaj účinky\nmatcha iri sencha\nya bao\nyue guang bai\nbílá pivoňka čaj\nda hong pao\ndarjeeling margaret's hope\ndian hong gong fu\ndraci studna\ndračí oči čaj"

    $`2`
    [1] "gaba caj\ngaiwan sada\ngong fu cha\nhohicha\nhojicha čaj\njaponský čaj kukicha\nkukicha čaj účinky\nlisovaný čaj\nliu bao tea\nnejlepší čaj\noolong gaba\npai mu tan\npivonka bila\nsencha matcha\nsilný zelený čaj\ntamaryokucha\ntie guan yin\nutesovy caj\nutesovy čaj\nvodní víla"

    $`3`
    [1] "vonící orchidej\nxiao zhong\nyin zhen\nzheng shan xiao zhong\nzlaty caj"

Připravím funkci, která z jedné dávky dotazů a šablony sestaví prompt.

``` r
build_prompt <- function(queries, template) {
  str_glue(prompt_template, queries = queries)
}

queries$query |> 
  head(45) |> 
  split_vector(20) |> 
  pluck(1) |> 
  build_prompt(prompt_template)
```

    Klasifikuj následující seznam vyhledávacích dotazů:

    kukicha
    hojicha
    čaj týdne
    gaiwan
    kukicha čaj
    čaj gaba
    gaba oolong
    darjeeling
    gaba čaj
    gaba čaj účinky
    hojicha čaj účinky
    matcha iri sencha
    ya bao
    yue guang bai
    bílá pivoňka čaj
    da hong pao
    darjeeling margaret's hope
    dian hong gong fu
    draci studna
    dračí oči čaj

    U každého dotazu uveď, zda označuje čaj (ano/ne), a pokud ano, napiš typ čaje (černý, zelený, bílý, oolong, puerh) a zemi původu. Výsledky uspořádej do tabulky ve formátu CSV s hodnotami oddělenými čárkou.

    Příklad:
    dotaz,označuje čaj,typ čaje, země původu
    dračí studna,ano,zelený,Čína
    gaiwan,ne,,
    zelený čaj,ano,zelený,
    darjeeling,ano,černý,Indie

Vyzkouším si vytvoření více promptů pomocí funkce map. Zároveň zvolím
vhodnou podmnožinu dotazů, abych pak API nepouštěl úplně na všechny.

``` r
queries |> 
  filter(
    impressions >= 2,
    clicks > 0
  ) |> 
  pull(query) |> 
  split_vector(20) |> 
  map(build_prompt, template = prompt_template)
```

    $`1`
    Klasifikuj následující seznam vyhledávacích dotazů:

    kukicha
    hojicha
    čaj týdne
    gaiwan
    kukicha čaj
    čaj gaba
    gaba oolong
    darjeeling
    gaba čaj
    gaba čaj účinky
    hojicha čaj účinky
    matcha iri sencha
    ya bao
    yue guang bai
    bílá pivoňka čaj
    da hong pao
    darjeeling margaret's hope
    dian hong gong fu
    draci studna
    dračí oči čaj

    U každého dotazu uveď, zda označuje čaj (ano/ne), a pokud ano, napiš typ čaje (černý, zelený, bílý, oolong, puerh) a zemi původu. Výsledky uspořádej do tabulky ve formátu CSV s hodnotami oddělenými čárkou.

    Příklad:
    dotaz,označuje čaj,typ čaje, země původu
    dračí studna,ano,zelený,Čína
    gaiwan,ne,,
    zelený čaj,ano,zelený,
    darjeeling,ano,černý,Indie

    $`2`
    Klasifikuj následující seznam vyhledávacích dotazů:

    gaba caj
    gaiwan sada
    gong fu cha
    hojicha čaj
    japonský čaj kukicha
    kukicha čaj účinky
    lisovaný čaj
    liu bao tea
    nejlepší čaj
    pai mu tan
    pivonka bila
    sencha matcha
    silný zelený čaj
    tamaryokucha
    tie guan yin
    utesovy caj
    vodní víla
    yin zhen
    zlaty caj
    útesový čaj

    U každého dotazu uveď, zda označuje čaj (ano/ne), a pokud ano, napiš typ čaje (černý, zelený, bílý, oolong, puerh) a zemi původu. Výsledky uspořádej do tabulky ve formátu CSV s hodnotami oddělenými čárkou.

    Příklad:
    dotaz,označuje čaj,typ čaje, země původu
    dračí studna,ano,zelený,Čína
    gaiwan,ne,,
    zelený čaj,ano,zelený,
    darjeeling,ano,černý,Indie

    $`3`
    Klasifikuj následující seznam vyhledávacích dotazů:

    čaj darjeeling
    čaj dračí studna
    čaj kukicha

    U každého dotazu uveď, zda označuje čaj (ano/ne), a pokud ano, napiš typ čaje (černý, zelený, bílý, oolong, puerh) a zemi původu. Výsledky uspořádej do tabulky ve formátu CSV s hodnotami oddělenými čárkou.

    Příklad:
    dotaz,označuje čaj,typ čaje, země původu
    dračí studna,ano,zelený,Čína
    gaiwan,ne,,
    zelený čaj,ano,zelený,
    darjeeling,ano,černý,Indie

Vytvořím funkci pro volání OpenAI API a zavolám API se všemi prompty.
Výsledek uložím do objektu `ai_answer` a ten si pak uložím na disk do
složky `data`, abych při opakovaném spouštění nevolal placené API
zbytečně znovu.

Když to budete poprvé zkoušet s vlastními daty, musíte před tím ze
složky `data` zrušit soubor `ai_answer.rds`. Jinak by se vám z něho
načitala moje data.

``` r
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

ai_answer_file_path <- "data/ai_answer.rds"

if (file.exists(ai_answer_file_path)) {
  ai_answer <- read_rds(ai_answer_file_path)
} else {
  ai_answer <- queries |> 
    filter(
      impressions >= 2,
      clicks > 0
    ) |> 
    pull(query) |> 
    split_vector(20) |> 
    map(build_prompt, template = prompt_template) |> 
    map(call_ai)
  
  ai_answer |> write_rds(ai_answer_file_path)
}
```

Z dat Search Console a odpovědi API vytvořím kompletní dataframe dotazů
s metrikami i klasifikací.

``` r
classified_queries <- queries |> 
  left_join(
    ai_answer |> 
      map(read_csv) |> 
      list_rbind() |> 
      janitor::clean_names() |> 
      mutate(across(typ_caje:zeme_puvodu, \(x) na_if(x, ","))),
    by = join_by(query == dotaz)
  )
```

Prozkoumám výsledky

``` r
classified_queries |> 
  filter(!is.na(oznacuje_caj)) |> 
  slice_sample(n = 10)
```

                         query clicks impressions        ctr position oznacuje_caj
    1    zheng shan xiao zhong      1           1 1.00000000 1.000000          ano
    2             tamaryokucha      1          12 0.08333333 2.166667          ano
    3                 gaba čaj      2          19 0.10526316 2.789474          ano
    4          gaba čaj účinky      2          12 0.16666667 1.000000          ano
    5              gaba oolong      3          18 0.16666667 3.833333          ano
    6               vodní víla      1          41 0.02439024 2.878049          ano
    7                   gaiwan      5         107 0.04672897 4.915888           ne
    8  zelený čaj dračí studna      0           1 0.00000000 7.000000          ano
    9        dian hong gong fu      1           5 0.20000000 1.600000          ano
    10              pai mu tan      1          28 0.03571429 7.892857          ano
       typ_caje zeme_puvodu
    1     černý        Čína
    2    zelený    Japonsko
    3    oolong   Tchaj-wan
    4    oolong   Tchaj-wan
    5    oolong   Tchaj-wan
    6     černý        Čína
    7      <NA>        <NA>
    8    zelený        Čína
    9     černý        Čína
    10     bílý        Čína

``` r
classified_queries |> count(oznacuje_caj, sort = TRUE)
```

      oznacuje_caj   n
    1         <NA> 419
    2          ano  42
    3           ne   3

``` r
classified_queries |> count(typ_caje, sort = TRUE)
```

                  typ_caje   n
    1                 <NA> 425
    2               zelený  15
    3                černý   9
    4               oolong   7
    5                 bílý   5
    6                puerh   2
    7 černý/oolong/červený   1

``` r
classified_queries |> count(zeme_puvodu, sort = TRUE)
```

        zeme_puvodu   n
    1          <NA> 425
    2          Čína  18
    3      Japonsko  11
    4     Tchaj-wan   5
    5         Indie   3
    6     neuvedeno   1
    7 Čína/Japonsko   1

## Vizualizace

Chci postupně zobrazit grafy pro všechny tři klasifikační dimenze. Abych
se neopakoval, napíšu si na to funkci.

``` r
plot_queries <- function(queries, variable) {
  queries |> 
    group_by({{variable}}) |> 
    summarise(
      n = n(),
      position = weighted.mean(position, impressions),
      impressions = sum(impressions),
      clicks = sum(clicks),
      ctr = clicks / impressions
    ) |> 
    relocate(position, .after = ctr) |> 
    pivot_longer(-{{variable}}, names_to = "metric") |> 
    mutate(metric = as_factor(metric)) |> 
    ggplot(aes(x = {{variable}}, y = value)) +
    geom_col() +
    facet_wrap(vars(metric), scales = "free", ncol = 2)
}
```

``` r
classified_queries |> 
  filter(!is.na(oznacuje_caj)) |> 
  plot_queries(oznacuje_caj)
```

![](klasifikace-caju_files/figure-commonmark/plot_oznacuje_caj-1.png)

``` r
classified_queries |> 
  filter(!is.na(typ_caje)) |> 
  plot_queries(typ_caje) +
  coord_flip()
```

![](klasifikace-caju_files/figure-commonmark/plot_typ_caje-1.png)

``` r
classified_queries |> 
  filter(!is.na(zeme_puvodu)) |> 
  plot_queries(zeme_puvodu) +
  coord_flip()
```

![](klasifikace-caju_files/figure-commonmark/plot_zeme_puvodu-1.png)

## Zápis klasifikace do Google Sheets

Vytvořte si vlastní Google spreadsheet (může být prázdný) a dejte se
jeho id místo mého. Na spreadsheet pak napojte vlastní report v Looker
Studiu podle [tohoto
vzoru](https://lookerstudio.google.com/u/0/reporting/6ecd0996-1f8e-4b4a-b26d-742285b25abb/page/p_yuweto9s5c).

``` r
spreadsheet_id <- "1oEGm7bAmJfC_efGu-unXdhHw9AGxMTmWULslLiNWrCY"
sheet_name <- "queries"
gs4_auth(email = Sys.getenv("MY_GOOGLE_ACCOUNT"))
```

``` r
classified_queries |> 
  filter(!is.na(oznacuje_caj)) |> 
  select(query, oznacuje_caj:zeme_puvodu) |> 
  write_sheet(spreadsheet_id, sheet_name)
```
