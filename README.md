# SEOloger květen 2023

## Zadání přednášky/workshopu

Chci ukázat využití R + [OpenAI API](https://platform.openai.com/overview) pro SEO. Mám na to jen hodinu a posluchačů může být až několik desítek. Měla by to tedy být spíš zjednodušená motivační ukázka, na kterou pak posluchači navážou vlastním studiem. K tomu jim dám zdroják, aby si měli doma s čím hrát.

## Zadání úlohy

1. Stáhnu data dotazů ze Search Console.
2. Pomocí OpenAI dotazy zjednodušeně oklasifikuju a nasegmentuju.
3. Metriky za segmenty dotazů nějak vizualizuju.

## Když zbyde čas

- Vyrobíme HTML report pro klienty.
- Vyrobíme skript, který pravidelně klasifikaci aktualizuje a uloží ji do Spreadsheetu pro Looker Studio.
- Ukážu alternátivní způsob klasifikace přes text embeddings.

## Vyzkoušejte si sami

1. Pokud ještě s R vůbec neumíte, prostudujte si můj návod [Od Excelu k R](https://www.prokopsw.cz/bookdown/excel-r/).
1. V RStudiu si založte projekt z tehoto repozitáře, nebo si ho stáhněte k sobě na disk.
1. Postupně projděte a přizpůsobte si soubor *klasifikace-caju.qmd*. Jsou v něm podrobnější instrukce.
1. Pak můžete prostudovat a vyzkoušet i skript na pravidelnou aktualizace v *R/reclassify-queries.R*.
