## ----Set random seed, include=FALSE--------------------------------------------------------------
seed <- 2025-04-07


## ----Load packages, include=FALSE----------------------------------------------------------------
library(tidyverse)
library(knitr)
library(kableExtra)
library(ggpubr)
library(ellmer)
library(pbapply)


## ----Add an AI agent, include=FALSE--------------------------------------------------------------
# llama3 <- chat_ollama(model = "llama3", base_url = Sys.getenv("OLLAMA_HOST"))


## ----Load functions, include=FALSE---------------------------------------------------------------
lapply(list.files("R/", full.names = TRUE), source)


## ----Set theme, include=FALSE--------------------------------------------------------------------
my_theme_set()


## ----Session information, echo=FALSE-------------------------------------------------------------
sessionInfo()


## ----Raw data paths, include=FALSE---------------------------------------------------------------
raw_data_paths <- list.files("inst/extdata/raw", full.names = TRUE)


## ----Raw data files - create, include=FALSE------------------------------------------------------
raw_data_files <-
  data.frame(path = raw_data_paths) |>
  mutate(no = seq(n())) |>
  select(no, everything()) |>
  rename_all(str_to_sentence)


## ----Raw data files - show, echo=FALSE-----------------------------------------------------------
raw_data_files |>
  show_table("raw_data_files", "Raw data files.")


## ----Read raw data, include=FALSE----------------------------------------------------------------
raw_data <-
  raw_data_paths |>
  `names<-`(
    str_remove_all(raw_data_paths, "inst/extdata/raw/|\\.[:alpha:]+$")
  ) |>
  lapply(read_data)


## ----Raw data column names - old, include=FALSE--------------------------------------------------
raw_data_old_colname <- lapply(raw_data, prep_attribute_list, "colname")


## ----Raw data column names - write, eval=FALSE, include=FALSE------------------------------------
## raw_data_old_colname |>
##   imap(~ write_csv(.x, paste0("inst/extdata/colname/", .y,".csv")))


## ----Raw data column names - new, include=FALSE--------------------------------------------------
raw_data_new_colname <-
  raw_data |>
  imap(
    ~ paste0("inst/extdata/colname/new_", .y, ".csv") |>
      read_csv(show_col_types = FALSE)
  )


## ----Raw data column names - show, echo=FALSE----------------------------------------------------
raw_data_new_colname |>
  imap(~ data.frame(filename = .y) |> cbind(.x)) |>
  reduce(rbind) |>
  show_table(
    "raw_data_new_colname"
    , "Raw data old and new column names."
    , max_height = "250px"
  )


## ----Raw data column names - update, include=FALSE-----------------------------------------------
upd_colname_data <-
  raw_data |>
  imap(~ prep_colname(.x, raw_data_new_colname[[.y]])) |>
  lapply(prep_coltype, remove_haven_label = TRUE)


## ----Colname-updated data column types - old, include=FALSE--------------------------------------
upd_colname_data_old_coltype <-
  lapply(upd_colname_data, prep_attribute_list, "coltype")


## ----Colname-updated data column types - write, eval=FALSE, include=FALSE------------------------
## upd_colname_data_old_coltype |>
##   imap(~ write_csv(.x, paste0("inst/extdata/coltype/", .y,".csv")))


## ----Colname-updated data column types - new, include=FALSE--------------------------------------
upd_colname_data_new_coltype <-
  upd_colname_data |>
  imap(
    ~ paste0("inst/extdata/coltype/new_", .y, ".csv") |>
      read_csv(show_col_types = FALSE)
  )


## ----Colname-updated data column types - show, echo=FALSE----------------------------------------
upd_colname_data_new_coltype |>
  imap(~ data.frame(filename = .y) |> cbind(.x)) |>
  reduce(rbind) |>
  show_table(
    "upd_colname_data_new_coltype"
    , "Column name-updated data old and new column types"
    , max_height = "250px"
  )


## ----Colname-updated data column types - update, include=FALSE-----------------------------------
upd_coltype_data <-
  upd_colname_data |>
  imap(~ prep_coltype(.x, upd_colname_data_new_coltype[[.y]]))


## ----Split data, include=FALSE-------------------------------------------------------------------
strata_name <-
  list(
    `0_Demographic+surfactant+Growth_838_DR_Su_20240813` = "ga"
    , `03 FKTP Non Kapitasi 260819` = NULL
    , data_batch1_moca_ina = NULL
    , raw_data3 = NULL
  )

whole_id <-
  upd_coltype_data |>
  imap(~ split_data(.x, seed, c("test", "dev"), 0.2, strata = strata_name[[.y]]))

dev_id <-
  upd_coltype_data |>
  imap(~ slice(.x, whole_id[[.y]]$dev)) |>
  imap(~ split_data(.x, seed, c("val", "train"), 0.2, strata = strata_name[[.y]]))

test_id <- lapply(whole_id, \(x) x$test)
val_id <- lapply(dev_id, \(x) x$val)
train_id <- lapply(dev_id, \(x) x$train)


## ----figure-1, echo=FALSE, fig.height=5, fig.width=10--------------------------------------------
upd_coltype_data |>
  imap(
    ~ list(
        Train = train_id[[.y]]
        , Validation = val_id[[.y]]
        , Test = test_id[[.y]]
      ) |>
      lapply(\(x) slice(.x, x)) |>
      imap(~ mutate(.x, set = .y, strata = "None")) |>
      reduce(rbind) |>
      select_at(
        c("set"
          , ifelse(is.null(strata_name[[.y]]), "strata", strata_name[[.y]])
        )
      ) |>
      `colnames<-`(c("set", "strata")) |>
      mutate_at("set", \(x) factor(x, unique(x))) |>
      mutate_at("strata", factor) |>
      mutate(dataset = .y)
  ) |>
  reduce(rbind) |>
  group_by_all() |>
  summarize(n = n(), .groups = "drop") |>
  ggplot(aes(strata, n, fill = set)) +
  geom_col(position = "fill", width = 0.8) +
  facet_wrap(~ dataset, scales = "free", ncol = 2) +
  xlab("Strata") +
  scale_y_continuous(
    "Proportion", breaks = seq(0, 1, 0.2), labels = scales::percent
  ) +
  scale_fill_discrete("Set") +
  theme(legend.position = "top")

