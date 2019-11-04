## Run this script from folder that contains sim_data.csv.
library(tidyverse)

my_write_delim <- function (x, path, delim = " ", na = "NA", append = FALSE, col_names = !append,
                            quote_escape = "double") {
  stopifnot(is.data.frame(x))
  x[] <- lapply(x, output_column)
  readr:::stream_delim(x, path, delim = delim, col_names = col_names,
                       append = append, na = na, quote_escape = quote_escape)
}

## Input file.
input_file <- ifelse(!interactive(), commandArgs(TRUE)[1], "sim_data.csv")

## Read data
sim_data <- read_csv(input_file)

## Get max lengths of ID and t, so we can pad these variables to appropriate lengths
max_id <- max(sim_data$ID)
max_time <- max(sim_data$t)

## Create tibble with data by each ID
sim_data_by_id <- sim_data %>%
  rename(time = t) %>%
  mutate(time = paste0("time", str_pad(time,
                                       width = nchar(max_time),
                                       pad = '0',
                                       side = 'left'))) %>%
  gather(key = 'gene', value = 'expression', -ID, -time) %>%
  spread(time, expression) %>%
  nest(raw = -ID) %>%
  ungroup() %>%
  mutate(ID = str_pad(ID, width = nchar(max_id), pad = '0', side = 'left'))

## Check if folders exist. If not, create
if (!dir.exists("raw_data"))
  dir.create("raw_data")
if (!dir.exists("permuted_data"))
  dir.create("permuted_data")
if (!dir.exists("normalized_data"))
  dir.create("normalized_data")

## Create 0-mean normalized data and permuted data
all_data <- sim_data_by_id %>%
  mutate(norm_and_permuted = map(raw, function(x){
    tmp <- x %>% gather(-gene, key = "time", value = "expression") %>%
      group_by(gene) %>%
      mutate(expression = expression - mean(expression),
             permuted = sample(expression)) %>%
      gather(key = 'type', value = 'expression', expression, permuted) %>%
      spread(time, expression) %>%
      group_by(type) %>%
      nest()

    out <- tibble(normalized = filter(tmp, type == 'expression')$data,
                  permuted = filter(tmp, type == 'permuted')$data)

    return(out)
  })) %>%
  unnest(norm_and_permuted) %>%
  gather(key = "type", value = "data", raw, normalized, permuted)


normalized_only <- all_data %>%
  filter(type == 'normalized') %>%
  select(-type) %>%
  unnest(cols = data) %>%
  arrange(ID, gene)

write_csv(x = normalized_only, "sim_data_normalized.csv")

## Write data to files. One file per ID by type (raw, normalized, or permuted)
with(all_data,
     pmap(list(x = ID, y = type, z = data),
          function(x,y,z)
            my_write_delim(z, path = paste0(y, "_data/", x, ".txt"),
                           delim = "\t")))

## Save file with gene names
my_write_delim(sim_data_by_id$raw[[1]] %>% select(gene),
               path = "list_of_genenames.txt", delim = "\t", col_names = F)

## Write rep files
all_data %>% select(ID = paste0(ID, ".txt"), type) %>% unique() %>%
  nest(data = ID) %>%
  mutate(write = map2(data, type, ~my_write_delim(x = .x,
                                                  path = paste0(.y, "_data/", .y, "_reps.txt"),
                                                  col_names = FALSE)))