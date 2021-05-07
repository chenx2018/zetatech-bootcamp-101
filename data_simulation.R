# loading packages-----------------------------------
library(tidyverse)

# loading data-----------------------------------
# loading a fake data set to build a example data set
dat <- read_rds("~/Desktop/temp/fake_fulldata.rds")

dat <- dat %>%
  select(
    prodNum:mstone,
    cardNum,
    contains("tag"),
    contains("price")
  )

# data manipulation-----------------------------------
# tidy names
dat <- dat %>%
  mutate(name = str_replace(name, "^金AU", "AU-"))

dat %>%
  count(name, sort = TRUE)

# anonymize cardNum
# using `fct_anon` function
dat <- dat %>%
  mutate(cardNum = factor(cardNum)) %>%
  mutate(cardNum = fct_anon(cardNum, prefix = "uid"))

dat %>%
  count(cardNum, sort = TRUE)

# anonymize prodNum
dat <- dat %>%
  mutate(prodNum = factor(prodNum)) %>%
  mutate(prodNum = fct_anon(prodNum, prefix = "item"))

# generate fake cellphone-----------------------------------
generate_cellphone <- function(size = 1) {
  a <- "134135136137138139147150151152157158159165172178182183184187188198"
  b <- "130131132145155156166171175176185186"
  c <- "133149153173177180181189199"
  d <- paste0(a, b, c)
  # get starting 3 char
  phone_3char <- stringi::stri_sub(
    d, seq(1, stringi::stri_length(d), by = 3), length = 3
  )
  # get anther 8 char
  phone_8char <- sample(10000121:99990100, size = size)

  res <- unique(paste0(phone_3char, phone_8char))
  # return
  res[1:size]
}

# number of unique customers
n_distinct(dat$cardNum)

dat <- dat %>%
  group_nest(cardNum) %>%
  mutate(cellphone = generate_cellphone(size = n_distinct(dat$cardNum))) %>%
  unnest(data)

dat %>%
  summary()



# save data-----------------------------------
