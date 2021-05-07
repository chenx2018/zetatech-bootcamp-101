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

# generate fake homephone-----------------------------------
dat <- dat %>%
  group_nest(cardNum) %>%
  mutate(
    homephone = paste0(
      "(0755)-",
      sample(10000121:99990100, size = n_distinct(dat$cardNum))
    )
  ) %>%
  unnest(data)

# generate fake cardHolder Name-----------------------------------

# name 数据集
fakeName <- read_rds("simulation_resource/fakeNames.rds")

fake_name_gender_dat <- fakeName %>%
  mutate(wt = case_when(
    gender == "女" ~ 90,
    TRUE ~ 10
  )) %>%
  slice_sample(n = n_distinct(dat$cardNum), weight_by = wt)

fake_name_gender_dat %>%
  count(gender)

dat <- dat %>%
  group_nest(cardNum) %>%
  bind_cols(select(fake_name_gender_dat, cardHolder = name, gender))%>%
  unnest(data)

# generate fake dob-----------------------------------

generate_age <- function(size) {
  x <- c(
    rnorm(n = round(20000*.2), 25, 2),
    rnorm(n = round(20000*.3), 35, 3),
    rnorm(n = round(20000*.35), 45, 3),
    rnorm(n = round(20000*.2), 50, 4),
    rnorm(n = round(20000*.05), 55, 2)
  )
  x <- x[x>20 & x<60]
  sample(round(x), size = size, replace = TRUE)
}


# add age
dat <- dat %>%
  group_nest(cardNum) %>%
  mutate(age = generate_age(nrow(.))) %>%
  unnest(data)

# add dob
dat <- dat %>%
  arrange(date, prodNum) %>%
  mutate(dob = as.Date(Sys.Date() - age*365))

# generate wechat staff-----------------------------------
generate_wechat_staff <- function(size) {
  name <- factor(sample(fake_name_gender_dat$name, size = 40))
  id <- fct_anon(name, prefix = "00")
  info <- sample(
    c("本人微信", "企业微信"),
    size = 40, replace = TRUE, prob = c(.3, .7)
  )
  res <- glue::glue("{name} \u27a3 工号:{id} \u27a3 {info}")
  sample(res, size = size, replace = TRUE) %>%
    as.character()
}

# generate fake wechat staff data-----------------------------------
fake_wechat_staff_dat <- dat %>%
  distinct(cellphone) %>%
  mutate(wechat_staff = generate_wechat_staff(nrow(.)))


# save data-----------------------------------
dat %>%
  write_rds("data/fake_fulldata.rds", compress = "gz")

fake_wechat_staff_dat %>%
  write_rds("data/fake_wechat_staff_dat.rds", compress = "gz")

