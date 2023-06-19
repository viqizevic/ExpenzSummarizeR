path <- "/Users/vicky/Documents/celestial/finance/Banking/N26/obsolete"
files <- list.files(path)
csvfiles <- files[grepl("\\.csv$",files)]

all0 <- Reduce(function(f0, fl) {
  x <- read.csv(file.path(path,fl)) %>% as_tibble %>% clean_names()
  y <- x %>% distinct()
  if (nrow(x) != nrow(y)) {
    warning("Found duplicate. Please check in csv file ", fl, immediate. = TRUE)
  }
  if (nrow(f0) == 0) return(x)
  else return(bind_rows(f0, x))
}, csvfiles, tibble())

all <- all0 %>%
  mutate(
    account_number = ifelse(is.na(account_number),"",account_number),
    payment_reference = ifelse(payment_reference=="-","",payment_reference)
  ) %>% 
  select(-category) %>% distinct()

all %>% count(account_number) %>% formattable()
all %>% count(payment_reference) %>% formattable()

all %>% pull(amount_eur) %>% sum()

write.csv(all, "data/n26.csv")

tb_n26c <- read_trans_file(file = "N26-transactions copy.xlsx", acc = "N26c", dateformat="%Y-%m-%d")

v <- bind_rows(tb_n26, tb_n26c)
w <- v %>% count(date, payee) %>% filter(n!=2,n!=4)
w %>% inner_join(v) %>% formattable()
