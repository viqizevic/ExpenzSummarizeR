
folder <- "N26"
folder <- "Commerzbank"
folder <- "Transferwise"

banking_fullpath <- "/Users/vicky/Documents/celestial/finance/Banking"
path <- file.path(banking_fullpath,folder)
files <- list.files(path)
csvfiles <- files[grepl("\\.csv$",files,ignore.case = TRUE)]

separator <- ";"
separator <- ","

all0 <- Reduce(function(f0, fl) {
  x <- read.csv(file.path(path,fl),sep = separator) %>% as_tibble %>% clean_names()
  y <- x %>% distinct()
  if (nrow(x) != nrow(y)) {
    warning("Found duplicate. Please check in csv file ", fl, immediate. = TRUE)
  }
  if (nrow(f0) == 0) return(x)
  else return(bind_rows(f0, x))
}, csvfiles, tibble())

# For N26
all <- all0 %>%
  distinct() %>% 
  arrange(date)

# For Commerzbank
all <- all0 %>%
  mutate(
    Date = dmy(buchungstag)
  ) %>% 
  arrange(Date) %>% 
  distinct() %>% 
  select(-Date)

# For Wise
all <- all0 %>%
  mutate(Date = dmy(date)) %>% 
  distinct() %>% 
  arrange(Date) %>% 
  select(-Date)

outfolder <- "data/out"
fileout = paste0(folder,".csv")
write.csv(all, file.path(outfolder,fileout))
