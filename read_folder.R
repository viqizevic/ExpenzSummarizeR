
folder <- "N26"
folder <- "Commerzbank"
folder <- "Transferwise"
folder <- "Barclaycard"
folder <- "LBB-Amazon"
folder <- "DeutscheBank"
folder <- "Wuestenrot"

banking_fullpath <- "/Users/vicky/Documents/celestial/finance/Banking"

separator <- ";"
separator <- ","

skiprows <- 0
skiprows <- 1
skiprows <- 4
skiprows <- 12

encodingtype <- "latin1"
encodingtype <- "UTF-8"
encodingtype <- "unknown"

read_folder <- function(folder, path, skiprows) {
  fullpath <- file.path(path,folder)
  files <- list.files(fullpath)
  tblfiles <- files[grepl("\\.(csv|xlsx)$",files,ignore.case = TRUE)]
  if (length(tblfiles)==0) {
    warning("No csv or xlsx file found. Please check folder ", fullpath, immediate. = TRUE)
  } else {
    # combine all files in folder into one tibble
    all0 <- Reduce(function(f0, fl) {
      if (grepl("csv$", fl, ignore.case = TRUE)) {
        x0 <- read.csv(file.path(fullpath,fl), sep = separator, skip = skiprows,
                      encoding = encodingtype)
      } else {
        x0 <- read_xlsx(file.path(fullpath,fl), skip = skiprows)
      }
      x <- x0  %>% as_tibble %>% clean_names()
      # check if any duplicate
      y <- x %>% distinct()
      if (nrow(x) != nrow(y)) {
        warning("Found duplicate. Please check in file ", fl, immediate. = TRUE)
      }
      # if first one then take as it is, else bind with previous one
      if (nrow(f0) == 0) return(x)
      else return(bind_rows(f0, x))
    }, tblfiles, tibble())
    
    # remove duplicates
    all1 <- all0 %>% distinct()
    return(all1)
  }
}

all0 <- read_folder(folder, banking_fullpath, skiprows)

sort_by_date <- function(df) {
  # Save as temporary date for sorting
  datevars <- c("date", "buchungstag")
  for (d in datevars) {
    if (d %in% names(df)) {
      df["TempDate"] <- df[d]
    }
  }
  if ("TempDate" %in% names(df)) {
    suppressWarnings({
      # Format as Date
      exdt <- pull(df,TempDate)[1]
      is_dmy <- !is.na(dmy(exdt))
      is_ymd <- !is.na(ymd(exdt))
      if (is_dmy) {
        df <- df %>% mutate(TempDate=dmy(TempDate))
      } else if (is_ymd) {
        df <- df %>% mutate(TempDate=ymd(TempDate))
      } else {
        warning("Unhandled date format. Please check date format ", exdt, immediate. = TRUE)
      }
    })
    df <- df %>% arrange(TempDate) %>% select(-TempDate)
  } else {
    warning("Not date variable found. Please check.", immediate. = TRUE)
  }
  return(df)
}
all <- sort_by_date(all0)

# For N26
all <- all0 %>%
  arrange(date)

# For Commerzbank
all <- all0 %>%
  mutate(
    Date = dmy(buchungstag)
  ) %>% 
  arrange(Date) %>% 
  select(-Date)

# For Wise
all <- all0 %>%
  mutate(Date = dmy(date)) %>% 
  arrange(Date) %>% 
  select(-Date)

# For Amazon
all <- all0 %>%
  mutate(
    Date = dmy(transaktionsdatum)
  ) %>% 
  arrange(Date) %>% 
  select(-Date)

# For Barclay
all <- all0 %>%
  mutate(
    Date = dmy(buchungsdatum_2)
  ) %>% 
  arrange(Date) %>% 
  select(-Date)

# For DB
all <- all0 %>%
  filter(buchungstag!="Kontostand") %>% 
  mutate(
    Date = dmy(buchungstag)
  ) %>% 
  arrange(Date) %>% 
  select(-Date)

outfolder <- "data/out"
fileout = paste0(folder,".csv")
write.csv(all, file.path(outfolder,fileout))
