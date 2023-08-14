######################################
# Read csv or xlsx files in folder
# and combine them as one csv file
######################################

read_folder <- function(path, folder, skiprows=0, separator=",",
                        encodingtype="unknown", fileencoding="") {
  # find files in folder
  fullpath <- file.path(path, folder)
  files <- list.files(fullpath)
  tblfiles <- files[grepl("\\.(csv|xlsx)$",files,ignore.case = TRUE)]
  if (length(tblfiles)==0) {
    warning("No csv or xlsx file found. Please check folder ", fullpath, immediate. = TRUE)
  } else {
    # combine all files in folder into one tibble
    all0 <- Reduce(function(f0, fl) {
      print(paste("Read file", fl))
      if (grepl("csv$", fl, ignore.case = TRUE)) {
        x0 <- read.csv(file.path(fullpath,fl), sep = separator, skip = skiprows,
                      encoding = encodingtype, fileEncoding = fileencoding)
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
    print(paste("Find #rows:", nrow(all0)))
    
    # remove duplicates
    all1 <- all0 %>% distinct()
    print(paste("Unique #rows:", nrow(all1)))
    
    # sort by date
    all2 <- sort_by_date(all1)
    
    # save as file
    save_combined_file(all2, folder)
  }
}

sort_by_date <- function(df) {
  # Save as temporary date for sorting
  datevars <- c("date", "buchung", "buchungstag", "buchungsdatum_2",
                "transaktionsdatum")
  for (d in datevars) {
    if (d %in% names(df)) {
      df["TempDate"] <- df[d]
      print(paste("Sort by:", d))
    }
  }
  if ("TempDate" %in% names(df)) {
    # check if any word in date variable
    word_rgx <- "^[A-Za-z]+$"
    grpl_word <- grepl(word_rgx, df$TempDate)
    if (any(grpl_word)) {
      # drop obs with word in date
      print(paste("Drop obs with word in date variable:", df$TempDate[grpl_word]))
      df <- df %>% filter(!grepl(word_rgx, TempDate))
    }
    suppressWarnings({
      # Format as Date
      exdt <- pull(df,TempDate)[1]
      is_dmy <- !is.na(dmy(exdt))
      is_ymd <- !is.na(ymd(exdt))
    })
    if (is_dmy) {
      df <- df %>% mutate(TempDate=dmy(TempDate))
    } else if (is_ymd) {
      df <- df %>% mutate(TempDate=ymd(TempDate))
    } else {
      warning("Unhandled date format. Please check date format ", exdt, immediate. = TRUE)
    }
    df <- df %>% arrange(TempDate) %>% select(-TempDate)
  } else {
    warning("No date variable found. Please check: ",
            paste(names(df), collapse = " "), immediate. = TRUE)
  }
  return(df)
}

save_combined_file <- function(df, filename) {
  fileout = file.path("data", "out", paste0(filename,".csv"))
  write.csv(df, fileout)
  print(paste("Saved file:", fileout))
}

.read_folders <- function() {
  path <- "/Users/vicky/Documents/celestial/finance/Banking"
  read_folder(path, "Barclaycard", 12)
  read_folder(path, "Commerzbank", separator = ";")
  read_folder(path, "DeutscheBank", 4, separator = ";", encodingtype = "latin1")
  read_folder(path, "ING-DiBa", 13, separator = ";", fileencoding = "latin1")
  read_folder(path, "LBB-Amazon", 1, separator = ";")
  read_folder(path, "N26")
  read_folder(path, "Transferwise")
}
