library(ggplot2)
theme_set(theme_classic())


get_data_for_chart <- function(datain, yr, expensefl="Y", threshold=2.5) {
  tball <- createSummary(datain, yr) %>% 
    mutate(AVAL = round(as.numeric(Total),digits=2)) %>%
    filter(!grepl("Saldo|Total",Category), AVAL!=0)
  if (expensefl == "Y") {
    tball <- tball %>% filter(AVAL<0) %>% 
      mutate(AVAL = AVAL*(-1)) %>% arrange(-AVAL)
  }
  totaval <- tball$AVAL %>% sum
  ds0 <- tball %>% select(Category, AVAL) %>% 
    mutate(
      PRCT=round((AVAL/totaval)*100,digits=1),
      Category=paste(Category, paste0("(",PRCT,"%)"))
    )
  ds1 <- ds0 %>% filter(PRCT>=threshold)
  cats <- ds1$Category
  restpct <- 100 - (ds1$PRCT %>% sum)
  restaval <- totaval - (ds1$AVAL %>% sum)
  if (restpct > 0) {
    catother <- paste("~Others", paste0("(",round(restpct,digits=1),"%)"))
    cats <- append(cats, catother)
    ds <- ds1 %>% add_row(Category=catother, AVAL=restaval, PRCT=restpct)
  } else {
    ds <- ds1
  }
  ds$Category <- factor(ds$Category, levels = cats)
  ds
}

create_pie_chart <- function(datain, year) {
  df <- get_data_for_chart(datain, year)
  
  pie <- ggplot(df, aes(x = "", y=PRCT, fill = factor(Category))) + 
    geom_bar(width = 1, stat = "identity") +
    theme(axis.line = element_blank(), 
          plot.title = element_text(hjust=0.5)) + 
    labs(
      fill="Category", 
      x=NULL, 
      y=NULL
    )
  
  pie + coord_polar(theta = "y", start=0)
}

create_pie_chart(tb,"2022")
