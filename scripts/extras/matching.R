
set.seed(1234)
#For menopause, get age-matched males and females 

#For each of these things, I want to apply this function: matchit() and then put into dataframe. Think of how to do this. 

#apply to FINAL
list1 <- split(FINAL, list(FINAL$RepGroup))
list2 <- list1[c(2,3,4,5,6)]
  
get.matched <- function(x){ #mods.deltadf
  males <- as.data.frame(list1[["Male"]])  
  merged <- full_join(males, as.data.frame(x)) %>%
    mutate(category = as.character(unique(x$RepGroup)))
  match.it <- matchit(Sex ~ Age,
                      data = merged,
                      method = "nearest",
                      ratio = 1)
  match.df <- match.data(match.it)[1:ncol(x)]
  match.df
}

list3 <- lapply(list2, get.matched)
df1 <- plyr::ldply(list3, data.frame, .id="Category")



