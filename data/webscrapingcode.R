if(!require('rvest')) {
  install.packages('rvest')
  library('rvest')
}
year <- seq(2009,2021,1)
tables_url = list()

ori_url= "https://www.espn.in/football/table/_/league/ENG.1/season/"
for (i in 1:length(year)){
  a = html_table(read_html(paste(ori_url,year[i],sep="")))
  for (j in 1:length(a[[2]])){
    a[[1]][j+1] = a[[2]][j]
  }
  g = list()
  g[[1]] = a[[1]]
  g[[1]][1] = substring(gsub('[0-9]+','',as.data.frame(g)[[1]]),4)
  tables_url[[i]]=g[[1]]
  
}