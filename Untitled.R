


instr <- function(str1,str2,startpos=1,n=1){
  aa=unlist(strsplit(substring(str1,startpos),str2))
  if(length(aa) < n+1 ) return(0);
  return(sum(nchar(aa[1:n])) + startpos+(n-1)*nchar(str2) )
}

googleNumber <- function(stock, date0){
  myDate=strsplit(date0,'/')
  thepage = readLines(paste('https://www.google.com/search?q=',stock,'&biw=1318&bih=606&source=lnt&tbs=cdr%3A1%2Ccd_min%3A',myDate[[1]][1],'%2F',myDate[[1]][2],'%2F',myDate[[1]][3],'%2Ccd_max%3A',myDate[[1]][1],'%2F',myDate[[1]][2],'%2F',myDate[[1]][3],'&tbm=nws',sep=""))
  num0=instr(thepage,'id=\"resultStats\"')
  return(strsplit(substring(thepage,num0+23, num0+50), " ")[[1]][1])
}


print(googleNumber('aapl','2/3/2015'))