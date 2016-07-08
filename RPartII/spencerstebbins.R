#Q1----------
fib = c(1,1)
i = 3
evensum = 0
while (fib[i-1] < 4000000) { 
  fib[i] = fib[i-1] + fib[i-2]
  if (fib[i] %% 2 == 0) {
    evensum = evensum + fib[i]
  }
  i= i + 1
}
#Q2---------
mv_multiply = function (m, v) {
  mapply("*",as.data.frame(m),v)
}
#Q3---------
mad = function(x) {
  m = median(x)
  ad = sapply(x, function(i){ i = abs(m - i)})
  median(ad)
}
#Q4---------
name = 'John Andrew Thomas'
words = strsplit(names, ' ')[[1]]
paste(words, '@gmail.com', sep='')
#Q5---------
letters = c('a','b','c','d','e')
paste(letters, rep(letters, each=5))