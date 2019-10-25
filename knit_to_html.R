
library(rmarkdown)
render("wnb.Rmd", output_file = "www/index.html")
#system("git subtree push --prefix www origin gh-pages")