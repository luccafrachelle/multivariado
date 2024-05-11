library(RefManageR)

cit1 <- as.BibEntry(citation("factoextra"))
cit2 <- as.BibEntry(citation("gridExtra"))
cit3 <- as.BibEntry(citation("tidyverse"))
cit4 <- as.BibEntry(citation("FactoMineR"))
cit5 <- as.BibEntry(citation("vcd"))
cit6 <- as.BibEntry(citation("ca"))
cit7 <- as.BibEntry(citation("knitr"))

WriteBib(c(cit1, cit2, cit3, cit4, cit5, cit6, cit7), file = "bibliografia.bib")
