library(RefManageR)


cit9 <-  as.BibEntry(citation("tidyverse"))
cit0 <- as.BibEntry(citation("cluster"))
cit1 <- as.BibEntry(citation("NbClust"))
cit2 <- as.BibEntry(citation("factoextra"))
cit3 <- as.BibEntry(citation("knitr"))
cit4 <- as.BibEntry(citation("gridExtra"))
cit5 <- as.BibEntry(citation("caret"))
cit6 <- as.BibEntry(citation("pROC"))
cit7 <- as.BibEntry(citation("MASS"))
cit8 <- as.BibEntry(citation("RefManageR"))

WriteBib(c(cit9, cit0, cit1, cit2, cit3, cit4, cit5, cit6, cit7, cit8), file = "bib_entrega.final.bib")