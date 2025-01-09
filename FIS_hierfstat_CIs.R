library(hierfstat)
library(adegenet)

genind <- read.genepop(file.choose())

newts.hfstat <- genind2hierfstat(genind)
basicstat <- basic.stats(genind, diploid = TRUE, digits = 2) 

basicstat$Fis

apply(basicstat$Fis, MARGIN = 2, FUN = mean, na.rm = TRUE) %>%
  round(digits = 3)

View(newts.hfstat)

names(basicstat)

boot.ppfis(newts.hfstat)

library(PopGenReport)

null.all(genind)

genind <- read.genepop(file.choose())

pb <- progress::progress_bar
pb        

pb <- txtProgressBar(min = 0, max = 100, style = 3)
for(i in 1:100) {
  Sys.sleep(0.1)
  setTxtProgressBar(pb, i)
}

newtgenind <- read.genepop(file.choose())

