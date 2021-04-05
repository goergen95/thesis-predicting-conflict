library(animation)
library(stringr)
pdfs = list.files("report/_bookdown_files/thesis_files/figure-latex/", pattern = ".pdf$", full.names = T)

for (pdf in pdfs){
  name = basename(pdf)
  png = file.path("report/presentation/libs/img", str_replace(name, "pdf", "png"))
  im.convert(files = pdf, output = png, extra.opts="-density 300")
}