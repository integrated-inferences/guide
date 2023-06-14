library("knitr")

# Get names of rmds
files <- list.files()
chapters <- files[grepl(".Rmd",files,ignore.case = T)]
chapters <- chapters[-which(chapters %in% c("ii.Rmd","index.Rmd"))]



the_chunks <- lapply(
  chapters,
  purl,
  documentation = 0
)
lapply(chunks,names)




p <- purl("test.Rmd")
read_chunk(p)

invisible(mapply(function(chunk, name) {
  writeLines(c(paste0("## ----",name,"----"), chunk), paste0("chunk-",name,".R"))
}, chunks, names(chunks)))
unlink(p) # delete the original purl script
knitr:::knit_code$restore() # remove chunks from current knitr session

unlink(the_chunks)
??purl
