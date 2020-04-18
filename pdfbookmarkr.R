# R script to simplify creation of bookmarks for pdf files
# 
# Background
#   As a professional (physician) that is semi-nomadic, I have a strong preference for digital (pdf-based) content
#   over physical copies of heavy and expensive textbooks or journal articles.
#   Bookmarks in PDF files are a convenient way to facilitate overview and rapid access to necessary information.
#   Unfortunately however, many PDF files do not have bookmarks.
#   In the past, I have manually set bookmarks using pdftk in linux, although this can be a tedious and time-consuming process.
#   This has led me to create this tool to semi-automate the process using R and the open-source linux tools freely available to the public.
# 
# Dependencies required for this tool to function
# external:
#   pdftk
#       Used to create bookmarks for output pdf file
#         The pdftk home page permalink is:
#         http://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/
#         The easy-to-remember shortcut is: www.pdftk.com
#
#   pagelabels-py
#       Used to generate Roman and Arabic-style page numbering for output pdf
#       see link: https://github.com/lovasoa/pagelabels-py
#
# Assumptions
#   Several assumptions have been made in writing this R script, although they can be easily adjusted and individualized with only basic R skills...
#     Paging of document
#       Often documents will begin with unnumbered pages or pages with Roman numerals, followed by Arabic numerals
#       This script assumes the document starts with 0 or more Roman numeral pages, followed by 1 or more Arabic numeral pages
#       [note: this allows for documents starting with Arabic numerals]
#       The default Arabic numeral starting page will be 1, but this can be changed as needed using the page_offset argument (see below)
#       
# Arguments
#   The following input arguments are required:
#      i_pdf=input pdf file (must include full file path)
#      o_pdf=output pdf file (does not require full path; default action is to output to same directory as input pdf)
#      bookmarks_csv=bookmark info in csv format (see below for details)
#   The following parameters are optional [if not set, default in square brackets]:
#     roman_offset = [0]
#     arabic_offset = [0]
#     These should take the value of the actual page numbers for the first arabic (and roman, if present) bookmarks
#         example .1) if the first roman bookmark is "v", and it corresponds to the 4th page of the input pdf, then roman_offset should be 4
#         example 2) if there are no roman numerals, and the first bookmark contains the arabic numeral 5, which is on the 3rd page of the input pdf, this implies
#           that the first 2 pages of the pdf were cut out. In order to maintain consistent numbering, add the argument arabic_offset = 3
#
# The bookmark_csv parameter should be the name of a csv file that contains the column headings, respectively: "Title", "Page","Level"
#   Title column: contains the title of the bookmark which will appear in the output pdf file
#   Page column: contains the page numbers in roman and/or arabic numerals based on the input pdf
#   Level column: contains the bookmark level, where 1 is the top-level; higher numbers are nested bookmarks for corresponding subsections, and sub-subsections
# 
# Future plans include:
#   1) include functionality that uses ghostscript tool to insert additional pdf metadata such as...
#   2) error handling
#   3) nested bookmarks (eg for chapters with subsections)

library("usethis", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.6")

# extract metadata from input pdf file, using meta_tagnames and parse into meta_parsed
get_meta<-function(pdf_in) {
  metadata<-system2("pdftk", args=c(pdf_in, "data_dump", "output"),stdout=TRUE)
  all_metatags<-levels(factor(gsub("^(.*):\ .*$","\\1",grep("^.*:\ (.*)$",metadata,value=TRUE))))
  meta_pattern<-paste("^",all_metatags,":\\ (.*)$",sep="")
  meta_parsed<-sapply(meta_pattern,function(x,metadata_in) gsub(x,"\\1",grep(x,metadata_in,value=TRUE)),metadata)
  names(meta_parsed)<-all_metatags
  return(meta_parsed)
}

bookmark_pdf<-function(pdf_in,pdf_out,bookmarks_csv,roman, arabic,roman_offset=NULL, arabic_offset=NULL) {
  setwd(dirname(pdf_in))
  meta_parsed<-get_meta(pdf_in)
  # extract the number of pages in the pdf file
  npages<-meta_parsed$NumberOfPages
  bookmarks<-read.csv(bookmarks_csv, stringsAsFactors=FALSE)
  # generate bookmark pages based on offsets
  # evaluate if any bookmarks are unnumbered or roman numerals, and convert them into numeric indices
  arabic_indices<-grep("^[0-9]+$",bookmarks$Page)
  roman_indices<-grep("^[0-9]+$",bookmarks$Page, invert=TRUE)
  bookmarks$nominalPage<-nominal_pages<-as.numeric(as.roman(bookmarks$Page))
  offsets<-mapply(function(x,y,z) ifelse(is.null(x), 0-z[y],x-z[y]),c(roman_offset,arabic_offset),c(roman_indices[1],arabic_indices[1]),MoreArgs=list(bookmarks$nominalPage))
  bookmarks$actualPage<-mapply(function(x,y,o) ifelse(x,y+o[2],y+o[1]), grepl("^[0-9]+$",bookmarks$Page),nominal_pages,MoreArgs=list(offsets))
  
  bookmark_tags<-paste("BookmarkBegin\nBookmarkTitle: ",bookmarks$Title,"\nBookmarkLevel: ",bookmarks$Level,"\nBookmarkPageNumber: ",bookmarks$actualPage,sep="")
  fileConn<-file("pdf_bookmarks.txt")
  writeLines(bookmark_tags,con=fileConn,sep="\n")
  close(fileConn)
  #python3 -m pagelabels --startpage 8 --type "arabic" --firstpagenum 337 --outfile /tmp/new.pdf /tmp/new.pdf
  if (length(roman_indices)!=0) {
    system(paste(c('python3 -m pagelabels --startpage',roman[1],' --type "roman lowercase" --firstpagenum',roman[2],' --outfile ',pdf_out,pdf_in),collapse = ' '))
    }
  if (length(arabic_indices)!=0) {
    system(paste(c('python3 -m pagelabels --startpage',arabic[1],' --type "arabic" --firstpagenum',arabic[2],' --outfile ',pdf_out,pdf_in),collapse = ' '))    }
  system2('pdftk', args=c(pdf_in,"update_info", "pdf_bookmarks.txt", "output", pdf_out))
  return(paste("Page numbering and bookmarks have been set in file ",pdf_out,sep=""))
}
