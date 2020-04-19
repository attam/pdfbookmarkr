# R script to simplify creation of bookmarks for pdf files
# 
# Background
#   I have a preference for digital (pdf-based) content over physical books due to my career and lifestyle.
#   PDF files generally have a built-in bookmark feature which are a convenient way to facilitate overview and rapid access to necessary information.
#   Unfortunately however, many of my PDF files do not have bookmarks for chapters/sections.
#   In the past, I have manually set bookmarks using pdftk and a text editor, although this can be a tedious and time-consuming process.
#   This has led me to create this tool to semi-automate the process using R and the open-source linux tools freely available to the public.
# 
# Dependencies required for this tool to function are GPL-licensed. This script is also licensed under GPL-3.0.
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
#   Several assumptions have been made in writing this R script...
#     Paging of document
#       The input pdf uses all Arabic numerals, or alternatively, has an initial section with unnumbered or Roman numeral pages
#       The default Arabic numeral starting page will be 1, but this can be changed as needed using the page_offset argument (see below)
#       
# Arguments
#   The following input arguments are required:
#      i_pdf=input pdf file (must include full file path)
#      o_pdf=output pdf file (does not require full path; default action is to output to same directory as input pdf)
#      bookmarks_csv=bookmark info in csv format (see below for details)
#   The following parameters are optional [if not set, default in square brackets]:
#     roman = 2-element vector in the format (page for which to start Roman numeral indices, initial Roman numeral index) [c(0,0)] (default is no Roman numerals)
#     arabic = 2-element vector in the format (page for which to start Arabic numeral indices, initial Arabic numeral index) [c(1,1)]
#
# The bookmark_csv parameter should be the name of a csv file that contains the column headings, respectively: "Title", "Page","Level"
#   Title column: contains the title of the bookmark which will appear in the output pdf file
#   Page column: contains the page numbers in roman and/or arabic numerals based on the input pdf
#   Level column: contains the bookmark level, where 1 is the top-level; higher numbers are nested bookmarks for corresponding subsections, and sub-subsections
#
#   Note: A new text file (pdf_bookmarks.txt) will be created and used to write the bookmarks to the pdf output file
# 
# Future plans include:
#   1) error handling
#   2) more flexibility with alternative numbering systems/labelling styles (eg, A-1, A-2, etc, using existing capabilities of pagelabels-py)


# extract metadata from input pdf file, using meta_tagnames and parse into meta_parsed
get_meta<-function(pdf_in) {
  metadata<-system2("pdftk", args=c(pdf_in, "data_dump", "output"),stdout=TRUE)
  all_metatags<-levels(factor(gsub("^(.*):\ .*$","\\1",grep("^.*:\ (.*)$",metadata,value=TRUE))))
  meta_pattern<-paste("^",all_metatags,":\\ (.*)$",sep="")
  meta_parsed<-sapply(meta_pattern,function(x,metadata_in) gsub(x,"\\1",grep(x,metadata_in,value=TRUE)),metadata)
  names(meta_parsed)<-all_metatags
  return(meta_parsed)
}

bookmark_pdf<-function(pdf_in,bookmarks_csv,roman=c(0,0), arabic=c(1,1), pdf_out) {
  setwd(dirname(pdf_in))
  meta_parsed<-get_meta(pdf_in)
  # extract the number of pages in the pdf file
  npages<-meta_parsed$NumberOfPages
  
  # read bookmark_csv file containing bookmark info to be written to pdf output file
  bookmarks<-read.csv(bookmarks_csv, stringsAsFactors=FALSE)
  
  # create dataframe containing bookmark info with corresponding actual page numbers
  arabic_indices<-grep("^[0-9]+$",bookmarks$Page)
  roman_indices<-grep("^[0-9]+$",bookmarks$Page, invert=TRUE)
  bookmarks$nominalPage<-nominal_pages<-as.numeric(as.roman(bookmarks$Page))
  roman_pages<-arabic[1]-roman[1]
  arabic_pages<-npages-roman_pages
  offsets<-list(roman=roman[1]-roman[2],arabic=arabic[1]-arabic[2])
  bookmarks$actualPage<-mapply(function(x,y,o) ifelse(x,y+o$arabic,y+o$roman), grepl("^[0-9]+$",bookmarks$Page),nominal_pages,MoreArgs=list(offsets))
  
  # write bookmark info to new file pdf_bookmarks.txt
  bookmark_tags<-paste("BookmarkBegin\nBookmarkTitle: ",bookmarks$Title,"\nBookmarkLevel: ",bookmarks$Level,"\nBookmarkPageNumber: ",bookmarks$actualPage,sep="")
  fileConn<-file("pdf_bookmarks.txt")
  writeLines(bookmark_tags,con=fileConn,sep="\n")
  close(fileConn)
  
  # add Arabic and/or Roman page numbering to the output pdf file
  if (length(roman_indices)!=0) {
    system(paste(c('python3 -m pagelabels --startpage',roman[1],' --type "roman lowercase" --firstpagenum',roman[2],' --outfile ',pdf_out,pdf_in),collapse = ' '))
    }
  if (length(arabic_indices)!=0) {
    system(paste(c('python3 -m pagelabels --startpage',arabic[1],' --type "arabic" --firstpagenum',arabic[2],' --outfile ',pdf_out,pdf_in),collapse = ' '))    }
  
  # write the bookmark info to the pdf output file
  system2('pdftk', args=c(pdf_in,"update_info", "pdf_bookmarks.txt", "output", pdf_out))
  return(paste("Page numbering and bookmarks have been set in file ",pdf_out,sep=""))
}
