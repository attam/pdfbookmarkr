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
#       Used to insert metadata and generate an output pdf file
#         The pdftk home page permalink is:
#         http://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/
#         The easy-to-remember shortcut is: www.pdftk.com
# 
#   pdfinfo:
#       Used to extract the page number of pdf files:
#         pdfinfo version 0.87.0
#         Copyright 2005-2020 The Poppler Developers - http://poppler.freedesktop.org
#         Copyright 1996-2011 Glyph & Cog, LLC
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
#      page_offset = actual page number of the first page that starts with arabic numerals [1]
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

bookmark_pdf<-function(pdf_in,pdf_out,bookmarks_csv,page_offset=NULL) {
  setwd(dirname(pdf_in))
  meta_parsed<-get_meta(pdf_in)
  # extract the number of pages in the pdf file
  npages<-meta_parsed$NumberOfPages
  bookmarks<-read.csv(bookmarks_csv, stringsAsFactors=FALSE)
  
  # evaluate if any bookmarks are unnumbered or roman numerals, and convert them into numeric indices
  # default is no roman/unnumbered pages at start of pdf, starting with 1 for arabic numerals: c(actual_page=0,starting_index_label=1)
  arabic_bookmarks_indices<-grep("^[0-9]+$",bookmarks$Page)
  roman_bookmarks_indices<--grep("^[0-9]+$",bookmarks$Page, invert=TRUE)
  max_roman<-ifelse(length(roman_bookmarks_indices)==0,0,as.numeric(bookmarks$Page[arabic_bookmarks_indices[1]])-1)
  page_offset<-ifelse(is.null(page_offset),1,page_offset)
  label_start_arabic<-c(page_offset,bookmark)
  #label_start_arabic<-c(max_roman+1,npages-bookmarks$Page[arabic_bookmarks_indices[1]]-max_roman-1)
  
  prefix_roman<-""
  if (length(roman_bookmarks_indices)!=0) {
    label_start_roman<-c(ifelse(length(roman_bookmarks_indices)==0,0,1),npages-bookmarks$Page[arabic_bookmarks_indices[1]])  
    prefix_roman<-paste("PageLabelBegin\nPageLabelNewIndex: ",label_start_roman[0],"\nPageLabelStart: 1","PageLabelNumStyle: UppercaseRomanNumerals\n",sep="")
  }
  
  
  
  prefix_arabic<-paste("PageLabelBegin\nPageLabelNewIndex: ",arabic_index,"\nPageLabelStart: ",starting_arabic,"PageLabelNumStyle: DecimalArabicNumerals\n",sep="")
  
  
  new_indices<-vector(mode="numeric",length(bookmarks$Page))
  
  starting_arabic<-as.numeric(bookmarks$Page[min(which(grepl("^[0-9]+$", bookmarks$Page)))])
  
  arabic_index<-ifelse(min(which(grepl("^[0-9]+$", bookmarks$Page)))==1,1,starting_arabic)
  
  first_roman<-ifelse(min(which(grepl("^[0-9]+$", bookmarks$Page)))==1,1,1)
  #<-ifelse(is.null(starting_arabic),1,starting_arabic)
  arabic_index<-ifelse(is.null(starting_arabic),1,starting_arabic)
  if (min(which(grepl("^[0-9]+$", bookmarks$Page)))==1)
    roman_indices<-as.roman(bookmarks$Pagewhich(!grepl("^[0-9]+$", bookmarks$Page, perl = T)))
  numeric_indices<-which(grepl("^[0-9]+$", bookmarks$Page, perl = T))
  
  
  # default setting of page indices for roman and arabic numerals in the format c(actual, labelled)
  arabic_index<-ifelse(is.null(starting_arabic),1,starting_arabic)
  roman_index<-ifelse(is.null(max_roman),c(1,starting_arabic),c(max_roman,starting_arabic))
  
  
  # adjust page numbering based on starting indices for roman/unnumbered and arabic numerals provided  
  bookmark_input[,2]<-bookmark_input[,2]+max_roman
  bookmark_export<-paste("BookmarkBegin\nBookmarkTitle: ",bookmark_input[,1],"\nBookmarkLevel: 1\nBookmarkPageNumber: ",bookmark_input[,2],"\n",sep="")
  
  fileConn<-file(pdf_bookmarks.txt)
  writeLines(c(prefix_roman,prefix_arabic,bookmark_export),fileConn)
  close(fileConn)
  system2('pdftk', args=c(pdf_in,"update_info", "pdf_bookmarks.txt", "output", pdf_out))
  return()
}