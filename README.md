# pdfbookmarkr
R script to facilitate insertion of bookmark and metadata in pdf files

Background
  As a professional (physician) that is semi-nomadic, I have a strong preference for digital (pdf-based) content
  over physical copies of heavy and expensive textbooks or journal articles.
  Bookmarks in PDF files are a convenient way to facilitate overview and rapid access to necessary information.
  Unfortunately however, many PDF files do not have bookmarks.
  In the past, I have manually set bookmarks using pdftk in linux, although this can be a tedious and time-consuming process.
  This has led me to create this tool to semi-automate the process using R and the open-source linux tools freely available to the public.

Dependencies required for this tool to function
pdftk
    Used to insert metadata and generate an output pdf file
      The pdftk home page permalink is:
      http://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/
      The easy-to-remember shortcut is: www.pdftk.com

pdfinfo
    Used to extract the page number of pdf files:
      pdfinfo version 0.87.0
      Copyright 2005-2020 The Poppler Developers - http://poppler.freedesktop.org
      Copyright 1996-2011 Glyph & Cog, LLC


Assumptions
  Several assumptions have been made in writing this R script, although they can be easily adjusted and individualized with only basic R skills...
    Paging of document
      Often documents will begin with Roman numerals, followed by Arabic numerals
      This script assumes such an arrangement, and will require the user state the maximum Roman page number before transitioning to Arabic numerals
      The default Arabic numeral starting page will be 1, but this can be changed as needed (see below)

Arguments
  The following input arguments are required:
     i_pdf=input pdf file
     o_pdf=output pdf file
     bookmarks=bookmark list in csv format (see below for details)
     max_roman=highest number of roman numerals
     starting_arabic=number of first page with arabic numerals (default = 1)

The bookmark parameter should be a csv file that contains the column headings: "Page","Title"
  The Page column should contain the page numbers in roman and/or arabic numerals based on the input pdf
  The Title column should contain the title of the bookmark which will appear in the output pdf file

Future plans include:
  1) include functionality that uses ghostscript tool to insert additional pdf metadata such as...
  /Title (Document title)
  /Author (Author name)
  /Subject (Subject description)
  /Keywords (comma, separated, keywords)
  /ModDate (D:20061204092842)
  /CreationDate (D:20061204092842)
  /Creator (application name or creator note)
  /Producer (PDF producer name or note)
  /DOCINFO pdfmark
  see http://milan.kupcevic.net/ghostscript-ps-pdf/ for more details on ghostscript tool
  2) error handling