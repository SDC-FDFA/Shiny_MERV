
# comment_fun <- function(txt, cmt) {
#   b1 <- block_list(fpar(ftext(cmt)))
#   
#   comment_i_1 <- run_comment(
#     cmt = b1, run = ftext(txt),
#     author = "GQ",
#     date = Sys.Date(),
#     initials = "GQ"
#   )
#   
#   return(fpar(comment_i_1))
#   
# }

comment_fun <- function(txt, cmt) {
  cmt_lines <- strsplit(cmt, "\n")[[1]]
  
  b1 <- do.call(block_list, lapply(cmt_lines, function(line) fpar(ftext(line))))
  
  comment_i_1 <- run_comment(
    cmt = b1, run = ftext(txt),
    author = "Guiding Questions",
    date = Sys.Date(),
    initials = "Guiding Questions"
  )
  
  return(fpar(comment_i_1))
  
}

text_input_field <- function(doc, placeholder = "Click here to enter text") {
  
  xml_field <- paste0(
    '<w:sdt xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">',
    '<w:sdtPr>',
    '<w:tag w:val="text_field"/>',
    '<w:showingPlcHdr/>',
    #'<w:text/>',
    '</w:sdtPr>',
    '<w:sdtContent>',
    '<w:p>',
    '<w:r>',
    '<w:rPr><w:rStyle w:val="PlaceholderText"/></w:rPr>',
    '<w:t>', placeholder, '</w:t>',
    '</w:r>',
    '</w:p>',
    '</w:sdtContent>',
    '</w:sdt>'
  )
  
  doc <- doc |>
    body_add_xml(str = xml_field)
  
  return(doc)
}
