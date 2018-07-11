#' Clean up Mendeley bibtex library and save it somewhere else
#'
#' @param tex_file Path to tex_file which should be checked for citations
#' @param bibtex_file Path to Mendeley bibtex library
#' @param write_file Path and filename to which the new bibtex library is written
#' @param entry_remove Vector string that defines the entries in each reference to be removed

#' @importFrom magrittr %>% equals
#' @importFrom purrr map map2

#' @export



clean_library <- function(tex_file, bibtex_file, write_file,
                          entry_remove =  c("abstract",
                                            "file",
                                            "keywords",
                                            "url",
                                            "primaryClass",
                                            "eprint",
                                            "archivePrefix",
                                            "arxivId")) {
  entry_label <- readLines(tex_file) %>%
    .[grepl("\\cite",.)] %>%
    strsplit(., "\\{|\\}") %>%
    map(., ~.x[grep("\\cite",.x)+1]) %>%
    map(., ~.x[!grepl("\\cite",.x)]) %>%
    map(., ~strsplit(.x, ",")) %>%
    unlist() %>%
    trimws(., "both") %>%
    unique() %>%
    sort()

  bibtex_library <- readLines(bibtex_file)

  entry_start <- bibtex_library %>%
    trimws(., which = "both") %>%
    substr(.,1,1) %>%
    equals(.,"@") %>%
    which(.)

  entry_end <- c(entry_start[2:length(entry_start)], length(bibtex_library)) - 1

  bibtex_library <- map2(entry_start, entry_end, ~bibtex_library[.x:.y])

  bibtex_label <- bibtex_library %>%
    map(., ~.x[1]) %>%
    map(., ~strsplit(.x, "\\{|\\,")) %>%
    map(., unlist) %>%
    map(., ~.x[2]) %>%
    unlist(.)

  bibtex_missing <- entry_label[!(entry_label %in% bibtex_label)]

  remove_entry <- function(entry, entry_remove) {
    labels <- entry %>%
      strsplit(., "=") %>%
      map(., ~.[1]) %>%
      unlist(.) %>%
      trimws(.)
    entry[!(labels %in% entry_remove)]
  }

  names(bibtex_library) <- bibtex_label
  bibtex_library <- bibtex_library[entry_label]
  bibtex_library <- bibtex_library[!is.na(names(bibtex_library))] %>%
    map(., ~remove_entry(.x, entry_remove)) %>%
    unlist(.)

  writeLines(bibtex_library, write_file)
  cat(paste("References", paste(bibtex_missing, collapse = ", "), " were not available in bibtex library!"))

}

