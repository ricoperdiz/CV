split_authors <- function(authors) {
  list_authors <- 
    stringr::str_split(authors, " and ") %>%
    lapply(., function(x) {
      sapply(x, function(x) {
        names <- stringr::str_split(x, pattern = " ")
        sapply(names, function(x) {
          maxnames <- length(x)
          lastname <- x[maxnames]
          othernames <-
            x[-maxnames] %>%
            sapply(., function(x) {
              substr(x, 1, 1) %>%
                paste0(., ".")
            }) %>%
            glue_collapse(., sep = " ")
          outnames <- glue(lastname, othernames, .sep = ", ")
        })
      })
    })
  return(list_authors)
}
bib_entries <- function(file) {
  bib <-
    RefManageR::ReadBib(file, check = FALSE)
  family <-
    map_chr(bib, function(x) {
      map_chr(x$author, function(names) {
        paste(names$family, collapse = " ")
      }) %>%
        paste(collapse = ", ")
    })
  out <-
    dplyr::as_tibble(bib) %>% mutate(surnames = family)
  list_authors <-
    split_authors(out$author)
  out$authors_sep <-
    sapply(list_authors, function(x) {
      glue_collapse(x, sep = ", ", last = " & ")
    })
  out$authors_sep <- gsub("Perdiz, R. O.", "**Perdiz, R. O.**", out$authors_sep)
  # Editors
  out$eds_sep <- out$editor
  out$eds_sep[grepl(" and ", out$eds_sep)] <-
    out$eds_sep[grepl(" and ", out$eds_sep)] %>% 
    split_authors(.) %>% 
    sapply(., function(x) {
      glue_collapse(x, sep = ", ", last = " & ")
    })
  
  return(out)
}
prep_bib <- function(out) {
  
  prep_bibdf <-
    out %>% 
    mutate(
      pub_numb = pmap_chr(list(bibtype, volume, number, pages), function(bibtype, volume, number, pages) {
        
        if (bibtype == "Article") {
          
          if (!is.na(number) & !is.na(pages)) {
            pub_numb_pre <- 
              glue(
                "{volume}({number}): {pages}"
              )
          } else if (is.na(number) & !is.na(pages)) {
            pub_numb_pre <-  
              glue(
                "{volume}: {pages}"
              )
          } else if (!is.na(number) & !is.na(pages)) {
            pub_numb_pre <- 
              glue(
                "{volume}"
              )
          } else {
            pub_numb_pre <- ""
          }
          return(pub_numb_pre)
          
        } else if (bibtype == "InCollection") {
          if (is.na(volume) & !is.na(pages)) {
            pub_numb_pre <- glue("{pages}")
          } else if (is.na(number) & !is.na(pages)) {
            pub_numb_pre <- glue("{volume}: {pages}")
          } else {
            ""
          }
          return(pub_numb_pre)
        } else {
          return("")
        }
        
      }),
      is_year = map_lgl(year, function(x) {
        xx <- as.numeric(x)
        return(!is.na(xx))
      }),
      year_up = map2_chr(year, is_year, function(year, is_year) {
        if (is_year) {
          year_up <- year
        } else {
          year_up <- sub(".*\\}(.+)", "\\1", year)
        }
        return(year_up)
      }),
      doi_text = map_chr(doi, function(doi) {
        if(is.na(doi)) {
          return(glue("."))
        } else {
          return(glue(".\nDOI: {doi}"))
        }
      })#,title2 = map_chr(title, ~gsub("\\{|\\}", "", .x))
    )
  return(prep_bibdf)
}
print_bib <- function(bibdf) {
  
  # out <- 
  #   bib_entries(file) %>% 
  #   prep_bib(.)
  # bibdf <- out
  # bibdf <-
  #   out %>%
  #   filter(bibtype == "MastersThesis")
  
  if (!is.data.frame(bibdf) | !is_tibble(bibdf)) {
    stop("bibdf must be a dataframe.")
  } else {
    message("bibdf is ready to rumble")
  }
  bibtype <- unique(bibdf$bibtype)
  
  if (bibtype == "Article") {
    bib_out <- 
      bibdf %>% 
      arrange(desc(year), authors_sep) %>% 
      glue_data(
        "(@) {authors_sep} {year_up}. {title}. _{journal}_ {pub_numb}{doi_text} \\
\n
")
    } else if (bibtype == "InCollection") {
    bib_out <- 
      bibdf %>% 
      arrange(desc(year), authors_sep) %>% 
      # select(authors_sep, year_up)
      glue_data(
        "(1) {authors_sep} {year_up}. {title}. In: {eds_sep} (Eds.) _{booktitle}_. {publisher}, {address}. Pp. {pages}. \\
\n
        "
      )
  } else if (bibtype == "InProceedings") {
    bib_out <- 
      bibdf %>% 
      arrange(desc(year), authors_sep) %>% 
      # select(authors_sep, year_up)
      glue_data(
        "(1) {authors_sep} {year_up}. {title}. In: _{booktitle}_. {publisher}, {address}. \\
        \n"
      )
  } else if (bibtype == "Misc") {
    bib_out <- 
      bibdf %>% 
      arrange(desc(year), authors_sep) %>% 
      # select(authors_sep, year_up)
      glue_data(
        "(1) {authors_sep} {year_up}. {title}. Dataset published by {howpublished}. Available for download at: {url}{doi_text} \\
        \n"
      )
  } else if (bibtype == "MastersThesis") {
  bib_out <- 
    bibdf %>% 
    arrange(desc(year), authors_sep) %>% 
    # select(authors_sep, year_up)
    glue_data(
      "(1) {authors_sep} {year_up}. {title}. {school}, {address}, Brasil. Pp. {pages} \\
      \n"
    )
}
  # bibdf$bibtype
  #   bib_out
  #   bibdf %>% names
  # bibdf$pages
  # bibdf %>% View
  return(bib_out)
}
