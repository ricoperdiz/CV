clean_latex <- function(col) {
  vetor <-
    gsub("{\\'a}", "á", col, fixed = TRUE) %>%
    gsub("{\\'e}", "é", ., fixed = TRUE) %>%
    gsub("{\\'i}", "í", ., fixed = TRUE) %>%
    gsub("\\'i", "í", ., fixed = TRUE) %>%
    gsub("{\\'u}", "ú", ., fixed = TRUE) %>%
    gsub("{\\^a}", "â", ., fixed = TRUE) %>%
    gsub("{\\^e}", "ê", ., fixed = TRUE) %>%
    gsub("{\\^o}", "ô", ., fixed = TRUE) %>%
    gsub("{\\'o}", "ó", ., fixed = TRUE) %>%
    gsub("{\\~a}", "ã", ., fixed = TRUE) %>%
    gsub("\\cc", "ç", ., fixed = TRUE)
  vetor <- gsub("\\{|\\}|\\\\textit", "", vetor)
  return(vetor)
}

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
            glue::glue_collapse(., sep = " ")
          outnames <- glue::glue(lastname, othernames, .sep = ", ")
        })
      })
    })
  return(list_authors)
}
bib_entries <- function(file) {
  bib <-
    RefManageR::ReadBib(file, check = FALSE)
  family <-
    purrr::map_chr(bib, function(x) {
      purrr::map_chr(x$author, function(names) {
        paste(names$family, collapse = " ")
      }) %>%
        paste(collapse = ", ")
    })
  out <-
    dplyr::as_tibble(bib) %>% dplyr::mutate(surnames = family)
  list_authors <-
    split_authors(out$author)
  out$authors_sep <-
    sapply(list_authors, function(x) {
      glue::glue_collapse(x, sep = ", ", last = " & ")
    })
  out$authors_sep <-
    gsub("Perdiz, R. O.", "**Perdiz, R. O.**", out$authors_sep)
  # Editors
  if (any(is.na(unique(out$editor)))) {
    out$eds_sep <- out$editor
    out$eds_sep[grepl(" and ", out$eds_sep)] <-
      out$eds_sep[grepl(" and ", out$eds_sep)] %>%
      split_authors(.) %>%
      sapply(., function(x) {
        glue::glue_collapse(x, sep = ", ", last = " & ")
      })
  }

  return(out)
}
prep_bib <- function(out) {
  # Teste # Comentar apos testar
  # out = bib_entries(file)
  # out %>% View
  # ########

  prep_bibdf <-
    out %>%
    dplyr::mutate(
      pub_numb = purrr::pmap_chr(list(bibtype, volume, number, pages), function(bibtype, volume, number, pages) {
        if (bibtype == "Article") {
          if (!is.na(number) & !is.na(pages)) {
            pub_numb_pre <-
              glue::glue("{volume}({number}): {pages}")
          } else if (is.na(number) & !is.na(pages)) {
            pub_numb_pre <-
              glue::glue("{volume}: {pages}")
          } else if (is.na(number) & is.na(pages)) {
            pub_numb_pre <-
              glue::glue("{volume}")
          } else {
            pub_numb_pre <- ""
          }
          return(pub_numb_pre)
        } else if (bibtype == "InCollection") {
          if (is.na(volume) & !is.na(pages)) {
            pub_numb_pre <- glue::glue("{pages}")
          } else if (is.na(number) & !is.na(pages)) {
            pub_numb_pre <- glue::glue("{volume}: {pages}")
          } else {
            pub_numb_pre <- ""
          }
          return(pub_numb_pre)
        } else {
          return("")
        }
      }),
      is_year = purrr::map_lgl(year, function(x) {
        xx <- as.numeric(x)
        vetor_lgl <- as.logical(!is.na(xx))
        return(vetor_lgl)
      }),
      year_up = purrr::map2_chr(year, is_year, function(year, is_year) {
        if (is_year) {
          year_up <- year
        } else {
          year_up <- sub(".*\\}(.+)", "\\1", year)
        }
        return(year_up)
      }),
      doi_text = purrr::map_chr(doi, function(doi) {
        if (is.na(doi)) {
          return(glue::glue(""))
        } else {
          return(glue::glue("{doi}"))
        }
      }) # ,title2 = purrr::map_chr(title, ~gsub("\\{|\\}", "", .x))
    )
  return(prep_bibdf)
}
print_bib <- function(bibdf) {
  # Teste # Comentar depois de testar #####
  # out <-
  #   bib_entries(file) %>%
  #   prep_bib(.)
  # # # bibdf <- out
  # bibdf <-
  #   out %>%
  #   filter(bibtype == "Article") %>%
  #   filter(grepl("dataset", comment))
  # #   #   #   # filter(bibtype == "MastersThesis")
  #   filter(bibtype == "Book")
  # # filter(bibtype == "Misc")
  # #   # filter(bibtype == "InCollection")
  # #   filter(bibtype == "Article") %>%
  # #   filter(grepl("preprint", comment)) %>%
  # #   filter(is_year)
  # bibdf
  ######################


  if (!is.data.frame(bibdf) | !is_tibble(bibdf)) {
    stop("bibdf must be a dataframe.")
  } else {
    message("bibdf is ready to rumble")
  }

  # combinacoes <- distinct(out, bibtype, comment) %>% arrange(bibtype)
  # combinacoes


  bibtype <- unique(bibdf$bibtype)
  note <- unique(bibdf$note)
  url <- unique(bibdf$url)
  comment <- unique(bibdf$comment)
  
  if (bibtype == "Article") {
    #                             #
    ### # ARTIGOS PUBLICADOS ----
    #                             #
    
    #                             #
    ### # NO DATAPAPER ----
    #                             #
    if (!grepl("dataset|preprint", comment)) {
      # Se acabou de ser publicado e ainda nao possui numero do volume
      if (bibdf$pub_numb == "NA") {
        bib_out <-
          bibdf %>%
          dplyr::arrange(desc(year), authors_sep) %>%
          glue::glue_data("(@) {authors_sep} {year_up}. {title}. _{journal}_. \\
        {doi_text}\\
\n
")
        #                             #
        ### Publicacao normal, com NUMERO DO VOLUME
        #                             #
      } else {
        bib_out <-
          bibdf %>%
          dplyr::arrange(desc(year), authors_sep) %>%
          glue::glue_data(
            "(@) {authors_sep} {year_up}. {title}. _{journal}_ {pub_numb}. \\
        {doi_text}\\
\n
"
          )
      }
      #                             #
      ### PREPRINT ----
      #                             #
    } else if (grepl("preprint", comment)) {
      bib_out <-
        bibdf %>%
        dplyr::arrange(desc(year), authors_sep) %>%
        glue::glue_data("(@) {authors_sep} {year_up}. {title}. _{journal}_. \\
        {doi_text}\\
\n
")
      #                             #
      ### # DATAPAPER ----
      #                             #
    } else if (grepl("dataset", comment)) {
      bib_out <-
        bibdf %>%
        dplyr::arrange(desc(year), authors_sep) %>%
        glue::glue_data(
          "(@) {authors_sep} {year_up}. {title}. _{journal}_ {pub_numb}.\\
        {doi_text}\\
\n
"
        )
    }
    #                             #
    ### CAPITULO DE LIVROS ----
    #                             #
  } else if (bibtype == "InCollection" && is.na(url)) {
    bib_out <-
      bibdf %>%
      dplyr::arrange(desc(year), authors_sep) %>%
      # select(authors_sep, year_up)
      glue::glue_data(
        "(@) {authors_sep} {year_up}. {title}. In: {eds_sep} (Eds.) _{booktitle}_. {publisher}, {address}. Pp. {pages}. \\
\n
        "
      )
    #                             #
    ### CAPITULOS DE LIVROS ----
    #                             #
  } else if (bibtype == "InCollection" && !is.na(url)) {
    if (note == "portuguese") {
      bib_out <-
        bibdf %>%
        # dplyr::arrange(desc(year), authors_sep) %>%
        # select(authors_sep, year_up)
        glue::glue_data(
          "(@) {authors_sep} {year_up}. {title}. In: _{booktitle}_. {publisher}, {address}. Disponível em: <{url}>. \\
\n
        "
        )
    } else {
      bib_out <-
        bibdf %>%
        # dplyr::arrange(desc(year), authors_sep) %>%
        # select(authors_sep, year_up)
        glue::glue_data(
          "(@) {authors_sep} {year_up}. {title}. In: _{booktitle}_. {publisher}, {address}. Available at: <{url}>. \\
\n
        "
        )
    }
    #                             #
    ### LIVROS ----
    #                             #
  } else if (bibtype == "Book" && !is.na(url)) {
    if (note == "portuguese") {
      bib_out <-
        bibdf %>%
        dplyr::arrange(desc(year), authors_sep) %>%
        # select(authors_sep, year_up)
        glue::glue_data(
          "(@) {authors_sep} {year_up}. {title}. Disponível em: <{url}>. \\
\n
        "
        )
    } else {
      bib_out <-
        bibdf %>%
        dplyr::arrange(desc(year), authors_sep) %>%
        # select(authors_sep, year_up)
        glue::glue_data(
          "(@) {authors_sep} {year_up}. {title}. Available at: <{url}>. \\
\n
        "
        )
    }
    #                             #
    ### RESUMOS ----
    #                             #
  } else if (bibtype == "InProceedings") {
        bib_out <-
          bibdf %>%
          dplyr::arrange(desc(year), authors_sep) %>%
          # select(authors_sep, year_up)
          glue::glue_data(
            "(@) {authors_sep} {year_up}. {title}. In: _{booktitle}_. {address}. \\
        \n"
          )
    #                             #
    ### DATASETS WITHOUT DATAPAPER----
    #                             #
  } else if (grepl("dataset", comment)) {
      bib_out <-
        bibdf %>%
        dplyr::arrange(desc(year), authors_sep) %>%
        # select(authors_sep, year_up)
        glue::glue_data(
          "(@) {authors_sep} {year_up}. {title}. Dataset published by {howpublished}. Available for download at: {url}{doi_text} \\
        \n"
        )
    #                             #
    ### DOCUMENTARIOS - ENTREVISTAS  ----
    #                             #
  } else if (bibtype == "Misc") {
    if (any(note %in% c("Documentario", "Interview"))) {
      bib_out <-
        bibdf %>%
        dplyr::mutate_at("year", as.numeric) %>%
        dplyr::mutate_at("title", ~ gsub("^\\{|\\}$", "", .)) %>%
        dplyr::arrange(desc(year), authors_sep) %>%
        glue::glue_data("* {title} {year}. \\
      \n")
    }
    #                             #
    ### Dissertacao MESTRADO ----
    #                             #
  } else if (bibtype == "MastersThesis") {
    if (note == "portuguese") {
      bib_out <-
        bibdf %>%
        dplyr::arrange(desc(year), authors_sep) %>%
        # select(authors_sep, year_up)
        glue::glue_data(
          "(@) {authors_sep} {year_up}. {title}. Dissertação de mestrado, {school}, {address}. Pp. {pages}. Disponível em: <{url}>. \\
      \n"
        )
    } else {
      bib_out <-
        bibdf %>%
        dplyr::arrange(desc(year), authors_sep) %>%
        # select(authors_sep, year_up)
        glue::glue_data(
          "(@) {authors_sep} {year_up}. {title}. Master's Thesis, {school}, {address}, Brasil. Pp. {pages}. Available at: <{url}>. \\
      \n"
        )
    }

    #                             #
    ### Tese de DOUTORADO ----
    #                             #
  } else if (bibtype == "PhdThesis") {
    if (note == "portuguese") {
      bib_out <-
        bibdf %>%
        dplyr::arrange(desc(year), authors_sep) %>%
        # select(authors_sep, year_up)
        glue::glue_data(
          "(@) {authors_sep} {year_up}. {title}. Tese de doutorado, {school}, {address}. Pp. {pages}. Disponível em: <{url}>. \\
      \n"
        )
    } else {
      bib_out <-
        bibdf %>%
        dplyr::arrange(desc(year), authors_sep) %>%
        # select(authors_sep, year_up)
        glue::glue_data(
          "(@) {authors_sep} {year_up}. {title}. PhD thesis, {school}, {address}. Pp. {pages}. Available at: <{url}>. \\
      \n"
        )
    }
    #                             #
    ### PARECERES
    #                             #
  } else if (bibtype == "TechReport") {
    # revout$authors_sep
    # revout %>%
    #   filter((bibtype == "TechReport" & grepl("revisão", note)))
    bib_out <-
      bibdf %>%
      # revout %>%
      # filter((bibtype == "TechReport" & grepl("revisão", note))) %>%
      dplyr::mutate_at("year", as.numeric) %>%
      dplyr::mutate_at("title", ~ gsub("^\\{|\\}$", "", .)) %>%
      dplyr::arrange(desc(year), authors_sep) %>%
      glue::glue_data("(@) {authors_sep} {year}. {title}. \\
      \n")
  }
  # bibdf$bibtype
  #   bib_out
  #   bibdf %>% names
  # bibdf$pages
  # bibdf %>% View
  
  return(bib_out)
}
