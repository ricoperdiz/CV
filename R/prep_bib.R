#' Limpa códigos LaTeX para texto simples.
#'
#' @param col Vetor de texto de tamanho 1 indicando o nome da coluna
#'
#' @return
#' @export
#'
#' @examples
clean_latex <- function(col) {
  vetor <- gsub("\\'a", "á", col, fixed = TRUE)
  vetor <- gsub("\\'e", "é", vetor, fixed = TRUE)
  vetor <- gsub("\\'i", "í", vetor, fixed = TRUE)
  vetor <- gsub("\\'i", "í", vetor, fixed = TRUE)
  vetor <- gsub("\\'u", "ú", vetor, fixed = TRUE)
  vetor <- gsub("\\^a", "â", vetor, fixed = TRUE)
  vetor <- gsub("\\^e", "ê", vetor, fixed = TRUE)
  vetor <- gsub("\\^o", "ô", vetor, fixed = TRUE)
  vetor <- gsub("\\'o", "ó", vetor, fixed = TRUE)
  vetor <- gsub("\\~a", "ã", vetor, fixed = TRUE)
  vetor <- gsub("\\~n", "ñ", vetor, fixed = TRUE)
  vetor <- gsub("\\cc", "ç", vetor, fixed = TRUE)
  vetor <- gsub("\\{|\\}|\\\\textit|\\\\textbf", "", vetor)
  return(vetor)
}

# Teste
library("testit")
assert('clean latex works', {
  (clean_latex("Francisco Farro{\\~n}ay") == "Francisco Farroñay")
  
  (clean_latex("Amorim, Andr{\\'e} M{\\'a}rcio") == "Amorim, André Márcio")
})

#' Quebra lista de autores em um vetor de texto
#'
#' @param authors
#'
#' @return
#' @export
#'
#' @examples
split_authors <- function(authors) {
  list_authors <- stringr::str_split(authors, " and ")
  list_authors_collapsed <- NULL
  for (i in seq_along(list_authors)) {
    # i = 30
    one_row_author <- list_authors[[i]]
    one_row_author_split <-
      vapply(one_row_author, split_author_names, list(1))
    collapsed <- lapply(one_row_author_split, collapse_author_names)
    list_authors_collapsed <-
      c(list_authors_collapsed, list(collapsed))
  }
  return(list_authors_collapsed)
}
split_author_names <- function(one_row_author) {
  if (any(grepl("\\{", one_row_author))) {
    pos <- grep("\\{", one_row_author)
    list_of_names <- NULL
    for (i in seq_along(one_row_author)) {
      # i = 3
      if (!i %in% pos) {
        list_of_names_simple <-
          stringr::str_split(one_row_author[i], pattern = " ")
        list_of_names <- c(list_of_names, list_of_names_simple)
      } else {
        list_of_names_simple <-
          stringr::str_split(one_row_author[i], pattern = "\\{") %>%
          unlist(.) %>%
          gsub("\\}", "", .) %>%
          list(.)
        list_of_names <- c(list_of_names, list_of_names_simple)
      }
    }
    
    return(list_of_names)
  } else {
    list_of_names <- stringr::str_split(one_row_author, pattern = " ")
    return(list_of_names)
  }
  
  
}
prepare_first_names <- function(first_names) {
  extract_first_letters <- substr(first_names, 1, 1)
  res <- paste0(extract_first_letters, ".")
  return(res)
}
collapse_author_names <- function(author_names_split) {
  maxnames <- length(author_names_split)
  lastname <- author_names_split[maxnames]
  othernames <-
    glue::glue_collapse(
      vapply(
        author_names_split[-maxnames],
        FUN =  prepare_first_names,
        FUN.VALUE = character(1)
      ),
      sep = " "
    )
  outnames <- glue::glue(lastname, othernames, .sep = ", ")
  return(outnames)
}


bib_entries <- function(file) {
  # reviews <- "/Users/ricoperdiz/Documents/PROFISSIONAL/producao_cientifica/08_pareceres/reviews.bib"
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
    gsub("Perdiz, R. O.|Perdiz, R.", "**Perdiz, R. O.**", out$authors_sep)
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
  # out = bib_entries(reviews)
  # out %>% View
  # ########
  if (!"volume"  %in% colnames(out)) {
    df_input <- mutate(out,
                       volume = "",
                       number = "")
  } else {
    df_input <- out
  }
  
  prep_bibdf <-
    dplyr::mutate(
      df_input,
      pub_numb = purrr::pmap_chr(list(bibtype, volume, number, pages), function(bibtype, volume, number, pages) {
        if (bibtype == "Article") {
          if (!is.na(number) && !is.na(pages)) {
            pub_numb_pre <-
              glue::glue("{volume}({number}): {pages}")
          } else if (is.na(number) & !is.na(pages)) {
            pub_numb_pre <-
              glue::glue("{volume}: {pages}")
          } else if (is.na(number) & is.na(pages)) {
            pub_numb_pre <-
              glue::glue("{volume}")
          } else if (is.na(pages)) {
            pub_numb_pre <-
              glue::glue("{volume}({number})}")
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
          return(glue::glue("https://doi.org/{doi}"))
        }
      }) # ,title2 = purrr::map_chr(title, ~gsub("\\{|\\}", "", .x))
    )
  return(prep_bibdf)
}
print_bib <- function(bibdf) {
  # Teste # Comentar depois de testar #####
  # out <-
  #   bib_entries(reviews) %>%
  #   prep_bib(.)
  # bibdf <- out
  # bibdf <-
  #   out %>%
  #   filter(bibtype == "Article")
  # 
  # bibdf %>% View
    # filter((bibtype == "Article" & grepl("dataset", comment)))
  #
  # #   #   #   # filter(bibtype == "MastersThesis")
  #   filter(bibtype == "Book")
  # # filter(bibtype == "Misc")
  # #   # filter(bibtype == "InCollection")
  # #   filter(bibtype == "Article") %>%
  # #   filter(grepl("preprint", comment)) %>%
  # #   filter(is_year)
  # bibdf
  ######################
  
  # bibdf <- dad
  
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
    ### # SEM DATAPAPER PREPRINT IUCN DATAPAPER----
    #                             #
    if (!grepl("dataset|preprint|iucn", comment)) {
      # Se acabou de ser publicado e ainda nao possui numero do volume
      if (bibdf$pub_numb == "NA") {
        bib_out <-
          bibdf %>%
          dplyr::arrange(desc(year), authors_sep) %>%
          glue::glue_data("{authors_sep} {year_up}. {title}. _{journal}_.  \\
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
            "{authors_sep} {year_up}. {title}. _{journal}_ {pub_numb}.  \\
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
        glue::glue_data("{authors_sep} {year_up}. {title}. _{journal}_.  \\
        {doi_text}\\
\n
")
      #                             #
      ### # COM DATAPAPER PREPRINT IUCN DATAPAPER ----
      #                             #
    } else if (grepl("dataset", comment)) {
      bib_out <-
        bibdf %>%
        dplyr::arrange(desc(year), authors_sep) %>%
        glue::glue_data(
          "{authors_sep} {year_up}. {title}. _{journal}_ {pub_numb}.  \\
        {doi_text}\\
\n
"
        )
      #                             #
      ### IUCN ----
      #                             #
    } else if (grepl("iucn", comment)) {
      # if (!"pub_numb"  %in% colnames(bibdf)) {
        bib_out <-
          bibdf %>%
          dplyr::arrange(desc(year), authors_sep) %>%
          glue::glue_data("{authors_sep} {year}. _{title}_. _{journal}_ {year}: {pages}.  \\
        https://doi.org/{doi}\\
\n
")
      # }
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
        "{authors_sep} {year_up}. {title}. In: {eds_sep} (Eds.) _{booktitle}_. {publisher}, {address}. Pp. {pages}.  \\
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
          "{authors_sep} {year_up}. {title}. In: _{booktitle}_. {publisher}, {address}. Disponível em: <{url}>.  \\
\n
        "
        )
    } else {
      bib_out <-
        bibdf %>%
        # dplyr::arrange(desc(year), authors_sep) %>%
        # select(authors_sep, year_up)
        glue::glue_data(
          "{authors_sep} {year_up}. {title}. In: _{booktitle}_. {publisher}, {address}. Available at: <{url}>.  \\
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
        glue::glue_data("{authors_sep} {year_up}. {title}. Disponível em: <{url}>.  \\
\n
        ")
    } else {
      bib_out <-
        bibdf %>%
        dplyr::arrange(desc(year), authors_sep) %>%
        # select(authors_sep, year_up)
        glue::glue_data("{authors_sep} {year_up}. {title}. Available at: <{url}>.  \\
\n
        ")
    }
    #                             #
    ### RESUMOS ----
    #                             #
  } else if (bibtype == "InProceedings") {
    bib_out <-
      bibdf %>%
      dplyr::arrange(desc(year), authors_sep) %>%
      # select(authors_sep, year_up)
      glue::glue_data("{authors_sep} {year_up}. {title}. In: _{booktitle}_. {address}.  \\
        \n")
    #                             #
    ### DATASETS WITHOUT DATAPAPER----
    #                             #
  } else if (grepl("dataset", comment)) {
    if (note == "portuguese") {
      bib_out <-
        bibdf %>%
        dplyr::arrange(desc(year), authors_sep) %>%
        # select(authors_sep, year_up)
        glue::glue_data(
          "{authors_sep} {year_up}. {title}. Conjunto de dados publicado por {howpublished}. Disponível para baixar em: <{doi_text}>.  \\
        \n"
        )
    } else {
      bib_out <-
        bibdf %>%
        dplyr::arrange(desc(year), authors_sep) %>%
        # select(authors_sep, year_up)
        glue::glue_data(
          "{authors_sep} {year_up}. {title}. Dataset published by {howpublished}. Available for download at: <{doi_text}>.  \\
        \n"
        )
    }
    #                             #
    ### DOCUMENTARIOS - ENTREVISTAS  ----
    #                             #
  } else if (bibtype == "Misc") {
    if (any(note %in% c("Documentario", "documentario", "Documentary", "documentary","Interview", "interview","entrevista", "Entrevista"))) {
      bib_out <-
        bibdf %>%
        dplyr::mutate_at("year", as.numeric) %>%
        dplyr::mutate_at("title", ~ gsub("^\\{|\\}$", "", .)) %>%
        dplyr::arrange(desc(year), authors_sep) %>%
        glue::glue_data("* {title} {year}.  \\
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
          "{authors_sep} {year_up}. {title}. Dissertação de mestrado, {school}, {address}. Pp. {pages}. Disponível em: <{url}>.  \\
      \n"
        )
    } else {
      bib_out <-
        bibdf %>%
        dplyr::arrange(desc(year), authors_sep) %>%
        # select(authors_sep, year_up)
        glue::glue_data(
          "{authors_sep} {year_up}. {title}. Master's Thesis, {school}, {address}, Brasil. Pp. {pages}. Available at: <{url}>.  \\
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
          "{authors_sep} {year_up}. {title}. Tese de doutorado, {school}, {address}. Pp. {pages}. Disponível em: <{url}>.  \\
      \n"
        )
    } else {
      bib_out <-
        bibdf %>%
        dplyr::arrange(desc(year), authors_sep) %>%
        # select(authors_sep, year_up)
        glue::glue_data(
          "{authors_sep} {year_up}. {title}. PhD thesis, {school}, {address}. Pp. {pages}. Available at: <{url}>.  \\
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
      glue::glue_data("{authors_sep} {year}. {title}.  \\
      \n")
  }
  # bibdf$bibtype
  #   bib_out
  #   bibdf %>% names
  # bibdf$pages
  # bibdf %>% View
  
  return(bib_out)
}

print_bib_data <- function(bib_data, fix_accent = TRUE) {
  if (fix_accent) {
    res <- 
      print_bib(bib_data) %>%
      fix_accent_papers(.)
    return(res)
  } else {
    res <- 
      print_bib(bib_data)
    return(res)
  }
  
}

fix_accent_papers <- function(glued_text) {
  res <- 
    gsub("Journal_ \\.", "Journal_\\.", glued_text) %>%
    gsub("Brief_ \\.", "Brief_\\.", .) %>%
    gsub("gicas_ \\.", "gicas_\\.", .) %>%
    gsub("_Nature_ \\.", "_Nature_\\.", .) %>%
    gsub("Ciências_ \\.", "Ciências_\\.", .) %>%
    gsub("\\\\textit|\\{|\\}", "", .) %>%
    # acentos
    gsub("\\\\\'e", "é", .) %>%
    gsub("\\\\\\^e", "ê", .) %>%
    gsub("\\\\\'o", "ó", .) %>%
    gsub("\\\\\'a", "á", .) %>%
    gsub("\\\\\\^o", "ô", .) %>%
    gsub("\\\\\\^a", "â", .) %>%
    gsub("\\\\\\~a", "ã", .) %>%
    gsub("\\. NA, NA\\.", "\\.", .) %>%
    gsub(", NA\\.", "\\.", .) %>% 
    gsub(" NA.", ".", .)
  return(res)
}
print_published_data <-
  function(published_data, list_type = "numbered", ...) {
    # published_data = media_appear
    published_data_glued <-
      data.frame(texto = rep(NA, nrow(published_data)))
    
    if (list_type == "numbered") {
      for (i in seq_along(published_data$bibtype)) {
        # i = 1
        published_data_glued$texto[i] <-
          glue::glue("{i}. {print_bib_data(published_data[i,], ...)}")
      }
      published_data_glued %>%
        glue::glue_data("{texto}")
    } else if (list_type == "bullet") {
      for (i in seq_along(published_data$bibtype)) {
        # i = 1
        published_data_glued$texto[i] <-
          glue::glue("* {print_bib_data(published_data[i,], ...)}")
      }
      published_data_glued %>%
        glue::glue_data("{texto}")
    } else if (list_type == "as_is") {
      for (i in seq_along(published_data$bibtype)) {
        # i = 1
        published_data_glued$texto[i] <-
          print_bib_data(published_data[i, ], ...)
      }
      published_data_glued %>%
        glue::glue_data("{texto}")
    }
  }
