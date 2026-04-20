plot_pothciv <- function(x, title = "") {

  # check if multiple outcomes, only 1 can have varying CIVs

  df <- x$all
  civs <- x$CIVs

  if(ncol(civs)>1) {

    nval <- apply(civs, 2, function(x) length(unique(x)))

    if(length(which(nval>1)) == 1) {

      outcome <- colnames(civs)[which(nval>1)]

      df <- select(ungroup(df), poth, !!sym(outcome)) %>% summarise(poth = unique(poth), .by = !!sym(outcome))

    } else {

      stop("Function only compatible for objects where only one outcome has a range of CIVs")

    }

  } else {

    outcome <- colnames(civs)[1]

  }

  ggplot(df, aes(x = !!sym(outcome), y = poth)) +
    geom_line() +
    geom_point(col = "black", shape = 21, size = 2.5, fill = "hotpink") +
    theme_bw() +
    geom_hline(yintercept = 0) +
    labs(x = paste0("CIV (", colnames(civs)[1], ")"), y = "POTH", title = title)



}
