my_corr_plots <- function(cor_list, data, db_name = "SOGEDI") {
  p1 <- wrap_elements(
    ~corrplot::corrplot(
      cor_list$pearson,
      method = "color",
      type = "upper",
      col = colorRampPalette(c("#E16462", "white", "#0D0887"))(12),
      tl.pos = "lt",
      tl.col = "black",
      addrect = 2,
      rect.col = "black",
      addCoef.col = "white",
      cl.cex = 0.8,
      cl.align.text = 'l',
      number.cex = 1.1,
      na.label = "-",
      bg = "white"
    )
  ) + labs(title = 'I. Pearson correlations')
  
  p2 <- wrap_elements(
    ~corrplot::corrplot(
      cor_list$polychoric,
      method = "color",
      type = "upper",
      col = colorRampPalette(c("#E16462", "white", "#0D0887"))(12),
      tl.pos = "lt",
      tl.col = "black",
      addrect = 2,
      rect.col = "black",
      addCoef.col = "white",
      cl.cex = 0.8,
      cl.align.text = 'l',
      number.cex = 1.1,
      na.label = "-",
      bg = "white"
    )
  ) + labs(title = 'II. Polychoric correlations')
  
  p1 / p2 +
    plot_annotation(
      caption = paste0(
        "Source: Authors calculation based on ", db_name, 
        " database (n=", nrow(data), ")"
      )
    )
}

# Ejemplo de uso:
# Primero calculas las matrices con fit_correlations
# res <- fit_correlations(db_proc, c("eco_in_1", "eco_in_2", "eco_in_3"))
# Luego generas la visualización:
# my_corr_plots(res, db_proc, "SOGEDI")

my_corr_plots(res1, db_proc, "SOGEDI")
