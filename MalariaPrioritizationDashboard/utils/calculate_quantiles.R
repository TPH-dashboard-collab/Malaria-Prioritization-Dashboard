# Funcoes reutilizaveis de quantis

calcular_quantis <- function(x, probs = c(0, 0.20, 0.40, 0.60, 0.80, 1)) {
  stopifnot(is.numeric(x))
  q <- quantile(x, probs = probs, na.rm = TRUE)
  data.frame(quantil = names(q), valor = round(unname(q), 3))
}

classificar_quintil <- function(x) {
  cut(x,
      breaks         = quantile(x, probs = seq(0, 1, 0.2), na.rm = TRUE),
      labels         = paste0("Q", 1:5),
      include.lowest = TRUE)
}

