set.seed(57911624)
exampleresults <- function(name){
  data.frame(variable = c('nmr_l_vldl_p', 'nmr_m_vldl_p', 'nmr_s_vldl_p',
                          'nmr_idl_p',
                          'nmr_l_ldl_p', 'nmr_m_ldl_p', 'nmr_s_ldl_p',
                          'nmr_l_vldl_tg', 'nmr_m_vldl_tg', 'nmr_s_vldl_tg',
                          'nmr_idl_tg',
                          'nmr_l_ldl_tg', 'nmr_m_ldl_tg', 'nmr_s_ldl_tg',
                          'nmr_l_vldl_c', 'nmr_m_vldl_c', 'nmr_s_vldl_c',
                          'nmr_idl_c',
                          'nmr_l_ldl_c', 'nmr_m_ldl_c', 'nmr_s_ldl_c'),
             estimate = rnorm(21, 0, 0.02),
             stderr   = 0.012 + abs(rnorm(21, 0, 0.015)),
             n        = round(runif(21, 100, 2000)),
             nb       = round(runif(21, 100, 2000)),
             shape    = rep(15, 21),
             colour   = "black",
             fill     = "black",
             bold     = FALSE,
             diamond  = FALSE,
             ciunder  = FALSE,
             stringsAsFactors = FALSE,
             name = name)
}

forest_plot_data <- rbind(exampleresults("A"),
                          exampleresults("B"))

usethis::use_data(ckbplotr_forest_data, overwrite = TRUE)
