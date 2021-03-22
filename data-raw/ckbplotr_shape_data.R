ckbplotr_shape_data <- data.frame(
  est       = c(   1, 1.25,  1.5, 1.75,  1.7, 1.85, 2.25, 2.6),
  se        = c(0.08, 0.03, 0.07, 0.15, 0.08, 0.08, 0.07, 0.1),
  rf        = c(  20,   28,   38,   50, 18.5,   25,   37,  47),
  n         = c( 109,  103,  143,  104,  140,  134,  127,  99),
  is_female = c(   0,    0,    0,    0,    1,    1,    1,   1)
)

usethis::use_data(ckbplotr_shape_data, overwrite = TRUE)
