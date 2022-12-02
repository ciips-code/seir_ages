trade_off_summary$pobreza_primer_semestre <- c(-4.7679,-4.7662,-6.4195,-8.1135,-9.7840)
trade_off_summary$pobreza_segundo_semestre <- c(-3.1902,-3.1895,-4.5031,-5.9280,-7.3501)

m1 <- lm(trade_off_summary$muertes_primer_semestre ~ trade_off_summary$costo_primer_semestre, data = trade_off_summary)
m2 <- lm(trade_off_summary$muertes_segundo_semestre ~ trade_off_summary$costo_segundo_semestre, data = trade_off_summary)
m3 <- lm(trade_off_summary$muertes_primer_semestre ~ trade_off_summary$pobreza_primer_semestre, data = trade_off_summary)
m4 <- lm(trade_off_summary$muertes_segundo_semestre ~ trade_off_summary$pobreza_segundo_semestre, data = trade_off_summary)

save.image("data/trade_off_summary.RData")
