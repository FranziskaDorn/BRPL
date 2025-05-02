# tidy up
rm(list = ls())
gc(reset = TRUE)
# Set-Up
set.seed(5000)


# Load all
devtools::load_all()

data <- readRDS("~/Git/BRPL/misc/pov_line_data_no_weights.rds")

# Anwendung
# myecdf(data = total1, var = "INval")
tt <- brpl(data = data, var1="leisure", var2="inc_expenses", tau = 0.15)

tt

plot(tt)
