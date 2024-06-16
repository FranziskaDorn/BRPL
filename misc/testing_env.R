library(devtools)

data("nutritionpoverty")
bivqfun_old(data = nutritionpoverty, var1="NUval", var2="INval", tau = 0.25)

bivqfun(data = nutritionpoverty, var1="NUval", var2="INval", tau = 0.25)
