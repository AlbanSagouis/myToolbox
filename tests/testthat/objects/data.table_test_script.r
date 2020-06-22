# data.table functions test
library(data.table)
library(rprojroot)

dt <- as.data.table(cbind(x=1:4, y=1:2))
tt <- dt[, .(sumtst = sum(x)), by = y]

fwrite(tt, paste0(rprojroot::find_package_root_file(),'/tests/testthat/objects/dt_test_success.csv'))

