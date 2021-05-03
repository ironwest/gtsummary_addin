# library(tidyverse)
#
# datr <- read_csv("https://cdn1.sph.harvard.edu/wp-content/uploads/sites/1268/1268/20/nhefs.csv")
#
# dat <- datr %>%
#   select(
#     death, sbp, dbp, sex, age,race, income, wt71,
#     marital, school)
#
# model1 <- glm("death ~ wt71",binomial,dat)
# model2 <- glm("death ~ wt71 + sex + age",binomial,dat)
# model3 <- glm("death ~ wt71 + sex + age + sbp",binomial,dat)
# model4 <- glm("death ~ wt71 + sex + age + sbp + race + income",binomial,dat)
# model5 <- glm("death ~ wt71 + sex + age + sbp + race + income + marital + school",binomial,dat)
#
# tab1 <- tbl_regression(model1)
# tab2 <- tbl_regression(model2)
# tab3 <- tbl_regression(model3)
# tab4 <- tbl_regression(model4)
# tab5 <- tbl_regression(model5)
#
# tabmerge <-tbl_merge(tbls = list(tab1,tab2,tab3,tab4,tab5))
# tabmerge
# tabmerge$table_body <- tabmerge$table_body %>% filter(label=="wt71")
