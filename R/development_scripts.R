# library(tidyverse)
# library(gtsummary)
#
# ppp <- palmerpenguins::penguins
#
# colnames(ppp)
#
#
# fmla_con_uni <- "flipper_length_mm ~ sex"
# fmla_con_multi <- "flipper_length_mm ~ sex + island + species + bill_length_mm + body_mass_g"
# fmla_dic_uni <- "sex ~ flipper_length_mm"
# fmla_dic_multi <- "sex ~ flipper_length_mm + island + species + bill_length_mm + body_mass_g"
#
# get_terms <- function(mod){
#   attributes(mod$terms)$term.labels
# }
#
#
# #stats::lm
# fmla_con_multi <- "flipper_length_mm ~ sex + island + species + bill_length_mm + body_mass_g"
# model_lm <- lm(formula = fmla_con_multi, data = ppp)
#
# model_lm$model %>% colnames()
#
# get_terms(model_lm)
# class(model_lm)
#
# #stats::glm
# fmla_dic_multi <- "sex ~ flipper_length_mm + island + species + bill_length_mm + body_mass_g"
# model_glm <- glm(formula = fmla_dic_multi, data = ppp, family = binomial)
#
# model_glm$model %>% colnames()
#
# model_glm2 <- glm(formula = "sex~species*island", family=binomial, data=ppp)
#
# model_glm2$model %>% colnames()
#
# model_glm2$model %>% as_tibble()
# model_glm2$formula
#
# tt <- model_glm2$terms
# str(tt)
# attributes(tt)$term.labels
#
#
# get_terms(model_glm2)
# class(model_glm2)
#
# #survival::coxph
# library(survival)
# bladder1 <- bladder[bladder$enum < 5, ]
# model_coxph <- coxph(Surv(stop, event) ~ (rx + size + number) * strata(enum),
#       cluster = id, bladder1)
#
# get_terms(model_coxph)
# class(model_coxph)
#
# tbl_regression(model_coxph, include=c(rx, "rx:strata(enum)"))
#
# #survival::clogit
# resp <- levels(logan$occupation)
# n <- nrow(logan)
# indx <- rep(1:n, length(resp))
# logan2 <- data.frame(logan[indx,],
#                      id = indx,
#                      tocc = factor(rep(resp, each=n)))
# logan2$case <- (logan2$occupation == logan2$tocc)
# model_clogit <- clogit(case ~ tocc + tocc:education + strata(id), logan2)
#
# get_terms(model_clogit)
# class(model_clogit)
#
# tbl_regression(model_clogit)
#
# #survival::survreg
# model_survreg <- survreg(Surv(durable, durable>0, type='left') ~ age + quant,
#                     data=tobin, dist='gaussian')
#
# get_terms(model_survreg)
# class(model_survreg)
# tbl_regression(model_survreg, include="age")
#
# #lme4::glmer
# library(lme4)
#   model_glmer <- glmer(cbind(incidence, size - incidence) ~ period + (1 | herd),
#              data = cbpp, family = binomial)
#
# str(model_glmer)
#
# get_terms(model_glmer) #model_glmer@frame %>% names()?
# class(model_glmer)
# tbl_regression(model_glmer, include="herd")
#
# #lme4::lmer
# model_lmer <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
# get_terms(model_lmer) #?model_lmer@frame %>% names()
# class(model_lmer)
# tbl_regression(model_lmer)
#
# #geepack::geeglm
# library(geepack)
# data(dietox)
# dietox$Cu     <- as.factor(dietox$Cu)
# mf <- formula(Weight ~ Cu * (Time + I(Time^2) + I(Time^3)))
# model_geeglm <- geeglm(mf, data=dietox, id=Pig, family=poisson("identity"), corstr="ar1")
# mf2 <- formula(Weight ~ Cu * Time + I(Time^2) + I(Time^3))
# model_geeglm <- geeglm(mf2, data=dietox, id=Pig, family=poisson("identity"), corstr="ar1")
# anova(gee2)
#
# get_terms(model_geeglm)
# class(model_geeglm)
# tbl_regression(model_geeglm, include=c("I(Time^2)"))
#
#
#
# class(model_lm) # "lm"
# class(model_glm) # "glm", "lm
# class(model_coxph) # "coxph"
# class(model_clogit) # "clogit", "coxph"
# class(model_survreg) # "survreg"
# class(model_glmer) # "lme4"
# class(model_lmer) # "lmerMod
# class(model_geeglm) # "geeglm", "gee", "glm", "lm"
#
# class_for_models <- c("lm", "glm", "coxph", "clogit", "survreg", "lme4", "lmerMod", "geeglm", "gee")
#
# model_glm$terms
# tbl_regression(model_glm, include=c("island"))
#
# gtsummary::tbl_regression(
#   mod_lm_multi,
#   label = aaa,
#   exponentiate = FALSE,
#   include = everything(),
#   show_single_row = NULL,
#   conf.level = NULL,
#   intercept = FALSE,
#   estimate_fun = NULL,
#   pvalue_fun = NULL,
#   tidy_fun = broom::tidy,
#   add_estimate_to_reference_rows = FALSE,
#   show_yesno = NULL,
#   exclude = NULL
# )
#
#
#
#
#
#
#
#
# #----------------------------------
# #
# # library(shiny)
# # source("R/module_load_variable.R")
# # source("R/argument_options.R")
# #
# # ui <- fluidPage(
# #   variableLoaderModalUI(id = "test")
# # )
# #
# # server <- function(input,output,session){
# #   res <- variableLoaderModalServer(id = "test", target_type = "data.frame")
# #   observeEvent(res(),{
# #     print(res())
# #   })
# # }
# #
# # shinyApp(ui,server)
#
# #dat <- iris
# # library(palmerpenguins)
# # ppp <- penguins
