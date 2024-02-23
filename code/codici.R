pkg()
library(lme4)
library(sjPlot)
library(patchwork)
library(ggstats)
dt <- read_excel(here("data","datipupe.xlsx"))
#dt$idpupe <- seq(1:nrow(dt))
dt <- clean_names(dt)
new_dati <- dt %>% 
  select(specie, larval_diet, replica, day) %>%  
  unique() %>%   
  group_by(specie, larval_diet, replica) %>% 
  mutate(timing = rank(day)) %>% 
  right_join(dt, by = c("specie", "larval_diet", "replica", "day"))

 

plott <- new_dati %>% 
  split(.$specie) %>% 
  map( ~ ggplot(.)+
         aes(x = timing, y = pupal_weight)+
         geom_boxplot(aes(x = factor(timing), y = pupal_weight))+ 
         geom_jitter(alpha = 0.3)+
         geom_smooth(se = FALSE, method = "lm")+
         facet_wrap(larval_diet ~ replica, scales = "free", ncol = 3)+
         theme_bw()
         
       )

fit <- new_dati %>%
  mutate(replica = factor(replica),
         pweight = scale(pupal_weight)) %>%
  split(.$specie) %>%
  map(~ lmer(pupal_weight*1000 ~   timing  + larval_diet + (1|replica), data = .))

saveRDS(fit, "models.RDS")


fit <- readRDS("models.RDS")



 

#Argyrostoma
 
 
argy <- fit$argyrostoma

ggcoef_table(
  argy, intercept = TRUE,add_reference_rows = FALSE,
  table_stat = c("estimate", "std.error", "ci"),
  ci_pattern = "{conf.low} to {conf.high}",
  significance = NULL, 
  colour = NULL,
  table_stat_label = list(
    estimate = scales::label_number(accuracy = .001),
    conf.low = scales::label_number(accuracy = .01),
    conf.high = scales::label_number(accuracy = .01),
    std.error = scales::label_number(accuracy = .001),
    label = toupper
  ),
  table_header = c( "Coef.", "SE", "CI"),
  table_witdhs = c(2, 3), 
  shape_guide = FALSE,
  
  categorical_terms_pattern = "{level} (ref: {reference_level})",
)+ labs(title = "Argyrostoma")


c.vicinaIzsler <- fit$c.vicinaIzsler
ggcoef_table(
  c.vicinaIzsler, intercept = TRUE,add_reference_rows = FALSE,
  table_stat = c("estimate", "std.error", "ci"),
  ci_pattern = "{conf.low} to {conf.high}",
  significance = NULL, 
  colour = NULL,
  table_stat_label = list(
    estimate = scales::label_number(accuracy = .001),
    conf.low = scales::label_number(accuracy = .01),
    conf.high = scales::label_number(accuracy = .01),
    std.error = scales::label_number(accuracy = .001),
    label = toupper
  ),
  table_header = c( "Coef.", "SE", "CI"),
  table_witdhs = c(2, 3), 
  shape_guide = FALSE,
  
  categorical_terms_pattern = "{level} (ref: {reference_level})",
)+ labs(title = "vicinaIzsler")

domestica <- fit$domestica
ggcoef_table(
  domestica, intercept = TRUE,add_reference_rows = FALSE,
  table_stat = c("estimate", "std.error", "ci"),
  ci_pattern = "{conf.low} to {conf.high}",
  significance = NULL, 
  colour = NULL,
  table_stat_label = list(
    estimate = scales::label_number(accuracy = .001),
    conf.low = scales::label_number(accuracy = .01),
    conf.high = scales::label_number(accuracy = .01),
    std.error = scales::label_number(accuracy = .001),
    label = toupper
  ),
  table_header = c( "Coef.", "SE", "CI"),
  table_witdhs = c(2, 3), 
  shape_guide = FALSE,
  
  categorical_terms_pattern = "{level} (ref: {reference_level})",
)+ labs(title = "domestica")
 

sericata <- fit$sericata
ggcoef_table(
  sericata, intercept = TRUE,add_reference_rows = FALSE,
  table_stat = c("estimate", "std.error", "ci"),
  ci_pattern = "{conf.low} to {conf.high}",
  significance = NULL, 
  colour = NULL,
  table_stat_label = list(
    estimate = scales::label_number(accuracy = .001),
    conf.low = scales::label_number(accuracy = .01),
    conf.high = scales::label_number(accuracy = .01),
    std.error = scales::label_number(accuracy = .001),
    label = toupper
  ),
  table_header = c( "Coef.", "SE", "CI"),
  table_witdhs = c(2, 3), 
  shape_guide = FALSE,
  
  categorical_terms_pattern = "{level} (ref: {reference_level})",
)+ labs(title = "sericata")



illucens <- fit$illucens
ggcoef_table(
  illucens, intercept = TRUE,add_reference_rows = FALSE,
  table_stat = c("estimate", "std.error", "ci"),
  ci_pattern = "{conf.low} to {conf.high}",
  significance = NULL, 
  colour = NULL,
  table_stat_label = list(
    estimate = scales::label_number(accuracy = .001),
    conf.low = scales::label_number(accuracy = .01),
    conf.high = scales::label_number(accuracy = .01),
    std.error = scales::label_number(accuracy = .001),
    label = toupper
  ),
  table_header = c( "Coef.", "SE", "CI"),
  table_witdhs = c(2, 3), 
  shape_guide = FALSE,
  
  categorical_terms_pattern = "{level} (ref: {reference_level})",
)+ labs(title = "illucens")





# p1 <- plot_model(argy, type = "re",show.values = TRUE,  value.offset = .3,show.intercept = T) +
#    theme_bw()+labs(title = "")
# levels(p2$data$term) <-c("liver vs artificial", "pig muscle vs artificial", "timing", "(Intercept)")
# 
# pd1 <- tab_model(argy, show.p = F, show.icc = F,
#                  pred.labels = c( "Intercept", 
#                                   "liver vs artificial diet", 
#                                   "pig muscle vs aritificial diet", 
#                                   "timeing") )
 
# View(pd1) 
# 
# (p1|(gridExtra::tableGrob(pd1)))+plot_annotation(title = "argyrostoma")
 
 
#izsler
izsler <- fit$c.vicinaIzsler


pd1 <- describe_posterior(
  izsler,
  centrality = "median",
  test = c("p_direction"))

p1 <- plott$c.vicinaIzsler
p2 <- plot_model(izsler, type = "est",show.values = TRUE,  value.offset = .3,show.intercept = F) +
  theme_ipsum_rc()+labs(title = "")


(p1|(p2/gridExtra::tableGrob(pd1)))+plot_annotation(title = "izsler")




#domestica
dom <- fit$domestica

pd1 <- describe_posterior(
  dom,
  centrality = "median",
  test = c("p_direction")
)


p1 <- plott$domestica
p2 <- plot_model(dom, type = "est",show.values = TRUE,  value.offset = .3,show.intercept = F) +
  theme_ipsum_rc()+labs(title = "")


(p1|(p2/gridExtra::tableGrob(pd1)))+plot_annotation(title = "domestica")



#illucens
illuc <- fit$illucens

pd1 <- describe_posterior(
  illuc,
  centrality = "median",
  test = c("p_direction")
)

p1 <- plott$illucens
p2 <- plot_model(illuc, type = "est",show.values = TRUE,  value.offset = .3,show.intercept = F) +
  theme_ipsum_rc()+labs(title = "")


(p1|(p2/gridExtra::tableGrob(pd1)))+plot_annotation(title = "illucens")


#sericata
ser <- fit$sericata

pd1 <- describe_posterior(
  ser,
  centrality = "median",
  test = c("p_direction")
)
p1 <- plott$sericata
p2 <- plot_model(ser, type = "est",show.values = TRUE,  value.offset = .3,show.intercept = F) +
  theme_ipsum_rc()+labs(title = "")


(p1|(p2/gridExtra::tableGrob(pd1)))+plot_annotation(title = "sericata")



# 
# fit <- new_dati %>% 
#   mutate(replica = factor(replica), 
#          pweight = scale(pupal_weight)) %>% 
#   split(.$specie) %>%  
#   map(~ stan_lmer(pupal_weight*1000 ~   timing  + larval_diet + (1|replica), data = .,
#                    iter = 4000, adapt_delta = 0.9999, cores = 8))
# 
# saveRDS(fit, "models.RDS")
# 
# fit <- readRDS("models.RDS")



