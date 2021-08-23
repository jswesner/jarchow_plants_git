library(brms)
library(readr)
library(tidyverse)
library(ggthemes)
library(bayesplot)
library(ggrepel)
library(janitor)
library(cowplot)

# Load Data ---------------------------------------------------------------

nosp_nogp <- read_csv("data/nosp_nogp.csv") # for Model 1
species <- read_csv("data/species.csv") # for Model 2 & 7
group <- read_csv("data/group.csv")  # for Model 3 & 5
group2 <- read_csv("data/group2.csv")  # for Model 4 & 6 


# Prior prediction --------------------------------------------------------
N = 100
prior_pred <- tibble(Intercept = rnorm(N, 0, 50),
                     b_hits = rnorm(N, 0, 20),
                     b_Month = rnorm(N, 0, 20),
                     Area = rcauchy(N, 0, 1),
                     Transect = rcauchy(N, 0, 1),
                     iter = 1:N) %>% 
  expand_grid(hits = m2c_mt$data$hits) %>% 
  mutate(hits_fit = Intercept + b_hits*hits)

prior_pred %>% 
  ggplot(aes(x = hits, y = hits_fit, group = iter)) +
  geom_line()


# Fit Models ------------------------------------------------

# load fitted models or re-fit them below by removing "#'s"
m2c_mt <- readRDS("models/m2c_mt.rds") # model 1
m2_sp <- readRDS('models/m2_sp.rds') # model 2
m2_gp <- readRDS('models/m2_gp.rds') # model 3
m2_gp2 <- readRDS('models/m2_gp2.rds') # model 4
gp1jul <- readRDS("models/gp1jul.rds") # model 5 - july data only
gp2jul <- readRDS("models/gp2jul.rds") # model 6 - july data only
spjul <- readRDS("models/spjul.rds") # model 7 - july data only

# m2c_mt <- brm(Biomass~hits*Month+(1|Area)+(1|Transect),data=nosp_nogp,family=gaussian(),
#               prior=c(prior(normal(0,20),class="b"),
#                       prior(normal(0,50),class="Intercept"),
#                       prior(cauchy(0,1),class="sd")),
#             chains=4,iter=2000) 
# 
# saveRDS(m2c_mt, file = "models/m2c_mt.rds")


# m2_sp <- brm(Biomass~hits*Month+(1|Species)+(1|Area)+(1|Transect),data=species,family=gaussian(),
#            prior=c(prior(normal(0,20),class="b"),
#                    prior(normal(0,50),class="Intercept"),
#                    prior(cauchy(0,1),class="sd")),
#            chains=4)
# 
# saveRDS(m2_sp, file = "models/m2_sp.rds")


# m2_gp <- brm(Biomass~hits*Month+(1|Group)+(1|Area)+(1|Transect),data=group,family=gaussian(),
#            prior=c(prior(normal(0,20),class="b"),
#                    prior(normal(0,50),class="Intercept"),
#                    prior(cauchy(0,1),class="sd")),
#            chains=4)
# 
# saveRDS(m2_gp, file = "models/m2_gp.rds")

# m2_gp2 <- brm(Biomass~hits*Month+(1|Group)+(1|Area)+(1|Transect),data=group2,family=gaussian(),
#            prior=c(prior(normal(0,20),class="b"),
#                    prior(normal(0,50),class="Intercept"),
#                    prior(cauchy(0,1),class="sd")),
#            chains=4)

# saveRDS(m2_gp2, file = "models/m2_gp2.rds")



# gp1jul<-brm(Biomass~hits*Group+(1|Area)+(1|Transect),data=subset(group,Month=="July"),family=gaussian(),
#             prior=c(prior(normal(0,20),class="b"),
#                     prior(normal(0,50),class="Intercept"),
#                     prior(cauchy(0,1),class="sd")),
#             chains=4,iter=2000) 
# 
# saveRDS(gp1jul, file = "models/gp1jul.rds")
# 
# gp2jul<-brm(Biomass~hits*Group+(1|Area)+(1|Transect),data=subset(group2,Month=="July"),family=gaussian(),
#             prior=c(prior(normal(0,20),class="b"),
#                     prior(normal(0,50),class="Intercept"),
#                     prior(cauchy(0,1),class="sd")),
#             chains=4,iter=2000) 
# 
# saveRDS(gp2jul, file = "models/gp2jul.rds")
# 
# spjul<-brm(Biomass~hits*Species+(1|Area)+(1|Transect),data=subset(species,Month=="July"),family=gaussian(),
#            prior=c(prior(normal(0,20),class="b"),
#                    prior(normal(0,50),class="Intercept"),
#                    prior(cauchy(0,1),class="sd")),
#            chains=4,iter=2000) 
# 
# saveRDS(spjul, file = "models/spjul.rds")


# Posteriors --------------------------------------------------------------
all_posts <- readRDS(file = "posteriors/all_posts.rds")

# m2c_mt_posts <- posterior_samples(m2c_mt) %>% mutate(model = "Model 1", iter = 1:nrow(.))
# m2_sp_posts <- posterior_samples(m2_sp) %>% mutate(model = "Model 2", iter = 1:nrow(.))
# m2_gp_posts <- posterior_samples(m2_gp) %>% mutate(model = "Model 3", iter = 1:nrow(.))
# m2_gp2_posts <- posterior_samples(m2_gp2) %>% mutate(model = "Model 4", iter = 1:nrow(.))
# gp1jul_posts <- posterior_samples(gp1jul) %>% mutate(model = "Model 5", iter = 1:nrow(.))
# gp2jul_posts <- posterior_samples(gp2jul) %>% mutate(model = "Model 6", iter = 1:nrow(.))
# spjul_posts <- posterior_samples(spjul) %>% mutate(model = "Model 7", iter = 1:nrow(.))
# 
# all_posts <- bind_rows(m2c_mt_posts,
#                        m2_sp_posts,
#                        m2_gp_posts,
#                        m2_gp2_posts,
#                        gp1jul_posts,
#                        gp2jul_posts,
#                        spjul_posts)

# saveRDS(all_posts %>% clean_names(), file = "posteriors/all_posts.rds")


# Check Models ------------------------------------------------------------

pp_check(m2c_mt)
pp_check(m2_sp)
pp_check(m2_gp)
pp_check(m2_gp2)
pp_check(gp1jul)
pp_check(gp2jul)
pp_check(spjul)


# R-squared ---------------------------------------------------------------

partial_r2_bymonth <- function(model) {
  data <- model$data %>% 
  as_tibble() %>%
  group_split(Month)
  
  m2_r2 <- lapply(data, function(x){bayes_R2(model, newdata = x)})
  
  m2_r2 <- do.call(rbind.data.frame, m2_r2) %>% as_tibble() %>% 
  mutate(Month = sort(unique(model$data$Month)))
}

m1_r2 <- partial_r2_bymonth(model = m2c_mt) %>% mutate(model = "Model 1")
m2_r2 <- partial_r2_bymonth(model = m2_sp) %>% mutate(model = "Model 2")
m3_r2 <- partial_r2_bymonth(model = m2_gp) %>% mutate(model = "Model 3")
m4_r2 <- partial_r2_bymonth(model = m2_gp2) %>% mutate(model = "Model 4")

partial_r2_bygroup <- function(model) {
  data <- model$data %>% 
    as_tibble() %>%
    group_split(Group)
  
  m2_r2 <- lapply(data, function(x){bayes_R2(model, newdata = x)})
  
  m2_r2 <- do.call(rbind.data.frame, m2_r2) %>% as_tibble() %>% 
    mutate(Group = sort(unique(model$data$Group)))
}

m5_r2 <- partial_r2_bygroup(model = gp1jul) %>% mutate(model = "Model 5")
m6_r2 <- partial_r2_bygroup(model = gp2jul) %>% mutate(model = "Model 6")


partial_r2_byspecies <- function(model) {
  data <- model$data %>% 
    as_tibble() %>%
    group_split(Species)
  
  m2_r2 <- lapply(data, function(x){bayes_R2(model, newdata = x)})
  
  m2_r2 <- do.call(rbind.data.frame, m2_r2) %>% as_tibble() %>% 
    mutate(Species = sort(unique(model$data$Species)))
}

m7_r2 <- partial_r2_byspecies(model = spjul) %>% mutate(model = "Model 7")


all_partial_r2 <- bind_rows(m1_r2,
                            m2_r2,
                            m3_r2,
                            m4_r2,
                            m5_r2,
                            m6_r2,
                            m7_r2) %>% 
  mutate_if(is.numeric, ~round(., 2))

write.csv(all_partial_r2, file = "summaries/all_partial_r2.csv", row.names = F)

# Plots --------------------------------------------------------
# plot data
frequencies <- species %>% 
  group_by(species) %>% 
  summarize(species_total_hits = sum(Hits),
            species_total_biomass = sum(Biomass)) %>% 
  ungroup() %>% 
  mutate(total_hits = sum(species_total_hits),
         total_biomass = sum(species_total_biomass),
         proportion_hits = species_total_hits/total_hits,
         proportion_biomass = species_total_biomass/total_biomass) %>% 
  arrange(-proportion_biomass) %>% 
  select(-total_hits, -total_biomass) %>% 
  mutate(proportion_hits = round(proportion_hits, 2),
         proportion_biomass = round(proportion_biomass, 2)) 

write.csv(frequencies, file = "summaries/frequencies.csv", row.names = F)

raw_data <- bind_rows(nosp_nogp, 
                      species,
                      group,
                      group2) %>% 
  mutate(Month = fct_relevel(Month, "May"))

# conditional_effects
marg1<-marginal_effects(m2c_mt,effects="hits:Month",robust=FALSE,method="fitted")
marg2<-marginal_effects(m2_sp,effects="hits:Month",robust=FALSE,method="fitted")
marg3<-marginal_effects(m2_gp,effects="hits:Month",robust=FALSE,method="fitted")
marg4<-marginal_effects(m2_gp2,effects="hits:Month",robust=FALSE,method="fitted")

marg1<-as.data.frame(marg1$`hits:Month`)
marg2<-as.data.frame(marg2$`hits:Month`)
marg3<-as.data.frame(marg3$`hits:Month`)
marg4<-as.data.frame(marg4$`hits:Month`)

marg1$model<-paste0("Model 1",marg1$model)
marg2$model<-paste0("Model 2",marg2$model)
marg3$model<-paste0("Model 3",marg3$model)
marg4$model<-paste0("Model 4",marg4$model)

d_all <- bind_rows(marg1,marg2,marg3,marg4) %>% 
  mutate(Month = fct_relevel(Month, "May"))

saveRDS(d_all, "posteriors/d_all.rds")

# trim posteriors to only max data points
max_data <- raw_data %>% group_by(Month, model) %>% filter(hits == max(hits)) %>% distinct(hits, Month, model) %>% 
  rename(max_hits = hits)

d_all_trim <- d_all %>% left_join(max_data) %>% 
  mutate(estimate__ = case_when(hits <= max_hits ~ estimate__,
                                TRUE ~ -100),
         lower__ = case_when(hits <= max_hits ~ lower__,
                             TRUE ~ -100),
         upper__ = case_when(hits <= max_hits ~ upper__,
                             TRUE ~ -100)) %>% 
  filter(estimate__ > -100)

letters_plot <- d_all_trim %>% distinct(model, Month) %>% 
  arrange(model, Month) %>% 
  mutate(letters = paste0(letters[1:12], ")"),
         x = 6, y = 900)

plot_four_models <- combine567 <- sp_trim %>% 
  filter(n >= 20) %>% 
  rename(group_label = Species) %>% 
  mutate(model = "Species") %>% 
  bind_rows(group_trim %>% mutate(model = "Growth Form")) %>% 
  bind_rows(morph_trim %>% mutate(model = "Morphology")) %>% 
  mutate(model = fct_relevel(model, "Species", "Morphology"))

letters_combine <- tibble(label = c("c)", "b)", "a)"),
                          model = c("Growth Form", "Morphology", "Species")) %>% 
  mutate(hits = 5, 
         estimate__ = 600)%>% 
  mutate(model = fct_relevel(model, "Species", "Morphology"))




ggsave(plot_four_models, file = "plots/plot_four_models.jpg", width = 6, height = 7, dpi = 500)
saveRDS(plot_four_models, file = "plots/plot_four_models.rds")


# compare slopes
slopes <- all_posts %>% 
  group_by(model) %>% 
  mutate(July_hits = b_hits,
         May_hits = b_hits + b_hits_month_may,
         September_hits = b_hits + b_hits_month_september) %>% 
  select(July_hits, May_hits, September_hits,
         iter, model) %>% 
  pivot_longer(c(-iter, -model), values_to = "slope") %>% 
  separate(name, c("month", "delete")) %>% 
  select(-delete) %>% 
  mutate(month = fct_relevel(month, "May"),
         model_number = as.numeric(str_sub(model, -1)))


slope_plot <- slopes %>% 
  filter(model_number < 5) %>% 
  group_by(model, month) %>% 
  summarize(mean = mean(slope),
            lower = quantile(slope, prob = 0.025),
            upper = quantile(slope, prob = 0.975)) %>% 
  ggplot(aes(x = month, color = model, y = mean, ymin = lower, ymax = upper)) +
  geom_pointrange(position = position_dodge(width = 0.4),
                  size = 1) +
  scale_color_grey() +
  labs(y = "Slope (Mean +/- 95% CrI)",
       color = "",
       x = "Month") +
  theme_bw() +
  theme(text = element_text(size = 14))

ggsave(slope_plot, file = "plots/slope_plot.jpg", dpi = 500, width = 6, height = 4)
saveRDS(slope_plot, file = "plots/slope_plot.rds")


# plot by species and groups
morph_plot <- plot(conditional_effects(gp1jul, effects = "hits:Group"), points = T)
group_plot <- plot(conditional_effects(gp2jul, effects = "hits:Group"), points = T)
species_plot <- plot(conditional_effects(spjul, effects = "hits:Species"), points = T)


max_sp <- spjul$data %>% as_tibble() %>%
  mutate(model = "Model 7") %>% 
  group_by(Species, model) %>% 
  filter(hits == max(hits)) %>% distinct(hits, Species, model) %>% 
  rename(max_hits = hits)

letters_plot <- d_all_trim %>% distinct(model, Month) %>% 
  arrange(model, Month) %>% 
  mutate(letters = paste0(letters[1:12], ")"),
         x = 6, y = 900)

sp_tally <- spjul$data %>% group_by(Species) %>% 
  tally()

sp_rank <- spjul$data %>% 
  group_by(Species) %>% 
  mutate(max = (max(Biomass) - min(Biomass))/max(Biomass)) %>% 
  left_join(sp_tally) %>% 
  distinct(Species, max, n) %>%
  ungroup() %>% 
  mutate(order = 1:nrow(.),
         letter = letters[order],
         label = paste0(letter, ") ", Species))
  

sp_trim <- species_plot$`hits:Species`$data %>% 
  as_tibble() %>% 
  left_join(max_sp) %>% 
  mutate(estimate__ = case_when(hits <= max_hits ~ estimate__,
                                TRUE ~ -100),
         lower__ = case_when(hits <= max_hits ~ lower__,
                             TRUE ~ -100),
         upper__ = case_when(hits <= max_hits ~ upper__,
                             TRUE ~ -100)) %>% 
  filter(estimate__ > -100) %>% 
  left_join(sp_rank)

raw_sp <- spjul$data %>% as_tibble() %>% 
  group_by(Species) %>% 
  left_join(sp_rank)



species_figure <- sp_trim %>% 
  filter(n > 20) %>% 
  ggplot(aes(x = hits, y = estimate__, fill = Species)) + 
  geom_line() + 
  geom_ribbon(aes(ymax = lower__, ymin = upper__), alpha = 0.2) + 
  geom_point(data = raw_sp %>% filter(n>20), 
             aes(color = Species, x = hits, y = Biomass), 
             size = 1, shape = 20) +
  geom_text_repel(data = sp_trim %>% group_by(Species) %>% filter(hits == max(hits)),
                  aes(label = Species),
                  nudge_y = 0,
                  nudge_x = 15, size = 3) +
  xlim(0, 300) +
  guides(color = F, fill = F) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(y = expression(paste("Aboveground Biomass (g/",m^2, ")")),
       x = "Hits")


max_morph <- gp1jul$data %>% as_tibble() %>%
  group_by(Group) %>% 
  filter(hits == max(hits)) %>% distinct(hits, Group) %>% 
  rename(max_hits = hits)

morph_trim <- morph_plot$`hits:Group`$data %>% 
  left_join(max_morph) %>% 
  mutate(estimate__ = case_when(hits <= max_hits ~ estimate__,
                                TRUE ~ -100),
         lower__ = case_when(hits <= max_hits ~ lower__,
                             TRUE ~ -100),
         upper__ = case_when(hits <= max_hits ~ upper__,
                             TRUE ~ -100)) %>% 
  filter(estimate__ > -100) %>% 
  mutate(group_label = case_when(Group == "3" ~ "Tall Forbs",
                                 Group == "4" ~ "Wide-leaved Grass",
                                 Group == "2" ~ "Short Forbs",
                                 Group == "1" ~ "Decumbant/Rosette",
                                 TRUE ~ "Narrow-leaved Grass"))


morph_figure <- morph_trim %>% 
  ggplot(aes(x = hits, fill = Group)) + 
  geom_line(aes(y = estimate__)) + 
  geom_ribbon(aes(ymax = lower__, ymin = upper__), alpha = 0.2) + 
  geom_point(data = gp1jul$data, 
             aes(color = Group, x = hits, y = Biomass), 
             size = 1, shape = 20) +
  geom_text_repel(data = morph_trim %>% group_by(Group) %>% filter(hits == max(hits)),
                  aes(label = group_label, y = estimate__),
                  nudge_x = 15, size = 3) +
  xlim(0, 300) +
  guides(color = F, fill = F) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(y = expression(paste("Aboveground Biomass (g/",m^2, ")")),
       x = "Hits")


max_group <- gp2jul$data %>% as_tibble() %>%
  group_by(Group) %>% 
  filter(hits == max(hits)) %>% distinct(hits, Group) %>% 
  rename(max_hits = hits)

group_trim <- group_plot$`hits:Group`$data %>% 
  left_join(max_group) %>% 
  mutate(estimate__ = case_when(hits <= max_hits ~ estimate__,
                                TRUE ~ -100),
         lower__ = case_when(hits <= max_hits ~ lower__,
                             TRUE ~ -100),
         upper__ = case_when(hits <= max_hits ~ upper__,
                             TRUE ~ -100)) %>% 
  filter(estimate__ > -100) %>% 
  mutate(group_label = case_when(Group == "1" ~ "Forbs", 
                                 TRUE ~ "Graminoids"))


group_figure <- group_trim %>% 
  ggplot(aes(x = hits, fill = Group)) + 
  geom_line(aes(y = estimate__)) + 
  geom_ribbon(aes(ymax = lower__, ymin = upper__), alpha = 0.2) + 
  geom_point(data = gp2jul$data, 
             aes(color = Group, x = hits, y = Biomass), 
             size = 1, shape = 20) +
  geom_text_repel(data = group_trim %>% group_by(Group) %>% filter(hits == max(hits)),
                  aes(label = group_label, y = estimate__),
                  nudge_x = 15, size = 3) +
  xlim(0, 300) +
  guides(color = F, fill = F) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(text = element_text(size = 12)) +
  labs(y = expression(paste("Aboveground Biomass (g/",m^2, ")")),
       x = "Hits")



combine567 <- sp_trim %>% 
  filter(n >= 20) %>% 
  rename(group_label = Species) %>% 
  mutate(model = "Species") %>% 
  bind_rows(group_trim %>% mutate(model = "Growth Form")) %>% 
  bind_rows(morph_trim %>% mutate(model = "Morphology")) %>% 
  mutate(model = fct_relevel(model, "Species", "Morphology"))

letters_combine <- tibble(label = c("c)", "b)", "a)"),
                          model = c("Growth Form", "Morphology", "Species")) %>% 
  mutate(hits = 5, 
         estimate__ = 615)%>% 
  mutate(model = fct_relevel(model, "Species", "Morphology"))



three_panel <- combine567 %>% 
  ggplot(aes(x = hits, y = estimate__)) +
  geom_line(aes(group = group_label)) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = group_label), alpha = 0.4) +
  geom_text_repel(data = combine567 %>% group_by(group_label) %>% filter(hits == max(hits)),aes(label = group_label),
                  nudge_y = 1,
                  nudge_x = 19, size = 3) +
  geom_text(data = letters_combine, aes(label = label)) +
  guides(color = F,
         fill = F) +
  facet_grid(model ~ .) +
  scale_fill_viridis_d() +
  labs(y = expression(paste("Aboveground Biomass (g/",m^2, ")")),
       x = "Hits") +
  theme_bw() +
  theme(text = element_text(size = 14)) +
  xlim(0, 380)

ggsave(three_panel, file = "plots/three_panel.jpg", width = 4, height = 9, dpi = 500)
saveRDS(three_panel, file = "plots/three_panel.rds")
