library(brms)
library(readr)
library(tidyverse)
library(ggthemes)
library(bayesplot)
library(ggrepel)
library(janitor)
library(cowplot)

# load fitted models or re-fit them below by removing "#'s"
m2c_mt <- readRDS("models/m2c_mt.rds") # model 1
m2_sp <- readRDS('models/m2_sp.rds') # model 2
m2_gp <- readRDS('models/m2_gp.rds') # model 3
m2_gp2 <- readRDS('models/m2_gp2.rds') # model 4
gp1jul <- readRDS("models/gp1jul.rds") # model 5 - july data only
gp2jul <- readRDS("models/gp2jul.rds") # model 6 - july data only
spjul <- readRDS("models/spjul.rds") # model 7 - july data only



# Refit model 1 with different sized data sets ----------------------------------

all_data <- m2_sp$data
growth_data <- m2_gp2$data
morph_data <- m2_gp$data

# m2c_mt_alldata <- update(m2c_mt, newdata = all_data)
# m2c_mt_growthdata <- update(m2c_mt, newdata = growth_data)
# m2c_mt_morphdata <- update(m2c_mt, newdata = morph_data)
# 
# saveRDS(m2c_mt_alldata, file = "models/m2c_mt_alldata.rds")
# saveRDS(m2c_mt_growthdata, file = "models/m2c_mt_growthdata.rds")
# saveRDS(m2c_mt_morphdata, file = "models/m2c_mt_morphdata.rds")

m2c_mt_alldata <- readRDS("models/m2c_mt_alldata.rds")
m2c_mt_growthdata <- readRDS("models/m2c_mt_growthdata.rds")
m2c_mt_morphdata <- readRDS("models/m2c_mt_morphdata.rds")


mod1_post <- posterior_samples(m2c_mt) %>% clean_names() %>% 
  mutate(may_hits = b_hits + b_hits_month_may,
         july_hits = b_hits,
         sep_hits = b_hits + b_hits_month_september) %>% 
  select(may_hits, july_hits, sep_hits) %>% 
  mutate(n = nrow(m2c_mt$data),
         model = "Original Model 1")

mod1sp_post <- posterior_samples(m2c_mt_alldata) %>% clean_names() %>% 
  mutate(may_hits = b_hits + b_hits_month_may,
         july_hits = b_hits,
         sep_hits = b_hits + b_hits_month_september) %>% 
  select(may_hits, july_hits, sep_hits) %>% 
  mutate(n = nrow(spjul$data),
         model = "Model 1 with all data")

mod1gp_post <-  posterior_samples(m2c_mt_growthdata) %>% clean_names() %>% 
  mutate(may_hits = b_hits + b_hits_month_may,
         july_hits = b_hits,
         sep_hits = b_hits + b_hits_month_september) %>% 
  select(may_hits, july_hits, sep_hits) %>% 
  mutate(n = nrow(growth_data),
         model = "Model 1 with growth data")

mod1morph_post <-  posterior_samples(m2c_mt_morphdata) %>% clean_names() %>% 
  mutate(may_hits = b_hits + b_hits_month_may,
         july_hits = b_hits,
         sep_hits = b_hits + b_hits_month_september) %>% 
  select(may_hits, july_hits, sep_hits) %>% 
  mutate(n = nrow(morph_data),
         model = "Model 1 with morph data")


all_sims <- bind_rows(mod1_post, 
                      mod1sp_post,
                      mod1gp_post,
                      mod1morph_post)


all_sims_summary <- all_sims %>% 
  pivot_longer(cols = c(may_hits, july_hits, sep_hits)) %>% 
  separate(name, c("month", "measure")) %>% 
  group_by(model, month, n) %>% 
  summarize(mean = mean(value),
            low = quantile(value, prob = 0.025),
            high = quantile(value, prob = 0.975))


all_sims_summary %>% 
  ggplot(aes(x = month, y = mean, ymin = low, ymax = high,
             color = model)) + 
  geom_pointrange(position = position_dodge(width = 0.5)) 

