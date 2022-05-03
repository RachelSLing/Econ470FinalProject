# Load necessary packages and Data
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales,
               ggbeeswarm, extrafont, MatchIt, ggrepel, cobalt, stargazer, lfe, dotwhisker, here, fixest, modelsummary)
insurance <- readRDS("/Users/rachelling/Downloads/insurance.rds")

acs_medicaid <- readRDS("/Users/rachelling/Downloads/acs_medicaid.rds")
acs_medicaid$date_adopted <- NULL
insurance_data_final <- left_join(acs_medicaid, kff.final, by="State")

# Summarize the data 
## Calculate share for each group
ins.dat <- insurance_data_final %>%
  mutate(perc_private = (ins_employer + ins_direct)/adult_pop,
         perc_public = (ins_medicare + ins_medicaid)/adult_pop,
         perc_ins = (adult_pop - uninsured)/adult_pop,
         perc_unins = uninsured/adult_pop,
         perc_employer = ins_employer/adult_pop,
         perc_medicaid = ins_medicaid/adult_pop,
         perc_medicare = ins_medicare/adult_pop,
         perc_direct = ins_direct/adult_pop)
## Share of Insured (not pictured)
percent_insured <- ins.dat %>%
  select(perc_ins, year) %>%
  group_by(year) %>%
  summarize(mean1=mean(perc_ins))

perc_ins_plot <- ggplot(percent_insured, aes(x=year, y=mean1)) +
  geom_line() + ylab("Share") + ggtitle("Share of Total Insured Individuals over Time") +
  theme(plot.title = element_text(hjust = 0.5))

## Share of Uninsured 

percent_uninsured <- ins.dat %>%
  select(perc_unins, year) %>%
  group_by(year) %>%
  summarize(mean2=mean(perc_unins))

perc_unins_plot <- ggplot(percent_uninsured, aes(x=year, y=mean2)) +
  geom_line() + ylab("Share")
## Share of Private Insurance vs Medicaid 
pc_med <- ins.dat %>%
  select(perc_private, perc_medicaid, year) %>%
  group_by(year) %>%
  summarize(priv_mean=mean(perc_private), medic_mean=mean(perc_medicaid))

pc_med_long <- pc_med %>% 
  pivot_longer(
    cols = priv_mean:medic_mean, names_to = "Type",
    values_to = "Share"
  )

pc.med.plot <- ggplot(pc_med_long, aes(x=year, y=Share, group=Type, linetype=Type)) + geom_line() + theme_classic() 


# Estimate the ATEs (Difference-in-Differences)
## Calculating pre/post period, ATT
dd.means <- ins.dat %>% 
  group_by(expand_ever, year) %>% 
  summarize(means = mean(perc_private)) %>%
  filter(year=="2012" | year=="2015") %>%
  na.omit
## Estimate of effect of Medicaid Expansion on (comparing pre/post periods)
insurance_2014 <- ins.dat %>% 
  mutate(post = (year>=2014), treat=post*expand_ever) %>%
  filter(is.na(expand_year) | expand_year==2014) %>%
  group_by(expand_ever, year) %>% 
  mutate(mean=mean(perc_private)) 

# Graph 
ins.plot.dat <- ins.dat %>% filter(!is.na(expand_ever)) %>%
  group_by(expand_ever, year) %>% summarize(mean=mean(perc_private))

pla.plot <- ggplot(data=ins.plot.dat, aes(x=year,y=mean,group=expand_ever,linetype=expand_ever)) + 
  geom_line() + geom_point() + theme_bw() +
  geom_vline(xintercept=2013.5, color="red") +
  geom_text(data = ins.plot.dat %>% filter(year == 2016), 
            aes(label = c("Non-expansion","Expansion"),
                x = year + 1,
                y = mean)) +
  guides(linetype=FALSE) +
  labs(
    x="Year",
    y="Fraction Privately Insured"
  )
# DD vs FE 
dd.est <- lm(perc_private ~ post + expand_ever + post*expand_ever, data=insurance_2014)
summary(dd.est)
fe.est <- summary(felm(perc_private ~ treat | factor(State) + factor(year), data=insurance_2014))

save.image("analysis.RData")

