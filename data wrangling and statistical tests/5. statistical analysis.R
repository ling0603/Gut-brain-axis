#packages used
library(glmmTMB) #model construction 
library(DHARMa) #model check
library(performance) #model performance
library(emmeans) #predict values according to the model
library(ggplot2)#visualization
library(ggh4x)#visualization
library(dplyr)#data process
library(ggtext) #visualization - get different colour x-axis
library(ggeffects)#visualization

###############################################################
#test the the effect of interaction types on interaction frames

#input data 
input_dir <- "../final_data"

files <- list.files(
  input_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

data <- purrr::map_dfr(files, read_csv, show_col_types = FALSE)

day0_data <- data %>%
  filter(grepl("0507|0602", source_video)) %>%
  filter(!grepl("Q", interaction_type))%>%
  filter(interaction_type != "AMBIGUOUS")

#descriptive stats
day0_data %>%
  count()
day0_data %>%
  count(interaction_type)
day0_data %>%
  count(colony_treatment)
day0_data %>%
  count(interaction_type, colony_treatment) %>%
  arrange(interaction_type, colony_treatment)
#construct model
m_nb <- glmmTMB(
  interaction_frames ~ interaction_type 
  + colony_treatment 
  +(1|colony)
  +offset(log(cooccurrence_frames)),
  family = nbinom2,
  data = day0_data
)

#check model
set.seed(42)
res_nb <- simulateResiduals(
  fittedModel = m_nb,
  n = 1000
)
plot(res_nb)
testUniformity(res_nb)
testDispersion(res_nb)
testOutliers(res_nb)
testZeroInflation(res_nb)

# get model performance 
r2(m_nb)

#post hoc test
emm_interaction <- emmeans(
  m_nb,
  ~ interaction_type,
  type = "response",
  offset = log(1000)
)

emm_interaction
pairs(
  emm_interaction,
  adjust = "tukey"
)

#visualization 
# Estimated interaction frames per 1,000 co-occurrence frames
emm_nb <- emmeans(
  m_nb,
  ~ interaction_type * colony_treatment,
  type = "response",
  offset = log(1000)
)
emm_nb_df <- as.data.frame(emm_nb)
emm_nb_df
names(emm_nb_df)
#visualization grouped by  colony treatment
plot_data <- emm_nb_df %>%
  mutate(
    colony_treatment = factor(
      colony_treatment,
      levels = c("Antibiotic", "Sucrose")
    ),
    interaction_type = factor(
      interaction_type,
      levels = c("HH","HMC", "MCMC", "HMD","MCMD", "MDMD")
    ),
    
    # Nested axis: interaction type above, colony treatment below
    x_nested = interaction(
      colony_treatment,
      interaction_type,
      sep = "."
    )
  )
#get the order
emm_nb_df$interaction_type <- factor(
  emm_nb_df$interaction_type,
  levels = c("HH", "HMC", "MCMC", "MCMD","HMD",  "MDMD")
)
#get colour difference 
label_cols <- c(
  HH   = "<span style='color:black;'>HH</span>",
  HMC  = "<span style='color:black;'>H</span><span style='color:#E69F00;'>MC</span>",
  MCMC = "<span style='color:#E69F00;'>MCMC</span>",
  HMD  = "<span style='color:black;'>H</span><span style='color:#009E73;'>MD</span>",
  MCMD = "<span style='color:#E69F00;'>MC</span><span style='color:#009E73;'>MD</span>",
  MDMD = "<span style='color:#009E73;'>MDMD</span>"
)

ggplot(
  emm_nb_df,
  aes(
    x = interaction_type,
    y = response
  )
) +
  geom_point(
    size = 3,
    colour = "black"
  ) +
  geom_errorbar(
    aes(
      ymin = asymp.LCL,
      ymax = asymp.UCL
    ),
    width = 0.15,
    linewidth = 0.6,
    colour = "grey30"
  ) +
  facet_grid(
    . ~ colony_treatment,
    scales = "free_x",
    space = "free_x",
    switch = "x"
  ) +
  labs(
    x = NULL,
    y = "Predicted interaction frames per 1,000 co-occurrence frames"
  ) +
  scale_x_discrete(
    labels = label_cols
  ) +
  theme_classic(base_size = 13) +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.x.bottom = element_text(
      size = 12,
      face = "bold"
    ),
    panel.spacing.x = unit(1.2, "lines"),
    axis.text.x = ggtext::element_markdown(
      size = 11,
      face = "bold"
    )
  )

###############################################################
#test the the temporal change under different interaction types, newcomer ratio, and hive treatment
#input data
all_data <- all_interactions %>%
  filter(interaction_type %in% c("HH","HMC", "HMD","MCMD","MDMD","MCMC")) %>%
  mutate(
    interaction_type = factor(
      interaction_type,
      levels = c("HH", "HMD","HMC","MCMD","MCMC","MDMD")
    ),
    pair_id = paste(
      colony,
      pmin(tag_1, tag_2),
      pmax(tag_1, tag_2),
      sep = "_"
    ),
    colony = factor(colony),
    colony_treatment = factor(colony_treatment),
    day = as.numeric(day)
  ) %>%
  filter(
    cooccurrence_frames > 0,
    interaction_frames >= 0
  )

#getting the data description 
descriptive_stats <- all_data %>%
  filter(interaction_type %in% c("HH", "HMD", "HMC", "MDMC","MDMD","MCMC")) %>%
  group_by(
    interaction_type,
    newcomer_ratio,
    colony_treatment
  ) %>%
  summarise(
    N = n()
  )
descriptive_stats

#construct model
m_all_3way <- glmmTMB(
  interaction_frames ~
    day* interaction_type* newcomer_ratio +day*newcomer_ratio* colony_treatment+
    offset(log(cooccurrence_frames)) +
    (1 | colony),
  family = nbinom2,
  data = all_data
)
summary(m_all_3way)

#check model
plot(m_all_3way)
testUniformity(m_all_3way)
testDispersion(m_all_3way)
testOutliers(m_all_3way)
testZeroInflation(m_all_3way)

#get model performance
r2(m_all_3way) 

#post hoc test
#1. compare between different interaction types
all_slopes <- emtrends(
  m_all_3way,
  ~ interaction_type | newcomer_ratio *colony_treatment,
  var = "day"
)
pairs(all_slopes, adjust = "tukey")

#2.compare between different newcomer ratios
all_ratio_slopes <- emtrends(
  m_all_3way,
  ~ newcomer_ratio | interaction_type*colony_treatment,
  var = "day"
)
pairs(all_ratio_slopes, adjust = "tukey")

#comparing the HMD day 0 intercept between different newcomer ratio 
HMD_day0 <- emmeans(
  m_all_3way,
  ~ newcomer_ratio | colony_treatment,
  at = list(
    day = 0,
    interaction_type = "HMD",
    cooccurrence_frames = 1000
  ),
  type = "response"
)

pairs(HMD_day0, adjust = "tukey")

#comparing HMD and HMC day 0 intercept
emm_day0 <- emmeans(
  m_all_3way,
  ~ interaction_type | newcomer_ratio * colony_treatment,
  at = list(
    day = 0,
    cooccurrence_frames = 1000
  )
)

contrast(
  emm_day0,
  method = list(
    "HMD / HMC" = c(0, 1, -1, 0, 0, 0)
  ),
  by = c("newcomer_ratio", "colony_treatment")
) |>
  summary(type = "response")

#visualization
#include hive treatment and newcomer ratio, and the offset factor
pred_all <- ggpredict(
  m_all_3way,
  terms = c(
    "day [0:10]",
    "interaction_type",
    "colony_treatment",
    "newcomer_ratio"
  ),
  condition = c(
    cooccurrence_frames = 1000
  )
)

#filter only hh, hmd, hmc, and catagorise the panels
pred_plot <- as.data.frame(pred_all) %>%
  dplyr::filter(
    group %in% c("HH", "HMD", "HMC")
  ) %>%
  dplyr::mutate(
    
    # Hive treatment
    facet = dplyr::recode(
      as.character(facet),
      "Antibiotic" = "Antibiotic-treated hive",
      "Sucrose" = "Sucrose-treated hive"
    ),
    
    # Newcomer ratio
    panel = dplyr::recode(
      as.character(panel),
      "MC3_MD7" = "MC:MD = 3:7",
      "MC7_MD3" = "MC:MD = 7:3"
    )
  )

#adjust colour 
# Colourblind-friendly palette (Okabe-Ito)
cb_cols <- c(
  HH  = "grey70",  # Grey
  HMD = "#009E73",  # Bluish green
  HMC = "#E69F00"   # Orange
)

# Matching line types
cb_lty <- c(
  HH  = "solid",
  HMD = "dashed",
  HMC = "dotdash"
)

#plot
ggplot(
  pred_plot,
  aes(
    x = x,
    y = predicted,
    colour = group,
    fill = group,
    linetype = group,
    group = group       
  )
) +
  geom_line(
    linewidth = 1.2
  ) +
  
  facet_grid(
    panel ~ facet
  ) +
  
  scale_colour_manual(
    values = cb_cols,
    breaks = c("HH", "HMD", "HMC"),
    name = "Interaction type"
  ) +
  
  scale_fill_manual(
    values = cb_cols,
    breaks = c("HH", "HMD", "HMC"),
    guide = "none"
  ) +
  
  scale_linetype_manual(
    values = cb_lty,
    breaks = c("HH", "HMD", "HMC"),
    name = "Interaction type"
  ) +
  
  scale_x_continuous(
    breaks = 0:10
  ) +
  
  labs(
    x = "Experimental day",
    y = "Predicted interaction frames\nper 1,000 co-occurrence frames"
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    legend.position = "right",
    
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 11),
    
    strip.background = element_rect(
      fill = "grey95",
      colour = "black"
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    ),
    
    panel.spacing = unit(0.8, "lines")
  )
