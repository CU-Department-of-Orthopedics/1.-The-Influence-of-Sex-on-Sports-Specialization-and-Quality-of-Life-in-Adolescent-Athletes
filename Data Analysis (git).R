## Data Analysis + Visualization 

rm(list = ls())

source("Data Clean.R")

library(table1)

# Continuous Var Render fn. 
render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits = 3, round.integers = F, digits.pct = 2), 
       c("", "Mean (SD)" = sprintf("%s (&plusmn;%s)", MEAN, SD)))
}

# Categorical Var Render fn. 
render.cat <- function(x) {
  c("", 
    sapply(stats.default(x), 
           function(y) with(y, sprintf("%d (%0.0f%%)", FREQ, PCT))))
}

# T-test/Chi-square p-value column fn.
pvalue <- function(x, ...) {
  y <- unlist(x)
  g <- factor(rep(1:length(x), times = sapply(x, length)))
  if (is.numeric(y)) {
    p <- t.test(y ~ g)$p.value
  } else {
    p <- chisq.test(table(y, g))$p.value
  }
  c("", sub("<", "&lt;", format.pval(p, digits = 3, eps = 0.001)))
}

# Additional Data Clean
dat.vis <- dat %>% drop_na(spec_level)
dat.vis$spec_level <- factor(dat.vis$spec_level, levels = c("Low", "Medium", "High"))

# PROMIS Mobility ANOVA
mod.mob <- aov(
  formula = promis_mob ~ spec_level*sex,   
  data = dat)

# Extracts ANOVA table 
sum.mod.mob <- as.data.frame(summary.aov(mod.mob)[[1]])
rownames(sum.mod.mob) <- c("Specialization Level", "Sex", "Interaction", "Residuals")

# Post-hoc test
TukeyHSD(mod.mob)

p.mob <- ggplot(
  data = dat.vis,
  aes(
    x = spec_level, 
    y = promis_mob,
    fill = sex
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  theme_bw(

    ) + 
  theme(
    legend.position = "bottom"
  ) + 
  labs(
    x = "Specialization Level",
    y = "Mobility PROMIS",
    fill = "Sex"
    ) + 
  scale_fill_brewer(
    palette = "Dark2"
  )

p.mob2 <- ggplot(
  data = dat.vis,
  aes(
    x = spec_level, 
    y = promis_mob,
    fill = sex
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  theme_bw(
    
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  labs(
    x = "Specialization Level",
    y = "Mobility PROMIS",
    fill = "Sex",
    title = "Mobility PROMIS (bad plot, sorry)"
  ) + 
  scale_fill_brewer(
    palette = "Dark2"
  ) + 
  facet_wrap(
    ~spec_level
  )

# PROMIS Anxiety 
mod.anx <- aov(
  formula = promis_anx ~ spec_level*sex,   
  data = dat)

summary.aov(mod.anx)
TukeyHSD(mod.anx)

sum.mod.anx <- as.data.frame(summary.aov(mod.anx)[[1]])
rownames(sum.mod.anx) <- c("Specialization Level", "Sex", "Interaction", "Residuals")

TukeyHSD(mod.anx)

p.anx <- ggplot(
  data = dat.vis,
  aes(
    x = spec_level, 
    y = promis_anx,
    fill = sex
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  theme_bw(
    
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  labs(
    x = "Specialization Level",
    y = "Anxiety PROMIS",
    fill = "Sex",
    title = "Anxiety PROMIS"
  ) + 
  scale_fill_brewer(
    palette = "Dark2"
  )


# PROMIS Depression 
mod.dep <- aov(
  formula = promis_dep ~ spec_level*sex,
  data = dat)

summary.aov(mod.dep)
TukeyHSD(mod.dep)

sum.mod.dep <- as.data.frame(summary.aov(mod.dep)[[1]])
rownames(sum.mod.dep) <- c("Specialization Level", "Sex", "Interaction", "Residuals")

p.dep <- ggplot(
  data = dat.vis,
  aes(
    x = spec_level, 
    y = promis_dep,
    fill = sex
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  theme_bw(
    
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  labs(
    x = "Specialization Level",
    y = "Depression PROMIS",
    fill = "Sex",
    title = "Depression PROMIS"
  ) + 
  scale_fill_brewer(
    palette = "Dark2"
  )


# PROMIS Fatigue 
mod.fat <- aov(
  formula = promis_fat ~ spec_level*sex,
  data = dat)

summary.aov(mod.fat)
TukeyHSD(mod.fat)

sum.mod.fat <- as.data.frame(summary.aov(mod.fat)[[1]])
rownames(sum.mod.fat) <- c("Specialization Level", "Sex", "Interaction", "Residuals")


p.fat <- ggplot(
  data = dat.vis,
  aes(
    x = spec_level, 
    y = promis_fat,
    fill = sex
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  theme_bw(
    
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  labs(
    x = "Specialization Level",
    y = "Fatigue PROMIS",
    fill = "Sex",
    title = "Fatigue PROMIS"
  ) + 
  scale_fill_brewer(
    palette = "Dark2"
  )


# PROMIS Peer  
mod.peer <- aov(
  formula = promis_peer ~ spec_level*sex,
  data = dat)

summary.aov(mod.peer)

sum.mod.peer <- as.data.frame(summary.aov(mod.peer)[[1]])
rownames(sum.mod.peer) <- c("Specialization Level", "Sex", "Interaction", "Residuals")

p.peer <- ggplot(
  data = dat.vis,
  aes(
    x = spec_level, 
    y = promis_peer,
    fill = sex
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  theme_bw(
    
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  labs(
    x = "Specialization Level",
    y = "Peer PROMIS",
    fill = "Sex",
    title = "Peer PROMIS"
  ) + 
  scale_fill_brewer(
    palette = "Dark2"
  )

# PROMIS Pain  
mod.pain <- aov(
  formula = promis_pain ~ spec_level*sex,
  data = dat)

summary.aov(mod.pain)

sum.mod.pain <- as.data.frame(summary.aov(mod.pain)[[1]])
rownames(sum.mod.pain) <- c("Specialization Level", "Sex", "Interaction", "Residuals")

p.pain <- ggplot(
  data = dat.vis,
  aes(
    x = spec_level, 
    y = promis_pain,
    fill = sex
  )
) + 
  stat_boxplot(
    geom = "errorbar"
  ) + 
  geom_boxplot(
    
  ) + 
  theme_bw(
    
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  labs(
    x = "Specialization Level",
    y = "Pain PROMIS",
    fill = "Sex",
    title = "Pain PROMIS"
  ) + 
  scale_fill_brewer(
    palette = "Dark2"
  )

## Summary Tables

dat.sum <- dat.vis

# Relabels factor names 
label(dat.sum$promis_mob) <- "Mobility"
label(dat.sum$promis_anx) <- "Anxiety"
label(dat.sum$promis_dep) <- "Depression"
label(dat.sum$promis_fat) <- "Fatigue"
label(dat.sum$promis_peer) <- "Peer Relations"
label(dat.sum$promis_pain) <- "Pain"

table1(
  ~ promis_mob + promis_anx + promis_dep + promis_fat + promis_peer + promis_pain | sex + spec_level,
  data = dat.sum, 
  rowlabelhead = "Specialization Level",
  caption = "PROMIS Domains by Sex and Specialization Level",
  overall = F, 
  render.continuous = render.cont, 
  render.categorical = render.cat,
  render.missing = NULL
)

label(dat.sum$age) <- "Age (yrs)"
label(dat.sum$height_cm) <- "Height (cm)"
label(dat.sum$weight_kg) <- "Weight (kg)"
label(dat.sum$hours_per_week_sport) <- "Hours/Week in Sport"

table1(
  ~ age + height_cm + weight_kg + hours_per_week_sport | sex + spec_level,
  data = dat.sum, 
  rowlabelhead = "Specialization Level",
  caption = "Demographics by Sex and Specialization Level",
  overall = F, 
  render.continuous = render.cont, 
  render.categorical = render.cat,
  render.missing = NULL
)


