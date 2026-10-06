## HW 3 Capstone
#########################################################

## load all relevant files
df_rost <- read.csv("C:/Users/liqiq/capstone/atusrost_2024.dat", header = TRUE)
df_act <- read.csv("C:/Users/liqiq/capstone/atusact_2024.dat", header = TRUE)
df_resp <- read.csv("C:/Users/liqiq/capstone/atusresp_2024.dat", header = TRUE)
df_who <- read.csv("C:/Users/liqiq/capstone/atuswho_2024.dat", header = TRUE)

# using relevant library
library(vioplot)
library(dplyr)

#get kids under 10 years olf
kids_under_10 <- df_rost %>%
  filter(
    TULINENO > 1,
    TEAGE < 10
  ) %>%
  select(TUCASEID, TULINENO) %>%
  distinct()

# Household-level list
households_with_kid_under_10 <- kids_under_10 %>%
  distinct(TUCASEID)

# -----------------------------------------------------------------------------
# GET RESPONDENT AGE
# -----------------------------------------------------------------------------

resp_age <- df_rost %>%
  filter(TULINENO == 1) %>%
  select(TUCASEID, TEAGE)


# -----------------------------------------------------------------------------
# FILTER TO EMPLOYED ADULTS WHO HAVE A CHILD UNDER 10
# -----------------------------------------------------------------------------

adult_workers <- df_resp %>%
  inner_join(resp_age, by = "TUCASEID") %>%
  
  # THIS IS THE IMPORTANT PART YOU WERE MISSING
  inner_join(households_with_kid_under_10,
             by = "TUCASEID") %>%
  
  filter(
    TELFS %in% c(1, 2),
    TEAGE >= 18
  ) %>%
  
  mutate(
    industry = case_when(
      TRMJIND1 %in% c(7, 8) ~ "Business",
      TRMJIND1 %in% c(2, 3, 4, 5) ~ "Production",
      TRUE ~ "Other"
    )
  ) %>%
  
  select(
    TUCASEID,
    TEAGE,
    industry
  )


# -----------------------------------------------------------------------------
# IDENTIFY EATING AT NOON
# -----------------------------------------------------------------------------

is_noon <- function(start_str, stop_str) {
  
  start_min <-
    as.numeric(substr(start_str, 1, 2)) * 60 +
    as.numeric(substr(start_str, 4, 5))
  
  stop_min <-
    as.numeric(substr(stop_str, 1, 2)) * 60 +
    as.numeric(substr(stop_str, 4, 5))
  
  if (stop_min < start_min) {
    stop_min <- stop_min + 1440
  }
  
  start_min <= 720 & stop_min >= 720
}


noon_eating_act <- df_act %>%
  
  # ATUS major activity category 11
  filter(substr(as.character(TRCODE), 1, 2) == "11") %>%
  
  rowwise() %>%
  filter(is_noon(TUSTARTTIM, TUSTOPTIME)) %>%
  ungroup() %>%
  
  select(
    TUCASEID,
    TUACTIVITY_N
  )


# -----------------------------------------------------------------------------
# WAS A HOUSEHOLD CHILD UNDER 10 PRESENT
# -----------------------------------------------------------------------------

noon_eating_with_kids <- noon_eating_act %>%
  
  inner_join(
    df_who,
    by = c("TUCASEID", "TUACTIVITY_N")
  ) %>%
  
  # Match WHO-file person to actual household child under 10
  inner_join(
    kids_under_10,
    by = c("TUCASEID", "TULINENO")
  ) %>%
  
  distinct(TUCASEID) %>%
  
  mutate(
    eating_with_kid = "Yes"
  )

# -----------------------------------------------------------------------------
# 6. CREATE FINAL DATA
# -----------------------------------------------------------------------------

data_p <- adult_workers %>%
  
  left_join(
    noon_eating_with_kids,
    by = "TUCASEID"
  ) %>%
  
  mutate(
    
    eating_with_kid =
      ifelse(
        is.na(eating_with_kid),
        "No",
        "Yes"
      ),
    
    industry = factor(
      industry,
      levels = c(
        "Business",
        "Production",
        "Other"
      )
    ),
    
    eating_with_kid = factor(
      eating_with_kid,
      levels = c(
        "No",
        "Yes"
      )
    ),
    
    group = interaction(
      industry,
      eating_with_kid,
      sep = "_"
    ),
    
    group = factor(
      group,
      levels = c(
        "Business_No",
        "Business_Yes",
        "Production_No",
        "Production_Yes",
        "Other_No",
        "Other_Yes"
      )
    )
  )


# -------------------
# FINAL PLOT
# -------------------

##########################################
# Exact group order
data_p$group <- factor(
  data_p$group,
  levels = c(
    "Business_No",
    "Business_Yes",
    "Production_No",
    "Production_Yes",
    "Other_No",
    "Other_Yes"
  )
)

# Colors
cols <- c(
  "#E99AA7", "#E99AA7",
  "#6495ED", "#6495ED",
  "#FFB51B", "#FFB51B"
)

point_cols <- c(
  "#FF3B30", "#FF3B30",
  "#145BFF", "#145BFF",
  "#E69A00", "#E69A00"
)

# Reference is wide and relatively short
par(
  mar = c(5.0, 4.6, 4.1, 1.5),
  mgp = c(2.7, 0.8, 0),
  las = 1,
  cex.axis = 0.85,
  cex.lab = 0.90,
  cex.main = 0.90
)

# -----------------------------------------------------------------------------
# VIOLINS
# -----------------------------------------------------------------------------

vioplot(
  TEAGE ~ group,
  data = data_p,
  
  col = cols,
  
  # softer outline like reference
  border = "#555555",
  lwd = 1,
  
  # DO NOT USE areaEqual = TRUE
  # This was causing your Yes groups to become tiny.
  
  # Width of each violin
  wex = 0.80,
  
  # slightly smoother KDE
  # boxplot inside violin
  rectCol = "white",
  lineCol = "#333333",
  colMed = "black",
  pchMed = 19,
  
  ylim = c(17, 87),
  
  names = c(
    "No", "Yes",
    "No", "Yes",
    "No", "Yes"
  ),
  
  xlab = "Eating with kid (under 10) at noon?",
  ylab = "Age",
  
  main =
    "Ages of adults with jobs,\nby industry and whether they are eating with kids (under 10) at noon"
)


# -----------------------------------------------------------------------------
# INDIVIDUAL POINTS
# -----------------------------------------------------------------------------

set.seed(123)

group_levels <- levels(data_p$group)

for (i in seq_along(group_levels)) {
  
  ages <- data_p$TEAGE[
    data_p$group == group_levels[i]
  ]
  
  stripchart(
    ages,
    
    at = i,
    
    method = "jitter",
    
    jitter = 0.08,
    
    vertical = TRUE,
    add = TRUE,
    pch = 21,
    
    bg = NA,
    col = point_cols[i],
    
    cex = 0.42,
    
    lwd = 0.65
  )
}

# -----------------------------------------------------------------------------
# LEGEND
# -----------------------------------------------------------------------------

legend(
  "topleft",
  
  legend = c(
    "Business",
    "Production",
    "Other"
  ),
  
  fill = c(
    "#E99AA7",
    "#6495ED",
    "#FFB51B"
  ),
  
  border = "#555555",
  
  bg = "white",
  box.col = "#777777",
  box.lwd = 0.8,
  
  cex = 0.80,
  
  y.intersp = 0.85,
  x.intersp = 0.65,
  
  inset = c(0.012, 0.012)
)