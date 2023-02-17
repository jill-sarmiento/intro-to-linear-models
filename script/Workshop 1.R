#____________________________________________________--
# INTRODUCTION TO STATS___________________________----

#____________________________________________________--
# LOAD PACKAGES___________________________________----

library(tidyverse)
library(here)
library(janitor)
library(kableExtra)

darwin <- read_csv(here("data", "darwin_raw_data.csv"))

#____________________________________________________--
# DATA ANALYSIS______________________________________----
# Check the data format.

# STRUCTURE--
glimpse("darwin")

# TIDY FORMAT--
head("darwin")

# VARIABLE NAMES--
colnames("darwin")

# CLEAN UP COLUMN NAMES--
darwin <- janitor::clean_names(darwin)

# CHECK FOR DUPLICATION --
darwin %>% 
  duplicated() %>% 
  sum()

# CHECK FOR TYPOS_________________________________--
# TYPOS - IMPOSSIBLE VALUES.

darwin %>% 
  summarise(min=min(height, na.rm=TRUE), 
            max=max(height, na.rm=TRUE))

# TYPOS - DISTINCT CHARACTER / VALUES

darwin %>% 
  distinct(pair)

darwin %>% 
  distinct(type)

# MISSING VALUES__________________________________--

darwin %>% 
  is.na() %>% 
  sum()

#____________________________________________________--
# SUMMARY_________________________________________----

summary(darwin)

#____________________________________________________--
# VISUALISATION____________________________________----

darwin %>% 
  ggplot(aes(x=type,
             y=height))+
  geom_point()

#____________________________________________________--
# COMPARING GROUPS_________________________________----

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))

# MAKE NEW OBJECT --

darwin_summary <-darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))

# MAKE A SUMMARY PLOT --

darwin_summary %>% 
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw()

##____________________________________________________--
# KABLE EXTRA_______________________________________----

darwin_summary %>% 
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

# 