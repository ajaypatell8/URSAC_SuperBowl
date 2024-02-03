#load in packages------------------------------------------------------------------------------
#package with nfl data
library(nflreadr)

#package with nfl logos for plotting
library(nflreadr)

#package that contains functions to manipulate data
library(tidyverse)

#package that contains various graphing tools
library(ggplot2)

#packages to create clean tables
library(gt)
library(gtExtras)



#we'll first load in our data------------------------------------------------------------------
#we use the load_participation function from nflreadr as it has personnel/coverage data
data <- nflreadr::load_participation(seasons = 2023, include_pbp = TRUE)

#let's first analyze how each team performs against certain coverages on offense and defense
offense <- data %>% 
  #make sure we aren't missing coverage labels
  filter(!is.na(defense_coverage_type)) %>% 
  #filter to only kc and sf on offense / %in% works by saying if our data is in this vector, keep it
  #and only passes
  filter(possession_team %in% c('KC', 'SF'), pass_attempt == 1) %>% 
  #group by each offense and coverage faced
  group_by(possession_team, defense_coverage_type) %>% 
  #we want to see yards per pass, epa per pass, and number of passes
  summarise(yards = mean(yards_gained),
            epa = mean(epa),
            plays = n()) %>% 
  #we'll also add a new column saying that is each team's offensive results
  mutate(sideOfBall = 'Offense') %>% 
  #rename the possessionTeam column to just team
  rename(team = possession_team)

#let's view what we made
View(offense)

#create the same dataframe but for defense
defense <- data %>% 
  #make sure we aren't missing coverage labels
  filter(!is.na(defense_coverage_type)) %>% 
  #filter to only kc and sf on defense / %in% works by saying if our data is in this vector, keep it
  #and only passes
  filter(defteam %in% c('KC', 'SF'), pass_attempt == 1) %>% 
  #group by each defense and coverage faced
  group_by(defteam, defense_coverage_type) %>% 
  #we want to see yards per pass, epa per pass, and number of passes
  summarise(yards = mean(yards_gained),
            epa = mean(epa),
            plays = n()) %>% 
  #we'll also add a new column saying that is each team's offensive results
  mutate(sideOfBall = 'Defense') %>% 
  #rename the defteam column to just team
  rename(team = defteam)

#we have our two dataframes -> let's bind them together to have all the data in one df
coverage <- bind_rows(offense, defense)

#remove plays that don't happen too often
#we'll keep all coverages that have at least 25 plays and remove 2_MAN since it's not common
coverage <- coverage %>% 
  filter(plays >= 25, defense_coverage_type != '2_MAN')

#let's visualize it
#we want coverage on the x-axis and epa on the y-axis
ggplot(coverage, aes(x = reorder(defense_coverage_type, -epa), y = epa, fill = team)) +
  #this will create the bars on the plot
  geom_bar(position="dodge", stat="identity", aes(color = team, fill = team)) + 
  #faceting will create subplots based on a variable
  facet_wrap(~sideOfBall) +
  #adding labels to our plot
  labs(x = "Coverage",
       y = "EPA Per Play",
       title = "How Do The Super Bowl Teams Perform By Coverage?",
       subtitle = "2023 | Higher EPA Is Better For Offense",
       caption = 'Data: @nflreadr') +
  theme_minimal() +
  #this will add the coloring to the bars based on the team
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(alpha = 0.8) +
  #this will add logos to our plot
  nflplotR::geom_nfl_logos(aes(team_abbr = team), height = 0.05) +
  #rotates our axis labels so they're easier to read
  theme(axis.text.x = element_text(angle = 45))

#let's move on to something else, finding receiver performance for each team------------------------
#we want to create a table of receivers showing catches, yards, and a couple other stats
rec <- data %>% 
  #filter to catches only and KC/SF
  filter(complete_pass == 1, possession_team %in% c("KC", "SF")) %>% 
  #group by the receiver
  group_by(possession_team, receiver_player_name, receiver_player_id) %>% 
  #aDOT is the average depth of target
  summarise(catches = n(),
            aDOT = mean(air_yards),
            yac = mean(yards_after_catch),
            yards = sum(yards_gained),
            touchdowns = sum(touchdown)) %>% 
  #keep players that have at least 25 catches
  filter(catches >= 25)

#let's make our table
rec %>% 
  #we first ungroup by receiver and instead group by team
  ungroup() %>% 
  group_by(possession_team) %>% 
  #let's order our data by total yards
  arrange(desc(yards)) %>% 
  gt() %>% 
  #add player headshots
  nflplotR::gt_nfl_headshots('receiver_player_id') %>% 
  #rename columns
  cols_label(
    receiver_player_name = 'Player',
    receiver_player_id = '',
    catches = 'Catches',
    yac = 'YAC',
    yards = 'Yards',
    touchdowns = 'TDs'
  ) %>% 
  #let's replace KC and SF with their wordmarks
  nflplotR::gt_nfl_wordmarks(locations = cells_row_groups()) %>% 
  #let's add a fancy theme
  gt_theme_538() %>% 
  #round adot and yac
  fmt_number(columns = c(yac, aDOT)) %>% 
  #let's add color scaling to the yards column
  gt_hulk_col_numeric(columns = yards) %>% 
  #and lastly let's give it labels
  tab_header(title = 'Super Bowl Receivers Preview',
             subtitle = '2023 | Minimum 25 Catches | Data: @nflreadr')
  

