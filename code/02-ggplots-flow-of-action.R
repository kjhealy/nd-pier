
## ----05-work-with-dplyr-and-geoms-1, message = TRUE---------------------------
library(here)      # manage file paths
library(socviz)    # data and some useful functions
library(tidyverse) # your friend and mine
library(gapminder) # gapminder data


## ----03-make-a-graph-12-------------------------------------------------------
gapminder


## ----03-make-a-graph-13-------------------------------------------------------
dim(gapminder)


## ----03-make-a-graph-14-------------------------------------------------------
p <- ggplot(data = gapminder)


## ----03-make-a-graph-15-------------------------------------------------------
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp))


## ----03-make-a-graph-16, fig.cap='This empty plot has no geoms.', fig.width=8, fig.height=5----
p


## ----03-make-a-graph-17, fig.cap='A scatterplot of Life Expectancy vs GDP', fig.width=8, fig.height=5----
p + geom_point() 


## ----03-make-a-graph-18, fig.cap='A scatterplot of Life Expectancy vs GDP', fig.width=8, fig.height=5----
p + geom_smooth() 


## ----03-make-a-graph-19, fig.cap='Life Expectancy vs GDP, using a smoother.', fig.width=8, fig.height=5----

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y=lifeExp))
p + geom_smooth()



## ----03-make-a-graph-20, fig.cap='Life Expectancy vs GDP, using a smoother.', fig.width=8, fig.height=5----

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y=lifeExp))
p + geom_point() + geom_smooth()



## ----reveal-additive1, include = FALSE----------------------------------------
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y=lifeExp))
p + geom_smooth() + 
  geom_point() 


## ----codefig-functionargs, message=FALSE, fig.show="hide", fig.cap="An ill-advised linear fit", fig.width=4.8, fig.height=4.5----
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp))
p + geom_point() + 
  geom_smooth(method = "lm") 


## ----03-make-a-graph-21, echo=FALSE-------------------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-functionargs", "png"))


## ----reveal-logtrans, include = FALSE-----------------------------------------
 p <- ggplot(data = gapminder, 
             mapping = aes(x = gdpPercap, 
                           y=lifeExp))
p + geom_point() +
    geom_smooth(method = "lm") +
    scale_x_log10()


## ----reveal-logtrans2, include = FALSE----------------------------------------
p <- ggplot(data = gapminder, 
            mapping = aes(x = gdpPercap, 
                          y=lifeExp))
p + geom_point() +
    geom_smooth(method = "lm") +
    scale_x_log10(labels = scales::label_dollar())


## ----codefig-logtranslab, message=FALSE, fig.show="hide", fig.width=5, fig.height=4.5----
p <- ggplot(data = gapminder, 
            mapping = aes(x = gdpPercap, 
                          y = lifeExp))
p + geom_point() + 
  geom_smooth(method = "lm") +
    scale_x_log10(labels = scales::label_dollar()) +
    labs(x = "GDP Per Capita", 
         y = "Life Expectancy in Years",
         title = "Economic Growth and Life Expectancy",
         subtitle = "Data points are country-years",
         caption = "Source: Gapminder.")


## ----03-make-a-graph-22, echo=FALSE-------------------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-logtranslab", "png"))


## ----03-make-a-graph-23-------------------------------------------------------
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp,
                          color = "purple"))

## Put in an object for convenience
p_out <- p + geom_point() +
    geom_smooth(method = "loess") +
    scale_x_log10()


## ----03-make-a-graph-24, fig.width=8, fig.height=5----------------------------
p_out


## ----03-make-a-graph-25-------------------------------------------------------
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp))

## Put in an object for convenience
p_out <- p + geom_point(color = "purple") +
    geom_smooth(method = "loess") +
    scale_x_log10()


## ----03-make-a-graph-26, fig.width=8, fig.height=5----------------------------
p_out


## ----03-make-a-graph-27-------------------------------------------------------
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp)) 
p_out <- p + geom_point(alpha = 0.3) +
    geom_smooth(color = "orange", 
                se = FALSE, 
                linewidth = 8, 
                method = "lm") +
    scale_x_log10()


## ----03-make-a-graph-28, fig.width=8.5, fig.height=5--------------------------
p_out


## ----codefig-alphapoints, message=FALSE, fig.show="hide", fig.width=5, fig.height=4.5----
p <- ggplot(data = gapminder, 
            mapping = aes(x = gdpPercap, 
                          y = lifeExp))
p + geom_point(alpha = 0.3) + #<<
  geom_smooth(method = "lm") +
    scale_x_log10(labels = scales::label_dollar()) +
    labs(x = "GDP Per Capita", 
         y = "Life Expectancy in Years",
         title = "Economic Growth and Life Expectancy",
         subtitle = "Data points are country-years",
         caption = "Source: Gapminder.")


## ----03-make-a-graph-extra, echo=FALSE----------------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-alphapoints", "png"))


## ----reveal-pergeom1, include = FALSE-----------------------------------------
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp,
                          color = continent,
                          fill = continent))
p + geom_point() +
    geom_smooth(method = "loess") +
    scale_x_log10(labels = scales::label_dollar())


## ----reveal-pergeom2, include = FALSE-----------------------------------------
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp))
p + geom_point(mapping = aes(color = continent)) +
    geom_smooth(method = "loess") +
    scale_x_log10(labels = scales::label_dollar())
 


## ----05-work-with-dplyr-and-geoms-7-------------------------------------------
gss_sm |> 
  select(id, bigregion, religion) 


## ----reveal-onetablevar, include = FALSE--------------------------------------
gss_sm |> 
  group_by(bigregion) |>  #<<
  summarize(total = n()) 


## ----reveal-pipe1, include = FALSE--------------------------------------------
gss_sm |>  
  group_by(bigregion, religion) |> 
  summarize(total = n()) |> 
  mutate(freq = total / sum(total),
           pct = round((freq*100), 1))



## ----05-work-with-dplyr-and-geoms-9-------------------------------------------
gss_sm |> 
  group_by(bigregion, religion) |> #<<
  summarize(total = n()) |> 
  mutate(freq = total / sum(total),
           pct = round((freq*100), 1))


## ----05-work-with-dplyr-and-geoms-10------------------------------------------
gss_sm |> 
  group_by(bigregion, religion) |> 
  summarize(total = n()) |> 
  mutate(freq = total / sum(total),
         pct = round((freq*100), 1)) #<<


## ----05-work-with-dplyr-and-geoms-11------------------------------------------
gss_sm |> 
  group_by(bigregion, religion) |> #<<
  summarize(total = n()) |> #<<
  mutate(freq = total / sum(total),
           pct = round((freq*100), 1)) 


## ----05-work-with-dplyr-and-geoms-12------------------------------------------
gss_sm |> 
  group_by(bigregion, religion) |> #<<
  summarize(n = n()) #<<


## ----05-work-with-dplyr-and-geoms-13------------------------------------------
gss_sm |> 
  group_by(bigregion, religion) |> 
  tally() #<<


## ----05-work-with-dplyr-and-geoms-14------------------------------------------
gss_sm |> 
  count(bigregion, religion) #<<


## ----05-work-with-dplyr-and-geoms-15------------------------------------------
## Calculate pct religion within region?
rel_by_region <- gss_sm |> 
  count(bigregion, religion) |> 
  mutate(pct = round((n/sum(n))*100, 1)) 

rel_by_region


## ----05-work-with-dplyr-and-geoms-16------------------------------------------
## Each region should sum to ~100
rel_by_region |> 
  group_by(bigregion) |> 
  summarize(total = sum(pct)) 



## ----05-work-with-dplyr-and-geoms-17------------------------------------------
rel_by_region <- gss_sm |> 
  count(bigregion, religion) |> #<< 
  mutate(pct = round((n/sum(n))*100, 1)) 


## ----05-work-with-dplyr-and-geoms-18------------------------------------------
rel_by_region |> 
  summarize(total = sum(pct))


## ----05-work-with-dplyr-and-geoms-19------------------------------------------
rel_by_region <- gss_sm |> 
  group_by(bigregion, religion) |> #<<
  tally() |> #<<
  mutate(pct = round((n/sum(n))*100, 1)) 


## ----05-work-with-dplyr-and-geoms-20------------------------------------------
# Check
rel_by_region |> 
  group_by(bigregion) |> 
  summarize(total = sum(pct))



## ----05-work-with-dplyr-and-geoms-21, eval = FALSE----------------------------
## gss_sm |>
##   count(bigregion, religion) |>
##   pivot_wider(names_from = bigregion, values_from = n) |>  #<<
##   kable()


## ----05-work-with-dplyr-and-geoms-22, echo = FALSE----------------------------
gss_sm |> 
  count(bigregion, religion) |> 
  pivot_wider(names_from = bigregion, values_from = n) |> 
  knitr::kable()  


## ----05-work-with-dplyr-and-geoms-23, fig.height=4, fig.width=15--------------
gss_sm |> 
  group_by(bigregion, religion) |> 
  tally() |> 
  mutate(pct = round((n/sum(n))*100, 1)) |> 
  drop_na() |> 
  ggplot(mapping = aes(x = pct, y = reorder(religion, -pct), fill = religion)) + #<<
  geom_col() + #<<
    labs(x = "Percent", y = NULL) +
    guides(fill = "none") + 
    facet_wrap(~ bigregion, nrow = 1)


## ----05-work-with-dplyr-and-geoms-24------------------------------------------
rel_by_region <- gss_sm |> 
  group_by(bigregion, religion) |> 
  tally() |> 
  mutate(pct = round((n/sum(n))*100, 1)) |> 
  drop_na()


head(rel_by_region)


## ----05-work-with-dplyr-and-geoms-25------------------------------------------
p <- ggplot(data = rel_by_region, 
                mapping = aes(x = bigregion, 
                              y = pct, 
                              fill = religion))
p_out <- p + geom_col(position = "dodge") +
    labs(x = "Region",
         y = "Percent", 
         fill = "Religion") 


## ----05-work-with-dplyr-and-geoms-26, echo = FALSE, fig.height=7, fig.width=12----
p_out


## ----05-work-with-dplyr-and-geoms-27, echo = FALSE, fig.height=7, fig.width=12----
p_out


## ----05-work-with-dplyr-and-geoms-28------------------------------------------
p <- ggplot(data = rel_by_region, 
                mapping = aes(x = pct, #<<
                              y = reorder(religion, -pct), #<<
                              fill = religion))
p_out_facet <- p + geom_col() +
  guides(fill = "none") + 
  facet_wrap(~ bigregion, nrow = 1) +
  labs(x = "Percent",
       y = NULL) 



## ----05-work-with-dplyr-and-geoms-29, echo = FALSE, fig.height = 3.5, fig.width=15----
p_out_facet


## ----05-work-with-dplyr-and-geoms-37------------------------------------------
organdata


## ----05-work-with-dplyr-and-geoms-38, fig.width=10, fig.height=6--------------
p <- ggplot(data = organdata,
            mapping = aes(x = year, y = donors))
p + geom_point()


## ----05-work-with-dplyr-and-geoms-39, fig.width=10, fig.height=6--------------
p <- ggplot(data = organdata,
            mapping = aes(x = year, y = donors))
p + geom_line() 


## ----05-work-with-dplyr-and-geoms-40, fig.width=10, fig.height=6--------------
p <- ggplot(data = organdata,
            mapping = aes(x = year, y = donors))
p + geom_line(aes(group = country)) 


## ----05-work-with-dplyr-and-geoms-41a, fig.width=21, fig.height=8-------------
p <- ggplot(data = organdata,
            mapping = aes(x = year, y = donors))
p + geom_line() + 
  facet_wrap(~ country, nrow = 3)


## ----05-work-with-dplyr-and-geoms-41b, fig.width=21, fig.height=8-------------
p <- ggplot(data = organdata,
            mapping = aes(x = year, y = donors))
p + geom_line() + 
  facet_wrap(~ reorder(country, donors, na.rm = TRUE), nrow = 3)


## ----05-work-with-dplyr-and-geoms-41c, fig.width=21, fig.height=8-------------
p <- ggplot(data = organdata,
            mapping = aes(x = year, y = donors))
p + geom_line() + 
  facet_wrap(~ reorder(country, -donors, na.rm = TRUE), nrow = 3)


## ----05-work-with-dplyr-and-geoms-42, fig.width = 15, fig.height=5------------
## Pipeline the data directly; then it's implicitly the first argument to `ggplot()`
organdata |> 
  ggplot(mapping = aes(x = country, y = donors)) + 
  geom_boxplot()


## ----05-work-with-dplyr-and-geoms-43, fig.width=10, fig.height=6--------------
organdata |> 
  ggplot(mapping = aes(x = donors, y = country)) + #<<
  geom_boxplot() +
  labs(y = NULL)


## ----05-work-with-dplyr-and-geoms-44, fig.width=10, fig.height=6--------------
organdata |> 
  ggplot(mapping = aes(x = donors, y = reorder(country, donors, na.rm = TRUE))) + #<<
  geom_boxplot() +
  labs(y = NULL)


## ----05-work-with-dplyr-and-geoms-45, fig.width=10, fig.height=6--------------
organdata |> 
  ggplot(mapping = aes(x = donors, y = reorder(country, donors, sd, na.rm = TRUE))) + #<<
  geom_boxplot() +
  labs(y = NULL)


## ----05-work-with-dplyr-and-geoms-46, fig.width=10, fig.height=6--------------
organdata |> 
  ggplot(mapping = aes(x = donors, y = reorder(country, donors, na.rm = TRUE), fill = world)) + #<<
  geom_boxplot() +
  labs(y = NULL)


## ----05-work-with-dplyr-and-geoms-47, fig.width=10, fig.height=5.5------------
organdata |> 
  ggplot(mapping = aes(x = donors, y = reorder(country, donors, na.rm = TRUE), color = world)) + 
  geom_point(size = rel(3)) + #<<
  labs(y = NULL)


## ----05-work-with-dplyr-and-geoms-48, fig.width=10, fig.height=6--------------
organdata |> 
  ggplot(mapping = aes(x = donors, y = reorder(country, donors, na.rm = TRUE), color = world)) + 
  geom_jitter(size = rel(3)) + #<<
  labs(y = NULL)


## ----05-work-with-dplyr-and-geoms-49, fig.width=10, fig.height=6--------------
organdata |> 
  ggplot(mapping = aes(x = donors, y = reorder(country, donors, na.rm = TRUE),
                       color = world)) + 
  geom_jitter(size = rel(3), position = position_jitter(height = 0.1)) + #<<
  labs(y = NULL)


## ----05-work-with-dplyr-and-geoms-50------------------------------------------
by_country <- organdata |>  
  group_by(consent_law, country)  |> 
    summarize(donors_mean= mean(donors, na.rm = TRUE),
              donors_sd = sd(donors, na.rm = TRUE),
              gdp_mean = mean(gdp, na.rm = TRUE),
              health_mean = mean(health, na.rm = TRUE),
              roads_mean = mean(roads, na.rm = TRUE),
              cerebvas_mean = mean(cerebvas, na.rm = TRUE))

head(by_country)


## ----05-work-with-dplyr-and-geoms-51------------------------------------------
by_country <- organdata |> 
  group_by(consent_law, country) |>
    summarize(across(where(is.numeric),#<<
                     list(mean = mean, 
                          sd = sd),
                      na.rm = TRUE))
head(by_country)              
              


## ----05-work-with-dplyr-and-geoms-52------------------------------------------
by_country <- organdata |> 
  group_by(consent_law, country) |>
    summarize(across(where(is.numeric),
                     list(mean = mean, 
                          sd = sd),
                      na.rm = TRUE), 
              .groups = "drop") #<<
head(by_country)              
              


## ----codefig-consent1, message=FALSE, fig.show="hide", fig.width=8, fig.height=5----
by_country |> 
  ggplot(mapping = 
           aes(x = donors_mean, 
               y = reorder(country, donors_mean),
               color = consent_law)) + 
  geom_point(size=3) +
  labs(x = "Donor Procurement Rate",
       y = NULL, 
       color = "Consent Law")


## ----05-work-with-dplyr-and-geoms-53, echo=FALSE------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-consent1", "png"))



## ----codefig-consent2, message=FALSE, fig.show="hide", fig.width=8, fig.height=5----
by_country |> 
  ggplot(mapping = 
           aes(x = donors_mean, 
               y = reorder(country, donors_mean),
               color = consent_law)) + 
  geom_point(size=3) +
  guides(color = "none") +
  facet_wrap(~ consent_law) + #<<
  labs(x = "Donor Procurement Rate",
       y = NULL, 
       color = "Consent Law")


## ----05-work-with-dplyr-and-geoms-54, echo=FALSE------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-consent2", "png"))


## ----codefig-consent2a, message=FALSE, fig.show="hide", fig.width=5, fig.height=9----
by_country |> 
  ggplot(mapping = 
           aes(x = donors_mean, 
               y = reorder(country, donors_mean),
               color = consent_law)) + 
  geom_point(size=3) +
  guides(color = "none") +
  facet_wrap(~ consent_law, ncol = 1) + #<<
  labs(x = "Donor Procurement Rate",
       y = NULL, 
       color = "Consent Law")


## ----05-work-with-dplyr-and-geoms-55, echo=FALSE------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-consent2a", "png"))


## ----codefig-consent3, message=FALSE, fig.show="hide", fig.width=8, fig.height=6----
by_country |> 
  ggplot(mapping = 
           aes(x = donors_mean, 
               y = reorder(country, donors_mean),
               color = consent_law)) + 
  geom_point(size=3) +
  guides(color = "none") +
  facet_wrap(~ consent_law, 
             ncol = 1,
             scales = "free_y") +  #<<
  labs(x = "Donor Procurement Rate",
       y = NULL, 
       color = "Consent Law")


## ----05-work-with-dplyr-and-geoms-56, echo=FALSE------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-consent3", "png"))


## ----codefig-consent4, message=FALSE, fig.show="hide", fig.width=8, fig.height=6----
by_country |> 
  ggplot(mapping = 
           aes(x = donors_mean, 
               y = reorder(country, donors_mean),
               color = consent_law)) + 
  geom_pointrange(mapping = #<<
                    aes(xmin = donors_mean - donors_sd, #<<
                        xmax = donors_mean + donors_sd)) + #<<
  guides(color = "none") +
  facet_wrap(~ consent_law, 
             ncol = 1,
             scales = "free_y") +  
  labs(x = "Donor Procurement Rate",
       y = NULL, 
       color = "Consent Law")


## ----05-work-with-dplyr-and-geoms-57, echo=FALSE------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-consent4", "png"))


## ----05-work-with-dplyr-and-geoms-61------------------------------------------
elections_historic


## ----05-work-with-dplyr-and-geoms-62------------------------------------------
## The packages we'll use in addition to ggplot
library(ggrepel) #<<
library(scales) #<<

p_title <- "Presidential Elections: Popular & Electoral College Margins"
p_subtitle <- "1824-2016"
p_caption <- "Data for 2016 are provisional."
x_label <- "Winner's share of Popular Vote"
y_label <- "Winner's share of Electoral College Votes"


## ----codefig-presplot1, message=FALSE, fig.show="hide", fig.width=5, fig.height=4.5----
p <- ggplot(data = elections_historic, 
            mapping = aes(x = popular_pct, 
                          y = ec_pct,
                          label = winner_label))

p + geom_hline(yintercept = 0.5, 
               linewidth = 1.4, 
               color = "gray80") +
    geom_vline(xintercept = 0.5, 
               linewidth = 1.4, 
               color = "gray80") +
    geom_point()



## ----05-work-with-dplyr-and-geoms-63, echo=FALSE------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-presplot1", "png"))


## ----codefig-presplot2, message=FALSE, fig.show="hide", fig.width=5, fig.height=4.5----
p <- ggplot(data = elections_historic, 
            mapping = aes(x = popular_pct, 
                          y = ec_pct,
                          label = winner_label))

p + geom_hline(yintercept = 0.5, 
               linewidth = 1.4, color = "gray80") +
  geom_vline(xintercept = 0.5, 
             linewidth = 1.4, color = "gray80") +
  geom_point() + 
  geom_text_repel()



## ----05-work-with-dplyr-and-geoms-64, echo=FALSE------------------------------
  knitr::include_graphics(
  knitr::fig_chunk("codefig-presplot2", "png"))


## ----05-work-with-dplyr-and-geoms-65------------------------------------------
p <- ggplot(data = elections_historic, 
            mapping  = aes(x = popular_pct, 
                           y = ec_pct,
                           label = winner_label))

p_out <- p + 
  geom_hline(yintercept = 0.5, 
             linewidth = 1.4, 
             color = "gray80") +
  geom_vline(xintercept = 0.5, 
             linewidth = 1.4, 
             color = "gray80") +
  geom_point() + 
  geom_text_repel() #<<



## ----05-work-with-dplyr-and-geoms-66, echo = FALSE, fig.width=15, fig.height=8.5----
p_out


## ----05-work-with-dplyr-and-geoms-67------------------------------------------
p <- ggplot(data = elections_historic, 
            mapping  = aes(x = popular_pct, 
                           y = ec_pct,
                           label = winner_label))
p_out <- p + geom_hline(yintercept = 0.5, 
                        linewidth = 1.4, 
                        color = "gray80") +
    geom_vline(xintercept = 0.5, 
               linewidth = 1.4, 
               color = "gray80") +
    geom_point() +
    geom_text_repel() +
    scale_x_continuous(labels = label_percent()) + #<<
    scale_y_continuous(labels = label_percent()) #<<


## ----05-work-with-dplyr-and-geoms-68, echo = FALSE, fig.width=15, fig.height=8.5----
p_out


## ----05-work-with-dplyr-and-geoms-69------------------------------------------
p <- ggplot(data = elections_historic, 
            mapping  = aes(x = popular_pct, 
                           y = ec_pct,
                           label = winner_label))
p_out <- p + geom_hline(yintercept = 0.5, 
                        linewidth = 1.4, 
                        color = "gray80") +
  geom_vline(xintercept = 0.5, 
             linewidth = 1.4, 
             color = "gray80") +
  geom_point() +
  geom_text_repel(mapping = aes(family = "Tenso Slide")) +#<<
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_percent()) +
  labs(x = x_label, y = y_label,  #<<
       title = p_title, 
       subtitle = p_subtitle,
       caption = p_caption)   
  
  
  


## ----05-work-with-dplyr-and-geoms-70, echo = FALSE, fig.width=15, fig.height=8.5----
p_out


## ----05-work-with-dplyr-and-geoms-83, echo=FALSE------------------------------
kjhslides::kjh_set_slide_theme()

