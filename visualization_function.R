library("ggplot2")
library("dplyr")
library("plotly")
# install.packages("egg")
library("egg")
---------------------------------
#I added a new function to plot the relationship between spending and expectancy of each country
plotcountry <- function(area){
  p1 <- filter(df, country == area) %>%
    ggplot(mapping = aes(x = spending, y = expectancy))+
    geom_point(cex = 1)+
    geom_line(color="blue")
  theme(legend.position = "none")
  plot(p1)
}
plotcountry("China")
---------------------------------
# quick look at the relationship of spending and expectancy of all countries

p_all <- filter(df, country %in% c(unique(df$country))) %>% 
  ggplot(mapping = aes(x = spending, y = expectancy, color = country))+
  geom_point(cex = .5)+
  theme(legend.position = "none")
 #ggplotly(p_all)


# spending and expectancy for one country

plotly <- function(area){
  
  p1 <- filter(df, country == area) %>%
    plot_ly(x = ~year, y = ~spending) %>% 
    add_lines(name = "spending")
  
  p2 <- filter(df, country == area) %>%
    plot_ly(x = ~year, y = ~expectancy) %>% 
    add_lines(name = "expectancy")
  
  subplot(p1, p2)
  # %>%  layout(title = paste("Trends of spending and expectancy in", area))
}

 plotly("China")






# trends of spending and expectancy within countries
trends <- function(area){
  set.seed(2)
  selected <- c(area, sample(unique(df$country), 10, replace = FALSE))
  
  p1 <- filter(df, country %in% selected) %>% 
    ggplot(mapping = aes(x = year, y = spending, group = country, color = country))+
    geom_line()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    ggtitle("Spending comparison")+
    theme(legend.position = "none")
  # plot(p1)

  
  p2 <- filter(df, country %in% selected) %>% 
    ggplot(mapping = aes(x = year, y = expectancy, group = country, color = country))+
    geom_line()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    ggtitle("Expectancy comparison")
  # plot(p2)
  
  grid.arrange(p1, p2, widths=c(3, 4))

}

 #trends("China")

rela <- function(area){
  set.seed(2)
  selected <- c(area, sample(unique(df$country), 10, replace = FALSE))
  
  p3 <- filter(df, country %in% selected) %>% 
    ggplot(mapping = aes(x = spending, y = expectancy))+
    geom_point(aes(color = country))
  # +ggtitle("Spending and expectancy for ten countries")
  plot(p3)
  
}


 rela("China")
