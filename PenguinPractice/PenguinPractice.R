#Practicing with the Penguins
library(palmerpenguins)
library(tidyverse)

data(penguins)

penguins[!is.na(penguins)]

penguins$body_mass_kg <- penguins$body_mass_g *1000

#Creating a graph of where each Penguin Species lives
ggplot(data = penguins) + 
aes(x = species) + geom_bar(fill = "blue") +
  facet_wrap(~island)

#Graphing the flipper length vs the species
ggplot(data = penguins) +
  aes(x = flipper_length_mm) +
  geom_bar(fill = "blue") +
  facet_wrap(~species)

#Graphing the body mass vs the species
ggplot(data = penguins) + 
  aes(x = body_mass_kg, fill = island) + geom_bar() +
  facet_wrap(~species)

#Graphing the bill length vs the species
ggplot(data = penguins) +
  aes(x = bill_length_mm, y = body_mass_kg, color = species) +
  geom_point()