
# load packages
library(tidyverse)
library(palmerpenguins)
library(ggplot2)
ggplot2::theme_set(ggplot2::theme_minimal())




# EDA examples 

# histogram for quantitative var
ggplot(data = penguins, aes(x=flipper_length_mm)) + geom_histogram()

# comparing quantitative var (flipper length) with categorical var (species), with fill
ggplot(data = penguins, aes(x=flipper_length_mm, fill=species)) + geom_histogram()

# as above, but with facet wrap
ggplot(data = penguins, aes(x=flipper_length_mm)) + geom_histogram() + facet_wrap(vars(species))

# boxplot
penguins %>% ggplot( aes(x=flipper_length_mm, fill=species)) + geom_boxplot()

# scatterplot, 2 quantitative vars
penguins %>% ggplot(aes(x=flipper_length_mm, y=bill_length_mm)) + geom_point()

# scatterplot, 2 quantitative vars
penguins %>% ggplot(aes(x=flipper_length_mm, y=bill_length_mm, color=species)) + geom_point()


# data.set.maria %>% ggplot() equivalent to: ggplot(data=data.set.maria)


