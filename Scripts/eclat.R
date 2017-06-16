#Eclat

library(arules)

dataset = read.transactions("Data/Market_Basket_Optimisation.csv", sep = ",",
                            rm.duplicates = TRUE)

summary(dataset)
itemFrequencyPlot(dataset, topN=100)

# Training Eclat on the dataset
rules = eclat(dataset, parameter = list(support = 0.004, minlen = 2))

# Visualizing the rules
inspect(sort(rules, by = "support")[1:10])

library(arulesViz)
plot(rules, method = "graph", interactive = TRUE)
