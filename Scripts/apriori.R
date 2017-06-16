library(arules)

dataset = read.transactions("Data/Market_Basket_Optimisation.csv", sep = ",",
                            rm.duplicates = TRUE)

summary(dataset)
itemFrequencyPlot(dataset, topN=100)

# Training Apriori on the dataset
rules = apriori(dataset, parameter = list(support = 0.004, confidence = 0.2))

# Visualizing the rules
inspect(sort(rules, by = "lift")[1:10])

library(arulesViz)
plot(rules, method = "graph")
