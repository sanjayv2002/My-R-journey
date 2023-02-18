# T - test on iris petal Lengths
# Sanjay Kumar V
# sanjay.murthy.29@gmail.com
# 2023-02-18

# Comparing setosa and versicolor

setosa <- iris[iris$Species == "setosa",]
versicolor <- iris[iris$Species == "versicolor",]

# Comparing Petal.Length of these two species
t.test(x = setosa$Petal.Length, y = versicolor$Petal.Length)

#Challenge 1
# lets take virginica

virginica <- iris[iris$Species == "virginica",]

#comaparing petal lengths of setosa and virginica
t.test(x = setosa$Petal.Length, y = virginica$Petal.Length)

#Comparing Petal Length of versicolor and virginica
t.test(x = versicolor$Petal.Length, y = virginica$Petal.Length)

