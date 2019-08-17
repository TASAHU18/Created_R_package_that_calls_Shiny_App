# Created an R-package that calls up the Shiny App
## Description of the dataset
Dataset is divided into training and test sets
The data consists of 5 variables:
1. Height: A numerical value
2. Weight: A numerical value
3. Age: A numerical value
4. Male: A binary value. 0 indicates female and 1 indicates male
5. Gender: Mapping to factor of 4

## Tasks performed
1. Developed a function that plots the Age, Height and Weight of the training data
color coded according to Gender
2. Built models using K-means and Gaussian Mixture algorithm and plotted 3D visualizations using plotly - Age, Height and Weight, color coded according to cluster
3. Utilized nbclust library to find the optimal number of clusters for K-means
4. Developed functions to compare both the models on the basis of accuracy
5. Implemented K-Nearest Neighbors after clustering for class predictions of test data in each cluster and achieved an overall accuracy of around 80% 

## Author
_Takesh Sahu_
