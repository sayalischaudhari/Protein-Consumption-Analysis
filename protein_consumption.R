#Protein-Consumption-Analysis

#Loading the requited dataset
protein_consumption <- read.csv("C:/Users/sayal/Downloads/Protein_Consumption.csv",row.names = 1)
#Print the data on console
head(protein_consumption)
#Printing the dimension of data to console
dim(protein_consumption)


#Principal components analysis

#Applying PCA function on the dataset
protein_pca <- prcomp(protein_consumption, scale=TRUE)
#Printing the results of pca to console
protein_pca
#Printing the summary of the pca to console
summary(protein_pca)


#We get from summary the std deviation, Proportion of Variance and the cummulative variance.
#In order to find the eigen values we need to square the std deviations. Which is done as below

#Storing and Printing the eigen values on the console
eigen_protien <- protein_pca$sdev^2 
eigen_protien
#Assigning names of PC to the values of PCA
names(eigen_protien) <- paste("PC",1:10,sep="")
#Printing the sum of eigen values to console
sum(eigen_protien)


#Visualizing the results of PCA

#Importing the required libraries
library(factoextra)
#Printing the Scree plot of PCA to console
fviz_eig(protein_pca)


#In order to retain the values of Principal Components we have 2 approaches take in account the values whose eigen values are greater than 0.7 or we can take components who account for 70-90% variablility.
#Here , in our case if we take 0.7 as threshold for eigen values we get first four components


#Printing the names of the eigenvalues who crosses 0.7 threshold value
names(eigen_protien[eigen_protien>0.7])
#Printing the summary of PCA to concole
summary(protein_pca)

#If we look at the summary we know that first four principal components accounts for roughly 82% of the total variance. Thus, we can consider our first four components as our Principal components.
#Now we predict the value of these components using predict function

# Constructing the new dataframe with 4 Principal components and output variable
new_protien <- predict(protein_pca)[,1:4]
#Changing the row names of the 
row.names(new_protien) <- row.names(protein_consumption)
#Printing the head of the data
head(new_protien)


#Cluster Analysis (Using Principal Component Analysis):
  
#Agglomerative Clustering Using Single, Complete and average Linkage 

#1.Single Linkage

#Calculating distance Matrix for the PRincipal components
dist.protien <- dist(new_protien,method='euclidean')
#Calculating Agglomerative  Clustering using single linkage
clustprotien.nn <- hclust(dist.protien, method = "single")
#Plotting the Agglomeratuve Clustering
plot(clustprotien.nn,hang=-1,xlab="Object",ylab="Distance between Protien_values",main="Dendrogram of Countries using Single Linkage")


#2.Complete Linkage


#Calculating Agglomerative  Clustering using single linkage
clustprotien.nn <- hclust(dist.protien, method = "complete")
#Plotting the Agglomeratuve Clustering
plot(clustprotien.nn,hang=-1,xlab="Object",ylab="Distance between Protien_values",main="Dendrogram of Countries using Complete Linkage")

#3. Average Linkage

#Calculating Agglomerative  Clustering using Average linkage i.e calculating the average distance
clustprotien.nn <- hclust(dist.protien, method = "average")
#Plotting the Agglomeratuve Clustering
plot(clustprotien.nn,hang=-1,xlab="Object",ylab="Distance between Protien_values",main="Dendrogram of Countries using Average Linkage")

#We get the dendograms for single linkage, complete linkage and average linkage as above. We can Thus group the protien consumption by countries as shown above. If the data is large it is not advised to use agglomerative clustering instead we use k-means clustering.


#Q3.Identify the important factors underlying the observed variables and examine the relationships between the countries with respect to these factors

#Ans: In order to solve this we need to use the original data

#Printing the head of data to console
head(protein_consumption)
#Loading the required library
library(psych)
#Applying Factor Analysis on the data with 4 factors
fit.pc <- principal(protein_consumption,nfactors = 4, rotate = "varimax")
#Printing the results of Factor Analysis
fit.pc
#rounding the values to 3 decimal places
round(fit.pc$values, 3)
#Printing the loading data to console for the 
fit.pc$loadings


#Now we look at the cummunality
fit.pc$communality
#Printing the scores
fit.pc$scores
# See Correlations within Factors
fa.plot(fit.pc) 
#Visualize the relationship
fa.diagram(fit.pc)
#Visualizing the data
vss(protein_consumption)
