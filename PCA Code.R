# Select PCA Data
f=file.choose()
countries_data = read.table(f, header = TRUE, stringsAsFactors=FALSE)

# Prepare and assign names to data instances 
countries_names <- c("Albania", "Argentina", "Australia", "Austria", "Benin", "Bolivia", "Brazil",
                     "Cambodia", "China", "Colombia", "Croatia", "El Salvador", "France", "Greece",
                     "Guatemala", "Iran", "Italy", "Malawi", "Netherlands", "Pakistan", "Papua New Guinea",
                     "Peru", "Romania", "USA", "Zimbabwe")
rownames(countries_data) <- countries_names

#look at the variance to see if scaling needs to be done
countries_var <- apply(countries_data, 2, var) 
countries_var 

#var for GDP and IMR much higher than the others so we perform scaling  (mean=0 and var=1 for all p)
countries_sc_data <- scale(countries_data)

#euclidean distance for scaled data set
countires_sc_dist <- dist(countries_sc_data, method="euclidean") 

#perform MDS for scaled data where we have 1 dimension
countries_sc_mds1 <- cmdscale(countires_sc_dist, k=1)
countries_sc_mds1 <- -countries_sc_mds1
plot(cbind(sort(countries_sc_mds1),rep(0,25)), main="MDS with 1 Dimension", ylab="",xlab="Distance",yaxt="n",frame.plot = FALSE)
lines(cbind(sort(countries_sc_mds1),rep(0,25)),type = "line")

#perform MDS for scaled data where we have 2 dimensions
countries_sc_mds2 <- cmdscale(countires_sc_dist, k=2)

# rotate second dimension 
countries_sc_mds2 <- -countries_sc_mds2
plot(countries_sc_mds2, xlab="Dimensions 1", ylab="Dimensions 2", main="MDS with 2 Dimensions")
text(countries_sc_mds2, countries_names, cex=1)

# Comparison with PCA
pr.out = prcomp(countries_data, scale = TRUE) 
biplot(pr.out, scale=0)

#stress test for 1 and 2 dimensional MDS 
stress_mds1 <- cmdscale(countires_sc_dist , k=1, eig=TRUE)$GOF[1] 
stress_mds2 <- cmdscale(countires_sc_dist , k=2, eig=TRUE)$GOF[1] 
