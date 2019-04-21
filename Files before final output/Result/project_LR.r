setwd("C:/Users/Helios/Desktop/Visual Analytics/Project")
rm(list=ls())

# Load the data.
raw <- read.csv('data_6037.csv')

# Drop the X column.
raw <- raw[,2:19]

# Drop redundant geolocation columns (we're only using zipcode for this basic model).
drops <- c('latitude', 'longitude', 'rawcensustractandblock', 'regionidcity', 'censustractandblock')
raw <- raw[ , !(names(raw) %in% drops)]

# Define the dummy variables.

# Since linear regression relies on *unique* response values for
# each predictor X, we have to use dummy variables.  Here, we will
# simply make all predictors categorical.

#cols <- colnames(raw)

# Don't make the predictor categorical.
#cols <- cols[cols != 'logerror']
#raw[,cols] <- lapply(raw[,cols], factor)

raw <- unique(raw)



# Now develop the simple model.

# Create 10 different samples of size 1000, without replacement.

# How many records are there?
n_records = length(raw[,1])

# Create the indices to draw from, but make sure
# to not draw the same record twice.

sample_indices <- list()
available_records <- c(1:n_records)

for(i in 1:10) {
	sample_indices[[i]] <- sample(available_records, 1000, replace=F)
	available_records <- setdiff(available_records, sample_indices)
}

# Now fit the model with each set of random indices.

# Create a list of lists to hold the coefficients.

list_names <- colnames(raw)
list_names <- setdiff(list_names, 'logerror')
coefficients <- list()
coefficients[['intercept']] <- FALSE

# Initialize the coefficients list.
for(name in list_names) {
	coefficients[[name]] <- FALSE
}

for(i in 1:10) {

	# What is the data set (data frame coercion is required to use lm)?
	data_set <- as.data.frame(raw[sample_indices[[i]],])

	fit.simple <- lm(logerror ~ bathroomcnt + bedroomcnt + buildingqualitytypeid + calculatedfinishedsquarefeet + heatingorsystemtypeid + lotsizesquarefeet + propertylandusetypeid + regionidzip + unitcnt + yearbuilt + structuretaxvaluedollarcnt + assessmentyear, data = data_set)
	
	# Add the coefficients to their respective lists.
	intercept_helper <- fit.simple$coefficients['(Intercept)']
	names(intercept_helper) <- NULL
	coefficients[['intercept']] <- c(coefficients[['intercept']], intercept_helper)
	for(j in list_names) {
		coefficients[[j]] <- c(coefficients[[j]], fit.simple$coefficients[[j]])
	}
}

# Update the list of variables.
list_names <- c('intercept', list_names)

# Remove the first element of each coefficient since it was just a placeholder.
for(name in list_names) {
	coefficients[[name]] <- coefficients[[name]][-1]
}

# Now take the median value for each coefficient.
for(name in list_names) {
	coefficients[[name]] <- median(coefficients[[name]])
}

# Residual Analysis

# Print each plot of the residuals.
#for(name in list_names) {
#	par(mfrow = c(1,1))
#	plot(name,residuals(fit.simple), xlab=name, ylab="Residuals")
#	abline(h=0,col="red")
#	readline()
#}

par(mfrow = c(2,1))
hist(residuals(fit.simple),xlab="Residuals",main="Histogram of residuals",col="blue")
qqnorm(residuals(fit.simple))
qqline(residuals(fit.simple),col="blue")












