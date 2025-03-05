#install packages
install.packages("Boruta")
install.packages("randomForest")
install.packages("ggplot2")
install.packages("PerformanceAnalytics")
install.packages("GGally")
install.packages("reshape2")
install.packages("plotly")
install.packages("igraph")



library(Boruta)
library(randomForest)
library(ggplot2)
library(PerformanceAnalytics)
library(GGally)
library(ggplot2)
library(reshape2)
library(plotly)

#load df
df <- read.csv("C:/Users/pemba/Documents/Amazon_Projects/dea_project/mock_dataset.csv")


#Convert categorical var's into factors
df$shift <- as.factor (df$shift)


#Define formula and run Boruta

set.seed(123)
boruta_result <- Boruta::Boruta(utr_controllable ~ ., data = df , doTrace = 3)   

print (boruta_result)


#Plott boruta importance score
plot(boruta_result, las = 2, cex.axis = 0.7)


#Final features

boruta_final <- Boruta::TentativeRoughFix(boruta_result)

important_features <- Boruta::getSelectedAttributes(boruta_final, withTentative = FALSE)
print(important_features)


# Visualize results
final_plot <- plot(boruta_final, las = 2, cex.axis = 0.7)



#------------------------------------------------



#Finding intershift diffrences using boruta

# Split the data by shift
shift_groups <- split(df, df$shift)

# For shift 'bhn'
shift_bhn <- shift_groups[["bhn"]]
boruta_bhn <- Boruta(utr_controllable ~ ., data = shift_bhn, doTrace = 2)
plot(boruta_bhn, main = "Feature Importance for Shift: bhn")

# For shift 'fhn'
shift_fhn <- shift_groups[["fhn"]]
boruta_fhn <- Boruta(utr_controllable ~ ., data = shift_fhn, doTrace = 2)
plot(boruta_fhn, main = "Feature Importance for Shift: fhn")

# For shift 'bhd'
shift_bhd <- shift_groups[["bhd"]]
boruta_bhd <- Boruta(utr_controllable ~ ., data = shift_bhd, doTrace = 2)
plot(boruta_bhd, main = "Feature Importance for Shift: bhd")

# For shift 'fhd'
shift_fhd <- shift_groups[["fhd"]]
boruta_fhd <- Boruta(utr_controllable ~ ., data = shift_fhd, doTrace = 2)
plot(boruta_fhd, main = "Feature Importance for Shift: fhd")
  
  


#---------------------------------------------------------------------------------------------------------------------



#Create a correlation matrix for each shift



#bhn#

# Subset data for shift 'bhn'
shift_bhn <- subset(df, shift == "bhn")

# Select numeric columns for correlation
numeric_bhn <- shift_bhn[, sapply(shift_bhn, is.numeric)]

# Calculate correlation matrix
cor_matrix_bhn <- cor(numeric_bhn, use = "complete.obs")

# Print correlation matrix
cat("Correlation Matrix for Shift: bhn\n")
print(cor_matrix_bhn)








# Select specific columns for correlation
selected_columns <- c("dispatch_tph", "outbound_tph", "inbound_tph", "building_tph", "utr_controllable")
cor_data_bhn <- shift_bhn[, selected_columns]

# Calculate correlation matrix
cor_matrix_bhn <- cor(cor_data_bhn, use = "complete.obs")

# Print correlation matrix
cat("Correlation Matrix for Shift: bhn\n")
print(cor_matrix_bhn)

# Plot correlation matrix
chart.Correlation(cor_data_bhn, histogram = TRUE, pch = 19, main = "Correlation Matrix for Shift: bhn")


# Create a pair plot
ggpairs(shift_bhn[, c("dispatch_tph", "outbound_tph", "inbound_tph", "building_tph", "utr_controllable")],
        title = "Pairwise Scatterplots of Selected Variables")





#heat map

# Calculate correlation matrix
cor_matrix <- cor(shift_bhn[, c("dispatch_tph", "outbound_tph", "inbound_tph", "building_tph", "utr_controllable")], use = "complete.obs")

# Melt the correlation matrix for ggplot
melted_cor_matrix <- melt(cor_matrix)

# Plot heatmap
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen", midpoint = 0) +
  labs(title = "Heatmap of Correlation Matrix", x = "", y = "") +
  theme_minimal()




#3d scatterplots

# Create a 3D scatterplot
plot_ly(shift_bhn, x = ~dispatch_tph, y = ~outbound_tph, z = ~utr_controllable, color = ~utr_controllable, colors = c("blue", "red")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = "Dispatch TPH"),
                      yaxis = list(title = "Outbound TPH"),
                      zaxis = list(title = "UTR Controllable")),
         title = "3D Scatterplot: Dispatch TPH, Outbound TPH, and UTR Controllable")



#regression plot
# Load necessary library
library(ggplot2)

# Scatterplot with regression line for dispatch_tph vs utr_controllable
ggplot(shift_bhn, aes(x = dispatch_tph, y = utr_controllable)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regression Plot: UTR Controllable vs Dispatch TPH",
       x = "Dispatch TPH", y = "UTR Controllable") +
  theme_minimal()


# Scatterplot with regression line for outbound_tph vs utr_controllable

ggplot(shift_bhn, aes(x = outbound_tph, y = utr_controllable)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regression Plot: UTR Controllable vs Dispatch TPH",
       x = "Dispatch TPH", y = "UTR Controllable") +
  theme_minimal()


# Scatterplot with regression line for inbound_tph vs utr_controllable

ggplot(shift_bhn, aes(x = inbound_tph, y = utr_controllable)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regression Plot: UTR Controllable vs Dispatch TPH",
       x = "Dispatch TPH", y = "UTR Controllable") +
  theme_minimal()



# Scatterplot with regression line for building_tph vs utr_controllable

ggplot(shift_bhn, aes(x = building_tph, y = utr_controllable)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regression Plot: UTR Controllable vs Dispatch TPH",
       x = "Dispatch TPH", y = "UTR Controllable") +
  theme_minimal()






#-----------------------------------------------------------------------------



#fhn#

# Subset data for shift 'fhn'
shift_fhn <- subset(df, shift == "fhn")

# Select numeric columns for correlation
numeric_fhn <- shift_fhn[, sapply(shift_fhn, is.numeric)]

# Calculate correlation matrix
cor_matrix_fhn <- cor(numeric_fhn, use = "complete.obs")

# Print correlation matrix
cat("Correlation Matrix for Shift: fhn\n")
print(cor_matrix_fhn)








# Select specific columns for correlation
selected_columns <- c("dispatch_tph", "outbound_tph", "inbound_tph", "building_tph", "utr_controllable")
cor_data_fhn <- shift_fhn[, selected_columns]

# Calculate correlation matrix
cor_matrix_fhn <- cor(cor_data_fhn, use = "complete.obs")

# Print correlation matrix
cat("Correlation Matrix for Shift: fhn\n")
print(cor_matrix_fhn)

# Plot correlation matrix
chart.Correlation(cor_data_fhn, histogram = TRUE, pch = 19, main = "Correlation Matrix for Shift: fhn")


# Create a pair plot
ggpairs(shift_fhn[, c("dispatch_tph", "outbound_tph", "inbound_tph", "building_tph", "utr_controllable")],
        title = "Pairwise Scatterplots of Selected Variables")





#heat map

# Calculate correlation matrix
cor_matrix <- cor(shift_fhn[, c("dispatch_tph", "outbound_tph", "inbound_tph", "building_tph", "utr_controllable")], use = "complete.obs")

# Melt the correlation matrix for ggplot
melted_cor_matrix <- melt(cor_matrix)

# Plot heatmap
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen", midpoint = 0) +
  labs(title = "Heatmap of Correlation Matrix", x = "", y = "") +
  theme_minimal()




#3d scatterplots

# Create a 3D scatterplot
plot_ly(shift_fhn, x = ~dispatch_tph, y = ~outbound_tph, z = ~utr_controllable, color = ~utr_controllable, colors = c("blue", "red")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = "Dispatch TPH"),
                      yaxis = list(title = "Outbound TPH"),
                      zaxis = list(title = "UTR Controllable")),
         title = "3D Scatterplot: Dispatch TPH, Outbound TPH, and UTR Controllable")



#regression plot
# Load necessary library
library(ggplot2)

# Scatterplot with regression line for dispatch_tph vs utr_controllable
ggplot(shift_fhn, aes(x = dispatch_tph, y = utr_controllable)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regression Plot: UTR Controllable vs Dispatch TPH",
       x = "Dispatch TPH", y = "UTR Controllable") +
  theme_minimal()


# Scatterplot with regression line for outbound_tph vs utr_controllable

ggplot(shift_fhn, aes(x = outbound_tph, y = utr_controllable)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regression Plot: UTR Controllable vs Dispatch TPH",
       x = "Dispatch TPH", y = "UTR Controllable") +
  theme_minimal()


# Scatterplot with regression line for inbound_tph vs utr_controllable

ggplot(shift_fhn, aes(x = inbound_tph, y = utr_controllable)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regression Plot: UTR Controllable vs Dispatch TPH",
       x = "Dispatch TPH", y = "UTR Controllable") +
  theme_minimal()



# Scatterplot with regression line for building_tph vs utr_controllable

ggplot(shift_fhn, aes(x = building_tph, y = utr_controllable)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regression Plot: UTR Controllable vs Dispatch TPH",
       x = "Dispatch TPH", y = "UTR Controllable") +
  theme_minimal()



#--------------------------------------------------------------------------------



#bhd#

# Subset data for shift 'bhd'
shift_bhd <- subset(df, shift == "bhd")

# Select numeric columns for correlation
numeric_bhd <- shift_bhd[, sapply(shift_bhd, is.numeric)]

# Calculate correlation matrix
cor_matrix_bhd <- cor(numeric_bhd, use = "complete.obs")

# Print correlation matrix
cat("Correlation Matrix for Shift: bhd\n")
print(cor_matrix_bhd)








# Select specific columns for correlation
selected_columns <- c("dispatch_tph", "outbound_tph", "inbound_tph", "building_tph", "utr_controllable")
cor_data_bhd <- shift_bhd[, selected_columns]

# Calculate correlation matrix
cor_matrix_bhd <- cor(cor_data_bhd, use = "complete.obs")

# Print correlation matrix
cat("Correlation Matrix for Shift: bhd\n")
print(cor_matrix_bhd)

# Plot correlation matrix
chart.Correlation(cor_data_bhd, histogram = TRUE, pch = 19, main = "Correlation Matrix for Shift: bhd")


# Create a pair plot
ggpairs(shift_bhd[, c("dispatch_tph", "outbound_tph", "inbound_tph", "building_tph", "utr_controllable")],
        title = "Pairwise Scatterplots of Selected Variables")





#heat map

# Calculate correlation matrix
cor_matrix <- cor(shift_bhd[, c("dispatch_tph", "outbound_tph", "inbound_tph", "building_tph", "utr_controllable")], use = "complete.obs")

# Melt the correlation matrix for ggplot
melted_cor_matrix <- melt(cor_matrix)

# Plot heatmap
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen", midpoint = 0) +
  labs(title = "Heatmap of Correlation Matrix", x = "", y = "") +
  theme_minimal()




#3d scatterplots

# Create a 3D scatterplot
plot_ly(shift_bhd, x = ~dispatch_tph, y = ~outbound_tph, z = ~utr_controllable, color = ~utr_controllable, colors = c("blue", "red")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = "Dispatch TPH"),
                      yaxis = list(title = "Outbound TPH"),
                      zaxis = list(title = "UTR Controllable")),
         title = "3D Scatterplot: Dispatch TPH, Outbound TPH, and UTR Controllable")



#regression plot
# Load necessary library
library(ggplot2)

# Scatterplot with regression line for dispatch_tph vs utr_controllable
ggplot(shift_bhd, aes(x = dispatch_tph, y = utr_controllable)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regression Plot: UTR Controllable vs Dispatch TPH",
       x = "Dispatch TPH", y = "UTR Controllable") +
  theme_minimal()


# Scatterplot with regression line for outbound_tph vs utr_controllable

ggplot(shift_bhd, aes(x = outbound_tph, y = utr_controllable)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regression Plot: UTR Controllable vs Dispatch TPH",
       x = "Dispatch TPH", y = "UTR Controllable") +
  theme_minimal()


# Scatterplot with regression line for inbound_tph vs utr_controllable

ggplot(shift_bhd, aes(x = inbound_tph, y = utr_controllable)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regression Plot: UTR Controllable vs Dispatch TPH",
       x = "Dispatch TPH", y = "UTR Controllable") +
  theme_minimal()



# Scatterplot with regression line for building_tph vs utr_controllable

ggplot(shift_bhd, aes(x = building_tph, y = utr_controllable)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regression Plot: UTR Controllable vs Dispatch TPH",
       x = "Dispatch TPH", y = "UTR Controllable") +
  theme_minimal()




#---------------------------------------------------------------------------



#fhd#

# Subset data for shift 'fhd'
shift_fhd <- subset(df, shift == "fhd")

# Select numeric columns for correlation
numeric_fhd <- shift_fhd[, sapply(shift_fhd, is.numeric)]

# Calculate correlation matrix
cor_matrix_fhd <- cor(numeric_fhd, use = "complete.obs")

# Print correlation matrix
cat("Correlation Matrix for Shift: fhd\n")
print(cor_matrix_fhd)








# Select specific columns for correlation
selected_columns <- c("dispatch_tph", "outbound_tph", "inbound_tph", "building_tph", "utr_controllable")
cor_data_fhd <- shift_fhd[, selected_columns]

# Calculate correlation matrix
cor_matrix_fhd <- cor(cor_data_fhd, use = "complete.obs")

# Print correlation matrix
cat("Correlation Matrix for Shift: fhd\n")
print(cor_matrix_fhd)

# Plot correlation matrix
chart.Correlation(cor_data_fhd, histogram = TRUE, pch = 19, main = "Correlation Matrix for Shift: fhd")


# Create a pair plot
ggpairs(shift_fhd[, c("dispatch_tph", "outbound_tph", "inbound_tph", "building_tph", "utr_controllable")],
        title = "Pairwise Scatterplots of Selected Variables")





#heat map

# Calculate correlation matrix
cor_matrix <- cor(shift_fhd[, c("dispatch_tph", "outbound_tph", "inbound_tph", "building_tph", "utr_controllable")], use = "complete.obs")

# Melt the correlation matrix for ggplot
melted_cor_matrix <- melt(cor_matrix)

# Plot heatmap
ggplot(melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkred", mid = "white", high = "darkgreen", midpoint = 0) +
  labs(title = "Heatmap of Correlation Matrix", x = "", y = "") +
  theme_minimal()




#3d scatterplots

# Create a 3D scatterplot
plot_ly(shift_fhd, x = ~dispatch_tph, y = ~outbound_tph, z = ~utr_controllable, color = ~utr_controllable, colors = c("blue", "red")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = "Dispatch TPH"),
                      yaxis = list(title = "Outbound TPH"),
                      zaxis = list(title = "UTR Controllable")),
         title = "3D Scatterplot: Dispatch TPH, Outbound TPH, and UTR Controllable")



#regression plot
# Load necessary library
library(ggplot2)

# Scatterplot with regression line for dispatch_tph vs utr_controllable
ggplot(shift_fhd, aes(x = dispatch_tph, y = utr_controllable)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regression Plot: UTR Controllable vs Dispatch TPH",
       x = "Dispatch TPH", y = "UTR Controllable") +
  theme_minimal()


# Scatterplot with regression line for outbound_tph vs utr_controllable

ggplot(shift_fhd, aes(x = outbound_tph, y = utr_controllable)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regression Plot: UTR Controllable vs Dispatch TPH",
       x = "Dispatch TPH", y = "UTR Controllable") +
  theme_minimal()


# Scatterplot with regression line for inbound_tph vs utr_controllable

ggplot(shift_fhd, aes(x = inbound_tph, y = utr_controllable)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regression Plot: UTR Controllable vs Dispatch TPH",
       x = "Dispatch TPH", y = "UTR Controllable") +
  theme_minimal()



# Scatterplot with regression line for building_tph vs utr_controllable

ggplot(shift_fhd, aes(x = building_tph, y = utr_controllable)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Regression Plot: UTR Controllable vs Dispatch TPH",
       x = "Dispatch TPH", y = "UTR Controllable") +
  theme_minimal()




