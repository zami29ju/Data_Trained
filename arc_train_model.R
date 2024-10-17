
rm(list=ls())
library(Momocs)

## setting file name in the R directory
images_folder <- "train_data//grain_images"
image_filenames <- list.files (images_folder, full.names = T)

## Importing data
data <- import_jpg(image_filenames, threshold = 0.9)

## Setting data into R prescribed format
data <- Out(data)
data

## Checking data whether it imported properly
panel(data,name=TRUE)
stack(data)

## Importing additional file for image description; It can be done in two ways
image_description_csv_file_path <- list.files('train_data//CSV_file', full.names = T)
modern_rice <- read.csv(image_description_csv_file_path)

#Checking data table structure whether it uploads properly
str(modern_rice)
modern_rice

#adding csv. file to the  momocs main data file; data=main data file of jpeg; modern_rice=imported+
#csv. file
data$fac <- modern_rice
##Checking final data file
data

## Filtering all dorsal/lateral/shape (to efourier and then PCA and LDA)
modernOut.l <- data %>% filter(View=="Lateral")
modernOut.l%>% coo_center() %>% coo_align() %>% coo_scale() %>%
  coo_slidedirection("up") %>% stack()
#Effourier transformation by Lateral/Dorsal/Polar
modernOut.l.fourier<- efourier(modernOut.l, nb.h =8,norm=FALSE, start = TRUE)

#-------------------------------------------------------------------------------
## PCA

##PCA by L
modernOut.pca.l<- PCA(modernOut.l.fourier)
plot_PCA(modernOut.pca.l, ~G..Variety)

##LDA by L
modernOut.l.lda <- LDA(modernOut.pca.l, 2)

# plot the cross-validation table      # tabular version
plot_CV(modernOut.l.lda)                      
plot_LDA(modernOut.l.lda)
modernOut.l.lda
?LDA_files
#--------------------------------------------------------------------------------------------


# plot_PCA(modernOut.pca.l, ~G..Variety)
saveRDS(modernOut.pca.l, "Outputs//trained_L_PCA.rds")

#LDA by Lateral view;2= column no of data file on which based on we want to make LDA, here the column is G. variety.
modernOut.l.lda <- LDA(modernOut.pca.l, 2) 
saveRDS(modernOut.l.lda, "Outputs//trained_L_LDA.rds")

# plot the cross-validation table      # tabular version
plot_CV(modernOut.l.lda)                      
plot_LDA(modernOut.l.lda)
modernOut.l.lda



# # TESTING
# train_PCA <- readRDS("trained_L_PCA.rds")
# train_LDA <- readRDS("trained_L_LDA.rds")
# 
# # we redo PCA with saved train_PCA
# test_PCA <- rePCA(train_PCA, modernOut.l.fourier)
# 
# # then we perform reLDA on test_PCA data using train_LDA
# test_LDA <- reLDA(test_PCA, train_LDA)
# test_LDA$class
