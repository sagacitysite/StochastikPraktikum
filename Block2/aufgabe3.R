#################### Initialization ####################

# Load package
library(EBImage)

# Load image from parent directory
imgLenaColor <- readImage("lena.png")

# Change image to grayscale
imgLenaGray <- channel(imgLenaColor, "gray")

# Display images
par(mfrow = c(1,2))
display(imgLenaColor, method = "raster")
display(imgLenaGray, method = "raster")

#################### Add Noise ####################

fnAddNoise <- function(img, rnoise, ...) {
  # This function adds noise specified by rnoise to a grayscale image. If the
  # image is not grayscale, it gets converted to grayscale first.
  # 
  # Args:
  #   img:    Image
  #   rnoise: Random noise generation function (e.g. rnorm for Gaussian noise)
  #   ...:    Further arguments passed to or from other methods
  #   
  # Returns:
  #   Grayscale image with added noise
  
  # Check if grayscale and convert if necessary
  if(colorMode(img) != 0) {img <- channel(img, "gray")}
  
  # Add noise
  m <- dim(img)[1]
  p <- dim(img)[2]
  imgNoise <- Image(imageData(img) + matrix(rnoise(m*p, ...), m, p))
  
  # Adjust values below 0 and above 1
  imgNoise[imgNoise < 0] <- 0
  imgNoise[imgNoise > 1] <- 1
  
  return(imgNoise)
}


imgLenaNoise1 <- fnAddNoise(imgLenaGray, rnorm, sd = 0.1)
imgLenaNoise2 <- fnAddNoise(imgLenaGray, rnorm, sd = 0.25)
imgLenaNoise3 <- fnAddNoise(imgLenaGray, rnorm, sd = 0.5)

par(mfrow = c(1,3))
display(imgLenaNoise1, method = "raster")
display(imgLenaNoise2, method = "raster")
display(imgLenaNoise3, method = "raster")

#################### Nadaraya-Watson-Estimator implementation ####################

fnNadarayaWatson <- function(img, K, h) {
  # The Nadaraya-Watson-estimator with weights defined by kernel K and bandwidth
  # h for denoising/smoothing an image
  # 
  # Args:
  #   img:  Image to be denoised
  #   K:    Smoothing kernel used in weights
  #   h:    Bandwidth used in weights
  #   
  # Returns:
  #   Denoised/smoothed image
  
  # Check if grayscale and convert if necessary
  if(colorMode(img) != 0) {img <- channel(img, "gray")}
  
  # Get Y and dimensions
  Y <- imageData(img)
  m <- dim(img)[1]
  p <- dim(img)[2]
  
  # Generate M and N
  M <- K((1/h) * outer(1:m, 1:m, FUN = "-"))
  M <- M / rowSums(M)
  
  N <- K((1/h) * outer(1:p, 1:p, FUN = "-"))
  N <- N / rowSums(N)
  
  return(Image(M %*% Y %*% t(N)))
}

#################### Nadaraya-Watson-Estimator evaluation ####################

fnEvalNWGaussianNoise <- function(img, vsigma, vh) {
  # This function is a wrapper to compare the denoising results of images with 
  # different levels of Gaussian noise for the Nadaraya-Watson-estimator with
  # weights defined by the Gaussian kernel and rectangular kernel respectively 
  # for different bandwidths h.
  # 
  # Args:
  #   img:    Image
  #   vsigma: Vector of standard deviations used to add noise
  #   vh:     Vector of bandwidths used in weights of the
  #           Nadaraya-Watson-estimator
  #   
  # Returns:
  #   -
  
  # Define helper function for label implementation
  fnLabel <- function(label) {
    text(x = 20, y = 20, adj = c(0,1), col = "red", cex = 1.5, label = label)
  }
  
  # Set size of frame
  par(mfrow = c(2*length(vh)+1, length(vsigma)))
  
  # Initalize noise image list
  imagesNoise <- list()
  
  for (i in 1:length(vsigma)) {
    # Add noise to image and plot
    imgNoise <- fnAddNoise(img, rnorm, sd = vsigma[i])
    imagesNoise[i] <- list(imgNoise)
    display(imgNoise, method = "raster")
    fnLabel(substitute(paste(sigma, "=", sd), list(sd=vsigma[i])))
  }
  
  cat('Gaussian kernel')
  for (h in vh) {
    # NW-Denoising with Gaussian kernel
    for (i in 1:length(vsigma)) {
      display(fnNadarayaWatson(imagesNoise[[i]], fnGaussianKernel, h),
              method = "raster")
      fnLabel(substitute(paste(sigma, "=", sd, ", h=", h),
                         list(sd=vsigma[i], h=h)))
    }
  }
  
  cat('Rectangular kernel')
  for (h in vh) { 
    # NW-Denoising with rectangular kernel
    for (i in 1:length(vsigma)) {
      display(fnNadarayaWatson(imagesNoise[[i]], fnRectangularKernel, h), 
              method = "raster")
      fnLabel(substitute(paste(sigma, "=", sd, ", h=", h),
                         list(sd=vsigma[i], h=h)))
    }
  }
}

# Lena processing
sigma <- c(0.1, 0.25, 0.5)
h <- c(1, 3, 5, 50)

fnEvalNWGaussianNoise(imgLenaGray, sigma, h)

# Porsche processing
sigma <- c(0.1, 0.25, 0.5)
h <- c(1, 3, 5, 50)

imgPorsche <- readImage("porsche.jpeg")
fnEvalNWGaussianNoise(imgPorsche, sigma, h)

#################### Nadaraya-Watson-estimator evaluation non-Gaussian noise #################### 

fnEvalNWCauchyNoise <- function(img, vdf, vh) {
  # This function is a wrapper to compare the denoising results of images with 
  # different levels of t-distributed noise for the Nadaraya-Watson-estimator 
  # with weights defined by the Gaussian kernel and rectangular kernel
  # respectively for different bandwidths h.
  # 
  # Args:
  #   img:  Image
  #   vdf:  Vector of degrees of freedom used to add noise
  #   vh:   Vector of bandwidths used in weights of the
  #         Nadaraya-Watson-estimator
  #   
  # Returns:
  #   -
  
  # Define helper function for label implementation
  fnLabel <- function(label) {
    text(x = 20, y = 20, adj = c(0,1), col = "red", cex = 1.5, label = label)
  }
  
  # Set size of frame
  par(mfrow = c(2*length(vh)+1, length(vdf)))
  
  # Initalize noise image list
  imagesNoise <- list()
  
  for (i in 1:length(vdf)) {
    # Add noise to image and plot
    imgNoise <- fnAddNoise(img, rt, df = vdf[i])
    imagesNoise[i] <- list(imgNoise)
    display(imgNoise, method = "raster")
    fnLabel(substitute(paste("df=", df), list(df=vdf[i])))
  }
  
  cat('Gaussian kernel')
  for (h in vh) {
    # NW-Denoising with Gaussian kernel
    for (i in 1:length(vdf)) {
      display(fnNadarayaWatson(imagesNoise[[i]], fnGaussianKernel, h),
              method = "raster")
      fnLabel(substitute(paste("df=", df, ", h=", h),
                         list(df=vdf[i], h=h)))
    }
  }
  
  cat('Rectangular kernel')
  for (h in vh) { 
    # NW-Denoising with rectangular kernel
    for (i in 1:length(vdf)) {
      display(fnNadarayaWatson(imagesNoise[[i]], fnRectangularKernel, h), 
              method = "raster")
      fnLabel(substitute(paste("df=", df, ", h=", h),
                         list(df=vdf[i], h=h)))
    }
  }
}

# Lena processing (non-Gaussian Noise)
df <- c(100, 50, 1)
h <- c(1, 3, 5, 50)

fnEvalNWCauchyNoise(imgLenaGray, df, h)
