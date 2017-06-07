# GUI tool for randomized PCA method

This is a very simple GUI tool (based on R and Shiny), which allows to get a user experience with randomized PCA methods being applied for exploratory analysis of hyperspectral images. The tool is made as a supplementary material for a corresponding paper to be published in Journal of Chemometrics (in press). More information can be also found in original paper described a general approach for using randomness in computing low-rank approximation of data: (https://arxiv.org/abs/0909.4061).

So far the tool has quite limited possibilities and mostly aims at comparing different methods and parameters from user experience point of view (brushing, background removal, etc.).

## Requirements

You need to have [R](https://www.r-project.org) and [RStudio desktop](https://www.rstudio.com/products/RStudio/#Desktop) installed. RStudio is not necessary but is highly recommended as it makes a lot of things much easier.
Besides that, the following packages are required:

* `shiny`
* `shinydashboard`
* `hexbin`

## How to use

1. Download a ZIP with the latest releaze
2. Unzip it to any suitable folder (e.g. `Desktop`)
3. Open the unzipped folder in *RStudio*
4. Set the open folder as working directory
5. Open file *server.R* and click on `Run app` button

## Details

By default the app loads a hyperspectral image from file `image.rds` located to the same folder as files for the app. The image is available in the file (293 Gb) and corresponds to the case B from the paper.

You can also use your own image, it has to be saved in the same file (same name, same path) and contain the hyperspectral data as a 2D matrix with rows corresponding to image pixels and columns  corresponding to the variables (wavelength, wavenumbers, etc). The matrix should have two mandatory attributes:

* `width` — width of image in pixels
* `height` — height of the image in pixels

The product of values for the two arguments should correspond to the number of rows in the matrix. Other optional attributes are:

* `xaxis.values` — optional values for x-axis (e.g. wavelength in nm or wavenumbers in reciprocal cm)
* `xaxis.names` — optional name for x-axis (e.g. "Wavelength, nm" or "Wavenumbers, cm-1")


