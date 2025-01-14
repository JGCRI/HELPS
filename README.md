# HELPS
<br />

<!-------------------------->
<!-------------------------->
# <a name="Contents"></a>Contents
<!-------------------------->
<!-------------------------->

- [Key Links](#KeyLinks)
- [Introduction](#Introduction)
- [Citation](#Citation)
- [Installation Guide](#InstallGuides)
- [How-to Guides](#How-toGuides)
- [User Notice](#UserNotice)

<br />

<!-------------------------->
<!-------------------------->
# <a name="KeyLinks"></a>Key Links
<!-------------------------->
<!-------------------------->

- Github: https://github.com/JGCRI/HELPS
- Webpage: https://jgcri.github.io/HELPS/

[Back to Contents](#Contents)

<br />

<!-------------------------->
<!-------------------------->
# <a name="Introduction"></a>Introduction
<!-------------------------->
<!-------------------------->

`HELPS` is designed to translate climate projections to heat-induced labor productivity loss by sectors.


![Figure. 1. HELPS package schematic. HELPS can process daily and monthly 0.5-degree grid-level input data. Stars denote package functions.](vignettes/Schematic.jpg)

[Back to Contents](#Contents)

<br />

<!-------------------------->
<!-------------------------->
# <a name="RelatedPublications"></a>Related Publications
<!-------------------------->
<!-------------------------->

Sheng, D. et al. Omitting labor responses to heat stress underestimates future climate impact on agriculture. (Under review) doi:https://doi.org/10.21203/rs.3.rs-5000229/v1.

[Back to Contents](#Contents)

<br />


<!-------------------------->
<!-------------------------->
# <a name="InstallationGuides"></a>Installation Guides
<!-------------------------->
<!-------------------------->

1. Download and install:

    - R (https://www.r-project.org/)
    - R studio (https://www.rstudio.com/)
    
2. Open R studio:

```
install.packages('devtools')
devtools::install_github('dsheng1026/HELPS')
renv::init()
renv::restore()
devtools::load_all()
```
renv::restore() helps to install package dependencies for `HELPS`, but users might need to download a few R packages individually through RStudio's guidance.
You should now be set to run the driver without running into any package version issues. Note that if you have completed steps related to renv once, your R session should automatically connect to a private library when you open `HELPS.Rproj`, and you can run the code below to use the package.

```
devtools::load_all()
```

[Back to Contents](#Contents)

<br />


<!-------------------------->
<!-------------------------->
# <a name="How-toGuides"></a>How-to Guides
<!-------------------------->
<!-------------------------->



- Please explore the package vignette (https://jgcri.github.io/HELPS/articles/HELPS-vignette.html) to use the package.

[Back to Contents](#Contents)

<br />

<!-------------------------->
<!-------------------------->
# <a name="UserNotice"></a>User Notice
<!-------------------------->
<!-------------------------->

The `HELPS` package operates on 0.5 degree resolution, bias-corrected outputs from Earth System Models and General Circulation Models participating in the CMIP process. Several options to access such data exist:
- The ISIMIP2b repository (https://data.isimip.org/search/tree/ISIMIP2b/InputData/climate/atmosphere/) contains outputs from CMIP5-era models (GFDL-ESM2M, HadGEM2-ES, IPSL-CM5A-LR, and MIROC5) for specific scenarios
- The ISIMIP3b repository (https://data.isimip.org/search/tree/ISIMIP3b/InputData/climate/atmosphere/) contains outputs from CMIP6-era models (GFDL-ESM4, IPSL-CM6A-LR, MPI-ESM1-2-HR, MRI-ESM2-0, and UKSEM1-0-LL) for specific scenarios
- If a user wishes to explore other CMIP models or bias-correct model data against different observational data than that used by ISIMIP, the BASD python package (https://github.com/JGCRI/basd) is available for ease of use. BASD implements an extension of the bias adjustment and statistical downscaling method used in ISIMIP3b (ISIMIP3BASD, Lange 2021 https://zenodo.org/records/4686991 and  https://gmd.copernicus.org/articles/12/3055/2019/). The ISIMIP3BASD code base can also be used directly. For additional information, please see https://www.isimip.org/documents/413/ISIMIP3b_bias_adjustment_fact_sheet_Gnsz7CO.pdf 
- If a user wishes to explore novel scenarios not covered by CMIP models or the ISIMIP collection, we suggest a combination of STITCHES emulation (https://github.com/JGCRI/stitches) and BASD bias correction and downscaling. 

HELPS has been tested with both ISIMIP data and outputs from BASD.


[Back to Contents](#Contents)

<br />


