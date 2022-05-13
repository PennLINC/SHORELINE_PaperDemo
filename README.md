# SHORELINE Paper Reproducible Analysis

This package was developed to reproduce, document, and share the results of
the SHORELINE benchmarking paper developed by Matt Cieslak for the
Penn Lifespan Informatics & Neuroimaging Center (manuscript in preparation).

The abstract for the paper reads as follows:

> Head motion correction is particularly challenging in diffusion-weighted MRI (dMRI) 
scans due to the dramatic changes in image contrast at different gradient strengths
and directions. Currently head motion correction is typically performed using a
Gaussian Process model implemented in FSL’s Eddy. Recently, the 3dSHORE-based
SHORELine method was introduced to correct any non-single-shelled sequence.
Here we perform a comprehensive evaluation of both methods on realistic
simulations of a software fiber phantom and known ground-truth head motion.
We demonstrate that both methods perform remarkably well, but that 
performance can be affected by sampling scheme, the pervasiveness of
head motion and the denoising strategy applied before head motion correction.

This package specifically documents the statistical analysis portion of the paper.
Under Articles, you'll find an Rmarkdown file that produces the statistical analysis
dataset; a walkthrough of the analyses performed; and the code for producing
visualizations and table content. Dense code for preprocessing the datasets are
also available in the `/R` folder.

You can also reproduce the analyses for yourself by downloading this package with
`remotes::install_github("PennLINC/SHORELINE_PaperDemo")` and emulating the
Article, or online using Binder.
