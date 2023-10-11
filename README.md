# Functional diversity of macroinvertebrate communities in urban streams

Code and data to reproduce analyses of the article *title* </br>

The article explores the effect of urbanization-related changes on the functional diversity and composition of freshwater macroinvertebrate communities in the Esquel-Percy basin (Patagonia, Argentina). 
&nbsp;

## 👨‍🔬 Original article:
Please, use this citation to reference the code:
```
L. B. Epele, M. G. Grech, E. A. Williams-Subiza, C. Stenert, K. McLean, H. S. Greig, L. Maltchik, M. M. Pires, M. S. Bird, A. Boissezon,
D. Boix, E. Demierre, P. E. García, S. Gascón, M. Jeffries, J. M. Kneitel, O. Loskutova, L. M. Manzo, G. Mataloni, M. C. Mlambo, B. Oertli,
J. Sala, E. E. Scheibler, H. Wu, S. A. Wissinger, D. P. Batzer, 

Perils of life on the edge: Climatic threats to global diversity patterns of wetland macroinvertebrates. 
Sci. Total Environ. 820, 153052 (2022).
```
&nbsp;

## R files description:

* 0_FD_functions.R: R script to estimate Functional Diversity (FD) metrics
* 0_quality_funct_space_fromdist.R: R function for computing the quality of functional dendrogramm and multidimensional functional spaces. This function is a simplified version of the Appendix S1 associated to Maire et al. 2015 (Global Ecol. and Biogeogr.)
* 1_FS script_all.R: Main code to reproduce the results presented in the paper

&nbsp;

## 📦 Dependencies
To run the code from this repository, the following packages are required: 
```ade4, ape, dplyr, geometry, ggplot2, ggrepel, ggtext, MASS, MuMIn, nlme, patchwork, picante, tidyr, usedist, vegan```

&nbsp;

## 📬 Contact
Please send questions or problems related to the use of this code to Emilio Williams-Subiza (ewilsub@gmail.com).
