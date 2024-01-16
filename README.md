# Functional diversity of macroinvertebrate communities in urban streams

Code and data to reproduce analyses of the study titled *Effects of urbanization-related disturbances on macroinvertebrate communities in a Patagonian river system: insights from a functional approach*, submitted for publication to Urban Ecosystems. </br> 

&nbsp;This research was part of my PhD thesis *Efectos de la expansión urbana sobre la integridad ecológica de una cuenca patagónica
Calidad de agua, hábitat y comunidades de macroinvertebrados* (Effects of urban expansion on the ecological integrity of a Patagonian basin
Water quality, habitat, and macroinvertebrate communities).

&nbsp;

## R files description:

* /data:
  * com_hel.RDS - Hellinger transformed community data
  * env_log.RDS - log transformed environmental data
  * traits_fuzzy.RDS - fuzzy trait matrix
  * traits.xlsx - raw trait matrix
* /analysis:
  
* 0_quality_funct_space_fromdist.R: R function for computing the quality of functional dendrogramm and multidimensional functional spaces. This function is a simplified version of the Appendix S1 associated to Maire et al. 2015 (Global Ecol. and Biogeogr.)
* 1_FS script_all.R: Main code to reproduce the results presented in the paper

&nbsp;

## 📦 Dependencies
To run the code from this repository, the following packages are required: 
```ade4, ape, dplyr, geometry, ggplot2, ggrepel, ggtext, MASS, MuMIn, nlme, patchwork, picante, tidyr, usedist, vegan```

&nbsp;

## 📬 Contact
Please send questions or problems related to the use of this code to Emilio Williams-Subiza (ewilsub@gmail.com).
