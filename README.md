# Functional diversity of macroinvertebrate communities in urban streams

Code and data to reproduce analyses of the study titled *Effects of urbanization-related disturbances on macroinvertebrate communities in a Patagonian river system: insights from a functional approach*, published in Urban Ecosystems (2024). </br> 


This research was part of my PhD thesis *Efectos de la expansión urbana sobre la integridad ecológica de una cuenca patagónica
Calidad de agua, hábitat y comunidades de macroinvertebrados* (Effects of urban expansion on the ecological integrity of a Patagonian basin
Water quality, habitat, and macroinvertebrate communities).</br>

&nbsp;

**Abstract** </br>
The expansion of urban areas has led to land-use changes that affect aquatic biodiversity and ecosystem processes through a number of ways, such as increased runoff, water pollution, habitat homogenization, and disturbance of the natural flow. It is well known that urbanization induces profound changes in freshwater macroinvertebrate communities, but relatively few studies have approached this topic using trait-based methods. This severely limits our ability to monitor and predict potential alterations of ecosystem processes. We here provide empirical evidence about the effects of urbanization-related changes on the trait composition and functional diversity of freshwater macroinvertebrates from Patagonia (Argentina). Macroinvertebrate sampling and environmental characterization were carried out in 13 study sites distributed across three water courses in the Futaleufú river basin. We found changes in environmental conditions over the urban impact gradient, in turn accompanied by variation in functional diversity and trait composition of aquatic macroinvertebrates. Specifically, we detected lower functional diversity in urban and post-urban reaches, mainly attributed to shifts in the distribution of taxa within the functional space, rather to the extirpation of functionally distinct taxa located at the edges of it (i.e., functional evenness and dispersion decreased, but functional richness remained constant). We also found signs of functional homogenization at the most impaired sites, where gatherers and tegument-breathing taxa dominated. These changes were mainly explained by conductivity, biological oxygen demand, and oxygen levels in the water column.

&nbsp;

## R files description:
* **FdivFunctions.R** - custom functions to calculate functional metrics  by Cayetano Gutiérrez-Cánovas. </br>See Gutiérrez-Cánovas C, Sánchez-Fernández D, Cañedo-Argüelles M, et al (2019) Do all roads lead to Rome? Exploring community trajectories in response to anthropogenic salinization and dilution of rivers. Philos Trans R Soc B Biol Sci 374:20180009.

* /data:
  * **com_hel.RDS** - Hellinger transformed community data.
  * **env_log.RDS** - log transformed environmental data.
  * **traits_fuzzy.RDS** - fuzzy trait matrix.
  * **trait_composition.RDS** - relative abundance of trait modalities.
  * **traits.xlsx** - raw trait matrix.
  * **env.xlsx** - raw environmental data.

* /analysis:
  * **functional_metrics.R** - custom ggplot theme.
  * **glmm1.R** - code to run functional metrics models.
  * **glmm2.R** - code to run trait modalities models.
  * **rlq.R** - code to run rlq and fourth corner analyses.

* /figures:
  * **custom_theme_functional_thesis.R** - code to calculate functional metrics and run null models.
  * **fig_env_variables.R** - code to plot Fig.3.
  * **fig_functional_metrics.R** - code to plot Fig.4.
  * **fig_glmm1.R** - code to plot Fig.5.
  * **fig_RLQ.R** - code to plot Fig.6.
  * **fig_glmm2.R** - code to plot Fig.7.

*note: to plot the figures, the scripts inside /analysis must be run first.*
  
&nbsp;

## 📦 Dependencies
To run the code from this repository, the following packages are required: 
```ade4, ape, dplyr, geometry, ggplot2, ggrepel, ggtext, MASS, MuMIn, nlme, patchwork, picante, tidyr, usedist, vegan```

&nbsp;

## 📬 Contact
Please send questions or problems related to the use of this code to Emilio Williams-Subiza (ewilsub@gmail.com).
