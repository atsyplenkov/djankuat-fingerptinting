[![doi](https://img.shields.io/badge/doi-10.1016%2Fj.catena.2021.105285-success.svg?style=github)][doi]

# Djankuat river (Caucasus Mountains) sediment source fingerprinting

This repository centralizes the main processing steps to explore sediment dynamics after exceptional erosion event. It is meant to accompany a journal article ([*Tsyplenkov et al., 2021*](https://doi.org/10.1016/j.catena.2021.105285)). This study is part of the ongoing Russian Science Foundation project No. 19-17-00181: *«Quantitative assessment of the slope sediment flux and its changes in the Holocene for the Caucasus mountain rivers.»*

For a deep introduction to the study, please refer to:
>Tsyplenkov, A., Vanmaercke, M., Collins, A.L., Kharchenko, S., Golosov, V., 2021. Elucidating suspended sediment dynamics in a glacierized catchment after an exceptional erosion event: The Djankuat catchment, Caucasus Mountains, Russia. CATENA 203, 105285. https://doi.org/10.1016/j.catena.2021.105285

***

Full text of the paper is available [here][doi].

To replicate main results, follow the instructions in `R` directory and, of course, feel free to explore in depth the chunks of code. 

Follow us on Twitter: [@atsyplen][ats], [@MatthiasVanmae1][mvm].

[doi]: https://doi.org/10.1016/j.catena.2021.105285
[ats]: https://twitter.com/atsyplen
[mvm]: https://twitter.com/MatthiasVanmae1

***

## REPLICATION. HOW TO
1. Fork this repository or [unzip the archive][arch].
2. Using RStudio open "djankuat-fingerptinting.Rproj" file in the main project directory.
3. Run the `R/00_prepare-r-session.R` file. 
4. Now you can run other files in `R` directory.

## LOGIC OF THE PROCESS
The whole process is split into four parts, which is reflected in the structure of R scripts. Every `R` file is independent and not related to others. For example, if you want to reproduce only fingerprinting results, then run `R/04_fingerprinting.R`.
The names of the scripts are quite indicative, and each script is reasonably commented.

## SEE ALSO
 - [Our other paper, which explores intra-event suspended sediment dynamics in the same basin (Djankuat)][jss]
 - [Study describing suspended sediment spatial patterns in the Caucasus mountains][piahs]
 - [Paper (Lizaga et al., 2020) describing the FingerPro model][fingerpro]

[fingerpro]: https://doi.org/10.1007/s11269-020-02650-0
[jss]: https://doi.org/10.1007/s11368-020-02633-z
[piahs]: https://doi.org/10.5194/piahs-381-87-2019
