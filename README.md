# Harvest-Strategy-Types

**Shiny App for exploring alternative harvest rule types.**


**The app is available online at https://solv-code.shinyapps.io/harveststrategytypes/**

**New for Version 2.0**

* additional HCR types
* option to compare two alternative HCR directly
* option to create plots without abundance overlay (just clear the 'Run-med' input)



### Purpose

This app illustrates different types of harvest strategies for Pacific salmon (Sockeye, Chinook, Coho, Chum, Pink). These species are *anadromous* (spawn in freshwater, migrate to the ocean, then return for spawning) and *semelparous* (spawn only once, then die).  Harvest targets for each year's returning run of salmon are typically set based on a spawning target or an exploitation rate target. Targets may be fixed or change with abundance.

Supporting information  is being compiled in the [wiki pages](https://github.com/SOLV-Code/Harvest-Strategy-Types/wiki)

* [HCR Definitions and Taxonomy](https://github.com/SOLV-Code/Harvest-Strategy-Types/wiki/HCR-Definitions-&-Taxonomy)

* [Other HCR Apps and Packages](https://github.com/SOLV-Code/Harvest-Strategy-Types/wiki/Other-HCR-Apps-&-Packages)

* [Further Reading](https://github.com/SOLV-Code/Harvest-Strategy-Types/wiki/Further-Reading)

* [Worked Example: Fraser Chum Clockwork](https://github.com/SOLV-Code/Harvest-Strategy-Types/wiki/Worked-Example:-Fraser-Chum-%22Clockwork%22-Stepped-Harvest-Strategy)

* [Worked Example: Fraser River Odd-year Pinks](https://github.com/SOLV-Code/Harvest-Strategy-Types/wiki/Worked-Example:-Fraser-River-Odd%E2%80%90year-Pink-Salmon)

* [Worked Example: Fraser Sockeye TAM Rules](https://github.com/SOLV-Code/Harvest-Strategy-Types/wiki/Worked-Example:-Fraser-Sockeye-TAM-Rule)

* [Worked Example: Skeena Cdn Mixed-stock Marine Comm Fishery](https://github.com/SOLV-Code/Harvest-Strategy-Types/wiki/Worked-Example:-Skeena-Sockeye-Canadian-Marine-Commercial-Fishery)

* [Worked Example: Weird "Bump" in Spawner Target for Ice Hockey Stick HCR](https://github.com/SOLV-Code/Harvest-Strategy-Types/wiki/Worked-Example:-Weird-Bump-in-Ice-Hockey-Stick)

### How to use the App

The app displays different types of harvest strategy, and shows the implied targets at different levels of abundance. For example, if a stock is managed to a fixed escapement target of 30,000, what are the *implied* exploitation rate and catch targets at 5,000 or 50,000 or 150,000 run size?

Users can change the specifications for each type of harvest rule (e.g. set the fixed exploitation rate, or set the steps for a stepped ER rule) and vary the abundance estimate (e.g. median and lower/upper bounds on in-season run size estimate).


### Additional Features

Additional harvest strategy types and diagnostic plots will be added over time. You can follow progress at [this thread](https://github.com/SOLV-Code/Harvest-Strategy-Types/issues/3) and request features at [this thread](https://github.com/SOLV-Code/Harvest-Strategy-Types/issues/11).


### Citation

To cite the app directly, use:

G. Pestal, C. Carr-Harris, and A.-M. Huang. 2025. An interactive tool for exploring alternative harvest rule types for Pacific Salmon. Available online at https://solv-code.shinyapps.io/harveststrategytypes

If you need a permament digital object identifier (doi), cite the Zenodo release of the Github repository:

G. Pestal, C. Carr-Harris, and A.-M. Huang. 2025. SOLV-Code/Harvest-Strategy-Types (v2.0). Zenodo. https://doi.org/10.5281/zenodo.14635493

[![DOI](https://zenodo.org/badge/497840416.svg)](https://doi.org/10.5281/zenodo.14635493)

