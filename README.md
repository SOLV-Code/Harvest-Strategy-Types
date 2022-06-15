# Harvest-Strategy-Types

**Shiny App for exploring alternative harvest rule types.**


The app is available online at https://solv-code.shinyapps.io/harveststrategytypes/


### Purpose

This app illustrates different types of harvest strategies for Pacific salmon (Sockeye, Chinook, Coho, Chum, Pink). These species are *anadromous* (spawn in freshwater, migrate to the ocean, then return for spawning) and *semelparous* (spawn only once, then die).  Harvest targets for each year's returning run of salmon are typically set based on a spawning target or an exploitation rate target. Targets may be fixed or change with abundance.

Supporting information  is being compiled in the [wiki pages](https://github.com/SOLV-Code/Harvest-Strategy-Types/wiki)
   * [Further Reading](https://github.com/SOLV-Code/Harvest-Strategy-Types/wiki/Further-Reading)
   * [Worked Examples](https://github.com/SOLV-Code/Harvest-Strategy-Types/wiki/Worked-Examples) : using the app the plot harvest strategies from different fisheries


### How to use the App

The app displays different types of harvest strategy, and shows the implied targets at different levels of abundance. For example, if a stock is managed to a fixed exploitatiom rate target of 50%, what are the *implied* spawner and catch targets.

Users can change the specifications for each type of harvest rule (e.g. set the fixed exploitation rate, or set the steps for a stepped ER rule) and vary the abundance estimate (e.g. median and lower/upper bounds on in-season run size estimate).


### Planned Features

The following additional features are planned:

* option for "minimum ER" when Rate = "Total Allowable Mort"
* option for including in-river mortality
* Fraser Pink- style harvest rule
* option to display boxplot of historic run sizes


You can follow progress at [this thread](https://github.com/SOLV-Code/Harvest-Strategy-Types/issues/3).


You can add feature requests at [this thread](https://github.com/SOLV-Code/Harvest-Strategy-Types/issues/11).


