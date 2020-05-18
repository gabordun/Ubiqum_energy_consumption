# Ubiqum_energy_consumption project

The task is to analyse a household energy consumption data to find regular patterns over time. The datasets spreads over almost four years on a minutely frequency and was collected by three individual submeters and an overall meter.

I used cluster analyses to identify patterns over the days of a week in terms of energy consumption and of 
which part of the appartment was active and to what extent.

The main findings are: 
      - patterns could be identified;
      - particularly this households' cycle can be described by 2-4 clusters a day;
      - worth to mention that weekend follows less complicated patterns compared to the weekdays.

The results could be used for making plans for potential new appartments' electricity system, or for benchmarking,
to provide operational framework for a warning system.

Language : R

Database: load from SQL with MySQL

Packages for cluster analyses: cluster, mclust, NbClust
