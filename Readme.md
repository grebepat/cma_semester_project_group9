# Proposal for Semester Project

```{=html}
<!-- 
Please render a pdf version of this Markdown document with the command below (in your bash terminal) and push this file to Github

quarto render Readme.md --to pdf
-->
```
**Patterns & Trends in Environmental Data / Computational Movement Analysis Geo 880**

| Semester:      | FS24                                                          |
|:-----------------------|:-----------------------------------------------|
| **Data:**      | Strava bicycle data from several bike messengers in Zurich    |
| **Title:**     | Cycling Secrets: Decoding Bike Messenger Paths for Efficiency |
| **Student 1:** | Maurin Huonder                                                |
| **Student 2:** | Patrick Greber                                                |

## Abstract

This project investigates route choice modeling for bike messengers in Zurich. Firstly, we will compare the paths of chosen routes (deliveries) with a generated set of modeled routes (choice-set/alternatives) to estimate deviations from the most direct paths available, thereby assessing efficiency in terms of traveled distance. Secondly, we will adapt our definition of efficiency (shortest route) to implement the idea of resistance/convenience to our analysis. To achieve this, we will enrich Zurich’s route network with additional attributes to establish a cost-path network. Attributes such as traveled distance, slope, traffic volumes, street type and width, and the number of intersections could be incorporated into the new network. An efficient route choice will then be defined by selecting options with an optimized cost-path ratio. Given potential time constraints, we will focus primarily on the first approach (shortest route) and consider the second approach depending on the remaining time budget.

## Research Questions

1.  Do bike messengers always choose the shortest possible route during their deliveries? -\> Efficiency in terms of travel distance

2.  How do street network attributes influence the route choice behaviour of bike messengers in Zurich? -\> Efficiency in terms of an optimised cost-path ratio

3.  Is there a combination of above criteria that would be preferable in terms of speed/convenience?

## Results / products

We anticipate that the observed route choices will align only partially with the shortest possible routes. Instead, the chosen routes are likely influenced by factors such as road type and slope. We assume a trade-off between the most direct path and resistance/convenience, with bike messengers leveraging their knowledge of the local street network to optimize this balance/trade-off.

## Data

We will utilize movement data from Strava, collected by several bike messengers in Zurich. The data, stored in GPX files, encompasses approximately 60 shifts per messenger, with each shift including multiple trajectories/deliveries. The shifts vary in terms of distance traveled, working hours, and the number of deliveries. All contextual data for enriching the route network is sourced from open-source platforms:

-   SwissTLM3D (street network, street type, and width)
-   SwissALTI3D (slope)
-   Motorized Individual Traffic Volume Data (traffic volume)
-   OSM (traffic lights, intersections/junctions)

## Analytical concepts

1.  **Segmentation:** Split the raw data to isolate individual deliveries, generating single-way trajectories for each delivery.
2.  **Map Matching:** Perform map matching of the messenger data to extract start and end points of each trajectory.
3.  **Route Modeling:** Model alternative routes using the start and end points of segmented trajectories.
4.  **Comparison:** Compare the modeled trajectories with the actual trajectories in terms of the shortest path.
5.  **Enhanced Analysis:** Repeat the analysis with an enriched street network that includes resistance attributes.

## R concepts

For modeling alternative trajectories, we will use the HERE REST API via the R package `HereR`. Additionally, we may consider using the BFSLE algorithm as discussed in recent literature (see here: <https://doi.org/10.1016/j.tra.2023.103723>).

## Risk analysis

The primary challenge is the implementation of resistance factors. The multitude of attributes influencing cost-path networks or personal route choices makes meaningful integration complex. Addressing and incorporating these attributes effectively will be a significant challenge.

## Questions?

1.  Should we combine the street network attributes into a single resistance attribute, or should we keep them separate to identify which attributes significantly influence route choice?
2.  Is this project feasible given the limited time and resources?
3.  Other ideas for this data?
4. Rauf / Runter Fahren? Unterschiedliche Routenwahl bez. Geschwindigkeit
5. Mit Datenmengen umgehen, gerade gpkg von swisstopo sehr gross, selbst wenn zugeschnitten
