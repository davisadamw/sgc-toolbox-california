
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sgc-toolbox-california

<!-- badges: start -->
<!-- badges: end -->

The goal of sgc-toolbox-california is to populate an EV Toolbox for
California using adoption totals from the EPA Project / CARB Scenario
with the Synthetic Population developed for “Couldn’t Car Less: Where
Will New Electric Cars, Trucks, and SUVs be Located? A Household-Level
EV Adoption Model for California” \~ Trisha Ramadoss, Adam Davis, Gil
Tal TRBAM-22-03706 .

# Inputs

-   Synthetic Population for California
-   Annual EV Ownership totals for all household categories / sizes

# Scripts included:

-   01\_\* Use synthetic population to convert statewide totals to
    Tract-level ownership totals
-   02\_\* Interpolate tract-level ownership totals
-   03\_\* Produce and interpolate tract-level commute info
-   04\_\* Any additional data prep needed for website, likely including
    spatial data prep
