# Shark Species Relative Abundance Estimation

This repository contains data mining and predictive modeling work using iNaturalist data to estimate the relative abundance of various shark species in Hawaii and the Bahamas. The analysis employs zero-truncated negative binomial predictive modeling.

## Reports

#### Context
Sharks remain one of the most vulnerable and exploited groups of large marine animals on the planet and it is largely due to knowledge gaps in their population ecology. To empower conservation, we can utilize untapped resources for reconstructing population trends. Social Networks contain vast amounts of observations of sharks in the wild. Here, we use iNaturalist to source shark sightings, develop proxies of effort, and predict time series of relative abundance indices.

#### Model Rationale
We sourced opportunistic shark sightings uploaded by iNaturalist users in two representative regions that are highly trafficked for tourism, underwater photography, and are popular among social media: Hawaii and the Bahamas. To develop abundance indices, we used the entire presence of iNaturalist users in the respective regions as a proxy for observation effort.

The biases concerning this data are popular among most social network platforms. Without surveying users, we cannot gauge their affinity to capture shark presence as opposed to other wildlife, which is important for weighting effort. Second, the opportunistic nature of this data means absences cannot be trusted and therefor only if a shark was observed, the data is approximately correct. And third, we assumed that if a user observed a shark, subsequent non-shark observations are not considered shark absences and instead absences are grouped by trips in temporal bins. Ideally, the resolution of these trips would be daily, but given the sparseness of the data, we used monthly. Additionally, iNaturalist tags observations with three categories of validity in a field called `quality_grade`: `"research" "needs_id" "casual"`. We only sourced research-grade observations that have been validated by at least two people and do not contain identification disagreements. There is also a field that designates observations from aquariums or non-wild enclosures `captive_cultivated`, and so we only selected wild observations.

We based our modeling methods on the Baum et al., 2003 report to predict abundance trends of various Northwest Atlantic shark populations. They used Generalized Linear Models (GLMs) with zero-truncated negative binomial distribution to account for unreporting in pelagic longline logbook data. We adopted this method here because our data likewise represents unreported and sparse shark sightings. 

#### The Data

Dates: 1983 - 2024\
Regions:

**Bahamas** [19.7121, -80.0365, 27.3775, -69.7489] 
- Shark observations: 592
- Non-shark observations: 24,015
- Shark species: 16

**Hawaii** [16.5, -179.5, 29.5, -152.5]
- Shark observations: 554
- Non-shark observations: 60,187
- Shark species: 12

#### The Model
- NBGLM where shark_observations > 0
```
shark_observations ~ year_observed + latitude_bin * longitude_bin + offset(log_effort)
```
For a regional trend, we estimated the number of shark sightings, grouped by year, and then averaged predicted shark sightings and observed effort. Then we standardized Sightings Per Unit Effort (SPUE) time series for area and year, and offset log effort to account for the bias of users' exposure to observe sharks. We scaled and transformed this relative abundance prediction `log(100x)` and then created 95% confidence intervals. Then, we set the initial relative abundance to 1 for comparison among species-specific trends.

#### To Consider 
- A common phenomenon among social networks is `submission fatigue` where the average number of submissions coming from the same user will decline with enthusiasm to use the app. How can this phenomenon be accounted for in the shark observations and the effort?

### Hawaii Species Relative Abundance

The report for the relative abundance of shark species in Hawaii can be found in the following PDF:

- [Hawaii Species Relative Abundance Report](figures/Hawaii_species_relative_abundance.pdf)

### Bahamas Species Relative Abundance

The report for the relative abundance of shark species in the Bahamas can be found in the following PDF:

- [Bahamas Species Relative Abundance Report](figures/Bahamas_species_relative_abundance.pdf)

## Repository Structure

- `./`: Contains the R scripts used for data processing and modeling.
- `data/`: Contains the raw and processed data files.
- `figures/`: Contains the generated reports and plots.
- `README.md`: This file, providing an overview of the repository.

## Usage

To replicate the analysis, follow these steps:

1. Clone the repository:
    ```bash
    git clone https://github.com/yourusername/shark-abundance.git
    cd shark-abundance
    ```
2. Source iNaturalist with `rinat` package and `inat_cron.R` 

3. Run `inat_species.R` to organize data, create and implement ZTNB model, and output reports

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- iNaturalist for providing the data.
- The R community for the powerful tools and packages.
