# mincost-matching
STATS 606 Project with Julian Bernado, Josh Garman, Yizhou Gu, and Samuel Rosenberg.

## state of the code pipeline
right now, there are main analysis files:

```
1_make_grid.R
2_simulate_flows.R
3_model_times.R
4_generate_figures.R
```

these call the following scripts that provide helper functionality:

```
grid_helpers.R
mcf_helpers.R
data_gen.R
networks.R
```

the inputs and outputs of the analysis files can be represented as:

```
1_make_grid.R
    input: none
    output: grids/prognostic_grid.csv, grids/propensity_grid.csv
    description: grids the data

2_simulate_flows.R
    input: grids/prognostic_grid.csv, grids/propensity_grid.csv
    output: results/prognostic_results.csv, results/propensity_results.csv
    description: runs the experiment on the grid. takes maybe 20 minutes with the current grid values so we can scale our grid up a bit if desired.

3_model_times.R
    input: results/prognostic_results.csv, results/propensity_results.csv
    output: none
    description: fits a random forest model to our results and tries to get a better algorithm picking strategy than just always picking simplex

4_generate_plots.R
    input: results/prognostic_results.csv, results/propensity_results.csv
    output: figures/tables/overall_performance.md, figures/plots/prognostic_{c}.png, figures/plots/propensity_{c}.png [c is n, p, mc, rel_mc, imbalance]
    description: makes plots for the report showing computational time as a function of our grid parameters
```

## next steps
i think all possible next steps are:
- make the grid fully consistent
    - currently there's some issues with getting the proper $mu^*$ values such that the prognostic grid is a bit smaller than the propensity grid
    - in the propensity model, we should mirror the prognostic case by only simulating b when $B(n,p,a)$ is greater than 0.99. this might introduce the same kind of integration errors as are referenced in the above sub-bullet.
- fully investigate whether we can use our experiment to predict best algorithm at a higher clip than just always picking network simplex
    - if so, incorporate into report the personalized recommendations
    - if not, let's demonstrate in the report that we tried to do this, it's a bolstering of the neutral result
- think about any other nice visualization or tests
- clean up the report (tables inconsistent, expand on mcf algorithms, add an image showing what the actual graphs look like)
- not sure i really like the name either
