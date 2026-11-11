# NFL Decision Science

By JJ Groner

How much is it worth for an NFL offense to line up, read the defense, and then decide whether to snap the ball? This three-paper series estimates the value of that option on fourth downs and two-point tries. If the offense backs out, the model prices the cheaper of its two fallbacks in win-probability terms: taking a delay-of-game penalty or spending a timeout.

The [published series](https://jjgroner.substack.com/) explains the football decisions, modeling assumptions, and results.

| Paper | Question | Where to start |
| --- | --- | --- |
| **Line Up and Look** | What is the season-level value of reading the defense before committing on fourth down or a two-point try? | [Fourth-down model](Breakeven_VOI.R), [two-point model](Breakeven_VOI_2pt.R), [scenario estimates](Paper_Build.R) |
| **The Win-Probability Cost of a Delay-of-Game Penalty on Fourth Down and Two-Point Attempts** | How much does a five-yard penalty cost before a punt, field goal, or extra point? | [Scenario estimates and intervals](DOG_v8_final.R), [game-state lookup](DOG_Grid.R) |
| **What a Timeout Is Worth** | What does it cost to spend a timeout now, given the half, clock, score, and timeouts remaining? | [First-half model](Phase9_EarlyUse_H1.R), [second-half model](H2/H2_Phase7k_TierEight.R) |

![Fourth-down decision map by field position and yards to go in a tied third-quarter game](results/figures/F01_decision_map.png)

This example holds the score and game clock fixed and varies field position and yards to go. The full analysis estimates roughly **0.14 to 0.30 wins per team per regular season** across the four scenarios studied. The reference scenario is about **0.23 wins**. The range reflects assumptions about how often the offense sees a favorable look and how much that look improves its conversion probability. Those quantities are varied in sensitivity analysis because the play-by-play data do not identify them.

## Data and methods

- The models use [nflverse play-by-play](https://github.com/nflverse/nflverse-data/releases/tag/pbp) from the 2017–2024 NFL seasons. The main line-up valuation sample uses regular-season decisions from 2022–2023.
- [nfl4th](https://github.com/nflverse/nfl4th) provides baseline fourth-down and two-point probabilities. The series uses generalized additive models to estimate win-probability surfaces and fourth-down go rates, and uses Monte Carlo intervals or bootstrap sensitivity where appropriate.
- [DecisionLayer_Assemble.R](DecisionLayer_Assemble.R) combines the delay-of-game and timeout costs. The [worked fourth-and-five example](DecisionLayer_Example_4and5.R) shows when each fallback is cheaper.

## Run the analysis

Run these commands from the repository root. The scripts were prepared with R 4.5.2, nflreadr 1.5.0, nflfastR 5.2.0, and nfl4th 1.0.7. The data and baseline models are downloaded from nflverse.

```sh
Rscript --vanilla install_packages.R
Rscript --vanilla run_analysis.R
```

Run just the penalty models with `Rscript --vanilla run_analysis.R delay` or just the timeout models with `Rscript --vanilla run_analysis.R timeouts`. The full model order is in [run_analysis.R](run_analysis.R). Generated files go to `build/`, including figures in `build/figures/` and tables in `build/tables/`. A selection of saved [figures](results/figures/) and [tables](results/tables/) is included for browsing.

The scenario estimates, worked decision-layer examples, and figure generation were rerun using saved model inputs. A fresh rebuild from downloaded play-by-play has yet to be verified.
