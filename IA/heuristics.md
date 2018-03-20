
## Heuristics

Using a heuristic in order to solve a problem means using
non rigorous methods to solve it, that is, unlike algorithms, heuristics
find solutions by intelligent guesswork rather than following some
pre-established formula.

Heuristics are *rules-of-thumbs*.

**When to use heuristics?**

Heuristics are employed in cases where
algorithms give unsatisfactory results
or no results at all.

Is important to notice, that due to their intuitive nature, heuristics
find *local solutions*.

## Informed search algorithms

### Hill climbing

* This is an **informed search** algorithm and a very naive one since it makes choices by only considering current information and doesn't recovers from past mistakes, that is, it takes the first best choice found without considering the rest of the alternatives.

### Steepest-ascent hill climbing
In contrast to **hill climbing**, it considers all the alternatives before making a choice.

Even though **steepest-ascent hill climbing** analyzes all the options before making a move, it can't **backtrack**, that is, it can't go back to a previously visited state in order to take another direction thus  getting stuck in local minima/maxima.

### Best-first search

This search method keeps a list of soon to be visited (in the *open list*) and visited nodes.

Nodes in the *open list* are ranked by their proximity to the goal state. This proximity is measured by a heuristic measure and thus the performance of this method heavily relies on it.
