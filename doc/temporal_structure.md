# Temporal Structure

In order to deterministically generate events over time, we need a model that does a few things:

* generates from a seed and keeps things deterministic all the way down
* generates discrete events over time in realistic patterns by incorporating:
  * Day/Night
  * Daily/Weekly Work Hours
  * Independent factor (ARMA)
  * dependent factor (group dynamic, etc)
* generates unique activity for each actor
  * that is still beholden to dependent factors

At the event level, we'll need the following:

* when did the event occur?
* who is the actor for this event?
* a long representing the seed for generating this event.
  * should be a function of
    * the simulation (base) seed
    * the time
    * the actor
* ~~a double representing the coarse "effectiveness" of the actor when the event occurs~~
  * ~~harder to reason about this one, as no value/desirability is available~~
