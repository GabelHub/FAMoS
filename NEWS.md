NEWS
================

# FAMoS 0.2.0

## Major changes

  - FAMoS can now handle cost functions that do not rely on AIC(c) or
    BIC. Any selection criterion can be used and FAMoS will attempt to
    minimise it. Therefore, the option *nr.of.data* was removed. The
    selection criterion value now needs to be returned by the cost
    function *fit.fn*.

  - FAMoS can now use other fitting routines than *optim*. The custom
    fitting routines can be included in the cost function *fit.fn*. The
    use of a custom fitting routine requires to set *use.optim = FALSE*.
    In this case, the cost function *fit.fn* needs to return a list
    containing the selection criterion value as well as a named
    parameter vector describing the optimised fit.

## Minor changes

  - Graphic settings get returned to their previous values after calling
    FAMoS plotting functions.

  - Log files are no longer saved using *sink*.

  - The default number for *optim.runs* was set to 1.

  - Futures are no longer used as default option.

  - The function *sc.order* now plots logarithmically, if the ratio
    between minimal and maximal values is larger than 300.

  - FAMoS now checks during each run, if the used cost function has
    changed. In this case, an interactive warning will be issued to the
    user.

  - Included an option to silence the verbose output of FAMoS (*verbose
    = FALSE*).

  - FAMos now returns the total number of tested models.

  - If a custom fitting routine is used, no previous applicability
    checks are performed.

## Bug fixes

  - Fixed the problem of multiple best models. If multiple best models
    are found, only the first one is used.

  - The function *famos.performance* now displays the grid properly.
