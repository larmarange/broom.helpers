# ggstats

<details>

* Version: 0.1.1
* GitHub: https://github.com/larmarange/ggstats
* Source code: https://github.com/cran/ggstats
* Date/Publication: 2022-11-24 00:20:08 UTC
* Number of recursive dependencies: 103

Run `revdep_details(, "ggstats")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running 'spelling.R'
      Running 'testthat.R'
     ERROR
    Running the tests in 'tests/testthat.R' failed.
    Last 13 lines of output:
        6. │   └─broom.helpers::tidy_plus_plus(...)
        7. │     └─model %>% ...
        8. └─broom.helpers::tidy_and_attach(...)
        9.   └─base::tryCatch(...)
       10.     └─base (local) tryCatchList(expr, classes, parentenv, handlers)
    ...
       11.       └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
       12.         └─value[[3L]](cond)
       13.           └─base::tryCatch(...)
       14.             └─base (local) tryCatchList(expr, classes, parentenv, handlers)
       15.               └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
       16.                 └─value[[3L]](cond)
      
      [ FAIL 1 | WARN 13 | SKIP 10 | PASS 4 ]
      Error: Test failures
      Execution halted
    ```

