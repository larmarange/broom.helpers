# GGally

<details>

* Version: 2.1.2
* GitHub: https://github.com/ggobi/ggally
* Source code: https://github.com/cran/GGally
* Date/Publication: 2021-06-21 04:40:10 UTC
* Number of recursive dependencies: 142

Run `revdepcheck::revdep_details(, "GGally")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running 'spelling.R'
      Running 'testthat.R'
     ERROR
    Running the tests in 'tests/testthat.R' failed.
    Last 13 lines of output:
        2. │ ├─testthat::expect_error(print(x), NA) at test-ggcoef_model.R:6:4
        3. │ │ └─testthat:::quasi_capture(...)
        4. │ │   ├─testthat (local) .capture(...)
        5. │ │   │ └─base::withCallingHandlers(...)
        6. │ │   └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
    ...
        7. │ └─base::print(x)
        8. ├─GGally::ggcoef_model(mod2)
        9. │ └─GGally:::ggcoef_data(...)
       10. │   └─broom.helpers::tidy_plus_plus(...)
       11. │     └─res %>% ...
       12. └─broom.helpers::tidy_add_estimate_to_reference_rows(...)
      
      [ FAIL 1 | WARN 21 | SKIP 3 | PASS 626 ]
      Error: Test failures
      Execution halted
    ```

