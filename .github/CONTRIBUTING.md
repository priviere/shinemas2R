# Contribute to shinemas2R

If you want to interact with us, please file an [issue]() or send me an email : pamriviere@protonmail.com


`shinemas2R` is based on the webservice API developped in SHiNeMaS.

In order to implement new queries you need:

1. to update the API in SHiNeMaS code so that shinemas2R can call it
2. to update the code of the function `shinemas` 

## Update the API in SHiNeMaS code

TO DO

## Update the code of the function `shinemas` 

- Fork the git repository
- Update the function `shinemas` in your fork:
  - add a `query_type`
    - in the documentation in the top of the function
    - in the code by adding
    ```R
    if( query_type == "your-package_your-query-name"){
      data = get_data_from_shinemas(db_url, user, password, token, query = "your-package_your-query-name")
      vec_fac = c("") # to complete
      for(v in vec_fac){ data[v] = as.factor(as.character(v)) }
      vec_num = c("") # to complete
      for(v in vec_num){ data[v] = as.numeric(as.character(v)) }
    }
    ```
    Note that `query_type` is under the format [package-name]_[query-name]. For example PPBstats_data_agro stands for data_agro format for package PPBstats.
  - update `match.arg` in the begenning of the code with `"your-package_your-query-name"`
  - add `@param` if there are filters if needed
  - update `@details` in the begenning of the function if needed
- Update 'package supported' section in README.md
- Send a Pull Request with a reference to the original issue









    
