# Simple PSM produces correct results.

    WAoAAAACAAQCAQACAwAAAAMTAAAACwAAABAAAAAEAAQACQAAAANseXMABAAJAAAAA2x5cwAE
    AAkAAAAFcWFseXMABAAJAAAABXFhbHlzAAAAEAAAAAQABAAJAAAAB2Nvc3RfaGMABAAJAAAA
    B2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMAAAAQAAAABAAEAAkAAAAQ
    Q0hFTU8gdnMuIFRBUkdFVAAEAAkAAAAQVEFSR0VUIHZzLiBDSEVNTwAEAAkAAAAQQ0hFTU8g
    dnMuIFRBUkdFVAAEAAkAAAAQVEFSR0VUIHZzLiBDSEVNTwAAABAAAAAEAAQACQAAAAVDSEVN
    TwAEAAkAAAAGVEFSR0VUAAQACQAAAAVDSEVNTwAEAAkAAAAGVEFSR0VUAAAAEAAAAAQABAAJ
    AAAABlRBUkdFVAAEAAkAAAAFQ0hFTU8ABAAJAAAABlRBUkdFVAAEAAkAAAAFQ0hFTU8AAAAO
    AAAABEATdI/LYc1zQBgcJvNi0OhADb1PswsiOkASncv0urKWAAAADgAAAARBAoosv4v6skEh
    fXP5TcCZQQKKLL+L+rJBIX1z+U3AmQAAAA4AAAAEv/KeXKAEDdQ/8p5coAQN1L/t+SDZqQvI
    P+35INmpC8gAAAAOAAAABMEZtdGS1YPZQRm10ZLVg9nBGbXRktWD2UEZtdGS1YPZAAAADgAA
    AATBFhgfg4XyUEEWGB+DhfJQwRty5NSUVFdBG3Lk1JRUVwAAABAAAAAEAAQACQAAAAczNjE5
    OTIqAAQACQAAAAYzNjE5OTIABAAJAAAABzQ0OTcyMSoABAAJAAAABjQ0OTcyMQAABAIAAAAB
    AAQACQAAAAVuYW1lcwAAABAAAAALAAQACQAAAAZoZWFsdGgABAAJAAAABGVjb24ABAAJAAAA
    BnNlcmllcwAEAAkAAAAIcmVmZXJlbnQABAAJAAAACmNvbXBhcmF0b3IABAAJAAAABmVmZmVj
    dAAEAAkAAAAEY29zdAAEAAkAAAAHZGVmZmVjdAAEAAkAAAAFZGNvc3QABAAJAAAABGljZXIA
    BAAJAAAAC2ljZXJfc3RyaW5nAAAEAgAAAAEABAAJAAAACXJvdy5uYW1lcwAAAA0AAAACgAAA
    AP////wAAAQCAAAAAQAEAAkAAAAFY2xhc3MAAAAQAAAAAQAEAAkAAAAKZGF0YS5mcmFtZQAA
    AP4=

---

    Code
      exported
    Output
      $`Inputs - Settings`
      # A tibble: 4 × 2
        setting      value           
        <chr>        <chr>           
      1 disc_cost    0.03            
      2 disc_eff     0.03            
      3 cycle_length 30.4166666666667
      4 n_cycles     240             
      
      $`Inputs - Strategies`
      # A tibble: 2 × 2
        name   desc            
        <chr>  <chr>           
      1 CHEMO  Chemotherapy    
      2 TARGET Targeted Therapy
      
      $`Inputs - States`
      # A tibble: 3 × 4
        name  desc              prob limit
        <chr> <chr>            <dbl> <dbl>
      1 PF    Progression-Free     1     0
      2 PP    Post-Progression     0     0
      3 DEAD  Dead                 0     0
      
      $`Inputs - Transitions`
      # A tibble: 4 × 4
        strategy endpoint cycle_length value     
        <chr>    <chr>           <dbl> <chr>     
      1 CHEMO    PFS                 1 pfs_chemo 
      2 TARGET   PFS                 1 pfs_target
      3 CHEMO    OS                  1 os_chemo  
      4 TARGET   OS                  1 os_target 
      
      $`Inputs - Health Values`
      # A tibble: 4 × 5
        name     description                 strategy state value             
        <chr>    <chr>                       <chr>    <chr> <chr>             
      1 pf_ly    Progression-free life-years All      PF    cycle_length_years
      2 pp_ly    Post-progression life-years All      PP    cycle_length_years
      3 pf_qalys Progression-free QALYs      All      PF    pf_ly * pfs_util  
      4 pp_qalys Post-Progression QALYs      All      PP    pp_ly * pps_util  
      
      $`Inputs - Econ Values`
      # A tibble: 4 × 5
        name         description                         strategy state value         
        <chr>        <chr>                               <chr>    <chr> <chr>         
      1 med_cost     Medication cost                     CHEMO    PF    chemo_cost * …
      2 pf_care_cost Routine care cost, pre-progression  All      PF    cycle_length_…
      3 med_cost     Label...                            TARGET   PF    target_cost *…
      4 pp_care_cost Routine care cost, post-progression All      PP    cycle_length_…
      
      $`Inputs - Health Summ`
      # A tibble: 4 × 4
        name  description value       wtp
        <chr> <chr>       <chr>     <dbl>
      1 lys   Life-Years  pf_ly    100000
      2 lys   Life-Years  pp_ly    100000
      3 qalys QALYs       pf_qalys 150000
      4 qalys QALYs       pp_qalys 150000
      
      $`Inputs - Econ Summ`
      # A tibble: 3 × 3
        name    description             value       
        <chr>   <chr>                   <chr>       
      1 cost_hc Healthcare system costs med_cost    
      2 cost_hc Label...                pf_care_cost
      3 cost_hc Label...                pp_care_cost
      
      $`Inputs - Parameters`
      # A tibble: 12 × 6
         name          desc                       value low       high      psa       
         <chr>         <chr>                      <chr> <chr>     <chr>     <chr>     
       1 pfs_shape     PFS shape parameter, chemo 1.2   bc * 0.75 bc * 1.25 "lognorma…
       2 pfs_scale     PFS scale parameter, chemo 40.3  bc * 0.75 bc * 1.25 "lognorma…
       3 pfs_hr_target PFS HR, target             0.67  0.6       0.8       "lognorma…
       4 os_shape      OS shape parameter, chemo  1.1   0.95      1.34      "lognorma…
       5 os_scale      OS scale parameter, chemo  70.4  bc * 0.75 bc * 1.25 "lognorma…
       6 os_hr_target  OS HR, target              0.74  bc * 0.75 bc * 1.25 "lognorma…
       7 chemo_cost    Cost per month, chemo      2000  bc - 500  bc + 500  ""        
       8 target_cost   Cost per month, target     10000 bc - 3000 bc + 3000 ""        
       9 pfs_cost      Cost per month, PFS        1000  bc - 700  bc + 700  "normal(m…
      10 pps_cost      Cost per month, PPS        2000  0         10000     "normal(m…
      11 pfs_util      Utility value, PFS         0.82  bc - 0.05 bc + 0.05 "lognorma…
      12 pps_util      Utility value, PPS         0.68  bc - 0.1  bc + 0.1  "lognorma…
      
      $`Inputs - Surv Dists`
      # A tibble: 4 × 2
        name       value                                                      
        <chr>      <chr>                                                      
      1 pfs_chemo  "define_survival(dist = \"weibull\", pfs_shape, pfs_scale)"
      2 os_chemo   "define_survival(dist = \"weibull\", os_shape, os_scale)"  
      3 pfs_target "apply_hr(pfs_chemo, pfs_hr_target)"                       
      4 os_target  "apply_hr(os_chemo, os_hr_target)"                         
      
      $`Calc - Params`
      # A tibble: 480 × 31
         strategy group        state_time cycle model_time cycle_length_days
         <chr>    <chr>             <dbl> <dbl>      <dbl>             <dbl>
       1 CHEMO    All Patients          1     1          1              30.4
       2 CHEMO    All Patients          1     2          2              30.4
       3 CHEMO    All Patients          1     3          3              30.4
       4 CHEMO    All Patients          1     4          4              30.4
       5 CHEMO    All Patients          1     5          5              30.4
       6 CHEMO    All Patients          1     6          6              30.4
       7 CHEMO    All Patients          1     7          7              30.4
       8 CHEMO    All Patients          1     8          8              30.4
       9 CHEMO    All Patients          1     9          9              30.4
      10 CHEMO    All Patients          1    10         10              30.4
      # ℹ 470 more rows
      # ℹ 25 more variables: cycle_length_weeks <dbl>, cycle_length_months <dbl>,
      #   cycle_length_years <dbl>, model_day <dbl>, model_week <dbl>,
      #   model_month <dbl>, model_year <dbl>, state_day <dbl>, state_week <dbl>,
      #   state_month <dbl>, state_year <dbl>, disc_h <dbl>, disc_e <dbl>,
      #   pfs_shape <dbl>, pfs_scale <dbl>, pfs_hr_target <dbl>, os_shape <dbl>,
      #   os_scale <dbl>, os_hr_target <dbl>, chemo_cost <dbl>, target_cost <dbl>, …
      
      $`Calc - Trans`
      # A tibble: 482 × 5
         strategy group        cycle   pfs    os
         <chr>    <chr>        <dbl> <dbl> <dbl>
       1 CHEMO    All Patients     0 1     1    
       2 CHEMO    All Patients     1 0.988 0.991
       3 CHEMO    All Patients     2 0.973 0.980
       4 CHEMO    All Patients     3 0.957 0.969
       5 CHEMO    All Patients     4 0.939 0.958
       6 CHEMO    All Patients     5 0.922 0.947
       7 CHEMO    All Patients     6 0.903 0.936
       8 CHEMO    All Patients     7 0.885 0.924
       9 CHEMO    All Patients     8 0.866 0.913
      10 CHEMO    All Patients     9 0.847 0.901
      # ℹ 472 more rows
      
      $`Calc - Unit Values`
      # A tibble: 1,440 × 24
         strategy group cycle state pf_ly pp_ly pf_qalys pp_qalys   lys qalys med_cost
         <chr>    <chr> <dbl> <chr> <dbl> <dbl>    <dbl>    <dbl> <dbl> <dbl>    <dbl>
       1 CHEMO    All …     1 DEAD      0     0        0        0     0     0        0
       2 CHEMO    All …     2 DEAD      0     0        0        0     0     0        0
       3 CHEMO    All …     3 DEAD      0     0        0        0     0     0        0
       4 CHEMO    All …     4 DEAD      0     0        0        0     0     0        0
       5 CHEMO    All …     5 DEAD      0     0        0        0     0     0        0
       6 CHEMO    All …     6 DEAD      0     0        0        0     0     0        0
       7 CHEMO    All …     7 DEAD      0     0        0        0     0     0        0
       8 CHEMO    All …     8 DEAD      0     0        0        0     0     0        0
       9 CHEMO    All …     9 DEAD      0     0        0        0     0     0        0
      10 CHEMO    All …    10 DEAD      0     0        0        0     0     0        0
      # ℹ 1,430 more rows
      # ℹ 13 more variables: pf_care_cost <dbl>, pp_care_cost <dbl>, cost_hc <dbl>,
      #   .disc_pf_ly <dbl>, .disc_pp_ly <dbl>, .disc_pf_qalys <dbl>,
      #   .disc_pp_qalys <dbl>, .disc_lys <dbl>, .disc_qalys <dbl>,
      #   .disc_med_cost <dbl>, .disc_pf_care_cost <dbl>, .disc_pp_care_cost <dbl>,
      #   .disc_cost_hc <dbl>
      
      $`Calc - Values`
      # A tibble: 480 × 23
         strategy group  cycle  pf_ly   pp_ly pf_qalys pp_qalys    lys  qalys med_cost
         <chr>    <chr>  <dbl>  <dbl>   <dbl>    <dbl>    <dbl>  <dbl>  <dbl>    <dbl>
       1 CHEMO    All P…     1 0.0828 1.06e-4   0.0679  7.19e-5 0.0829 0.0680    1988.
       2 CHEMO    All P…     2 0.0817 4.04e-4   0.0670  2.75e-4 0.0821 0.0673    1961.
       3 CHEMO    All P…     3 0.0804 8.27e-4   0.0659  5.63e-4 0.0812 0.0665    1930.
       4 CHEMO    All P…     4 0.0790 1.32e-3   0.0648  8.94e-4 0.0803 0.0657    1896.
       5 CHEMO    All P…     5 0.0775 1.85e-3   0.0636  1.25e-3 0.0794 0.0648    1861.
       6 CHEMO    All P…     6 0.0760 2.40e-3   0.0623  1.63e-3 0.0784 0.0640    1825.
       7 CHEMO    All P…     7 0.0745 2.98e-3   0.0611  2.03e-3 0.0775 0.0631    1788.
       8 CHEMO    All P…     8 0.0730 3.57e-3   0.0598  2.43e-3 0.0765 0.0623    1751.
       9 CHEMO    All P…     9 0.0714 4.17e-3   0.0586  2.84e-3 0.0756 0.0614    1714.
      10 CHEMO    All P…    10 0.0698 4.77e-3   0.0573  3.25e-3 0.0746 0.0605    1676.
      # ℹ 470 more rows
      # ℹ 13 more variables: pf_care_cost <dbl>, pp_care_cost <dbl>, cost_hc <dbl>,
      #   .disc_pf_ly <dbl>, .disc_pp_ly <dbl>, .disc_pf_qalys <dbl>,
      #   .disc_pp_qalys <dbl>, .disc_lys <dbl>, .disc_qalys <dbl>,
      #   .disc_med_cost <dbl>, .disc_pf_care_cost <dbl>, .disc_pp_care_cost <dbl>,
      #   .disc_cost_hc <dbl>
      
      $`Results - Trace`
      # A tibble: 482 × 8
         model_day model_week model_month model_year series    PF      PP    DEAD
             <dbl>      <dbl>       <dbl>      <dbl> <chr>  <dbl>   <dbl>   <dbl>
       1       0         0           0        0      CHEMO  1     0       0      
       2      30.4       4.35        1        0.0833 CHEMO  0.988 0.00254 0.00924
       3      60.8       8.69        2        0.167  CHEMO  0.973 0.00715 0.0197 
       4      91.3      13.0         3        0.25   CHEMO  0.957 0.0127  0.0306 
       5     122.       17.4         4        0.333  CHEMO  0.939 0.0189  0.0418 
       6     152.       21.7         5.00     0.417  CHEMO  0.922 0.0254  0.0531 
       7     182.       26.1         6.00     0.500  CHEMO  0.903 0.0323  0.0645 
       8     213.       30.4         7.00     0.583  CHEMO  0.885 0.0393  0.0759 
       9     243.       34.8         8.00     0.667  CHEMO  0.866 0.0464  0.0874 
      10     274.       39.1         9.00     0.750  CHEMO  0.847 0.0537  0.0988 
      # ℹ 472 more rows
      
      $`Results - Trace (Corrected)`
      # A tibble: 480 × 8
         model_day model_week model_month model_year series    PF      PP    DEAD
             <dbl>      <dbl>       <dbl>      <dbl> <chr>  <dbl>   <dbl>   <dbl>
       1      30.4       4.35        1        0.0833 CHEMO  0.994 0.00127 0.00462
       2      60.8       8.69        2        0.167  CHEMO  0.981 0.00484 0.0145 
       3      91.3      13.0         3        0.25   CHEMO  0.965 0.00993 0.0252 
       4     122.       17.4         4        0.333  CHEMO  0.948 0.0158  0.0362 
       5     152.       21.7         5.00     0.417  CHEMO  0.930 0.0221  0.0474 
       6     182.       26.1         6.00     0.500  CHEMO  0.912 0.0288  0.0588 
       7     213.       30.4         7.00     0.583  CHEMO  0.894 0.0358  0.0702 
       8     243.       34.8         8.00     0.667  CHEMO  0.875 0.0429  0.0816 
       9     274.       39.1         9.00     0.750  CHEMO  0.857 0.0501  0.0931 
      10     304.       43.5        10        0.833  CHEMO  0.838 0.0573  0.105  
      # ℹ 470 more rows
      
      $`Results - Outcomes`
      # A tibble: 32 × 5
         outcome series           group    disc   value
         <chr>   <chr>            <chr>    <lgl>  <dbl>
       1 lys     CHEMO            pf_ly    TRUE   2.93 
       2 lys     TARGET           pf_ly    TRUE   3.97 
       3 lys     TARGET vs. CHEMO pf_ly    TRUE   1.04 
       4 lys     CHEMO vs. TARGET pf_ly    TRUE  -1.04 
       5 lys     CHEMO            pp_ly    TRUE   1.94 
       6 lys     TARGET           pp_ly    TRUE   2.06 
       7 lys     TARGET vs. CHEMO pp_ly    TRUE   0.125
       8 lys     CHEMO vs. TARGET pp_ly    TRUE  -0.125
       9 qalys   CHEMO            pf_qalys TRUE   2.40 
      10 qalys   TARGET           pf_qalys TRUE   3.25 
      # ℹ 22 more rows
      
      $`Results - Costs`
      # A tibble: 24 × 5
         outcome series           group        disc     value
         <chr>   <chr>            <chr>        <lgl>    <dbl>
       1 cost_hc CHEMO            med_cost     TRUE    70291.
       2 cost_hc TARGET           med_cost     TRUE   476060.
       3 cost_hc TARGET vs. CHEMO med_cost     TRUE   405769.
       4 cost_hc CHEMO vs. TARGET med_cost     TRUE  -405769.
       5 cost_hc CHEMO            pf_care_cost TRUE    35146.
       6 cost_hc TARGET           pf_care_cost TRUE    47606.
       7 cost_hc TARGET vs. CHEMO pf_care_cost TRUE    12460.
       8 cost_hc CHEMO vs. TARGET pf_care_cost TRUE   -12460.
       9 cost_hc CHEMO            pp_care_cost TRUE    46441.
      10 cost_hc TARGET           pp_care_cost TRUE    49448.
      # ℹ 14 more rows
      
      $`Results - CE`
      # A tibble: 4 × 11
        hsumm    esumm health_outcome econ_outcome series   cost   eff   dcost deffect
        <chr>    <chr> <chr>          <chr>        <chr>   <dbl> <dbl>   <dbl>   <dbl>
      1 .disc_l… .dis… .disc_lys      .disc_cost_… CHEMO  1.52e5  4.86     NA   NA    
      2 .disc_l… .dis… .disc_lys      .disc_cost_… TARGET 5.73e5  6.03 421236.   1.16 
      3 .disc_q… .dis… .disc_qalys    .disc_cost_… CHEMO  1.52e5  3.72     NA   NA    
      4 .disc_q… .dis… .disc_qalys    .disc_cost_… TARGET 5.73e5  4.65 421236.   0.937
      # ℹ 2 more variables: dref <chr>, icer <dbl>
      
      $`Results - NMB`
      # A tibble: 14 × 6
         outcome series           group        disc  type        value
         <chr>   <chr>            <chr>        <lgl> <chr>       <dbl>
       1 lys     TARGET vs. CHEMO pf_ly        TRUE  health    103836.
       2 lys     CHEMO vs. TARGET pf_ly        TRUE  health   -103836.
       3 lys     TARGET vs. CHEMO pp_ly        TRUE  health     12530.
       4 lys     CHEMO vs. TARGET pp_ly        TRUE  health    -12530.
       5 qalys   TARGET vs. CHEMO pf_qalys     TRUE  health    127718.
       6 qalys   CHEMO vs. TARGET pf_qalys     TRUE  health   -127718.
       7 qalys   TARGET vs. CHEMO pp_qalys     TRUE  health     12781.
       8 qalys   CHEMO vs. TARGET pp_qalys     TRUE  health    -12781.
       9 cost_hc TARGET vs. CHEMO med_cost     TRUE  economic -405769.
      10 cost_hc CHEMO vs. TARGET med_cost     TRUE  economic  405769.
      11 cost_hc TARGET vs. CHEMO pf_care_cost TRUE  economic  -12460.
      12 cost_hc CHEMO vs. TARGET pf_care_cost TRUE  economic   12460.
      13 cost_hc TARGET vs. CHEMO pp_care_cost TRUE  economic   -3007.
      14 cost_hc CHEMO vs. TARGET pp_care_cost TRUE  economic    3007.
      

---

    Code
      exported
    Output
      $`Inputs - Settings`
      # A tibble: 4 × 2
        setting      value           
        <chr>        <chr>           
      1 disc_cost    0.03            
      2 disc_eff     0.03            
      3 cycle_length 30.4166666666667
      4 n_cycles     240             
      
      $`Inputs - Strategies`
      # A tibble: 2 × 2
        name   desc            
        <chr>  <chr>           
      1 CHEMO  Chemotherapy    
      2 TARGET Targeted Therapy
      
      $`Inputs - States`
      # A tibble: 3 × 4
        name  desc              prob limit
        <chr> <chr>            <dbl> <dbl>
      1 PF    Progression-Free     1     0
      2 PP    Post-Progression     0     0
      3 DEAD  Dead                 0     0
      
      $`Inputs - Transitions`
      # A tibble: 4 × 4
        strategy endpoint cycle_length value     
        <chr>    <chr>           <dbl> <chr>     
      1 CHEMO    PFS                 1 pfs_chemo 
      2 TARGET   PFS                 1 pfs_target
      3 CHEMO    OS                  1 os_chemo  
      4 TARGET   OS                  1 os_target 
      
      $`Inputs - Health Values`
      # A tibble: 4 × 5
        name     description                 strategy state value             
        <chr>    <chr>                       <chr>    <chr> <chr>             
      1 pf_ly    Progression-free life-years All      PF    cycle_length_years
      2 pp_ly    Post-progression life-years All      PP    cycle_length_years
      3 pf_qalys Progression-free QALYs      All      PF    pf_ly * pfs_util  
      4 pp_qalys Post-Progression QALYs      All      PP    pp_ly * pps_util  
      
      $`Inputs - Econ Values`
      # A tibble: 4 × 5
        name         description                         strategy state value         
        <chr>        <chr>                               <chr>    <chr> <chr>         
      1 med_cost     Medication cost                     CHEMO    PF    chemo_cost * …
      2 pf_care_cost Routine care cost, pre-progression  All      PF    cycle_length_…
      3 med_cost     Label...                            TARGET   PF    target_cost *…
      4 pp_care_cost Routine care cost, post-progression All      PP    cycle_length_…
      
      $`Inputs - Health Summ`
      # A tibble: 4 × 4
        name  description value       wtp
        <chr> <chr>       <chr>     <dbl>
      1 lys   Life-Years  pf_ly    100000
      2 lys   Life-Years  pp_ly    100000
      3 qalys QALYs       pf_qalys 150000
      4 qalys QALYs       pp_qalys 150000
      
      $`Inputs - Econ Summ`
      # A tibble: 3 × 3
        name    description             value       
        <chr>   <chr>                   <chr>       
      1 cost_hc Healthcare system costs med_cost    
      2 cost_hc Label...                pf_care_cost
      3 cost_hc Label...                pp_care_cost
      
      $`Inputs - Parameters`
      # A tibble: 12 × 6
         name          desc                       value low       high      psa       
         <chr>         <chr>                      <chr> <chr>     <chr>     <chr>     
       1 pfs_shape     PFS shape parameter, chemo 1.2   bc * 0.75 bc * 1.25 "lognorma…
       2 pfs_scale     PFS scale parameter, chemo 40.3  bc * 0.75 bc * 1.25 "lognorma…
       3 pfs_hr_target PFS HR, target             0.67  0.6       0.8       "lognorma…
       4 os_shape      OS shape parameter, chemo  1.1   0.95      1.34      "lognorma…
       5 os_scale      OS scale parameter, chemo  70.4  bc * 0.75 bc * 1.25 "lognorma…
       6 os_hr_target  OS HR, target              0.74  bc * 0.75 bc * 1.25 "lognorma…
       7 chemo_cost    Cost per month, chemo      2000  bc - 500  bc + 500  ""        
       8 target_cost   Cost per month, target     10000 bc - 3000 bc + 3000 ""        
       9 pfs_cost      Cost per month, PFS        1000  bc - 700  bc + 700  "normal(m…
      10 pps_cost      Cost per month, PPS        2000  0         10000     "normal(m…
      11 pfs_util      Utility value, PFS         0.82  bc - 0.05 bc + 0.05 "lognorma…
      12 pps_util      Utility value, PPS         0.68  bc - 0.1  bc + 0.1  "lognorma…
      
      $`Inputs - Surv Dists`
      # A tibble: 4 × 2
        name       value                                                      
        <chr>      <chr>                                                      
      1 pfs_chemo  "define_survival(dist = \"weibull\", pfs_shape, pfs_scale)"
      2 os_chemo   "define_survival(dist = \"weibull\", os_shape, os_scale)"  
      3 pfs_target "apply_hr(pfs_chemo, pfs_hr_target)"                       
      4 os_target  "apply_hr(os_chemo, os_hr_target)"                         
      
      $`Calc - Params`
      # A tibble: 480 × 31
         strategy group        state_time cycle model_time cycle_length_days
         <chr>    <chr>             <dbl> <dbl>      <dbl>             <dbl>
       1 CHEMO    All Patients          1     1          1              30.4
       2 CHEMO    All Patients          1     2          2              30.4
       3 CHEMO    All Patients          1     3          3              30.4
       4 CHEMO    All Patients          1     4          4              30.4
       5 CHEMO    All Patients          1     5          5              30.4
       6 CHEMO    All Patients          1     6          6              30.4
       7 CHEMO    All Patients          1     7          7              30.4
       8 CHEMO    All Patients          1     8          8              30.4
       9 CHEMO    All Patients          1     9          9              30.4
      10 CHEMO    All Patients          1    10         10              30.4
      # ℹ 470 more rows
      # ℹ 25 more variables: cycle_length_weeks <dbl>, cycle_length_months <dbl>,
      #   cycle_length_years <dbl>, model_day <dbl>, model_week <dbl>,
      #   model_month <dbl>, model_year <dbl>, state_day <dbl>, state_week <dbl>,
      #   state_month <dbl>, state_year <dbl>, disc_h <dbl>, disc_e <dbl>,
      #   pfs_shape <dbl>, pfs_scale <dbl>, pfs_hr_target <dbl>, os_shape <dbl>,
      #   os_scale <dbl>, os_hr_target <dbl>, chemo_cost <dbl>, target_cost <dbl>, …
      
      $`Calc - Trans`
      # A tibble: 482 × 5
         strategy group        cycle   pfs    os
         <chr>    <chr>        <dbl> <dbl> <dbl>
       1 CHEMO    All Patients     0 1     1    
       2 CHEMO    All Patients     1 0.988 0.991
       3 CHEMO    All Patients     2 0.973 0.980
       4 CHEMO    All Patients     3 0.957 0.969
       5 CHEMO    All Patients     4 0.939 0.958
       6 CHEMO    All Patients     5 0.922 0.947
       7 CHEMO    All Patients     6 0.903 0.936
       8 CHEMO    All Patients     7 0.885 0.924
       9 CHEMO    All Patients     8 0.866 0.913
      10 CHEMO    All Patients     9 0.847 0.901
      # ℹ 472 more rows
      
      $`Calc - Unit Values`
      # A tibble: 1,440 × 24
         strategy group cycle state pf_ly pp_ly pf_qalys pp_qalys   lys qalys med_cost
         <chr>    <chr> <dbl> <chr> <dbl> <dbl>    <dbl>    <dbl> <dbl> <dbl>    <dbl>
       1 CHEMO    All …     1 DEAD      0     0        0        0     0     0        0
       2 CHEMO    All …     2 DEAD      0     0        0        0     0     0        0
       3 CHEMO    All …     3 DEAD      0     0        0        0     0     0        0
       4 CHEMO    All …     4 DEAD      0     0        0        0     0     0        0
       5 CHEMO    All …     5 DEAD      0     0        0        0     0     0        0
       6 CHEMO    All …     6 DEAD      0     0        0        0     0     0        0
       7 CHEMO    All …     7 DEAD      0     0        0        0     0     0        0
       8 CHEMO    All …     8 DEAD      0     0        0        0     0     0        0
       9 CHEMO    All …     9 DEAD      0     0        0        0     0     0        0
      10 CHEMO    All …    10 DEAD      0     0        0        0     0     0        0
      # ℹ 1,430 more rows
      # ℹ 13 more variables: pf_care_cost <dbl>, pp_care_cost <dbl>, cost_hc <dbl>,
      #   .disc_pf_ly <dbl>, .disc_pp_ly <dbl>, .disc_pf_qalys <dbl>,
      #   .disc_pp_qalys <dbl>, .disc_lys <dbl>, .disc_qalys <dbl>,
      #   .disc_med_cost <dbl>, .disc_pf_care_cost <dbl>, .disc_pp_care_cost <dbl>,
      #   .disc_cost_hc <dbl>
      
      $`Calc - Values`
      # A tibble: 480 × 23
         strategy group  cycle  pf_ly   pp_ly pf_qalys pp_qalys    lys  qalys med_cost
         <chr>    <chr>  <dbl>  <dbl>   <dbl>    <dbl>    <dbl>  <dbl>  <dbl>    <dbl>
       1 CHEMO    All P…     1 0.0828 1.06e-4   0.0679  7.19e-5 0.0829 0.0680    1988.
       2 CHEMO    All P…     2 0.0817 4.04e-4   0.0670  2.75e-4 0.0821 0.0673    1961.
       3 CHEMO    All P…     3 0.0804 8.27e-4   0.0659  5.63e-4 0.0812 0.0665    1930.
       4 CHEMO    All P…     4 0.0790 1.32e-3   0.0648  8.94e-4 0.0803 0.0657    1896.
       5 CHEMO    All P…     5 0.0775 1.85e-3   0.0636  1.25e-3 0.0794 0.0648    1861.
       6 CHEMO    All P…     6 0.0760 2.40e-3   0.0623  1.63e-3 0.0784 0.0640    1825.
       7 CHEMO    All P…     7 0.0745 2.98e-3   0.0611  2.03e-3 0.0775 0.0631    1788.
       8 CHEMO    All P…     8 0.0730 3.57e-3   0.0598  2.43e-3 0.0765 0.0623    1751.
       9 CHEMO    All P…     9 0.0714 4.17e-3   0.0586  2.84e-3 0.0756 0.0614    1714.
      10 CHEMO    All P…    10 0.0698 4.77e-3   0.0573  3.25e-3 0.0746 0.0605    1676.
      # ℹ 470 more rows
      # ℹ 13 more variables: pf_care_cost <dbl>, pp_care_cost <dbl>, cost_hc <dbl>,
      #   .disc_pf_ly <dbl>, .disc_pp_ly <dbl>, .disc_pf_qalys <dbl>,
      #   .disc_pp_qalys <dbl>, .disc_lys <dbl>, .disc_qalys <dbl>,
      #   .disc_med_cost <dbl>, .disc_pf_care_cost <dbl>, .disc_pp_care_cost <dbl>,
      #   .disc_cost_hc <dbl>
      
      $`Results - Trace`
      # A tibble: 482 × 8
         model_day model_week model_month model_year series    PF      PP    DEAD
             <dbl>      <dbl>       <dbl>      <dbl> <chr>  <dbl>   <dbl>   <dbl>
       1       0         0           0        0      CHEMO  1     0       0      
       2      30.4       4.35        1        0.0833 CHEMO  0.988 0.00254 0.00924
       3      60.8       8.69        2        0.167  CHEMO  0.973 0.00715 0.0197 
       4      91.3      13.0         3        0.25   CHEMO  0.957 0.0127  0.0306 
       5     122.       17.4         4        0.333  CHEMO  0.939 0.0189  0.0418 
       6     152.       21.7         5.00     0.417  CHEMO  0.922 0.0254  0.0531 
       7     182.       26.1         6.00     0.500  CHEMO  0.903 0.0323  0.0645 
       8     213.       30.4         7.00     0.583  CHEMO  0.885 0.0393  0.0759 
       9     243.       34.8         8.00     0.667  CHEMO  0.866 0.0464  0.0874 
      10     274.       39.1         9.00     0.750  CHEMO  0.847 0.0537  0.0988 
      # ℹ 472 more rows
      
      $`Results - Trace (Corrected)`
      # A tibble: 480 × 8
         model_day model_week model_month model_year series    PF      PP    DEAD
             <dbl>      <dbl>       <dbl>      <dbl> <chr>  <dbl>   <dbl>   <dbl>
       1      30.4       4.35        1        0.0833 CHEMO  0.994 0.00127 0.00462
       2      60.8       8.69        2        0.167  CHEMO  0.981 0.00484 0.0145 
       3      91.3      13.0         3        0.25   CHEMO  0.965 0.00993 0.0252 
       4     122.       17.4         4        0.333  CHEMO  0.948 0.0158  0.0362 
       5     152.       21.7         5.00     0.417  CHEMO  0.930 0.0221  0.0474 
       6     182.       26.1         6.00     0.500  CHEMO  0.912 0.0288  0.0588 
       7     213.       30.4         7.00     0.583  CHEMO  0.894 0.0358  0.0702 
       8     243.       34.8         8.00     0.667  CHEMO  0.875 0.0429  0.0816 
       9     274.       39.1         9.00     0.750  CHEMO  0.857 0.0501  0.0931 
      10     304.       43.5        10        0.833  CHEMO  0.838 0.0573  0.105  
      # ℹ 470 more rows
      
      $`Results - Outcomes`
      # A tibble: 32 × 5
         outcome series           group    disc   value
         <chr>   <chr>            <chr>    <lgl>  <dbl>
       1 lys     CHEMO            pf_ly    TRUE   2.93 
       2 lys     TARGET           pf_ly    TRUE   3.97 
       3 lys     TARGET vs. CHEMO pf_ly    TRUE   1.04 
       4 lys     CHEMO vs. TARGET pf_ly    TRUE  -1.04 
       5 lys     CHEMO            pp_ly    TRUE   1.94 
       6 lys     TARGET           pp_ly    TRUE   2.06 
       7 lys     TARGET vs. CHEMO pp_ly    TRUE   0.125
       8 lys     CHEMO vs. TARGET pp_ly    TRUE  -0.125
       9 qalys   CHEMO            pf_qalys TRUE   2.40 
      10 qalys   TARGET           pf_qalys TRUE   3.25 
      # ℹ 22 more rows
      
      $`Results - Costs`
      # A tibble: 24 × 5
         outcome series           group        disc     value
         <chr>   <chr>            <chr>        <lgl>    <dbl>
       1 cost_hc CHEMO            med_cost     TRUE    70291.
       2 cost_hc TARGET           med_cost     TRUE   476060.
       3 cost_hc TARGET vs. CHEMO med_cost     TRUE   405769.
       4 cost_hc CHEMO vs. TARGET med_cost     TRUE  -405769.
       5 cost_hc CHEMO            pf_care_cost TRUE    35146.
       6 cost_hc TARGET           pf_care_cost TRUE    47606.
       7 cost_hc TARGET vs. CHEMO pf_care_cost TRUE    12460.
       8 cost_hc CHEMO vs. TARGET pf_care_cost TRUE   -12460.
       9 cost_hc CHEMO            pp_care_cost TRUE    46441.
      10 cost_hc TARGET           pp_care_cost TRUE    49448.
      # ℹ 14 more rows
      
      $`Results - CE`
      # A tibble: 4 × 11
        hsumm    esumm health_outcome econ_outcome series   cost   eff   dcost deffect
        <chr>    <chr> <chr>          <chr>        <chr>   <dbl> <dbl>   <dbl>   <dbl>
      1 .disc_l… .dis… .disc_lys      .disc_cost_… CHEMO  1.52e5  4.86     NA   NA    
      2 .disc_l… .dis… .disc_lys      .disc_cost_… TARGET 5.73e5  6.03 421236.   1.16 
      3 .disc_q… .dis… .disc_qalys    .disc_cost_… CHEMO  1.52e5  3.72     NA   NA    
      4 .disc_q… .dis… .disc_qalys    .disc_cost_… TARGET 5.73e5  4.65 421236.   0.937
      # ℹ 2 more variables: dref <chr>, icer <dbl>
      
      $`Results - NMB`
      # A tibble: 14 × 6
         outcome series           group        disc  type        value
         <chr>   <chr>            <chr>        <lgl> <chr>       <dbl>
       1 lys     TARGET vs. CHEMO pf_ly        TRUE  health    103836.
       2 lys     CHEMO vs. TARGET pf_ly        TRUE  health   -103836.
       3 lys     TARGET vs. CHEMO pp_ly        TRUE  health     12530.
       4 lys     CHEMO vs. TARGET pp_ly        TRUE  health    -12530.
       5 qalys   TARGET vs. CHEMO pf_qalys     TRUE  health    127718.
       6 qalys   CHEMO vs. TARGET pf_qalys     TRUE  health   -127718.
       7 qalys   TARGET vs. CHEMO pp_qalys     TRUE  health     12781.
       8 qalys   CHEMO vs. TARGET pp_qalys     TRUE  health    -12781.
       9 cost_hc TARGET vs. CHEMO med_cost     TRUE  economic -405769.
      10 cost_hc CHEMO vs. TARGET med_cost     TRUE  economic  405769.
      11 cost_hc TARGET vs. CHEMO pf_care_cost TRUE  economic  -12460.
      12 cost_hc CHEMO vs. TARGET pf_care_cost TRUE  economic   12460.
      13 cost_hc TARGET vs. CHEMO pp_care_cost TRUE  economic   -3007.
      14 cost_hc CHEMO vs. TARGET pp_care_cost TRUE  economic    3007.
      

# Custom PSM produces correct results.

    WAoAAAACAAQCAQACAwAAAAMTAAAACwAAABAAAAAYAAQACQAAAANseXMABAAJAAAAA2x5cwAE
    AAkAAAADbHlzAAQACQAAAANseXMABAAJAAAAA2x5cwAEAAkAAAADbHlzAAQACQAAAANseXMA
    BAAJAAAAA2x5cwAEAAkAAAADbHlzAAQACQAAAANseXMABAAJAAAAA2x5cwAEAAkAAAADbHlz
    AAQACQAAAAVxYWx5cwAEAAkAAAAFcWFseXMABAAJAAAABXFhbHlzAAQACQAAAAVxYWx5cwAE
    AAkAAAAFcWFseXMABAAJAAAABXFhbHlzAAQACQAAAAVxYWx5cwAEAAkAAAAFcWFseXMABAAJ
    AAAABXFhbHlzAAQACQAAAAVxYWx5cwAEAAkAAAAFcWFseXMABAAJAAAABXFhbHlzAAAAEAAA
    ABgABAAJAAAAB2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMABAAJAAAA
    B2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMA
    BAAJAAAAB2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMABAAJAAAAB2Nv
    c3RfaGMABAAJAAAAB2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMABAAJ
    AAAAB2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMABAAJAAAAB2Nvc3Rf
    aGMABAAJAAAAB2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMABAAJAAAA
    B2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMAAAAQAAAAGAAEAAkAAAAN
    Y2hjayB2cy4gY2hwbAAEAAkAAAANY2hjayB2cy4gbXlmbwAEAAkAAAANY2hjayB2cy4gcmxw
    cwAEAAkAAAANY2hwbCB2cy4gY2hjawAEAAkAAAANY2hwbCB2cy4gbXlmbwAEAAkAAAANY2hw
    bCB2cy4gcmxwcwAEAAkAAAANbXlmbyB2cy4gY2hjawAEAAkAAAANbXlmbyB2cy4gY2hwbAAE
    AAkAAAANbXlmbyB2cy4gcmxwcwAEAAkAAAANcmxwcyB2cy4gY2hjawAEAAkAAAANcmxwcyB2
    cy4gY2hwbAAEAAkAAAANcmxwcyB2cy4gbXlmbwAEAAkAAAANY2hjayB2cy4gY2hwbAAEAAkA
    AAANY2hjayB2cy4gbXlmbwAEAAkAAAANY2hjayB2cy4gcmxwcwAEAAkAAAANY2hwbCB2cy4g
    Y2hjawAEAAkAAAANY2hwbCB2cy4gbXlmbwAEAAkAAAANY2hwbCB2cy4gcmxwcwAEAAkAAAAN
    bXlmbyB2cy4gY2hjawAEAAkAAAANbXlmbyB2cy4gY2hwbAAEAAkAAAANbXlmbyB2cy4gcmxw
    cwAEAAkAAAANcmxwcyB2cy4gY2hjawAEAAkAAAANcmxwcyB2cy4gY2hwbAAEAAkAAAANcmxw
    cyB2cy4gbXlmbwAAABAAAAAYAAQACQAAAARjaGNrAAQACQAAAARjaGNrAAQACQAAAARjaGNr
    AAQACQAAAARjaHBsAAQACQAAAARjaHBsAAQACQAAAARjaHBsAAQACQAAAARteWZvAAQACQAA
    AARteWZvAAQACQAAAARteWZvAAQACQAAAARybHBzAAQACQAAAARybHBzAAQACQAAAARybHBz
    AAQACQAAAARjaGNrAAQACQAAAARjaGNrAAQACQAAAARjaGNrAAQACQAAAARjaHBsAAQACQAA
    AARjaHBsAAQACQAAAARjaHBsAAQACQAAAARteWZvAAQACQAAAARteWZvAAQACQAAAARteWZv
    AAQACQAAAARybHBzAAQACQAAAARybHBzAAQACQAAAARybHBzAAAAEAAAABgABAAJAAAABGNo
    cGwABAAJAAAABG15Zm8ABAAJAAAABHJscHMABAAJAAAABGNoY2sABAAJAAAABG15Zm8ABAAJ
    AAAABHJscHMABAAJAAAABGNoY2sABAAJAAAABGNocGwABAAJAAAABHJscHMABAAJAAAABGNo
    Y2sABAAJAAAABGNocGwABAAJAAAABG15Zm8ABAAJAAAABGNocGwABAAJAAAABG15Zm8ABAAJ
    AAAABHJscHMABAAJAAAABGNoY2sABAAJAAAABG15Zm8ABAAJAAAABHJscHMABAAJAAAABGNo
    Y2sABAAJAAAABGNocGwABAAJAAAABHJscHMABAAJAAAABGNoY2sABAAJAAAABGNocGwABAAJ
    AAAABG15Zm8AAAAOAAAAGEAHxzyRIKluQAfHPJEgqW5AB8c8kSCpbkACVNp4Mgn3QAJU2ngy
    CfdAAlTaeDIJ90AA+EHx9mIZQAD4QfH2YhlAAPhB8fZiGUAI5tP/3q2GQAjm0//erYZACObT
    /96thkAAI4v31EM3QAAji/fUQzdAACOL99RDNz/4m3aBXaNWP/ibdoFdo1Y/+Jt2gV2jVj/2
    sI4lyuqRP/awjiXK6pE/9rCOJcrqkUAA7gvQzZXVQADuC9DNldVAAO4L0M2V1QAAAA4AAAAY
    QPW6dCSbEYZA9bp0JJsRhkD1unQkmxGGQOqMMhpVXxNA6owyGlVfE0DqjDIaVV8TQOp/Yecr
    1alA6n9h5yvVqUDqf2HnK9WpQPlHawAZzItA+UdrABnMi0D5R2sAGcyLQPW6dCSbEYZA9bp0
    JJsRhkD1unQkmxGGQOqMMhpVXxNA6owyGlVfE0DqjDIaVV8TQOp/Yecr1alA6n9h5yvVqUDq
    f2HnK9WpQPlHawAZzItA+UdrABnMi0D5R2sAGcyLAAAADgAAABg/5cmIY7p93D/rO+p8qR1U
    v8H5duvgQYC/5cmIY7p93D/FyYhjun3gv+pH5h6yjjy/6zvqfKkdVL/FyYhjun3gv++6SDeh
    LbQ/wfl26+BBgD/qR+Yeso48P++6SDehLbQ/3q6FuSuMYD/jLROTuze6v7lP+x8qU8C/3q6F
    uSuMYD++roW5K4xQv+KBQkB7EKi/4y0Tk7s3ur++roW5K4xQv+ZXEveggjI/uU/7HypTwD/i
    gUJAexCoP+ZXEveggjIAAAAOAAAAGEDg6LYu4MP5QOD1hmIKTWPAzGe22/XYKMDg6LYu4MP5
    QFmgZlMS1ADA6AKj5d46A8Dg9YZiCk1jwFmgZlMS1ADA6A90GQfDbUDMZ7bb9dgoQOgCo+Xe
    OgNA6A90GQfDbUDg6LYu4MP5QOD1hmIKTWPAzGe22/XYKMDg6LYu4MP5QFmgZlMS1ADA6AKj
    5d46A8Dg9YZiCk1jwFmgZlMS1ADA6A90GQfDbUDMZ7bb9dgoQOgCo+XeOgNA6A90GQfDbQAA
    AA4AAAAYQOjVy1TXqatA4+1Kf9aIKcD5SO0vPD4+wOjVy1TXqatAgtHK9ICJYcDtPD/5o5Ne
    wOPtSn/WiCnAgtHK9ICJYcDoRFLKOC6gQPlI7S88Pj5A7Tw/+aOTXkDoRFLKOC6gQPGisv9E
    d2BA7EzmnVm1eMEB9HSEWTSFwPGisv9Ed2BAirpGJzQR+8D0wpm/4ookwOxM5p1ZtXjAirpG
    JzQR+8DxO2ZQ2Zb9QQH0dIRZNIVA9MKZv+KKJEDxO2ZQ2Zb9AAAAEAAAABgABAAJAAAABTUw
    ODYyAAQACQAAAAU0MDgxMAAEAAkAAAAHMTAzNTY3KgAEAAkAAAAGNTA4NjIqAAQACQAAAAM2
    MDIABAAJAAAABjU5ODc0KgAEAAkAAAAGNDA4MTAqAAQACQAAAAQ2MDIqAAQACQAAAAY0OTY5
    OSoABAAJAAAABjEwMzU2NwAEAAkAAAAFNTk4NzQABAAJAAAABTQ5Njk5AAQACQAAAAU3MjIz
    NQAEAAkAAAAFNTc5NTkABAAJAAAABzE0NzA4NyoABAAJAAAABjcyMjM1KgAEAAkAAAADODU1
    AAQACQAAAAY4NTAzNCoABAAJAAAABjU3OTU5KgAEAAkAAAAEODU1KgAEAAkAAAAGNzA1ODIq
    AAQACQAAAAYxNDcwODcABAAJAAAABTg1MDM0AAQACQAAAAU3MDU4MgAABAIAAAABAAQACQAA
    AAVuYW1lcwAAABAAAAALAAQACQAAAAZoZWFsdGgABAAJAAAABGVjb24ABAAJAAAABnNlcmll
    cwAEAAkAAAAIcmVmZXJlbnQABAAJAAAACmNvbXBhcmF0b3IABAAJAAAABmVmZmVjdAAEAAkA
    AAAEY29zdAAEAAkAAAAHZGVmZmVjdAAEAAkAAAAFZGNvc3QABAAJAAAABGljZXIABAAJAAAA
    C2ljZXJfc3RyaW5nAAAEAgAAAAEABAAJAAAACXJvdy5uYW1lcwAAAA0AAAACgAAAAP///+gA
    AAQCAAAAAQAEAAkAAAAFY2xhc3MAAAAQAAAAAQAEAAkAAAAKZGF0YS5mcmFtZQAAAP4=

---

    Code
      exported
    Output
      $`Inputs - Settings`
      # A tibble: 4 × 2
        setting      value
        <chr>        <chr>
      1 disc_cost    0.035
      2 disc_eff     0.035
      3 cycle_length 365  
      4 n_cycles     20   
      
      $`Inputs - Strategies`
      # A tibble: 4 × 2
        name  desc       
        <chr> <chr>      
      1 myfo  Myfosfamide
      2 chpl  Chemoplatin
      3 chck  Checkimab  
      4 rlps  Relapsinib 
      
      $`Inputs - States`
      # A tibble: 4 × 4
        name  desc       prob                                                    limit
        <chr> <chr>      <chr>                                                   <dbl>
      1 RESP  Response   dispatch_strategy(myfo = rr_myfo, chpl = rr_chpl, chck…     0
      2 REL   Relapse    0                                                           0
      3 REF   Refractory C                                                           0
      4 DEAD  Dead       0                                                           0
      
      $`Inputs - Transitions`
      # A tibble: 4 × 5
        strategy state formula                                             id    value
        <chr>    <chr> <chr>                                               <chr> <chr>
      1 All      RESP  rr * surv_prob(rfs, model_month)                    6e50… rr *…
      2 All      REL   rr * (surv_prob(os_resp, model_month) - surv_prob(… aefc… rr *…
      3 All      REF   (1 - rr) * surv_prob(os_nresp, model_month)         d1f8… (1 -…
      4 All      DEAD  C                                                   8c1d… C    
      
      $`Inputs - Health Values`
      # A tibble: 6 × 5
        name        description               strategy state value                    
        <chr>       <chr>                     <chr>    <chr> <chr>                    
      1 ly_resp     Life Years in Response    All      RESP  cycle_length_years       
      2 ly_nresp    Life Years in Nonresponse All      REL   cycle_length_years       
      3 qalys_resp  QALYs in Response         All      RESP  util_resp * cycle_length…
      4 qalys_nresp QALYs in Nonresponse      All      REL   util_rel * cycle_length_…
      5 ly_nresp    Label...                  All      REF   cycle_length_years       
      6 qalys_nresp Label...                  All      REF   util_ref * cycle_length_…
      
      $`Inputs - Econ Values`
      # A tibble: 7 × 5
        name       description               strategy state value                     
        <chr>      <chr>                     <chr>    <chr> <chr>                     
      1 cost_med   Medication cost           myfo     RESP  ucost_myfo * cycle_length…
      2 cost_resp  Routine care, response    All      All   ucost_resp * cycle_length…
      3 cost_nresp Routine care, nonresponse All      All   ucost_nresp * cycle_lengt…
      4 cost_trans Transplant                All      REF   rescale_prob(p_transplant…
      5 cost_med   Label...                  chpl     RESP  ucost_chpl * cycle_length…
      6 cost_med   Label...                  chck     RESP  ucost_chck * cycle_length…
      7 cost_med   Label...                  rlps     RESP  ucost_rlps * cycle_length…
      
      $`Inputs - Health Summ`
      # A tibble: 4 × 4
        name  description                 value          wtp
        <chr> <chr>                       <chr>        <dbl>
      1 lys   Life-years                  ly_resp      20000
      2 lys   Life-years                  ly_nresp     20000
      3 qalys Quality-adjusted life-years qalys_resp   30000
      4 qalys Label...                    qalys_nresp 100000
      
      $`Inputs - Econ Summ`
      # A tibble: 4 × 3
        name    description             value     
        <chr>   <chr>                   <chr>     
      1 cost_hc Healthcare system costs cost_med  
      2 cost_hc Label...                cost_resp 
      3 cost_hc Label...                cost_nresp
      4 cost_hc Label...                cost_trans
      
      $`Inputs - Parameters`
      # A tibble: 21 × 6
         name            desc                       value            low   high  psa  
         <chr>           <chr>                      <chr>            <chr> <chr> <chr>
       1 rr_myfo         Response rate, myfosfamide 0.31             "bc … "bc … "bin…
       2 rr_chpl         Response rate, chemoplatin 0.35             "bc … "bc … "bin…
       3 rr_chck         Response rate, checkimab   0.51             "bc … "bc … "bin…
       4 rr_rlps         Response rate, relapsinib  0.543            "bc … "bc … "bin…
       5 rr              Relapse rate               dispatch_strate… ""    ""    ""   
       6 rfs_scale       Scale parameter, RFS       6                "5.4… "7.1" "log…
       7 rfs_shape       Shape parameter, RFS       0.89             "0.7… "1.0… "log…
       8 os_resp_meanlog log mean OS, responders    4                "3.4… "4.5… "nor…
       9 os_resp_sdlog   log SD OS, responders      0.71             "0.5… "0.9… "log…
      10 os_nresp_rate   Death rate, nonresponders  0.12             "0.0… "0.1… "log…
      # ℹ 11 more rows
      
      $`Inputs - Surv Dists`
      # A tibble: 3 × 2
        name     value                                                                
        <chr>    <chr>                                                                
      1 rfs      "define_survival(shape = rfs_shape, scale = rfs_scale, dist = \"weib…
      2 os_resp  "define_survival(meanlog = os_resp_meanlog, sdlog = os_resp_sdlog, d…
      3 os_nresp "define_survival(rate = os_nresp_rate, dist = \"exp\")"              
      
      $`Calc - Params`
      # A tibble: 80 × 40
         strategy group        state_time cycle model_time cycle_length_days
         <chr>    <chr>             <dbl> <dbl>      <dbl>             <dbl>
       1 myfo     All Patients          1     1          1               365
       2 myfo     All Patients          1     2          2               365
       3 myfo     All Patients          1     3          3               365
       4 myfo     All Patients          1     4          4               365
       5 myfo     All Patients          1     5          5               365
       6 myfo     All Patients          1     6          6               365
       7 myfo     All Patients          1     7          7               365
       8 myfo     All Patients          1     8          8               365
       9 myfo     All Patients          1     9          9               365
      10 myfo     All Patients          1    10         10               365
      # ℹ 70 more rows
      # ℹ 34 more variables: cycle_length_weeks <dbl>, cycle_length_months <dbl>,
      #   cycle_length_years <dbl>, model_day <dbl>, model_week <dbl>,
      #   model_month <dbl>, model_year <dbl>, state_day <dbl>, state_week <dbl>,
      #   state_month <dbl>, state_year <dbl>, disc_h <dbl>, disc_e <dbl>,
      #   rr_myfo <dbl>, rr_chpl <dbl>, rr_chck <dbl>, rr_rlps <dbl>, rr <dbl>,
      #   rfs_scale <dbl>, rfs_shape <dbl>, os_resp_meanlog <dbl>, …
      
      $`Calc - Trans`
      # A tibble: 84 × 9
         model_day model_week model_month model_year series        RESP    REL     REF
             <dbl>      <dbl>       <dbl>      <dbl> <chr>        <dbl>  <dbl>   <dbl>
       1         0        0             0          0 myfo   0.31        0      6.9 e-1
       2       365       52.1          12          1 myfo   0.0486      0.256  1.63e-1
       3       730      104.           24          2 myfo   0.0100      0.262  3.87e-2
       4      1095      156.           36          3 myfo   0.00225     0.221  9.18e-3
       5      1460      209.           48          4 myfo   0.000534    0.177  2.17e-3
       6      1825      261.           60          5 myfo   0.000132    0.138  5.15e-4
       7      2190      313.           72          6 myfo   0.0000336   0.108  1.22e-4
       8      2555      365            84          7 myfo   0.00000877  0.0843 2.89e-5
       9      2920      417.           96          8 myfo   0.00000234  0.0661 6.85e-6
      10      3285      469.          108          9 myfo   0.000000636 0.0522 1.62e-6
      # ℹ 74 more rows
      # ℹ 1 more variable: DEAD <dbl>
      
      $`Calc - Unit Values`
      # A tibble: 320 × 26
         strategy group      cycle state ly_resp ly_nresp qalys_resp qalys_nresp   lys
         <chr>    <chr>      <dbl> <chr>   <dbl>    <dbl>      <dbl>       <dbl> <dbl>
       1 chck     All Patie…     1 DEAD        0        0          0           0     0
       2 chck     All Patie…     2 DEAD        0        0          0           0     0
       3 chck     All Patie…     3 DEAD        0        0          0           0     0
       4 chck     All Patie…     4 DEAD        0        0          0           0     0
       5 chck     All Patie…     5 DEAD        0        0          0           0     0
       6 chck     All Patie…     6 DEAD        0        0          0           0     0
       7 chck     All Patie…     7 DEAD        0        0          0           0     0
       8 chck     All Patie…     8 DEAD        0        0          0           0     0
       9 chck     All Patie…     9 DEAD        0        0          0           0     0
      10 chck     All Patie…    10 DEAD        0        0          0           0     0
      # ℹ 310 more rows
      # ℹ 17 more variables: qalys <dbl>, cost_med <dbl>, cost_resp <dbl>,
      #   cost_nresp <dbl>, cost_trans <dbl>, cost_hc <dbl>, .disc_ly_resp <dbl>,
      #   .disc_ly_nresp <dbl>, .disc_qalys_resp <dbl>, .disc_qalys_nresp <dbl>,
      #   .disc_lys <dbl>, .disc_qalys <dbl>, .disc_cost_med <dbl>,
      #   .disc_cost_resp <dbl>, .disc_cost_nresp <dbl>, .disc_cost_trans <dbl>,
      #   .disc_cost_hc <dbl>
      
      $`Calc - Values`
      # A tibble: 80 × 25
         strategy group    cycle ly_resp ly_nresp qalys_resp qalys_nresp    lys  qalys
         <chr>    <chr>    <dbl>   <dbl>    <dbl>      <dbl>       <dbl>  <dbl>  <dbl>
       1 chck     All Pat…     1 2.95e-1   0.514     2.39e-1      0.326  0.809  0.565 
       2 chck     All Pat…     2 4.82e-2   0.498     3.90e-2      0.329  0.546  0.368 
       3 chck     All Pat…     3 1.01e-2   0.414     8.16e-3      0.277  0.424  0.285 
       4 chck     All Pat…     4 2.29e-3   0.332     1.85e-3      0.222  0.334  0.224 
       5 chck     All Pat…     5 5.48e-4   0.260     4.44e-4      0.174  0.261  0.175 
       6 chck     All Pat…     6 1.36e-4   0.203     1.10e-4      0.136  0.203  0.136 
       7 chck     All Pat…     7 3.48e-5   0.158     2.82e-5      0.106  0.158  0.106 
       8 chck     All Pat…     8 9.14e-6   0.124     7.40e-6      0.0829 0.124  0.0829
       9 chck     All Pat…     9 2.45e-6   0.0973    1.98e-6      0.0652 0.0973 0.0652
      10 chck     All Pat…    10 6.67e-7   0.0770    5.40e-7      0.0516 0.0770 0.0516
      # ℹ 70 more rows
      # ℹ 16 more variables: cost_med <dbl>, cost_resp <dbl>, cost_nresp <dbl>,
      #   cost_trans <dbl>, cost_hc <dbl>, .disc_ly_resp <dbl>, .disc_ly_nresp <dbl>,
      #   .disc_qalys_resp <dbl>, .disc_qalys_nresp <dbl>, .disc_lys <dbl>,
      #   .disc_qalys <dbl>, .disc_cost_med <dbl>, .disc_cost_resp <dbl>,
      #   .disc_cost_nresp <dbl>, .disc_cost_trans <dbl>, .disc_cost_hc <dbl>
      
      $`Results - Trace`
      # A tibble: 84 × 9
         model_day model_week model_month model_year series        RESP    REL     REF
             <dbl>      <dbl>       <dbl>      <dbl> <chr>        <dbl>  <dbl>   <dbl>
       1         0        0             0          0 myfo   0.31        0      6.9 e-1
       2       365       52.1          12          1 myfo   0.0486      0.256  1.63e-1
       3       730      104.           24          2 myfo   0.0100      0.262  3.87e-2
       4      1095      156.           36          3 myfo   0.00225     0.221  9.18e-3
       5      1460      209.           48          4 myfo   0.000534    0.177  2.17e-3
       6      1825      261.           60          5 myfo   0.000132    0.138  5.15e-4
       7      2190      313.           72          6 myfo   0.0000336   0.108  1.22e-4
       8      2555      365            84          7 myfo   0.00000877  0.0843 2.89e-5
       9      2920      417.           96          8 myfo   0.00000234  0.0661 6.85e-6
      10      3285      469.          108          9 myfo   0.000000636 0.0522 1.62e-6
      # ℹ 74 more rows
      # ℹ 1 more variable: DEAD <dbl>
      
      $`Results - Trace (Corrected)`
      # A tibble: 80 × 9
         model_day model_week model_month model_year series        RESP    REL     REF
             <dbl>      <dbl>       <dbl>      <dbl> <chr>        <dbl>  <dbl>   <dbl>
       1       365       52.1          12          1 myfo   0.179       0.128  4.27e-1
       2       730      104.           24          2 myfo   0.0293      0.259  1.01e-1
       3      1095      156.           36          3 myfo   0.00612     0.242  2.40e-2
       4      1460      209.           48          4 myfo   0.00139     0.199  5.68e-3
       5      1825      261.           60          5 myfo   0.000333    0.158  1.34e-3
       6      2190      313.           72          6 myfo   0.0000827   0.123  3.19e-4
       7      2555      365            84          7 myfo   0.0000212   0.0961 7.55e-5
       8      2920      417.           96          8 myfo   0.00000556  0.0752 1.79e-5
       9      3285      469.          108          9 myfo   0.00000149  0.0592 4.24e-6
      10      3650      521.          120         10 myfo   0.000000405 0.0468 1.00e-6
      # ℹ 70 more rows
      # ℹ 1 more variable: DEAD <dbl>
      
      $`Results - Outcomes`
      # A tibble: 128 × 5
         outcome series        group   disc    value
         <chr>   <chr>         <chr>   <lgl>   <dbl>
       1 lys     myfo          ly_resp TRUE   0.215 
       2 lys     chpl          ly_resp TRUE   0.243 
       3 lys     chck          ly_resp TRUE   0.354 
       4 lys     rlps          ly_resp TRUE   0.377 
       5 lys     chpl vs. myfo ly_resp TRUE   0.0277
       6 lys     chck vs. myfo ly_resp TRUE   0.139 
       7 lys     rlps vs. myfo ly_resp TRUE   0.162 
       8 lys     myfo vs. chpl ly_resp TRUE  -0.0277
       9 lys     chck vs. chpl ly_resp TRUE   0.111 
      10 lys     rlps vs. chpl ly_resp TRUE   0.134 
      # ℹ 118 more rows
      
      $`Results - Costs`
      # A tibble: 128 × 5
         outcome series        group    disc   value
         <chr>   <chr>         <chr>    <lgl>  <dbl>
       1 cost_hc myfo          cost_med TRUE   9141.
       2 cost_hc chpl          cost_med TRUE   9302.
       3 cost_hc chck          cost_med TRUE  44162.
       4 cost_hc rlps          cost_med TRUE  58753.
       5 cost_hc chpl vs. myfo cost_med TRUE    160.
       6 cost_hc chck vs. myfo cost_med TRUE  35021.
       7 cost_hc rlps vs. myfo cost_med TRUE  49612.
       8 cost_hc myfo vs. chpl cost_med TRUE   -160.
       9 cost_hc chck vs. chpl cost_med TRUE  34861.
      10 cost_hc rlps vs. chpl cost_med TRUE  49452.
      # ℹ 118 more rows
      
      $`Results - CE`
      # A tibble: 8 × 11
        hsumm     esumm health_outcome econ_outcome series   cost   eff  dcost deffect
        <chr>     <chr> <chr>          <chr>        <chr>   <dbl> <dbl>  <dbl>   <dbl>
      1 .disc_lys .dis… .disc_lys      .disc_cost_… myfo   5.43e4  2.12    NA  NA     
      2 .disc_lys .dis… .disc_lys      .disc_cost_… chpl   5.44e4  2.29   103.  0.170 
      3 .disc_lys .dis… .disc_lys      .disc_cost_… chck   8.90e4  2.97 34630.  0.681 
      4 .disc_lys .dis… .disc_lys      .disc_cost_… rlps   1.04e5  3.11 14543.  0.140 
      5 .disc_qa… .dis… .disc_qalys    .disc_cost_… myfo   5.43e4  1.42    NA  NA     
      6 .disc_qa… .dis… .disc_qalys    .disc_cost_… chpl   5.44e4  1.54   103.  0.120 
      7 .disc_qa… .dis… .disc_qalys    .disc_cost_… chck   8.90e4  2.02 34630.  0.479 
      8 .disc_qa… .dis… .disc_qalys    .disc_cost_… rlps   1.04e5  2.12 14543.  0.0989
      # ℹ 2 more variables: dref <chr>, icer <dbl>
      
      $`Results - NMB`
      # A tibble: 96 × 6
         outcome series        group   disc  type    value
         <chr>   <chr>         <chr>   <lgl> <chr>   <dbl>
       1 lys     chpl vs. myfo ly_resp TRUE  health   555.
       2 lys     chck vs. myfo ly_resp TRUE  health  2774.
       3 lys     rlps vs. myfo ly_resp TRUE  health  3231.
       4 lys     myfo vs. chpl ly_resp TRUE  health  -555.
       5 lys     chck vs. chpl ly_resp TRUE  health  2219.
       6 lys     rlps vs. chpl ly_resp TRUE  health  2676.
       7 lys     myfo vs. chck ly_resp TRUE  health -2774.
       8 lys     chpl vs. chck ly_resp TRUE  health -2219.
       9 lys     rlps vs. chck ly_resp TRUE  health   458.
      10 lys     myfo vs. rlps ly_resp TRUE  health -3231.
      # ℹ 86 more rows
      

---

    Code
      exported
    Output
      $`Inputs - Settings`
      # A tibble: 4 × 2
        setting      value
        <chr>        <chr>
      1 disc_cost    0.035
      2 disc_eff     0.035
      3 cycle_length 365  
      4 n_cycles     20   
      
      $`Inputs - Strategies`
      # A tibble: 4 × 2
        name  desc       
        <chr> <chr>      
      1 myfo  Myfosfamide
      2 chpl  Chemoplatin
      3 chck  Checkimab  
      4 rlps  Relapsinib 
      
      $`Inputs - States`
      # A tibble: 4 × 4
        name  desc       prob                                                    limit
        <chr> <chr>      <chr>                                                   <dbl>
      1 RESP  Response   dispatch_strategy(myfo = rr_myfo, chpl = rr_chpl, chck…     0
      2 REL   Relapse    0                                                           0
      3 REF   Refractory C                                                           0
      4 DEAD  Dead       0                                                           0
      
      $`Inputs - Transitions`
      # A tibble: 4 × 5
        strategy state formula                                             id    value
        <chr>    <chr> <chr>                                               <chr> <chr>
      1 All      RESP  rr * surv_prob(rfs, model_month)                    6e50… rr *…
      2 All      REL   rr * (surv_prob(os_resp, model_month) - surv_prob(… aefc… rr *…
      3 All      REF   (1 - rr) * surv_prob(os_nresp, model_month)         d1f8… (1 -…
      4 All      DEAD  C                                                   8c1d… C    
      
      $`Inputs - Health Values`
      # A tibble: 6 × 5
        name        description               strategy state value                    
        <chr>       <chr>                     <chr>    <chr> <chr>                    
      1 ly_resp     Life Years in Response    All      RESP  cycle_length_years       
      2 ly_nresp    Life Years in Nonresponse All      REL   cycle_length_years       
      3 qalys_resp  QALYs in Response         All      RESP  util_resp * cycle_length…
      4 qalys_nresp QALYs in Nonresponse      All      REL   util_rel * cycle_length_…
      5 ly_nresp    Label...                  All      REF   cycle_length_years       
      6 qalys_nresp Label...                  All      REF   util_ref * cycle_length_…
      
      $`Inputs - Econ Values`
      # A tibble: 7 × 5
        name       description               strategy state value                     
        <chr>      <chr>                     <chr>    <chr> <chr>                     
      1 cost_med   Medication cost           myfo     RESP  ucost_myfo * cycle_length…
      2 cost_resp  Routine care, response    All      All   ucost_resp * cycle_length…
      3 cost_nresp Routine care, nonresponse All      All   ucost_nresp * cycle_lengt…
      4 cost_trans Transplant                All      REF   rescale_prob(p_transplant…
      5 cost_med   Label...                  chpl     RESP  ucost_chpl * cycle_length…
      6 cost_med   Label...                  chck     RESP  ucost_chck * cycle_length…
      7 cost_med   Label...                  rlps     RESP  ucost_rlps * cycle_length…
      
      $`Inputs - Health Summ`
      # A tibble: 4 × 4
        name  description                 value          wtp
        <chr> <chr>                       <chr>        <dbl>
      1 lys   Life-years                  ly_resp      20000
      2 lys   Life-years                  ly_nresp     20000
      3 qalys Quality-adjusted life-years qalys_resp   30000
      4 qalys Label...                    qalys_nresp 100000
      
      $`Inputs - Econ Summ`
      # A tibble: 4 × 3
        name    description             value     
        <chr>   <chr>                   <chr>     
      1 cost_hc Healthcare system costs cost_med  
      2 cost_hc Label...                cost_resp 
      3 cost_hc Label...                cost_nresp
      4 cost_hc Label...                cost_trans
      
      $`Inputs - Parameters`
      # A tibble: 21 × 6
         name            desc                       value            low   high  psa  
         <chr>           <chr>                      <chr>            <chr> <chr> <chr>
       1 rr_myfo         Response rate, myfosfamide 0.31             "bc … "bc … "bin…
       2 rr_chpl         Response rate, chemoplatin 0.35             "bc … "bc … "bin…
       3 rr_chck         Response rate, checkimab   0.51             "bc … "bc … "bin…
       4 rr_rlps         Response rate, relapsinib  0.543            "bc … "bc … "bin…
       5 rr              Relapse rate               dispatch_strate… ""    ""    ""   
       6 rfs_scale       Scale parameter, RFS       6                "5.4… "7.1" "log…
       7 rfs_shape       Shape parameter, RFS       0.89             "0.7… "1.0… "log…
       8 os_resp_meanlog log mean OS, responders    4                "3.4… "4.5… "nor…
       9 os_resp_sdlog   log SD OS, responders      0.71             "0.5… "0.9… "log…
      10 os_nresp_rate   Death rate, nonresponders  0.12             "0.0… "0.1… "log…
      # ℹ 11 more rows
      
      $`Inputs - Surv Dists`
      # A tibble: 3 × 2
        name     value                                                                
        <chr>    <chr>                                                                
      1 rfs      "define_survival(shape = rfs_shape, scale = rfs_scale, dist = \"weib…
      2 os_resp  "define_survival(meanlog = os_resp_meanlog, sdlog = os_resp_sdlog, d…
      3 os_nresp "define_survival(rate = os_nresp_rate, dist = \"exp\")"              
      
      $`Calc - Params`
      # A tibble: 80 × 40
         strategy group        state_time cycle model_time cycle_length_days
         <chr>    <chr>             <dbl> <dbl>      <dbl>             <dbl>
       1 myfo     All Patients          1     1          1               365
       2 myfo     All Patients          1     2          2               365
       3 myfo     All Patients          1     3          3               365
       4 myfo     All Patients          1     4          4               365
       5 myfo     All Patients          1     5          5               365
       6 myfo     All Patients          1     6          6               365
       7 myfo     All Patients          1     7          7               365
       8 myfo     All Patients          1     8          8               365
       9 myfo     All Patients          1     9          9               365
      10 myfo     All Patients          1    10         10               365
      # ℹ 70 more rows
      # ℹ 34 more variables: cycle_length_weeks <dbl>, cycle_length_months <dbl>,
      #   cycle_length_years <dbl>, model_day <dbl>, model_week <dbl>,
      #   model_month <dbl>, model_year <dbl>, state_day <dbl>, state_week <dbl>,
      #   state_month <dbl>, state_year <dbl>, disc_h <dbl>, disc_e <dbl>,
      #   rr_myfo <dbl>, rr_chpl <dbl>, rr_chck <dbl>, rr_rlps <dbl>, rr <dbl>,
      #   rfs_scale <dbl>, rfs_shape <dbl>, os_resp_meanlog <dbl>, …
      
      $`Calc - Trans`
      # A tibble: 84 × 9
         model_day model_week model_month model_year series        RESP    REL     REF
             <dbl>      <dbl>       <dbl>      <dbl> <chr>        <dbl>  <dbl>   <dbl>
       1         0        0             0          0 myfo   0.31        0      6.9 e-1
       2       365       52.1          12          1 myfo   0.0486      0.256  1.63e-1
       3       730      104.           24          2 myfo   0.0100      0.262  3.87e-2
       4      1095      156.           36          3 myfo   0.00225     0.221  9.18e-3
       5      1460      209.           48          4 myfo   0.000534    0.177  2.17e-3
       6      1825      261.           60          5 myfo   0.000132    0.138  5.15e-4
       7      2190      313.           72          6 myfo   0.0000336   0.108  1.22e-4
       8      2555      365            84          7 myfo   0.00000877  0.0843 2.89e-5
       9      2920      417.           96          8 myfo   0.00000234  0.0661 6.85e-6
      10      3285      469.          108          9 myfo   0.000000636 0.0522 1.62e-6
      # ℹ 74 more rows
      # ℹ 1 more variable: DEAD <dbl>
      
      $`Calc - Unit Values`
      # A tibble: 320 × 26
         strategy group      cycle state ly_resp ly_nresp qalys_resp qalys_nresp   lys
         <chr>    <chr>      <dbl> <chr>   <dbl>    <dbl>      <dbl>       <dbl> <dbl>
       1 chck     All Patie…     1 DEAD        0        0          0           0     0
       2 chck     All Patie…     2 DEAD        0        0          0           0     0
       3 chck     All Patie…     3 DEAD        0        0          0           0     0
       4 chck     All Patie…     4 DEAD        0        0          0           0     0
       5 chck     All Patie…     5 DEAD        0        0          0           0     0
       6 chck     All Patie…     6 DEAD        0        0          0           0     0
       7 chck     All Patie…     7 DEAD        0        0          0           0     0
       8 chck     All Patie…     8 DEAD        0        0          0           0     0
       9 chck     All Patie…     9 DEAD        0        0          0           0     0
      10 chck     All Patie…    10 DEAD        0        0          0           0     0
      # ℹ 310 more rows
      # ℹ 17 more variables: qalys <dbl>, cost_med <dbl>, cost_resp <dbl>,
      #   cost_nresp <dbl>, cost_trans <dbl>, cost_hc <dbl>, .disc_ly_resp <dbl>,
      #   .disc_ly_nresp <dbl>, .disc_qalys_resp <dbl>, .disc_qalys_nresp <dbl>,
      #   .disc_lys <dbl>, .disc_qalys <dbl>, .disc_cost_med <dbl>,
      #   .disc_cost_resp <dbl>, .disc_cost_nresp <dbl>, .disc_cost_trans <dbl>,
      #   .disc_cost_hc <dbl>
      
      $`Calc - Values`
      # A tibble: 80 × 25
         strategy group    cycle ly_resp ly_nresp qalys_resp qalys_nresp    lys  qalys
         <chr>    <chr>    <dbl>   <dbl>    <dbl>      <dbl>       <dbl>  <dbl>  <dbl>
       1 chck     All Pat…     1 2.95e-1   0.514     2.39e-1      0.326  0.809  0.565 
       2 chck     All Pat…     2 4.82e-2   0.498     3.90e-2      0.329  0.546  0.368 
       3 chck     All Pat…     3 1.01e-2   0.414     8.16e-3      0.277  0.424  0.285 
       4 chck     All Pat…     4 2.29e-3   0.332     1.85e-3      0.222  0.334  0.224 
       5 chck     All Pat…     5 5.48e-4   0.260     4.44e-4      0.174  0.261  0.175 
       6 chck     All Pat…     6 1.36e-4   0.203     1.10e-4      0.136  0.203  0.136 
       7 chck     All Pat…     7 3.48e-5   0.158     2.82e-5      0.106  0.158  0.106 
       8 chck     All Pat…     8 9.14e-6   0.124     7.40e-6      0.0829 0.124  0.0829
       9 chck     All Pat…     9 2.45e-6   0.0973    1.98e-6      0.0652 0.0973 0.0652
      10 chck     All Pat…    10 6.67e-7   0.0770    5.40e-7      0.0516 0.0770 0.0516
      # ℹ 70 more rows
      # ℹ 16 more variables: cost_med <dbl>, cost_resp <dbl>, cost_nresp <dbl>,
      #   cost_trans <dbl>, cost_hc <dbl>, .disc_ly_resp <dbl>, .disc_ly_nresp <dbl>,
      #   .disc_qalys_resp <dbl>, .disc_qalys_nresp <dbl>, .disc_lys <dbl>,
      #   .disc_qalys <dbl>, .disc_cost_med <dbl>, .disc_cost_resp <dbl>,
      #   .disc_cost_nresp <dbl>, .disc_cost_trans <dbl>, .disc_cost_hc <dbl>
      
      $`Results - Trace`
      # A tibble: 84 × 9
         model_day model_week model_month model_year series        RESP    REL     REF
             <dbl>      <dbl>       <dbl>      <dbl> <chr>        <dbl>  <dbl>   <dbl>
       1         0        0             0          0 myfo   0.31        0      6.9 e-1
       2       365       52.1          12          1 myfo   0.0486      0.256  1.63e-1
       3       730      104.           24          2 myfo   0.0100      0.262  3.87e-2
       4      1095      156.           36          3 myfo   0.00225     0.221  9.18e-3
       5      1460      209.           48          4 myfo   0.000534    0.177  2.17e-3
       6      1825      261.           60          5 myfo   0.000132    0.138  5.15e-4
       7      2190      313.           72          6 myfo   0.0000336   0.108  1.22e-4
       8      2555      365            84          7 myfo   0.00000877  0.0843 2.89e-5
       9      2920      417.           96          8 myfo   0.00000234  0.0661 6.85e-6
      10      3285      469.          108          9 myfo   0.000000636 0.0522 1.62e-6
      # ℹ 74 more rows
      # ℹ 1 more variable: DEAD <dbl>
      
      $`Results - Trace (Corrected)`
      # A tibble: 80 × 9
         model_day model_week model_month model_year series        RESP    REL     REF
             <dbl>      <dbl>       <dbl>      <dbl> <chr>        <dbl>  <dbl>   <dbl>
       1       365       52.1          12          1 myfo   0.179       0.128  4.27e-1
       2       730      104.           24          2 myfo   0.0293      0.259  1.01e-1
       3      1095      156.           36          3 myfo   0.00612     0.242  2.40e-2
       4      1460      209.           48          4 myfo   0.00139     0.199  5.68e-3
       5      1825      261.           60          5 myfo   0.000333    0.158  1.34e-3
       6      2190      313.           72          6 myfo   0.0000827   0.123  3.19e-4
       7      2555      365            84          7 myfo   0.0000212   0.0961 7.55e-5
       8      2920      417.           96          8 myfo   0.00000556  0.0752 1.79e-5
       9      3285      469.          108          9 myfo   0.00000149  0.0592 4.24e-6
      10      3650      521.          120         10 myfo   0.000000405 0.0468 1.00e-6
      # ℹ 70 more rows
      # ℹ 1 more variable: DEAD <dbl>
      
      $`Results - Outcomes`
      # A tibble: 128 × 5
         outcome series        group   disc    value
         <chr>   <chr>         <chr>   <lgl>   <dbl>
       1 lys     myfo          ly_resp TRUE   0.215 
       2 lys     chpl          ly_resp TRUE   0.243 
       3 lys     chck          ly_resp TRUE   0.354 
       4 lys     rlps          ly_resp TRUE   0.377 
       5 lys     chpl vs. myfo ly_resp TRUE   0.0277
       6 lys     chck vs. myfo ly_resp TRUE   0.139 
       7 lys     rlps vs. myfo ly_resp TRUE   0.162 
       8 lys     myfo vs. chpl ly_resp TRUE  -0.0277
       9 lys     chck vs. chpl ly_resp TRUE   0.111 
      10 lys     rlps vs. chpl ly_resp TRUE   0.134 
      # ℹ 118 more rows
      
      $`Results - Costs`
      # A tibble: 128 × 5
         outcome series        group    disc   value
         <chr>   <chr>         <chr>    <lgl>  <dbl>
       1 cost_hc myfo          cost_med TRUE   9141.
       2 cost_hc chpl          cost_med TRUE   9302.
       3 cost_hc chck          cost_med TRUE  44162.
       4 cost_hc rlps          cost_med TRUE  58753.
       5 cost_hc chpl vs. myfo cost_med TRUE    160.
       6 cost_hc chck vs. myfo cost_med TRUE  35021.
       7 cost_hc rlps vs. myfo cost_med TRUE  49612.
       8 cost_hc myfo vs. chpl cost_med TRUE   -160.
       9 cost_hc chck vs. chpl cost_med TRUE  34861.
      10 cost_hc rlps vs. chpl cost_med TRUE  49452.
      # ℹ 118 more rows
      
      $`Results - CE`
      # A tibble: 8 × 11
        hsumm     esumm health_outcome econ_outcome series   cost   eff  dcost deffect
        <chr>     <chr> <chr>          <chr>        <chr>   <dbl> <dbl>  <dbl>   <dbl>
      1 .disc_lys .dis… .disc_lys      .disc_cost_… myfo   5.43e4  2.12    NA  NA     
      2 .disc_lys .dis… .disc_lys      .disc_cost_… chpl   5.44e4  2.29   103.  0.170 
      3 .disc_lys .dis… .disc_lys      .disc_cost_… chck   8.90e4  2.97 34630.  0.681 
      4 .disc_lys .dis… .disc_lys      .disc_cost_… rlps   1.04e5  3.11 14543.  0.140 
      5 .disc_qa… .dis… .disc_qalys    .disc_cost_… myfo   5.43e4  1.42    NA  NA     
      6 .disc_qa… .dis… .disc_qalys    .disc_cost_… chpl   5.44e4  1.54   103.  0.120 
      7 .disc_qa… .dis… .disc_qalys    .disc_cost_… chck   8.90e4  2.02 34630.  0.479 
      8 .disc_qa… .dis… .disc_qalys    .disc_cost_… rlps   1.04e5  2.12 14543.  0.0989
      # ℹ 2 more variables: dref <chr>, icer <dbl>
      
      $`Results - NMB`
      # A tibble: 96 × 6
         outcome series        group   disc  type    value
         <chr>   <chr>         <chr>   <lgl> <chr>   <dbl>
       1 lys     chpl vs. myfo ly_resp TRUE  health   555.
       2 lys     chck vs. myfo ly_resp TRUE  health  2774.
       3 lys     rlps vs. myfo ly_resp TRUE  health  3231.
       4 lys     myfo vs. chpl ly_resp TRUE  health  -555.
       5 lys     chck vs. chpl ly_resp TRUE  health  2219.
       6 lys     rlps vs. chpl ly_resp TRUE  health  2676.
       7 lys     myfo vs. chck ly_resp TRUE  health -2774.
       8 lys     chpl vs. chck ly_resp TRUE  health -2219.
       9 lys     rlps vs. chck ly_resp TRUE  health   458.
      10 lys     myfo vs. rlps ly_resp TRUE  health -3231.
      # ℹ 86 more rows
      

# Groups Model produces correct results.

    WAoAAAACAAQCAQACAwAAAAMTAAAACwAAABAAAAAMAAQACQAAAANseXMABAAJAAAAA2x5cwAE
    AAkAAAADbHlzAAQACQAAAANseXMABAAJAAAAA2x5cwAEAAkAAAADbHlzAAQACQAAAAVxYWx5
    cwAEAAkAAAAFcWFseXMABAAJAAAABXFhbHlzAAQACQAAAAVxYWx5cwAEAAkAAAAFcWFseXMA
    BAAJAAAABXFhbHlzAAAAEAAAAAwABAAJAAAACGNvc3RzX2hjAAQACQAAAAhjb3N0c19oYwAE
    AAkAAAAIY29zdHNfaGMABAAJAAAACGNvc3RzX2hjAAQACQAAAAhjb3N0c19oYwAEAAkAAAAI
    Y29zdHNfaGMABAAJAAAACGNvc3RzX2hjAAQACQAAAAhjb3N0c19oYwAEAAkAAAAIY29zdHNf
    aGMABAAJAAAACGNvc3RzX2hjAAQACQAAAAhjb3N0c19oYwAEAAkAAAAIY29zdHNfaGMAAAAQ
    AAAADAAEAAkAAAAPY2hlbW8gdnMuIGltbXVuAAQACQAAABBjaGVtbyB2cy4gdGFyZ2V0AAQA
    CQAAAA9pbW11biB2cy4gY2hlbW8ABAAJAAAAEGltbXVuIHZzLiB0YXJnZXQABAAJAAAAEHRh
    cmdldCB2cy4gY2hlbW8ABAAJAAAAEHRhcmdldCB2cy4gaW1tdW4ABAAJAAAAD2NoZW1vIHZz
    LiBpbW11bgAEAAkAAAAQY2hlbW8gdnMuIHRhcmdldAAEAAkAAAAPaW1tdW4gdnMuIGNoZW1v
    AAQACQAAABBpbW11biB2cy4gdGFyZ2V0AAQACQAAABB0YXJnZXQgdnMuIGNoZW1vAAQACQAA
    ABB0YXJnZXQgdnMuIGltbXVuAAAAEAAAAAwABAAJAAAABWNoZW1vAAQACQAAAAVjaGVtbwAE
    AAkAAAAFaW1tdW4ABAAJAAAABWltbXVuAAQACQAAAAZ0YXJnZXQABAAJAAAABnRhcmdldAAE
    AAkAAAAFY2hlbW8ABAAJAAAABWNoZW1vAAQACQAAAAVpbW11bgAEAAkAAAAFaW1tdW4ABAAJ
    AAAABnRhcmdldAAEAAkAAAAGdGFyZ2V0AAAAEAAAAAwABAAJAAAABWltbXVuAAQACQAAAAZ0
    YXJnZXQABAAJAAAABWNoZW1vAAQACQAAAAZ0YXJnZXQABAAJAAAABWNoZW1vAAQACQAAAAVp
    bW11bgAEAAkAAAAFaW1tdW4ABAAJAAAABnRhcmdldAAEAAkAAAAFY2hlbW8ABAAJAAAABnRh
    cmdldAAEAAkAAAAFY2hlbW8ABAAJAAAABWltbXVuAAAADgAAAAxAF64afPP95EAXrhp88/3k
    QB6gESrW04dAHqARKtbTh0AabXQeW7wqQBptdB5bvCpADyFESDtjjkAPIURIO2OOQBX/bQZK
    0XFAFf9tBkrRcUASEwBV9dHJQBITAFX10ckAAAAOAAAADEEeNIsSm81uQR40ixKbzW5BHlCc
    TYtrXEEeUJxNi2tcQRxRPTsMXEdBHFE9OwxcR0EeNIsSm81uQR40ixKbzW5BHlCcTYtrXEEe
    UJxNi2tcQRxRPTsMXEdBHFE9OwxcRwAAAA4AAAAMv/vH2reLVoy/5frNCz3yMD/7x9q3i1aM
    P/DKdDHsXXQ/5frNCz3yML/wynQx7F10v/m7K4i0fqi/5BLxjsEAED/5uyuItH6oP+9jZYKn
    /UA/5BLxjsEAEL/vY2WCp/1AAAAADgAAAAzAnBE6753uAEDeNN149xJwQJwROu+d7gBA3/Xx
    J/DxUMDeNN149xJwwN/18Sfw8VDAnBE6753uAEDeNN149xJwQJwROu+d7gBA3/XxJ/DxUMDe
    NN149xJwwN/18Sfw8VAAAAAOAAAADMCQKkKUVAujf/AAAAAAAABAkCpClFQLo0DedJQs/6CS
    //AAAAAAAADA3nSULP+gksCRc+H3GhYTf/AAAAAAAABAkXPh9xoWE0DgSrNl2O/3//AAAAAA
    AADA4EqzZdjv9wAAABAAAAAMAAQACQAAAAUxMDM1KgAEAAkAAAAJRG9taW5hdGVkAAQACQAA
    AAQxMDM1AAQACQAAAAUzMTE4NgAEAAkAAAAIRG9taW5hbnQABAAJAAAABjMxMTg2KgAEAAkA
    AAAFMTExNyoABAAJAAAACURvbWluYXRlZAAEAAkAAAAEMTExNwAEAAkAAAAFMzMzNjYABAAJ
    AAAACERvbWluYW50AAQACQAAAAYzMzM2NioAAAQCAAAAAQAEAAkAAAAFbmFtZXMAAAAQAAAA
    CwAEAAkAAAAGaGVhbHRoAAQACQAAAARlY29uAAQACQAAAAZzZXJpZXMABAAJAAAACHJlZmVy
    ZW50AAQACQAAAApjb21wYXJhdG9yAAQACQAAAAZlZmZlY3QABAAJAAAABGNvc3QABAAJAAAA
    B2RlZmZlY3QABAAJAAAABWRjb3N0AAQACQAAAARpY2VyAAQACQAAAAtpY2VyX3N0cmluZwAA
    BAIAAAABAAQACQAAAAlyb3cubmFtZXMAAAANAAAAAoAAAAD////0AAAEAgAAAAEABAAJAAAA
    BWNsYXNzAAAAEAAAAAEABAAJAAAACmRhdGEuZnJhbWUAAAD+

---

    Code
      exported
    Output
      $`Inputs - Settings`
      # A tibble: 9 × 2
        setting             value     
        <chr>               <chr>     
      1 disc_cost           0.03      
      2 disc_eff            0.03      
      3 n_cycles            40        
      4 method              life-table
      5 disc_method         start     
      6 CycleLength         1         
      7 CycleLengthUnits    years     
      8 ModelTimeframe      20        
      9 ModelTimeframeUnits years     
      
      $`Inputs - Groups`
      # A tibble: 2 × 3
        name           weight start_age
        <chr>          <chr>  <chr>    
      1 "\"adults\""   0.8    60       
      2 "\"children\"" 0.2    12       
      
      $`Inputs - Strategies`
      # A tibble: 3 × 2
        name   desc            
        <chr>  <chr>           
      1 immun  Immunotherapy   
      2 target Targeted therapy
      3 chemo  Chemotherapy    
      
      $`Inputs - States`
      # A tibble: 3 × 4
        name  desc         prob  limit
        <chr> <chr>        <chr> <dbl>
      1 rf    Relapse-free 1         5
      2 rel   Post-relapse 0         5
      3 dead  Dead         0         1
      
      $`Inputs - Transitions`
      # A tibble: 6 × 4
        strategy from  to    value                    
        <chr>    <chr> <chr> <chr>                    
      1 All      rf    rel   rfs_prob * (1 - rfs_mort)
      2 All      rf    dead  rfs_mort                 
      3 All      rf    rf    C                        
      4 All      rel   dead  rel_mort                 
      5 All      rel   rel   C                        
      6 All      dead  dead  1                        
      
      $`Inputs - Health Values`
      # A tibble: 6 × 5
        name       label                        strategy state value                  
        <chr>      <chr>                        <chr>    <chr> <chr>                  
      1 febn       Cases of febrile neutropenia All      rf    p_febn                 
      2 lys_rf     Relapse-free life years      All      rf    cycle_length_years     
      3 lys_rel    Post-relapse life years      All      rel   cycle_length_years     
      4 qalys_rf   Relapse-free QALYs           All      rf    lys_rf * util_rf       
      5 qalys_rel  Post-relapse QALYs           All      rel   lys_rel* util_rel      
      6 qalys_febn Febrile neutropenia QALYs    All      rf    febn * disutil_febn * …
      
      $`Inputs - Econ Values`
      # A tibble: 8 × 5
        name      label                       strategy state    value                 
        <chr>     <chr>                       <chr>    <chr>    <chr>                 
      1 cost_med  Cost of medication          chemo    rf       dose_chemo * freq_che…
      2 cost_med  Cost of medication          target   rf       dose_target * freq_ta…
      3 cost_med  Cost of medication          immun    rf       dose_immun * freq_imm…
      4 cost_febn Cost of febrile neutropenia All      rf       ucost_febn * p_febn   
      5 cost_rf   Routine care, relapse-free  All      rf       ucost_rf * cycle_leng…
      6 cost_rel  Routine care, post-relapse  All      rel      ucost_rel * cycle_len…
      7 cost_term Cost of terminal care       All      rf→dead  50000                 
      8 cost_term Cost of terminal care       All      rel→dead 50000                 
      
      $`Inputs - Health Summ`
      # A tibble: 5 × 4
        name  description value         wtp
        <chr> <chr>       <chr>       <dbl>
      1 lys   Life Years  lys_rf     100000
      2 lys   Life Years  lys_rel    100000
      3 qalys QALYs       qalys_rf   200000
      4 qalys QALYs       qalys_rel  200000
      5 qalys QALYs       qalys_febn 200000
      
      $`Inputs - Econ Summ`
      # A tibble: 4 × 4
        name     description value       wtp
        <chr>    <chr>       <chr>     <dbl>
      1 costs_hc Cost (HC)   cost_med     NA
      2 costs_hc Cost (HC)   cost_febn    NA
      3 costs_hc Cost (HC)   cost_rf      NA
      4 costs_hc Cost (HC)   cost_rel     NA
      
      $`Inputs - Parameters`
      # A tibble: 38 × 6
         name              desc                                value low   high  psa  
         <chr>             <chr>                               <chr> <chr> <chr> <chr>
       1 current_age       Current age                         "sta… ""    ""    ""   
       2 percent_male      Percent male                        "0.4… "0.4" "0.6" "bet…
       3 gp_mort_male      Annualized general-population deat… "loo… ""    ""    ""   
       4 gp_mort_female    Annualized general-population deat… "loo… ""    ""    ""   
       5 gp_mort           Annualized general-population deat… "gp_… ""    ""    ""   
       6 gp_mort_per_cycle Per-cycle general-population death… "res… ""    ""    ""   
       7 rfs_p1            Relapse-free survival shape parame… "1.1… "bc … "bc … ""   
       8 rfs_p2            Relapse-free survival scale parame… "32.… "bc … "bc … ""   
       9 target_hr         Hazard ratio of relapse, targeted … "0.6… "0.3" "0.8" ""   
      10 immun_hr          Hazard ratio of relapse, immunothe… "0.3… ""    ""    ""   
      # ℹ 28 more rows
      
      $`Tbl - life_table`
      # A tibble: 240 × 5
           age sex   prob_death n_alive life_expectancy
         <dbl> <chr>      <dbl>   <dbl>           <dbl>
       1     0 male    0.00632   100000            76.3
       2     1 male    0.000396   99368            75.8
       3     2 male    0.000282   99328            74.8
       4     3 male    0.000212   99300            73.9
       5     4 male    0.000186   99279            72.9
       6     5 male    0.000162   99261            71.9
       7     6 male    0.000144   99245            70.9
       8     7 male    0.000129   99231            69.9
       9     8 male    0.000114   99218            68.9
      10     9 male    0.0001     99206            67.9
      # ℹ 230 more rows
      
      $`Calc - Params`
      # A tibble: 600 × 59
         strategy group  state_time cycle model_time cycle_length_days
         <chr>    <chr>       <dbl> <dbl>      <dbl>             <dbl>
       1 immun    adults          1     1          1               365
       2 immun    adults          1     2          2               365
       3 immun    adults          1     3          3               365
       4 immun    adults          1     4          4               365
       5 immun    adults          1     5          5               365
       6 immun    adults          1     6          6               365
       7 immun    adults          1     7          7               365
       8 immun    adults          1     8          8               365
       9 immun    adults          1     9          9               365
      10 immun    adults          1    10         10               365
      # ℹ 590 more rows
      # ℹ 53 more variables: cycle_length_weeks <dbl>, cycle_length_months <dbl>,
      #   cycle_length_years <dbl>, model_day <dbl>, model_week <dbl>,
      #   model_month <dbl>, model_year <dbl>, state_day <dbl>, state_week <dbl>,
      #   state_month <dbl>, state_year <dbl>, disc_h <dbl>, disc_e <dbl>,
      #   .group <chr>, start_age <dbl>, current_age <dbl>, percent_male <dbl>,
      #   gp_mort_male <dbl>, gp_mort_female <dbl>, gp_mort <dbl>, …
      
      $`Calc - Trans`
      # A tibble: 1,320 × 15
         strategy group cycle from  .rf_1 .rf_2 .rf_3 .rf_4 .rf_5 .rel_1 .rel_2 .rel_3
         <chr>    <chr> <dbl> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl>
       1 immun    adul…     1 .rf_1     0 0.860 0     0     0      0.116  0      0    
       2 immun    adul…     1 .rf_2     0 0     0.842 0     0      0.134  0      0    
       3 immun    adul…     1 .rf_3     0 0     0     0.834 0      0.142  0      0    
       4 immun    adul…     1 .rf_4     0 0     0     0     0.828  0.148  0      0    
       5 immun    adul…     1 .rf_5     0 0     0     0     0.824  0.152  0      0    
       6 immun    adul…     1 .rel…     0 0     0     0     0      0      0.866  0    
       7 immun    adul…     1 .rel…     0 0     0     0     0      0      0      0.800
       8 immun    adul…     1 .rel…     0 0     0     0     0      0      0      0    
       9 immun    adul…     1 .rel…     0 0     0     0     0      0      0      0    
      10 immun    adul…     1 .rel…     0 0     0     0     0      0      0      0    
      # ℹ 1,310 more rows
      # ℹ 3 more variables: .rel_4 <dbl>, .rel_5 <dbl>, dead <dbl>
      
      $`Calc - Unit Values`
      # A tibble: 1,320 × 32
         strategy group cycle state  febn lys_rf lys_rel qalys_rf qalys_rel qalys_febn
         <chr>    <chr> <dbl> <chr> <dbl>  <dbl>   <dbl>    <dbl>     <dbl>      <dbl>
       1 chemo    adul…     1 .rel…     0      0       1        0      0.55          0
       2 chemo    adul…     2 .rel…     0      0       1        0      0.55          0
       3 chemo    adul…     3 .rel…     0      0       1        0      0.55          0
       4 chemo    adul…     4 .rel…     0      0       1        0      0.55          0
       5 chemo    adul…     5 .rel…     0      0       1        0      0.55          0
       6 chemo    adul…     6 .rel…     0      0       1        0      0.55          0
       7 chemo    adul…     7 .rel…     0      0       1        0      0.55          0
       8 chemo    adul…     8 .rel…     0      0       1        0      0.55          0
       9 chemo    adul…     9 .rel…     0      0       1        0      0.55          0
      10 chemo    adul…    10 .rel…     0      0       1        0      0.55          0
      # ℹ 1,310 more rows
      # ℹ 22 more variables: lys <dbl>, qalys <dbl>, cost_med <dbl>, cost_febn <dbl>,
      #   cost_rf <dbl>, cost_rel <dbl>, cost_term <dbl>, costs_hc <dbl>,
      #   .disc_febn <dbl>, .disc_lys_rf <dbl>, .disc_lys_rel <dbl>,
      #   .disc_qalys_rf <dbl>, .disc_qalys_rel <dbl>, .disc_qalys_febn <dbl>,
      #   .disc_lys <dbl>, .disc_qalys <dbl>, .disc_cost_med <dbl>,
      #   .disc_cost_febn <dbl>, .disc_cost_rf <dbl>, .disc_cost_rel <dbl>, …
      
      $`Calc - Values`
      # A tibble: 120 × 31
         strategy group  cycle     febn lys_rf lys_rel qalys_rf qalys_rel  qalys_febn
         <chr>    <chr>  <dbl>    <dbl>  <dbl>   <dbl>    <dbl>     <dbl>       <dbl>
       1 chemo    adults     1 0.0128   0.850    0.138   0.697     0.0758 -0.000440  
       2 chemo    adults     2 0.00872  0.581    0.367   0.477     0.202  -0.000301  
       3 chemo    adults     3 0.00570  0.380    0.496   0.311     0.273  -0.000197  
       4 chemo    adults     4 0.00363  0.242    0.532   0.199     0.292  -0.000125  
       5 chemo    adults     5 0.00228  0.152    0.505   0.124     0.278  -0.0000786 
       6 chemo    adults     6 0.00141  0.0940   0.444   0.0771    0.244  -0.0000487 
       7 chemo    adults     7 0.000872 0.0581   0.369   0.0477    0.203  -0.0000301 
       8 chemo    adults     8 0.000537 0.0358   0.296   0.0294    0.163  -0.0000185 
       9 chemo    adults     9 0.000330 0.0220   0.231   0.0180    0.127  -0.0000114 
      10 chemo    adults    10 0.000202 0.0135   0.177   0.0110    0.0973 -0.00000697
      # ℹ 110 more rows
      # ℹ 22 more variables: lys <dbl>, qalys <dbl>, cost_med <dbl>, cost_febn <dbl>,
      #   cost_rf <dbl>, cost_rel <dbl>, cost_term <dbl>, costs_hc <dbl>,
      #   .disc_febn <dbl>, .disc_lys_rf <dbl>, .disc_lys_rel <dbl>,
      #   .disc_qalys_rf <dbl>, .disc_qalys_rel <dbl>, .disc_qalys_febn <dbl>,
      #   .disc_lys <dbl>, .disc_qalys <dbl>, .disc_cost_med <dbl>,
      #   .disc_cost_febn <dbl>, .disc_cost_rf <dbl>, .disc_cost_rel <dbl>, …
      
      $`Results - Trace`
      # A tibble: 63 × 8
         model_day model_week model_month model_year series    rf   rel   dead
             <dbl>      <dbl>       <dbl>      <dbl> <chr>  <dbl> <dbl>  <dbl>
       1         0        0             0          0 immun  1     0     0     
       2       365       52.1          12          1 immun  0.865 0.116 0.0193
       3       730      104.           24          2 immun  0.730 0.217 0.0525
       4      1095      156.           36          3 immun  0.610 0.286 0.104 
       5      1460      209.           48          4 immun  0.506 0.323 0.172 
       6      1825      261.           60          5 immun  0.416 0.334 0.250 
       7      2190      313.           72          6 immun  0.342 0.325 0.333 
       8      2555      365            84          7 immun  0.280 0.303 0.416 
       9      2920      417.           96          8 immun  0.229 0.275 0.496 
      10      3285      469.          108          9 immun  0.187 0.244 0.569 
      # ℹ 53 more rows
      
      $`Results - Trace (Corrected)`
      # A tibble: 60 × 8
         model_day model_week model_month model_year series    rf    rel    dead
             <dbl>      <dbl>       <dbl>      <dbl> <chr>  <dbl>  <dbl>   <dbl>
       1       365       52.1          12          1 immun  0.932 0.0581 0.00963
       2       730      104.           24          2 immun  0.797 0.167  0.0359 
       3      1095      156.           36          3 immun  0.670 0.251  0.0784 
       4      1460      209.           48          4 immun  0.558 0.304  0.138  
       5      1825      261.           60          5 immun  0.461 0.328  0.211  
       6      2190      313.           72          6 immun  0.379 0.329  0.291  
       7      2555      365            84          7 immun  0.311 0.314  0.375  
       8      2920      417.           96          8 immun  0.255 0.289  0.456  
       9      3285      469.          108          9 immun  0.208 0.259  0.532  
      10      3650      521.          120         10 immun  0.170 0.228  0.602  
      # ℹ 50 more rows
      
      $`Results - Outcomes`
      # A tibble: 90 × 5
         outcome series           group   disc   value
         <chr>   <chr>            <chr>   <lgl>  <dbl>
       1 lys     immun            lys_rf  TRUE   4.78 
       2 lys     target           lys_rf  TRUE   3.28 
       3 lys     chemo            lys_rf  TRUE   2.36 
       4 lys     target vs. immun lys_rf  TRUE  -1.49 
       5 lys     chemo vs. immun  lys_rf  TRUE  -2.42 
       6 lys     immun vs. target lys_rf  TRUE   1.49 
       7 lys     chemo vs. target lys_rf  TRUE  -0.924
       8 lys     immun vs. chemo  lys_rf  TRUE   2.42 
       9 lys     target vs. chemo lys_rf  TRUE   0.924
      10 lys     immun            lys_rel TRUE   2.88 
      # ℹ 80 more rows
      
      $`Results - Costs`
      # A tibble: 72 × 5
         outcome  series           group     disc     value
         <chr>    <chr>            <chr>     <lgl>    <dbl>
       1 costs_hc immun            cost_med  TRUE   48142. 
       2 costs_hc target           cost_med  TRUE   26459. 
       3 costs_hc chemo            cost_med  TRUE   67888. 
       4 costs_hc target vs. immun cost_med  TRUE  -21683. 
       5 costs_hc chemo vs. immun  cost_med  TRUE   19746. 
       6 costs_hc immun vs. target cost_med  TRUE   21683. 
       7 costs_hc chemo vs. target cost_med  TRUE   41429. 
       8 costs_hc immun vs. chemo  cost_med  TRUE  -19746. 
       9 costs_hc target vs. chemo cost_med  TRUE  -41429. 
      10 costs_hc immun            cost_febn TRUE      92.4
      # ℹ 62 more rows
      
      $`Results - CE`
      # A tibble: 6 × 11
        hsumm     esumm health_outcome econ_outcome series   cost   eff  dcost deffect
        <chr>     <chr> <chr>          <chr>        <chr>   <dbl> <dbl>  <dbl>   <dbl>
      1 .disc_lys .dis… .disc_lys      .disc_costs… target 4.64e5  6.61    NA   NA    
      2 .disc_lys .dis… .disc_lys      .disc_costs… chemo  4.95e5  5.92 30931.  -0.687
      3 .disc_lys .dis… .disc_lys      .disc_costs… immun  4.97e5  7.66 32728.   1.05 
      4 .disc_qa… .dis… .disc_qalys    .disc_costs… target 4.64e5  4.52    NA   NA    
      5 .disc_qa… .dis… .disc_qalys    .disc_costs… chemo  4.95e5  3.89 30931.  -0.627
      6 .disc_qa… .dis… .disc_qalys    .disc_costs… immun  4.97e5  5.50 32728.   0.981
      # ℹ 2 more variables: dref <chr>, icer <dbl>
      
      $`Results - NMB`
      # A tibble: 54 × 6
         outcome series           group   disc  type      value
         <chr>   <chr>            <chr>   <lgl> <chr>     <dbl>
       1 lys     target vs. immun lys_rf  TRUE  health -149490.
       2 lys     chemo vs. immun  lys_rf  TRUE  health -241881.
       3 lys     immun vs. target lys_rf  TRUE  health  149490.
       4 lys     chemo vs. target lys_rf  TRUE  health  -92390.
       5 lys     immun vs. chemo  lys_rf  TRUE  health  241881.
       6 lys     target vs. chemo lys_rf  TRUE  health   92390.
       7 lys     target vs. immun lys_rel TRUE  health   44547.
       8 lys     chemo vs. immun  lys_rel TRUE  health   68251.
       9 lys     immun vs. target lys_rel TRUE  health  -44547.
      10 lys     chemo vs. target lys_rel TRUE  health   23704.
      # ℹ 44 more rows
      

---

    Code
      exported
    Output
      $`Inputs - Settings`
      # A tibble: 9 × 2
        setting             value     
        <chr>               <chr>     
      1 disc_cost           0.03      
      2 disc_eff            0.03      
      3 n_cycles            40        
      4 method              life-table
      5 disc_method         start     
      6 CycleLength         1         
      7 CycleLengthUnits    years     
      8 ModelTimeframe      20        
      9 ModelTimeframeUnits years     
      
      $`Inputs - Groups`
      # A tibble: 2 × 3
        name           weight start_age
        <chr>          <chr>  <chr>    
      1 "\"adults\""   0.8    60       
      2 "\"children\"" 0.2    12       
      
      $`Inputs - Strategies`
      # A tibble: 3 × 2
        name   desc            
        <chr>  <chr>           
      1 immun  Immunotherapy   
      2 target Targeted therapy
      3 chemo  Chemotherapy    
      
      $`Inputs - States`
      # A tibble: 3 × 4
        name  desc         prob  limit
        <chr> <chr>        <chr> <dbl>
      1 rf    Relapse-free 1         5
      2 rel   Post-relapse 0         5
      3 dead  Dead         0         1
      
      $`Inputs - Transitions`
      # A tibble: 6 × 4
        strategy from  to    value                    
        <chr>    <chr> <chr> <chr>                    
      1 All      rf    rel   rfs_prob * (1 - rfs_mort)
      2 All      rf    dead  rfs_mort                 
      3 All      rf    rf    C                        
      4 All      rel   dead  rel_mort                 
      5 All      rel   rel   C                        
      6 All      dead  dead  1                        
      
      $`Inputs - Health Values`
      # A tibble: 6 × 5
        name       label                        strategy state value                  
        <chr>      <chr>                        <chr>    <chr> <chr>                  
      1 febn       Cases of febrile neutropenia All      rf    p_febn                 
      2 lys_rf     Relapse-free life years      All      rf    cycle_length_years     
      3 lys_rel    Post-relapse life years      All      rel   cycle_length_years     
      4 qalys_rf   Relapse-free QALYs           All      rf    lys_rf * util_rf       
      5 qalys_rel  Post-relapse QALYs           All      rel   lys_rel* util_rel      
      6 qalys_febn Febrile neutropenia QALYs    All      rf    febn * disutil_febn * …
      
      $`Inputs - Econ Values`
      # A tibble: 8 × 5
        name      label                       strategy state    value                 
        <chr>     <chr>                       <chr>    <chr>    <chr>                 
      1 cost_med  Cost of medication          chemo    rf       dose_chemo * freq_che…
      2 cost_med  Cost of medication          target   rf       dose_target * freq_ta…
      3 cost_med  Cost of medication          immun    rf       dose_immun * freq_imm…
      4 cost_febn Cost of febrile neutropenia All      rf       ucost_febn * p_febn   
      5 cost_rf   Routine care, relapse-free  All      rf       ucost_rf * cycle_leng…
      6 cost_rel  Routine care, post-relapse  All      rel      ucost_rel * cycle_len…
      7 cost_term Cost of terminal care       All      rf→dead  50000                 
      8 cost_term Cost of terminal care       All      rel→dead 50000                 
      
      $`Inputs - Health Summ`
      # A tibble: 5 × 4
        name  description value         wtp
        <chr> <chr>       <chr>       <dbl>
      1 lys   Life Years  lys_rf     100000
      2 lys   Life Years  lys_rel    100000
      3 qalys QALYs       qalys_rf   200000
      4 qalys QALYs       qalys_rel  200000
      5 qalys QALYs       qalys_febn 200000
      
      $`Inputs - Econ Summ`
      # A tibble: 4 × 4
        name     description value       wtp
        <chr>    <chr>       <chr>     <dbl>
      1 costs_hc Cost (HC)   cost_med     NA
      2 costs_hc Cost (HC)   cost_febn    NA
      3 costs_hc Cost (HC)   cost_rf      NA
      4 costs_hc Cost (HC)   cost_rel     NA
      
      $`Inputs - Parameters`
      # A tibble: 38 × 6
         name              desc                                value low   high  psa  
         <chr>             <chr>                               <chr> <chr> <chr> <chr>
       1 current_age       Current age                         "sta… ""    ""    ""   
       2 percent_male      Percent male                        "0.4… "0.4" "0.6" "bet…
       3 gp_mort_male      Annualized general-population deat… "loo… ""    ""    ""   
       4 gp_mort_female    Annualized general-population deat… "loo… ""    ""    ""   
       5 gp_mort           Annualized general-population deat… "gp_… ""    ""    ""   
       6 gp_mort_per_cycle Per-cycle general-population death… "res… ""    ""    ""   
       7 rfs_p1            Relapse-free survival shape parame… "1.1… "bc … "bc … ""   
       8 rfs_p2            Relapse-free survival scale parame… "32.… "bc … "bc … ""   
       9 target_hr         Hazard ratio of relapse, targeted … "0.6… "0.3" "0.8" ""   
      10 immun_hr          Hazard ratio of relapse, immunothe… "0.3… ""    ""    ""   
      # ℹ 28 more rows
      
      $`Tbl - life_table`
      # A tibble: 240 × 5
           age sex   prob_death n_alive life_expectancy
         <dbl> <chr>      <dbl>   <dbl>           <dbl>
       1     0 male    0.00632   100000            76.3
       2     1 male    0.000396   99368            75.8
       3     2 male    0.000282   99328            74.8
       4     3 male    0.000212   99300            73.9
       5     4 male    0.000186   99279            72.9
       6     5 male    0.000162   99261            71.9
       7     6 male    0.000144   99245            70.9
       8     7 male    0.000129   99231            69.9
       9     8 male    0.000114   99218            68.9
      10     9 male    0.0001     99206            67.9
      # ℹ 230 more rows
      
      $`Calc - Params`
      # A tibble: 600 × 59
         strategy group  state_time cycle model_time cycle_length_days
         <chr>    <chr>       <dbl> <dbl>      <dbl>             <dbl>
       1 immun    adults          1     1          1               365
       2 immun    adults          1     2          2               365
       3 immun    adults          1     3          3               365
       4 immun    adults          1     4          4               365
       5 immun    adults          1     5          5               365
       6 immun    adults          1     6          6               365
       7 immun    adults          1     7          7               365
       8 immun    adults          1     8          8               365
       9 immun    adults          1     9          9               365
      10 immun    adults          1    10         10               365
      # ℹ 590 more rows
      # ℹ 53 more variables: cycle_length_weeks <dbl>, cycle_length_months <dbl>,
      #   cycle_length_years <dbl>, model_day <dbl>, model_week <dbl>,
      #   model_month <dbl>, model_year <dbl>, state_day <dbl>, state_week <dbl>,
      #   state_month <dbl>, state_year <dbl>, disc_h <dbl>, disc_e <dbl>,
      #   .group <chr>, start_age <dbl>, current_age <dbl>, percent_male <dbl>,
      #   gp_mort_male <dbl>, gp_mort_female <dbl>, gp_mort <dbl>, …
      
      $`Calc - Trans`
      # A tibble: 1,320 × 15
         strategy group cycle from  .rf_1 .rf_2 .rf_3 .rf_4 .rf_5 .rel_1 .rel_2 .rel_3
         <chr>    <chr> <dbl> <chr> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl>
       1 immun    adul…     1 .rf_1     0 0.860 0     0     0      0.116  0      0    
       2 immun    adul…     1 .rf_2     0 0     0.842 0     0      0.134  0      0    
       3 immun    adul…     1 .rf_3     0 0     0     0.834 0      0.142  0      0    
       4 immun    adul…     1 .rf_4     0 0     0     0     0.828  0.148  0      0    
       5 immun    adul…     1 .rf_5     0 0     0     0     0.824  0.152  0      0    
       6 immun    adul…     1 .rel…     0 0     0     0     0      0      0.866  0    
       7 immun    adul…     1 .rel…     0 0     0     0     0      0      0      0.800
       8 immun    adul…     1 .rel…     0 0     0     0     0      0      0      0    
       9 immun    adul…     1 .rel…     0 0     0     0     0      0      0      0    
      10 immun    adul…     1 .rel…     0 0     0     0     0      0      0      0    
      # ℹ 1,310 more rows
      # ℹ 3 more variables: .rel_4 <dbl>, .rel_5 <dbl>, dead <dbl>
      
      $`Calc - Unit Values`
      # A tibble: 1,320 × 32
         strategy group cycle state  febn lys_rf lys_rel qalys_rf qalys_rel qalys_febn
         <chr>    <chr> <dbl> <chr> <dbl>  <dbl>   <dbl>    <dbl>     <dbl>      <dbl>
       1 chemo    adul…     1 .rel…     0      0       1        0      0.55          0
       2 chemo    adul…     2 .rel…     0      0       1        0      0.55          0
       3 chemo    adul…     3 .rel…     0      0       1        0      0.55          0
       4 chemo    adul…     4 .rel…     0      0       1        0      0.55          0
       5 chemo    adul…     5 .rel…     0      0       1        0      0.55          0
       6 chemo    adul…     6 .rel…     0      0       1        0      0.55          0
       7 chemo    adul…     7 .rel…     0      0       1        0      0.55          0
       8 chemo    adul…     8 .rel…     0      0       1        0      0.55          0
       9 chemo    adul…     9 .rel…     0      0       1        0      0.55          0
      10 chemo    adul…    10 .rel…     0      0       1        0      0.55          0
      # ℹ 1,310 more rows
      # ℹ 22 more variables: lys <dbl>, qalys <dbl>, cost_med <dbl>, cost_febn <dbl>,
      #   cost_rf <dbl>, cost_rel <dbl>, cost_term <dbl>, costs_hc <dbl>,
      #   .disc_febn <dbl>, .disc_lys_rf <dbl>, .disc_lys_rel <dbl>,
      #   .disc_qalys_rf <dbl>, .disc_qalys_rel <dbl>, .disc_qalys_febn <dbl>,
      #   .disc_lys <dbl>, .disc_qalys <dbl>, .disc_cost_med <dbl>,
      #   .disc_cost_febn <dbl>, .disc_cost_rf <dbl>, .disc_cost_rel <dbl>, …
      
      $`Calc - Values`
      # A tibble: 120 × 31
         strategy group  cycle     febn lys_rf lys_rel qalys_rf qalys_rel  qalys_febn
         <chr>    <chr>  <dbl>    <dbl>  <dbl>   <dbl>    <dbl>     <dbl>       <dbl>
       1 chemo    adults     1 0.0128   0.850    0.138   0.697     0.0758 -0.000440  
       2 chemo    adults     2 0.00872  0.581    0.367   0.477     0.202  -0.000301  
       3 chemo    adults     3 0.00570  0.380    0.496   0.311     0.273  -0.000197  
       4 chemo    adults     4 0.00363  0.242    0.532   0.199     0.292  -0.000125  
       5 chemo    adults     5 0.00228  0.152    0.505   0.124     0.278  -0.0000786 
       6 chemo    adults     6 0.00141  0.0940   0.444   0.0771    0.244  -0.0000487 
       7 chemo    adults     7 0.000872 0.0581   0.369   0.0477    0.203  -0.0000301 
       8 chemo    adults     8 0.000537 0.0358   0.296   0.0294    0.163  -0.0000185 
       9 chemo    adults     9 0.000330 0.0220   0.231   0.0180    0.127  -0.0000114 
      10 chemo    adults    10 0.000202 0.0135   0.177   0.0110    0.0973 -0.00000697
      # ℹ 110 more rows
      # ℹ 22 more variables: lys <dbl>, qalys <dbl>, cost_med <dbl>, cost_febn <dbl>,
      #   cost_rf <dbl>, cost_rel <dbl>, cost_term <dbl>, costs_hc <dbl>,
      #   .disc_febn <dbl>, .disc_lys_rf <dbl>, .disc_lys_rel <dbl>,
      #   .disc_qalys_rf <dbl>, .disc_qalys_rel <dbl>, .disc_qalys_febn <dbl>,
      #   .disc_lys <dbl>, .disc_qalys <dbl>, .disc_cost_med <dbl>,
      #   .disc_cost_febn <dbl>, .disc_cost_rf <dbl>, .disc_cost_rel <dbl>, …
      
      $`Results - Trace`
      # A tibble: 63 × 8
         model_day model_week model_month model_year series    rf   rel   dead
             <dbl>      <dbl>       <dbl>      <dbl> <chr>  <dbl> <dbl>  <dbl>
       1         0        0             0          0 immun  1     0     0     
       2       365       52.1          12          1 immun  0.865 0.116 0.0193
       3       730      104.           24          2 immun  0.730 0.217 0.0525
       4      1095      156.           36          3 immun  0.610 0.286 0.104 
       5      1460      209.           48          4 immun  0.506 0.323 0.172 
       6      1825      261.           60          5 immun  0.416 0.334 0.250 
       7      2190      313.           72          6 immun  0.342 0.325 0.333 
       8      2555      365            84          7 immun  0.280 0.303 0.416 
       9      2920      417.           96          8 immun  0.229 0.275 0.496 
      10      3285      469.          108          9 immun  0.187 0.244 0.569 
      # ℹ 53 more rows
      
      $`Results - Trace (Corrected)`
      # A tibble: 60 × 8
         model_day model_week model_month model_year series    rf    rel    dead
             <dbl>      <dbl>       <dbl>      <dbl> <chr>  <dbl>  <dbl>   <dbl>
       1       365       52.1          12          1 immun  0.932 0.0581 0.00963
       2       730      104.           24          2 immun  0.797 0.167  0.0359 
       3      1095      156.           36          3 immun  0.670 0.251  0.0784 
       4      1460      209.           48          4 immun  0.558 0.304  0.138  
       5      1825      261.           60          5 immun  0.461 0.328  0.211  
       6      2190      313.           72          6 immun  0.379 0.329  0.291  
       7      2555      365            84          7 immun  0.311 0.314  0.375  
       8      2920      417.           96          8 immun  0.255 0.289  0.456  
       9      3285      469.          108          9 immun  0.208 0.259  0.532  
      10      3650      521.          120         10 immun  0.170 0.228  0.602  
      # ℹ 50 more rows
      
      $`Results - Outcomes`
      # A tibble: 90 × 5
         outcome series           group   disc   value
         <chr>   <chr>            <chr>   <lgl>  <dbl>
       1 lys     immun            lys_rf  TRUE   4.78 
       2 lys     target           lys_rf  TRUE   3.28 
       3 lys     chemo            lys_rf  TRUE   2.36 
       4 lys     target vs. immun lys_rf  TRUE  -1.49 
       5 lys     chemo vs. immun  lys_rf  TRUE  -2.42 
       6 lys     immun vs. target lys_rf  TRUE   1.49 
       7 lys     chemo vs. target lys_rf  TRUE  -0.924
       8 lys     immun vs. chemo  lys_rf  TRUE   2.42 
       9 lys     target vs. chemo lys_rf  TRUE   0.924
      10 lys     immun            lys_rel TRUE   2.88 
      # ℹ 80 more rows
      
      $`Results - Costs`
      # A tibble: 72 × 5
         outcome  series           group     disc     value
         <chr>    <chr>            <chr>     <lgl>    <dbl>
       1 costs_hc immun            cost_med  TRUE   48142. 
       2 costs_hc target           cost_med  TRUE   26459. 
       3 costs_hc chemo            cost_med  TRUE   67888. 
       4 costs_hc target vs. immun cost_med  TRUE  -21683. 
       5 costs_hc chemo vs. immun  cost_med  TRUE   19746. 
       6 costs_hc immun vs. target cost_med  TRUE   21683. 
       7 costs_hc chemo vs. target cost_med  TRUE   41429. 
       8 costs_hc immun vs. chemo  cost_med  TRUE  -19746. 
       9 costs_hc target vs. chemo cost_med  TRUE  -41429. 
      10 costs_hc immun            cost_febn TRUE      92.4
      # ℹ 62 more rows
      
      $`Results - CE`
      # A tibble: 6 × 11
        hsumm     esumm health_outcome econ_outcome series   cost   eff  dcost deffect
        <chr>     <chr> <chr>          <chr>        <chr>   <dbl> <dbl>  <dbl>   <dbl>
      1 .disc_lys .dis… .disc_lys      .disc_costs… target 4.64e5  6.61    NA   NA    
      2 .disc_lys .dis… .disc_lys      .disc_costs… chemo  4.95e5  5.92 30931.  -0.687
      3 .disc_lys .dis… .disc_lys      .disc_costs… immun  4.97e5  7.66 32728.   1.05 
      4 .disc_qa… .dis… .disc_qalys    .disc_costs… target 4.64e5  4.52    NA   NA    
      5 .disc_qa… .dis… .disc_qalys    .disc_costs… chemo  4.95e5  3.89 30931.  -0.627
      6 .disc_qa… .dis… .disc_qalys    .disc_costs… immun  4.97e5  5.50 32728.   0.981
      # ℹ 2 more variables: dref <chr>, icer <dbl>
      
      $`Results - NMB`
      # A tibble: 54 × 6
         outcome series           group   disc  type      value
         <chr>   <chr>            <chr>   <lgl> <chr>     <dbl>
       1 lys     target vs. immun lys_rf  TRUE  health -149490.
       2 lys     chemo vs. immun  lys_rf  TRUE  health -241881.
       3 lys     immun vs. target lys_rf  TRUE  health  149490.
       4 lys     chemo vs. target lys_rf  TRUE  health  -92390.
       5 lys     immun vs. chemo  lys_rf  TRUE  health  241881.
       6 lys     target vs. chemo lys_rf  TRUE  health   92390.
       7 lys     target vs. immun lys_rel TRUE  health   44547.
       8 lys     chemo vs. immun  lys_rel TRUE  health   68251.
       9 lys     immun vs. target lys_rel TRUE  health  -44547.
      10 lys     chemo vs. target lys_rel TRUE  health   23704.
      # ℹ 44 more rows
      

# Simple Markov Model produces correct results.

    WAoAAAACAAQCAQACAwAAAAMTAAAACwAAABAAAAAEAAQACQAAAANseXMABAAJAAAAA2x5cwAE
    AAkAAAAFcWFseXMABAAJAAAABXFhbHlzAAAAEAAAAAQABAAJAAAAB2Nvc3RfaGMABAAJAAAA
    B2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMAAAAQAAAABAAEAAkAAAAL
    bmF0IHZzLiBuZXcABAAJAAAAC25ldyB2cy4gbmF0AAQACQAAAAtuYXQgdnMuIG5ldwAEAAkA
    AAALbmV3IHZzLiBuYXQAAAAQAAAABAAEAAkAAAADbmF0AAQACQAAAANuZXcABAAJAAAAA25h
    dAAEAAkAAAADbmV3AAAAEAAAAAQABAAJAAAAA25ldwAEAAkAAAADbmF0AAQACQAAAANuZXcA
    BAAJAAAAA25hdAAAAA4AAAAEQCJu4w06q5lAI7rkn78ceEAYLlv89iOXQBqnO4rVO+wAAAAO
    AAAABEDpSKNsuVptQQANyw5CKZhA6UijbLlabUEADcsOQimYAAAADgAAAAS/5MAZKEcN8D/k
    wBkoRw3wv+PG/G74wqg/48b8bvjCqAAAAA4AAAAEwPN3RGYnpfpA83dEZiel+sDzd0RmJ6X6
    QPN3RGYnpfoAAAAOAAAABMD+BOV5IU36QP4E5XkhTfrA/38DcYFKskD/fwNxgUqyAAAAEAAA
    AAQABAAJAAAABzEyMjk1OCoABAAJAAAABjEyMjk1OAAEAAkAAAAHMTI5MDA4KgAEAAkAAAAG
    MTI5MDA4AAAEAgAAAAEABAAJAAAABW5hbWVzAAAAEAAAAAsABAAJAAAABmhlYWx0aAAEAAkA
    AAAEZWNvbgAEAAkAAAAGc2VyaWVzAAQACQAAAAhyZWZlcmVudAAEAAkAAAAKY29tcGFyYXRv
    cgAEAAkAAAAGZWZmZWN0AAQACQAAAARjb3N0AAQACQAAAAdkZWZmZWN0AAQACQAAAAVkY29z
    dAAEAAkAAAAEaWNlcgAEAAkAAAALaWNlcl9zdHJpbmcAAAQCAAAAAQAEAAkAAAAJcm93Lm5h
    bWVzAAAADQAAAAKAAAAA/////AAABAIAAAABAAQACQAAAAVjbGFzcwAAABAAAAABAAQACQAA
    AApkYXRhLmZyYW1lAAAA/g==

---

    Code
      exported
    Output
      $`Inputs - Settings`
      # A tibble: 4 × 2
        setting      value
        <chr>        <chr>
      1 disc_cost    0.03 
      2 disc_eff     0.03 
      3 cycle_length 365  
      4 n_cycles     20   
      
      $`Inputs - Strategies`
      # A tibble: 2 × 2
        name  desc           
        <chr> <chr>          
      1 nat   Natural History
      2 new   New Treatment  
      
      $`Inputs - States`
      # A tibble: 3 × 4
        name  desc               prob  limit
        <chr> <chr>              <chr> <dbl>
      1 well  Patient is healthy 1         0
      2 sick  Patient is sick    0         0
      3 dead  Patient is dead    0         0
      
      $`Inputs - Transitions`
      # A tibble: 7 × 4
        strategy from  to    value       
        <chr>    <chr> <chr> <chr>       
      1 nat      well  sick  p_sick_nat  
      2 new      well  sick  p_sick_new  
      3 All      well  dead  p_death_well
      4 All      well  well  C           
      5 All      sick  dead  p_death_sick
      6 All      sick  sick  C           
      7 All      dead  dead  1           
      
      $`Inputs - Health Values`
      # A tibble: 5 × 5
        name       description     strategy state value                   
        <chr>      <chr>           <chr>    <chr> <chr>                   
      1 well_lys   Well life-years All      well  cycle_length_years      
      2 sick_lys   Sick life-years All      sick  cycle_length_years      
      3 well_qalys Well QALYs      nat      All   util_well_nat * well_lys
      4 well_qalys Label...        new      All   util_well_new * well_lys
      5 sick_qalys Sick QALYs      All      All   sick_lys * util_sick    
      
      $`Inputs - Econ Values`
      # A tibble: 3 × 5
        name      description        strategy state       value         
        <chr>     <chr>              <chr>    <chr>       <chr>         
      1 med_cost  Medication cost    new      well        cost_new      
      2 term_cost Terminal care cost All      sick→dead   cost_hosp_stay
      3 ae_cost   Adverse Event Cost nat      Model Start 100           
      
      $`Inputs - Health Summ`
      # A tibble: 4 × 4
        name  description value         wtp
        <chr> <chr>       <chr>       <dbl>
      1 lys   Life-years  well_lys   100000
      2 lys   Label...    sick_lys   100000
      3 qalys QALYs       well_qalys 150000
      4 qalys Label...    sick_qalys 100000
      
      $`Inputs - Econ Summ`
      # A tibble: 3 × 3
        name    description             value    
        <chr>   <chr>                   <chr>    
      1 cost_hc Healthcare system costs med_cost 
      2 cost_hc Label...                term_cost
      3 cost_hc Healthcare system costs ae_cost  
      
      $`Inputs - Parameters`
      # A tibble: 11 × 6
         name             desc                                 value low   high  psa  
         <chr>            <chr>                                <chr> <chr> <chr> <chr>
       1 p_sick_nat       Probability of getting sick, natura… 0.5   "bc … "bc … "bin…
       2 p_sick_new       Probability of getting sick, new dr… 0.30  "bc … "bc … "bin…
       3 p_death_well     Probability of dying while well      0.005 "0.0… "0.0… "bin…
       4 p_death_sick     Probability of dying while sick      0.08  "bc … "bc … "bin…
       5 cost_new         Cost per year of new drug            32000 "240… "480… ""   
       6 util_well_nat    Utility value for well state, natur… 0.85  ""    ""    "log…
       7 disutil_well_new Utility decrement associated with n… 0.02  ""    ""    "log…
       8 disutil_sick     Utility decrement associated with b… 0.23  ""    ""    "log…
       9 cost_hosp_stay   Cost of a hospital stay              85000 ""    ""    "nor…
      10 util_well_new    Utility value for well state, new d… util… ""    ""    ""   
      11 util_sick        Utility value for sick state         util… ""    ""    ""   
      
      $`Calc - Params`
      # A tibble: 40 × 30
         strategy group        state_time cycle model_time cycle_length_days
         <chr>    <chr>             <dbl> <dbl>      <dbl>             <dbl>
       1 nat      All Patients          1     1          1               365
       2 nat      All Patients          1     2          2               365
       3 nat      All Patients          1     3          3               365
       4 nat      All Patients          1     4          4               365
       5 nat      All Patients          1     5          5               365
       6 nat      All Patients          1     6          6               365
       7 nat      All Patients          1     7          7               365
       8 nat      All Patients          1     8          8               365
       9 nat      All Patients          1     9          9               365
      10 nat      All Patients          1    10         10               365
      # ℹ 30 more rows
      # ℹ 24 more variables: cycle_length_weeks <dbl>, cycle_length_months <dbl>,
      #   cycle_length_years <dbl>, model_day <dbl>, model_week <dbl>,
      #   model_month <dbl>, model_year <dbl>, state_day <dbl>, state_week <dbl>,
      #   state_month <dbl>, state_year <dbl>, disc_h <dbl>, disc_e <dbl>,
      #   p_sick_nat <dbl>, p_sick_new <dbl>, p_death_well <dbl>, p_death_sick <dbl>,
      #   cost_new <dbl>, util_well_nat <dbl>, disutil_well_new <dbl>, …
      
      $`Calc - Trans`
      # A tibble: 120 × 7
         strategy group        cycle from   well  sick  dead
         <chr>    <chr>        <dbl> <chr> <dbl> <dbl> <dbl>
       1 nat      All Patients     1 well  0.495  0.5  0.005
       2 nat      All Patients     1 sick  0      0.92 0.08 
       3 nat      All Patients     1 dead  0      0    1    
       4 nat      All Patients     2 well  0.495  0.5  0.005
       5 nat      All Patients     2 sick  0      0.92 0.08 
       6 nat      All Patients     2 dead  0      0    1    
       7 nat      All Patients     3 well  0.495  0.5  0.005
       8 nat      All Patients     3 sick  0      0.92 0.08 
       9 nat      All Patients     3 dead  0      0    1    
      10 nat      All Patients     4 well  0.495  0.5  0.005
      # ℹ 110 more rows
      
      $`Calc - Unit Values`
      # A tibble: 120 × 24
         strategy group      cycle state well_lys sick_lys well_qalys sick_qalys   lys
         <chr>    <chr>      <dbl> <chr>    <dbl>    <dbl>      <dbl>      <dbl> <dbl>
       1 nat      All Patie…     1 dead         0        0          0          0     0
       2 nat      All Patie…     2 dead         0        0          0          0     0
       3 nat      All Patie…     3 dead         0        0          0          0     0
       4 nat      All Patie…     4 dead         0        0          0          0     0
       5 nat      All Patie…     5 dead         0        0          0          0     0
       6 nat      All Patie…     6 dead         0        0          0          0     0
       7 nat      All Patie…     7 dead         0        0          0          0     0
       8 nat      All Patie…     8 dead         0        0          0          0     0
       9 nat      All Patie…     9 dead         0        0          0          0     0
      10 nat      All Patie…    10 dead         0        0          0          0     0
      # ℹ 110 more rows
      # ℹ 15 more variables: qalys <dbl>, med_cost <dbl>, term_cost <dbl>,
      #   ae_cost <dbl>, cost_hc <dbl>, .disc_well_lys <dbl>, .disc_sick_lys <dbl>,
      #   .disc_well_qalys <dbl>, .disc_sick_qalys <dbl>, .disc_lys <dbl>,
      #   .disc_qalys <dbl>, .disc_med_cost <dbl>, .disc_term_cost <dbl>,
      #   .disc_ae_cost <dbl>, .disc_cost_hc <dbl>
      
      $`Calc - Values`
      # A tibble: 40 × 23
         strategy group      cycle well_lys sick_lys well_qalys sick_qalys   lys qalys
         <chr>    <chr>      <dbl>    <dbl>    <dbl>      <dbl>      <dbl> <dbl> <dbl>
       1 nat      All Patie…     1  0.748      0.25     0.635        0.155 0.998 0.790
       2 nat      All Patie…     2  0.370      0.604    0.315        0.374 0.974 0.689
       3 nat      All Patie…     3  0.183      0.740    0.156        0.459 0.924 0.615
       4 nat      All Patie…     4  0.0907     0.773    0.0771       0.479 0.863 0.556
       5 nat      All Patie…     5  0.0449     0.756    0.0381       0.469 0.801 0.507
       6 nat      All Patie…     6  0.0222     0.718    0.0189       0.445 0.740 0.464
       7 nat      All Patie…     7  0.0110     0.672    0.00935      0.417 0.683 0.426
       8 nat      All Patie…     8  0.00544    0.624    0.00463      0.387 0.629 0.391
       9 nat      All Patie…     9  0.00269    0.576    0.00229      0.357 0.579 0.360
      10 nat      All Patie…    10  0.00133    0.532    0.00113      0.330 0.533 0.331
      # ℹ 30 more rows
      # ℹ 14 more variables: med_cost <dbl>, term_cost <dbl>, ae_cost <dbl>,
      #   cost_hc <dbl>, .disc_well_lys <dbl>, .disc_sick_lys <dbl>,
      #   .disc_well_qalys <dbl>, .disc_sick_qalys <dbl>, .disc_lys <dbl>,
      #   .disc_qalys <dbl>, .disc_med_cost <dbl>, .disc_term_cost <dbl>,
      #   .disc_ae_cost <dbl>, .disc_cost_hc <dbl>
      
      $`Results - Trace`
      # A tibble: 42 × 8
         model_day model_week model_month model_year series    well  sick   dead
             <dbl>      <dbl>       <dbl>      <dbl> <chr>    <dbl> <dbl>  <dbl>
       1         0        0             0          0 nat    1       0     0     
       2       365       52.1          12          1 nat    0.495   0.5   0.005 
       3       730      104.           24          2 nat    0.245   0.708 0.0475
       4      1095      156.           36          3 nat    0.121   0.773 0.105 
       5      1460      209.           48          4 nat    0.0600  0.772 0.168 
       6      1825      261.           60          5 nat    0.0297  0.740 0.230 
       7      2190      313.           72          6 nat    0.0147  0.696 0.289 
       8      2555      365            84          7 nat    0.00728 0.648 0.345 
       9      2920      417.           96          8 nat    0.00360 0.600 0.397 
      10      3285      469.          108          9 nat    0.00178 0.553 0.445 
      # ℹ 32 more rows
      
      $`Results - Trace (Corrected)`
      # A tibble: 40 × 8
         model_day model_week model_month model_year series    well  sick   dead
             <dbl>      <dbl>       <dbl>      <dbl> <chr>    <dbl> <dbl>  <dbl>
       1       365       52.1          12          1 nat    0.748   0.25  0.0025
       2       730      104.           24          2 nat    0.370   0.604 0.0262
       3      1095      156.           36          3 nat    0.183   0.740 0.0764
       4      1460      209.           48          4 nat    0.0907  0.773 0.137 
       5      1825      261.           60          5 nat    0.0449  0.756 0.199 
       6      2190      313.           72          6 nat    0.0222  0.718 0.260 
       7      2555      365            84          7 nat    0.0110  0.672 0.317 
       8      2920      417.           96          8 nat    0.00544 0.624 0.371 
       9      3285      469.          108          9 nat    0.00269 0.576 0.421 
      10      3650      521.          120         10 nat    0.00133 0.532 0.467 
      # ℹ 30 more rows
      
      $`Results - Outcomes`
      # A tibble: 32 × 5
         outcome series      group      disc   value
         <chr>   <chr>       <chr>      <lgl>  <dbl>
       1 lys     nat         well_lys   TRUE   1.44 
       2 lys     new         well_lys   TRUE   2.60 
       3 lys     new vs. nat well_lys   TRUE   1.17 
       4 lys     nat vs. new well_lys   TRUE  -1.17 
       5 lys     nat         sick_lys   TRUE   7.78 
       6 lys     new         sick_lys   TRUE   7.26 
       7 lys     new vs. nat sick_lys   TRUE  -0.517
       8 lys     nat vs. new sick_lys   TRUE   0.517
       9 qalys   nat         well_qalys TRUE   1.22 
      10 qalys   new         well_qalys TRUE   2.16 
      # ℹ 22 more rows
      
      $`Results - Costs`
      # A tibble: 24 × 5
         outcome series      group     disc    value
         <chr>   <chr>       <chr>     <lgl>   <dbl>
       1 cost_hc nat         med_cost  TRUE       0 
       2 cost_hc new         med_cost  TRUE   83352.
       3 cost_hc new vs. nat med_cost  TRUE   83352.
       4 cost_hc nat vs. new med_cost  TRUE  -83352.
       5 cost_hc nat         term_cost TRUE   51681.
       6 cost_hc new         term_cost TRUE   48161.
       7 cost_hc new vs. nat term_cost TRUE   -3520.
       8 cost_hc nat vs. new term_cost TRUE    3520.
       9 cost_hc nat         ae_cost   TRUE     100 
      10 cost_hc new         ae_cost   TRUE       0 
      # ℹ 14 more rows
      
      $`Results - CE`
      # A tibble: 4 × 11
        hsumm     esumm health_outcome econ_outcome series   cost   eff  dcost deffect
        <chr>     <chr> <chr>          <chr>        <chr>   <dbl> <dbl>  <dbl>   <dbl>
      1 .disc_lys .dis… .disc_lys      .disc_cost_… nat    5.18e4  9.22    NA   NA    
      2 .disc_lys .dis… .disc_lys      .disc_cost_… new    1.32e5  9.87 79732.   0.648
      3 .disc_qa… .dis… .disc_qalys    .disc_cost_… nat    5.18e4  6.05    NA   NA    
      4 .disc_qa… .dis… .disc_qalys    .disc_cost_… new    1.32e5  6.66 79732.   0.618
      # ℹ 2 more variables: dref <chr>, icer <dbl>
      
      $`Results - NMB`
      # A tibble: 14 × 6
         outcome series      group      disc  type        value
         <chr>   <chr>       <chr>      <lgl> <chr>       <dbl>
       1 lys     new vs. nat well_lys   TRUE  health    116564.
       2 lys     nat vs. new well_lys   TRUE  health   -116564.
       3 lys     new vs. nat sick_lys   TRUE  health    -51719.
       4 lys     nat vs. new sick_lys   TRUE  health     51719.
       5 qalys   new vs. nat well_qalys TRUE  health    140805.
       6 qalys   nat vs. new well_qalys TRUE  health   -140805.
       7 qalys   new vs. nat sick_qalys TRUE  health    -48098.
       8 qalys   nat vs. new sick_qalys TRUE  health     48098.
       9 cost_hc new vs. nat med_cost   TRUE  economic  -83352.
      10 cost_hc nat vs. new med_cost   TRUE  economic   83352.
      11 cost_hc new vs. nat term_cost  TRUE  economic    3520.
      12 cost_hc nat vs. new term_cost  TRUE  economic   -3520.
      13 cost_hc new vs. nat ae_cost    TRUE  economic     100 
      14 cost_hc nat vs. new ae_cost    TRUE  economic    -100 
      

---

    Code
      exported
    Output
      $`Inputs - Settings`
      # A tibble: 4 × 2
        setting      value
        <chr>        <chr>
      1 disc_cost    0.03 
      2 disc_eff     0.03 
      3 cycle_length 365  
      4 n_cycles     20   
      
      $`Inputs - Strategies`
      # A tibble: 2 × 2
        name  desc           
        <chr> <chr>          
      1 nat   Natural History
      2 new   New Treatment  
      
      $`Inputs - States`
      # A tibble: 3 × 4
        name  desc               prob  limit
        <chr> <chr>              <chr> <dbl>
      1 well  Patient is healthy 1         0
      2 sick  Patient is sick    0         0
      3 dead  Patient is dead    0         0
      
      $`Inputs - Transitions`
      # A tibble: 7 × 4
        strategy from  to    value       
        <chr>    <chr> <chr> <chr>       
      1 nat      well  sick  p_sick_nat  
      2 new      well  sick  p_sick_new  
      3 All      well  dead  p_death_well
      4 All      well  well  C           
      5 All      sick  dead  p_death_sick
      6 All      sick  sick  C           
      7 All      dead  dead  1           
      
      $`Inputs - Health Values`
      # A tibble: 5 × 5
        name       description     strategy state value                   
        <chr>      <chr>           <chr>    <chr> <chr>                   
      1 well_lys   Well life-years All      well  cycle_length_years      
      2 sick_lys   Sick life-years All      sick  cycle_length_years      
      3 well_qalys Well QALYs      nat      All   util_well_nat * well_lys
      4 well_qalys Label...        new      All   util_well_new * well_lys
      5 sick_qalys Sick QALYs      All      All   sick_lys * util_sick    
      
      $`Inputs - Econ Values`
      # A tibble: 3 × 5
        name      description        strategy state       value         
        <chr>     <chr>              <chr>    <chr>       <chr>         
      1 med_cost  Medication cost    new      well        cost_new      
      2 term_cost Terminal care cost All      sick→dead   cost_hosp_stay
      3 ae_cost   Adverse Event Cost nat      Model Start 100           
      
      $`Inputs - Health Summ`
      # A tibble: 4 × 4
        name  description value         wtp
        <chr> <chr>       <chr>       <dbl>
      1 lys   Life-years  well_lys   100000
      2 lys   Label...    sick_lys   100000
      3 qalys QALYs       well_qalys 150000
      4 qalys Label...    sick_qalys 100000
      
      $`Inputs - Econ Summ`
      # A tibble: 3 × 3
        name    description             value    
        <chr>   <chr>                   <chr>    
      1 cost_hc Healthcare system costs med_cost 
      2 cost_hc Label...                term_cost
      3 cost_hc Healthcare system costs ae_cost  
      
      $`Inputs - Parameters`
      # A tibble: 11 × 6
         name             desc                                 value low   high  psa  
         <chr>            <chr>                                <chr> <chr> <chr> <chr>
       1 p_sick_nat       Probability of getting sick, natura… 0.5   "bc … "bc … "bin…
       2 p_sick_new       Probability of getting sick, new dr… 0.30  "bc … "bc … "bin…
       3 p_death_well     Probability of dying while well      0.005 "0.0… "0.0… "bin…
       4 p_death_sick     Probability of dying while sick      0.08  "bc … "bc … "bin…
       5 cost_new         Cost per year of new drug            32000 "240… "480… ""   
       6 util_well_nat    Utility value for well state, natur… 0.85  ""    ""    "log…
       7 disutil_well_new Utility decrement associated with n… 0.02  ""    ""    "log…
       8 disutil_sick     Utility decrement associated with b… 0.23  ""    ""    "log…
       9 cost_hosp_stay   Cost of a hospital stay              85000 ""    ""    "nor…
      10 util_well_new    Utility value for well state, new d… util… ""    ""    ""   
      11 util_sick        Utility value for sick state         util… ""    ""    ""   
      
      $`Calc - Params`
      # A tibble: 40 × 30
         strategy group        state_time cycle model_time cycle_length_days
         <chr>    <chr>             <dbl> <dbl>      <dbl>             <dbl>
       1 nat      All Patients          1     1          1               365
       2 nat      All Patients          1     2          2               365
       3 nat      All Patients          1     3          3               365
       4 nat      All Patients          1     4          4               365
       5 nat      All Patients          1     5          5               365
       6 nat      All Patients          1     6          6               365
       7 nat      All Patients          1     7          7               365
       8 nat      All Patients          1     8          8               365
       9 nat      All Patients          1     9          9               365
      10 nat      All Patients          1    10         10               365
      # ℹ 30 more rows
      # ℹ 24 more variables: cycle_length_weeks <dbl>, cycle_length_months <dbl>,
      #   cycle_length_years <dbl>, model_day <dbl>, model_week <dbl>,
      #   model_month <dbl>, model_year <dbl>, state_day <dbl>, state_week <dbl>,
      #   state_month <dbl>, state_year <dbl>, disc_h <dbl>, disc_e <dbl>,
      #   p_sick_nat <dbl>, p_sick_new <dbl>, p_death_well <dbl>, p_death_sick <dbl>,
      #   cost_new <dbl>, util_well_nat <dbl>, disutil_well_new <dbl>, …
      
      $`Calc - Trans`
      # A tibble: 120 × 7
         strategy group        cycle from   well  sick  dead
         <chr>    <chr>        <dbl> <chr> <dbl> <dbl> <dbl>
       1 nat      All Patients     1 well  0.495  0.5  0.005
       2 nat      All Patients     1 sick  0      0.92 0.08 
       3 nat      All Patients     1 dead  0      0    1    
       4 nat      All Patients     2 well  0.495  0.5  0.005
       5 nat      All Patients     2 sick  0      0.92 0.08 
       6 nat      All Patients     2 dead  0      0    1    
       7 nat      All Patients     3 well  0.495  0.5  0.005
       8 nat      All Patients     3 sick  0      0.92 0.08 
       9 nat      All Patients     3 dead  0      0    1    
      10 nat      All Patients     4 well  0.495  0.5  0.005
      # ℹ 110 more rows
      
      $`Calc - Unit Values`
      # A tibble: 120 × 24
         strategy group      cycle state well_lys sick_lys well_qalys sick_qalys   lys
         <chr>    <chr>      <dbl> <chr>    <dbl>    <dbl>      <dbl>      <dbl> <dbl>
       1 nat      All Patie…     1 dead         0        0          0          0     0
       2 nat      All Patie…     2 dead         0        0          0          0     0
       3 nat      All Patie…     3 dead         0        0          0          0     0
       4 nat      All Patie…     4 dead         0        0          0          0     0
       5 nat      All Patie…     5 dead         0        0          0          0     0
       6 nat      All Patie…     6 dead         0        0          0          0     0
       7 nat      All Patie…     7 dead         0        0          0          0     0
       8 nat      All Patie…     8 dead         0        0          0          0     0
       9 nat      All Patie…     9 dead         0        0          0          0     0
      10 nat      All Patie…    10 dead         0        0          0          0     0
      # ℹ 110 more rows
      # ℹ 15 more variables: qalys <dbl>, med_cost <dbl>, term_cost <dbl>,
      #   ae_cost <dbl>, cost_hc <dbl>, .disc_well_lys <dbl>, .disc_sick_lys <dbl>,
      #   .disc_well_qalys <dbl>, .disc_sick_qalys <dbl>, .disc_lys <dbl>,
      #   .disc_qalys <dbl>, .disc_med_cost <dbl>, .disc_term_cost <dbl>,
      #   .disc_ae_cost <dbl>, .disc_cost_hc <dbl>
      
      $`Calc - Values`
      # A tibble: 40 × 23
         strategy group      cycle well_lys sick_lys well_qalys sick_qalys   lys qalys
         <chr>    <chr>      <dbl>    <dbl>    <dbl>      <dbl>      <dbl> <dbl> <dbl>
       1 nat      All Patie…     1  0.748      0.25     0.635        0.155 0.998 0.790
       2 nat      All Patie…     2  0.370      0.604    0.315        0.374 0.974 0.689
       3 nat      All Patie…     3  0.183      0.740    0.156        0.459 0.924 0.615
       4 nat      All Patie…     4  0.0907     0.773    0.0771       0.479 0.863 0.556
       5 nat      All Patie…     5  0.0449     0.756    0.0381       0.469 0.801 0.507
       6 nat      All Patie…     6  0.0222     0.718    0.0189       0.445 0.740 0.464
       7 nat      All Patie…     7  0.0110     0.672    0.00935      0.417 0.683 0.426
       8 nat      All Patie…     8  0.00544    0.624    0.00463      0.387 0.629 0.391
       9 nat      All Patie…     9  0.00269    0.576    0.00229      0.357 0.579 0.360
      10 nat      All Patie…    10  0.00133    0.532    0.00113      0.330 0.533 0.331
      # ℹ 30 more rows
      # ℹ 14 more variables: med_cost <dbl>, term_cost <dbl>, ae_cost <dbl>,
      #   cost_hc <dbl>, .disc_well_lys <dbl>, .disc_sick_lys <dbl>,
      #   .disc_well_qalys <dbl>, .disc_sick_qalys <dbl>, .disc_lys <dbl>,
      #   .disc_qalys <dbl>, .disc_med_cost <dbl>, .disc_term_cost <dbl>,
      #   .disc_ae_cost <dbl>, .disc_cost_hc <dbl>
      
      $`Results - Trace`
      # A tibble: 42 × 8
         model_day model_week model_month model_year series    well  sick   dead
             <dbl>      <dbl>       <dbl>      <dbl> <chr>    <dbl> <dbl>  <dbl>
       1         0        0             0          0 nat    1       0     0     
       2       365       52.1          12          1 nat    0.495   0.5   0.005 
       3       730      104.           24          2 nat    0.245   0.708 0.0475
       4      1095      156.           36          3 nat    0.121   0.773 0.105 
       5      1460      209.           48          4 nat    0.0600  0.772 0.168 
       6      1825      261.           60          5 nat    0.0297  0.740 0.230 
       7      2190      313.           72          6 nat    0.0147  0.696 0.289 
       8      2555      365            84          7 nat    0.00728 0.648 0.345 
       9      2920      417.           96          8 nat    0.00360 0.600 0.397 
      10      3285      469.          108          9 nat    0.00178 0.553 0.445 
      # ℹ 32 more rows
      
      $`Results - Trace (Corrected)`
      # A tibble: 40 × 8
         model_day model_week model_month model_year series    well  sick   dead
             <dbl>      <dbl>       <dbl>      <dbl> <chr>    <dbl> <dbl>  <dbl>
       1       365       52.1          12          1 nat    0.748   0.25  0.0025
       2       730      104.           24          2 nat    0.370   0.604 0.0262
       3      1095      156.           36          3 nat    0.183   0.740 0.0764
       4      1460      209.           48          4 nat    0.0907  0.773 0.137 
       5      1825      261.           60          5 nat    0.0449  0.756 0.199 
       6      2190      313.           72          6 nat    0.0222  0.718 0.260 
       7      2555      365            84          7 nat    0.0110  0.672 0.317 
       8      2920      417.           96          8 nat    0.00544 0.624 0.371 
       9      3285      469.          108          9 nat    0.00269 0.576 0.421 
      10      3650      521.          120         10 nat    0.00133 0.532 0.467 
      # ℹ 30 more rows
      
      $`Results - Outcomes`
      # A tibble: 32 × 5
         outcome series      group      disc   value
         <chr>   <chr>       <chr>      <lgl>  <dbl>
       1 lys     nat         well_lys   TRUE   1.44 
       2 lys     new         well_lys   TRUE   2.60 
       3 lys     new vs. nat well_lys   TRUE   1.17 
       4 lys     nat vs. new well_lys   TRUE  -1.17 
       5 lys     nat         sick_lys   TRUE   7.78 
       6 lys     new         sick_lys   TRUE   7.26 
       7 lys     new vs. nat sick_lys   TRUE  -0.517
       8 lys     nat vs. new sick_lys   TRUE   0.517
       9 qalys   nat         well_qalys TRUE   1.22 
      10 qalys   new         well_qalys TRUE   2.16 
      # ℹ 22 more rows
      
      $`Results - Costs`
      # A tibble: 24 × 5
         outcome series      group     disc    value
         <chr>   <chr>       <chr>     <lgl>   <dbl>
       1 cost_hc nat         med_cost  TRUE       0 
       2 cost_hc new         med_cost  TRUE   83352.
       3 cost_hc new vs. nat med_cost  TRUE   83352.
       4 cost_hc nat vs. new med_cost  TRUE  -83352.
       5 cost_hc nat         term_cost TRUE   51681.
       6 cost_hc new         term_cost TRUE   48161.
       7 cost_hc new vs. nat term_cost TRUE   -3520.
       8 cost_hc nat vs. new term_cost TRUE    3520.
       9 cost_hc nat         ae_cost   TRUE     100 
      10 cost_hc new         ae_cost   TRUE       0 
      # ℹ 14 more rows
      
      $`Results - CE`
      # A tibble: 4 × 11
        hsumm     esumm health_outcome econ_outcome series   cost   eff  dcost deffect
        <chr>     <chr> <chr>          <chr>        <chr>   <dbl> <dbl>  <dbl>   <dbl>
      1 .disc_lys .dis… .disc_lys      .disc_cost_… nat    5.18e4  9.22    NA   NA    
      2 .disc_lys .dis… .disc_lys      .disc_cost_… new    1.32e5  9.87 79732.   0.648
      3 .disc_qa… .dis… .disc_qalys    .disc_cost_… nat    5.18e4  6.05    NA   NA    
      4 .disc_qa… .dis… .disc_qalys    .disc_cost_… new    1.32e5  6.66 79732.   0.618
      # ℹ 2 more variables: dref <chr>, icer <dbl>
      
      $`Results - NMB`
      # A tibble: 14 × 6
         outcome series      group      disc  type        value
         <chr>   <chr>       <chr>      <lgl> <chr>       <dbl>
       1 lys     new vs. nat well_lys   TRUE  health    116564.
       2 lys     nat vs. new well_lys   TRUE  health   -116564.
       3 lys     new vs. nat sick_lys   TRUE  health    -51719.
       4 lys     nat vs. new sick_lys   TRUE  health     51719.
       5 qalys   new vs. nat well_qalys TRUE  health    140805.
       6 qalys   nat vs. new well_qalys TRUE  health   -140805.
       7 qalys   new vs. nat sick_qalys TRUE  health    -48098.
       8 qalys   nat vs. new sick_qalys TRUE  health     48098.
       9 cost_hc new vs. nat med_cost   TRUE  economic  -83352.
      10 cost_hc nat vs. new med_cost   TRUE  economic   83352.
      11 cost_hc new vs. nat term_cost  TRUE  economic    3520.
      12 cost_hc nat vs. new term_cost  TRUE  economic   -3520.
      13 cost_hc new vs. nat ae_cost    TRUE  economic     100 
      14 cost_hc nat vs. new ae_cost    TRUE  economic    -100 
      

# Advanced Survival Modeling produces correct results.

    WAoAAAACAAQCAQACAwAAAAMTAAAACwAAABAAAAAEAAQACQAAAANseXMABAAJAAAAA2x5cwAE
    AAkAAAAFcWFseXMABAAJAAAABXFhbHlzAAAAEAAAAAQABAAJAAAAB2Nvc3RfaGMABAAJAAAA
    B2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMAAAAQAAAABAAEAAkAAAAQ
    Q0hFTU8gdnMuIFRBUkdFVAAEAAkAAAAQVEFSR0VUIHZzLiBDSEVNTwAEAAkAAAAQQ0hFTU8g
    dnMuIFRBUkdFVAAEAAkAAAAQVEFSR0VUIHZzLiBDSEVNTwAAABAAAAAEAAQACQAAAAVDSEVN
    TwAEAAkAAAAGVEFSR0VUAAQACQAAAAVDSEVNTwAEAAkAAAAGVEFSR0VUAAAAEAAAAAQABAAJ
    AAAABlRBUkdFVAAEAAkAAAAFQ0hFTU8ABAAJAAAABlRBUkdFVAAEAAkAAAAFQ0hFTU8AAAAO
    AAAABEAmV+Rlrle2QC49rk02qiBAIBUXrc/kCEAmP5HpNyseAAAADgAAAARBErCjmzG9EEEu
    5xbe9wkrQRKwo5sxvRBBLucW3vcJKwAAAA4AAAAEwA+XJ54hSahAD5cnniFJqMAIqejtnRxY
    QAip6O2dHFgAAAAOAAAABMEljsURXiqjQSWOxRFeKqPBJY7FEV4qo0EljsURXiqjAAAADgAA
    AATBBdZRQuW6ZUEF1lFC5bplwQv4V+FtLNpBC/hX4W0s2gAAABAAAAAEAAQACQAAAAcxNzg4
    OTAqAAQACQAAAAYxNzg4OTAABAAJAAAABzIyOTEzMSoABAAJAAAABjIyOTEzMQAABAIAAAAB
    AAQACQAAAAVuYW1lcwAAABAAAAALAAQACQAAAAZoZWFsdGgABAAJAAAABGVjb24ABAAJAAAA
    BnNlcmllcwAEAAkAAAAIcmVmZXJlbnQABAAJAAAACmNvbXBhcmF0b3IABAAJAAAABmVmZmVj
    dAAEAAkAAAAEY29zdAAEAAkAAAAHZGVmZmVjdAAEAAkAAAAFZGNvc3QABAAJAAAABGljZXIA
    BAAJAAAAC2ljZXJfc3RyaW5nAAAEAgAAAAEABAAJAAAACXJvdy5uYW1lcwAAAA0AAAACgAAA
    AP////wAAAQCAAAAAQAEAAkAAAAFY2xhc3MAAAAQAAAAAQAEAAkAAAAKZGF0YS5mcmFtZQAA
    AP4=

# TA447 Replication produces correct results.

    WAoAAAACAAQCAQACAwAAAAMTAAAACwAAABAAAAAEAAQACQAAAApMaWZlX3llYXJzAAQACQAA
    AApMaWZlX3llYXJzAAQACQAAAAVRQUxZcwAEAAkAAAAFUUFMWXMAAAAQAAAABAAEAAkAAAAL
    VG90YWxfQ29zdHMABAAJAAAAC1RvdGFsX0Nvc3RzAAQACQAAAAtUb3RhbF9Db3N0cwAEAAkA
    AAALVG90YWxfQ29zdHMAAAAQAAAABAAEAAkAAAAOUGVtYnJvIHZzLiBTT0MABAAJAAAADlNP
    QyB2cy4gUGVtYnJvAAQACQAAAA5QZW1icm8gdnMuIFNPQwAEAAkAAAAOU09DIHZzLiBQZW1i
    cm8AAAAQAAAABAAEAAkAAAAGUGVtYnJvAAQACQAAAANTT0MABAAJAAAABlBlbWJybwAEAAkA
    AAADU09DAAAAEAAAAAQABAAJAAAAA1NPQwAEAAkAAAAGUGVtYnJvAAQACQAAAANTT0MABAAJ
    AAAABlBlbWJybwAAAA4AAAAEQAX8nSsWiqE/84B0a7o9gkAAcM2iDENzP+vIsp/z2BoAAAAO
    AAAABED8pJX1iFPYQNjXdWxzT+VA/KSV9YhT2EDY13Vsc0/lAAAADgAAAAQ/+HjF6nLXwL/4
    eMXqctfAP/L9QfQemtm/8v1B9B6a2QAAAA4AAAAEQPZuuJprf9/A9m64mmt/30D2briaa3/f
    wPZuuJprf98AAAAOAAAABEDtVVl9+QhSwO1VWX35CFJA8ua0KpiRLsDy5rQqmJEuAAAAEAAA
    AAQABAAJAAAABTYwMDc1AAQACQAAAAY2MDA3NSoABAAJAAAABTc3NDE5AAQACQAAAAY3NzQx
    OSoAAAQCAAAAAQAEAAkAAAAFbmFtZXMAAAAQAAAACwAEAAkAAAAGaGVhbHRoAAQACQAAAARl
    Y29uAAQACQAAAAZzZXJpZXMABAAJAAAACHJlZmVyZW50AAQACQAAAApjb21wYXJhdG9yAAQA
    CQAAAAZlZmZlY3QABAAJAAAABGNvc3QABAAJAAAAB2RlZmZlY3QABAAJAAAABWRjb3N0AAQA
    CQAAAARpY2VyAAQACQAAAAtpY2VyX3N0cmluZwAABAIAAAABAAQACQAAAAlyb3cubmFtZXMA
    AAANAAAAAoAAAAD////8AAAEAgAAAAEABAAJAAAABWNsYXNzAAAAEAAAAAEABAAJAAAACmRh
    dGEuZnJhbWUAAAD+

# Shared State-Time produces correct results.

    WAoAAAACAAQCAQACAwAAAAMTAAAACwAAABAAAAAEAAQACQAAAANseXMABAAJAAAAA2x5cwAE
    AAkAAAAFcWFseXMABAAJAAAABXFhbHlzAAAAEAAAAAQABAAJAAAAB2Nvc3RfaGMABAAJAAAA
    B2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMAAAAQAAAABAAEAAkAAAAL
    bmF0IHZzLiBuZXcABAAJAAAAC25ldyB2cy4gbmF0AAQACQAAAAtuYXQgdnMuIG5ldwAEAAkA
    AAALbmV3IHZzLiBuYXQAAAAQAAAABAAEAAkAAAADbmF0AAQACQAAAANuZXcABAAJAAAAA25h
    dAAEAAkAAAADbmV3AAAAEAAAAAQABAAJAAAAA25ldwAEAAkAAAADbmF0AAQACQAAAANuZXcA
    BAAJAAAAA25hdAAAAA4AAAAEQBAh+bRlUFNAFFQmUsiRtkAGrHTOH354QA2dh9NBccwAAAAO
    AAAABEDlGTiLMsYbQP6INxTbrYJA5Rk4izLGG0D+iDcU262CAAAADgAAAAS/8MiyeY0FjD/w
    yLJ5jQWMv+vETBSHzVA/68RMFIfNUAAAAA4AAAAEwPP7ms9CSnRA8/uaz0JKdMDz+5rPQkp0
    QPP7ms9CSnQAAAAOAAAABMDzDKgcWgv6QPMMqBxaC/rA9weCcWenb0D3B4JxZ6dvAAAAEAAA
    AAQABAAJAAAABjc4MDI3KgAEAAkAAAAFNzgwMjcABAAJAAAABjk0MzI4KgAEAAkAAAAFOTQz
    MjgAAAQCAAAAAQAEAAkAAAAFbmFtZXMAAAAQAAAACwAEAAkAAAAGaGVhbHRoAAQACQAAAARl
    Y29uAAQACQAAAAZzZXJpZXMABAAJAAAACHJlZmVyZW50AAQACQAAAApjb21wYXJhdG9yAAQA
    CQAAAAZlZmZlY3QABAAJAAAABGNvc3QABAAJAAAAB2RlZmZlY3QABAAJAAAABWRjb3N0AAQA
    CQAAAARpY2VyAAQACQAAAAtpY2VyX3N0cmluZwAABAIAAAABAAQACQAAAAlyb3cubmFtZXMA
    AAANAAAAAoAAAAD////8AAAEAgAAAAEABAAJAAAABWNsYXNzAAAAEAAAAAEABAAJAAAACmRh
    dGEuZnJhbWUAAAD+

# Sparse Matrix produces correct results.

    WAoAAAACAAQCAQACAwAAAAMTAAAACwAAABAAAAAEAAQACQAAAANseXMABAAJAAAAA2x5cwAE
    AAkAAAAFcWFseXMABAAJAAAABXFhbHlzAAAAEAAAAAQABAAJAAAAB2Nvc3RfaGMABAAJAAAA
    B2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMABAAJAAAAB2Nvc3RfaGMAAAAQAAAABAAEAAkAAAAL
    bmF0IHZzLiBuZXcABAAJAAAAC25ldyB2cy4gbmF0AAQACQAAAAtuYXQgdnMuIG5ldwAEAAkA
    AAALbmV3IHZzLiBuYXQAAAAQAAAABAAEAAkAAAADbmF0AAQACQAAAANuZXcABAAJAAAAA25h
    dAAEAAkAAAADbmV3AAAAEAAAAAQABAAJAAAAA25ldwAEAAkAAAADbmF0AAQACQAAAANuZXcA
    BAAJAAAAA25hdAAAAA4AAAAEQBe4xWgB9qhAG6muO0z21EAQCsL5xiFLQBNbCSh9uYkAAAAO
    AAAABEDxRTp9ddoBQQKGE4N+hzJA8UU6fXXaAUEChhODfocyAAAADgAAAAS/74dGmlgBYD/v
    h0aaWAFgv+qCMXW8wfA/6oIxdbzB8AAAAA4AAAAEwPPG7ImHNGNA88bsiYc0Y8DzxuyJhzRj
    QPPG7ImHNGMAAAAOAAAABMD0EqbGiB4IQPQSpsaIHgjA99++X538NkD3375fnfw2AAAAEAAA
    AAQABAAJAAAABjgyMjE4KgAEAAkAAAAFODIyMTgABAAJAAAABjk3Nzg4KgAEAAkAAAAFOTc3
    ODgAAAQCAAAAAQAEAAkAAAAFbmFtZXMAAAAQAAAACwAEAAkAAAAGaGVhbHRoAAQACQAAAARl
    Y29uAAQACQAAAAZzZXJpZXMABAAJAAAACHJlZmVyZW50AAQACQAAAApjb21wYXJhdG9yAAQA
    CQAAAAZlZmZlY3QABAAJAAAABGNvc3QABAAJAAAAB2RlZmZlY3QABAAJAAAABWRjb3N0AAQA
    CQAAAARpY2VyAAQACQAAAAtpY2VyX3N0cmluZwAABAIAAAABAAQACQAAAAlyb3cubmFtZXMA
    AAANAAAAAoAAAAD////8AAAEAgAAAAEABAAJAAAABWNsYXNzAAAAEAAAAAEABAAJAAAACmRh
    dGEuZnJhbWUAAAD+

