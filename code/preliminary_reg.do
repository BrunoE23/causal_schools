


ssc install reghdfe



use "final_data.dta", replace 



reghdfe avg_stem_share ///
rbd_admitido ///
PROM_NOTAS PTJE_RANKING ///
math_max leng_max 
year_1st_app, ///
a(br_code) cluster(br_code) r


*Ask ChatGPT how to store coefficients
*Ask ChatGPT if this way of counting how many effects are significant works

results = J(450, 5, .)

local row_counter = 1 

foreach rbd_code in levelsof(rbd_admitido) {

mat results[`row', 1] = rbd_code 
mat results[`row', 2] =_b[`row' + 7]
mat results[`row', 3] =_se[`row' + 7]
mat results[`row', 4] =_stat[`row' + 7]

mat results[`row', 5] = 0
if abs(results[`row', 4]) > 2  {mat results[`row', 5] = 1 }

local row_counter = `row_counter' + 1 

}

*Ask ChatGPT if this way of displaying how many effects are significant works

local n_effects_sig = sum(results[.,5])
di "number of significant stats are `n_effects_sig'"

*Ask ChatGPT if this way of computing mean and sd of effects 
local hte_mean  = mean(results[.,2])
local hte_sd    = sd(results[.,2])

* Ask ChatGPT how to make this histogram of effects
set theme plainplot

histogram(results[.,2]), ///
labs(title = "Histogram of school effects on STEM content of college application", x = "Estimated effect, simple model") ///
vline(xintercept =  hte_mean - hte_sd) ///
vline(xintercept =  hte_mean + hte_sd) ///
label(" mean = "`hte_mean'" , SD = "`hte_sd'" ") 

