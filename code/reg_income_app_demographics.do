
cd "C:\Users\brunem\Research\causal_schools"


use "C:\Users\brunem\Dropbox\causal_schools\data\clean\all_apps.dta", clear


* -----------------------------
* Variable construction
* -----------------------------


gen avg_income_y4_usd = avg_income_y4 / 913


bysort year_1st_app: egen std_math = std(math_max) if math_max != 0
bysort year_1st_app: egen std_leng = std(leng_max) if leng_max != 0

bysort year_1st_app: egen std_gpa  = std(PTJE_NEM)
bysort year_1st_app: egen std_rnkg = std(PTJE_RANKING)


label var female "Female, binary"
label var income_decile "Income Decile, 1-10"
label var std_math "Z-score math exam"
label var std_leng "Z-score verbal exam"
label var took_only_science "Chose exclusively science exam, binary"
label var took_only_history "Chose exclusively history exam, binary"

label var std_gpa      "Z-score GPA score"
label var std_rnkg     "Z-score School-relative GPA score"

label var avg_income_y4_usd "Average expected income at year 4"

* -----------------------------
* Regressions
* -----------------------------
eststo clear

* OLS
eststo m1: reg avg_income_y4_usd female income_decile, r 
eststo m2: reg avg_income_y4_usd female income_decile std_math std_leng std_gpa std_rnkg, r
eststo m3: reg avg_income_y4_usd female income_decile std_math std_leng std_gpa std_rnkg took_only*, r

* School fixed effects
eststo m4: areg avg_income_y4_usd female income_decile, absorb(grad_rbd_psu) r 
eststo m5: areg avg_income_y4_usd female income_decile std_math std_leng std_gpa std_rnkg, absorb(grad_rbd_psu) r 
eststo m6: areg avg_income_y4_usd female income_decile std_math std_leng std_gpa std_rnkg took_only*, absorb(grad_rbd_psu) r 

* -----------------------------
* LaTeX table export
* -----------------------------
   *using "./output/tables/income_regressions.tex", ///
	
esttab m1 m2 m3 m4 m5 m6 ///
	using "./output/tables/income_regressions.tex", /// 
    replace booktabs label ///
    b(3) se(3) ///
    star(* 0.10 ** 0.05 *** 0.01) ///
	stats(N r2, ///
      labels("Observations" "R-squared") ///
      fmt(0 3)) /// 
    mgroups("OLS" "School Fixed Effects", ///
            pattern(1 0 0 1 0 0) ///
            prefix(\multicolumn{@span}{c}{) suffix(}) span) ///
			 posthead("\cmidrule(lr){2-4}\cmidrule(lr){5-7}") ///
  mtitles("(1)" "(2)" "(3)" "(4)" "(5)" "(6)") ///
    nodepvars ///  
	nonumbers /// 
	nonotes