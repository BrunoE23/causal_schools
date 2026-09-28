RC VAM -> ACROPOLIS TRANSFER PACKAGE
=====================================

This folder has the two R scripts, already up to date with every fix from
today's debugging. It does NOT have the big data files -- those are binary/
large and live in Box, which I can't reach directly, so grab them yourself:

FROM Box (C:\Users\brunem\Box\causal_schools\...), COPY INTO THIS FOLDER,
preserving the same relative path:

  1. data\clean\rc_vam_stata_analysis_sample.dta
     -> acropolis_transfer\data\clean\rc_vam_stata_analysis_sample.dta
     (the cached estimation sample -- this is the one that matters most)

  2. data\clean\rc_vam_school_values\rc_vam_school_values_r_noreg.csv
     -> acropolis_transfer\data\clean\rc_vam_school_values\rc_vam_school_values_r_noreg.csv

  3. data\clean\rc_vam_school_values\rc_vam_school_values_diagnostics_r_noreg.csv
     -> acropolis_transfer\data\clean\rc_vam_school_values\rc_vam_school_values_diagnostics_r_noreg.csv

(#2 and #3 are your partial results so far -- 4 of 8 outcomes done:
z_year_math_max, z_year_leng_max, admission_exam_taker, higher_ed_enrolled_m1.
Bringing them along means the script on Acropolis will automatically skip
those 4 and only run the remaining 4: stem_enrollment_m1,
log_program_income_clp_m1, program_certified_years_m1,
inst_certified_years_m1 -- no manual outcome selection needed on your end.)

Once all 3 are copied in, this whole acropolis_transfer folder should mirror:
  acropolis_transfer\
    code\claude\rc_vam\00_build_rc_vam_analysis_sample.R
    code\claude\rc_vam\01_construct_rc_vam_school_values_noreg.R
    data\clean\rc_vam_stata_analysis_sample.dta
    data\clean\rc_vam_school_values\rc_vam_school_values_r_noreg.csv
    data\clean\rc_vam_school_values\rc_vam_school_values_diagnostics_r_noreg.csv

Then copy the WHOLE acropolis_transfer folder to Acropolis (scp/rsync/sftp --
see the "how do I access files on the server" FAQ link SSCS sent you).

ON ACROPOLIS
------------
1. SSH in (you're already on VPN):
     ssh <cnetid>@acropolis.uchicago.edu
   (confirm exact hostname from SSCS's onboarding doc if this doesn't work)

2. Point the script at wherever you put this folder, e.g. if you uploaded it
   to ~/acropolis_transfer on the cluster:
     export CAUSAL_SCHOOLS_DATA_WD=~/acropolis_transfer

   The script also needs the repo root for one file (the income outcomes
   CSV) -- that one's small and text, not brought over here. Either copy
   output/tables/mifuturo_matricula_income/mifuturo_person_level_income_outcomes.csv
   from your repo into ~/acropolis_transfer/output/tables/mifuturo_matricula_income/
   too (only needed if you ever rerun 00_build_rc_vam_analysis_sample.R on
   Acropolis -- NOT needed just to run the noreg estimation script, since
   that only reads the already-built .dta).

3. First run, install R packages if not already present:
     Rscript -e 'install.packages(c("fixest","data.table","dplyr","haven"), repos="https://cloud.r-project.org")'

4. Run the estimation script (from inside acropolis_transfer/code/claude/rc_vam):
     cd acropolis_transfer/code/claude/rc_vam
     Rscript 01_construct_rc_vam_school_values_noreg.R
   With the 4 already-done outcomes' results copied in (step above), this
   will automatically detect them and only run the remaining 4. No env var
   needed unless you want to force something specific:
     - one outcome only:      RC_VAM_NOREG_OUTCOMES=stem_enrollment_m1 Rscript 01_construct_rc_vam_school_values_noreg.R
     - a batch of 4:          RC_VAM_NOREG_BATCH=2 Rscript 01_construct_rc_vam_school_values_noreg.R
     - force a recompute:     RC_VAM_NOREG_FORCE=1 Rscript 01_construct_rc_vam_school_values_noreg.R

5. Given Acropolis's login node has 2TB RAM (per SSCS), none of this should
   need cluster job submission (qsub) -- running directly on the login node
   should be more than enough memory-wise for what crashed locally. If SSCS
   asks you not to run heavy jobs on the login node directly, use their
   qsub examples (/share/qsub_examples) to submit it as a job instead.

6. When done, bring data\clean\rc_vam_school_values\rc_vam_school_values_r_noreg.csv
   and its diagnostics file back to Box the same way you brought the .dta
   here, so everything's back in one place.
