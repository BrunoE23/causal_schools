# Validate RSS implied gains with unshrunk cross-outcome IV

Date: 2026-09-23

The RSS implied-gain matrix is expressed in units of latent, unshrunk school
value added. Its lottery-based validation therefore uses unshrunk attended,
offered, and DA-probability expected school value added rather than EB posterior
values.

For each directed pair of dimensions, the direct estimate instruments attended
row-dimension VA with offered row-dimension VA and uses the column student
outcome as the dependent variable. The comparison prediction multiplies the
RSS projection from row VA to column VA by the unshrunk same-outcome IV
pass-through for the column outcome. Each regression follows the destination
outcome's main-sample rule, so score and postsecondary-choice outcomes retain
their established exam-taking requirements while exam taking and financial-aid
application use their broader samples.

The comparison is currently descriptive. A formal equality test requires joint
inference because the RSS slope, same-outcome IV, and direct cross-outcome IV
share schools and students. The delta-method column saved with the detailed
results assumes independence and must not be used as the paper's formal test.
