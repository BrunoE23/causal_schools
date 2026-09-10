"""Render the verified staff-characteristics report from saved analysis tables."""
from pathlib import Path
from datetime import date
from xml.sax.saxutils import escape
import json
import hashlib
import pandas as pd
from reportlab.lib import colors
from reportlab.lib.styles import ParagraphStyle, getSampleStyleSheet
from reportlab.pdfbase import pdfmetrics
from reportlab.pdfbase.ttfonts import TTFont
from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer, Table, TableStyle, PageBreak, Image
from pypdf import PdfReader

ROOT = Path(__file__).resolve().parents[3]
DATA = ROOT / "data/clean/staff_quality_va"
FIG = ROOT / "output/figures/staff_quality_va"
OUT = ROOT / "output/pdf/staff_quality_and_school_va_report.pdf"
OUT.parent.mkdir(parents=True, exist_ok=True)
qa = json.loads((DATA / "staff_va_independent_verification.json").read_text())
assert qa["status"] == "passed"
for name, fingerprint in qa["report_input_sha256"].items():
    assert hashlib.sha256((DATA / name).read_bytes()).hexdigest() == fingerprint, f"Changed input since verification: {name}"
assert (DATA / "staff_va_independent_verification.json").stat().st_mtime >= (DATA / "staff_va_correlations.csv").stat().st_mtime
r = pd.read_csv(DATA / "staff_va_correlations.csv")
rob = pd.read_csv(DATA / "staff_va_index_robustness.csv")
diag = pd.read_csv(DATA / "staff_index_diagnostics.csv").set_index("ROLE")
loads = pd.read_csv(DATA / "staff_pca_loadings.csv")
wide = pd.read_csv(DATA / "school_staff_indices_and_measures.csv")
tenure = pd.read_csv(DATA / "staff_tenure_audit.csv")
outcomes = pd.read_csv(DATA / "staff_va_outcome_dictionary.csv")
assert len(pd.read_csv(DATA / "staff_va_analysis_warnings.csv")) == 0
for name, filename in [("Arial", "arial.ttf"), ("Arial-Bold", "arialbd.ttf")]:
    pdfmetrics.registerFont(TTFont(name, str(Path("C:/Windows/Fonts") / filename)))
pdfmetrics.registerFontFamily("Arial", normal="Arial", bold="Arial-Bold", italic="Arial", boldItalic="Arial-Bold")
NAVY = colors.HexColor("#213a4b")
TEAL = colors.HexColor("#126e70")
GRAY = colors.HexColor("#556674")
PALE = colors.HexColor("#edf3f5")
styles = getSampleStyleSheet()
styles.add(ParagraphStyle(name="ReportTitle", fontName="Arial-Bold", fontSize=24, leading=29, textColor=NAVY, spaceAfter=12))
styles.add(ParagraphStyle(name="Section", fontName="Arial-Bold", fontSize=18, leading=23, textColor=NAVY, spaceAfter=12))
styles.add(ParagraphStyle(name="Sub", fontName="Arial-Bold", fontSize=12, leading=16, textColor=TEAL, spaceBefore=9, spaceAfter=7))
styles.add(ParagraphStyle(name="Text", fontName="Arial", fontSize=10.5, leading=15, textColor=NAVY, spaceAfter=9))
styles.add(ParagraphStyle(name="Note", fontName="Arial", fontSize=9, leading=12.5, textColor=GRAY, spaceAfter=8))
styles.add(ParagraphStyle(name="Cell", fontName="Arial", fontSize=9, leading=12, textColor=NAVY))
styles.add(ParagraphStyle(name="Head", fontName="Arial-Bold", fontSize=9, leading=11.5, textColor=colors.white))
story = []
WIDTH = 516
def p(text, style="Text"):
    story.append(Paragraph(text, styles[style]))
def title(text):
    p(text, "Section")
def sub(text):
    p(text, "Sub")
def page():
    story.append(PageBreak())
def table(headers, rows, widths, size=9):
    assert sum(widths) == WIDTH
    data = [[Paragraph(escape(str(x)), styles["Head"]) for x in headers]]
    data += [[Paragraph(escape(str(x)), styles["Cell"]) for x in row] for row in rows]
    tab = Table(data, colWidths=widths, repeatRows=1, hAlign="LEFT")
    tab.setStyle(TableStyle([
        ("BACKGROUND", (0,0),(-1,0), NAVY), ("VALIGN",(0,0),(-1,-1),"TOP"),
        ("ROWBACKGROUNDS",(0,1),(-1,-1),[colors.white, PALE]),
        ("LEFTPADDING",(0,0),(-1,-1),7), ("RIGHTPADDING",(0,0),(-1,-1),7),
        ("TOPPADDING",(0,0),(-1,-1),6), ("BOTTOMPADDING",(0,0),(-1,-1),6),
        ("LINEBELOW",(0,-1),(-1,-1),.5,colors.HexColor("#c6d3da"))]))
    story.extend([tab, Spacer(1, 9)])
def get(role, metric, outcome):
    found = r[r.ROLE.eq(role) & r.METRIC.eq(metric) & r.OUTCOME.eq(outcome)]
    assert len(found) == 1
    return found.iloc[0]
def qfmt(v):
    return "<0.001" if v < .001 else f"{v:.3f}"
def ci(row):
    return f"[{row.ADJUSTED_CI_LOW:.3f}, {row.ADJUSTED_CI_HIGH:.3f}]"
main_outcomes = ["z_year_math_max", "z_year_leng_max", "log_program_income_full_clp_m1"]
short = {"z_year_math_max":"Math", "z_year_leng_max":"Language", "log_program_income_full_clp_m1":"Projected income"}
roles = {"counselor":"Orientadores", "teacher":"HS teachers"}

# 1. Full outcome overview, including weak and negative associations.
p("Staff characteristics<br/>and school value added", "ReportTitle")
p(f"Staff window: 2018-2024 | Observed histories: 2013 onward | {date.today():%d %B %Y}", "Note")
p("<b>Teacher credentials have the clearest association with VA.</b> Orientador summary indices have weak links to both higher-education and test-score VA. Teacher links become much smaller after conditioning on school size and characteristics.")
sub("Balanced indices: correlations across all 12 outcomes")
rows = []
for outcome in outcomes.OUTCOME:
    c, t = get("counselor", "balanced_index", outcome), get("teacher", "balanced_index", outcome)
    rows.append([c.OUTCOME_LABEL, f"{c.N:,}", f"{c.PEARSON_R:.3f}", f"{t.N:,}", f"{t.PEARSON_R:.3f}"])
table(["VA outcome", "Orientador N", "Orientador r", "Teacher N", "Teacher r"], rows, [224,73,73,73,73])
p("Each row uses saved empirical-Bayes (EB) VA and equally weighted schools. The balanced index gives equal weight to standardized experience and credential blocks. N is the number of schools with the index and that VA outcome.", "Note")
p("The teacher <b>credentials block</b> alone correlates 0.406 with math VA, 0.512 with language VA and 0.446 with full projected-income VA. These are school-level associations, not causal effects of staff credentials or estimates of individual staff quality.")
page()

# 2. Definitions and measurement audit.
title("What the two scores measure")
p("The common starting universe is 3,682 schools and 757,999 VA-sample students from grade-8 cohorts 2017-2020. The completed balanced indices cover <b>1,642 orientador schools</b> and <b>2,706 HS-teacher schools</b>. Missing scores remain missing.")
table(["Component", "Orientadores", "HS teachers"], [
    ["Experience: prior role years", "Primary-function orientador years, 2013 through t-1", "Observed HS-teaching years, 2013 through t-1"],
    ["Experience: school-role spell", "Consecutive years as orientador at this school, including t", "Consecutive HS-teaching years at this school, including t"],
    ["Credentials: university", "Share with a reported university tertiary qualification", "Same definition"],
    ["Credentials: teaching title", "Share with a reported teaching qualification", "Share with a reported HS teaching qualification"]
], [142,187,187])
p("Standardize the four school-period components within each role's complete-case sample. Average the two experience components and standardize that block. Do the same for credentials. Average the two standardized blocks, then standardize the balanced index to SD 1. All weights are independent of VA.", "Note")
sub("Roster, timing and missingness")
p("Orientadores include primary or secondary function 9, across all appointments. Teachers require classroom function and their own regular youth-HS assignment. Count each person once per school-year. Later promotions never rewrite earlier histories. Observed experience is left-censored in 2013, not lifetime experience.")
p("Average equally across staff within school-year and across active-role years within 2018-2024. Require seven known annual role counts, 80% observed eligible members for an annual attribute, and 80% valid active eligible years for its period mean. Indices additionally need at least three active years. No staff is not zero staff qualifications.")
sub("Fields that need separate treatment")
p("<b>Reported school tenure:</b> zero-coded for 6.5% of orientador records in 2018 versus 95.1% in 2019, frequently for people already observed at that school. Keep reported tenure unchanged for audit; exclude its period average from the indices and analyze 2018 tenure separately.")
p("<b>Credentials:</b> institution types are observed, not institution names or rankings. Orientation mentions are asked for applicable basic-education titles, so no recorded mention does not establish no counseling training. Math/language specialty matching uses the same HS subject-assignment slot.", "Note")
page()

# 3. Counselor matrix and explicit exploratory exceptions.
title("Orientadores: weak summary-index links")
p("Pearson correlations with EB school VA. Every displayed cell uses its own observed support; the complete table includes N, confidence intervals and missingness status.", "Note")
story.append(Image(str(FIG / "counselor_correlations.png"), width=WIDTH, height=357))
p("The balanced index correlates about 0.04-0.05 with scores and full projected-income VA. None of its adjusted associations survives the role-wide multiple-testing correction. This does not establish that counseling is unimportant: the recorded characteristics may be noisy or incomplete.")
sig = r[r.ROLE.eq("counselor") & r.ADJUSTED_Q_BH.lt(.05)].sort_values("ADJUSTED_Q_BH")
sub(f"Exploratory exceptions: {len(sig)} adjusted associations with q &lt; 0.05")
labels = {"role_presence_share":"Years with counselors present", "va_students_per_primary_counselor":"VA students / primary counselor",
          "prior_main_role_change_rate":"Prior main-role switching", "staff_per1000_va_students":"Counselors / 1,000 VA students"}
table(["Characteristic", "VA outcome", "Adj. beta", "BH q"], [
    [labels.get(row.METRIC, row.METRIC_LABEL), row.OUTCOME_LABEL, f"{row.ADJUSTED_BETA_SD:.3f}", qfmt(row.ADJUSTED_Q_BH)]
    for row in sig.itertuples()], [225,151,70,70])
p("These capacity/trajectory links are exploratory and do not validate a general orientador-quality score. Beta is in outcome SD per characteristic SD; q adjusts all 444 orientador metric-outcome tests.", "Note")
page()

# 4. Teacher matrix.
title("HS teachers: credentials carry more of the association")
p("Same color scale and outcome order as the orientador matrix. Subject matches refer to recorded teaching specialties among teachers assigned to that HS subject.", "Note")
story.append(Image(str(FIG / "teacher_correlations.png"), width=WIDTH, height=357))
p("Teacher credentials have stronger correlations than the experience block. The balanced index is positively related to math, language and full projected-income VA, but it is not uniformly positively associated with every higher-education outcome.")
sub("Subject-specific credential checks")
rows = []
for metric, outcome, label in [("math_subject_match", "z_year_math_max", "Math match / math VA"),
                                ("language_subject_match", "z_year_leng_max", "Language match / language VA")]:
    a = get("teacher", metric, outcome)
    rows.append([label, f"{a.N:,}", f"{a.PEARSON_R:.3f}", f"{a.ADJUSTED_BETA_SD:.3f}", qfmt(a.ADJUSTED_Q_BH)])
table(["Pair", "Schools", "Pearson r", "Adj. beta", "BH q"], rows, [224,73,73,73,73])
p("A teacher explicitly lacking a reported teaching title contributes zero recorded teaching specialty, rather than disappearing from the subject-matching denominator. Unknown credentials remain unknown. This does not equate non-teaching degrees with lack of subject knowledge.", "Note")
page()

# 5. Adjusted associations, no selection by significance.
title("How much remains after school adjustment?")
p("Each row is a separate school-level regression. Both the staff measure and EB VA are standardized within its estimation sample. Controls are log VA-sample size and its square, administrative dependency, region, rural status, TP/artistic offerings, and basic-school offerings. School-directory controls are a 2024 snapshot.")
for role in roles:
    sub(roles[role])
    rows = []
    for metric, label in [("balanced_index", "Balanced"), ("career_index", "Experience"), ("credentials_index", "Credentials")]:
        for outcome in main_outcomes:
            a = get(role, metric, outcome)
            rows.append([label, short[outcome], f"{a.N_ADJUSTED:,}", f"{a.ADJUSTED_BETA_SD:.3f}", ci(a), qfmt(a.ADJUSTED_Q_BH)])
    table(["Index/block", "Outcome", "N", "Beta (SD)", "95% HC1 CI", "BH q"], rows, [85,110,51,68,135,67])
p("HC1 confidence intervals and t-based p-values are conditional on the saved estimated VA. They do not propagate VA-estimation uncertainty or address unobserved sorting. BH q-values are computed over all 444 orientador or 432 teacher tests, separately for adjusted associations.", "Note")
page()

# 6. PCA and robustness.
title("PCA and robustness checks")
sub("PCA does not collapse experience and credentials equally")
rows = []
for component, label in [("prior_role_years", "Prior role years"), ("school_role_spell_years", "School-role spell"), ("university_share", "University qualification")]:
    a = loads[loads.COMPONENT.eq(component)].set_index("ROLE")
    rows.append([label, f"{a.loc['counselor','PC1_LOADING']:.3f}", f"{a.loc['teacher','PC1_LOADING']:.3f}"])
a = loads[loads.COMPONENT.isin(["teaching_title_share", "hs_teaching_title_share"])].set_index("ROLE")
rows.append(["Teaching title (HS title for teachers)", f"{a.loc['counselor','PC1_LOADING']:.3f}", f"{a.loc['teacher','PC1_LOADING']:.3f}"])
rows.append(["PC1 share of four-component variance", f"{diag.loc['counselor','PC1_VARIANCE_SHARE']:.1%}", f"{diag.loc['teacher','PC1_VARIANCE_SHARE']:.1%}"])
table(["Component / diagnostic", "Orientadores", "HS teachers"], rows, [282,117,117])
p("For orientadores, PC1 is mostly observed experience. Experience and credentials blocks correlate only 0.062 for orientadores and 0.099 for teachers. Keep both blocks visible; the balanced index is a transparent summary, not evidence of one latent quality factor.", "Note")
sub("Balanced-index Pearson r under alternative checks")
rows = []
for role in roles:
    for outcome in main_outcomes:
        a = get(role, "balanced_index", outcome)
        b = rob[rob.ROLE.eq(role) & rob.METRIC.eq("balanced_index") & rob.OUTCOME.eq(outcome)].set_index("SUBSAMPLE")
        rows.append([("Orient." if role == "counselor" else "Teacher") + " / " + short[outcome], f"{a.PEARSON_R:.3f}",
                     f"{b.loc['no_private_paid','PEARSON_R']:.3f}", f"{b.loc['at_least_100_va_students','PEARSON_R']:.3f}",
                     f"{b.loc['at_least_5_active_staff_years','PEARSON_R']:.3f}", f"{a.UNSHRUNK_VA_R:.3f}"])
table(["Role / outcome", "Main", "No private paid", "100+ VA students", "5+ active years", "Unshrunk VA"], rows, [176,48,73,73,73,73])
p("The first three alternatives restrict schools. The last retains the matched sample and replaces EB VA with unshrunk VA. Full robustness tables retain sample sizes and adjusted estimates; rank and student-weighted correlations are included in the main CSV.", "Note")
sub("Early teacher-history sensitivity")
rows = []
for outcome in main_outcomes:
    a = get("teacher", "balanced_index_history2016", outcome)
    rows.append([short[outcome], f"{a.N:,}", f"{a.PEARSON_R:.3f}", f"{a.ADJUSTED_BETA_SD:.3f}"])
table(["VA outcome", "Schools", "Pearson r", "Adjusted beta"], rows, [240,92,92,92])
p("This separately fitted index starts prior HS experience in 2016 because 2013-2015 assignment slots are often missing. Teacher associations persist; it is not a reconstruction of the missing early history.", "Note")
page()

# 7. Prespecified scatter panels show weak counselor relationships too.
title("The school-level pattern")
p("Balanced indices against math-score and full projected-income VA. Small gray points are schools; teal points are means within 20 rank-based index bins. Bins are descriptive, with no fitted causal curve or confidence band.", "Note")
story.append(Image(str(FIG / "balanced_index_scatter.png"), width=WIDTH, height=364))
sub("Interpretation")
p("The teacher pattern is visible across the index distribution. The orientador pattern is much flatter. These figures use the balanced indices specified from staff variables, not weights selected to predict the displayed outcomes.")
p("School adjustment substantially attenuates teacher associations. For example, the balanced teacher index has an unadjusted projected-income correlation of 0.342, versus an adjusted coefficient of 0.052 SD per index SD. The latter is not a 5.2% wage effect: its outcome is standardized estimated log projected-income VA.")
p("Contemporaneous staff composition may reflect school sorting, management, resources or responses to past performance. A weak orientador correlation can also reflect measurement error, sparse staff rosters, and the incomplete measure of counseling-specific training. Neither pattern identifies the causal contribution of an individual staff member.")
page()

# 8. Reproducibility and remaining data limitations.
title("Methods, sources and verification")
sub("Analysis scope")
p("The analysis crosses 37 orientador measures and 36 teacher measures with 12 saved outcomes: 876 main pairs. It also estimates 288 index-by-outcome robustness specifications across three school restrictions. All measured associations remain in the CSV outputs, including weak and negative estimates. Broken tenure-period and pure coverage/applicability diagnostics are retained outside the main association family.")
p("Main correlations use equal school weights. Student-weighted correlations use the common broad-VA student counts, not inverse-variance weights. Pearson intervals use the usual correlation test; Spearman ranks canonicalize values to 10 significant digits to prevent floating-point tie breaking. Original precision is retained for indices, Pearson correlations and regressions.", "Note")
sub("Source data and definitions")
p("<b>Staff:</b> MINEDUC annual public teacher directories in the project's Box raw-data folder, 2013-2024, with annual source paths, sizes and timestamps saved in the manifest. The 2024 <i>ER_Cargos Docentes, bases publicas</i> codebook documents tenure and qualifications; Annex VI identifies math subsector 32001 and Spanish-language 31001. Raw files are unchanged.")
p("<b>VA:</b> the saved All-sample observational school VA and EB table. Outcome-specific supports are preserved; missing VA is not imputed and VA is not re-estimated. Higher-education enrollment has a slightly different source support, so its join is restricted to the common broad school universe.")
p("<b>School context:</b> the official 2024 school directory. Staffing ratios use pooled VA-sample students divided by average annual staff headcount, not total HS enrollment, annual caseload or hours-adjusted capacity. Orientadores at mixed-level schools are not allocated exclusively to HS.")
sub("Checks completed")
p(f"34 synthetic construction checks passed. Independent Python calculations verified {qa['annual_metric_coverage_gates_checked']:,} annual metric cells, {qa['period_aggregates_checked']:,} period aggregates, {qa['school_index_scores_reconstructed']:,} index scores, all 876 Pearson/rank/weighted correlations and unshrunk-VA counterparts, main BH adjustments, and 12 exact HC1 regressions. Annual orientador counts reconcile to the existing staffing-ratio build. No analysis warnings were recorded.")
sub("Reusable outputs")
table(["Output", "Contents"], [
    ["school_staff_indices_and_measures.csv", "School-period components, indices and coverage"],
    ["staff_va_correlations.csv", "All 876 main associations, N, intervals, q-values"],
    ["staff_va_index_robustness.csv", "288 alternative-sample index associations"],
    ["Index standardizations and PCA loadings", "Reproducible scaling, weights and PCA diagnostics"],
], [259,257])
p("Tables and audits are under data/clean/staff_quality_va/. The task README documents the full build and refresh sequence. Individual staff records remain local and excluded from Git. Remaining data limitations are recorded in project memory; the analysis does not resolve the missing early teaching assignments or the reported-tenure coding break.", "Note")

def footer(canvas, doc):
    canvas.saveState()
    canvas.setStrokeColor(colors.HexColor("#ced9df"))
    canvas.line(48, 42, 564, 42)
    canvas.setFont("Arial", 8)
    canvas.setFillColor(GRAY)
    canvas.drawString(48, 28, "Causal Schools | Staff characteristics and school VA")
    canvas.drawRightString(564, 28, str(doc.page))
    canvas.restoreState()

doc = SimpleDocTemplate(str(OUT), pagesize=(612,792), leftMargin=48, rightMargin=48,
                        topMargin=43, bottomMargin=55, title="Staff characteristics and school value added", author="Causal Schools")
doc.build(story, onFirstPage=footer, onLaterPages=footer)
reader = PdfReader(OUT)
assert len(reader.pages) == 8, f"Expected 8 pages, obtained {len(reader.pages)}; inspect layout before delivery."
for i, pg in enumerate(reader.pages, 1):
    text = pg.extract_text()
    assert len(text) > 300, f"Unexpectedly empty page {i}"
print(f"Created {len(reader.pages)}-page report: {OUT}")
