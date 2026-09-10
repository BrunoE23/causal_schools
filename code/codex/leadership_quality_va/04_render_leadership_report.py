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
DATA = ROOT / "data/clean/leadership_quality_va"
FIG = ROOT / "output/figures/leadership_quality_va"
OUT = ROOT / "output/pdf/leadership_characteristics_and_school_va_report.pdf"
OUT.parent.mkdir(parents=True, exist_ok=True)
qa = json.loads((DATA / "leadership_va_independent_verification.json").read_text())
assert qa["status"] == "passed"
for name, fingerprint in qa["report_input_sha256"].items():
    assert hashlib.sha256((DATA / name).read_bytes()).hexdigest() == fingerprint, f"Changed input since verification: {name}"
assert (DATA / "leadership_va_independent_verification.json").stat().st_mtime >= (DATA / "leadership_va_correlations.csv").stat().st_mtime
r = pd.read_csv(DATA / "leadership_va_correlations.csv")
rob = pd.read_csv(DATA / "leadership_va_index_robustness.csv")
diag = pd.read_csv(DATA / "leadership_index_diagnostics.csv").set_index("ROLE")
loads = pd.read_csv(DATA / "leadership_pca_loadings.csv")
wide = pd.read_csv(DATA / "leadership_indices_and_measures.csv")
tenure = pd.read_csv(DATA / "leadership_tenure_audit.csv")
outcomes = pd.read_csv(DATA / "leadership_va_outcome_dictionary.csv")
assert len(pd.read_csv(DATA / "leadership_va_analysis_warnings.csv")) == 0
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



history = json.loads((DATA / "leadership_history_verification.json").read_text())
assert history["status"] == "passed"
coverage = pd.read_csv(DATA / "leadership_feature_coverage.csv").set_index("METRIC")
def row(metric, outcome):
    return get("leadership", metric, outcome)
def fmt(v):
    return f"{v:.3f}"
def figure(name, height):
    story.append(Image(str(FIG / name), width=WIDTH, height=height))

p("Leadership characteristics<br/>and school value added", "ReportTitle")
p(f"Staff window: 2018-2024 | Histories: 2013 onward | {date.today():%d %B %Y}", "Note")
p("<b>Experience and credential scores show weak links to school VA.</b> The balanced index covers 3,461 schools. Its small raw correlations largely disappear after adjustment for observed school characteristics. Leadership headcount has clearer associations, but measures staffing capacity rather than individual quality.")
sub("Balanced index across all 12 VA outcomes")
table(["VA outcome", "Schools", "Pearson r", "Adj. beta", "BH q"],
      [[row("balanced_index", o).OUTCOME_LABEL, f'{row("balanced_index", o).N:,}',
        fmt(row("balanced_index", o).PEARSON_R), fmt(row("balanced_index", o).ADJUSTED_BETA_SD),
        qfmt(row("balanced_index", o).ADJUSTED_Q_BH)] for o in outcomes.OUTCOME],
      [238,66,72,70,70])
p("Each row uses its own observed outcome support. Main correlations give equal weight to schools and use saved empirical-Bayes VA. Adjusted beta is in VA SD per leadership-index SD, not a percentage income effect.", "Note")
p("No adjusted balanced-index association survives the 504-test leadership-family BH correction. This is not evidence that leadership is unimportant: observed qualifications and histories do not measure all relevant management skills.")
page()

title("Who counts, and how the score is built")
p("Leadership means primary or secondary function <b>3 Planta Directiva, 4 Director, 10 Directiva, or 15 Subdirector</b>. All appointments are examined; each person counts once per school-year. There is no classroom or HS-teaching-assignment restriction.")
p("The starting universe is 3,682 schools and 757,999 pooled VA students from grade-8 cohorts 2017-2020. The leadership panel contains 13,104 people and 51,397 person-school-years in 2018-2024. Code 16 and technical-pedagogical/inspection staff do not qualify unless they also hold an included leadership function.")
sub("Four components, fixed before looking at VA")
labels = {"prior_role_years":"Prior primary leadership years", "school_role_spell_years":"Current leadership spell at school",
          "university_share":"University-qualification share", "teaching_title_share":"Teaching-qualification share"}
table(["Component", "Observed schools", "Period mean", "PC1 loading"],
      [[label, f'{int(coverage.loc[m,"N_OBSERVED"]):,}',
        f'{coverage.loc[m,"MEAN"]:.2f}',
        fmt(loads[loads.ROLE.eq("leadership") & loads.COMPONENT.eq(m)].PC1_LOADING.iloc[0])]
       for m,label in labels.items()], [245,91,90,90])
p("Component coverage differs; the index requires all four and at least three active leadership years. Qualifying means are equal-person averages within a school-year and equal-active-year averages over 2018-2024. Require seven known annual role counts, at least 80% observed members per annual attribute and 80% valid active years.", "Note")
p("Standardize each component across complete schools. Average and standardize the two experience components; do the same for credentials. Average the two standardized blocks, then standardize the balanced index to SD 1. Staffing counts are not included in this score.")
sub("PCA is mainly an experience score")
p(f'PC1 explains {diag.loc["leadership","PC1_VARIANCE_SHARE"]:.1%} of four-component variance. Its loadings are concentrated on experience, with mixed signs on credentials. The experience and credential blocks correlate {diag.loc["leadership","COR_CAREER_CREDENTIALS"]:.3f}. Retain separate blocks; PCA does not establish one latent quality factor.')
p("University names and rankings are unavailable. Teaching qualifications and recorded specialties are not management-specific credentials. The high university and teaching-title shares also limit differentiation between leaders.", "Note")
page()

title("Components and staffing capacity")
p("Pearson correlations with EB school VA; each cell uses its own observed support. The full local tables include all 42 measures and all 12 outcomes, not only the selected matrix below.", "Note")
figure("leadership_correlations.png", 357)
sub("Headcount has clearer links than the balanced score")
table(["Mean leader headcount / VA", "Schools", "Pearson r", "Adj. beta", "BH q"],
      [[short[o], f'{row("mean_headcount",o).N:,}', fmt(row("mean_headcount",o).PEARSON_R),
        fmt(row("mean_headcount",o).ADJUSTED_BETA_SD), qfmt(row("mean_headcount",o).ADJUSTED_Q_BH)]
       for o in main_outcomes], [224,73,73,73,73])
p("Headcount and leaders per student describe capacity, not qualifications. Role-composition shares also reflect team structure: a lower director share may simply mean more other leaders. Codes can overlap within a person-year, so their shares need not sum to one.", "Note")
page()

title("Adjusted associations: keep the blocks separate")
p("Each row is a separate regression of standardized EB VA on one standardized leadership measure. Controls are log pooled VA sample size and its square, dependency, region, rural status, TP/artistic offerings and basic-school offerings. Directory controls are a 2024 snapshot, not predetermined characteristics.")
table(["Index/block", "VA outcome", "N", "Beta (SD)", "95% HC1 CI", "BH q"],
      [[label, short[o], f'{row(m,o).N_ADJUSTED:,}', fmt(row(m,o).ADJUSTED_BETA_SD), ci(row(m,o)),
        qfmt(row(m,o).ADJUSTED_Q_BH)]
       for m,label in [("balanced_index","Balanced"),("career_index","Experience"),("credentials_index","Credentials")]
       for o in main_outcomes], [86,108,52,70,132,68])
p("Neither experience nor the recorded-credentials block provides a strong adjusted signal in these three outcomes. The estimates do not identify causal effects: school resources, sorting and unobserved management can affect both staffing and VA.")
sub("Full-family inference")
sig = r[r.ADJUSTED_Q_BH.lt(.05)]
table(["Metric block", "Adjusted pairs with BH q < 0.05"],
      [[block.replace("_"," ").capitalize(), str(int((sig.BLOCK == block).sum()))]
       for block in ["index","career","credentials","staffing","role_composition","baseline_tenure","history_sensitivity"]],
      [300,216])
p(f'The full family contains 42 measures x 12 outcomes = 504 tests; {len(sig)} adjusted pairs have q below 0.05. Separate BH corrections are applied to Pearson and adjusted p-values. HC1 intervals and t-based p-values condition on the saved VA estimates; they do not propagate VA-estimation uncertainty.', "Note")
page()

title("History coding and robustness")
p("Experience uses only information available through each staff year. Prior primary-function years stop at t-1; current spells include t. Moves among leadership codes preserve leadership experience. Gaps reset consecutive spells but not cumulative experience. Later promotions never downgrade earlier observations.")
sub("Director coding changed after 2014")
p(f'MINEDUC Annex III, footnote 27, states that director code 4 included profesores encargados through 2014. Among current leaders, {history["pre2015_director_exposed_people"]:,} people have earlier code-4 history and {history["possible_2015_code_break_people"]} have a possible 2014 code-4 to 2015 code-16 transition. These comparisons are excluded from switching rates. They are coding flags, not proven career transitions.')
p("The sensitivity index rebuilds BOTH prior primary leadership years and school-leadership spells using only 2015 onward. It does not add current code-16 staff to leadership. Its complete-case sample is 3,447 schools.")
table(["VA outcome", "Main r", "2015+ r", "2015+ N", "2015+ adj. beta"],
      [[short[o], fmt(row("balanced_index",o).PEARSON_R), fmt(row("balanced_index_history2015",o).PEARSON_R),
        f'{row("balanced_index_history2015",o).N:,}', fmt(row("balanced_index_history2015",o).ADJUSTED_BETA_SD)]
       for o in main_outcomes], [182,70,70,80,114])
sub("Balanced-index correlations under other checks")
checks = [("Main EB",None),("No private-paid schools","no_private_paid"),
          ("At least 100 VA students","at_least_100_va_students"),
          ("At least five active years","at_least_5_active_staff_years")]
table(["Sample / VA choice","Math r","Language r","Income r"],
      [[label]+[fmt(row("balanced_index",o).PEARSON_R if key is None else
                    rob[rob.METRIC.eq("balanced_index") & rob.OUTCOME.eq(o) & rob.SUBSAMPLE.eq(key)].PEARSON_R.iloc[0])
                  for o in main_outcomes] for label,key in checks] +
      [["Unshrunk VA"]+[fmt(row("balanced_index",o).UNSHRUNK_VA_R) for o in main_outcomes]] +
      [["Student-weighted"]+[fmt(row("balanced_index",o).STUDENT_WEIGHTED_R) for o in main_outcomes]] +
      [["Spearman rank"]+[fmt(row("balanced_index",o).SPEARMAN_R) for o in main_outcomes]],
      [258,86,86,86])
p("Alternative-school checks retain the original index scaling; regressions standardize within their own estimation sample. The full 144-row robustness table preserves N and adjusted estimates. BH corrections are within each robustness subsample.", "Note")
page()

title("Patterns, limitations and verification")
figure("balanced_index_scatter.png", 233)
p("Gray dots are schools; teal dots are means in 20 rank-based index bins. VA is standardized within each panel. These are unadjusted displays, not causal dose-response relationships.", "Note")
sub("Data limitations that remain")
p("Reported school tenure is zero for 5.6% of leadership records in 2018 versus 68.8% in 2019, including many previously observed at the same school. Preserve it unchanged for audit; exclude the period field from the index and analyze 2018 separately. Observed career history is left-censored in 2013.")
p("Staffing ratios divide pooled VA-sample student counts by mean annual leadership headcount. They are not annual enrollment, workload, time allocation or actual HS caseload. Leaders at mixed-level schools may serve students outside HS.")
sub("Sources and checks")
p("Sources: MINEDUC public annual staff directories, 2013-2024, and the distributed 2024 ER_Cargos Docentes, bases publicas codebook (Annex III, p. 16); the existing broad-VA school context; and the saved All-sample school empirical-Bayes VA export. Raw files and source VA were not modified.", "Note")
p(f'28 leadership construction tests passed. Independent calculations rebuilt {history["history_values_rebuilt"]:,} staff-history values, all {qa["annual_metric_coverage_gates_checked"]:,} annual metric cells, {qa["period_aggregates_checked"]:,} period aggregates and {qa["school_index_scores_reconstructed"]:,} balanced-index scores, plus blocks and PCA. All 504 main correlations, 144 robustness correlations, all BH adjustments and {qa["independent_HC1_regressions"]} exact HC1 regressions were checked. No analysis warnings were recorded.', "Note")
p("Reusable tables, dictionaries and audits are in data/clean/leadership_quality_va/. The task README gives the reproduction sequence. Individual records remain local and excluded from Git; earlier teacher/orientador results remain byte-identical.", "Note")

def footer(canvas, doc):
    canvas.saveState()
    canvas.setStrokeColor(colors.HexColor("#ced9df"))
    canvas.line(48,42,564,42)
    canvas.setFont("Arial",8); canvas.setFillColor(GRAY)
    canvas.drawString(48,28,"Causal Schools | Leadership characteristics and school VA")
    canvas.drawRightString(564,28,str(doc.page))
    canvas.restoreState()
doc = SimpleDocTemplate(str(OUT), pagesize=(612,792), leftMargin=48,rightMargin=48,
                        topMargin=43,bottomMargin=55,title="Leadership characteristics and school value added",author="Causal Schools")
doc.build(story,onFirstPage=footer,onLaterPages=footer)
reader = PdfReader(OUT)
assert len(reader.pages) == 6, f"Expected 6 pages, obtained {len(reader.pages)}; inspect layout."
assert all(len(pg.extract_text()) > 300 for pg in reader.pages)
print(f"Created {len(reader.pages)}-page report: {OUT}")
