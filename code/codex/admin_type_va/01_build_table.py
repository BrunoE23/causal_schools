"""Regress school EB value added on administration type and curricular track."""
from pathlib import Path
import math
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[3]
OUT = ROOT / 'output/tables/admin_type_va'; OUT.mkdir(parents=True, exist_ok=True)
OUTCOMES = [('z_year_math_max','Math VA (SD)'),('z_year_leng_max','Verbal VA (SD)'),
 ('high_inst_m1','HP inst. VA (pp)'),('high_paying_field_m1','HP field VA (pp)'),
 ('log_program_income_clp_m1','Income VA (log pts.)'),('admission_exam_taker','Exam VA (pp)'),
 ('any_postulacion','Fin. aid app. VA (pp)')]
RATE_OUTCOMES = {'high_inst_m1','high_paying_field_m1','admission_exam_taker','any_postulacion'}
ROWS = [('intercept','Intercept (omitted: Municipal, 100--249 students, no tracks)','base'),
 ('admin_1','Municipal corporation','admin'),('admin_3','Private subsidized','admin'),
 ('admin_4','Fully private','admin'),('admin_5','Delegated administration','admin'),
 ('admin_6','SLEP','admin')]
ROWS += [('size_under100','Fewer than 100 students','size'),
         ('size_250_499','250--499 students','size'),
         ('size_500_plus','500+ students','size')]
ROWS += [('tp','Technical-professional','track'),('artistic','Artistic','track')]

x = pd.read_csv(ROOT/'data/clean/staff_lasso_va/school_predictors.csv').sort_values('RBD').reset_index(drop=True)
enrollment = pd.read_csv(ROOT/'data/clean/staff_va_compact_inputs/hs_enrollment_universe_2017_2020.csv')
x = x.merge(enrollment[['RBD','HS_ENROLLED_EST']], on='RBD', how='left', validate='one_to_one')
admin = np.ones(len(x), int)
for k in (2,3,4,5,6): admin[x[f'school__dependency_{k}'].eq(1).to_numpy()] = k
art_rbd = set(pd.read_csv(ROOT/'data/clean/staff_va_compact_inputs/artistic_rbd_2024.csv').RBD)
artistic = x.RBD.isin(art_rbd).to_numpy(float)
tp = x.school__has_tp_or_artistic.to_numpy(float) - artistic
assert set(np.unique(tp)) <= {0.,1.}
va = pd.read_csv(ROOT/'data/clean/staff_va_compact_inputs/eb_school_rbd_observational_values_cohorts_2017_2020.csv',
 usecols=['school_rbd','analysis_sample','outcome','controlled_value_added_eb_centered_student'])
va = va[va.analysis_sample.eq('All')]

def fit(X,y):
    inv=np.linalg.inv(X.T@X); b=inv@X.T@y; e=y-X@b; n,k=X.shape
    V=n/(n-k)*inv@(X*e[:,None]).T@(X*e[:,None])@inv
    return b,np.sqrt(np.diag(V))

out=[]
for outcome,_ in OUTCOMES:
    y=va[va.outcome.eq(outcome)].set_index('school_rbd').reindex(x.RBD).controlled_value_added_eb_centered_student.to_numpy()
    keep=np.isfinite(y)&x.HS_ENROLLED_EST.gt(0).to_numpy()
    yy=y[keep] * (100 if outcome in RATE_OUTCOMES else 1); a=admin[keep]; t=tp[keep]; q=artistic[keep]
    size=x.loc[keep,'HS_ENROLLED_EST'].to_numpy()
    s0=size<100; s1=(size>=100)&(size<250); s2=(size>=250)&(size<500); s3=size>=500
    students=np.exp(x.loc[keep,'school__log_va_students'].to_numpy())
    municipal_share_all=students[a==2].sum()/students.sum()
    size_100_249_share_all=students[s1].sum()/students.sum()
    X=np.column_stack([np.ones(keep.sum()),*(a==k for k in (1,3,4,5,6)),s0,s2,s3,t,q]).astype(float)
    assert np.linalg.matrix_rank(X)==X.shape[1]
    b,se=fit(X,yy); masks=[(a==2)&(t==0)&(q==0)&s1,a==1,a==3,a==4,a==5,a==6,s0,s2,s3,t==1,q==1]
    for j,((feature,label,block),mask) in enumerate(zip(ROWS,masks)):
        p=math.erfc(abs(b[j]/se[j])/math.sqrt(2)); stars='***' if p<.01 else '**' if p<.05 else '*' if p<.10 else ''
        out.append(dict(outcome=outcome,feature=feature,label=label,block=block,estimate=b[j],se=se[j],p=p,stars=stars,
          schools=int(mask.sum()),student_share=students[mask].sum()/students.sum(),sample_schools=int(keep.sum())))
        out[-1]['municipal_share_all']=municipal_share_all
        out[-1]['size_100_249_share_all']=size_100_249_share_all
r=pd.DataFrame(out); r.to_csv(OUT/'admin_type_va.csv',index=False)

def cell(d): return f'{d.estimate:.3f}'+(f'$^{{{d.stars}}}$' if d.stars else '')
cols=[o for o,_ in OUTCOMES]; nc=len(cols)
L=[r'\begin{table}[!htbp]',r'\centering',r'\caption{School value added by administration type, school size, and curricular track}',
 r'\label{tab:admin-type-va}',r'\resizebox{\textwidth}{!}{%',r'\begin{tabular}{lrc'+'c'*nc+'}',r'\toprule',
 ' & $N$ schools & Share of students & '+' & '.join(f'({j+1})' for j in range(nc))+r' \\',
 ' & & & '+' & '.join(z for _,z in OUTCOMES)+r' \\',r'\midrule']
prev=None
for feature,label,block in ROWS:
    if prev=='base':
        L.append(r'\multicolumn{'+str(nc+3)+r'}{l}{\textit{Administration type}} \\')
    elif block=='track' and prev!='track':
        L.append(r'\multicolumn{'+str(nc+3)+r'}{l}{\textit{School tracks}} \\')
    elif block=='size' and prev!='size':
        L.append(r'\multicolumn{'+str(nc+3)+r'}{l}{\textit{School size}} \\')
    z=r[r.feature.eq(feature)].set_index('outcome').loc[cols]; n=int(z.schools.iloc[0]); sh=z.student_share.iloc[0]
    share_text = r'$<0.1\%$' if 0 < sh < .001 else f'{100*sh:.1f}\\%'
    L.append(label+f' & {n:,} & {share_text} & '+' & '.join(cell(z.loc[o]) for o in cols)+r' \\')
    L.append(' & & & '+' & '.join(f'({z.loc[o,"se"]:.3f})' for o in cols)+r' \\'); prev=block
municipal_share=100*r.municipal_share_all.iloc[0]
size_base_share=100*r.size_100_249_share_all.iloc[0]
L += [r'\midrule',f'Schools & {int(r.sample_schools.iloc[0]):,} & 100.0\\% & '+' & '.join(['']*nc)+r' \\',
 r'\bottomrule',r'\end{tabular}}',r'\par\medskip',r'\footnotesize',r'\begin{minipage}{\textwidth}',
 'Notes: Each column is a school-level OLS regression of the indicated Empirical-Bayes value-added measure on administration-type, school-size, and curricular-track indicators. Outcomes are reported in their original units: math and verbal achievement in student standard deviations, binary outcomes in percentage points (pp), and projected income in log points. The omitted categories are Municipal DAEM, 100--249 estimated high-school students, and neither technical-professional nor artistic; the first row reports the intercept. All other rows report regression coefficients. There is no minimum-school-size restriction, and fully private schools are included. School size is estimated total enrollment across grades 9--12 using the four grade-8 cohorts. The first two columns report the number of schools and share of pooled value-added-sample students satisfying each row definition. Unconditionally, Municipal schools account for '+f'{municipal_share:.1f}'+r'\% of students and schools with 100--249 students account for '+f'{size_base_share:.1f}'+r'\%; these shares do not condition on the other omitted categories. Size and track categories overlap administration types. Heteroskedasticity-robust (HC1) standard errors are in parentheses and do not incorporate estimation error in school value added. '+r'* $p<0.10$, ** $p<0.05$, *** $p<0.01$.',
 r'\end{minipage}',r'\end{table}']
(OUT/'admin_type_va.tex').write_text('\n'.join(L)+'\n',encoding='utf-8')
print(r.to_string(index=False))
