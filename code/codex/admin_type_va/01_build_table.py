"""Regress school EB value added on administration type and curricular track."""
from pathlib import Path
import math
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[3]
OUT = ROOT / 'output/tables/admin_type_va'; OUT.mkdir(parents=True, exist_ok=True)
OUTCOMES = [('z_year_math_max','Math VA'),('z_year_leng_max','Verbal VA'),
 ('high_inst_m1','HP inst. VA'),('high_paying_field_m1','HP field VA'),
 ('log_program_income_clp_m1','Income VA'),('admission_exam_taker','Exam VA'),
 ('any_postulacion','Fin. aid app. VA')]
ROWS = [('intercept','Intercept (omitted: Municipal, no tracks, 100--249 students)','base'),
 ('admin_1','Municipal corporation','admin'),('admin_3','Private subsidized','admin'),
 ('admin_4','Fully private','admin'),('admin_5','Delegated administration','admin'),
 ('admin_6','SLEP','admin'),('tp','Technical-professional','track'),('artistic','Artistic','track')]
ROWS += [('size_under100','Fewer than 100 students','size'),
         ('size_250_499','250--499 students','size'),
         ('size_500_plus','500+ students','size')]

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
    yy=(y[keep]-y[keep].mean())/y[keep].std(ddof=1); a=admin[keep]; t=tp[keep]; q=artistic[keep]
    size=x.loc[keep,'HS_ENROLLED_EST'].to_numpy()
    s0=size<100; s1=(size>=100)&(size<250); s2=(size>=250)&(size<500); s3=size>=500
    students=np.exp(x.loc[keep,'school__log_va_students'].to_numpy())
    X=np.column_stack([np.ones(keep.sum()),*(a==k for k in (1,3,4,5,6)),t,q,s0,s2,s3]).astype(float)
    assert np.linalg.matrix_rank(X)==X.shape[1]
    b,se=fit(X,yy); masks=[(a==2)&(t==0)&(q==0)&s1,a==1,a==3,a==4,a==5,a==6,t==1,q==1,s0,s2,s3]
    for j,((feature,label,block),mask) in enumerate(zip(ROWS,masks)):
        p=math.erfc(abs(b[j]/se[j])/math.sqrt(2)); stars='***' if p<.01 else '**' if p<.05 else '*' if p<.10 else ''
        out.append(dict(outcome=outcome,feature=feature,label=label,block=block,estimate=b[j],se=se[j],p=p,stars=stars,
          schools=int(mask.sum()),student_share=students[mask].sum()/students.sum(),sample_schools=int(keep.sum())))
r=pd.DataFrame(out); r.to_csv(OUT/'admin_type_va.csv',index=False)

def cell(d): return f'{d.estimate:.3f}'+(f'$^{{{d.stars}}}$' if d.stars else '')
cols=[o for o,_ in OUTCOMES]; nc=len(cols)
L=[r'\begin{table}[!htbp]',r'\centering',r'\caption{School value added by administration type, curricular track, and school size}',
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
L += [r'\midrule',f'Schools & {int(r.sample_schools.iloc[0]):,} & 100.0\\% & '+' & '.join(['']*nc)+r' \\',
 r'\bottomrule',r'\end{tabular}}',r'\par\medskip',r'\footnotesize',r'\begin{minipage}{\textwidth}',
 'Notes: Each column is a school-level OLS regression of the indicated Empirical-Bayes value-added measure on administration-type, curricular-track, and school-size indicators. Value added is standardized across schools. The omitted categories are Municipal DAEM, neither technical-professional nor artistic, and 100--249 estimated high-school students; the first row reports the intercept. All other rows report regression coefficients. There is no minimum-school-size restriction, and fully private schools are included. School size is estimated total enrollment across grades 9--12 using the four grade-8 cohorts. The first two columns report the number of schools and share of pooled value-added-sample students satisfying each row definition. Track and size categories overlap administration types. Heteroskedasticity-robust (HC1) standard errors are in parentheses and do not incorporate estimation error in school value added. '+r'* $p<0.10$, ** $p<0.05$, *** $p<0.01$.',
 r'\end{minipage}',r'\end{table}']
(OUT/'admin_type_va.tex').write_text('\n'.join(L)+'\n',encoding='utf-8')
print(r.to_string(index=False))
