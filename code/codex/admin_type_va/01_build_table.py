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
ROWS = [('intercept','Intercept (omitted category: Municipal)','base'),
 ('admin_1',r'Corporaci\'on Municipal','admin'),('admin_3','Private subsidized','admin'),
 ('admin_4','Fully private','admin'),('admin_5','Delegated administration','admin'),
 ('admin_6','SLEP','admin'),('tp','Technical-professional','track'),('artistic','Artistic','track')]

x = pd.read_csv(ROOT/'data/clean/staff_lasso_va/school_predictors.csv').sort_values('RBD').reset_index(drop=True)
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
    keep=np.isfinite(y)&(np.exp(x.school__log_va_students.to_numpy())>=100-1e-6)
    yy=(y[keep]-y[keep].mean())/y[keep].std(ddof=1); a=admin[keep]; t=tp[keep]; q=artistic[keep]
    students=np.exp(x.loc[keep,'school__log_va_students'].to_numpy())
    X=np.column_stack([np.ones(keep.sum()),*(a==k for k in (1,3,4,5,6)),t,q]).astype(float)
    assert np.linalg.matrix_rank(X)==X.shape[1]
    b,se=fit(X,yy); masks=[(a==2)&(t==0)&(q==0),a==1,a==3,a==4,a==5,a==6,t==1,q==1]
    for j,((feature,label,block),mask) in enumerate(zip(ROWS,masks)):
        p=math.erfc(abs(b[j]/se[j])/math.sqrt(2)); stars='***' if p<.01 else '**' if p<.05 else '*' if p<.10 else ''
        out.append(dict(outcome=outcome,feature=feature,label=label,block=block,estimate=b[j],se=se[j],p=p,stars=stars,
          schools=int(mask.sum()),student_share=students[mask].sum()/students.sum(),sample_schools=int(keep.sum())))
r=pd.DataFrame(out); r.to_csv(OUT/'admin_type_va.csv',index=False)

def cell(d): return f'{d.estimate:.3f}'+(f'$^{{{d.stars}}}$' if d.stars else '')
cols=[o for o,_ in OUTCOMES]; nc=len(cols)
L=[r'\begin{table}[!htbp]',r'\centering',r'\caption{School value added by administration type and curricular track}',
 r'\label{tab:admin-type-va}',r'\resizebox{\textwidth}{!}{%',r'\begin{tabular}{lrc'+'c'*nc+'}',r'\toprule',
 ' & $N$ schools & Share of students & '+' & '.join(f'({j+1})' for j in range(nc))+r' \\',
 ' & & & '+' & '.join(z for _,z in OUTCOMES)+r' \\',r'\midrule']
prev=None
for feature,label,block in ROWS:
    if prev=='base' or (block=='track' and prev!='track'): L.append(r'\midrule')
    z=r[r.feature.eq(feature)].set_index('outcome').loc[cols]; n=int(z.schools.iloc[0]); sh=z.student_share.iloc[0]
    share_text = r'$<0.1\%$' if 0 < sh < .001 else f'{100*sh:.1f}\\%'
    L.append(label+f' & {n:,} & {share_text} & '+' & '.join(cell(z.loc[o]) for o in cols)+r' \\')
    L.append(' & & & '+' & '.join(f'({z.loc[o,"se"]:.3f})' for o in cols)+r' \\'); prev=block
L += [r'\midrule',f'Schools & {int(r.sample_schools.iloc[0]):,} & 100.0\\% & '+' & '.join(['']*nc)+r' \\',
 r'\bottomrule',r'\end{tabular}}',r'\par\medskip',r'\footnotesize',r'\begin{minipage}{\textwidth}',
 'Notes: Each column is a school-level OLS regression of the indicated Empirical-Bayes value-added measure on administration-type indicators and indicators for technical-professional and artistic offerings. Value added is standardized across schools. The omitted administration category is Municipal DAEM; the first row reports the intercept. All other rows report regression coefficients. The sample contains schools with at least 100 students in the value-added sample and includes fully private schools. The first two columns report the number of schools and share of pooled value-added-sample students satisfying each row definition. For the intercept, this is the Municipal group with neither track indicator; track categories overlap administration types. Heteroskedasticity-robust (HC1) standard errors are in parentheses and do not incorporate estimation error in school value added. '+r'* $p<0.10$, ** $p<0.05$, *** $p<0.01$.',
 r'\end{minipage}',r'\end{table}']
(OUT/'admin_type_va.tex').write_text('\n'.join(L)+'\n',encoding='utf-8')
print(r.to_string(index=False))
