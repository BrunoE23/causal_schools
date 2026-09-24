"""Plot the school-size distribution in the administration-VA sample."""
from pathlib import Path
import numpy as np
import pandas as pd
from PIL import Image, ImageDraw, ImageFont

ROOT = Path(__file__).resolve().parents[3]
OUT = ROOT / 'output/figures/admin_type_va'
OUT.mkdir(parents=True, exist_ok=True)
x = pd.read_csv(ROOT/'data/clean/staff_lasso_va/school_predictors.csv')
e = pd.read_csv(ROOT/'data/clean/staff_va_compact_inputs/hs_enrollment_universe_2017_2020.csv')
x = x.merge(e[['RBD','HS_ENROLLED_EST']], on='RBD', how='left')
admin = np.ones(len(x), int)
for k in (2,3,4,5,6): admin[x[f'school__dependency_{k}'].eq(1).to_numpy()] = k
labels = {1:'Corp. municipal',2:'Municipal',3:'Private subsidized',4:'Fully private',
          5:'Delegated',6:'SLEP'}
x['Administration'] = pd.Series(admin).map(labels)
keep = (np.exp(x.school__log_va_students) >= 100-1e-6) & x.HS_ENROLLED_EST.gt(0)
d = x.loc[keep, ['RBD','Administration','HS_ENROLLED_EST']].copy()
d.to_csv(OUT/'school_size_distribution_data.csv', index=False)

W,H=2200,900; im=Image.new('RGB',(W,H),'white'); dr=ImageDraw.Draw(im)
font_path='C:/Windows/Fonts/arial.ttf'; bold_path='C:/Windows/Fonts/arialbd.ttf'
font=lambda n,b=False: ImageFont.truetype(bold_path if b else font_path,n)
dr.text((W//2,35),'Distribution of school size',font=font(42,True),fill='#222',anchor='ma')

# Histogram, with the full observed range.
left,top,right,bottom=120,135,1250,730
bins=np.arange(100,2701,100); hist,edges=np.histogram(d.HS_ENROLLED_EST,bins=bins)
xmap=lambda z:left+(z-100)/(2600-100)*(right-left); ymax=hist.max()*1.08
ymap=lambda z:bottom-z/ymax*(bottom-top)
for h,a,b in zip(hist,edges[:-1],edges[1:]):
    dr.rectangle((xmap(a)+1,ymap(h),xmap(b)-1,bottom),fill='#4472A9')
dr.line((left,top,left,bottom,right,bottom),fill='#333',width=2)
for z in range(100,2601,500):
    xx=xmap(z); dr.line((xx,bottom,xx,bottom+8),fill='#333',width=2)
    dr.text((xx,bottom+14),f'{z:,}',font=font(22),fill='#333',anchor='ma')
for z in range(0,int(ymax)+1,100):
    yy=ymap(z); dr.text((left-15,yy),str(z),font=font(22),fill='#333',anchor='rm')
dr.text(((left+right)//2,790),'Estimated students enrolled',font=font(27),fill='#222',anchor='ma')
dr.text((left,top-25),'Number of schools',font=font(23),fill='#444',anchor='ls')
dr.text(((left+right)//2,95),'Overall distribution',font=font(30,True),fill='#222',anchor='ma')
med,mean=d.HS_ENROLLED_EST.median(),d.HS_ENROLLED_EST.mean()
for z,col,w in [(med,'#9C2F2F',5),(mean,'#222222',3)]: dr.line((xmap(z),top,xmap(z),bottom),fill=col,width=w)
dr.text((right-10,150),f'Median: {med:,.0f}\nMean: {mean:,.0f}',font=font(24),fill='#222',anchor='ra',spacing=8)

# Horizontal box plots by administration type (whiskers at 10th/90th pct.).
l2,r2=1580,2120; t2,b2=160,700; xmax=1400
dr.text(((l2+r2)//2,95),'By administration type',font=font(30,True),fill='#222',anchor='ma')
x2=lambda z:l2+min(z,xmax)/xmax*(r2-l2)
order=['Municipal','Corp. municipal','Private subsidized','Fully private','Delegated','SLEP']
for i,g in enumerate(order):
    vals=d.loc[d.Administration.eq(g),'HS_ENROLLED_EST'].to_numpy(); p10,q1,md,q3,p90=np.quantile(vals,[.1,.25,.5,.75,.9]); y=190+i*82
    dr.text((l2-20,y),g,font=font(23),fill='#222',anchor='rm')
    dr.line((x2(p10),y,x2(p90),y),fill='#4472A9',width=4)
    dr.rectangle((x2(q1),y-18,x2(q3),y+18),fill='#B8CBE3',outline='#4472A9',width=3)
    dr.line((x2(md),y-20,x2(md),y+20),fill='#9C2F2F',width=5)
for z in (0,500,1000,1400):
    xx=x2(z); dr.line((xx,b2,xx,b2+8),fill='#333',width=2); dr.text((xx,b2+14),f'{z:,}',font=font(22),fill='#333',anchor='ma')
dr.line((l2,b2,r2,b2),fill='#333',width=2)
dr.text(((l2+r2)//2,790),'Estimated students enrolled',font=font(27),fill='#222',anchor='ma')
dr.text((W//2,865),'Sample: 2,334 schools with at least 100 students contributing to value-added estimates',font=font(22),fill='#555',anchor='ma')
im.save(OUT/'school_size_distribution.png')
print(d.HS_ENROLLED_EST.describe(percentiles=[.1,.25,.5,.75,.9,.95,.99]).round(1))
