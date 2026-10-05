import csv
import re
from pathlib import Path

root = Path('data/clean/sae_participation')
ranges = {2013: [25, 26, 27, 28, 29], 2014: [17, 18, 19, 20, 21, 22, 25],
          2015: [24, 25, 26, 27, 28, 29, 30, 31, 32], 2016: [18, 22, 23, 25, 26, 27, 28]}
out = []
for year, questions in ranges.items():
    out.append(str(year))
    for r in csv.DictReader((root / f'glosa_{year}__cpad.csv').open(encoding='utf-8-sig', newline='')):
        m = re.match(r'cpad_p(\d+)', r['V1'])
        if m and int(m[1]) in questions:
            out.append(r['V1'] + ': ' + re.sub(r'\s+', ' ', r['V2'])[-220:] + ' | ' + re.sub(r'\s+', ' ', r['V3']))
text = '\n'.join(out)
Path('tmp/sae_parent_candidates.txt').write_text(text, encoding='utf-8')
print(text)
