// Exercise the generated report's aggregate-only table and local artifact links.
const fs = require('fs');
const path = require('path');
const vm = require('vm');

try {
  const report = path.resolve('output/reports/applicant_composition_trends/report.html');
  const html = fs.readFileSync(report, 'utf8');
  const scripts = [...html.matchAll(/<script>([\s\S]*?)<\/script>/g)];
  if (scripts.length !== 1) throw new Error('Unexpected script count');
  const nodes = { outcome: { value: 'simce_baseline' }, search: { value: '' }, count: {}, rows: {} };
  for (const node of Object.values(nodes)) node.addEventListener = () => {};
  const context = vm.createContext({ document: { getElementById: id => nodes[id] } });
  vm.runInContext(scripts[0][1], context);
  const outcomes = vm.runInContext('[...new Set(data.map(d => d.outcome))]', context);
  if (vm.runInContext('data.length', context) !== 1000 || outcomes.length !== 10) {
    throw new Error('Unexpected report-data dimensions');
  }
  for (const outcome of outcomes) {
    nodes.outcome.value = outcome;
    vm.runInContext('draw()', context);
    if (nodes.count.textContent !== '100 schools' || (nodes.rows.innerHTML.match(/<tr>/g) || []).length !== 100) {
      throw new Error(`Incomplete table: ${outcome}`);
    }
  }
  nodes.search.value = '2935';
  vm.runInContext('draw()', context);
  if (nodes.count.textContent !== '1 schools') throw new Error('RBD search failed');
  if (!nodes.rows.innerHTML.includes('LICEO MARTA DONOSO ESPEJO')) throw new Error('Wrong school returned');
  const missing = [...html.matchAll(/(?:src|href)="(\.\.\/[^"#]+)"/g)]
    .map(x => path.resolve(path.dirname(report), x[1])).filter(x => !fs.existsSync(x));
  if (missing.length) throw new Error(`Broken links: ${missing.join(', ')}`);
  console.log('Report checks passed: all 10 outcome tables, school search, and every figure/download link.');
} catch (error) {
  console.error(error.message);
  process.exitCode = 1;
}
