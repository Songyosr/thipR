"""Generate THIP guide notebooks from the package's guide sources.

No execution, credentials, DB access, or third-party Python dependencies.
Synthetic chunks become runnable cells; eval=FALSE snippets remain examples.
"""
import argparse
import json
import re
from pathlib import Path

# (source relative to vignettes/, title); the course guides are website-only articles.
SOURCES = [('start-here.Rmd', 'Start here'),
           ('articles/meet-the-data.Rmd', 'Meet the THIP data'),
           ('articles/database-from-r.Rmd', 'Working with a database from R'),
           ('articles/question-to-cohort.Rmd', 'From question to cohort'),
           ('articles/look-back-follow-up.Rmd', 'Look back and follow up'),
           ('articles/condition-flags.Rmd', 'Flag conditions'),
           ('articles/getting-started.Rmd', 'Worked example'),
           ('articles/data-checks.Rmd', 'Check before you trust'),
           ('articles/responsible-use.Rmd', 'Using THIP data responsibly')]
GUIDES = [Path(source).stem for source, _ in SOURCES]

def links(text):
    for guide in GUIDES:
        text = text.replace(guide + '.html', guide + '.ipynb')
    return text

def notebook(cells):
    for i, cell in enumerate(cells):
        cell['id'] = 'guide-cell-' + str(i)
    return {'cells': cells, 'metadata': {'kernelspec': {'name': 'ir', 'display_name': 'R',
            'language': 'R'}, 'language_info': {'name': 'R'}}, 'nbformat': 4, 'nbformat_minor': 5}

def markdown(text):
    return {'cell_type': 'markdown', 'metadata': {}, 'source': links(text.strip())}

def convert(path):
    text = path.read_text()
    title = re.search(r'^title: "(.*)"$', text, re.M).group(1)
    text = re.sub(r'^---\n.*?\n---\n', '', text, count=1, flags=re.S)
    cells = [markdown('# ' + title + '\n\n[All guides](Documentation.ipynb)')]
    pattern = re.compile(r'^```\{r([^}]*)\}\n(.*?)^```\s*$', re.M | re.S)
    pos = 0
    for match in pattern.finditer(text):
        prose = text[pos:match.start()]
        if prose.strip(): cells.append(markdown(prose))
        opts, code = match.group(1), match.group(2).rstrip()
        if re.search(r'include\s*=\s*FALSE', opts):
            pass  # Formatting-only knitr setup; never a data/analysis dependency.
        elif re.search(r'echo\s*=\s*FALSE', opts) and re.search(r'results\s*=\s*"asis"', opts):
            pass  # Inlined figures and widgets for the website, not analysis code.
        elif re.search(r'eval\s*=\s*FALSE', opts):
            cells.append(markdown('Example for an authorized THIP session (not run automatically):\n\n```r\n' + code + '\n```'))
        else:
            cells.append({'cell_type': 'code', 'metadata': {}, 'source': code,
                          'outputs': [], 'execution_count': None})
        pos = match.end()
    if text[pos:].strip(): cells.append(markdown(text[pos:]))
    return notebook(cells)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('output', type=Path)
    args = parser.parse_args()
    repo = Path(__file__).resolve().parents[1]
    args.output.mkdir(parents=True, exist_ok=True)
    for (source, _), guide in zip(SOURCES, GUIDES):
        (args.output / (guide + '.ipynb')).write_text(json.dumps(convert(repo / 'vignettes' / source), indent=2) + '\n')
    index = '# thipR guide index\n\nStart here sets up access; the guides after it form a short course, best read in order.\n\n'
    for i, (guide, (_, title)) in enumerate(zip(GUIDES, SOURCES)):
        index += f'{i}. [{title}]({guide}.ipynb)\n'
    index += '\nThe course guides run on the example database from `thip_connect_example()`, which needs the duckdb package; on THIP, connect with `thip_connect()` instead.\n\nPublic documentation: https://songyosr.github.io/thipR/. These THIP notebook copies require your platform login. Generated notebooks contain no patient records or credentials.'
    (args.output / 'Documentation.ipynb').write_text(json.dumps(notebook([markdown(index)]), indent=2) + '\n')
