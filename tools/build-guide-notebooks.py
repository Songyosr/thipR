"""Generate THIP guide notebooks from the package vignette sources.

No execution, credentials, DB access, or third-party Python dependencies.
Synthetic chunks become runnable cells; eval=FALSE snippets remain examples.
"""
import argparse
import json
import re
from pathlib import Path

GUIDES = ['start-here', 'thip-sources', 'time-windows', 'condition-flags', 'getting-started']
TITLES = ['Start here', 'Prepare and filter sources', 'Retrieve time windows',
          'Flag conditions locally', 'Worked cohort example']

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
        elif re.search(r'eval\s*=\s*FALSE', opts):
            cells.append(markdown('Example for an authorized THIP session (not run automatically):\n\n```r\n' + code + '\n```'))
        else:
            if 'library(thipR)' in code:
                code = '.libPaths(c(path.expand("~/R/thipR-library"), .libPaths()))\n' + code
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
    for guide in GUIDES:
        (args.output / (guide + '.ipynb')).write_text(json.dumps(convert(repo / 'vignettes' / (guide + '.Rmd')), indent=2) + '\n')
    index = '# thipR guide index\n\nChoose one short guide. Each synthetic notebook is independent; no need to run the other notebooks first.\n\n'
    for i, (guide, title) in enumerate(zip(GUIDES, TITLES), 1):
        index += f'{i}. [{title}]({guide}.ipynb)\n'
    index += '\nGuides 1–2 explain authorized remote access with nonexecuted snippets. Guides 3–5 contain runnable synthetic cells, with no DB access.\n\nThe same guides are installed as R vignettes; for example:\n\n```r\nvignette("time-windows", package = "thipR")\n```\n\nPublic documentation: https://songyosr.github.io/thipR/. These THIP notebook copies require your platform login. Generated notebooks contain no patient records or credentials.'
    (args.output / 'Documentation.ipynb').write_text(json.dumps(notebook([markdown(index)]), indent=2) + '\n')
