"""Check the complete public site before upload (including search and Markdown).

This is a regression guard, not a substitute for reviewing new documentation.
Add new public pages deliberately; never upload the repository as a Pages artifact.
"""
import argparse
import json
import os
import re
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urlsplit

GUIDES = {"start-here"}
# Old guide URLs kept as redirects to their replacements (see _pkgdown.yml).
REDIRECTS = {"thip-sources", "time-windows"}
# Website-only articles (vignettes/articles); pkgdown also writes a redirect page
# for each at articles/articles/<name>.
WEB_ARTICLES = {"meet-the-data", "database-from-r", "question-to-cohort", "look-back-follow-up",
                "condition-flags", "getting-started", "data-checks", "responsible-use"}
REFERENCES = {"collect.thip_window_plan", "icd_flag", "thip_collect", "thip_columns",
              "thip_connect", "thip_disconnect", "thip_filter_icd", "thip_setup",
              "thip_table", "thip_tables", "thip_window", "thip_date_audit",
              "thip_connect_example", "thip_fields",
              "thip_source_summary", "thip_run_info"}
PAGES = {"index", "authors", "404", "articles/index", "reference/index", "news/index",
         "AGENTS"}  # pkgdown renders every root .md; AGENTS.md is public guidance
PAGES |= {"articles/" + name for name in GUIDES | WEB_ARTICLES | REDIRECTS}
PAGES |= {"articles/articles/" + name for name in WEB_ARTICLES}
PAGES |= {"reference/" + name for name in REFERENCES}
ASSETS = {".css", ".js", ".map", ".svg", ".png", ".ico", ".woff", ".woff2", ".ttf"}
GENERATED = {"search.json", "sitemap.xml", "pkgdown.yml", "llms.txt", ".nojekyll", "deps/data-deps.txt"}
PATTERNS = {
    "internal material": r"VERIFICATION|ARCHITECTURE|meta/(?:runtime|workflows|benchmarks)|/Users/|candidate source rows|real-data rerun",
    "build machine description": r"\b\w+@(?:Darwin|Linux|Windows)\b",
    "private network address": r"\b10\.\d+\.\d+\.\d+\b|\b192\.168\.\d+\.\d+\b",
    "credential marker": r"gh[pousr]_[A-Za-z0-9]{30,}|github_pat_[A-Za-z0-9_]{30,}|-----BEGIN (?:RSA |OPENSSH |EC )?PRIVATE KEY-----",
    "credential assignment": r'''(?i)(?:password|token|api_key)\s*[=:]\s*["'][^"']{8,}["']''',
}
# Platform hostnames and account paths are kept out of the repository. Supply them
# as one regular expression in THIP_PRIVATE_PATTERNS (a CI secret, or locally).
if os.environ.get("THIP_PRIVATE_PATTERNS"):
    PATTERNS["private platform pattern"] = os.environ["THIP_PRIVATE_PATTERNS"]


class Links(HTMLParser):
    def __init__(self):
        super().__init__()
        self.targets = []

    def handle_starttag(self, tag, attrs):
        self.targets.extend(v for k, v in attrs if k in ("href", "src") and v)


def check(root):
    root = root.resolve()
    errors = []
    files = [p for p in root.rglob("*") if p.is_file() or p.is_symlink()]
    for p in files:
        name = p.relative_to(root).as_posix()
        if p.is_symlink():
            errors.append(f"Symlink: {name}")
            continue
        if p.suffix in {".html", ".md"}:
            if name.rsplit(".", 1)[0] not in PAGES:
                errors.append(f"Unreviewed page: {name}")
        elif name not in GENERATED and p.suffix not in ASSETS:
            errors.append(f"Unreviewed asset: {name}")
        if p.suffix in {".html", ".md", ".txt", ".json", ".yml", ".xml"}:
            content = p.read_text()
            for label, pattern in PATTERNS.items():
                if re.search(pattern, content):
                    errors.append(f"{label}: {name}")  # Never print matching values.
            if p.suffix == ".html":
                parser = Links()
                parser.feed(content)
                for target in parser.targets:
                    url = urlsplit(target)
                    if url.scheme or url.netloc or not url.path:
                        continue
                    resolved = (p.parent / unquote(url.path)).resolve()
                    if not resolved.is_relative_to(root) or not resolved.exists():
                        errors.append(f"Broken local link in {name}")
    for page in PAGES:
        if not (root / (page + ".html")).is_file():
            errors.append(f"Missing public page: {page}")
    search = root / "search.json"
    if not search.is_file():
        errors.append("Missing search index")
    else:
        json.loads(search.read_text())
    if errors:
        raise ValueError("\n".join(sorted(set(errors))))
    print(f"Public site checked: {len(files)} files; {len(PAGES)} pages; search index included.")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("site", type=Path)
    check(parser.parse_args().site)
