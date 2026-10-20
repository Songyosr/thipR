"""Check tracked repository files for internal content before it becomes public.

Runs on every push and pull request. A regression guard, not a substitute for
review: this repository is public, so anything committed is published.
Never prints matching values.
"""
import os
import re
import subprocess
import sys

PATTERNS = {
    "internal workspace path": r"meta/(?:runtime|workflows|benchmarks|notes|irb|concepts|thip-audit)|/Users/[A-Za-z]|thipR-dev-archive",
    "internal result wording": r"candidate source rows|real-data rerun|thip-remote-runs/",
    "private network address": r"\b10\.\d+\.\d+\.\d+\b|\b192\.168\.\d+\.\d+\b",
    "credential marker": r"gh[pousr]_[A-Za-z0-9]{30,}|github_pat_[A-Za-z0-9_]{30,}|-----BEGIN (?:RSA |OPENSSH |EC )?PRIVATE KEY-----",
}
# Platform hostnames and account paths are kept out of the repository. Supply them
# as one regular expression in THIP_PRIVATE_PATTERNS (a CI secret, or locally).
if os.environ.get("THIP_PRIVATE_PATTERNS"):
    PATTERNS["private platform pattern"] = os.environ["THIP_PRIVATE_PATTERNS"]

# The checkers necessarily contain the pattern text itself.
SKIP = {"tools/check-public-docs.py", "tools/check-repo-files.py"}


def main():
    files = subprocess.run(["git", "ls-files", "-z"], capture_output=True, check=True).stdout
    errors = []
    for name in sorted(f for f in files.decode().split("\0") if f and f not in SKIP):
        try:
            content = open(name, encoding="utf-8").read()
        except (UnicodeDecodeError, FileNotFoundError, IsADirectoryError):
            continue
        for label, pattern in PATTERNS.items():
            if re.search(pattern, content):
                errors.append(f"{label}: {name}")
    if errors:
        sys.exit("Internal content found in tracked files:\n" + "\n".join(errors))
    private = "with" if "private platform pattern" in PATTERNS else "without"
    print(f"Repository files checked {private} private platform patterns: no internal content found.")


if __name__ == "__main__":
    main()
