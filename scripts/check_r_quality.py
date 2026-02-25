"""Quick R script quality check (subset of quality_score.py rubric)."""
import re
import sys

filepath = r"C:\Users\xc77\my-paper-ra\replications\FuEtal2024\R\replicate.R"

with open(filepath, encoding="utf-8") as f:
    content = f.read()
lines = content.split("\n")

# 1. Hardcoded absolute paths
# Match Windows drive paths (C:/ C:\) or Unix absolute paths (/word - not \n \t escapes)
path_pattern = re.compile(r"""['"][A-Za-z]:[/\\]|['"]/[A-Za-z]""")
skip_pattern = re.compile(r"http:|https:|file://|/tmp/")
path_issues = []
for i, line in enumerate(lines, 1):
    if path_pattern.search(line) and not skip_pattern.search(line):
        path_issues.append((i, line.strip()))

# 2. set.seed / randomness
random_fns = ["rnorm", "runif", "sample", "rbinom", "rnbinom"]
has_random = any(fn in content for fn in random_fns)
has_seed   = "set.seed" in content

# Report
print(f"Hardcoded absolute paths: {len(path_issues)}")
for ln, txt in path_issues[:15]:
    print(f"  Line {ln}: {txt[:90].encode('ascii', errors='replace').decode()}")

print(f"\nRandomness functions present: {has_random}")
print(f"set.seed present:             {has_seed}")
if has_random and not has_seed:
    print("  -> MAJOR: missing set.seed (-10 pts)")

score = 100
score -= 20 * len(path_issues)
if has_random and not has_seed:
    score -= 10

status = "COMMIT_READY" if score >= 80 else "BLOCKED"
print(f"\nEstimated score: {score}/100  [{status}]")
