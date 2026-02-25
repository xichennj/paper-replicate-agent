# Project Memory

Corrections and learned facts that persist across sessions.
When a mistake is corrected, append a `[LEARN:category]` entry below.

---

<!-- Append new entries below. Most recent at bottom. -->

## Workflow Patterns

[LEARN:workflow] Requirements specification phase catches ambiguity before planning → reduces rework 30-50%. Use spec-then-plan for complex/ambiguous tasks (>1 hour or >3 files).

[LEARN:workflow] Spec-then-plan protocol: AskUserQuestion (3-5 questions) → create `quality_reports/specs/YYYY-MM-DD_description.md` with MUST/SHOULD/MAY requirements → declare clarity status (CLEAR/ASSUMED/BLOCKED) → get approval → then draft plan.

[LEARN:workflow] Context survival before compression: (1) Update MEMORY.md with [LEARN] entries, (2) Ensure session log current (last 10 min), (3) Active plan saved to disk, (4) Open questions documented. The pre-compact hook displays checklist.

[LEARN:workflow] Plans, specs, and session logs must live on disk (not just in conversation) to survive compression and session boundaries. Quality reports only at merge time.

## Documentation Standards

[LEARN:documentation] When adding new features, update BOTH README and guide immediately to prevent documentation drift. Stale docs break user trust.

[LEARN:documentation] Always document new templates in README's "What's Included" section with purpose description. Template inventory must be complete and accurate.

[LEARN:documentation] Guide must be generic (framework-oriented) not prescriptive. Provide templates with examples for multiple workflows (LaTeX, R, Python, Jupyter), let users customize. No "thou shalt" rules.

[LEARN:documentation] Date fields in frontmatter and README must reflect latest significant changes. Users check dates to assess currency.

## Design Philosophy

[LEARN:design] Framework-oriented > Prescriptive rules. Constitutional governance works as a TEMPLATE with examples users customize to their domain. Same for requirements specs.

[LEARN:design] Quality standard for guide additions: useful + pedagogically strong + drives usage + leaves great impression + improves upon starting fresh + no redundancy + not slow. All 7 criteria must hold.

[LEARN:design] Generic means working for any academic workflow: pure LaTeX (no Quarto), pure R (no LaTeX), Python/Jupyter, any domain (not just econometrics). Test recommendations across use cases.

## File Organization

[LEARN:files] Specifications go in `quality_reports/specs/YYYY-MM-DD_description.md`, not scattered in root or other directories. Maintains structure.

[LEARN:files] Templates belong in `templates/` directory with descriptive names. Currently have: session-log.md, quality-report.md, exploration-readme.md, archive-readme.md, requirements-spec.md, constitutional-governance.md.

## Constitutional Governance

[LEARN:governance] Constitutional articles distinguish immutable principles (non-negotiable for quality/reproducibility) from flexible user preferences. Keep to 3-7 articles max.

[LEARN:governance] Example articles: Primary Artifact (which file is authoritative), Plan-First Threshold (when to plan), Quality Gate (minimum score), Verification Standard (what must pass), File Organization (where files live).

[LEARN:governance] Amendment process: Ask user if deviating from article is "amending Article X (permanent)" or "overriding for this task (one-time exception)". Preserves institutional memory.

## Skill Creation

[LEARN:skills] Effective skill descriptions use trigger phrases users actually say: "check citations", "format results", "validate protocol" → Claude knows when to load skill.

[LEARN:skills] Skills need 3 sections minimum: Instructions (step-by-step), Examples (concrete scenarios), Troubleshooting (common errors) → users can debug independently.

[LEARN:skills] Domain-specific examples beat generic ones: citation checker (psychology), protocol validator (biology), regression formatter (economics) → shows adaptability.

## Memory System

[LEARN:memory] Two-tier memory solves template vs working project tension: MEMORY.md (generic patterns, committed), personal-memory.md (machine-specific, gitignored) → cross-machine sync + local privacy.

[LEARN:memory] Post-merge hooks prompt reflection, don't auto-append → user maintains control while building habit.

## Meta-Governance

[LEARN:meta] Repository dual nature requires explicit governance: what's generic (commit) vs specific (gitignore) → prevents template pollution.

[LEARN:meta] Dogfooding principles must be enforced: plan-first, spec-then-plan, quality gates, session logs → we follow our own guide.

[LEARN:meta] Template development work (building infrastructure, docs) doesn't create session logs in quality_reports/ → those are for user work (slides, analysis), not meta-work. Keeps template clean for users who fork.

## R Coding Patterns

[LEARN:r-coding] `coeftest()` returns a matrix with class attributes, NOT a data frame. Never use `as.data.frame(coeftest(...))` — rownames are lost. Always index directly: `ct[idx, "Estimate"]`, `ct[idx, "Std. Error"]`, etc. where `idx <- which(rownames(ct) == term_name)`.

[LEARN:r-coding] ggplot2 named color palette collision: `c("Label" = PAL["name"])` preserves the inner vector name, causing "No shared levels found" warnings. Fix: always `unname(PAL["name"])` before wrapping in `scale_color_manual(values = c("Label" = unname(PAL["name"])))`.

[LEARN:r-coding] Event-study HC1 rank deficiency (silent 2022 drop): when high-NA factor covariates (`edu_f`, `bmi_cat`) cause listwise deletion, `vcovHC(type="HC1")` silently drops the last interaction term (latest year). `lm()` shows rank 30/30 and 0 aliased, yet `coeftest()` is missing the year. Fix: use a separate `event_covs` string without those high-NA factors for `event_formula()` only; keep full `base_covs` for DiD formula.

[LEARN:r-coding] R on Windows via bash (Claude Code): use `powershell.exe -Command "& 'C:\Program Files\R\R-4.5.2\bin\Rscript.exe' 'script.R'"`. Always prepend user library at top of every R script: `.libPaths(c(file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.5"), .libPaths()))`.

## Replication Methodology

[LEARN:replication] N over-count in DiD: first check whether the enrollment/insurance variable has a stricter continuous/full-year variant (e.g., `ins_ma` vs `ins_ma2`). The stricter variant often matches the paper's "full-year enrollees" definition and can close a 10–15% N gap.

[LEARN:replication] Residual N gaps after exhaustive filter testing (usually ~3%) most likely reflect an unpublished data-quality exclusion in the research-grade file (e.g., withdrawal list) not available in the PUF. Document this as unavoidable; verify that ADRD share and DiD direction are preserved.

[LEARN:replication] When testing N-filter combinations for MCBS-style DiD studies, systematically audit: MA variant × years-enrolled threshold × dual-eligibility × FFS exclusion × community-dwelling — in that order. Use a `build_sample()` helper that accepts the MA variable as a parameter.

[LEARN:replication] Working-paper eTable numbers can shift in the published version. Fu et al.: WP "eTable 1" (event study) became "eTable 3" in GLO DP No. 1716; two new eTables (raw means by year, pre/post summary) were added as eTables 1 & 2. Always cross-check supplement table numbers against the final published version before finalising replication targets.

[LEARN:replication] Fu et al. (2024) published as GLO Discussion Paper No. 1716 (2026). All core DiD and event-study numbers confirmed identical to working paper draft → replication score 82/100 validated against final publication. Official package: Stata 19.5 + `reghdfe`, `Dropbox/replication_pkg`; MA filter confirmed as `ins_ma2 == 1` (not `ins_ma`).

[LEARN:replication] Fu et al. code quality score: 100/100 after replacing hardcoded `DATA_PATH <- "C:/..."` with `here("data", ...)`. Single hardcoded path was the only rubric deduction; fix took one line. Always use `here()` for all data/output paths from the start.
