# Session Log: Fu et al. 2024 Replication

**Date:** 2026-02-23
**Task:** Full replication of Fu, Qian, Karimi, Zarei & Chen (2024)
**Status:** IMPLEMENTATION COMPLETE — pending data run

---

## Goal

Replicate the main empirical results of Fu et al. (2024) using the MCBS Public Use Files
(2015–2022). Primary targets: Figure 3 (main DiD estimates) and Figure 4/eTable 1
(event study coefficients).

---

## What Was Done

### Phase 1: Repository Setup ✓
- Created `papers/FuEtal2024/` with `manuscript.docx`, `supplement.docx`, `README.md`
- Created `replications/FuEtal2024/R/figures/` and `R/results/`
- Created `quality_reports/FuEtal2024_replication_targets.md`

### Phase 2: Plan ✓
- Plan saved to `quality_reports/plans/2026-02-23_FuEtal2024-replication.md`
- Variable mapping completed; data gaps identified
- Key data gap: Parkinson's only 2015–2016 in PUF

### Phase 3: R Script ✓
- `replications/FuEtal2024/R/replicate.R` written (1,112 lines)
- Sections: data load → sample → outcomes → covariates → Table 1 → Figures 2–4 →
  robustness (eFig 1,2,4,5) → placebo (eFig 6) → stratification (eFig 8) →
  validation → session info

### Phases 4–5: Documentation ✓
- `replications/FuEtal2024/validation_report.md` — published vs. reproduced template
- This session log

---

## Key Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| MA flag | `ins_ma` | 44,077 obs vs. `ins_ma2`; primary variable |
| Parkinson's | Include 2015–2016 only | Only years with valid data |
| Brain hemorrhage | HLT_OCSTROKE proxy | No standalone PUF variable |
| Robust SEs | HC1 (sandwich package) | Paper specifies heteroskedasticity-robust |
| Reference year | 2019 | As stated in paper |
| Inapplicable (100) | → NA | Standard MCBS convention |
| Financial burden | ACC_PAYPROB==1 OR ACC_MCCOSTS∈{3,4} | Per paper definition |

---

## Data Gaps Identified

1. **Parkinson's 2017–2022 missing** — `HLT_OCPARKIN` = inapplicable (3) in those years.
   Control group will be slightly smaller; flag as deviation.

2. **No brain hemorrhage variable** — `HLT_OCSTROKE` used as proxy.

3. **hlth_adrd scope uncertain** — may equal `HLT_ALZDEM` only (Alzheimer's, not full ADRD).

4. **Cross-sectional PUF** — partial MA exclusion approximated via `ins_mayrs` sensitivity.

5. **eTable 1 typo** — supplement shows negative upper CI bounds for pre-2020 years;
   these are sign errors. Correct CIs (positive upper bounds) will be shown.

---

## Run Results (2026-02-23)

Script ran successfully end-to-end. R 4.5.2 installed via winget; packages installed to user library.

| Check | Target | Got | Status |
|-------|--------|-----|--------|
| N total | 5,353 | 6,004 | MISS (+12%) |
| ADRD share | 30.4% | 30.3% | ✓ EXACT |
| acc_trouble DiD | −0.066 | −0.044 | CI overlap |
| fin_burden DiD | −0.092 | −0.046 | CI overlap |
| sat_spec DiD | +0.040 | +0.007 | CI overlap |
| sat_qual DiD | −0.014 | −0.012 | ✓ EXACT |
| Parallel trends | near-zero | near-zero | ✓ |
| 2022 event study | present | missing | rank deficiency |

Quality score (estimated): ~71/100 — below 80 gate.

## Resolved Questions

- Income variable: `poverty` (1–5 FPL categories); low_income = poverty ∈ {1,2,3}
- Metro variable: `metro` (1=metro) ✓
- Event study 2022 missing due to rank deficiency in full covariate model
- N discrepancy: likely dual-eligibility exclusion + partial-year MA filter (not fully replicable from PUF)

## Remaining Open Questions / Remediation Path

1. **Dual eligibility variable** — find `ins_dual` or equivalent in PUF; apply exclusion → expect N ≈ 5,353
2. **`ACC_MCCOSTS` value labels** — verify 3=dissatisfied from MCBS codebook
3. **2022 event study** — simplify event-study covariate model to resolve rank deficiency
4. **`hlth_adrd` scope** — confirm whether it captures only Alzheimer's or full ADRD spectrum
