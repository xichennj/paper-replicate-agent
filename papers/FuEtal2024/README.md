# Fu, Qian, Karimi, Zarei & Chen (2024)

**Title:** Risk Adjustment for ADRD in Medicare Advantage and Health Care Experiences

**Journal:** TBD (preprint / working paper as of 2026-02-23)

**Files:**
- `manuscript.docx` — Main manuscript
- `supplement.docx` — Supplementary materials (eMethods, eFigures, eTables)

---

## Paper Summary

The paper exploits the reinstatement of the Alzheimer's Disease and Related Dementias (ADRD)
Hierarchical Condition Category (HCC) into the Medicare Advantage (MA) risk-adjusted payment
formula in 2020 as a quasi-natural experiment.

**Design:** Difference-in-Differences (DiD)
**Sample:** MCBS Public Use Files, 2015–2022; MA enrollees only
**Treatment:** MA enrollees with ADRD (n≈1,629)
**Control:** MA enrollees with stroke, paralysis, or Parkinson's disease, no ADRD (n≈3,724)
**Final N:** 5,353 person-years (cross-sectional, NOT panel)

**Key findings (Figure 3 main DiD):**
- Trouble accessing care: −6.6 pp (p=0.005)
- Medical financial burden: −9.2 pp (p=0.009)
- Satisfaction with specialists: +4.0 pp (p=0.083, marginal)
- Satisfaction with quality of care: −1.4 pp (p=0.530, null)

---

## Data

- **Source:** Medicare Current Beneficiary Survey (MCBS) Public Use Files
- **Years:** 2015–2022
- **File:** `data/MCBS_PUF_all_years.dta` (gitignored — not in repo)
- **N (full PUF):** 105,318 person-years
- **N (analytic MA sample):** 5,353

---

## Key Variable Notes

| Concept | MCBS Variable | Notes |
|---------|--------------|-------|
| MA enrollment | `ins_ma` | 44,077 MA observations in full PUF |
| ADRD | `hlth_adrd` | = `HLT_ALZDEM`; captures Alzheimer's (may miss related dementias) |
| Stroke/brain hemorrhage | `HLT_OCSTROKE` | No separate hemorrhage variable in PUF |
| Paralysis | `HLT_OCPPARAL` | Combined complete + partial |
| Parkinson's | `HLT_OCPARKIN` | **CRITICAL: only available 2015–2016; missing 2017–2022** |
| Veterans (excluded) | `vet` | vet=1 = served in armed forces |

---

## Known Data Gaps (Deviations from Paper)

1. **Parkinson's 2017–2022:** `HLT_OCPARKIN` is inapplicable (value=3) in 2017–2022.
   Control group in those years contains only stroke/paralysis patients.
2. **Brain hemorrhage:** No standalone variable; `HLT_OCSTROKE` used as proxy.
3. **ADRD scope:** `hlth_adrd` may equal Alzheimer's only (vs. full ADRD spectrum).
4. **Cross-sectional PUF:** Cannot link enrollment across years; partial MA exclusion approximated.

---

## Replication

See `replications/FuEtal2024/` for the full replication script and outputs.
See `quality_reports/FuEtal2024_replication_targets.md` for all numeric targets.
