# Replication Targets: Fu, Qian, Karimi, Zarei & Chen (2024)

**Paper:** "Risk Adjustment for ADRD in Medicare Advantage and Health Care Experiences"
**Authors:** Fu, Qian, Karimi, Zarei & Chen
**Date recorded:** 2026-02-23
**Data:** MCBS Public Use Files 2015–2022 (`data/MCBS_PUF_all_years.dta`)

---

## Sample Targets (from paper Methods / Table 1)

| Target | Value | Tolerance | Source |
|--------|-------|-----------|--------|
| Final analytic N (MA sample) | 5,353 | ±50 | Paper Table 1 |
| ADRD (treatment group) | 1,629 (30.4%) | ±50 / ±0.5pp | Paper Table 1 |
| Neuro control group | 3,724 (69.6%) | ±50 / ±0.5pp | Paper Table 1 |
| Survey years | 2015–2022 | exact | Paper Methods |
| Data structure | Cross-sectional | — | Paper Methods |

---

## Table 1: Sample Characteristics (MA sample, N=5,353)

| Characteristic | ADRD Treatment | Neuro Control | Notes |
|----------------|---------------|---------------|-------|
| Age < 65 | ~18.4% | — | Paper Table 1 |
| Age 65–74 | ~23.2% | — | Paper Table 1 |
| Age 75+ | ~58.4% | — | Paper Table 1 |
| Male | ~33% | — | Paper Table 1 |
| Non-Hispanic White | ~61.7% | — | Paper Table 1 (race cat 1) |
| Black | ~15.7% | — | Paper Table 1 (race cat 2) |
| Hispanic | ~16.6% | — | Paper Table 1 (race cat 4) |
| Married with spouse | ~33% | — | Paper Table 1 |
| ≤ High school education | >50% | — | Paper Table 1 |
| ≥1 functional limitation | ~73% | — | Paper Table 1 |
| >2 chronic conditions | ~88% | — | Paper Table 1 |
| Any trouble accessing care | ~10% | ~6% | Paper Table 1 |
| Any financial burden | ~23% | ~15% | Paper Table 1 |
| Satisfied with specialists | >90% | >90% | Paper Table 1 |
| Satisfied with quality | >90% | >90% | Paper Table 1 |

---

## Figure 3: Main DiD Estimates (Treatment × Post coefficient)

**Model:** LPM, HC1 robust SEs
**Treatment:** MA enrollees with ADRD
**Control:** MA enrollees with stroke/paralysis/Parkinson's (no ADRD)
**Post:** year ≥ 2020

| Outcome | DiD Coefficient | 95% CI | P-value | Tolerance |
|---------|----------------|--------|---------|-----------|
| Any trouble accessing care | **−0.066** | [−0.112, −0.020] | 0.005 | ±0.01 |
| Any medical financial burden | **−0.092** | [−0.161, −0.023] | 0.009 | ±0.01 |
| Satisfaction with specialists | **+0.040** | [−0.005, +0.084] | 0.083 | ±0.01 |
| Satisfaction with quality of care | **−0.014** | [−0.056, +0.029] | 0.530 | ±0.01 (null) |

---

## Figure 4 / eTable 1: Event Study Coefficients (reference year = 2019)

### Outcome 1: Any Trouble Accessing Care (N=5,339)

| Year | Coefficient | SE | P-value | 95% CI |
|------|-------------|-----|---------|--------|
| 2015 | −0.0446 | 0.0448 | 0.320 | [−0.133, +0.043] |
| 2016 | −0.0156 | 0.0421 | 0.710 | [−0.098, +0.067] |
| 2017 | +0.0029 | 0.0460 | 0.949 | [−0.087, +0.093] |
| 2018 | −0.0221 | 0.0438 | 0.615 | [−0.108, +0.064] |
| 2019 | *(reference)* | | | |
| 2020 | −0.0481 | 0.0411 | 0.242 | [−0.129, +0.033] |
| 2021 | **−0.0989** | 0.0421 | **0.019** | [−0.181, −0.016] |
| 2022 | −0.0909 | 0.0471 | 0.054 | [−0.183, +0.001] |

*Note: eTable 1 in supplement contains typo — negative upper CI bounds for pre-2020 years.
Correct upper bounds should be positive (e.g., 2015: +0.043, not −0.0433). These values
are the correct targets.*

### Outcome 2: Medical Financial Burden (N=4,172; 2017+ only)

| Year | Coefficient | SE | P-value | 95% CI |
|------|-------------|-----|---------|--------|
| 2017 | −0.0639 | 0.0636 | 0.315 | [−0.189, +0.061] |
| 2018 | +0.0193 | 0.0624 | 0.757 | [−0.103, +0.142] |
| 2019 | *(reference)* | | | |
| 2020 | −0.0974 | 0.0584 | 0.095 | [−0.212, +0.017] |
| 2021 | −0.1034 | 0.0565 | 0.067 | [−0.214, +0.007] |
| 2022 | **−0.1174** | 0.0609 | **0.054** | [−0.237, +0.002] |

### Outcome 3: Satisfaction with Specialist Access (N=5,049)

| Year | Coefficient | SE | P-value | 95% CI |
|------|-------------|-----|---------|--------|
| 2015 | −0.0301 | 0.0482 | 0.532 | [−0.125, +0.064] |
| 2016 | −0.0012 | 0.0446 | 0.978 | [−0.089, +0.086] |
| 2017 | +0.0094 | 0.0446 | 0.832 | [−0.078, +0.097] |
| 2018 | +0.0261 | 0.0411 | 0.526 | [−0.055, +0.107] |
| 2019 | *(reference)* | | | |
| 2020 | +0.0119 | 0.0410 | 0.771 | [−0.068, +0.092] |
| 2021 | +0.0637 | 0.0419 | 0.128 | [−0.018, +0.146] |
| 2022 | +0.0526 | 0.0462 | 0.255 | [−0.038, +0.143] |

### Outcome 4: Satisfaction with Quality of Care (N=5,280)

| Year | Coefficient | SE | P-value | 95% CI |
|------|-------------|-----|---------|--------|
| 2015 | −0.0346 | 0.0374 | 0.355 | [−0.108, +0.039] |
| 2016 | −0.0039 | 0.0354 | 0.912 | [−0.073, +0.066] |
| 2017 | −0.0281 | 0.0394 | 0.477 | [−0.105, +0.049] |
| 2018 | −0.0014 | 0.0344 | 0.968 | [−0.069, +0.066] |
| 2019 | *(reference)* | | | |
| 2020 | −0.0351 | 0.0382 | 0.359 | [−0.110, +0.040] |
| 2021 | −0.0295 | 0.0380 | 0.437 | [−0.104, +0.045] |
| 2022 | −0.0103 | 0.0430 | 0.810 | [−0.095, +0.074] |

---

## eFigure 8: Stratification Results (selected)

| Subgroup | Outcome | DiD Coeff | 95% CI |
|----------|---------|-----------|--------|
| Income > 200% FPL | Trouble accessing care | −0.096 | [−0.190, −0.001] |
| Income ≤ 200% FPL | Trouble accessing care | −0.084 | [−0.148, −0.021] |
| Income ≤ 200% FPL | Financial burden | −0.131 | [−0.219, −0.044] |
| Non-White | Financial burden | −0.142 | [−0.259, −0.024] |
| White | Trouble accessing care | −0.056 | [−0.114, +0.002] |
| ≥ High school | Trouble accessing care | −0.103 | [−0.195, −0.011] |
| ≥ High school | Financial burden | −0.138 | [−0.279, +0.003] |
| Metro | Trouble accessing care | −0.065 | [−0.114, −0.016] |
| Metro | Satisfaction with specialists | +0.060 | [+0.012, +0.109] |

---

## Known Deviations from Paper

| Deviation | Reason | Expected Impact on Estimates |
|-----------|--------|------------------------------|
| Parkinson's (HLT_OCPARKIN) only available 2015–2016 | PUF data gap — coded inapplicable (value=3) in 2017–2022 | Control group 2017–2022 contains only stroke+paralysis; slightly different composition. N may be ~100–200 lower. |
| Brain hemorrhage not separately identified | No standalone PUF variable; HLT_OCSTROKE used | HLT_OCSTROKE likely includes hemorrhagic stroke; minimal impact |
| ADRD = `hlth_adrd` (may be Alzheimer's only) | Pending confirmation vs. full ADRD spectrum | Treatment group may be slightly smaller if related dementias excluded |
| Partial-year MA exclusion approximated | Cross-sectional PUF; cannot link to prior year | Small bias; sensitivity test (eFig2: ins_mayrs≥3) shows magnitude |
| eTable 1 typo: negative upper CI for pre-2020 | Supplement sign error; we report correct CIs | No impact on point estimates |
| Proxy × covariates full interaction | Computationally intensive; main DiD uses year FE directly | Minimal impact on DiD coefficient; SE may differ slightly |

---

## Verification Checklist

- [ ] Script runs end-to-end without errors
- [ ] N = 5,353 ± 50 (allowance for Parkinson's data gap)
- [ ] ADRD share = 30.4% ± 0.5pp
- [ ] Table 1 demographic %s within ±1pp of paper
- [ ] Figure 3, acc_trouble: DiD = −0.066 ± 0.01; CI [−0.112, −0.020]
- [ ] Figure 3, fin_burden:  DiD = −0.092 ± 0.01; CI [−0.161, −0.023]
- [ ] Figure 3, sat_spec:    DiD = +0.040 ± 0.01 (marginal, P≈0.083)
- [ ] Figure 3, sat_qual:    DiD ≈ −0.014, P > 0.05 (null result)
- [ ] Event study pre-2020 coefficients near zero (parallel trends satisfied)
- [ ] Event study 2021 acc_trouble ≈ −0.099 (p=0.019, significant)
- [ ] Quality score ≥ 80/100
