#!/usr/bin/env python3
"""
Apply tracked changes (revisions) to Methods_1.docx (HCAP dementia
classification manuscript prepared for JAMA submission).

Every edit is stored as a w:del + w:ins pair so Word/LibreOffice shows
the change with the original author set to "Chen, Xi".

Usage:
    python scripts/gen_methods1_tracked.py
"""

import zipfile
import shutil
import copy
import os
import sys
import tempfile
from lxml import etree

sys.stdout.reconfigure(encoding='utf-8', errors='replace')

INPUT  = r'C:\Users\xc77\Dropbox\Claude\Methods_1.docx'
OUTPUT = r'C:\Users\xc77\Dropbox\Claude\Methods_1_tracked.docx'

W_NS   = 'http://schemas.openxmlformats.org/wordprocessingml/2006/main'
XML_NS = 'http://www.w3.org/XML/1998/namespace'
AUTHOR = 'Chen, Xi'
DATE   = '2026-05-25T00:00:00Z'

_cid = [300]
def nid():
    _cid[0] += 1
    return str(_cid[0])

def wn(tag):  return f'{{{W_NS}}}{tag}'
def xn(tag):  return f'{{{XML_NS}}}{tag}'

# ---------------------------------------------------------------------------
# XML helpers
# ---------------------------------------------------------------------------

def run_text(r):
    return ''.join(t.text or '' for t in r.findall(wn('t')))

def para_plain_text(para):
    """Text from direct w:r children only (ignores existing ins/del)."""
    return ''.join(run_text(c) for c in para if c.tag == wn('r'))

def make_t(text):
    t = etree.Element(wn('t'))
    t.text = text
    if text and (text[0] == ' ' or text[-1] == ' '):
        t.set(xn('space'), 'preserve')
    return t

def clone_run_text(src_run, text):
    r = copy.deepcopy(src_run)
    for t in r.findall(wn('t')):
        r.remove(t)
    if text:
        r.append(make_t(text))
    return r

def make_del(run_list):
    d = etree.Element(wn('del'))
    d.set(wn('id'),     nid())
    d.set(wn('author'), AUTHOR)
    d.set(wn('date'),   DATE)
    for run in run_list:
        dr = copy.deepcopy(run)
        for t in dr.findall(wn('t')):
            t.tag = wn('delText')
        d.append(dr)
    return d

def make_ins(text, template_run=None):
    i = etree.Element(wn('ins'))
    i.set(wn('id'),     nid())
    i.set(wn('author'), AUTHOR)
    i.set(wn('date'),   DATE)
    r = etree.Element(wn('r'))
    if template_run is not None:
        rpr = template_run.find(wn('rPr'))
        if rpr is not None:
            r.append(copy.deepcopy(rpr))
    r.append(make_t(text))
    i.append(r)
    return i

# ---------------------------------------------------------------------------
# Core replacement
# ---------------------------------------------------------------------------

def apply_change(para, old, new):
    """
    Find `old` across direct w:r children of `para`, replace with a
    tracked-change pair (w:del + w:ins).  Returns True if applied.
    """
    children = list(para)

    run_items = []
    pos = 0
    for ci, c in enumerate(children):
        if c.tag == wn('r'):
            t = run_text(c)
            run_items.append((ci, c, pos, pos + len(t)))
            pos += len(t)

    full = ''.join(run_text(c) for c in children if c.tag == wn('r'))
    idx = full.find(old)
    if idx == -1:
        return False
    end = idx + len(old)

    aff = [(ci, r, rs, re) for ci, r, rs, re in run_items
           if re > idx and rs < end]
    if not aff:
        return False

    f_ci, f_r, f_rs, _ = aff[0]
    l_ci, l_r, l_rs, l_re = aff[-1]

    repl = []

    if idx > f_rs:
        repl.append(clone_run_text(f_r, run_text(f_r)[: idx - f_rs]))

    del_runs = []
    for ci, r, rs, re in aff:
        s = max(idx, rs) - rs
        e = min(end, re) - rs
        del_runs.append(clone_run_text(r, run_text(r)[s:e]))
    repl.append(make_del(del_runs))

    if new:
        repl.append(make_ins(new, f_r))

    if end < l_re:
        repl.append(clone_run_text(l_r, run_text(l_r)[end - l_rs:]))

    for ci, r, rs, re in reversed(aff):
        para.remove(children[ci])
    for i, e in enumerate(repl):
        para.insert(f_ci + i, e)

    return True


# ---------------------------------------------------------------------------
# Changes list  (old_text, new_text, description)
# ---------------------------------------------------------------------------

CHANGES = [

    # ── Data & Study Participants ───────────────────────────────────────────

    (
        '4425 HRS participant were',
        '4,425 HRS participants were',
        'Fix: add thousands comma to "4425"; plural "participant" → "participants"'
    ),

    # ── Model Training and Evaluation ──────────────────────────────────────

    (
        'base learners i.e., logistic regression, gradient boosting, XGBoost, and random forest, using',
        'base learners (i.e., logistic regression, gradient boosting, XGBoost, and random forest) using',
        'Fix punctuation: "i.e.," should be parenthetical "(i.e., …)" with closing paren before "using"'
    ),
    (
        'accuracy (overall proportion correctly classified), sensitivity (true positive rate), specificity (true negative rate)',
        'accuracy (overall proportion correctly classified), sensitivity (true positive rate), and specificity (true negative rate)',
        'Fix punctuation: add "and" before final item in three-part series'
    ),
    (
        'for each model performance metrics using',
        'for each model performance metric using',
        'Fix grammar: "metrics" → "metric" (singular after "each")'
    ),

    # ── Algorithmic Fairness Analysis ──────────────────────────────────────

    (
        'we conducted a algorithmic fairness analysis',
        'we conducted an algorithmic fairness analysis',
        'Fix article: "a" → "an" before vowel sound'
    ),
    (
        'educational (high school or more [reference], less than high school)',
        'educational attainment (high school or more [reference], less than high school)',
        'Fix incomplete noun phrase: "educational" → "educational attainment"'
    ),
    (
        'Equalized Odds requires both the true positive rate and false positive rate are simultaneously equal',
        'Equalized Odds requires both the true positive rate and false positive rate to be simultaneously equal',
        'Fix grammar: infinitive required after "requires" ("are" → "to be")'
    ),
    (
        # First occurrence: followed by "Statistical tests" — targets para 38
        'All analyses were performed conducted in R, version 4.6.0. Statistical tests',
        'All analyses were conducted in R, version 4.6.0. Statistical tests',
        'Fix double verb: "were performed conducted" → "were conducted" (Statistical Analyses section)'
    ),

    # ── Sample Characteristics ─────────────────────────────────────────────

    (
        '2,482 (71.1%)  participants',
        '2,482 (71.1%) participants',
        'Fix formatting: remove extra space'
    ),

    # ── Equal Opportunity ──────────────────────────────────────────────────

    (
        'those algorithms may under detect true dementia cases',
        'those algorithms may underdetect true dementia cases',
        'Fix word form: "under detect" → "underdetect" (compound verb)'
    ),

    # ── Predictive Equality ────────────────────────────────────────────────

    (
        'the Super Leaner (',
        'the Super Learner (',
        'Fix typo: "Super Leaner" → "Super Learner"'
    ),

    # ── Equalized Odds ─────────────────────────────────────────────────────

    (
        'racial and ethical groups',
        'racial and ethnic groups',
        'Fix typo: "ethical" → "ethnic"'
    ),
    (
        'ranged from 0.27-0.40 (Figure 3B)., which',
        'ranged from 0.27-0.40 (Figure 3B), which',
        'Fix punctuation: remove stray period before comma'
    ),

    # ── Comparison Across Models ───────────────────────────────────────────

    (
        'there is no single algorithm achieved both',
        'there is no single algorithm that achieved both',
        'Fix grammar: add missing relative pronoun "that"'
    ),
    (
        'high accuracy and septicity',
        'high accuracy and specificity',
        'Fix typo: "septicity" → "specificity"'
    ),
    (
        'ethnic  and age-related',
        'ethnic and age-related',
        'Fix formatting: remove extra space'
    ),
    (
        'The super leaner achieved the highest',
        'The Super Learner achieved the highest',
        'Fix typo + capitalization: "super leaner" → "Super Learner"'
    ),
    (
        'sensitivity (0.90)  and achieved the most favorable',
        'sensitivity (0.90) and the most favorable',
        'Fix: remove extra space and redundant second "achieved"'
    ),
    (
        'specificity(0.84)',
        'specificity (0.84)',
        'Fix formatting: add missing space before parenthesis'
    ),

    # ── Conclusion note ────────────────────────────────────────────────────

    (
        'logistic/LASSSO regression',
        'logistic/LASSO regression',
        'Fix typo: "LASSSO" → "LASSO" (extra S)'
    ),
    (
        'the super leaner may be preferable',
        'the Super Learner may be preferable',
        'Fix typo + capitalization: "super leaner" → "Super Learner"'
    ),

    # ── eMethods (para 174) ────────────────────────────────────────────────

    (
        # Second occurrence: end of paragraph — targets para 174
        'All analyses were performed conducted in R, version 4.6.0.',
        'All analyses were conducted in R, version 4.6.0.',
        'Fix double verb: "were performed conducted" → "were conducted" (eMethods)'
    ),
]

# ---------------------------------------------------------------------------
# Manual-review notes printed to console (not automated)
# ---------------------------------------------------------------------------

REVIEW_NOTES = [
    (
        'Para 34 (Fairness Analysis): The phrase "(can be removed)" appears in the text: '
        '"The other race category was excluded from race/ethnicity analyses due to insufficient '
        'sample size for reliable subgroup metric estimation (can be removed)." '
        'This is an author note that must be deleted before submission.'
    ),
    (
        'Para 57 (Comparison Across Models): The sentence ends with "Recommend: logistic?" — '
        'this is an author note that must be removed before submission. '
        'Finalize the recommendation as a proper concluding sentence.'
    ),
    (
        'Para 60 (Conclusion draft): The entire paragraph beginning "This could be used for '
        'the conclusion :" is a placeholder note, not final manuscript text. '
        'Either integrate into the Discussion/Conclusion section or delete before submission.'
    ),
    (
        'Para 36 (Predictive Equality definition): "An algorithm would exhibit equal predictive '
        'equality if…" — "exhibit equal predictive equality" is redundant. '
        'Consider: "An algorithm satisfies predictive equality if…"'
    ),
    (
        'Para 38 (Fairness citation): "Consistent with prior algorithmic fairness literature12,14" '
        '— citation 12 is van der Laan et al. (Super Learner), not a fairness paper. '
        'Verify the citation numbers; the intended references are likely 14–16 (Sahin, Yuan, Vyas).'
    ),
    (
        'Para 22 (Predictor references): "Building on the existing HRS dementia algorithms,4–10" '
        '— reference 10 (Livingston et al. 2024) is a Lancet Commission review of risk factors, '
        'not a dementia classification algorithm. Verify whether ref 10 belongs in this citation range.'
    ),
    (
        'Para 31 (Comparator algorithms): "Model performance was compared to five existing HRS '
        'dementia algorithms validated in ADAMS-based cohorts.6,7" — only two references (6, 7) '
        'are cited for five algorithms. Confirm that refs 6 and 7 together describe all five '
        'comparators, or add the missing references.'
    ),
]


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    if not os.path.exists(INPUT):
        sys.exit(f'Input not found: {INPUT}')

    shutil.copy(INPUT, OUTPUT)

    with zipfile.ZipFile(OUTPUT, 'r') as z:
        doc_xml_bytes = z.read('word/document.xml')

    tree = etree.fromstring(doc_xml_bytes)

    applied, missing = [], []

    for old, new, desc in CHANGES:
        found = False
        for para in tree.iter(wn('p')):
            if old in para_plain_text(para):
                if apply_change(para, old, new):
                    found = True
                    break
        if found:
            applied.append(desc)
        else:
            missing.append((old[:80], desc))

    modified = etree.tostring(
        tree, xml_declaration=True, encoding='UTF-8', standalone=True
    )

    with tempfile.NamedTemporaryFile(suffix='.docx', delete=False) as tf:
        tmp = tf.name

    with zipfile.ZipFile(OUTPUT, 'r') as zin, \
         zipfile.ZipFile(tmp, 'w', zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            zout.writestr(item, modified if item.filename == 'word/document.xml'
                          else zin.read(item.filename))

    os.remove(OUTPUT)
    shutil.copy(tmp, OUTPUT)
    os.remove(tmp)

    print(f'\nApplied {len(applied)}/{len(CHANGES)} changes:\n')
    for a in applied:
        print(f'  [OK]  {a}')
    if missing:
        print(f'\nNot found ({len(missing)}):')
        for old_snip, desc in missing:
            print(f'  [MISS]  {desc}')
            print(f'          searched: "{old_snip}"')

    print(f'\n{"-" * 60}')
    print('MANUAL REVIEW REQUIRED (not automated):')
    print(f'{"-" * 60}')
    for note in REVIEW_NOTES:
        print(f'\n  [!]  {note}')

    print(f'\nOutput → {OUTPUT}')


if __name__ == '__main__':
    main()
