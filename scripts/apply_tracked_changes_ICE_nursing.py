#!/usr/bin/env python3
"""
Apply tracked changes (revisions) to ICE_Nursing_home_v6_2026-05-16.docx.
Every edit is stored as a w:del + w:ins pair so Word/LibreOffice shows the
change with the original author set to "Claude Code".

Usage:
    python scripts/apply_tracked_changes_ICE_nursing.py
"""

import zipfile
import shutil
import copy
import os
import sys
from lxml import etree

INPUT  = r'C:\Users\xc77\Dropbox\Claude\ICE_Nursing_home_v6_2026-05-16.docx'
OUTPUT = r'C:\Users\xc77\Dropbox\Claude\ICE_Nursing_home_v7_2026-05-16.docx'

W_NS   = 'http://schemas.openxmlformats.org/wordprocessingml/2006/main'
XML_NS = 'http://www.w3.org/XML/1998/namespace'
AUTHOR = 'Claude Code'
DATE   = '2026-05-16T00:00:00Z'

_cid = [200]
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
    """Deep-copy a run's formatting, replace its text content."""
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
    Find `old` across the direct w:r children of `para`, replace with
    a tracked-change pair (w:del + w:ins).  Returns True if applied.
    """
    children = list(para)

    # Map each run to its position in the concatenated plain text
    run_items = []   # (child_index, run_elem, start, end)
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

    # Affected runs: any run whose text range overlaps [idx, end)
    aff = [(ci, r, rs, re) for ci, r, rs, re in run_items
           if re > idx and rs < end]
    if not aff:
        return False

    f_ci, f_r, f_rs, _ = aff[0]
    l_ci, l_r, l_rs, l_re = aff[-1]

    repl = []

    # Prefix: part of the first affected run that lies before the change
    if idx > f_rs:
        repl.append(clone_run_text(f_r, run_text(f_r)[: idx - f_rs]))

    # Deleted content
    del_runs = []
    for ci, r, rs, re in aff:
        s = max(idx, rs) - rs
        e = min(end, re) - rs
        del_runs.append(clone_run_text(r, run_text(r)[s:e]))
    repl.append(make_del(del_runs))

    # Inserted content
    if new:
        repl.append(make_ins(new, f_r))

    # Suffix: part of the last affected run that lies after the change
    if end < l_re:
        repl.append(clone_run_text(l_r, run_text(l_r)[end - l_rs:]))

    # Splice into para: remove affected runs, insert replacement elements
    for ci, r, rs, re in reversed(aff):
        para.remove(children[ci])
    for i, e in enumerate(repl):
        para.insert(f_ci + i, e)

    return True


# ---------------------------------------------------------------------------
# Changes list  (old_text, new_text, description)
# ---------------------------------------------------------------------------

CHANGES = [
    # ── Critical factual / grammatical errors ─────────────────────────────

    (
        'January 2014 to December 2025',
        'January 2024 to December 2025',
        'Fix typo: 2014 → 2024 in abstract study period'
    ),
    (
        'a medically independent population',
        'a medically dependent population',
        "Fix: 'independent' → 'dependent' (residents depend on others for care)"
    ),
    (
        'Immigration and Custom Enforcement (ICE)',
        'Immigration and Customs Enforcement (ICE)',
        "Fix agency name: 'Custom' → 'Customs'"
    ),
    (
        'has empirical examined',
        'has empirically examined',
        "Fix adverb: 'empirical' → 'empirically'"
    ),
    (
        "). ).",
        ").",
        'Remove duplicate closing punctuation'
    ),
    (
        'Asian and Yang',
        'Alsan and Yang',
        "Fix typo: 'Asian' → 'Alsan'"
    ),

    # ── Percentage inconsistency (IRR 1.022 = 2.2 %, not 2.1 %) ──────────

    (
        'a 2.1 percent higher total health deficiency citation rate',
        'a 2.2 percent higher total health deficiency citation rate',
        'Fix: 2.1% → 2.2% in abstract (IRR = 1.022 → 2.2% increase)'
    ),
    (
        'a 2.1% increase in total health deficiency citations',
        'a 2.2% increase in total health deficiency citations',
        'Fix: 2.1% → 2.2% in Key Points (consistent with body text and IRR)'
    ),

    # ── Study design label ────────────────────────────────────────────────

    (
        'In a nationwide cohort study of 19,224 facility-month observations',
        'In a nationwide repeated cross-sectional study of 19,224 facility-month observations',
        "Fix design label in Key Points: 'cohort' → 'repeated cross-sectional'"
    ),
    (
        'This nationwide cohort study comprises 19,224 facility-month observations',
        'This nationwide repeated cross-sectional study comprises 19,224 facility-month observations',
        "Fix design label in abstract Design section: 'cohort' → 'repeated cross-sectional'"
    ),

    # ── Figure 2 panel references (off by one) ────────────────────────────

    (
        'for street arrests compared with custodial arrests across all deficiency domains (Panel A)',
        'for street arrests compared with custodial arrests across all deficiency domains (Panel B)',
        'Fix panel ref: A → B (street vs. custodial = Figure 2 Panel B)'
    ),
    (
        'A similar pattern emerged when arrests were stratified by criminal history (Panel B).',
        'A similar pattern emerged when arrests were stratified by criminal history (Panel C).',
        'Fix panel ref: B → C (criminal vs. non-criminal = Figure 2 Panel C)'
    ),
    (
        'associations were driven by arrests of Hispanic individuals (Panel C).',
        'associations were driven by arrests of Hispanic individuals (Panel D).',
        'Fix panel ref: C → D (Hispanic arrests = Figure 2 Panel D)'
    ),

    # ── Facility fixed effects (logic error + extra "a") ──────────────────

    (
        'Facility fixed effects are feasible for this study as a more than a third',
        'Facility fixed effects are not feasible for this study as more than a third',
        "Fix limitation: add 'not'; remove spurious 'a' before 'more than'"
    ),

    # ── Word choice ───────────────────────────────────────────────────────

    (
        'The study also renders timely policy insights',
        'The study also provides timely policy insights',
        "Fix: 'renders' → 'provides'"
    ),
    (
        'the unexpected spillovers of immigration enforcement escalation around the second presidential term of the Trump administration to long-term care sector.',
        'the unintended spillovers of immigration enforcement escalation during the second Trump administration onto the long-term care sector.',
        "Fix: 'unexpected' → 'unintended'; fix prepositions (around/to → during/onto)"
    ),
    (
        'Yet no study has investigated these unexpected consequences of the most recent enforcement escalation.',
        'Yet no study has investigated these unintended consequences of the most recent enforcement escalation.',
        "Fix: 'unexpected' → 'unintended' (consistent with Key Points)"
    ),

    # ── Punctuation / formatting ──────────────────────────────────────────

    (
        '(i.e, total deficiencies:',
        '(i.e., total deficiencies:',
        "Fix punctuation: 'i.e,' → 'i.e.,' (missing period)"
    ),
    (
        '95% CI—1.014-1.029; P < .001)',
        '95% CI, 1.014-1.029; P < .001)',
        'Fix CI format in abstract: em dash → comma (JAMA style)'
    ),
    (
        '1.015–1.046; P =<.001)',
        '1.015–1.046; P < .001)',
        "Fix p-value format in abstract: 'P =<' → 'P <'"
    ),
]


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    if not os.path.exists(INPUT):
        sys.exit(f'Input not found: {INPUT}')

    shutil.copy(INPUT, OUTPUT)

    # Read document.xml from the docx zip
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
            missing.append((old[:70], desc))

    # Serialize
    modified = etree.tostring(
        tree, xml_declaration=True, encoding='UTF-8', standalone=True
    )

    # Rewrite the zip into a local temp file, then copy to final destination
    import tempfile
    with tempfile.NamedTemporaryFile(suffix='.docx', delete=False) as tf:
        tmp = tf.name

    with zipfile.ZipFile(OUTPUT, 'r') as zin, \
         zipfile.ZipFile(tmp, 'w', zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            zout.writestr(item, modified if item.filename == 'word/document.xml'
                          else zin.read(item.filename))

    # Remove the intermediate copy (shutil.copy created it earlier)
    os.remove(OUTPUT)
    shutil.copy(tmp, OUTPUT)
    os.remove(tmp)

    print(f'\nApplied {len(applied)}/{len(CHANGES)} changes:\n')
    for a in applied:
        print(f'  ✓  {a}')
    if missing:
        print(f'\nNot found ({len(missing)}):')
        for old_snip, desc in missing:
            print(f'  ✗  {desc}')
            print(f'       searched: "{old_snip}"')
    print(f'\nOutput → {OUTPUT}')


if __name__ == '__main__':
    main()
