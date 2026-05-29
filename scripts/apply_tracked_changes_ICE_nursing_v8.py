#!/usr/bin/env python3
"""
Apply a second round of tracked changes + reviewer comments to the JAMA
ICE nursing-home manuscript (v7 clean -> v8).

Round 2 scope (confirmed with author 2026-05-29):
  * Section A: 30 clear copyedits (grammar, subject-verb, articles,
    terminology/domain-name consistency, factual term fixes).
  * Section B: JAMA house style -- spelled-number -> numeral conversions and
    standardization of confidence-interval dashes (en-dash -> hyphen).
  * Section C: 7 Word COMMENTS flagging substantive inconsistencies that need
    author judgment (state count, 10-vs-11 subcategories, environmental vs
    pharmacy null result, leftover JAMA checklist placeholder, prime-age 24).

Every text edit is a tracked w:del + w:ins pair (author "Chen, Xi").
The 7 existing coauthor comments (ids 0-6) are preserved; new comments use
ids 7-13. Other docx parts are copied through unchanged.

Usage:
    python scripts/apply_tracked_changes_ICE_nursing_v8.py
"""

import os
import re
import sys
import copy
import shutil
import zipfile
import tempfile
from lxml import etree

INPUT  = r'C:\Users\xc77\Dropbox\Claude\ICE_Nursing_home_v7_2026-05-28_clean.docx'
OUTPUT = r'C:\Users\xc77\Dropbox\Claude\ICE_Nursing_home_v8_2026-05-29.docx'

W_NS   = 'http://schemas.openxmlformats.org/wordprocessingml/2006/main'
XML_NS = 'http://www.w3.org/XML/1998/namespace'
AUTHOR = 'Chen, Xi'
INITS  = 'CX'
DATE   = '2026-05-29T00:00:00Z'

_cid = [500]
def nid():
    _cid[0] += 1
    return str(_cid[0])

def wn(tag):  return f'{{{W_NS}}}{tag}'
def xn(tag):  return f'{{{XML_NS}}}{tag}'

# ---------------------------------------------------------------------------
# XML helpers (tracked-change pattern, same as the v6->v7 script)
# ---------------------------------------------------------------------------

def run_text(r):
    return ''.join(t.text or '' for t in r.findall(wn('t')))

def para_plain_text(para):
    """Text from direct w:r children only (ignores existing ins/del/comments)."""
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
    d.set(wn('id'), nid()); d.set(wn('author'), AUTHOR); d.set(wn('date'), DATE)
    for run in run_list:
        dr = copy.deepcopy(run)
        for t in dr.findall(wn('t')):
            t.tag = wn('delText')
        d.append(dr)
    return d

def make_ins(text, template_run=None):
    i = etree.Element(wn('ins'))
    i.set(wn('id'), nid()); i.set(wn('author'), AUTHOR); i.set(wn('date'), DATE)
    r = etree.Element(wn('r'))
    if template_run is not None:
        rpr = template_run.find(wn('rPr'))
        if rpr is not None:
            r.append(copy.deepcopy(rpr))
    r.append(make_t(text))
    i.append(r)
    return i

def _run_map(para):
    """Return (children, run_items, full_text) over direct w:r children only."""
    children = list(para)
    run_items, pos = [], 0
    for ci, c in enumerate(children):
        if c.tag == wn('r'):
            t = run_text(c)
            run_items.append((ci, c, pos, pos + len(t)))
            pos += len(t)
    full = ''.join(run_text(c) for c in children if c.tag == wn('r'))
    return children, run_items, full

def apply_change(para, old, new):
    """Replace `old` (across direct runs) with tracked w:del + w:ins."""
    children, run_items, full = _run_map(para)
    idx = full.find(old)
    if idx == -1:
        return False
    end = idx + len(old)
    aff = [(ci, r, rs, re) for ci, r, rs, re in run_items if re > idx and rs < end]
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
# Comment anchoring (in document.xml)
# ---------------------------------------------------------------------------

def add_comment_anchor(para, anchor, cid):
    """Wrap `anchor` text with commentRangeStart/End + a commentReference run."""
    children, run_items, full = _run_map(para)
    idx = full.find(anchor)
    if idx == -1:
        return False
    end = idx + len(anchor)
    aff = [(ci, r, rs, re) for ci, r, rs, re in run_items if re > idx and rs < end]
    if not aff:
        return False
    f_ci, f_r, f_rs, _ = aff[0]
    l_ci, l_r, l_rs, l_re = aff[-1]

    repl = []
    if idx > f_rs:
        repl.append(clone_run_text(f_r, run_text(f_r)[: idx - f_rs]))

    crs = etree.Element(wn('commentRangeStart')); crs.set(wn('id'), str(cid))
    repl.append(crs)
    for ci, r, rs, re in aff:
        s = max(idx, rs) - rs
        e = min(end, re) - rs
        repl.append(clone_run_text(r, run_text(r)[s:e]))
    cre = etree.Element(wn('commentRangeEnd')); cre.set(wn('id'), str(cid))
    repl.append(cre)

    rr = etree.Element(wn('r'))
    rpr = etree.Element(wn('rPr'))
    rstyle = etree.Element(wn('rStyle')); rstyle.set(wn('val'), 'CommentReference')
    rpr.append(rstyle); rr.append(rpr)
    cref = etree.Element(wn('commentReference')); cref.set(wn('id'), str(cid))
    rr.append(cref)
    repl.append(rr)

    if end < l_re:
        repl.append(clone_run_text(l_r, run_text(l_r)[end - l_rs:]))

    for ci, r, rs, re in reversed(aff):
        para.remove(children[ci])
    for i, e in enumerate(repl):
        para.insert(f_ci + i, e)
    return True

# ---------------------------------------------------------------------------
# Section A: clear copyedits  (old, new, description)
# ---------------------------------------------------------------------------

CHANGES = [
    # Abstract
    ('95% CI,1.012-1.026', '95% CI, 1.012-1.026',
     'Abstract Results: add space after "95% CI,"'),
    ('2.1 percent in urban than in rural facilities',
     '2.1 percent larger in urban than in rural facilities',
     'Abstract Results: add missing "larger"'),
    ('arrests, are associated with more deficiency citations in nursing homes',
     'arrests, is associated with more deficiency citations in nursing homes',
     'Abstract Conclusions: subject-verb (intensity ... is associated)'),
    ('received by a highly vulnerable nursing home residents who have limited flexibility',
     'received by highly vulnerable nursing home residents who have limited flexibility',
     'Abstract Conclusions: drop article "a" before plural "residents"'),
    # Introduction
    ('from the last six months of prior administration to',
     'from the last 6 months of the prior administration to',
     'Intro: add "the"; six -> 6'),
    ('street-arrests accelerated faster than',
     'street arrests accelerated faster than',
     'Intro: "street-arrests" -> "street arrests" (consistent hyphenation)'),
    ('due to deportation fears, raises absenteeism and increases turnover,',
     'due to deportation fears, raise absenteeism, and increase turnover,',
     'Intro: verb agreement/parallelism with "make"; add serial comma'),
    ('enforcement around the second Trump administration onto',
     'enforcement during the second Trump administration onto',
     'Intro: "around" -> "during"'),
    ('how nursing home response to escalated immigration shock is critical',
     'how nursing homes respond to escalated immigration shock is critical',
     'Intro: "nursing home response ... is" -> "nursing homes respond ..."'),
    # Methods
    ('waived from institutional board review',
     'exempt from institutional review board (IRB) review',
     'Methods: correct term "institutional review board (IRB)"'),
    ('quality data used inspection deficiency citations recorded in Centers',
     'quality data comprised inspection deficiency citations recorded in the Centers',
     'Methods: "used" -> "comprised"; add "the"'),
    ('summary data includes the total number',
     'summary data include the total number',
     'Methods: data take plural verb ("include")'),
    ('disaggregated by regulatory domains',
     'disaggregated by regulatory domain',
     'Methods: "domains" -> "domain" (counts by domain)'),
    ('the date of inspection and facility',
     'the date of inspection, and the facility',
     'Methods: serial comma + "the"'),
    ('arrest location date, arrest method',
     'arrest location, date, arrest method',
     'Methods: missing comma (arrest location, date)'),
    ('due to data availability in the Nursing Home Survey Summary',
     'due to data unavailability in the Nursing Home Survey Summary',
     'Methods: "availability" -> "unavailability" (states were excluded for lack of data)'),
    ('35.2% are with one investigation, and 62.4% with two investigations',
     '35.2% had 1 investigation, and 62.4% had 2 investigations',
     'Methods: idiom "had N investigation(s)"; numerals'),
    ('aggregate ten types of health deficiencies to three broad regulatory domains',
     'aggregate 10 types of health deficiencies into 3 broad regulatory domains',
     'Methods: numerals; "to" -> "into"'),
    ('associated with additional one ICE arrest per 100,000 population',
     'associated with one additional ICE arrest per 100,000 population',
     'Methods: word order "one additional"'),
    # Results
    ('administration & environment citations were 4.32',
     'administration and environment citations were 4.32',
     'Results: "&" -> "and" (consistent domain name)'),
    ('street arrests intensity outpaced',
     'street arrest intensity outpaced',
     'Results: "street arrests intensity" -> "street arrest intensity"'),
    ('for street arrests than custodial arrests',
     'for street arrests than for custodial arrests',
     'Results: add "for"'),
    ('each one additional ICE arrests per 100,000 state residents was associated',
     'each additional ICE arrest per 100,000 state residents was associated',
     'Results: "each one additional ... arrests" -> "each additional ... arrest"'),
    ('non-for-profit/government-owned facilities are statistically significant',
     'nonprofit/government-owned facilities was statistically significant',
     'Results: "non-for-profit" -> "nonprofit"; difference ... was'),
    ('Except the quality deficiencies, the difference between urban and rural facilities are significant',
     'Except for quality deficiencies, the difference between urban and rural facilities was significant',
     'Results: "Except for"; difference ... was'),
    ('with the largest effect observed for staffing',
     'with the largest association observed for staffing',
     'Results: "effect" -> "association" (observational language)'),
    # Discussion
    ('care quality, staffing, and administration and maintenance',
     'care quality, staffing, and administration and environment',
     'Discussion: "maintenance" -> "environment" (domain named "administration and environment")'),
    ('such as childcare sector',
     'such as the childcare sector',
     'Discussion: add article "the"'),
    ('increasing frequent deficiency',
     'increasingly frequent deficiency',
     'Discussion: "increasing frequent" -> "increasingly frequent"'),
    ('in nursing home facility',
     'in nursing home facilities',
     'Discussion: "facility" -> "facilities"'),
    ('further supports this interpretation',
     'further support this interpretation',
     'Discussion: "analyses ... further support"'),
    # Key Points heading
    ('Key points', 'Key Points', 'Heading capitalization "Key Points"'),
    # References
    ('Chidambaram P, Burns, Alice, Neuman, Tricia, Rudowitz, Robin',
     'Chidambaram P, Burns A, Neuman T, Rudowitz R',
     'Ref 30: JAMA author-name format'),

    # ---- Section B: JAMA numeral conversions ----
    ('Approximately one in five certified nursing assistants',
     'Approximately 1 in 5 certified nursing assistants', 'JAMA numerals: 1 in 5'),
    ('per 100,000 residents (one-month lag)',
     'per 100,000 residents (1-month lag)', 'JAMA numerals: 1-month lag'),
    ('We further disaggregated arrests along three dimensions',
     'We further disaggregated arrests along 3 dimensions', 'JAMA numerals: 3 dimensions'),
    ('makes three contributions to the literature',
     'makes 3 contributions to the literature', 'JAMA numerals: 3 contributions'),
    ('ranging from one to four months',
     'ranging from 1 to 4 months', 'JAMA numerals: 1 to 4 months'),
    ('averaged over the prior two, three, and four months',
     'averaged over the prior 2, 3, and 4 months', 'JAMA numerals: 2, 3, and 4 months'),
    ('broken down by three major regulatory deficiency domains',
     'broken down by 3 major regulatory deficiency domains', 'JAMA numerals: 3 domains'),
    ('from the most recent three inspection cycles',
     'from the most recent 3 inspection cycles', 'JAMA numerals: 3 inspection cycles'),
    ('Statistical tests were two-sided',
     'Statistical tests were 2-sided', 'JAMA numerals: 2-sided'),
]

# ---------------------------------------------------------------------------
# Section B: confidence-interval dash standardization (en-dash -> hyphen)
# ---------------------------------------------------------------------------

CI_DASH_RE = re.compile(r'\d\.\d{3}–\d\.\d{3}')

def standardize_ci_dashes(para):
    n = 0
    while True:
        _, _, full = _run_map(para)
        m = CI_DASH_RE.search(full)
        if not m:
            break
        old = m.group(0)
        new = old.replace('–', '-')
        if not apply_change(para, old, new):
            break
        n += 1
    return n

# ---------------------------------------------------------------------------
# Section C: comments (anchor_text, comment_text)
# ---------------------------------------------------------------------------

COMMENTS = [
    ('across 47 states',
     'State count: 50 states minus Kentucky and Pennsylvania = 48, not 47. '
     'Please confirm the correct number of states (also appears in Methods and '
     'the Table 1 footnote, which says "contiguous United States," implying 46).'),
    ('47 of the 50 states',
     'Count does not reconcile: 50 minus KY and PA = 48, not 47. If a third state '
     'is also excluded, name it; otherwise this should read 48. The Table 1 footnote '
     'says "contiguous United States," which conflicts with "47 of the 50 states."'),
    ('contiguous United States',
     '"Contiguous United States" (48 states) conflicts with the main text\'s '
     '"47 of the 50 states." Please make the geographic scope consistent across '
     'the abstract, Methods, and this footnote.'),
    ('10 deficiency subcategories',
     'Subcategory count: text says 10, but the Figure 2 note lists 11 (quality 4 + '
     'staffing 2 + administration 5). Please reconcile.'),
    ('infection control and pharmacy services',
     'Inconsistent with the Results, which state the non-significant subcategories '
     'were infection control and environmental deficiencies (not pharmacy). Please '
     'confirm which subcategories were not significantly associated.'),
    ('JAMA Original Investigations require',
     'Leftover editorial checklist. The required JAMA sections (Author Contributions; '
     'Conflict of Interest Disclosures; Funding/Support; Role of Funder/Sponsor; Data '
     'Sharing Statement) appear to be missing. Please add these and delete this '
     'placeholder before submission.'),
    ('prime-age population aged 24',
     'Prime working age is conventionally defined as 25-54. Confirm that 24-54 is intended.'),
]

# ---------------------------------------------------------------------------
# Build new comment XML fragments
# ---------------------------------------------------------------------------

def esc(s):
    return s.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')

def build_comment_parts(start_id):
    """Return (comments_frag, ext_frag, ids_frag, anchors[(cid, anchor)])."""
    comments_frag, ext_frag, ids_frag, anchors = [], [], [], []
    for k, (anchor, text) in enumerate(COMMENTS):
        cid = start_id + k
        para_id = f'{0xA0000000 + cid:08X}'
        dur_id  = f'{0xB0000000 + cid:08X}'
        comments_frag.append(
            f'<w:comment w:id="{cid}" w:author="{esc(AUTHOR)}" w:date="{DATE}" '
            f'w:initials="{INITS}">'
            f'<w:p w14:paraId="{para_id}" w14:textId="77777777">'
            f'<w:pPr><w:pStyle w:val="CommentText"/></w:pPr>'
            f'<w:r><w:rPr><w:rStyle w:val="CommentReference"/></w:rPr><w:annotationRef/></w:r>'
            f'<w:r><w:t xml:space="preserve">{esc(text)}</w:t></w:r>'
            f'</w:p></w:comment>'
        )
        ext_frag.append(f'<w15:commentEx w15:paraId="{para_id}" w15:done="0"/>')
        ids_frag.append(
            f'<w16cid:commentId w16cid:paraId="{para_id}" w16cid:durableId="{dur_id}"/>'
        )
        anchors.append((cid, anchor))
    return ''.join(comments_frag), ''.join(ext_frag), ''.join(ids_frag), anchors

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    if not os.path.exists(INPUT):
        sys.exit(f'Input not found: {INPUT}')

    with zipfile.ZipFile(INPUT, 'r') as z:
        doc_xml      = z.read('word/document.xml')
        comments_xml = z.read('word/comments.xml').decode('utf-8')
        ext_xml      = z.read('word/commentsExtended.xml').decode('utf-8')
        ids_xml      = z.read('word/commentsIds.xml').decode('utf-8')

    tree = etree.fromstring(doc_xml)

    # ---- determine next free comment id (existing are 0..6) ----
    existing = [int(c.get(wn('id'))) for c in tree.iter(wn('commentReference'))]
    # also scan comments.xml for ids
    existing += [int(m) for m in re.findall(r'<w:comment\b[^>]*\bw:id="(\d+)"', comments_xml)]
    start_id = (max(existing) + 1) if existing else 0

    # ---- Section A + B text edits ----
    applied, missing = [], []
    for old, new, desc in CHANGES:
        done = False
        for para in tree.iter(wn('p')):
            if old in para_plain_text(para):
                if apply_change(para, old, new):
                    done = True
                    break
        (applied if done else missing).append(desc if done else (old[:70], desc))

    # ---- Section B: CI dash standardization ----
    dash_total = 0
    for para in tree.iter(wn('p')):
        dash_total += standardize_ci_dashes(para)

    # ---- Section C: comment anchors ----
    comments_frag, ext_frag, ids_frag, anchors = build_comment_parts(start_id)
    anchored, anchor_missing = [], []
    for cid, anchor in anchors:
        placed = False
        for para in tree.iter(wn('p')):
            if anchor in para_plain_text(para):
                if add_comment_anchor(para, anchor, cid):
                    placed = True
                    break
        (anchored if placed else anchor_missing).append((cid, anchor))

    # ---- splice new comment fragments into the three comment parts ----
    comments_out = comments_xml.replace('</w:comments>', comments_frag + '</w:comments>')
    ext_out      = ext_xml.replace('</w15:commentsEx>', ext_frag + '</w15:commentsEx>')
    ids_out      = ids_xml.replace('</w16cid:commentsIds>', ids_frag + '</w16cid:commentsIds>')

    doc_out = etree.tostring(tree, xml_declaration=True, encoding='UTF-8', standalone=True)

    # ---- write new docx ----
    replacements = {
        'word/document.xml':         doc_out,
        'word/comments.xml':         comments_out.encode('utf-8'),
        'word/commentsExtended.xml': ext_out.encode('utf-8'),
        'word/commentsIds.xml':      ids_out.encode('utf-8'),
    }
    with tempfile.NamedTemporaryFile(suffix='.docx', delete=False) as tf:
        tmp = tf.name
    with zipfile.ZipFile(INPUT, 'r') as zin, \
         zipfile.ZipFile(tmp, 'w', zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            data = replacements.get(item.filename)
            zout.writestr(item, data if data is not None else zin.read(item.filename))
    if os.path.exists(OUTPUT):
        os.remove(OUTPUT)
    shutil.copy(tmp, OUTPUT)
    os.remove(tmp)

    # ---- report ----
    print(f'Tracked edits applied: {len(applied)}/{len(CHANGES)}')
    for a in applied:
        print(f'  + {a}')
    if missing:
        print(f'\nText edits NOT found ({len(missing)}):')
        for snip, desc in missing:
            print(f'  x {desc}\n      searched: "{snip}"')
    print(f'\nCI dashes standardized (en-dash -> hyphen): {dash_total}')
    print(f'\nComments inserted: {len(anchored)}/{len(COMMENTS)}  (ids {start_id}-{start_id+len(COMMENTS)-1})')
    for cid, anchor in anchored:
        print(f'  # id={cid} @ "{anchor}"')
    if anchor_missing:
        print(f'\nComment anchors NOT found ({len(anchor_missing)}):')
        for cid, anchor in anchor_missing:
            print(f'  x id={cid} "{anchor}"')
    print(f'\nOutput -> {OUTPUT}')


if __name__ == '__main__':
    main()
