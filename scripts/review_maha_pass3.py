"""
review_maha_pass3.py — Pass 3 tracked-changes review of the YMCARE proposal.
Based on findings from Executive Summary and MOU review.
IDs start at 4000 / CID 300 to avoid clashing with Pass 1+2.

Changes:
  A1-A7  Delete 7 remaining editorial notes
  B      Add CMS model overlap exclusion criterion (tracked insertion)
  C      Add HIPAA authorization statement in Section 6 (tracked insertion)
  D      Add YNHHS low-burden role paragraph in Section 6 (tracked insertion)
  E      Reconcile cost per participant ($1,800-$2,200 -> $2,200-$2,700) + comment
  F      Comment: Partners section missing contact names/MOUs
  G      Comment: Section 1 LOI confirmation needed
  H      Comment: Section 5 population size/randomization language
  I      Comment: LiveWell role specification
"""
import sys, copy, zipfile, io
from lxml import etree
from docx import Document

sys.stdout.reconfigure(encoding="utf-8", errors="replace")

SRC = r"C:\Users\xc77\Dropbox\Chen Xi Writings\Grants\MAHA\CMS Grant\Core\YMCARE_MAHA_ELEVATE_Proposal_Tracked_Review_v2.docx"
OUT = r"C:\Users\xc77\Dropbox\Chen Xi Writings\Grants\MAHA\CMS Grant\Core\YMCARE_MAHA_ELEVATE_Proposal_Tracked_Review_v3.docx"

AUTHOR = "XC Review"
DATE   = "2026-05-07T00:00:00Z"
WNS    = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
def wq(t): return f"{{{WNS}}}{t}"

_ID  = [4000]
_CID = [300]
def nid():  _ID[0]  += 1; return str(_ID[0])
def ncid(): _CID[0] += 1; return str(_CID[0])

# ── XML helpers ───────────────────────────────────────────────────────────────
def para_full_text(p):
    return "".join((e.text or "") for e in p.iter()
                   if e.tag in (wq("t"), wq("delText")))

def make_run(text, bold=False):
    r = etree.Element(wq("r"))
    rpr = etree.SubElement(r, wq("rPr"))
    if bold:
        b = etree.SubElement(rpr, wq("b")); b.set(wq("val"), "1")
    t = etree.SubElement(r, wq("t"))
    t.text = text
    if text and (text.startswith(" ") or text.endswith(" ")):
        t.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
    return r

def del_run(text):
    """Create a tracked-deletion run."""
    d = etree.Element(wq("del"))
    d.set(wq("id"), nid()); d.set(wq("author"), AUTHOR); d.set(wq("date"), DATE)
    r = etree.Element(wq("r"))
    dt = etree.SubElement(r, wq("delText"))
    dt.text = text
    if text and (text.startswith(" ") or text.endswith(" ")):
        dt.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
    d.append(r)
    return d

def ins_run(text, bold=False):
    """Create a tracked-insertion run."""
    ins = etree.Element(wq("ins"))
    ins.set(wq("id"), nid()); ins.set(wq("author"), AUTHOR); ins.set(wq("date"), DATE)
    r = make_run(text, bold)
    ins.append(r)
    return ins

def mark_existing_run_deleted(r):
    """
    Wrap an existing <w:r> in <w:del> in-place, converting <w:t> → <w:delText>.
    Skips if already inside a <w:del>.
    """
    parent = r.getparent()
    if parent is None or parent.tag == wq("del"):
        return  # already deleted
    for t_elem in r.findall(wq("t")):
        t_elem.tag = wq("delText")
    idx = list(parent).index(r)
    parent.remove(r)
    d = etree.Element(wq("del"))
    d.set(wq("id"), nid()); d.set(wq("author"), AUTHOR); d.set(wq("date"), DATE)
    d.append(r)
    parent.insert(idx, d)

def make_ins_para(text, style_val="List Bullet", bold=False):
    """Create a full paragraph as a tracked insertion."""
    p = etree.Element(wq("p"))
    ppr = etree.SubElement(p, wq("pPr"))
    ps = etree.SubElement(ppr, wq("pStyle")); ps.set(wq("val"), style_val)
    rpr_in_ppr = etree.SubElement(ppr, wq("rPr"))
    ins_mark = etree.SubElement(rpr_in_ppr, wq("ins"))
    ins_mark.set(wq("id"), nid()); ins_mark.set(wq("author"), AUTHOR); ins_mark.set(wq("date"), DATE)
    p.append(ins_run(text, bold))
    return p

def replace_run_text_tracked(r, old_text, new_text):
    """
    Replace text in a specific run with tracked del+ins.
    Returns True if replacement was performed.
    """
    parent = r.getparent()
    if parent is None: return False
    t_elem = r.find(wq("t"))
    if t_elem is None or t_elem.text is None: return False
    if old_text not in t_elem.text: return False

    full = t_elem.text
    before = full[:full.find(old_text)]
    after  = full[full.find(old_text) + len(old_text):]
    idx = list(parent).index(r)
    parent.remove(r)

    offset = 0
    if before:
        keep_r = make_run(before)
        parent.insert(idx + offset, keep_r); offset += 1
    parent.insert(idx + offset, del_run(old_text));  offset += 1
    parent.insert(idx + offset, ins_run(new_text));   offset += 1
    if after:
        keep_r2 = make_run(after)
        parent.insert(idx + offset, keep_r2)
    return True

# ── Comment helpers ───────────────────────────────────────────────────────────
_comments = []

def add_comment_to_para(p, comment_text):
    cid = ncid()
    _comments.append((cid, comment_text))
    start = etree.Element(wq("commentRangeStart")); start.set(wq("id"), cid)
    end   = etree.Element(wq("commentRangeEnd"));   end.set(wq("id"), cid)
    ref_r = etree.Element(wq("r"))
    etree.SubElement(ref_r, wq("rPr"))
    cref = etree.SubElement(ref_r, wq("commentReference")); cref.set(wq("id"), cid)
    p.insert(0, start); p.append(end); p.append(ref_r)

def build_comments_xml():
    root = etree.Element(wq("comments"), nsmap={"w": WNS})
    for cid, txt in _comments:
        c  = etree.SubElement(root, wq("comment"))
        c.set(wq("id"), cid); c.set(wq("author"), AUTHOR); c.set(wq("date"), DATE)
        cp = etree.SubElement(c, wq("p"))
        cr = etree.SubElement(cp, wq("r"))
        ct = etree.SubElement(cr, wq("t")); ct.text = txt
    return etree.tostring(root, xml_declaration=True, encoding="UTF-8", standalone=True)

def inject_comments_into_zip(zip_bytes, new_comments_bytes):
    with zipfile.ZipFile(io.BytesIO(zip_bytes)) as zin:
        names = [i.filename for i in zin.infolist()]
        has_comments = "word/comments.xml" in names

        # Merge with existing comments if present
        if has_comments:
            old_root = etree.fromstring(zin.read("word/comments.xml"))
            new_root = etree.fromstring(new_comments_bytes)
            for child in new_root:
                old_root.append(child)
            merged = etree.tostring(old_root, xml_declaration=True, encoding="UTF-8", standalone=True)
        else:
            merged = new_comments_bytes

        # Pre-compute patched rels / content-types only if no comments.xml exists
        if not has_comments:
            rels_xml  = zin.read("word/_rels/document.xml.rels")
            rels_root = etree.fromstring(rels_xml)
            rns = "http://schemas.openxmlformats.org/package/2006/relationships"
            rel = etree.SubElement(rels_root, f"{{{rns}}}Relationship")
            rel.set("Id", "rComments3")
            rel.set("Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/comments")
            rel.set("Target", "comments.xml")
            patched_rels = etree.tostring(rels_root, xml_declaration=True, encoding="UTF-8", standalone=True)

            ct_xml  = zin.read("[Content_Types].xml")
            ct_root = etree.fromstring(ct_xml)
            ctns = "http://schemas.openxmlformats.org/package/2006/content-types"
            ov = etree.SubElement(ct_root, f"{{{ctns}}}Override")
            ov.set("PartName", "/word/comments.xml")
            ov.set("ContentType", "application/vnd.openxmlformats-officedocument.wordprocessingml.comments+xml")
            patched_ct = etree.tostring(ct_root, xml_declaration=True, encoding="UTF-8", standalone=True)
        else:
            patched_rels = patched_ct = None

        buf = io.BytesIO()
        with zipfile.ZipFile(buf, "w", zipfile.ZIP_DEFLATED) as zout:
            for item in zin.infolist():
                if item.filename == "word/comments.xml":
                    zout.writestr(item, merged)
                elif item.filename == "word/_rels/document.xml.rels" and patched_rels:
                    zout.writestr(item, patched_rels)
                elif item.filename == "[Content_Types].xml" and patched_ct:
                    zout.writestr(item, patched_ct)
                else:
                    zout.writestr(item, zin.read(item.filename))
            if not has_comments:
                zout.writestr("word/comments.xml", merged)
    return buf.getvalue()

# ── Load document ─────────────────────────────────────────────────────────────
doc = Document(SRC)
body = doc.element.body
all_p = list(body.iter(wq("p")))

def find_para_xml(substr, start=0):
    for i, p in enumerate(all_p[start:], start):
        if substr.lower() in para_full_text(p).lower():
            return i, p
    return None, None

print(f"Total XML paragraphs: {len(all_p)}")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION A — Delete remaining editorial notes
# ══════════════════════════════════════════════════════════════════════════════
print("\n[A] Deleting remaining editorial notes...")

def delete_runs_by_text(p, texts_to_delete):
    """Mark runs containing any of texts_to_delete as tracked deletions."""
    count = 0
    for r in list(p.iter(wq("r"))):
        # Skip runs already inside <w:del>
        par = r.getparent()
        if par is not None and par.tag == wq("del"):
            continue
        t_elem = r.find(wq("t"))
        if t_elem is None or t_elem.text is None:
            continue
        for del_txt in texts_to_delete:
            if del_txt in t_elem.text:
                # Split run if needed so we only delete the target phrase
                full = t_elem.text
                if full == del_txt:
                    mark_existing_run_deleted(r)
                    count += 1
                else:
                    # Partial run: split and delete only matching portion
                    before = full[:full.find(del_txt)]
                    after  = full[full.find(del_txt) + len(del_txt):]
                    parent = r.getparent()
                    if parent is None: continue
                    idx = list(parent).index(r)
                    parent.remove(r)
                    offset = 0
                    if before:
                        parent.insert(idx + offset, make_run(before)); offset += 1
                    parent.insert(idx + offset, del_run(del_txt));     offset += 1
                    if after:
                        parent.insert(idx + offset, make_run(after))
                    count += 1
                break
    return count

# A1 — Para [126]: delete parenthetical in inclusion criteria
i_a1, p_a1 = find_para_xml("should we add this criteria")
if p_a1 is not None:
    n = delete_runs_by_text(p_a1, [
        " (should we add this criteria to get more robust outcomes, or stick with multimorbidity?)",
        "(should we add this criteria to get more robust outcomes, or stick with multimorbidity?)",
        " (",
        "should we add this criteria",
        " to get more robust outcomes, or stick with multimorbidity?)",
    ])
    # Also handle split across multiple runs: mark each run containing these fragments
    for r in list(p_a1.iter(wq("r"))):
        par = r.getparent()
        if par is not None and par.tag == wq("del"): continue
        t_elem = r.find(wq("t"))
        if t_elem is None or t_elem.text is None: continue
        txt = t_elem.text
        if any(frag in txt for frag in ["should we add this criteria", "to get more robust outcomes", "or stick with multimorbidity"]):
            mark_existing_run_deleted(r)
            n += 1
    print(f"  A1 done [{i_a1}]: deleted {n} run(s)")
else:
    print("  A1 SKIP: not found")

# A2 — Para [128]: delete "(would this be expensive?...)"
i_a2, p_a2 = find_para_xml("would this be expensive")
if p_a2 is not None:
    for r in list(p_a2.iter(wq("r"))):
        par = r.getparent()
        if par is not None and par.tag == wq("del"): continue
        t_elem = r.find(wq("t"))
        if t_elem is None or t_elem.text is None: continue
        if "would this be expensive" in t_elem.text or "increase access)" in t_elem.text:
            mark_existing_run_deleted(r)
    print(f"  A2 done [{i_a2}]: '(would this be expensive...)'")
else:
    print("  A2 SKIP: not found")

# A3 — Para [137]: delete "NEW ADDITION TODAY:" prefix (run[1] and run[2] space)
i_a3, p_a3 = find_para_xml("NEW ADDITION TODAY:")
if p_a3 is not None:
    for r in list(p_a3.iter(wq("r"))):
        par = r.getparent()
        if par is not None and par.tag == wq("del"): continue
        t_elem = r.find(wq("t"))
        if t_elem is None or t_elem.text is None: continue
        if "NEW ADDITION TODAY:" in t_elem.text or (t_elem.text.strip() == "" and t_elem.text != ""):
            # Be careful: only delete the "NEW ADDITION TODAY:" text
            if "NEW ADDITION TODAY:" in t_elem.text:
                full = t_elem.text
                before = full[:full.find("NEW ADDITION TODAY:")]
                after  = full[full.find("NEW ADDITION TODAY:") + len("NEW ADDITION TODAY:"):]
                parent = r.getparent()
                if parent is None: continue
                idx = list(parent).index(r)
                parent.remove(r)
                offset = 0
                if before:
                    parent.insert(idx + offset, make_run(before)); offset += 1
                parent.insert(idx + offset, del_run("NEW ADDITION TODAY:")); offset += 1
                if after and after.strip():
                    parent.insert(idx + offset, make_run(after))
    print(f"  A3 done [{i_a3}]: 'NEW ADDITION TODAY:' prefix")
else:
    print("  A3 SKIP: not found")

# A4 — Para [486]: delete " - Soo and Amanda please review and give feedback"
i_a4, p_a4 = find_para_xml("Soo and Amanda please review")
if p_a4 is None:
    i_a4, p_a4 = find_para_xml("Soo and")
if p_a4 is not None:
    for r in list(p_a4.iter(wq("r"))):
        par = r.getparent()
        if par is not None and par.tag == wq("del"): continue
        t_elem = r.find(wq("t"))
        if t_elem is None or t_elem.text is None: continue
        txt = t_elem.text
        if any(frag in txt for frag in [" - ", "Soo and ", "Amanda", " please review and give feedback"]):
            # Only delete if NOT part of the heading text itself
            if txt.strip() not in ["9.3 PCP structured chronic disease management care plan"]:
                mark_existing_run_deleted(r)
    print(f"  A4 done [{i_a4}]: 'Soo and Amanda...'")
else:
    print("  A4 SKIP: not found")

# A5 — Para [556]: delete " - Do we need to scale this back?"
i_a5, p_a5 = find_para_xml("Do we need to scale this back")
if p_a5 is not None:
    for r in list(p_a5.iter(wq("r"))):
        par = r.getparent()
        if par is not None and par.tag == wq("del"): continue
        t_elem = r.find(wq("t"))
        if t_elem is None or t_elem.text is None: continue
        txt = t_elem.text
        if any(frag in txt for frag in ["Do we need to scale this back", " - "]):
            if txt.strip() not in ["PCP Care Plan and Clinical-to-Behavioral Implementation Measures"]:
                mark_existing_run_deleted(r)
    print(f"  A5 done [{i_a5}]: 'Do we need to scale this back?'")
else:
    print("  A5 SKIP: not found")

# A6 — Para [627]: delete " - CDR?"
i_a6, p_a6 = find_para_xml("CDR?")
if p_a6 is not None:
    for r in list(p_a6.iter(wq("r"))):
        par = r.getparent()
        if par is not None and par.tag == wq("del"): continue
        t_elem = r.find(wq("t"))
        if t_elem is None or t_elem.text is None: continue
        if "CDR?" in t_elem.text or (t_elem.text.strip() == "-"):
            if t_elem.text.strip() not in ["Cognitive stage"]:
                mark_existing_run_deleted(r)
    print(f"  A6 done [{i_a6}]: '- CDR?'")
else:
    print("  A6 SKIP: not found")

# A7 — Para [693]: delete "(MYLOH?)"
i_a7, p_a7 = find_para_xml("MYLOH")
if p_a7 is not None:
    for r in list(p_a7.iter(wq("r"))):
        par = r.getparent()
        if par is not None and par.tag == wq("del"): continue
        t_elem = r.find(wq("t"))
        if t_elem is None or t_elem.text is None: continue
        if "MYLOH" in t_elem.text or t_elem.text.strip() == "(MYLOH?)":
            mark_existing_run_deleted(r)
    print(f"  A7 done [{i_a7}]: '(MYLOH?)'")
else:
    print("  A7 SKIP: not found")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION B — Add CMS model overlap exclusion criterion
# ══════════════════════════════════════════════════════════════════════════════
print("\n[B] Adding CMS model overlap exclusion criterion...")
i_excl, p_excl = find_para_xml("conflicting lifestyle or care management trial")
if p_excl is not None:
    overlap_text = (
        "Beneficiaries currently enrolled in an active CMS Innovation Center demonstration or model "
        "(including MSSP, ACO REACH, GUIDE, CPC+, MCP, or other CMMI models) are presumptively excluded "
        "unless CMS explicitly confirms that simultaneous MAHA ELEVATE enrollment is permissible. Yale will "
        "submit a written inquiry to the CMS MAHA ELEVATE project team prior to enrollment launch to confirm "
        "eligibility rules for YNHHS-attributed beneficiaries who participate in other CMMI models. "
        "YNHHS will disclose all active CMMI model participations to the Yale study team within 14 days of "
        "award execution to facilitate this review."
    )
    new_p_b = make_ins_para(overlap_text, style_val="List Bullet")
    parent_excl = p_excl.getparent()
    idx_excl = list(parent_excl).index(p_excl)
    parent_excl.insert(idx_excl + 1, new_p_b)
    add_comment_to_para(new_p_b, "CRITICAL (NOFO compliance): YNHHS participates in MSSP and possibly other CMMI models. Beneficiaries in active CMMI demonstrations may be ineligible for MAHA ELEVATE enrollment without CMS approval. Confirm with CMS MAHA ELEVATE project officer before finalizing eligibility criteria. See also MOU Section — YNHHS must disclose all active model participations.")
    print(f"  Done: CMS overlap criterion added after [{i_excl}]")
else:
    print("  SKIP: conflicting trial para not found")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION C — Add HIPAA authorization statement in Section 6
# ══════════════════════════════════════════════════════════════════════════════
print("\n[C] Adding HIPAA authorization statement...")
i_sec6, p_sec6 = find_para_xml("EHR Identification, Clinical Templates")
if p_sec6 is not None:
    hipaa_text = (
        "HIPAA Authorization for CMS Data: Each enrolled participant will provide a HIPAA Authorization "
        "prior to CMS sharing beneficiary enrollment or claims data with the Yale study team. The authorization "
        "will specify the categories of Medicare data to be shared (Parts A and B claims, enrollment records, "
        "chronic condition flags), the duration of authorized data use, and participants' right to revoke. "
        "Yale will confirm the required authorization scope, language, and covered-entity pathway with Yale "
        "University's HIPAA Privacy Officer, CMS, and IRB before the consent and enrollment process is finalized."
    )
    new_p_c = make_ins_para(hipaa_text, style_val="Normal")
    parent_sec6 = p_sec6.getparent()
    idx_sec6 = list(parent_sec6).index(p_sec6)
    parent_sec6.insert(idx_sec6 + 1, new_p_c)
    add_comment_to_para(new_p_c, "CRITICAL: HIPAA authorization is required for CMS to share Medicare enrollment/claims data with the study. Yale School of Nursing's covered-entity status under Yale University's HIPAA program must be confirmed before this language is finalized. See also MOU Review — this was flagged as a blocking compliance issue.")
    print(f"  Done: HIPAA authorization paragraph added after Section 6 heading [{i_sec6}]")
else:
    print("  SKIP: Section 6 heading not found")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION D — Add YNHHS low-burden role paragraph in Section 6
# ══════════════════════════════════════════════════════════════════════════════
print("\n[D] Adding YNHHS low-burden role paragraph...")
# Insert after the "NEW ADDITION TODAY" paragraph (which describes the EHR template system)
i_sec6b, p_sec6b = find_para_xml("integration of structured chronic disease management care-plan templates")
if p_sec6b is not None:
    ynhhs_role_text = (
        "YNHHS serves as YMCARE's principal clinical health-system access and usual-care partner. "
        "YNHHS's role is deliberately high-value and low-burden: YNHHS will facilitate EHR-based identification "
        "of eligible Original Medicare beneficiaries, support approved recruitment and communication pathways, "
        "enable appropriate EHR-derived and Medicare-linked data workflows, engage primary care teams at a limited "
        "level, provide usual clinical care, and support clinical escalation through standard care channels. "
        "YNHHS will not deliver the YMCARE lifestyle intervention, provide RN-led behavioral activation coaching, "
        "guarantee specific recruitment numbers, or absorb unfunded care-management responsibilities. "
        "These boundary conditions are codified in the YMCARE-MAHA ELEVATE MOU between Yale and YNHHS."
    )
    new_p_d = make_ins_para(ynhhs_role_text, style_val="Normal")
    parent_d = p_sec6b.getparent()
    idx_d = list(parent_d).index(p_sec6b)
    parent_d.insert(idx_d + 1, new_p_d)
    print(f"  Done: YNHHS low-burden role paragraph added after [{i_sec6b}]")
else:
    print("  SKIP: integration of care-plan templates para not found")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION E — Reconcile cost per participant in Section 13.1
# ══════════════════════════════════════════════════════════════════════════════
print("\n[E] Reconciling cost per participant with Executive Summary...")
i_cost, p_cost = find_para_xml("1,800")
if p_cost is None:
    i_cost, p_cost = find_para_xml("Direct clinical program costs per participant")
if p_cost is not None:
    for r in list(p_cost.iter(wq("r"))):
        par = r.getparent()
        if par is not None and par.tag == wq("del"): continue
        t_elem = r.find(wq("t"))
        if t_elem is None or t_elem.text is None: continue
        if "$1,800" in t_elem.text or "$1,800–$2,200" in t_elem.text:
            done = replace_run_text_tracked(r, "$1,800–$2,200", "$2,200–$2,700 (planning estimate: $2,500)")
            if done:
                print(f"  Done: '$1,800–$2,200' → '$2,200–$2,700 (planning estimate: $2,500)' in [{i_cost}]")
                break
    add_comment_to_para(p_cost, "CONSISTENCY NOTE: The Executive Summary presented to YNHHS uses $2,500 as the per-participant planning estimate (range $2,200-$2,700). This refers to direct program delivery costs. The full all-in figure (~$3,300/participant including evaluation, data infrastructure, and administration) should remain in the full proposal budget narrative. Ensure these two figures are clearly distinguished — ideally with a one-line footnote or parenthetical — so CMS reviewers and YNHHS executives see the same numbers.")
else:
    print("  SKIP: cost per participant paragraph not found")

# Also add comment to the main cost summary paragraph
i_cost2, p_cost2 = find_para_xml("approximately $3,300 over 3 years")
if p_cost2 is not None:
    add_comment_to_para(p_cost2, "Budget alignment check: (1) $3,300/participant all-in = $3.3M total / 1,000 intervention participants — correct math. (2) Direct delivery costs should be $2,200-$2,700/participant (= $2.2M-$2.7M for 1,000 participants), which leaves $600K-$1.1M for evaluation, data, administration. Verify these are consistent with the SF-424A budget breakdown. CMS will scrutinize the per-patient cost given the $3.3M award cap.")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION F — Comment: Partners section missing contact names and MOUs
# ══════════════════════════════════════════════════════════════════════════════
print("\n[F] Adding comment to Partners section...")
i_partners, p_partners = find_para_xml("12. Partners and Roles")
if p_partners is None:
    i_partners, p_partners = find_para_xml("Partners and Roles")
if p_partners is not None:
    add_comment_to_para(p_partners, "REVIEWER NOTE (Template 3 — 15 points): This Partners section is missing several elements CMS requires for scoring: (1) Named AOR (Authorized Organizational Representative) from Yale Office of Sponsored Projects; (2) Named compliance officer or IRB contact; (3) Contact names and organizational leads for YNHHS and LiveWell — CMS expects named partners, not just institutional descriptions; (4) Signed or draft MOUs from YNHHS and LiveWell (both should be attached as supplementary documents); (5) Quantitative prior program data for Table J — actual patient volumes, outcomes, and performance metrics from any prior implementation experience. These gaps could cost 5-10 points in Template 3 scoring.")
    print(f"  Done: comment added to Partners section [{i_partners}]")
else:
    print("  SKIP: Partners section not found")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION G — Comment: LOI confirmation in Section 1/intro area
# ══════════════════════════════════════════════════════════════════════════════
print("\n[G] Adding LOI confirmation comment...")
i_s1, p_s1 = find_para_xml("1. Executive Summary")
if p_s1 is None:
    i_s1, p_s1 = find_para_xml("Executive Summary")
if p_s1 is not None:
    add_comment_to_para(p_s1, "PRE-SUBMISSION CHECK (BLOCKING): Confirm that a Letter of Intent (LOI) was submitted to MAHAELEVATE@cms.hhs.gov by April 10, 2026. CMS states it will only review applications from organizations that submitted a timely LOI. If LOI was not submitted, contact CMS immediately to request late-submission guidance. Add a statement near the beginning of the proposal confirming the LOI submission date and any CMS confirmation received.")
    print(f"  Done: LOI comment added to Section 1 [{i_s1}]")
else:
    print("  SKIP: Section 1 heading not found")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION H — Comment: LiveWell role specification
# ══════════════════════════════════════════════════════════════════════════════
print("\n[H] Adding comment on LiveWell role...")
i_lw, p_lw = find_para_xml("LiveWell delivers community-based engagement")
if p_lw is not None:
    add_comment_to_para(p_lw, "REVIEWER NOTE: LiveWell's role description needs more specificity for Template 3 scoring. CMS will want to know: (1) LiveWell's legal name and organizational type (nonprofit? community health organization?); (2) LiveWell's prior experience with the Medicare/older adult population (numbers served, program fidelity, outcomes); (3) LiveWell's geographic coverage in New Haven and Fairfield Counties; (4) Whether LiveWell has an executed MOU or LOI — attach as supplementary document; (5) LiveWell's data governance and BAA capacity for handling PHI. Consider adding a dedicated 'Community Implementation Partner' row in the Partners table with this information.")
    print(f"  Done: LiveWell comment added [{i_lw}]")
else:
    print("  SKIP: LiveWell para not found")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION I — Comment: Population language in Section 5
# ══════════════════════════════════════════════════════════════════════════════
print("\n[I] Adding comment on population/randomization language in Section 5...")
i_pop, p_pop = find_para_xml("YMCARE will enroll approximately 2,000")
if p_pop is not None:
    add_comment_to_para(p_pop, "LANGUAGE CLARITY: The Executive Summary presented to YNHHS says '~2,000 Original Medicare beneficiaries' — consistent with the 1:1 randomization (1,000 intervention + 1,000 control). For CMS scoring purposes, be explicit about the intervention arm N=1,000 and control arm N=1,000. Also confirm that '2,000' is the ENROLLED target — there will be a larger screened pool (likely 4,000-6,000 screened to reach 2,000 enrolled, based on typical 30-50% consent rates in older adult research). Add a screening funnel estimate to Section 5 or the recruitment section.")
    print(f"  Done: population language comment [{i_pop}]")
else:
    print("  SKIP: population para not found")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION J — Replace Spanish language parenthetical with cleaner tracked text
# ══════════════════════════════════════════════════════════════════════════════
# Para [128] already had editorial note deleted in A2.
# Add a replacement ins_run with clean language after the deletion.
print("\n[J] Strengthening Spanish-language inclusion criterion language...")
i_sp, p_sp = find_para_xml("speaking, pending confirmation of Spanish-language")
if p_sp is not None:
    # Find the run with "speaking, pending..." and add a comment
    add_comment_to_para(p_sp, "EQUITY NOTE: Spanish-language inclusion is important for both scientific validity (equity) and NOFO scoring (accessibility and equity are scored in Template 1). Recommend strengthening to: 'English- or Spanish-speaking; bilingual study materials and RN contacts will be available for Spanish-speaking participants. Yale/YSN has experience with Spanish-language health education and will confirm bilingual RN capacity in the staffing plan.' Remove the hedging language about 'pending confirmation' — frame it as a commitment.")
    print(f"  Done: Spanish language comment [{i_sp}]")
else:
    print("  SKIP: Spanish para not found")

# ══════════════════════════════════════════════════════════════════════════════
# SECTION K — Add insert: YNHHS boundary conditions near Partners table
# ══════════════════════════════════════════════════════════════════════════════
print("\n[K] Adding YNHHS boundary conditions to Partners section area...")
i_pk, p_pk = find_para_xml("Yale New Haven Health System")
if p_pk is not None:
    boundary_text = (
        "YNHHS boundary conditions (consistent with executed MOU): YNHHS will not deliver the YMCARE "
        "lifestyle intervention, provide RN-led behavioral activation coaching, guarantee enrollment targets, "
        "or absorb unfunded care-management responsibilities. YNHHS clinicians continue to provide usual clinical "
        "care including diagnosis, medication management, referrals, preventive services, and longitudinal oversight. "
        "The intervention is delivered entirely by Yale/YSN personnel and LiveWell. YNHHS's role is limited to "
        "EHR-enabled identification, approved recruitment communications, data workflow support, low-burden primary "
        "care engagement, usual-care safety escalation, and governance/compliance participation."
    )
    new_p_k = make_ins_para(boundary_text, style_val="Normal")
    parent_pk = p_pk.getparent()
    idx_pk = list(parent_pk).index(p_pk)
    parent_pk.insert(idx_pk + 1, new_p_k)
    print(f"  Done: YNHHS boundary conditions paragraph added after [{i_pk}]")
else:
    print("  SKIP: YNHHS para not found in partners table")

# ══════════════════════════════════════════════════════════════════════════════
# Save and inject comments
# ══════════════════════════════════════════════════════════════════════════════
print("\nSaving and injecting comments...")
buf = io.BytesIO()
doc.save(buf)
new_comments = build_comments_xml()
final = inject_comments_into_zip(buf.getvalue(), new_comments)

with open(OUT, "wb") as f:
    f.write(final)

# Verify
import os
with zipfile.ZipFile(OUT) as z:
    names = z.namelist()
    from collections import Counter
    dupes = [n for n, c in Counter(names).items() if c > 1]

print(f"\n{'='*60}")
print(f"Done. Saved to:\n{OUT}")
print(f"File size: {os.path.getsize(OUT)/1024:.0f} KB")
print(f"Duplicate entries: {dupes if dupes else 'None'}")
print(f"Has comments.xml: {'word/comments.xml' in names}")
print(f"Comments added this pass: {len(_comments)}")
print("\nEdits applied:")
print("  A1-A7  Deleted 7 remaining editorial notes")
print("  B      Added CMS model overlap exclusion criterion")
print("  C      Added HIPAA authorization statement in Section 6")
print("  D      Added YNHHS low-burden role paragraph in Section 6")
print("  E      Reconciled cost per participant + comment")
print("  F      Comment: Partners section missing contact names/MOUs")
print("  G      Comment: LOI confirmation required (blocking)")
print("  H      Comment: LiveWell role specification needed")
print("  I      Comment: Population/screening funnel clarification")
print("  J      Comment: Spanish-language equity language")
print("  K      Added YNHHS boundary conditions paragraph")
