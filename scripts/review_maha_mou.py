"""
review_maha_mou.py — Tracked-changes review of YMCARE_MAHA_ELEVATE_YNHHS_MOU_Draft.docx
Adds: insertions, deletions, comment balloons for critical NOFO compliance gaps.
IDs start at 3000 / CID 200 to avoid clashing with proposal pass 1+2.
"""
import sys, copy, zipfile, io, re
from lxml import etree
from docx import Document

sys.stdout.reconfigure(encoding="utf-8", errors="replace")

SRC  = r"C:\Users\xc77\Dropbox\Chen Xi Writings\Grants\MAHA\CMS Grant\Core\YMCARE_MAHA_ELEVATE_YNHHS_MOU_Draft.docx"
OUT  = r"C:\Users\xc77\Dropbox\Chen Xi Writings\Grants\MAHA\CMS Grant\Core\YMCARE_MAHA_ELEVATE_YNHHS_MOU_Tracked_Review.docx"

AUTHOR = "XC Review"
DATE   = "2026-05-07T00:00:00Z"
WNS    = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
def wq(t): return f"{{{WNS}}}{t}"

_ID  = [3000]
_CID = [200]
def nid():  _ID[0]  += 1; return str(_ID[0])
def ncid(): _CID[0] += 1; return str(_CID[0])

# ── XML helpers ───────────────────────────────────────────────────────────────
def rpr_elem(bold=False):
    rpr = etree.Element(wq("rPr"))
    if bold:
        b = etree.SubElement(rpr, wq("b")); b.set(wq("val"), "1")
    return rpr

def make_run(text, bold=False):
    r = etree.Element(wq("r"))
    rpr = rpr_elem(bold)
    r.append(rpr)
    t = etree.SubElement(r, wq("t"))
    t.text = text
    if text.startswith(" ") or text.endswith(" "):
        t.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
    return r

def wrap_ins(child_elem):
    ins = etree.Element(wq("ins"))
    ins.set(wq("id"), nid())
    ins.set(wq("author"), AUTHOR)
    ins.set(wq("date"), DATE)
    ins.append(child_elem)
    return ins

def wrap_del(child_elem):
    d = etree.Element(wq("del"))
    d.set(wq("id"), nid())
    d.set(wq("author"), AUTHOR)
    d.set(wq("date"), DATE)
    d.append(child_elem)
    return d

def ins_run(text, bold=False):
    ins = etree.Element(wq("ins"))
    ins.set(wq("id"), nid())
    ins.set(wq("author"), AUTHOR)
    ins.set(wq("date"), DATE)
    r = make_run(text, bold)
    ins.append(r)
    return ins

def del_run(text):
    d = etree.Element(wq("del"))
    d.set(wq("id"), nid())
    d.set(wq("author"), AUTHOR)
    d.set(wq("date"), DATE)
    dr = etree.Element(wq("r"))
    dt = etree.SubElement(dr, wq("delText"))
    dt.text = text
    if text.startswith(" ") or text.endswith(" "):
        dt.set("{http://www.w3.org/XML/1998/namespace}space", "preserve")
    d.append(dr)
    return d

def make_ins_para(text, style_val="Normal", bold=False, numbering=None):
    """Create a full paragraph wrapped as tracked insertion."""
    p = etree.Element(wq("p"))
    ppr = etree.SubElement(p, wq("pPr"))
    ps = etree.SubElement(ppr, wq("pStyle"))
    ps.set(wq("val"), style_val)
    if numbering:
        num_pr = etree.SubElement(ppr, wq("numPr"))
        ilvl = etree.SubElement(num_pr, wq("ilvl")); ilvl.set(wq("val"), "0")
        numid = etree.SubElement(num_pr, wq("numId")); numid.set(wq("val"), numbering)
    # mark paragraph mark as inserted
    rpr = etree.SubElement(ppr, wq("rPr"))
    ins_rpr = etree.SubElement(rpr, wq("ins"))
    ins_rpr.set(wq("id"), nid()); ins_rpr.set(wq("author"), AUTHOR); ins_rpr.set(wq("date"), DATE)
    p.append(ins_run(text, bold))
    return p

def insert_paragraphs_after(body, ref_p, paras):
    """Insert list of lxml <w:p> elements after ref_p in body."""
    idx = list(body).index(ref_p)
    for i, new_p in enumerate(paras):
        body.insert(idx + 1 + i, new_p)

# ── Comment helpers ───────────────────────────────────────────────────────────
_comments = []  # list of (cid, text)

def add_comment_to_para(p, comment_text):
    cid = ncid()
    _comments.append((cid, comment_text))
    start = etree.Element(wq("commentRangeStart")); start.set(wq("id"), cid)
    end   = etree.Element(wq("commentRangeEnd"));   end.set(wq("id"), cid)
    ref_r = etree.Element(wq("r"))
    ref_rpr = etree.SubElement(ref_r, wq("rPr"))
    cref = etree.SubElement(ref_r, wq("commentReference")); cref.set(wq("id"), cid)
    p.insert(0, start)
    p.append(end)
    p.append(ref_r)

def build_comments_xml():
    root = etree.Element(
        "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}comments",
        nsmap={"w": WNS}
    )
    for cid, txt in _comments:
        c = etree.SubElement(root, wq("comment"))
        c.set(wq("id"), cid)
        c.set(wq("author"), AUTHOR)
        c.set(wq("date"), DATE)
        cp = etree.SubElement(c, wq("p"))
        cr = etree.SubElement(cp, wq("r"))
        ct = etree.SubElement(cr, wq("t"))
        ct.text = txt
    return etree.tostring(root, xml_declaration=True, encoding="UTF-8", standalone=True)

def inject_comments_into_zip(zip_bytes, new_comments_xml_bytes, existing_comments_xml=None):
    """Re-pack DOCX zip, merging old and new comments. Handles missing comments.xml cleanly."""
    if existing_comments_xml:
        old_root = etree.fromstring(existing_comments_xml)
        new_root = etree.fromstring(new_comments_xml_bytes)
        for child in new_root:
            old_root.append(child)
        merged = etree.tostring(old_root, xml_declaration=True, encoding="UTF-8", standalone=True)
    else:
        merged = new_comments_xml_bytes

    with zipfile.ZipFile(io.BytesIO(zip_bytes)) as zin:
        names = [i.filename for i in zin.infolist()]
        has_comments = "word/comments.xml" in names

        # Pre-compute patched rels and content_types if needed
        if not has_comments:
            rels_xml = zin.read("word/_rels/document.xml.rels")
            rels_root = etree.fromstring(rels_xml)
            rns = "http://schemas.openxmlformats.org/package/2006/relationships"
            new_rel = etree.SubElement(rels_root, f"{{{rns}}}Relationship")
            new_rel.set("Id", "rComments")
            new_rel.set("Type", "http://schemas.openxmlformats.org/officeDocument/2006/relationships/comments")
            new_rel.set("Target", "comments.xml")
            patched_rels = etree.tostring(rels_root, xml_declaration=True, encoding="UTF-8", standalone=True)

            ct_xml = zin.read("[Content_Types].xml")
            ct_root = etree.fromstring(ct_xml)
            ctns = "http://schemas.openxmlformats.org/package/2006/content-types"
            override = etree.SubElement(ct_root, f"{{{ctns}}}Override")
            override.set("PartName", "/word/comments.xml")
            override.set("ContentType", "application/vnd.openxmlformats-officedocument.wordprocessingml.comments+xml")
            patched_ct = etree.tostring(ct_root, xml_declaration=True, encoding="UTF-8", standalone=True)
        else:
            patched_rels = None
            patched_ct = None

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
all_paras = list(body.iter(wq("p")))

def find_para(substr, start=0):
    for i, p in enumerate(all_paras[start:], start):
        if substr.lower() in (p.text_content() if hasattr(p, 'text_content') else
                              "".join(t.text or "" for t in p.iter(wq("t")))).lower():
            return i, p
    return None, None

def para_text(p):
    return "".join(t.text or "" for t in p.iter(wq("t")))

# Print para index map for key sections
print("=== Locating key paragraphs ===")
for kw in ["10-15 minutes", "Data Governance", "Resource Commitments", "Non-Binding",
           "Signatures", "Letter of Support", "YNHHS is pleased", "Appendix C",
           "non-supplanting", "Billing", "Publications", "confidentiality",
           "intellectual property", "Governance and Operational", "LiveWell"]:
    i, p = find_para(kw)
    if p is not None:
        print(f"  [{i}] {para_text(p)[:80]}")
    else:
        print(f"  [NOT FOUND] {kw}")

# ── EDIT A: Soften PCP 10-15 min commitment (Section 6) ──────────────────────
print("\n[A] Softening PCP time estimate in Section 6...")
i6, p6 = find_para("10-15 minutes")
if p6 is not None:
    # Find and replace "approximately 10-15 minutes per enrolled participant, subject to workflow refinement"
    # with "minimal additional burden, with PCP time estimated at 10-15 minutes per enrolled participant pending workflow testing and PCP input"
    old_phrase = "approximately 10-15 minutes per enrolled participant, subject to workflow refinement"
    new_phrase = "minimal additional burden per enrolled participant; the 10-15 minute estimate is preliminary and subject to PCP input, workflow testing, and refinement prior to launch"
    for r in list(p6.iter(wq("r"))):
        t_elem = r.find(wq("t"))
        if t_elem is not None and t_elem.text and "10-15 minutes" in t_elem.text:
            full = t_elem.text
            before = full[:full.find("approximately 10-15")]
            after_start = full.find("subject to workflow refinement") + len("subject to workflow refinement")
            after = full[after_start:]
            parent = r.getparent()
            idx_r = list(parent).index(r)
            # keep 'before' in original run
            t_elem.text = before
            # insert del run for old phrase
            parent.insert(idx_r+1, del_run(old_phrase))
            # insert ins run for new phrase
            parent.insert(idx_r+2, ins_run(new_phrase))
            # keep 'after' in a new plain run if non-empty
            if after.strip():
                parent.insert(idx_r+3, make_run(after))
            break
    add_comment_to_para(p6, "REVIEWER NOTE: The 10-15 min PCP estimate may raise concern with YNHHS primary care leadership. Recommend piloting with 2-3 PCPs before committing to a specific number in any binding agreement. Remove or bracket this estimate in the version presented to YNHHS executives.")
    print(f"  Done. Para [{i6}]")
else:
    print("  [SKIP] 10-15 minutes not found")

# ── EDIT B: Add HIPAA authorization clause to Section 7 ──────────────────────
print("[B] Adding HIPAA authorization mechanism clause to Section 7...")
i7, p7 = find_para("HIPAA authorization language")
if p7 is not None:
    # Insert a new bullet after this paragraph clarifying HIPAA authorization pathway
    hipaa_text = (
        "HIPAA Authorization pathway: Yale School of Nursing's status as a HIPAA covered entity (or component thereof) "
        "requires legal confirmation prior to finalizing the data-sharing architecture. If YSN is not independently a "
        "covered entity, the Parties will obtain individual HIPAA authorizations from each enrolled participant before "
        "CMS shares beneficiary enrollment or claims data with the study team. Yale will seek guidance from Yale "
        "University's HIPAA Privacy Officer and consult with CMS on the required authorization pathway."
    )
    new_p = make_ins_para(hipaa_text, style_val="List Bullet")
    body_children = list(body)
    # find p7's position in body (it may be inside a table; use body-level fallback)
    try:
        idx_body = body_children.index(p7)
        body.insert(idx_body + 1, new_p)
    except ValueError:
        # p7 is in a table; insert after p7 in its parent
        parent7 = p7.getparent()
        idx_p7 = list(parent7).index(p7)
        parent7.insert(idx_p7 + 1, new_p)
    add_comment_to_para(p7, "CRITICAL (NOFO compliance): HIPAA authorization requirement is not resolved. CMS requires individual participant HIPAA authorization before sharing enrollment/claims data with non-covered entities. Yale legal/privacy must confirm YSN covered-entity status before application is submitted. This affects the entire data-sharing model.")
    print(f"  Done. Para [{i7}]")
else:
    print("  [SKIP] HIPAA authorization language not found")

# ── EDIT C: Add CMS model overlap disclosure clause ───────────────────────────
print("[C] Adding CMS model overlap disclosure clause...")
# Add after Section 7 data governance bullets, before Section 8
i8, p8 = find_para("non-supplanting")
if p8 is None:
    i8, p8 = find_para("Billing")
if p8 is not None:
    overlap_text = (
        "CMS model overlap disclosure: YNHHS will disclose to Yale, within 14 days of MOU execution, all active CMS "
        "Innovation model participations (including but not limited to MSSP, ACO REACH, GUIDE, CPC+, MCP, MIPS APM, "
        "or other CMMI models). Yale will review any disclosed model participations with CMS to identify applicable "
        "beneficiary overlap restrictions and adjust enrollment eligibility criteria as needed. Beneficiaries currently "
        "enrolled in another CMMI model may be ineligible or require CMS approval before enrollment in YMCARE-MAHA ELEVATE."
    )
    parent8 = p8.getparent()
    idx_p8 = list(parent8).index(p8)
    new_p_overlap = make_ins_para(overlap_text, style_val="List Bullet")
    parent8.insert(idx_p8, new_p_overlap)
    add_comment_to_para(new_p_overlap, "CRITICAL (NOFO compliance): YNHHS participates in multiple CMS models. MAHA ELEVATE prohibits enrolling beneficiaries who are simultaneously in other CMMI demonstrations unless CMS grants an exception. Must disclose all active model participations in the application (Template 1) and confirm beneficiary eligibility rules with CMS project officer before enrollment begins.")
    print(f"  Done. Inserted before para [{i8}]")
else:
    print("  [SKIP] Section 8 anchor not found")

# ── EDIT D: Add HDR portal access clause to Section 7 ─────────────────────────
print("[D] Adding HDR portal access clause...")
i_compliance, p_compliance = find_para("CMS model terms")
if p_compliance is not None:
    hdr_text = (
        "CMS Health Data Reporting (HDR) portal: Yale/YSN will establish and maintain the required CMS HDR portal "
        "account for submission of beneficiary-level data as required by the MAHA ELEVATE award. YNHHS will support "
        "HDR data preparation by providing agreed EHR-derived data elements on the CMS-specified submission schedule. "
        "CEHRT confirmation: YNHHS will confirm Epic's CHPL product ID (searchable at healthit.gov) and provide this "
        "information to Yale for inclusion in the Data Management Plan, as it is required for CMS scoring purposes."
    )
    parent_c = p_compliance.getparent()
    idx_c = list(parent_c).index(p_compliance)
    new_p_hdr = make_ins_para(hdr_text, style_val="List Bullet")
    parent_c.insert(idx_c + 1, new_p_hdr)
    add_comment_to_para(new_p_hdr, "ACTION REQUIRED: YNHHS IT/informatics must (1) confirm Epic CHPL ID at healthit.gov — this is worth 2 automatic scoring points in the DMP section of the application; (2) designate a contact for HDR portal data submissions. Both items are needed before May 15.")
    print(f"  Done. Para [{i_compliance}]")
else:
    print("  [SKIP] CMS model terms not found")

# ── EDIT E: Add reimbursement placeholder in Section 11 ──────────────────────
print("[E] Adding reimbursement placeholder in Section 11...")
i11, p11 = find_para("Anticipated YNHHS support is expected")
if p11 is not None:
    reimb_text = (
        "To the extent permitted under the MAHA ELEVATE award terms, Yale will seek to include reasonable "
        "reimbursement for direct YNHHS operational costs in the project budget, including EHR registry development, "
        "data extraction and quality review, informatics consultation, and communications review. Specific cost "
        "allocations will be confirmed in a subaward or services agreement if the project is funded."
    )
    parent11 = p11.getparent()
    idx_p11 = list(parent11).index(p11)
    new_p_reimb = make_ins_para(reimb_text, style_val="Normal")
    parent11.insert(idx_p11 + 1, new_p_reimb)
    add_comment_to_para(p11, "REVIEWER NOTE: YNHHS executives will ask whether YNHHS is compensated for its contributions. Consider adding a specific placeholder (e.g., '$X for EHR informatics consultation, $Y for data extraction') once the budget is finalized. YNHHS informatics and data-governance support typically costs $30k–$80k for a project of this scope.")
    print(f"  Done. Para [{i11}]")
else:
    print("  [SKIP] Section 11 para not found")

# ── EDIT F: Add publication review timeline in Section 12 ─────────────────────
print("[F] Adding publication review timeline to Section 12...")
i12, p12 = find_para("Publications, reports, and presentations")
if p12 is not None:
    pub_text = (
        "Publication and dissemination review process: Yale will provide YNHHS with a minimum of 30 days' advance "
        "written notice before submitting for publication or publicly presenting any findings that include YNHHS "
        "operational data, YNHHS patient-level outcomes, YNHHS names or marks, or YNHHS-specific performance results. "
        "YNHHS may submit written comments within this period. Disputes will be resolved through good-faith consultation; "
        "YNHHS may not unreasonably withhold consent to publication of bona fide scientific findings."
    )
    parent12 = p12.getparent()
    idx_p12 = list(parent12).index(p12)
    new_p_pub = make_ins_para(pub_text, style_val="List Bullet")
    parent12.insert(idx_p12 + 1, new_p_pub)
    add_comment_to_para(p12, "REVIEWER NOTE: Standard research MOU practice is a 30-day review window with an explicit statement that YNHHS cannot block publication of scientific findings. YNHHS legal will likely insert this language anyway — better to include it proactively.")
    print(f"  Done. Para [{i12}]")
else:
    print("  [SKIP] Publications paragraph not found")

# ── EDIT G: Comment on Appendix C (Letter of Support) — missing CEHRT/LOI ─────
print("[G] Adding comments to Appendix C (Letter of Support)...")
i_appc, p_appc = find_para("YNHHS is pleased to support")
if p_appc is not None:
    add_comment_to_para(p_appc, "REVIEWER NOTES on draft Letter of Support language: (1) Add explicit confirmation that LOI was submitted to MAHAELEVATE@cms.hhs.gov on April 10, 2026 — CMS will cross-check. (2) Add sentence confirming Epic CEHRT status and CHPL ID once confirmed. (3) Add sentence specifically mentioning YNHHS experience with CMS reporting (MIPS, QRDA III, quality reporting) — this supports both Template 3 and DMP scoring. (4) This draft will need to be on YNHHS letterhead, signed by the President or CEO (not a department head), and submitted as a PDF attachment — not embedded in the proposal.")
    print(f"  Done. Para [{i_appc}]")
else:
    print("  [SKIP] Appendix C letter not found")

# ── EDIT H: Comment on Signature block — AOR requirement ─────────────────────
print("[H] Adding AOR requirement comment to signature block...")
i_sig, p_sig = find_para("authorized to sign this MOU")
if p_sig is not None:
    add_comment_to_para(p_sig, "IMPORTANT: The Yale signatory must be the Authorized Organizational Representative (AOR) from the Yale Office of Research Administration — NOT the PI alone. CMS applications require institutional sign-off from the AOR. Confirm that the Yale signatory here is authorized under the Yale Grants & Contracts policy.")
    print(f"  Done. Para [{i_sig}]")
else:
    print("  [SKIP] Signature para not found")

# ── EDIT I: Comment on LiveWell scope ─────────────────────────────────────────
print("[I] Adding LiveWell scope comment...")
i_lw, p_lw = find_para("LiveWell or other approved partners")
if p_lw is not None:
    add_comment_to_para(p_lw, "REVIEWER NOTE: 'LiveWell or other approved partners' is appropriately flexible, but YNHHS may want to know LiveWell's data-sharing and liability terms before signing. Consider adding: 'Yale will provide YNHHS with a summary of data governance and liability arrangements with community implementation partners upon request.' This protects YNHHS from unknowingly endorsing a partner arrangement they haven't reviewed.")
    print(f"  Done. Para [{i_lw}]")
else:
    print("  [SKIP] LiveWell anchor not found")

# ── EDIT J: Add termination data-handling clause to Section 14 ───────────────
print("[J] Adding data handling post-termination clause to Section 14...")
i14, p14 = find_para("Termination will not affect obligations")
if p14 is not None:
    term_text = (
        "Upon termination or expiration of this MOU, each Party will, within 60 days, either (a) return to the other "
        "Party any confidential information, PHI, or proprietary materials received under this MOU, or (b) certify "
        "in writing that such materials have been destroyed in accordance with applicable law and institutional policy, "
        "except as retention is required by law, regulation, or CMS award terms."
    )
    parent14 = p14.getparent()
    idx_p14 = list(parent14).index(p14)
    new_p_term = make_ins_para(term_text, style_val="Normal")
    parent14.insert(idx_p14 + 1, new_p_term)
    print(f"  Done. Para [{i14}]")
else:
    print("  [SKIP] Termination para not found")

# ── EDIT K: Comment on non-binding vs operational specificity inconsistency ───
print("[K] Adding comment on non-binding vs. operational specificity inconsistency...")
i_nb, p_nb = find_para("non-binding financial obligations")
if p_nb is None:
    i_nb, p_nb = find_para("current intentions")
if p_nb is not None:
    add_comment_to_para(p_nb, "LEGAL CONSISTENCY NOTE: Section 15 disclaims binding financial obligations, but Appendix A and Section 6 contain specific operational commitments (e.g., EHR registry development, 10-15 min PCP time, data extraction). CMS reviewers reading a 'non-binding' MOU with this level of operational specificity may give it less weight than a subaward letter. Options: (1) Keep as-is and note this is a pre-award MOU to be superseded by a subaward — standard practice; (2) Add a sentence to Section 15 explicitly stating that operational commitments in Appendices A-B are intended as planning descriptions and not as binding service commitments until a subaward is executed.")
    print(f"  Done. Para [{i_nb}]")
else:
    print("  [SKIP] Non-binding para not found")

# ── Save and inject comments ──────────────────────────────────────────────────
print("\nSaving document and injecting comments...")
buf = io.BytesIO()
doc.save(buf)
zip_bytes = buf.getvalue()

# Check for existing comments.xml
with zipfile.ZipFile(io.BytesIO(zip_bytes)) as z:
    existing = z.read("word/comments.xml") if "word/comments.xml" in z.namelist() else None

new_comments = build_comments_xml()
final_zip = inject_comments_into_zip(zip_bytes, new_comments, existing_comments_xml=existing)

with open(OUT, "wb") as f:
    f.write(final_zip)

# Verify
with zipfile.ZipFile(OUT) as z:
    sz = len(final_zip)
    has_comments = "word/comments.xml" in z.namelist()
    comments_sz = z.getinfo("word/comments.xml").file_size if has_comments else 0

print(f"\n{'='*60}")
print(f"Done. Saved to:\n{OUT}")
print(f"File size: {sz/1024:.0f} KB")
print(f"Has comments.xml: {has_comments} ({comments_sz} bytes)")
print(f"Comments added: {len(_comments)}")
print(f"Edits applied: A (PCP time), B (HIPAA auth), C (CMS overlap), D (HDR/CEHRT),")
print(f"               E (reimbursement), F (publication timeline), G (letter of support),")
print(f"               H (AOR signature), I (LiveWell scope), J (termination data), K (non-binding)")
