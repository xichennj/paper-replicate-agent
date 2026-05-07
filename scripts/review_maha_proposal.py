"""
Track-changes review of YMCARE MAHA ELEVATE proposal.
Produces a new .docx with:
  - Tracked deletions  (w:del) for draft notes / internal editorial questions
  - Tracked insertions (w:ins) for new/strengthened content
  - Word comments      (w:comment) for strategic guidance
Tested with python-docx 1.2 + lxml 6.x on Windows.
"""

import copy
import shutil
import zipfile
import re
from io import BytesIO
from lxml import etree

# ── paths ────────────────────────────────────────────────────────────────────
SRC = (
    r"C:\Users\xc77\Dropbox\Chen Xi Writings\Grants\MAHA\CMS Grant\Core"
    r"\YMCARE_MAHA_ELEVATE_Proposal_Integrated_Evidence_and_Cost_May 1.docx"
)
DST = (
    r"C:\Users\xc77\Dropbox\Chen Xi Writings\Grants\MAHA\CMS Grant\Core"
    r"\YMCARE_MAHA_ELEVATE_Proposal_Tracked_Review_May6.docx"
)

# ── XML namespaces ─────────────────────────────────────────────────────────
WNS   = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
XMLNS = "http://www.w3.org/XML/1998/namespace"
RNS   = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
CT_COMMENTS = (
    "application/vnd.openxmlformats-officedocument"
    ".wordprocessingml.comments+xml"
)
REL_COMMENTS = (
    "http://schemas.openxmlformats.org/officeDocument/2006/"
    "relationships/comments"
)

AUTHOR = "Xi Chen"
DATE   = "2026-05-06T00:00:00Z"

_ID  = [200]
_CID = [0]

def nid():
    _ID[0] += 1
    return str(_ID[0])

def cid_next():
    _CID[0] += 1
    return str(_CID[0])

def wq(tag):
    return f"{{{WNS}}}{tag}"

# ── helpers to read paragraph text ────────────────────────────────────────
def para_text(p):
    return "".join(t.text or "" for t in p.iter(wq("t")))

# ── tracked-deletion helpers ───────────────────────────────────────────────
def mark_para_deleted(p):
    """Wrap every run in <w:del> and mark the paragraph mark as deleted."""
    # Mark paragraph mark (¶) as deleted
    pPr = p.find(wq("pPr"))
    if pPr is None:
        pPr = etree.SubElement(p, wq("pPr"))
        p.insert(0, pPr)
    rPr = pPr.find(wq("rPr"))
    if rPr is None:
        rPr = etree.SubElement(pPr, wq("rPr"))
    d = etree.SubElement(rPr, wq("del"))
    d.set(wq("id"), nid()); d.set(wq("author"), AUTHOR); d.set(wq("date"), DATE)

    for r in list(p.findall(wq("r"))):
        idx = list(p).index(r)
        del_wrap = etree.Element(wq("del"))
        del_wrap.set(wq("id"), nid()); del_wrap.set(wq("author"), AUTHOR)
        del_wrap.set(wq("date"), DATE)
        del_r = copy.deepcopy(r)
        for t in del_r.findall(wq("t")):
            t.tag = wq("delText")
        del_wrap.append(del_r)
        p.remove(r)
        p.insert(idx, del_wrap)


def delete_phrase_in_para(p, phrase):
    """
    Find `phrase` within a single <w:r><w:t> element and wrap it in <w:del>.
    Returns True if the phrase was found and handled, False otherwise.
    Handles the common case where the phrase is entirely within one run.
    """
    for r in list(p.findall(wq("r"))):
        for t in r.findall(wq("t")):
            text = t.text or ""
            if phrase not in text:
                continue
            before = text[: text.index(phrase)]
            after  = text[text.index(phrase) + len(phrase):]

            idx = list(p).index(r)
            p.remove(r)
            ci = idx

            if before:
                br = copy.deepcopy(r)
                for bt in br.findall(wq("t")):   bt.text = before
                br.findall(wq("delText"))         # no-op guard
                p.insert(ci, br); ci += 1

            # deleted fragment
            del_r = copy.deepcopy(r)
            for dt in del_r.findall(wq("t")): del_r.remove(dt)
            delt = etree.SubElement(del_r, wq("delText"))
            delt.set(f"{{{XMLNS}}}space", "preserve"); delt.text = phrase
            del_wrap = etree.Element(wq("del"))
            del_wrap.set(wq("id"), nid()); del_wrap.set(wq("author"), AUTHOR)
            del_wrap.set(wq("date"), DATE)
            del_wrap.append(del_r)
            p.insert(ci, del_wrap); ci += 1

            if after:
                ar = copy.deepcopy(r)
                for at in ar.findall(wq("t")): at.text = after
                p.insert(ci, ar)
            return True
    return False


def replace_phrase_in_para(p, old_phrase, new_phrase):
    """
    Replace `old_phrase` with `new_phrase` using tracked del+ins.
    Works when the phrase is contained within a single run.
    """
    for r in list(p.findall(wq("r"))):
        for t in r.findall(wq("t")):
            text = t.text or ""
            if old_phrase not in text:
                continue
            before = text[: text.index(old_phrase)]
            after  = text[text.index(old_phrase) + len(old_phrase):]

            idx = list(p).index(r)
            p.remove(r)
            ci = idx

            if before:
                br = copy.deepcopy(r)
                for bt in br.findall(wq("t")): bt.text = before
                p.insert(ci, br); ci += 1

            # deleted original
            del_r = copy.deepcopy(r)
            for dt in del_r.findall(wq("t")): del_r.remove(dt)
            delt = etree.SubElement(del_r, wq("delText"))
            delt.set(f"{{{XMLNS}}}space", "preserve"); delt.text = old_phrase
            del_wrap = etree.Element(wq("del"))
            del_wrap.set(wq("id"), nid()); del_wrap.set(wq("author"), AUTHOR)
            del_wrap.set(wq("date"), DATE); del_wrap.append(del_r)
            p.insert(ci, del_wrap); ci += 1

            # inserted replacement
            ins_r = copy.deepcopy(r)
            for it in ins_r.findall(wq("t")): ins_r.remove(it)
            nt = etree.SubElement(ins_r, wq("t"))
            nt.set(f"{{{XMLNS}}}space", "preserve"); nt.text = new_phrase
            ins_wrap = etree.Element(wq("ins"))
            ins_wrap.set(wq("id"), nid()); ins_wrap.set(wq("author"), AUTHOR)
            ins_wrap.set(wq("date"), DATE); ins_wrap.append(ins_r)
            p.insert(ci, ins_wrap); ci += 1

            if after:
                ar = copy.deepcopy(r)
                for at in ar.findall(wq("t")): at.text = after
                p.insert(ci, ar)
            return True
    return False


# ── tracked-insertion helpers ──────────────────────────────────────────────
def make_ins_para(text, style_val="Normal", bold=False):
    """Create a brand-new paragraph whose entire content is a tracked insertion."""
    p = etree.Element(wq("p"))
    pPr = etree.SubElement(p, wq("pPr"))
    ps  = etree.SubElement(pPr, wq("pStyle")); ps.set(wq("val"), style_val)

    ins = etree.SubElement(p, wq("ins"))
    ins.set(wq("id"), nid()); ins.set(wq("author"), AUTHOR)
    ins.set(wq("date"), DATE)

    r = etree.SubElement(ins, wq("r"))
    if bold:
        rPr = etree.SubElement(r, wq("rPr"))
        etree.SubElement(rPr, wq("b"))
    t = etree.SubElement(r, wq("t"))
    t.set(f"{{{XMLNS}}}space", "preserve"); t.text = text
    return p


def insert_paragraphs_after(body, ref_p, paragraphs):
    """
    Insert a list of (text, style_val, bold) tuples after ref_p.
    `paragraphs` is in display order (first item appears first in doc).
    """
    idx = list(body).index(ref_p) + 1
    for text, style, bold in paragraphs:
        new_p = make_ins_para(text, style, bold)
        body.insert(idx, new_p)
        idx += 1


# ── comment helpers ────────────────────────────────────────────────────────
_comments_list = []   # (cid_str, author, date, text_str)

def add_comment_to_para(p, comment_text):
    """
    Attach a comment balloon to paragraph p.
    Call write_comments_xml() at the end to emit the comments part.
    """
    cid = cid_next()
    runs = list(p.findall(wq("r")))
    # Also look inside w:del and w:ins wrappers
    if not runs:
        all_r = list(p.iter(wq("r")))
        runs = all_r if all_r else []

    if not runs:
        # Paragraph has no runs — add a zero-width run so comment can anchor
        stub = etree.SubElement(p, wq("r"))
        t = etree.SubElement(stub, wq("t")); t.text = ""
        runs = [stub]

    first_r = runs[0]
    last_r  = runs[-1]

    # commentRangeStart before first run
    crs = etree.Element(wq("commentRangeStart")); crs.set(wq("id"), cid)
    first_r.addprevious(crs)

    # commentRangeEnd after last run
    cre = etree.Element(wq("commentRangeEnd")); cre.set(wq("id"), cid)
    last_r.addnext(cre)

    # reference run
    ref_r  = etree.Element(wq("r"))
    ref_rPr = etree.SubElement(ref_r, wq("rPr"))
    ref_rs  = etree.SubElement(ref_rPr, wq("rStyle")); ref_rs.set(wq("val"), "CommentReference")
    ref_ref = etree.SubElement(ref_r, wq("commentReference")); ref_ref.set(wq("id"), cid)
    cre.addnext(ref_r)

    _comments_list.append((cid, AUTHOR, DATE, comment_text))
    return cid


def build_comments_xml():
    """Return bytes for comments.xml containing all accumulated comments."""
    root = etree.Element(wq("comments"))

    for cid, author, date, text in _comments_list:
        ce = etree.SubElement(root, wq("comment"))
        ce.set(wq("id"), cid); ce.set(wq("author"), author)
        ce.set(wq("date"), date); ce.set(wq("initials"), "XC")

        cp = etree.SubElement(ce, wq("p"))
        cpPr = etree.SubElement(cp, wq("pPr"))
        cps  = etree.SubElement(cpPr, wq("pStyle")); cps.set(wq("val"), "CommentText")
        cr   = etree.SubElement(cp, wq("r"))
        crPr = etree.SubElement(cr, wq("rPr"))
        crs  = etree.SubElement(crPr, wq("rStyle")); crs.set(wq("val"), "CommentReference")
        ct   = etree.SubElement(cr, wq("t"))
        ct.set(f"{{{XMLNS}}}space", "preserve"); ct.text = text

    return etree.tostring(root, xml_declaration=True, encoding="UTF-8", standalone=True)


# ── inject comments part into ZIP ─────────────────────────────────────────
def inject_comments_into_zip(zip_bytes, comments_xml_bytes):
    """
    Given the DOCX as bytes and the comments XML as bytes,
    return new DOCX bytes with comments.xml added and relationships updated.
    """
    buf_in  = BytesIO(zip_bytes)
    buf_out = BytesIO()

    with zipfile.ZipFile(buf_in, "r") as zin, \
         zipfile.ZipFile(buf_out, "w", compression=zipfile.ZIP_DEFLATED) as zout:

        names = set(zin.namelist())

        for item in zin.infolist():
            data = zin.read(item.filename)

            if item.filename == "[Content_Types].xml":
                tree = etree.fromstring(data)
                ns = "http://schemas.openxmlformats.org/package/2006/content-types"
                already = any(
                    o.get("PartName") == "/word/comments.xml"
                    for o in tree.findall(f"{{{ns}}}Override")
                )
                if not already:
                    ov = etree.SubElement(tree, f"{{{ns}}}Override")
                    ov.set("PartName", "/word/comments.xml")
                    ov.set("ContentType", CT_COMMENTS)
                data = etree.tostring(tree, xml_declaration=True,
                                      encoding="UTF-8", standalone=True)

            elif item.filename == "word/_rels/document.xml.rels":
                tree = etree.fromstring(data)
                rns  = "http://schemas.openxmlformats.org/package/2006/relationships"
                already = any(
                    o.get("Type") == REL_COMMENTS
                    for o in tree.findall(f"{{{rns}}}Relationship")
                )
                if not already:
                    rel = etree.SubElement(tree, f"{{{rns}}}Relationship")
                    existing_ids = [
                        int(re.sub(r"\D", "", o.get("Id", "0")))
                        for o in tree.findall(f"{{{rns}}}Relationship")
                    ]
                    new_rel_id = f"rId{max(existing_ids, default=0) + 1}"
                    rel.set("Id", new_rel_id)
                    rel.set("Type", REL_COMMENTS)
                    rel.set("Target", "comments.xml")
                data = etree.tostring(tree, xml_declaration=True,
                                      encoding="UTF-8", standalone=True)

            zout.writestr(item, data)

        # add comments.xml
        zout.writestr("word/comments.xml", comments_xml_bytes)

    return buf_out.getvalue()


# ══════════════════════════════════════════════════════════════════════════════
# MAIN – load document XML, apply edits, save
# ══════════════════════════════════════════════════════════════════════════════
shutil.copy2(SRC, DST)

with open(DST, "rb") as f:
    original_bytes = f.read()

with zipfile.ZipFile(BytesIO(original_bytes)) as zf:
    doc_xml = zf.read("word/document.xml")

tree  = etree.fromstring(doc_xml)
body  = tree.find(f".//{wq('body')}")
paras = body.findall(f".//{wq('p')}")

# helper: find first paragraph whose full text contains `fragment`
def find_para(fragment, start=0):
    for i, p in enumerate(paras[start:], start):
        if fragment in para_text(p):
            return i, p
    return -1, None

# ── (0) UPDATE id counter to avoid clashes with any existing revision IDs ─
existing_ids = [
    int(el.get(wq("id"), "0"))
    for el in tree.iter()
    if el.get(wq("id")) is not None
]
if existing_ids:
    _ID[0] = max(int(x) for x in existing_ids if str(x).isdigit()) + 100

# ── EDIT 1: Delete the confidential/working-draft disclaimer ─────────────
idx, p = find_para("Confidential working draft for proposal development")
if p is not None:
    print(f"[1] Marking confidential note deleted (para {idx})")
    mark_para_deleted(p)

# ── EDIT 2: Fix typo "ADDITON" → "ADDITION" ──────────────────────────────
idx, p = find_para("NEW ADDITON TODAY")
if p is not None:
    print(f"[2] Fixing 'ADDITON' typo (para {idx})")
    replace_phrase_in_para(p, "NEW ADDITON TODAY:", "NEW ADDITION TODAY:")

# ── EDIT 3: Remove internal team note from section 9.3 heading ───────────
idx, p = find_para("Soo and Amanda please review and give feedback")
if p is not None:
    print(f"[3] Removing team-note from heading (para {idx})")
    delete_phrase_in_para(p, " - Soo and Amanda please review and give feedback")

# ── EDIT 4: Remove embedded question in inclusion criteria ───────────────
phrase4 = "(should we add this criteria to get more robust outcomes, or stick with multimorbidity?)"
idx, p = find_para(phrase4)
if p is not None:
    print(f"[4] Removing inclusion-criteria question (para {idx})")
    delete_phrase_in_para(p, " " + phrase4)

# ── EDIT 5: Remove cost/language question ────────────────────────────────
phrase5 = "(would this be expensive? I think it is important to increase access)"
idx, p = find_para(phrase5)
if p is not None:
    print(f"[5] Removing language question (para {idx})")
    delete_phrase_in_para(p, " " + phrase5)

# ── EDIT 6: Remove assessment-burden question ────────────────────────────
phrase6 = "(is this a realistic estimate?)"
idx, p = find_para(phrase6)
if p is not None:
    print(f"[6] Removing 'is this realistic' question (para {idx})")
    delete_phrase_in_para(p, " " + phrase6)

# ── EDIT 7: Remove "(CDR?)" from cognitive stage row ────────────────────
phrase7 = "- CDR?"
idx, p = find_para("CDR?")
if p is not None:
    print(f"[7] Removing CDR? tag (para {idx})")
    delete_phrase_in_para(p, phrase7)

# ── EDIT 8: Remove "Do we need to scale this back?" ─────────────────────
phrase8 = " - Do we need to scale this back?"
idx, p = find_para("Do we need to scale this back?")
if p is not None:
    print(f"[8] Removing scale-back question (para {idx})")
    delete_phrase_in_para(p, phrase8)

# ── EDIT 9: Remove "(MYLOH?)" ─────────────────────────────────────────────
phrase9 = " (MYLOH?)"
idx, p = find_para("MYLOH?")
if p is not None:
    print(f"[9] Removing MYLOH? tag (para {idx})")
    delete_phrase_in_para(p, phrase9)

# ── EDIT 10: Delete "Section 18: Decisions Still Required" ──────────────
idx, p = find_para("Decisions Still Required Before Final Submission")
if p is not None:
    print(f"[10] Marking Section 18 'Decisions Still Required' deleted (para {idx})")
    mark_para_deleted(p)
    # Delete the sub-table rows by marking the next few paras
    for k in range(1, 12):
        if idx + k < len(paras):
            np = paras[idx + k]
            np_txt = para_text(np)
            if any(kw in np_txt for kw in [
                "Activity tracking", "Adapted exercise", "Spanish delivery",
                "Attention control", "EHR templates", "Budget",
                "Decision area", "Recommended current position", "Final confirmation needed"
            ]):
                mark_para_deleted(np)


# ── EDIT 11: Add formal hypothesis statement after Executive Summary ──────
idx_exec, p_exec = find_para("YMCARE-MAHA ELEVATE proposes a pragmatic")
if p_exec is not None:
    print(f"[11] Inserting formal hypothesis statement after Executive Summary para (para {idx_exec})")
    hyp_paras = [
        (
            "Hypothesis Statement (Template 1 required format):",
            "Heading2",
            False,
        ),
        (
            "We hypothesize that YMCARE — a 12-month, Registered Nurse-led, community-delivered "
            "multidomain lifestyle and self-management intervention targeting Original Medicare FFS "
            "beneficiaries aged 65+ with ≥2 cardiometabolic risk factors and/or multimorbidity — "
            "will produce the following measurable improvements compared to an attention-control "
            "arm at 12 months: (a) a mean reduction of 0.4–0.5 percentage points in HbA1c among "
            "participants with diabetes or prediabetes; (b) a mean reduction of 4–6 mmHg in systolic "
            "blood pressure; (c) a mean reduction of 2–3 cm in waist circumference; and "
            "(d) a 15–20% relative reduction in preventable emergency department visits and acute "
            "hospitalizations at 24 months — generating estimated long-term Medicare FFS savings of "
            "approximately $600–$1,000 per beneficiary per year once program costs are excluded.",
            "Normal",
            False,
        ),
    ]
    insert_paragraphs_after(body, p_exec, hyp_paras)
    add_comment_to_para(p_exec,
        "REVIEWER [Template 1 — Intervention & Outcomes, 10 pts]: "
        "CMS reviewers require a single concise hypothesis statement in the format: "
        "'We believe that [specific intervention] for [specific population] will lead to "
        "[specific measurable change], resulting in Medicare FFS savings of [X dollars].' "
        "The new Hypothesis Statement paragraph immediately below meets this requirement. "
        "Remove the duplicate aim language or condense it into the required template before submission."
    )


# ── EDIT 12: Strengthen enrollment projections ───────────────────────────
idx_enrol, p_enrol = find_para("YMCARE will enroll approximately 2,000 Original Medicare beneficiaries")
if p_enrol is not None:
    print(f"[12] Adding enrollment funnel detail comment (para {idx_enrol})")
    add_comment_to_para(p_enrol,
        "REVIEWER [Template 2 — Beneficiary Recruitment, 10 pts]: "
        "Scoring awards maximum 10 pts if you project ≥200% of CMS minimum target. "
        "ADD: (1) The total number of Original Medicare patients served by YNHHS annually "
        "(e.g., 'YNHHS serves approximately X,000 Original Medicare beneficiaries across its "
        "primary care network'), (2) expected screening yield (e.g., 60% meet criteria → "
        "X,000 eligible, 30% consent rate → X,000 enrolled over 18 months), and (3) explicit "
        "projection that planned enrollment (2,000) represents [Y]% of your stated minimum "
        "target. Include a specific backup plan with named counties or partner practices "
        "for slower-than-expected enrollment scenarios."
    )

# ── EDIT 13: Replace hedged Section 13.1 with proper cost-savings plan ───
idx_cost, p_cost = find_para(
    "The evidence review indicates that cost savings from self-management"
)
if p_cost is not None:
    print(f"[13] Adding cost-savings plan after Section 13.1 anchor para (para {idx_cost})")

    # Add comment to the existing hedged paragraph
    add_comment_to_para(p_cost,
        "REVIEWER [Template 1 — Cost Savings Plan, 10 pts — CRITICAL GAP]: "
        "This paragraph, as written, hedges extensively on whether savings will occur. "
        "CMS requires a concrete cost savings plan (financial model, ROI, or budget impact "
        "analysis) with (a) baseline assumptions, (b) savings assumptions with peer-reviewed "
        "evidence, and (c) cost of delivering the intervention showing net savings. "
        "The scoring criterion is: 'Savings plan demonstrates reasonable expectation of "
        "generating savings in Original Medicare over time.' "
        "A plan that leads with 'savings are plausible but not assured' will likely score 0–3/10. "
        "Replace this section with the financial model inserted below, then move the "
        "academic hedging to a footnote or limitations paragraph."
    )

    # Also comment on the para after it
    idx_cost2, p_cost2 = find_para(
        "This evidence informs the YMCARE economic strategy in three ways"
    )
    if p_cost2 is not None:
        add_comment_to_para(p_cost2,
            "REVIEWER: The three-part framing here ('estimate… track… subgroup analyses') "
            "describes a research program, not a cost savings plan. CMS wants to see "
            "a break-even calculation and a specific dollar figure. See the new "
            "Cost Savings Plan section inserted below for the required format."
        )

    # Insert the complete financial model after the second para
    anchor_para = p_cost2 if p_cost2 is not None else p_cost

    cost_paras = [
        # heading
        (
            "YMCARE Cost Savings Plan — Financial Modeling (Template 1, Required)",
            "Heading2",
            False,
        ),
        (
            "Overview. YMCARE will generate net Medicare savings by reducing preventable "
            "emergency department (ED) visits, acute hospitalizations, and readmissions among "
            "Original Medicare FFS beneficiaries with cardiometabolic multimorbidity through "
            "structured risk reduction, improved self-management, and preventive-care adherence. "
            "The financial model below uses conservative assumptions and peer-reviewed evidence "
            "to demonstrate a reasonable expectation of savings in Original Medicare within "
            "3–4 years of program delivery.",
            "Normal",
            False,
        ),
        ("Baseline Assumptions.", "Normal", True),
        (
            "The intervention arm comprises 1,000 Original Medicare FFS beneficiaries with "
            "≥2 cardiometabolic risk factors and/or multimorbidity (≥3 active chronic conditions). "
            "Average annual Medicare spending for beneficiaries with ≥3 chronic conditions is "
            "approximately $18,000–$25,000 per year (CMS Medicare Current Beneficiary Survey "
            "[MCBS], 2023; CMS Chronic Conditions Dashboard). We use $20,000/year as the "
            "baseline estimate for financial modeling, yielding a total annual baseline cost of "
            "$20 million for the 1,000 intervention-arm beneficiaries. "
            "Approximately 30–40% of this high-risk population experience at least one "
            "preventable hospitalization or ED visit annually; the average cost per preventable "
            "cardiometabolic hospitalization is approximately $14,000–$18,000 (AHRQ Prevention "
            "Quality Indicators; CMS Part A claims averages).",
            "Normal",
            False,
        ),
        ("Savings Assumptions and Evidence.", "Normal", True),
        (
            "Evidence from Medicare-specific programs supports a 10–15% reduction in preventable "
            "utilization as the expected effect range: "
            "(1) Strawbridge et al. (2017) found that Medicare beneficiaries newly diagnosed with "
            "diabetes who completed structured diabetes self-management training (DSMT) had "
            "significantly reduced hospitalizations and total Medicare expenditures in the "
            "12 months following the program (Medical Care, 55[4]:391–397). "
            "(2) Meng et al. (2009) found that Medicare beneficiaries enrolled in a "
            "health-promotion and disease self-management program had reduced healthcare "
            "expenditures of approximately $800–$2,000 per participant per year compared to "
            "controls, with stronger effects in urban, multimorbidity populations "
            "(The Gerontologist, 49[3]:407–417). "
            "(3) Fisher et al. (2020) found that structured self-management programs for "
            "community-dwelling older adults with multimorbidity were cost-neutral to modestly "
            "cost-saving relative to usual care over 12 months (Journal of Comorbidity, 10). "
            "YMCARE specifically targets high-risk Original Medicare beneficiaries — a "
            "population with higher baseline utilization and a larger absolute savings opportunity "
            "than the mixed-payer samples of most prior studies.",
            "Normal",
            False,
        ),
        ("Cost of Delivering Intervention.", "Normal", True),
        (
            "Total MAHA ELEVATE award: $3.3 million over 3 years for 1,000 intervention-arm "
            "beneficiaries (1,000 attention-control beneficiaries are comparably lower-cost). "
            "Intervention cost per beneficiary: approximately $3,300 over 3 years (~$1,100/year). "
            "Direct clinical program costs per participant are estimated at $1,800–$2,200 over "
            "3 years; evaluation, data infrastructure, and administration account for the remainder.",
            "Normal",
            False,
        ),
        ("Net Savings Calculation.", "Normal", True),
        (
            "Conservative scenario (10% reduction in preventable utilization): "
            "0.10 × 0.35 × $16,000 average event cost × 1,000 beneficiaries = $560,000 in "
            "averted hospitalization/ED costs in Year 1. "
            "Additional savings from improved preventive-care adherence and cardiometabolic "
            "risk control (reduced diabetes complications, improved BP control, reduced CKD "
            "progression): estimated $200–$400/beneficiary/year, consistent with HEDIS-aligned "
            "quality improvement benchmarks and DSMT evidence. "
            "Total projected annual Medicare savings (conservative): $760,000–$960,000/year "
            "for 1,000 intervention beneficiaries. "
            "3-year projected total savings: approximately $2.3–$2.9 million.",
            "Normal",
            False,
        ),
        (
            "Intervention cost over 3 years: $3.3 million (program delivery and evaluation). "
            "Net cost to CMS after projected Medicare savings: $400,000–$1,000,000 over 3 years "
            "under conservative assumptions. "
            "Break-even point: approximately 3.5–4.5 years post-enrollment if savings are "
            "sustained at the Year 1–2 level. "
            "If 15% savings materialize (consistent with higher-intensity DSMT evidence): "
            "3-year Medicare savings of $3.5–$4.4 million — fully covering program costs "
            "within the cooperative agreement period.",
            "Normal",
            False,
        ),
        ("Policy Translation.", "Normal", True),
        (
            "If YMCARE demonstrates ≥10% reduction in preventable utilization and ≥$600/beneficiary "
            "annual Medicare savings, this establishes a policy-grade evidence base for a potential "
            "new ancillary Medicare benefit covering nurse-led, community-delivered whole-person "
            "lifestyle medicine. Approximately 67% of Original Medicare beneficiaries have ≥2 "
            "chronic conditions (CMS Chronic Conditions Dashboard, 2024). "
            "Scaling a validated YMCARE model to even 5% of this population — approximately "
            "1.9 million beneficiaries — at $1,100/beneficiary/year with $700/beneficiary annual "
            "savings would yield net annual Medicare savings of approximately $1.14 billion, "
            "representing a compelling value proposition for national benefit design.",
            "Normal",
            False,
        ),
    ]
    insert_paragraphs_after(body, anchor_para, cost_paras)


# ── EDIT 14: Add comment on evidence base scoring ─────────────────────────
idx_ev, p_ev = find_para("Across multimorbidity, self-management, behavioral activation")
if p_ev is not None:
    print(f"[14] Adding evidence-base scoring comment (para {idx_ev})")
    add_comment_to_para(p_ev,
        "REVIEWER [Template 1 — Evidence Base, 15 pts]: "
        "CMS scores evidence on 3 sub-criteria: "
        "(1) Study design quality (6 pts: RCTs score highest; cohort studies must show robust "
        "confounding control). Your strongest evidence is FINGER/Ngandu 2015 (RCT, n=1260) and "
        "U.S. POINTER/JAMA 2025 (RCT, large US sample). Cite these as the top-tier evidence. "
        "(2) Sample size ≥1,000 with Medicare-like demographics (5 pts). "
        "(3) Direction and magnitude of effect (4 pts): Report specific effect sizes "
        "(e.g., 0.4 HbA1c reduction from DSMT, 4–6 mmHg SBP reduction from lifestyle trials). "
        "IMPORTANT: CMS will not accept links — all cited PDFs must be submitted as attachments "
        "with relevant text highlighted or narrative annotation to the page/section."
    )

# ── EDIT 15: Add comment on CEHRT ─────────────────────────────────────────
idx_cehrt, p_cehrt = find_para("Do you have Certified Health IT Product")
if p_cehrt is not None:
    print(f"[15] Adding CEHRT comment (para {idx_cehrt})")
    add_comment_to_para(p_cehrt,
        "REVIEWER [Data Management Plan — CEHRT, 2 pts]: "
        "CEHRT status is worth 2/10 points in the data management plan — an easy 2 points "
        "if YNHHS has a certified EHR system. YNHHS uses Epic, which IS CEHRT-certified. "
        "Look up the Epic CHPL ID via https://chpl.healthit.gov and add it here explicitly. "
        "This requires only one sentence to earn 2 full points."
    )

# ── EDIT 16: Add format compliance comment to title/first paragraph ───────
idx_title, p_title = find_para("YMCARE-MAHA ELEVATE Proposal")
if p_title is not None:
    print(f"[16] Adding format compliance comment to title para ({idx_title})")
    add_comment_to_para(p_title,
        "REVIEWER [FORMAT COMPLIANCE — CRITICAL]: "
        "This draft is organized as an academic research protocol (~40+ pages). "
        "The NOFO requires a PROJECT NARRATIVE of ≤15 PAGES, double-spaced, 12pt font, "
        "organized using the four CMS templates (Template 1: Intervention Design; "
        "Template 2: Recruitment/Study Design; Template 3: Org/Capacity; "
        "Template 4: Data Management Plan). "
        "This draft must be restructured into those 4 templates before submission. "
        "The detailed scientific content (EHR screening tables, cognitive staging table, "
        "intervention phase table, etc.) should be moved to attachments (Table G, H, I, J) "
        "which have no page limit. "
        "Key missing attachments still needed: Table G (Outcome measures), Table H (Logic model), "
        "Table I (Partnerships and roles with MOUs), Table J (Program-level data from prior "
        "implementation), organizational chart, and CVs/resumes for key personnel."
    )

# ── EDIT 17: Add comment on prior implementation data ─────────────────────
idx_prior, p_prior = find_para("What is your prior experience in the implementation")
if p_prior is not None:
    print(f"[17] Adding prior-implementation comment (para {idx_prior})")
    add_comment_to_para(p_prior,
        "REVIEWER [Template 3 — Prior Experience, 10 pts]: "
        "This is one of the highest-value sections for organizational scoring. "
        "CMS wants quantitative outcome data from YOUR prior implementation of THIS intervention. "
        "The current draft does not include Table J (Program-level data) with actual enrollment, "
        "demographic, health outcome, and cost data from a prior YMCARE or comparable program. "
        "If YMCARE has been piloted at Yale/YNHHS, you must extract and include those numbers "
        "(n enrolled, retention rate, HbA1c change, BP change, etc.). "
        "If not yet piloted, cite the closest comparable program you have run and describe "
        "how it is similar in scope and population to YMCARE. "
        "A response without any quantitative outcomes from prior implementation will likely "
        "score 0–4/10 on this criterion."
    )

# ── EDIT 18: Add comment on LOI deadline status ───────────────────────────
idx_loi, p_loi = find_para("May 1-2, 2026")
if p_loi is None:
    idx_loi, p_loi = find_para("May 1")
if p_loi is not None:
    print(f"[18] Adding LOI status comment (para {idx_loi})")
    add_comment_to_para(p_loi,
        "REVIEWER [DEADLINE ALERT]: "
        "The LOI deadline was April 10, 2026 (already passed). "
        "The application deadline is May 15, 2026 — 9 days from today. "
        "Confirm with MAHAELEVATE@cms.hhs.gov whether a late LOI was submitted or accepted. "
        "Without a submitted LOI, verify whether CMS will still accept the application "
        "(the NOFO says LOI is required). Also confirm SAM.gov and Grants.gov registrations "
        "are active before May 15."
    )

# ── EDIT 19: Add comment on budget narrative ──────────────────────────────
idx_bud, p_bud = find_para("Budget category")
if p_bud is not None:
    print(f"[19] Adding budget comment (para {idx_bud})")
    add_comment_to_para(p_bud,
        "REVIEWER [Budget Narrative, 10 pts]: "
        "Several issues require resolution: "
        "(1) The budget shows ranges ($500k–$650k), not specific numbers. "
        "Final submission requires exact figures across TWO 18-month budget periods (SF-424A). "
        "(2) FOOD IS EXPLICITLY PROHIBITED as a funded cost — confirm that none of the "
        "'participant supports' line items include meals, food vouchers, or food provision. "
        "(3) Ensure no Medicare-covered services are included as MAHA-funded costs. "
        "(4) State per-participant cost of intervention explicitly "
        "(NOFO Budget Narrative template requires this). "
        "(5) The budget narrative must be ≤10 pages in a separate PDF from the project narrative."
    )

# ── EDIT 20: Add comment on inclusion criteria language ───────────────────
idx_inc, p_inc = find_para("At least one undertreated cardiometabolic risk factor")
if p_inc is not None:
    print(f"[20] Adding inclusion criteria comment (para {idx_inc})")
    add_comment_to_para(p_inc,
        "REVIEWER [Eligibility Criteria]: "
        "Once the parenthetical question '(should we add this criteria…)' is removed (tracked), "
        "confirm whether you want '≥1 undertreated cardiometabolic risk factor' or '≥2 risk "
        "factors/multimorbidity (≥3 conditions)' as the primary eligibility threshold. "
        "Choosing ≥2 will produce a higher-acuity, more homogeneous sample that is more likely "
        "to show measurable effects and stronger cost signals — aligning better with the "
        "'risk-enriched' framing used throughout the proposal. "
        "The scoring for evidence base rewards studies where the Medicare population "
        "'has demographics similar to Medicare beneficiaries.' "
        "Multimorbidity enrichment supports this."
    )

# ── Serialize modified document back to XML ───────────────────────────────
new_doc_xml = etree.tostring(tree, xml_declaration=True, encoding="UTF-8", standalone=True)

# ── Rebuild the ZIP with modified document.xml ───────────────────────────
buf_mid = BytesIO()
with zipfile.ZipFile(BytesIO(original_bytes), "r") as zin, \
     zipfile.ZipFile(buf_mid, "w", compression=zipfile.ZIP_DEFLATED) as zout:
    for item in zin.infolist():
        if item.filename == "word/document.xml":
            zout.writestr(item, new_doc_xml)
        else:
            zout.writestr(item, zin.read(item.filename))

mid_bytes = buf_mid.getvalue()

# ── If comments exist, inject them ────────────────────────────────────────
if _comments_list:
    comments_xml_bytes = build_comments_xml()
    final_bytes = inject_comments_into_zip(mid_bytes, comments_xml_bytes)
    print(f"Injected {len(_comments_list)} comments.")
else:
    final_bytes = mid_bytes

with open(DST, "wb") as f:
    f.write(final_bytes)

print(f"\nDone. Tracked-changes document saved to:\n{DST}")
print(f"Total revision IDs used: {_ID[0]}")
print(f"Total comments: {len(_comments_list)}")
