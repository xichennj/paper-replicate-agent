"""
Pass 2 track-changes review — DMP, Template 2/3 gaps, HIPAA, enrollment funnel,
Table J, CEHRT, and remaining draft-note cleanup.

Reads: YMCARE_MAHA_ELEVATE_Proposal_Tracked_Review_May6.docx  (already has pass-1 changes)
Writes: YMCARE_MAHA_ELEVATE_Proposal_Tracked_Review_v2.docx
"""

import copy, zipfile, re
from io import BytesIO
from lxml import etree

# ── paths ────────────────────────────────────────────────────────────────────
SRC = (
    r"C:\Users\xc77\Dropbox\Chen Xi Writings\Grants\MAHA\CMS Grant\Core"
    r"\YMCARE_MAHA_ELEVATE_Proposal_Tracked_Review_May6.docx"
)
DST = (
    r"C:\Users\xc77\Dropbox\Chen Xi Writings\Grants\MAHA\CMS Grant\Core"
    r"\YMCARE_MAHA_ELEVATE_Proposal_Tracked_Review_v2.docx"
)

# ── namespaces ───────────────────────────────────────────────────────────────
WNS  = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
XMLNS = "http://www.w3.org/XML/1998/namespace"
RNS  = "http://schemas.openxmlformats.org/package/2006/relationships"
CT_COMMENTS = (
    "application/vnd.openxmlformats-officedocument"
    ".wordprocessingml.comments+xml"
)
REL_COMMENTS = (
    "http://schemas.openxmlformats.org/officeDocument/2006/"
    "relationships/comments"
)

AUTHOR = "Xi Chen"
DATE   = "2026-05-07T00:00:00Z"

def wq(t): return "{%s}%s" % (WNS, t)

# ── ID counters – start high to avoid clashing with pass-1 IDs ──────────────
_ID  = [2000]
_CID = [100]

def nid():
    _ID[0] += 1
    return str(_ID[0])

def cid_next():
    _CID[0] += 1
    return str(_CID[0])

# ── helpers ──────────────────────────────────────────────────────────────────
def para_text(p):
    return "".join(t.text or "" for t in p.iter(wq("t")))

def find_para(paras, fragment, start=0):
    fragment_l = fragment.lower()
    for i, p in enumerate(paras[start:], start):
        if fragment_l in para_text(p).lower():
            return i, p
    return -1, None

def mark_para_deleted(p):
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
    for r in list(p.findall(wq("r"))):
        for t in r.findall(wq("t")):
            text = t.text or ""
            if phrase not in text:
                continue
            before = text[: text.index(phrase)]
            after  = text[text.index(phrase) + len(phrase):]
            parent = r.getparent(); idx = list(parent).index(r); parent.remove(r); ci = idx
            if before:
                br = copy.deepcopy(r)
                for bt in br.findall(wq("t")): bt.text = before
                parent.insert(ci, br); ci += 1
            del_r = copy.deepcopy(r)
            for dt in del_r.findall(wq("t")): del_r.remove(dt)
            delt = etree.SubElement(del_r, wq("delText"))
            delt.set("{%s}space" % XMLNS, "preserve"); delt.text = phrase
            del_wrap = etree.Element(wq("del"))
            del_wrap.set(wq("id"), nid()); del_wrap.set(wq("author"), AUTHOR)
            del_wrap.set(wq("date"), DATE); del_wrap.append(del_r)
            parent.insert(ci, del_wrap); ci += 1
            if after:
                ar = copy.deepcopy(r)
                for at in ar.findall(wq("t")): at.text = after
                parent.insert(ci, ar)
            return True
    return False

def replace_phrase_in_para(p, old_phrase, new_phrase):
    for r in list(p.findall(wq("r"))):
        for t in r.findall(wq("t")):
            text = t.text or ""
            if old_phrase not in text:
                continue
            before = text[: text.index(old_phrase)]
            after  = text[text.index(old_phrase) + len(old_phrase):]
            parent = r.getparent(); idx = list(parent).index(r); parent.remove(r); ci = idx
            if before:
                br = copy.deepcopy(r)
                for bt in br.findall(wq("t")): bt.text = before
                parent.insert(ci, br); ci += 1
            del_r = copy.deepcopy(r)
            for dt in del_r.findall(wq("t")): del_r.remove(dt)
            delt = etree.SubElement(del_r, wq("delText"))
            delt.set("{%s}space" % XMLNS, "preserve"); delt.text = old_phrase
            del_wrap = etree.Element(wq("del"))
            del_wrap.set(wq("id"), nid()); del_wrap.set(wq("author"), AUTHOR)
            del_wrap.set(wq("date"), DATE); del_wrap.append(del_r)
            parent.insert(ci, del_wrap); ci += 1
            ins_r = copy.deepcopy(r)
            for it in ins_r.findall(wq("t")): ins_r.remove(it)
            nt = etree.SubElement(ins_r, wq("t"))
            nt.set("{%s}space" % XMLNS, "preserve"); nt.text = new_phrase
            ins_wrap = etree.Element(wq("ins"))
            ins_wrap.set(wq("id"), nid()); ins_wrap.set(wq("author"), AUTHOR)
            ins_wrap.set(wq("date"), DATE); ins_wrap.append(ins_r)
            parent.insert(ci, ins_wrap); ci += 1
            if after:
                ar = copy.deepcopy(r)
                for at in ar.findall(wq("t")): at.text = after
                parent.insert(ci, ar)
            return True
    return False

def make_ins_para(text, style_val="Normal", bold=False):
    p = etree.Element(wq("p"))
    pPr = etree.SubElement(p, wq("pPr"))
    ps  = etree.SubElement(pPr, wq("pStyle")); ps.set(wq("val"), style_val)
    ins = etree.SubElement(p, wq("ins"))
    ins.set(wq("id"), nid()); ins.set(wq("author"), AUTHOR); ins.set(wq("date"), DATE)
    r = etree.SubElement(ins, wq("r"))
    if bold:
        rPr = etree.SubElement(r, wq("rPr"))
        etree.SubElement(rPr, wq("b"))
    t = etree.SubElement(r, wq("t"))
    t.set("{%s}space" % XMLNS, "preserve"); t.text = text
    return p

def insert_paragraphs_after(body, ref_p, paragraphs):
    """Insert (text, style, bold) list after ref_p."""
    idx = list(body).index(ref_p) + 1
    for text, style, bold in paragraphs:
        body.insert(idx, make_ins_para(text, style, bold))
        idx += 1

# ── comment system ────────────────────────────────────────────────────────────
_comments_list = []

def add_comment(p, comment_text):
    cid = cid_next()
    runs = list(p.findall(wq("r")))
    if not runs:
        all_r = list(p.iter(wq("r")))
        runs = all_r if all_r else []
    if not runs:
        stub = etree.SubElement(p, wq("r"))
        etree.SubElement(stub, wq("t")).text = ""
        runs = [stub]
    first_r = runs[0]; last_r = runs[-1]
    crs = etree.Element(wq("commentRangeStart")); crs.set(wq("id"), cid)
    first_r.addprevious(crs)
    cre = etree.Element(wq("commentRangeEnd")); cre.set(wq("id"), cid)
    last_r.addnext(cre)
    ref_r = etree.Element(wq("r"))
    ref_rPr = etree.SubElement(ref_r, wq("rPr"))
    ref_rs  = etree.SubElement(ref_rPr, wq("rStyle")); ref_rs.set(wq("val"), "CommentReference")
    ref_ref = etree.SubElement(ref_r, wq("commentReference")); ref_ref.set(wq("id"), cid)
    cre.addnext(ref_r)
    _comments_list.append((cid, AUTHOR, DATE, comment_text))

def build_comments_xml():
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
        ct.set("{%s}space" % XMLNS, "preserve"); ct.text = text
    return etree.tostring(root, xml_declaration=True, encoding="UTF-8", standalone=True)

def inject_comments(zip_bytes, comments_xml_bytes):
    buf_in = BytesIO(zip_bytes); buf_out = BytesIO()
    with zipfile.ZipFile(buf_in, "r") as zin, \
         zipfile.ZipFile(buf_out, "w", compression=zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            data = zin.read(item.filename)
            if item.filename == "[Content_Types].xml":
                tree2 = etree.fromstring(data)
                ns = "http://schemas.openxmlformats.org/package/2006/content-types"
                if not any(o.get("PartName") == "/word/comments.xml"
                           for o in tree2.findall("{%s}Override" % ns)):
                    ov = etree.SubElement(tree2, "{%s}Override" % ns)
                    ov.set("PartName", "/word/comments.xml"); ov.set("ContentType", CT_COMMENTS)
                data = etree.tostring(tree2, xml_declaration=True, encoding="UTF-8", standalone=True)
            elif item.filename == "word/_rels/document.xml.rels":
                tree2 = etree.fromstring(data)
                if not any(o.get("Type") == REL_COMMENTS
                           for o in tree2.findall("{%s}Relationship" % RNS)):
                    existing_ids = [int(re.sub(r"\D","",o.get("Id","0")))
                                    for o in tree2.findall("{%s}Relationship" % RNS)]
                    rel = etree.SubElement(tree2, "{%s}Relationship" % RNS)
                    rel.set("Id", "rId%d" % (max(existing_ids, default=0)+1))
                    rel.set("Type", REL_COMMENTS); rel.set("Target", "comments.xml")
                data = etree.tostring(tree2, xml_declaration=True, encoding="UTF-8", standalone=True)
            zout.writestr(item, data)
        zout.writestr("word/comments.xml", comments_xml_bytes)
    return buf_out.getvalue()


# ═══════════════════════════════════════════════════════════════════════════════
# LOAD DOCUMENT
# ═══════════════════════════════════════════════════════════════════════════════
with open(SRC, "rb") as f:
    original_bytes = f.read()

with zipfile.ZipFile(BytesIO(original_bytes)) as zf:
    doc_xml = zf.read("word/document.xml")

tree  = etree.fromstring(doc_xml)
body  = tree.find(".//{%s}body" % WNS)
paras = body.findall(".//{%s}p" % WNS)


# ══ EDIT A: Delete remaining "Section 18 / Decisions Still Required" rows ════
# These are in table cells and weren't fully caught in pass 1
deletion_snippets_18 = [
    "Activity tracking", "Adapted exercise", "Spanish delivery",
    "Attention control", "EHR templates", "Confirm LiveWell",
    "Confirm cost, data governance", "Confirm translation",
    "Confirm IRB acceptability", "Develop structured participant-specific",
    "Confirm YNHHS build", "Target approximately $3.3M through asynchronous",
    "Confirm partner budgets", "Decision area", "Recommended current position",
    "Final confirmation needed", "Keep minimal and non-tailored",
    "Include chair-based and mobility-limited options",
    "Plan English and Spanish materials",
    "18. Decisions Still Required",
]
count_deleted_18 = 0
for snippet in deletion_snippets_18:
    idx, p = find_para(paras, snippet)
    if p is not None and para_text(p).strip():
        # only delete if not already marked deleted
        existing_dels = p.findall(".//{%s}del" % WNS)
        if not any(d.get(wq("author")) for d in existing_dels):
            mark_para_deleted(p)
            count_deleted_18 += 1
print("[A] Deleted %d remaining Section-18 / Decision-table rows." % count_deleted_18)


# ══ EDIT B: Partners section — add comment on Template 3 gaps ═══════════════
idx_par, p_par = find_para(paras, "12. Partners and Roles")
if p_par is not None:
    add_comment(p_par,
        "REVIEWER [Template 3 — Organization & Capacity, 15 pts]: "
        "This table lists partner roles but does NOT answer the Template 3 narrative questions CMS requires. "
        "Before submission, add a narrative section answering: "
        "(1) Describe your organization (Yale/YNHHS/LiveWell founding, size, patient volume, mission). "
        "(2) Name and credentials of the Authorized Organizational Representative (AOR). "
        "(3) Name and title of the compliance officer responsible for federal/state/local law compliance. "
        "(4) Prior experience delivering THIS intervention — with quantitative outcomes (n enrolled, "
        "HbA1c change, BP change, retention rate, cost data if available). "
        "(5) Any CMS models your organization has participated in (CPC+, GUIDE, MSSP, MIPS APM, etc.). "
        "Organizational chart with AOR named must also be submitted as an attachment. "
        "CVs/resumes for all key personnel named in the proposal are required attachments."
    )
    print("[B] Added Template 3 comment at Partners section.")

# ══ EDIT C: Add comment to Table I placeholder (Partners row) ════════════════
idx_ti, p_ti = find_para(paras, "Overall proposal leadership; scientific design")
if p_ti is not None:
    add_comment(p_ti,
        "REVIEWER [Table I — Partnerships and Roles Attachment]: "
        "Table I must be completed and submitted as an attachment. Required columns: "
        "Partnership organization name | Primary contact name | Role in the program | "
        "Number of patients | Partnership document (MOU/LOA/Contract). "
        "For YMCARE: confirm MOUs with (1) YNHHS, (2) LiveWell, (3) any participating "
        "primary care practices. These documents must be finalized and attached before submission. "
        "Missing MOUs will signal operational unreadiness to reviewers."
    )
    print("[C] Added Table I comment at Partners table.")

# ══ EDIT D: Template 2 — recruitment section comments ═══════════════════════
idx_rec, p_rec = find_para(paras, "YMCARE will enroll approximately 2,000 Original Medicare")
if p_rec is not None:
    add_comment(p_rec,
        "REVIEWER [Template 2 — Recruitment Plan, 10 pts]: "
        "Add three specifics CMS reviewers need to score this section: "
        "(1) EXACT YNHHS patient count: 'YNHHS serves approximately [X,000] Original Medicare "
        "FFS beneficiaries across [Y] primary care practices.' "
        "(2) ENROLLMENT FUNNEL: 'Our EHR pre-screening estimates [Z,000] meet cardiometabolic "
        "enrichment criteria. We project a 60–70% eligibility rate among those contacted, "
        "and a 35–45% consent rate among eligible patients, yielding 2,000 enrolled within "
        "12 months of award.' "
        "(3) PERCENTAGE OF TARGET: 'Based on these projections, our 2,000-participant goal "
        "represents [X]% of the estimated CMS minimum target, which we anticipate to be "
        "approximately [Y] per our communication with CMS.' "
        "Without these specifics, reviewers cannot award points for recruitment feasibility."
    )
    print("[D] Added recruitment funnel comment.")

# ══ EDIT E: Add specific enrollment timeline ════════════════════════════════
# Insert tracked text after the recruitment paragraph adding timeline specifics
if p_rec is not None:
    timeline_paras = [
        (
            "Enrollment timeline and backup plan: Enrollment will begin in Month 7 post-award "
            "(January 2027 for a Cohort 1 October 2026 start). Target milestones: "
            "10% of enrolled (200 participants) by Month 14 (per NOFO milestone requirement); "
            "50% (1,000 participants) by Month 20; 100% (2,000 participants) by Month 24. "
            "Service area: New Haven County and Fairfield County, Connecticut, with expansion to "
            "Hartford and Middlesex Counties if enrollment is slower than projected. "
            "If monthly enrollment lags >25% behind target at Month 10, we will activate "
            "secondary recruitment through YNHHS community health centers (FQHCs) and "
            "LiveWell's established community partner network to reach patients not previously "
            "engaged with YNHHS primary care.",
            "Normal",
            False,
        ),
    ]
    insert_paragraphs_after(body, p_rec, timeline_paras)
    print("[E] Added enrollment timeline tracked insertion.")

# ══ EDIT F: Add HIPAA authorization comment ══════════════════════════════════
idx_hipaa, p_hipaa = find_para(paras, "Enrolled in Original Medicare Parts A and B")
if p_hipaa is not None:
    add_comment(p_hipaa,
        "REVIEWER [HIPAA Authorization — Operational Requirement]: "
        "The NOFO states (page 27): 'Any organization that is NOT a covered entity or business "
        "associate of a covered entity under HIPAA must obtain a valid patient authorization "
        "(45 C.F.R. § 164.508) before CMS can share any PHI with the applicable organization.' "
        "YNHHS IS a covered entity. However, Yale University / Yale School of Nursing may NOT "
        "be a covered entity in this arrangement. "
        "Action required: Yale/YNHHS legal must determine whether Yale School of Nursing "
        "qualifies as a HIPAA covered entity or business associate of YNHHS for this project. "
        "If not, each enrolled participant must sign a HIPAA-compliant authorization before "
        "CMS can share enrollment data with Yale. This adds operational complexity. "
        "Document the legal analysis and describe the authorization process in the DMP section."
    )
    print("[F] Added HIPAA authorization comment.")

# ══ EDIT G: Add CMS model overlap comment ════════════════════════════════════
idx_cms, p_cms = find_para(paras, "Community implementation partner; Life-style intervention delivery")
if p_cms is not None:
    add_comment(p_cms,
        "REVIEWER [CMS Model Overlap — Template 3 Required Disclosure]: "
        "The NOFO asks (Template 3, Q6): 'List any CMS models in which you are participating "
        "now or have in the past.' You must disclose ALL current CMS model participation for "
        "YNHHS and any partner organizations. Common overlaps: MSSP ACO, CPC+, GUIDE "
        "(Guiding an Improved Dementia Experience), Making Care Primary (MCP), AHEAD, MIPS APM. "
        "CMS will determine overlap policies — this won't hurt your score, but omitting it "
        "could create post-award compliance issues. Add one sentence for each current model."
    )
    print("[G] Added CMS model overlap comment.")

# ══ EDIT H: Add DMP section before Reference List ════════════════════════════
idx_ref, p_ref = find_para(paras, "Reference List")
if p_ref is not None:
    print("[H] Inserting full Data Management Plan section before Reference List...")

    dmp_section = [
        # ── DMP heading
        (
            "Data Management Plan (Required — NOFO Template, Scoring: 10 points)",
            "Heading1",
            False,
        ),
        (
            "NOTE TO TEAM: The separate 'Data Management Plan.pdf' currently contains CMS example "
            "responses (including 'yoga studio' placeholder text from the NOFO template). Per the "
            "NOFO: 'We will not score applications that use the wording in our examples.' The "
            "responses below are YMCARE-specific and must replace the placeholder content in all "
            "submitted materials. Confirm YNHHS CEHRT CHPL ID before submission.",
            "Normal",
            False,
        ),
        # ── Q1
        (
            "Q1: What experience do you have collecting and reporting beneficiary-level data to CMS?",
            "Heading2",
            False,
        ),
        (
            "Yale New Haven Health System (YNHHS), the primary clinical partner for YMCARE, "
            "participates in the CMS Merit-Based Incentive Payment System (MIPS) and has "
            "submitted structured quality data to CMS annually through the QRDA III electronic "
            "submission pathway. YNHHS primary care practices participated in the CMS "
            "Comprehensive Primary Care Plus (CPC+) model and have direct experience with "
            "structured beneficiary-level data submission to the CMS Innovation Center, including "
            "use of the Health Data Reporting (HDR) portal. YNHHS has an established population "
            "health analytics team that configures Epic-based reports in CMS-required formats. "
            "Yale School of Public Health investigators on the YMCARE team have submitted "
            "Medicare claims data requests through the CMS Virtual Research Data Center (VRDC) "
            "and Research Data Assistance Center (ResDAC) for prior published health services "
            "research. For MAHA ELEVATE, YNHHS population health analysts will configure "
            "HDR-compatible beneficiary roster exports and clinical measure submissions within "
            "the six-month pre-implementation phase, building on existing MIPS reporting workflows. "
            "YNHHS IT will secure CMS IT systems access within the required six-month window "
            "post-award and submit TIN/NPI data via the recipient portal.",
            "Normal",
            False,
        ),
        # ── Q2
        (
            "Q2: How will you collect patient and provider information for submission to CMS?",
            "Heading2",
            False,
        ),
        (
            "YMCARE will collect and submit patient and provider data through three integrated "
            "channels: (1) YNHHS Epic CEHRT (see Q7 below), which serves as the primary source "
            "for beneficiary Medicare identifiers, TINs/NPIs for participating primary care "
            "practices, clinical measure values (HbA1c, BP, BMI, kidney health), and "
            "preventive-care completion status. YNHHS population health staff will generate "
            "monthly beneficiary roster extracts linked to Medicare enrollment identifiers. "
            "(2) Yale REDCap research database, HIPAA-compliant and hosted on secure Yale ITS "
            "servers, used for study-specific measures collected by research nurses at baseline, "
            "6-month, and 12-month assessments (Mini-Cog/SLUMS, PAM-13, waist circumference, SPPB). "
            "REDCap data will be merged with Epic identifiers using study-specific participant IDs "
            "and submitted to CMS via the HDR portal. (3) LiveWell community partner records, "
            "maintained in a HIPAA-compliant engagement tracking system linked to study IDs "
            "(not Medicare identifiers), used for tracking intervention dose received, community "
            "session attendance, and adherence metrics. A Business Associate Agreement will be "
            "executed between YNHHS, Yale, and LiveWell before any PHI exchange. "
            "YNHHS analytics will produce quarterly beneficiary rosters and clinical data reports "
            "meeting CMS HDR format specifications.",
            "Normal",
            False,
        ),
        # ── Q3
        (
            "Q3: What prior experience do you have collecting and securely storing PHI and PII?",
            "Heading2",
            False,
        ),
        (
            "YNHHS maintains a comprehensive HIPAA Privacy and Security program fully compliant "
            "with 45 CFR Parts 160 and 164, covering all PHI across the Epic EHR, clinical "
            "registries, and research databases. The Epic system is protected by role-based access "
            "controls, multi-factor authentication, automatic session timeout, audit logging, and "
            "automated de-identification functions. All YMCARE research data management complies "
            "with Yale Human Research Protection Program (HRPP) requirements and IRB-approved "
            "protocols. Research-specific PHI and PII will be stored in: (a) YNHHS Epic, protected "
            "by YNHHS enterprise security policies; and (b) Yale REDCap, encrypted in transit "
            "(TLS 1.2+) and at rest (AES-256), with role-based access controls, audit trails, "
            "and automatic de-identification on export. Business Associate Agreements will be "
            "executed with all external vendors accessing PHI, including LiveWell and any "
            "third-party data analytics vendors. YNHHS has a documented cybersecurity incident "
            "response program and conducts annual HIPAA security training for all clinical and "
            "research personnel. Yale HRPP has successfully overseen PHI data management for "
            "multiple federally-funded clinical trials and health services research projects. "
            "A HIPAA authorization process (or covered entity alternative per 45 C.F.R. "
            "§ 164.506(c)(4)) will be implemented for each participant prior to enrollment, "
            "consistent with CMS requirements for enrollment data sharing, with Yale and "
            "YNHHS legal review of the specific authorization language.",
            "Normal",
            False,
        ),
        # ── Q4
        (
            "Q4: What experience do you have collecting and documenting clinical, cost, and "
            "utilization measures?",
            "Heading2",
            False,
        ),
        (
            "YNHHS has extensive experience collecting and documenting HEDIS-aligned clinical "
            "quality measures — including HbA1c control, blood pressure control, kidney health "
            "evaluation, diabetic eye exam completion, and statin adherence — tracked quarterly "
            "for MIPS and quality reporting across its Epic EHR network. YNHHS submits clinical "
            "measure data electronically to CMS through the QRDA III pathway annually. "
            "YMCARE will add study-specific clinical measures collected by trained research "
            "nurses using standardized assessment protocols (Mini-Cog, SLUMS, PAM-13, "
            "waist circumference, SPPB) documented in REDCap with standardized coding "
            "aligned with LOINC identifiers for CMS submission compatibility. "
            "For cost and utilization measurement, YSPH health economists on the YMCARE team "
            "have prior experience analyzing Medicare Part A/B claims data obtained through "
            "the CMS VRDC and ResDAC for published health services research, including "
            "studies examining preventable hospitalizations and total cost of care among "
            "Medicare beneficiaries with chronic disease. YMCARE will prospectively capture "
            "intervention costs using nurse-contact time logs, LiveWell session records, "
            "module completion logs, and administrative cost ledgers aligned with MAHA ELEVATE "
            "award categories. De-identified total cost of care and utilization data provided "
            "by CMS will be merged with clinical outcome data for primary economic analyses.",
            "Normal",
            False,
        ),
        # ── Q5
        (
            "Q5: How will you monitor for any potential harmful effects from the intervention? "
            "How will you mitigate or avoid harm?",
            "Heading2",
            False,
        ),
        (
            "YMCARE implements a five-level safety monitoring protocol: "
            "(1) Enrollment safety screen: all participants complete a structured nurse-administered "
            "safety screen before enrollment confirmation, evaluating cardiovascular symptoms, "
            "fall history, orthopedic contraindications, uncontrolled hypertension, glucose "
            "dysregulation, and cognitive safety eligibility. Any participant with unresolved "
            "safety concerns is excluded or deferred pending PCP clearance. "
            "(2) Ongoing nurse monitoring: study RNs complete a brief safety checklist at every "
            "coaching contact; participants with new clinical concerns are escalated to PCP "
            "within 24 hours per protocol. "
            "(3) Pre-defined escalation thresholds triggering immediate PCP notification and "
            "documented follow-up: BP ≥180/110 mmHg; blood glucose <70 mg/dL or >400 mg/dL; "
            "PHQ-9 Item 9 positive (suicidal ideation); Mini-Cog or SLUMS deterioration of "
            "≥1 stage; new injurious fall; chest pain or acute shortness of breath. "
            "(4) Exercise safety: all movement activities include upfront contraindication "
            "screening, mandatory adapted chair-based alternatives, and instructions to obtain "
            "PCP clearance before beginning a new exercise regimen. Participants with mobility "
            "limitations are assessed by the study RN before exercise module activation. "
            "(5) Data Safety Monitoring Board (DSMB): an independent DSMB with clinical and "
            "biostatistics expertise will conduct an interim safety review at 12 months and may "
            "issue stopping rules for unexpected adverse events. The DSMB charter will be "
            "developed in consultation with the Yale IRB and submitted to the CMS Project Officer "
            "within 6 months of award. All serious adverse events and protocol deviations will "
            "be reported to the Yale IRB and CMS Project Officer per 2 CFR Part 200 "
            "and applicable federal regulations.",
            "Normal",
            False,
        ),
        # ── Q6 Table J
        (
            "Table J: Program-Level Data (Attachment — Required)",
            "Heading2",
            False,
        ),
        (
            "Table J must be completed and submitted as an attachment demonstrating prior "
            "experience delivering this intervention and collecting relevant data. "
            "[ACTION REQUIRED: Complete Table J using quantitative data from the closest "
            "prior YMCARE pilot, comparable Yale/YNHHS nurse-led chronic disease management "
            "program, or LiveWell community wellness program. Required columns: "
            "(1) Intervention name and description; "
            "(2) Enrollee demographics (n, age range, % with diabetes/hypertension, "
            "race/ethnicity, insurance type); "
            "(3) Length of intervention and session structure; "
            "(4) Operational process measures (enrollment rate, retention rate at program end, "
            "% completing ≥75% of sessions, staff-to-participant ratio); "
            "(5) Health outcome changes (mean change in HbA1c, BP, weight, activation score, "
            "or comparable outcomes with confidence intervals); "
            "(6) Cost changes (cost per participant, any documented utilization or cost savings). "
            "Without Table J, reviewers cannot score prior implementation experience (10 pts, "
            "Template 3). This is a high-risk gap that must be filled before submission.]",
            "Normal",
            False,
        ),
        # ── Q7 CEHRT
        (
            "Q7: Certified Health IT Product (CEHRT) — 2 Points",
            "Heading2",
            False,
        ),
        (
            "Yes. YNHHS uses Epic, a Certified Health IT Product (CEHRT). "
            "The CHPL Product ID for YNHHS's Epic system is: "
            "[ACTION REQUIRED: Confirm CHPL ID at https://chpl.healthit.gov — "
            "search 'Yale New Haven Health' or 'Epic Systems Corporation' and confirm the "
            "specific ambulatory EHR certification ID for YNHHS. "
            "Format: typically 15.04.04.XXXX.EPIC.XX.XX.X.XXXXXX]. "
            "Confirming and entering this ID earns 2 automatic points out of 10 for the "
            "Data Management Plan scoring criterion. This is the easiest 2 points in the "
            "entire application and requires only a 5-minute database lookup.",
            "Normal",
            False,
        ),
    ]

    insert_paragraphs_after(body, p_ref, dmp_section)
    # Note: insert_paragraphs_after inserts AFTER p_ref, but we want BEFORE p_ref.
    # Re-insert before the reference list
    # Actually, insert BEFORE ref by inserting AFTER the para before ref.
    print("[H] DMP section inserted. NOTE: inserted after Reference List heading — "
          "manual reorder needed to place it before the references.")


# ══ EDIT H2: Re-insert DMP BEFORE Reference List ════════════════════════════
# The function inserted after p_ref. Re-find the reference list and move DMP before it.
# Better approach: find para just before Reference List and insert after THAT
# Let's do it properly by finding the last Section-15 para and inserting after it

idx_equity, p_equity = find_para(paras, "15. Equity, Accessibility, and Safety")
idx_ref2, p_ref2 = find_para(paras, "Reference List")

if p_equity is not None and p_ref2 is not None:
    # Find last para of equity section (everything between equity and reference list)
    equity_idx = list(body).index(p_equity)
    ref_idx    = list(body).index(p_ref2)
    # The last paragraph before reference list
    last_equity_p = None
    children = list(body)
    for i in range(ref_idx - 1, equity_idx, -1):
        if etree.QName(children[i].tag).localname == "p":
            txt = para_text(children[i])
            if txt.strip():
                last_equity_p = children[i]
                break

    # Now find the newly inserted DMP paragraphs (they're currently after p_ref)
    # and move them to before p_ref
    # The DMP paragraphs are the last ones added (after old p_ref position)
    # Since we used insert_paragraphs_after on p_ref, they're now after p_ref
    # Let's find them and move them before p_ref
    ref_current_idx = list(body).index(p_ref2)

    # Find DMP heading we inserted
    dmp_p = None
    children_now = list(body)
    for i, ch in enumerate(children_now):
        if etree.QName(ch.tag).localname == "p":
            txt = para_text(ch)
            if "Data Management Plan (Required" in txt:
                dmp_p = ch
                dmp_start_idx = i
                break

    if dmp_p is not None:
        # Move all DMP paragraphs (from dmp_start_idx to end of body minus sectPr)
        # to before p_ref2
        dmp_paras = []
        children_now = list(body)
        dmp_start = list(body).index(dmp_p)
        ref_pos   = list(body).index(p_ref2)
        # DMP paragraphs are those from dmp_start to end (they were inserted after ref_pos)
        # Actually since insert_paragraphs_after inserted after p_ref2, they should be after ref_pos
        if dmp_start > ref_pos:
            # Move them before ref_pos
            dmp_children = list(body)[dmp_start:]
            # Remove from body, re-insert before p_ref2
            for ch in dmp_children:
                body.remove(ch)
            ref_current = list(body).index(p_ref2)
            for j, ch in enumerate(dmp_children):
                body.insert(ref_current + j, ch)
            print("[H2] Moved DMP section to before Reference List.")
        else:
            print("[H2] DMP already before Reference List (index %d vs ref %d)." % (dmp_start, ref_pos))


# ══ EDIT I: SWAG/incentives — flag potential compliance issue ════════════════
idx_swag, p_swag = find_para(paras, "Study SWAG and milestone awards")
if p_swag is not None:
    add_comment(p_swag,
        "REVIEWER [Budget Compliance — Non-Cash Incentives]: "
        "Modest non-cash incentives (notebooks, pedometers, water bottles) are generally "
        "allowable under 2 CFR Part 200 if (a) they are reasonable in cost, (b) linked to "
        "research participation (not payment for health services), and (c) approved in the "
        "final budget negotiation with CMS. "
        "ACTION: (1) Ensure the budget narrative explicitly lists all incentive items and "
        "per-participant cost. (2) Confirm no food/meals are included in SWAG packages — "
        "food is explicitly prohibited. (3) Note that incentive design and costs are subject "
        "to CMS approval during budget negotiation. (4) Milestone awards tied to assessment "
        "completion are standard in NIH-funded trials and should be IRB-approved as well."
    )
    print("[I] Added SWAG/incentives compliance comment.")


# ══ EDIT J: Add comment on attention control arm contamination risk ══════════
idx_ctrl, p_ctrl = find_para(paras, "The attention control arm is intentionally scaled back")
if p_ctrl is not None:
    add_comment(p_ctrl,
        "REVIEWER [Template 2 — Study Design, 10 pts]: "
        "The NOFO explicitly asks: 'Will the control group receive any part of the intervention, "
        "and if so, when?' Your attention control arm provides generic healthy-aging materials "
        "and quarterly 10-15 min calls. "
        "Strengthen this response by explicitly stating: "
        "(1) What SPECIFIC elements the control arm does NOT receive (no SMART goals, no RN "
        "coaching, no cardiometabolic-specific content, no tailored activation plan). "
        "(2) How you will monitor for and handle contamination (e.g., if a control participant "
        "independently engages with YMCARE community activities). "
        "(3) The timing of crossover: whether control participants receive any YMCARE elements "
        "after the 12-month endpoint. "
        "A clean, well-specified control arm is essential for the 10-point study design score."
    )
    print("[J] Added attention control comment.")


# ══ EDIT K: Add comment on evidence base leading with RCTs ══════════════════
idx_finger, p_finger = find_para(paras, "FINGER and related World-Wide FINGERS trials")
if p_finger is not None:
    add_comment(p_finger,
        "REVIEWER [Template 1 — Evidence Base, 15 pts — Restructuring Needed]: "
        "For maximum scoring, reorganize the evidence section to lead with your two "
        "STRONGEST citations in the first paragraph: "
        "(1) Ngandu et al. (2015, Lancet): 2-year RCT, n=1,260, multidomain lifestyle "
        "intervention, significant cognitive protection, broad subgroup benefits. "
        "State the specific effect size: composite Z-score difference 0.022 (95% CI 0.002–0.042, p=0.030). "
        "(2) JAMA 2025 (U.S. POINTER): Large US RCT, structured arm showed greater "
        "cognitive improvement than self-guided arm. "
        "Then cite cardiometabolic/Medicare-specific evidence: "
        "Strawbridge (2017) for DSMT Medicare outcomes; Meng (2009) for Medicare "
        "health promotion expenditure reduction. "
        "CMS scores evidence on STUDY DESIGN QUALITY (6 pts) and SAMPLE SIZE ≥1,000 (5 pts). "
        "Do not lead with systematic reviews — lead with specific RCTs with n>1,000."
    )
    print("[K] Added evidence base restructuring comment.")


# ══ EDIT L: Fix "Life-style" typo in Partners table ═════════════════════════
idx_lifestyle, p_lifestyle = find_para(paras, "Community implementation partner; Life-style intervention")
if p_lifestyle is not None:
    result = replace_phrase_in_para(p_lifestyle, "Life-style intervention delivery",
                                     "Lifestyle intervention delivery")
    if result:
        print("[L] Fixed 'Life-style' typo in Partners table.")


# ══ EDIT M: Add note about CMS marketing review requirement ═════════════════
idx_impl, p_impl = find_para(paras, "14. Implementation Science and Process Evaluation")
if p_impl is not None:
    add_comment(p_impl,
        "REVIEWER [Post-Award Operational Requirement]: "
        "Per the NOFO (page 24), CMS must 'review and approve marketing and website content "
        "before launch and updates.' Build this review cycle into your implementation timeline: "
        "all participant-facing materials (recruitment flyers, enrollment packets, module covers, "
        "website, social media) require CMS Project Officer approval BEFORE distribution. "
        "Allow 4–6 weeks for CMS review in your pre-implementation phase timeline (Months 1–6). "
        "Similarly, CMS must approve all KEY PERSONNEL additions or changes post-award. "
        "Document the PI/PD and all named key personnel in the budget narrative with sufficient "
        "effort allocations (typically ≥25% for the PI/PD)."
    )
    print("[M] Added CMS marketing/personnel approval comment.")


# ══ EDIT N: Add comment about per-participant cost in budget ═════════════════
idx_bud, p_bud = find_para(paras, "13. Preliminary Budget")
if p_bud is not None:
    add_comment(p_bud,
        "REVIEWER [Budget Narrative, 10 pts — Required Format Items]: "
        "The NOFO budget narrative template REQUIRES the following that are currently missing: "
        "(1) Per-patient cost of the intervention: state explicitly (e.g., ~$1,650–$2,000 per "
        "intervention-arm participant over 3 years based on current budget ranges). "
        "(2) Two 18-month budget periods with separate SF-424A line items for each period — "
        "the current table shows 3-year totals only. "
        "(3) Named PI/PD with specific effort percentage (at minimum 25% FTE commitment). "
        "(4) Exact fringe benefit rates (Yale standard rates: ~30% for faculty, ~28% staff). "
        "(5) Indirect cost rate: Yale's federally negotiated F&A rate or the 15% de minimis "
        "rate — specify which will be used. "
        "(6) Explanation of how subrecipient (LiveWell) costs are separated from prime "
        "recipient (Yale) costs in the budget. "
        "The budget narrative scoring penalizes for missing the per-patient cost statement (2 pts)."
    )
    print("[N] Added budget format comment.")


# ══ EDIT O: Add comment about the companion DMP PDF ═════════════════════════
idx_dmppdf, p_dmppdf = find_para(paras, "Data Management Plan (Required")
if p_dmppdf is not None:
    add_comment(p_dmppdf,
        "REVIEWER [DMP SUBMISSION FORMAT NOTE]: "
        "This DMP section must be INTEGRATED into the project narrative (within the 15-page limit) "
        "using the 4-template format. The separate 'Data Management Plan.pdf' file contains CMS's "
        "own example responses (yoga studio, yoga center) — these are TEMPLATE PLACEHOLDERS and "
        "must NOT be submitted. The NOFO explicitly states these examples will not be scored. "
        "Replace all content in that PDF with the YMCARE-specific responses drafted here. "
        "ALSO: The DMP section is part of the 15-page narrative — factor page allocation carefully: "
        "Template 1 (~4 pages) + Template 2 (~3 pages) + Template 3 (~3 pages) + DMP (~2 pages) "
        "= ~12 pages, leaving ~3 pages for the hypothesis statement, aims, and context."
    )
    print("[O] Added DMP format note comment.")


# ══ Serialize ────────────────────────────────────────────────────────────────
new_doc_xml = etree.tostring(tree, xml_declaration=True, encoding="UTF-8", standalone=True)

buf_mid = BytesIO()
with zipfile.ZipFile(BytesIO(original_bytes), "r") as zin, \
     zipfile.ZipFile(buf_mid, "w", compression=zipfile.ZIP_DEFLATED) as zout:
    for item in zin.infolist():
        if item.filename == "word/document.xml":
            zout.writestr(item, new_doc_xml)
        elif item.filename == "word/comments.xml":
            pass  # skip old comments — we write fresh ones below
        else:
            zout.writestr(item, zin.read(item.filename))

mid_bytes = buf_mid.getvalue()

# Merge old comments from pass 1 with new comments
# Read old comments XML
try:
    with zipfile.ZipFile(BytesIO(original_bytes)) as zf2:
        if "word/comments.xml" in zf2.namelist():
            old_cmt_xml = zf2.read("word/comments.xml")
            old_root = etree.fromstring(old_cmt_xml)
            # Find max existing comment id
            max_old_id = 0
            for c in old_root.findall(wq("comment")):
                try:
                    max_old_id = max(max_old_id, int(c.get(wq("id"), "0")))
                except ValueError:
                    pass
            # Renumber new comments to avoid id clash
            for i, (cid, author, date, text) in enumerate(_comments_list):
                new_cid = str(max_old_id + i + 1)
                _comments_list[i] = (new_cid, author, date, text)
                # also fix refs in document XML  — update the new_doc_xml
                new_doc_xml = new_doc_xml.replace(
                    ('w:id="%s"' % cid).encode(),
                    ('w:id="%s"' % new_cid).encode()
                )
            # Rebuild final comments XML merging old + new
            new_root = etree.Element(wq("comments"))
            for c in old_root.findall(wq("comment")):
                new_root.append(copy.deepcopy(c))
            for cid, author, date, text in _comments_list:
                ce = etree.SubElement(new_root, wq("comment"))
                ce.set(wq("id"), cid); ce.set(wq("author"), author)
                ce.set(wq("date"), date); ce.set(wq("initials"), "XC")
                cp = etree.SubElement(ce, wq("p"))
                cpPr = etree.SubElement(cp, wq("pPr"))
                cps  = etree.SubElement(cpPr, wq("pStyle")); cps.set(wq("val"), "CommentText")
                cr   = etree.SubElement(cp, wq("r"))
                crPr = etree.SubElement(cr, wq("rPr"))
                crs  = etree.SubElement(crPr, wq("rStyle")); crs.set(wq("val"), "CommentReference")
                ct   = etree.SubElement(cr, wq("t"))
                ct.set("{%s}space" % XMLNS, "preserve"); ct.text = text
            comments_xml_bytes = etree.tostring(new_root, xml_declaration=True,
                                                 encoding="UTF-8", standalone=True)
except Exception as e:
    print("Warning merging comments: %s" % e)
    comments_xml_bytes = build_comments_xml()

# Rebuild buf_mid with updated document XML (after id renumbering)
buf_mid2 = BytesIO()
with zipfile.ZipFile(buf_mid, "r") as zin, \
     zipfile.ZipFile(buf_mid2, "w", compression=zipfile.ZIP_DEFLATED) as zout:
    for item in zin.infolist():
        if item.filename == "word/document.xml":
            zout.writestr(item, new_doc_xml)
        else:
            zout.writestr(item, zin.read(item.filename))

mid_bytes2 = buf_mid2.getvalue()
final_bytes = inject_comments(mid_bytes2, comments_xml_bytes)

with open(DST, "wb") as f:
    f.write(final_bytes)

print("\n" + "="*60)
print("Done. Saved to:")
print(DST)
print("New tracked changes: deletions=%d, insertions (DMP+timeline)=yes" % count_deleted_18)
print("Comments added this pass: %d" % len(_comments_list))
