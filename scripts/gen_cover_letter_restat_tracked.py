"""
Apply tracked-change edits to the ReStat cover letter.

Uses Word's Compare() to produce proper tracked changes,
identical to the gen_tracked_pfl.py / gen_diet_temp_tracked.py workflow.

Edits:
  1.  Fill in journal name placeholder (xxxx -> Review of Economics and Statistics)
  2.  Fill in date placeholder (April xx -> May 14)
  3.  Grammar: "physiological mechanism" -> "physiological mechanisms"
  4.  Grammar: comma creates broken list in "climate change, more frequent..."
  5.  Style: "Documenting" participial -> "By documenting" (clearer agent)
  6.  Grammar: "contains 6190 words, includes three figures, and one table"
       broken list -> "contains 6,190 words, three figures, and one table"
  7.  Style: "it highlights" ambiguous antecedent -> "our study presents"
  8.  Style: "wide relevance" -> "broad relevance" (more common academic phrase)
"""

import shutil, win32com.client, os

INPUT_PATH  = r'C:\Users\xc77\Dropbox\Claude\Cover letter.docx'
EDITED_PATH = r'C:\Users\xc77\Dropbox\Claude\Cover letter_edited_temp.docx'
OUTPUT_PATH = r'C:\Users\xc77\Dropbox\Claude\Cover letter_tracked.docx'

shutil.copy2(INPUT_PATH, EDITED_PATH)
print("Copied original to temp.")

word = win32com.client.Dispatch('Word.Application')
word.Visible = False

try:
    doc = word.Documents.Open(os.path.abspath(EDITED_PATH))
    doc.TrackRevisions = False

    def fr(old, new, label=""):
        rng = doc.Content
        rng.Find.ClearFormatting()
        rng.Find.Replacement.ClearFormatting()
        found = rng.Find.Execute(
            FindText=old,
            ReplaceWith=new,
            Replace=2,           # wdReplaceAll
            MatchCase=True,
            MatchWholeWord=False,
            MatchWildcards=False,
            Forward=True,
            Wrap=1
        )
        status = "OK  " if found else "MISS"
        print(status + ": " + (label or old[:70]))
        return found

    changes = [

        # 1. Fill in date placeholder
        (
            "April xx, 2026",
            "May 14, 2026",
            "[1] Fill in date: 'April xx' -> 'May 14'",
        ),

        # 2. Fill in journal name placeholder
        (
            "for consideration as a Short Paper in xxxx.",
            "for consideration as a Short Paper in the Review of Economics and Statistics.",
            "[2] Fill in journal name: 'xxxx' -> 'Review of Economics and Statistics'",
        ),

        # 3. Grammar: "physiological mechanism" should be plural
        (
            "through physiological mechanism rather than physical activity.",
            "through physiological mechanisms rather than physical activity.",
            "[3] Grammar: 'mechanism' -> 'mechanisms'",
        ),

        # 4. Grammar: comma creates a broken list ("climate change, more frequent...")
        #    Fix: use "and" to join the two noun phrases
        (
            "linking climate change, more frequent extreme temperatures to human capital formation",
            "linking climate change and more frequent extreme temperatures to human capital formation",
            "[4] Grammar: comma -> 'and' in 'climate change, more frequent...'",
        ),

        # 5. Style: bare "Documenting" participial is abrupt; "By documenting" is cleaner
        (
            "Documenting a demand-side channel through which climate affects nutrition,",
            "By documenting a demand-side channel through which climate affects nutrition,",
            "[5] Style: 'Documenting' -> 'By documenting'",
        ),

        # 6. Grammar: "contains 6190 words, includes three figures, and one table"
        #    "includes" turns the sentence into a run-on; also add comma to 6190
        (
            "The main text of our manuscript contains 6190 words, includes three figures, and one table,",
            "The main text of our manuscript contains 6,190 words, three figures, and one table,",
            "[6] Grammar: fix broken list; add comma to 6190",
        ),

        # 7-8. Style: "it highlights a clear empirical finding with wide relevance"
        #      "it" is ambiguous (refers to the format, not the study)
        #      "wide" -> "broad" is more natural in academic prose
        (
            "as it highlights a clear empirical finding with wide relevance in a concise manner.",
            "as our study presents a clear empirical finding with broad relevance in a concise manner.",
            "[7-8] Style: fix ambiguous 'it'; 'wide' -> 'broad'",
        ),

    ]

    print("\nApplying " + str(len(changes)) + " changes...\n")
    for old, new, label in changes:
        fr(old, new, label)

    doc.Save()
    doc.Close(False)
    print("\nEdited file saved.")

    # Compare original vs edited to produce a proper tracked-changes document
    doc_orig = word.Documents.Open(os.path.abspath(INPUT_PATH))
    doc_orig.Compare(
        Name=os.path.abspath(EDITED_PATH),
        AuthorName="Xi Chen",
        CompareTarget=0,            # wdCompareTargetNew
        DetectFormatChanges=False,
        IgnoreAllComparisonWarnings=True
    )
    compared = word.ActiveDocument
    compared.SaveAs2(os.path.abspath(OUTPUT_PATH))
    compared.Close(False)
    doc_orig.Close(False)
    print("Track-changes file saved to: " + OUTPUT_PATH)

finally:
    word.Quit()
