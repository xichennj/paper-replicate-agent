"""
Apply tracked-change edits to the temperature-diet manuscript for ReStat submission.

Approach: make changes in a temp copy, then use Word's Compare() to produce
a proper tracked-changes document -- identical to the gen_tracked_pfl.py workflow.

31 edits addressing:
  Grammar (data/comes, data/captures, test if vs test whether)
  Filler phrases (novel, Based on this background, it is important to note that)
  Antecedent clarity (they = approach or bins? -> name CDD and HDD)
  Formality (diseases like -> such as; roughly calculate; reveal -> document)
  Redundancies (of interest, include but are not limited to, time-invariant clause)
  Prose tightening across Abstract, Intro, Data, Empirics, Results, Conclusion
"""

import shutil, win32com.client, os

INPUT_PATH  = r'C:\Users\xc77\Dropbox\Claude\Off-comfort Temperatures Promote High-Fat Diets260428.docx'
EDITED_PATH = r'C:\Users\xc77\Dropbox\Claude\Off-comfort Temperatures Promote High-Fat Diets260428_edited_temp.docx'
OUTPUT_PATH = r'C:\Users\xc77\Dropbox\Claude\Off-comfort Temperatures Promote High-Fat Diets260428_tracked.docx'

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
        tag = label or old[:60]
        status = "OK  " if found else "MISS"
        print(status + ": " + tag)
        return found

    changes = [

        # -- Abstract ----------------------------------------------------------

        # 1. Remove "novel" -- top journals routinely ask authors to remove this
        (
            "We provide novel evidence of how short-term exposures to off-comfort temperatures",
            "We provide evidence of how short-term exposures to off-comfort temperatures",
            "Abstract [1]: remove 'novel'",
        ),

        # 2. "The influences of" -> "The effects of" (more precise econ register)
        #    Also tighten "which indicates adaptive capacity" -> "consistent with adaptive capacity"
        (
            "The influences of temperature are larger for rural and lower-income households "
            "and smaller for households that own cooling and heating appliances, which indicates adaptive capacity.",
            "The effects of temperature are larger for rural and lower-income households "
            "and smaller for households with cooling and heating appliances, consistent with adaptive capacity.",
            "Abstract [2]: influences->effects; tighten adaptive-capacity clause",
        ),

        # -- Introduction ------------------------------------------------------

        # 3. Remove the heavy nominalization "enters the formation of"
        (
            "realized nutrition is the input that enters the formation of health capital",
            "realized nutrition is the direct input to health capital formation",
            "Intro [3]: tighten 'enters the formation of'",
        ),

        # -- Section 3: Data ---------------------------------------------------

        # 4. "nutrition intake" -> "dietary intake" (standard econ/epi term)
        (
            "effects of off-comfort temperatures on nutrition intake.",
            "effects of off-comfort temperatures on dietary intake.",
            "Data [4]: 'nutrition intake' -> 'dietary intake'",
        ),

        # 5. "further enriches our analysis" is vague; make it concrete
        (
            "In addition, it contains information on demographics, socioeconomic status, "
            "and health outcomes, which further enriches our analysis.",
            "It also contains information on demographics, socioeconomic status, "
            "and health outcomes, which we exploit in heterogeneity analyses.",
            "Data [5]: remove 'further enriches'; be specific",
        ),

        # 6. Subject-verb agreement: "data comes" -> "data come"
        (
            "The meteorological data comes from the China Meteorological Data Sharing Service (CMDSS) system.",
            "The meteorological data come from the China Meteorological Data Sharing Service (CMDSS) system.",
            "Data [6]: 'data comes' -> 'data come'",
        ),

        # 7. "using the distance...as weights" is redundant after "inverse-distance weighted average"
        (
            "We construct the daily county-level weather variables by taking an inverse-distance "
            "weighted average of all the stations located within a 50-mile radius of the county "
            "centroid, using the distance between the centroid and stations as weights.",
            "We construct daily county-level weather variables as inverse-distance-weighted "
            "averages of all stations within a 50-mile radius of the county centroid.",
            "Data [7]: remove redundant 'using the distance...as weights'",
        ),

        # 8. Fix dangling antecedent "they": unclear whether they = approach or bins
        (
            "than conventional temperature bins, because they measure not only "
            "whether the daily average temperature crosses a threshold but also by how much "
            "it exceeds or falls short of it.",
            "than conventional temperature bins, since CDD and HDD measure not only "
            "whether temperature crosses a threshold but also by how much.",
            "Data [8]: fix dangling 'they'; name CDD and HDD explicitly",
        ),

        # 9. "providing two important messages" is informal
        (
            "Table A2 shows the summary statistics of the variables used in this paper, "
            "providing two important messages.",
            "Table A2 presents summary statistics for the key variables, with two patterns worth noting.",
            "Data [9]: 'providing two important messages' -> professional phrasing",
        ),

        # 10. Tighten the sentence about children in the sample
        (
            "Considering the inclusion of children in the sample, the average values for the "
            "entire sample may exhibit slight deviations from those of the population aged "
            "18 years and older.",
            "Because the sample includes children, mean values deviate slightly from adult-only benchmarks.",
            "Data [10]: tighten children/deviation sentence",
        ),

        # -- Section 4: Empirical Strategies -----------------------------------

        # 11. Remove throat-clearing "Based on this background"
        (
            "Based on this background, we estimate the following regression:",
            "We estimate the following regression:",
            "Empirics [11]: remove 'Based on this background'",
        ),

        # 12. "primary outcomes of interest" -- "of interest" is redundant
        (
            "as our primary outcomes of interest.",
            "as our primary outcomes.",
            "Empirics [12]: remove redundant 'of interest'",
        ),

        # 13. "that do not change across survey waves" is redundant with "time-invariant"
        #     Use only the tail of the sentence to avoid the embedded field-code in ()
        (
            "inherent dietary preferences, or cultural eating habits that do not change across survey waves.",
            "dietary preferences, and cultural eating habits.",
            "Empirics [13]: remove 'that do not change across survey waves' (redundant)",
        ),

        # -- Section 5.1: Baseline Results -------------------------------------

        # 14. Subject-verb: "presents" -> "present" (two items listed)
        (
            "Figure 1(a) (and Table A3) presents the effects of short-term temperature "
            "fluctuations on dietary composition and macronutrient intake estimated by equation (1).",
            "Figure 1(a) (and Table A3) present the effects of short-term temperature "
            "fluctuations on dietary composition and macronutrient intake estimated by equation (1).",
            "Results 5.1 [14]: 'presents' -> 'present'",
        ),

        # 15. Recast "not captured by caloric measures alone" with an em-dash for flow
        (
            "The diet, therefore, becomes compositionally riskier even as total consumption falls, "
            "representing a deterioration in dietary quality that is not captured by caloric measures alone.",
            "The diet therefore becomes compositionally riskier even as total consumption falls—"
            "a deterioration in dietary quality that caloric measures alone would miss.",
            "Results 5.1 [15]: tighten 'not captured' clause; add em-dash",
        ),

        # 16. Recast the fat-insignificance clause for clarity
        (
            "while fat intake changes by only 0.151% and remains statistically insignificant.",
            "while the change in fat intake (0.151%) is statistically insignificant.",
            "Results 5.1 [16]: recast fat-insignificance clause",
        ),

        # 17. "opposite but complementary" is contradictory; channels are opposite, not complementary
        (
            "Cold operates through an opposite but complementary channel.",
            "Cold operates through an opposite channel.",
            "Results 5.1 [17]: remove 'but complementary'",
        ),

        # -- Section 5.1: BMI correlation paragraph ----------------------------

        # 18. "diseases like" -> "conditions such as" (formal register)
        (
            "particularly obesity, which is associated with severe diseases like cardiovascular "
            "conditions, diabetes, and cancers",
            "particularly obesity, which is associated with severe conditions such as "
            "cardiovascular disease, diabetes, and cancer",
            "Results 5.1 [18]: 'diseases like' -> 'conditions such as'",
        ),

        # 19. "OLS approach to reveal the correlation" -- reveal is informal
        (
            "We use the OLS approach to reveal the correlation of Body Mass Index (BMI) "
            "with energy intake and dietary structure",
            "We use OLS to document the association between Body Mass Index (BMI), "
            "energy intake, and dietary composition",
            "Results 5.1 [19]: 'reveal correlation' -> 'document association'",
        ),

        # 20. "The results reveal two patterns" -- reveal is informal
        (
            "The results reveal two patterns.",
            "Table 1 reports two patterns.",
            "Results 5.1 [20]: 'results reveal' -> 'Table 1 reports'",
        ),

        # 21. Remove filler "it is important to note that"
        (
            "However, it is important to note that our empirical strategy identifies the "
            "deterioration of short-term dietary composition.",
            "Our empirical strategy identifies short-term deterioration in dietary composition.",
            "Results 5.1 [21]: remove 'it is important to note that'",
        ),

        # -- Section 5.2: Food Groups ------------------------------------------

        # 22. "clearer pattern identification" is vague
        (
            "We simplify these items into six aggregated groups for clearer pattern identification.",
            "We aggregate these items into six groups for cleaner presentation.",
            "Results 5.2 [22]: 'clearer pattern identification' -> 'cleaner presentation'",
        ),

        # 23. Merge and clarify the confusing zero-values/log sentence
        (
            "Each group of food and condiments has zero values (see Table A2) and is small. "
            "Thus, we use the absolute amounts of consumption for each group as the outcome "
            "variable instead of the logarithm form after adding one.",
            "Because each food and condiment group contains zero values (Table A2), we use "
            "absolute consumption levels rather than logarithms as outcome variables.",
            "Results 5.2 [23]: merge and clarify zero-values/log sentence",
        ),

        # 24. "roughly calculate" undermines the analysis
        (
            "we roughly calculate the proportions of three macronutrients supplied by the "
            "four major food groups",
            "we calculate the approximate macronutrient composition of the four major food groups",
            "Results 5.2 [24]: 'roughly calculate' -> 'calculate the approximate'",
        ),

        # -- Section 5.3: Mechanisms -------------------------------------------

        # 25. "changes in time use and activity" -- "and activity" is vague; trim
        (
            "They combine several channels that the design does not separately identify, "
            "such as physiological appetite regulation and changes in time use and activity.",
            "They combine several channels that the design does not separately identify, "
            "including physiological appetite regulation and changes in time use.",
            "Results 5.3 [25]: trim 'and activity' from mechanism list",
        ),

        # 26. Tighten parenthetical cross-reference
        (
            "(for details about these variables, see Appendix A).",
            "(see Appendix A for details).",
            "Results 5.3 [26]: tighten cross-reference parenthetical",
        ),

        # -- Section 5.4: Heterogeneity and Adaptation -------------------------

        # 27. "include, but are not limited to" is legalese; unnecessary in academic prose
        (
            "These resources include, but are not limited to, better access to cooling "
            "and heating technologies (see Table A11).",
            "These resources include better access to cooling and heating technologies (Table A11).",
            "Results 5.4 [27]: remove legalese 'but are not limited to'",
        ),

        # 28. Subject-verb agreement: "our data captures" -> "our data capture"
        (
            "Notably, our data captures appliance ownership rather than actual usage.",
            "Notably, our data capture appliance ownership rather than actual usage.",
            "Results 5.4 [28]: 'data captures' -> 'data capture'",
        ),

        # -- Section 6: Conclusion ---------------------------------------------

        # 29. "provides evidence that" is weaker than "documents that" for an established result
        (
            "this paper provides evidence that short-term temperature deviations from comfort "
            "systematically shift dietary composition toward higher fat intake.",
            "this paper documents that short-term temperature deviations from comfort "
            "systematically shift dietary composition toward higher fat intake.",
            "Conclusion [29]: 'provides evidence that' -> 'documents that'",
        ),

        # 30. "test if" -> "test whether" (standard formal usage in economics)
        (
            "While we cannot test if short-run absolute fat increases are fully offset by thermogenic expenditure",
            "While we cannot test whether short-run absolute fat increases are fully offset by thermogenic expenditure",
            "Conclusion [30]: 'test if' -> 'test whether'",
        ),

        # 31. "scenario where" -> "scenario in which" (formal relative clause)
        (
            "these projections suggest a plausible scenario where the dietary consequences",
            "these projections suggest a plausible scenario in which the dietary consequences",
            "Conclusion [31]: 'scenario where' -> 'scenario in which'",
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
