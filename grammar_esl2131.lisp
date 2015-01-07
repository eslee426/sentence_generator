(defparameter *grammar*
  '((sentence -> (nounPhrase verbPhrase)
                 (nounPhrase-Plural verbPhrase-Plural)
                 (nounPhrase verbPhrase-Past)
                 (sentence CoordinatingConjunction sentence)
                 (SubordinatingConjunction sentence verbPhrase)
                 (expletivePhrase nounPhrase-Plural))

    (nounPhrase -> (adjectivePhrase AbstractNoun)
                   (Article adjectivePhrase Noun)
                   (AbstractNoun)
                   (Article Noun)
                   (PreAdjective adjectivePhrase Noun)
                   (nounPhrase prepositionalPhrase)
                   (Article NounAdjective Noun)
                   (NounAdjective Noun))

    (nounPhrase-Plural -> (adjectivePhrase NounPlural)
                          (NounPlural)
                          (nounPhrase-Plural prepositionalPhrase)
                          (adjectivePhrase NounAdjective NounPlural)
                          (nounPhrase-Plural relativeClause)
                          (nounPhrase-Plural CoordinatingConjunction nounPhrase)
                          (Article adjectivePhrase NounPlural))

    (adjectivePhrase -> (Adjective) 
                        (Adjective adjectivePhrase)
                        (adjectivePhrase CoordinatingConjunction adjectivePhrase))

    (prepositionalPhrase -> (Preposition nounPhrase)
                            (Preposition nounPhrase-Plural)
                            (Prep-Modifier Preposition nounPhrase-Plural))

    (verbPhrase -> (verbPhrase prepositionalPhrase)
                   (verbClauseTransitive nounPhrase)
                   (verbClauseIntransitive)
                   (beVerbClause)
                   (verbPhrase-Future)
                   (verbPhrase-Future nounPhrase))

    (verbPhrase-Past -> (verbPhrase-Past prepositionalPhrase)
                        (verbClauseTransitive-Past nounPhrase)
                        (verbClauseIntransitive-Past))

    (verbPhrase-Plural -> (verbPhrase-Plural prepositionalPhrase)
                          (verbClauseTransitive-Plural nounPhrase)
                          (verbClauseTransitive-Plural nounPhrase-Plural)
                          (beVerbClause))

    (verbClauseTransitive -> (VerbTrans) 
                             (HelpingVerb VerbTrans-Plural)
                             (HelpingVerb Adverb VerbTrans))

    (verbClauseTransitive-Plural -> (VerbTrans-Plural) 
                                    (HelpingVerb VerbTrans-Plural)
                                    (HelpingVerb Adverb VerbTrans-Plural))

    (verbClauseTransitive-Past -> (VerbTrans-Past)
                                  (HelpingVerb VerbTrans-Plural))

    (verbClauseIntransitive -> (VerbIntrans)
                               (HelpingVerb VerbIntrans)
                               (HelpingVerb VerbIntrans))

    (verbClauseIntransitive-Past -> (VerbIntrans-Past))

    (beVerbClause -> (HelpingVerb Adverb BeVerb Adjective)
                     (HelpingVerb BeVerb Adjective infinitivePhrase))

    (verbPhrase-Future -> (HelpingVerb BeVerb Verb-Past)
                          (HelpingVerb Adverb VerbTrans-Plural))

    (infinitivePhrase -> (Infinitive VerbTrans-Plural Object)
                         (Infinitive VerbTrans Adjective))


    (expletivePhrase -> (ExpletivePronoun ExpletiveVerb))

    (relativeClause -> (RelativePronoun VerbTrans-Past infinitivePhrase))

    (AbstractNoun -> education polarization)
    (Adjective -> higher prevailing flawed desired naked visible amateur able few solid several well business professional happy sad)
    (Adverb -> hardly not always easily happily briefly really)
    (Article -> the a)
    (BeVerb -> be)
    (CoordinatingConjunction -> and or but)
    (ExpletivePronoun -> there)
    (ExpletiveVerb -> were are)
    (Infinitive -> to)
    (HelpingVerb -> must will should would might)
    (Noun -> narrative strategy intent asteroid eye prosecution case penalty prosecution care cat dog house man woman hedgehog)
    (NounAdjective -> death job health)
    (NounPlural -> astronomers telescopes weeks gains industries services cats dogs men women children)
    (Object -> it)
    (PreAdjective -> this that)
    (Prep-Modifier -> such)
    (Preposition -> of to in to with as)
    (RelativePronoun -> that)
    (SubordinatingConjunction -> whether although because if)
    (VerbIntrans -> laugh cry run)
    (VerbIntrans-Past -> laughed cried fell ran)
    (Verb-Past -> decided)
    (VerbTrans -> counters achieves seeks pay reads loves)
    (VerbTrans-Past -> tend achieved played read wrote)
    (VerbTrans-Plural -> counter achieve seek read love see))
  "A grammar for a trivial subset of English.")

(defun targeted-sentence (rules)
  (apply-rules rules nil)
)

;list of rules using DFS order
;(THE MAN LIKED A WOMAN)
;(defparameter rules0 '((sentence 0) (noun-phrase 0) (Article 0) (Noun 0) (verb-phrase 0) (Verb 3) (noun-phrase 0) (Article 1) (Noun 2)))
(defparameter rules1 '((sentence 0) (nounPhrase 0) (adjectivePhrase 0) (Adjective 0) (AbstractNoun 0) 
                       (verbPhrase 0) (verbPhrase 1) (verbClauseTransitive 1) (HelpingVerb 0) (VerbTrans-Plural 0) (nounPhrase 1) (Article 0) (adjectivePhrase 0) (Adjective 1) (Noun 0) (prepositionalPhrase 0) (Preposition 0) (nounPhrase 2) (AbstractNoun 1)))
(defparameter rules2 '((sentence 0) (nounPhrase 4) (PreAdjective 0) (adjectivePhrase 0) (Adjective 2) (Noun 1) 
                       (verbPhrase 5) (verbPhrase-Future 1) (HelpingVerb 1) (Adverb 0) (VerbTrans-Plural 1) (nounPhrase 1) (Article 0) (adjectivePhrase 0) (Adjective 3) (Noun 2)))
(defparameter rules3 '((sentence 5) (expletivePhrase 0) (ExpletivePronoun 0) (ExpletiveVerb 0) (nounPhrase-Plural 2) (nounPhrase-Plural 4) (nounPhrase-Plural 2) (nounPhrase-Plural 3) (adjectivePhrase 0) (Adjective 9) (NounAdjective 1) (NounPlural 3)
                       (prepositionalPhrase 1) (Preposition 2) (nounPhrase-Plural 0) (adjectivePhrase 0) (Adjective 10) (NounPlural 4) (relativeClause 0) (RelativePronoun 0) (VerbTrans-Past 0) (infinitivePhrase 1) (Infinitive 0) (VerbTrans 3) (Adjective 11)
                       (prepositionalPhrase 2) (Prep-Modifier 0) (Preposition 5) (nounPhrase-Plural 5) (nounPhrase-Plural 0) (adjectivePhrase 2) (adjectivePhrase 0) (Adjective 12) (CoordinatingConjunction 0) (adjectivePhrase 0) (Adjective 13) (NounPlural 5)
                       (CoordinatingConjunction 0) (nounPhrase 7) (NounAdjective 2) (Noun 9)))
(defparameter rules4 '((sentence 3) (sentence 0) (nounPhrase 3) (Article 0) (Noun 3) 
                       (verbPhrase 0) (verbPhrase 3) (beVerbClause 0) (HelpingVerb 1) (Adverb 1) (BeVerb 0) (Adjective 5) (prepositionalPhrase 0) (Preposition 3) (nounPhrase 1) (Article 0) (adjectivePhrase 0) (Adjective 4) (Noun 4)
                       (CoordinatingConjunction 2) (sentence 1) (nounPhrase-Plural 0) (adjectivePhrase 0) (Adjective 6) (NounPlural 0)  
                       (verbPhrase-Plural 0) (verbPhrase-Plural 3) (beVerbClause 1) (HelpingVerb 2) (BeVerb 0) (Adjective 7) (infinitivePhrase 0) (Infinitive 0) (VerbTrans-Plural 5) (Object 0) (prepositionalPhrase 1) (Preposition 4) (nounPhrase-Plural 1) (NounPlural 1)))
(defparameter rules5 '((sentence 4) (SubordinatingConjunction 0) (sentence 0) (nounPhrase 3) (Article 0) (Noun 8) 
                       (verbPhrase 1) (verbClauseTransitive 1) (HelpingVerb 1) (VerbTrans-Plural 2) (nounPhrase 5) (nounPhrase 6) (Article 0) (NounAdjective 0) (Noun 7) (prepositionalPhrase 0) (Preposition 2) (nounPhrase 3) (Article 0) (Noun 6)
                       (verbPhrase 0) (verbPhrase 4) (verbPhrase-Future 0) (HelpingVerb 1) (BeVerb 0) (Verb-Past 0) (prepositionalPhrase 1) (Preposition 2) (nounPhrase-Plural 6) (Article 1) (adjectivePhrase 0) (Adjective 8) (NounPlural 2)))

;take a set of rules and the current sentence that has been generated so far
;here is what happens for the above example:
;when the first rule is called, the current sentence is empty and is rewritten to (noun-phrase verb-phrase)
;second rule: (Article Noun verb-phrase)
;3: (THE Noun verb-phrase)
;4: (THE MAN verb-phrase)
;5: (THE MAN Verb noun-phrase)
;6: (THE MAN LIKED noun-phrase)
;7: (THE MAN LIKED Article Noun)
;8: (THE MAN LIKED A Noun)
;9: (THE MAN LIKED A WOMAN)

(defun apply-rules (rules sentence)
  (cond 
    ((null rules) sentence)
    ((null sentence) (apply-rules (rest rules) (elt (rule-rhs (assoc (car (car rules)) *grammar*)) (second (car rules)))))
    (t (let ((rule-to-rewrite (car (car rules))) (new-rule (elt (rule-rhs (assoc (car (car rules)) *grammar*)) (second (car rules)))))
      (apply-rules (rest rules) (rewrite-sentence nil sentence rule-to-rewrite new-rule)))))) 

;simply rewrites a sentence replacing the first occurence of the variable "rule-to-rewrite" in "sentence-next" by the symbols in "new-rule" 
;example: (rewrite-sentence nil '(THE MAN verb-phrase) 'verb-phrase '(Verb noun-phrase))
;returns (THE MAN Verb noun-phrase)
(defun rewrite-sentence (sentence-pre sentence-next rule-to-rewrite new-rule)
    (cond ((null sentence-next) sentence-pre)
    (t 
      (if (equal (car sentence-next) rule-to-rewrite)
      (append (append sentence-pre (if (listp new-rule) new-rule (list new-rule))) (rest sentence-next))
      (rewrite-sentence (append sentence-pre (list (car sentence-next))) (rest sentence-next) rule-to-rewrite new-rule)))))      

(defun random-elt (list)
  (elt list
       (random (length list))))

(defun random-sentence (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'random-sentence phrase))
        ((rewrites phrase)
         (random-sentence (random-elt (rewrites phrase))))
        (t (list phrase))))

(defun generate-tree (phrase)
  "Generate a random sentence or phrase,
  with a complete parse tree."
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase
               (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))

(defun rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(random-sentence 'sentence)

; used to generate random sentences and write to file - given by TA
(defun write-to-file (sentence)
  (with-open-file (str "q1_esl2131.txt"
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (format str sentence)))

; used to generate random sentences and write to file - given by TA
(defun run2 ()
  (let ((sent (random-sentence 'sentence)))
    (write-to-file (if (validp sent) (format nil "+ ~S ~%" sent) (format nil "- ~S ~%" sent)))))

; used to generate random sentences and write to file - given by TA
(defun loop-run (N)
  (loop for i from 1 to N do (run2)))

; Checks if sentences are valid
(defun validp (sent)
  (setq length (validate-length sent))
  (setq duplicate (validate-duplicate-words sent))
  (setq subcon (validate-subordinating-conjunction sent))
  (setq art-count (validate-article-count sent))
  (and length duplicate subcon art-count))

; genreates a valid sentence = accepted under rules generated
(defun generateValid (nonTerm)
  (setq valid NIL)
  (loop while (not valid) do

    ; generate sentence and check if its valid
    (setq sent (random-sentence nonTerm) )
      (if (validp sent) (setq valid t)))
  (setq return sent))

; ensures that the length of sentence is less than 40 words
(defun validate-length (sent)
  (<= (length sent) 40))

; ensures that words do not duplicate alot - unless they are in the list of okay words
(defun validate-duplicate-words (sent)
 ; creates a list of words that can be repeated
 (setq okay-repeats (rewrites 'Article))
 (setq okay-repeats (append okay-repeats (rewrites 'BeVerb)))
 (setq okay-repeats (append okay-repeats (rewrites 'CoordinatingConjunction)))
 (setq okay-repeats (append okay-repeats (rewrites 'ExpletivePronoun)))
 (setq okay-repeats (append okay-repeats (rewrites 'ExpletiveVerb)))
 (setq okay-repeats (append okay-repeats (rewrites 'Preposition)))

 ; repeated word tracker
 (setq found NIL)

 (loop while (and (> (length sent) 1) (not found) ) do
   (setq current (first sent))

   ; if word is repeated
   (if (not (member current okay-repeats))
     ; check against okay words
     (if (member current (cdr sent))
       (setq found t)))
   (setq sent (cdr sent)))

  (not found))

; ensures that there are no two subordinating conjunctions in a row
(defun validate-subordinating-conjunction (sent)
  (setq sub-con (rewrites 'SubordinatingConjunction))

  (setq repeat NIL)
  (loop while (and (not repeat) (>= (length sent) 2)) do
    (setq current (first sent))
    (setq next (second sent))

    ; checks if consecutive words are both subordinating conjunctions
    (if (and (member current sub-con) (member next sub-con))
      (setq repeat t))
    (setq sent (cdr sent)))
  (not repeat))

; ensures that the sentnece does not have more than 7 articles or coordinating conjunctions
(defun validate-article-count (sent)
  ; list of okay-articles
  (setq okay-art-con(rewrites 'Article))
  (setq okay-art-con (append okay-art-con (rewrites 'CoordinatingConjunction)))(lo)
  (setq count 0)

  ; loop through words in sentence
  (loop while (>= (length sent) 1) do
    (setq current (first sent))
    (if (member current okay-art-con)
     (setq count (+ 1 count)))
     (setq sent (cdr sent)))

  (< count 7))
