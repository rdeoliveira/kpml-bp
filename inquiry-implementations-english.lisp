(in-package :penman)

;;; Inquiry implementations. Mostly drawn from the inquiry implementations
;;; accompanying the Penman91 release of Nigel. Extensions and changes 
;;; include those necessary for the merged upper model and the inquiries
;;; necessary for controlling morphological systems. 
;;;
;;; Current inquiry implementations are used by both English and Dutch.
;;;
;;; General cleaning up has been begun.
;;;
;;; Last Update:
;;; 12.11.96: John: cardinal number inqcodes have been patched.


(defun encaps-fetch-feature (feature term)
(if (kpml::term-graph-p (symbol-value term))
(kpml::fetch-feature feature term) nil
))

(defun encaps-fetch-subc-feature (feature term)
(if (kpml::term-graph-p (symbol-value term))
(kpml::fetch-subc-feature feature term) nil
))

(defun encaps-fetch-subc-feature-2 (feature term)
(if (kpml::term-graph-p (symbol-value term))
(kpml::fetch-subc-feature feature term) nil
))


(defun encaps-term-graph-type (term)
(if (kpml::term-graph-p (symbol-value term))
(kpml::term-graph-type term) nil
)ma)

(defun Ability-Q-Code (modal-property process participant)
  "See if the MODAL-PROPERTY is one of ABILITY."
  (declare (ignore process participant))
  (if (term-type-p modal-property '|Ability|) 'ability 'nonability))

(defun Above-Ten-Type-Q-Code (number)
  "Give back NUMBER, spelled."
  (case number
    (11 'eleven)
    (12 'twelve)
    (otherwise 'ten)))

(defun Above-Twenty-Q-Code (number)
  "Test magnitude of NUMBER."
  (if (< number 20) 'below 'above))

(defun Absolute-Extent-Q-Code (reln range)
  "Depends on type of RELN."
  (declare (ignore range))
  (if (term-type-p reln '|AbsoluteExtent|) 'absolute
      (if (term-type-p reln '|RelativeExtent|) 'nonabsolute)))

(defun Abstraction-Q-Code (conjunctive)
  "If an RST-ELABORATION, then ABSTRACTION."
  (if (term-type-p conjunctive '|uio#RSTElaboration|) 'abstraction 'notabstraction))

(defun Accompaniment-ID-Code (process)
  "Return the accompaniment feature."
  (fetch-relation-spec process '|accompaniment| 'Accompaniment-ID))

(defun Accompaniment-Mod-ID-Code (process)
  "Return the accompaniment feature as a reified relation."
  (fetch-relation-spec process '|accompaniment| 'Accompaniment-MOD-ID))

(defun Accompaniment-Modification-Q-Code (process)
  "Look for an accompaniment feature."
  (if (fetch-subc-feature '|accompaniment| process) 'accompanying))

(defun Accompaniment-Q-Code (process)
  "Look for an accompaniment feature."
  (if (fetch-relation '|accompaniment| process) 'Accompaniment))

(defun Accompaniment-Relation-Q-Code (process)
  "Is it an accompaniment relation?"
  (if (term-type-p process '|accompaniment|) 'Accompaniment 'notaccompaniment))

(defun Actor-ID-Code (process)
  "Return the actor feature."
  (fetch-subc-feature '|actor| process))

(defun Actualization-Constrainer-ID-Code (spec process)
  "Return some appropriate role."
  (declare (ignore spec)) ;;;Which arg do we really need?
  (or ; (fetch-subc-feature 'beneficiary   process) 
      (fetch-subc-feature '|domain|    process)
      (fetch-subc-feature '|range|     process)
      (fetch-subc-feature '|actor|     process)
      (fetch-subc-feature '|actee|     process)
      (fetch-subc-feature '|senser|    process)
      (fetch-subc-feature '|sayer|     process)
      (fetch-subc-feature '|saying|    process)
      (fetch-subc-feature '|addressee| process)))

(defun Actualization-Constrainer-Q-Code (spec process)
  "ACTUALIZATIONCONSTRAINER only if there's no theme."
  (declare (ignore spec))
  (if (fetch-feature '|theme| process) 'nonactualizationconstrainer))

(defun Addition-Q-Code (reln range)
  "If RELN is of type ADDITIVE."
  (declare (ignore range))
  (if (term-type-p reln '|additive|) 'addition 'nonaddition))

(defun Address-Q-Code (process)
  "If there is an ADDRESSEE feature, then ADDRESS."
  (if (fetch-subc-feature '|addressee| process) 'address 'nonaddress))

(defun Addressee-ID-Code (process)
  "If there is an ADDRESSEE feature, then ADDRESS."
  (fetch-subc-feature '|addressee| process))

(defun Addressee-Oriented-Q-Code (process)
  "If the process type is Addressee-Oriented-Verbal-Process, then ADDRESSEEORIENTED."
  (if (term-type-p process '|AddresseeOriented|) 
      'addresseeoriented 
      'notaddresseeoriented))

(defun AddressSpecificity-Q-Code (object)
  "If it's a  date, with month/day or month/year features, to position."
  (if (or (and (term-type-p object '|Date|)
	       (or (fetch-subc-feature '|Day| object)
		   (fetch-subc-feature '|Month| object))
	       (or (fetch-subc-feature '|Month| object)
		   (fetch-subc-feature '|Year| object)))
	  (and (term-type-p object '|ZeroDLocation|)
	       (fetch-subc-feature '|Latitude| object)
	       (fetch-subc-feature '|Longitude| object)))
      'ToPosition 
      'ToRegion))

(defun affected-id-code (p)
(setf r (sub-affected-id-code p))	  
r)
	  
(defun sub-Affected-ID-Code (process)
  "Return the actee of a DIRECTED-ACTION and actor of a NONDIRECTED-ACTION."
  (if (term-type-p process '|space#SpatialLocating|)
  (fetch-subc-feature '|attribute| process)
  (if (term-type-p process '|NonAffectingAction|)
      (fetch-subc-feature '|actor| process)
      (fetch-subc-feature '|actee| process))))

(defun Age-Modification-Q-Code (object)
  "Look for an AGE feature."
  (if (fetch-subc-feature '|AgePropertyAscription| object) 'age))

(defun Age-Modifier-ID-Code (object)
  "Return the AGE feature."
  (fetch-subc-feature '|AgePropertyAscription| object))

;(defun Agent-Mention-Q-Code (actee)
;  "Look for inverse speaker, creator features."
;  (if (or (fetch-subc-feature 'inverse-creator actee)
;	  (fetch-subc-feature 'inverse-speaker actee)) 'mention))

;(defun Agent-Mention-Q-Code (process)
;  "Look for an ACTOR."
;  (if (fetch-subc-feature 'actor process) 'mention))

;  Patch: mick, to allow instrument to become subject
;  see also Causer-ID-Code, Know-Manner-Q-Code 
(defun Agent-Mention-Q-Code (process)
  "Look for an ACTOR."
  (if (or (fetch-subc-feature '|actor| process)
	  (fetch-subc-feature '|instrumental| process))  'mention))

(defun Agent-Qualifier-ID-Code (activity)
  "Return the ACTOR."
  (fetch-subc-feature '|actor| activity))


(defun Agentive-Q-Code (manner process)
  "Is MANNER an AGENTIVE?"
  (declare (ignore process))
  (if (term-type-p manner '|agentive|) 'agentive 'nonagentive))

(defun Alternative-Q-Code (reln range)
  "If RELN is of type ALTERNATIVE."
  (declare (ignore range))
  (if (term-type-p reln '|alternative|) 'alternative 'nonalternative))

(defun Ambient-Q-Code (process)

  (if (term-type-p process '|Ambience|) 'ambient))

(defun Amount-Attention-Q-Code (object)
  "Who knows?"
  (if (not
	(or (eq (fetch-feature '|determiner| object) 'all)
	    (eq (fetch-feature '|determiner| object) 'the)
	    (eq (fetch-feature '|determiner| object) 'nodet)) )
      'minimalattention ))

(defun Answer-Q-Code (speech-act)
  "Is the type of the speech-act term ANSWER?"
  (if (term-type-p speech-act '|answer|) 'answer 'nonanswer))

(defun Anterior-Q-Code (temporal-relation)
  "Is TEMPORAL-RELATION anterior?"
  (if (term-type-p temporal-relation '|anterior|) 'anterior 'notanterior))

(defun Anteriority-Q-Code (temporal-relation)
  "We are not sure what it means for anteriority to be specifed, (for a nonanterior relation)."
  (if (term-type-p temporal-relation '|anterior|) 'anteriority 'noanteriority))

(defun Ascription-Q-Code (reln)
  "We just look to see if the variable--a relation--is an ascription."
  (if (term-type-p reln '|Ascription|) 'Ascription 'notascription))

#+ignore
(defun Attribuend-ID-Code (reln)
  "Return the PROPERTY-ASCRIPTION feature, or look for the DOMAIN feature of the parent term."
  (if (fetch-minimal-relation '|PropertyAscription| reln)
      (fetch-subc-feature '|domain| (fetch-minimal-relation '|PropertyAscription| reln))
      (if (and (not (term-type-p reln '|SimpleQuality|))
	       (not (fetch-subc-feature '|range| reln)))
	  (fetch-subc-feature '|domain| reln)
	  (or (fetch-subc-feature '|domain| (term-graph-parent (symbol-value reln)))
	      (term-graph-parent (symbol-value reln))))))

(defun Attribuend-ID-Code (property)
  "Previously  Dimension-ID code - but works better for Attribuend-id: give the relation of which PROPERTY is the range."
  (or (when (or (term-type-p property '|SimpleQuality|)
		(term-type-p property '|Quantity|)
		(term-type-p property '|SimpleThing|)) 
	(term-graph-parent (symbol-value property)))
      (when (term-type-p property '|PropertyAscription|)
	(fetch-subc-feature '|domain| property))))

(defun Attribuend-Qualification-Q-Code (obj)
  "Look for any PROPERTY-ASCRIPTION feature, making sure you only pick out RANGEs."
  (if (or 
	(and (fetch-minimal-relation '|PropertyAscription| obj)
	     (fetch-subc-feature '|range| 
				 (fetch-minimal-relation '|PropertyAscription| obj))
	     (eq (term-graph-symbol 
		   (symbol-value 
		     (fetch-subc-feature '|range| 
					 (fetch-minimal-relation '|PropertyAscription| obj))))
		 (term-graph-symbol (symbol-value obj))))
	(and ;(not (pledged-p obj))
	     (term-type-p obj 'two-place-relation)
	     (fetch-subc-feature '|domain| obj)
	     (not (fetch-subc-feature '|range| obj))))
      'attribuendqualification))

(defun Attribute-ID-Code (reln)
  "Find the RANGE feature."
  (or (fetch-subc-feature '|range| reln)
      reln))

(defun Axis-Of-Orientation-Q-Code (reln range)
  "Use the type of RELN."
  (declare (ignore range))

 
  (setf t1 (fetch-subc-feature '|space#hasSpatialModality| range))
  (when (not t1)   (setf t1 (fetch-feature '|space#hasSpatialModality| (fetch-subc-feature '|attribute| range))) )
  (if (term-type-p t1 '|space#HorizontalProjection|) 'horizontal
      (if (term-type-p t1 '|space#VerticalProjection|) 'vertical)))

(defun Behalf-Q-Code (relation)-
  "Is the relation a CLIENT?"
  (if (term-type-p relation '|client|) 'behalf 'nonbehalf))

(defun Behalf-ID-Code (process)
  "Return the CLIENT feature."
  (fetch-relation-spec process '|client| 'Behalf-ID))

(defun Below-Hundred-Q-Code (number)
  "Just test magnitude of NUMBER."
  (if (> number 99) 'notbelowhundred 'belowhundred))

(defun Below-Ten-Q-Code (number)
  "Just test magnitude of NUMBER."
  (if (> number 9) 'notbelowten 'belowten))

; john patch 20/5/96 - did not have zero...
(defun BelowTen-Type-Q-Code (item)
  "Just give back ITEM, spelled out."    
    (Case item
	  (0 'zero)
	  (1 'one)
	  (2 'two)
	  (3 'three)
	  (4 'four)
	  (5 'five)
	  (6 'six)
	  (7 'seven)
	  (8 'eight)
	  (otherwise 'nine)))

(defun Beneficiary-ID-Code (process)
  "Return the beneficiary or recipient."
  (or (fetch-feature '|beneficiary| process)
      (fetch-feature '|recipient|   process)))

(defun Between-Q-Code (reln range)
  "Look for a between feature."
  (declare (ignore range))
  (if (term-type-p reln '|between| ) 'between 'notbetween))

(defun Body-Of-Water-Q-Code (object)
  "Is it an ocean?"
  (if (term-type-p object '|Ocean|) 'bodyofwater 'notbodyofwater))

(defun Cardinal-Complexity-Q-Code (number)
  "If number is less than 20, or a multiple of ten, it's expressed by a single position."
  (if (or (< number 20)
	  (and (< number 100)
	       (eql 0 (rem number 10)))
	  (and (not (< number 100)) 
	       (< number 1000)
	       (eql 0 (rem number 100)))
	  (and (not (<= number 1000))  ;; changed for 1000 
	       (< number 10000)
	       (eql 0 (rem number 1000)))
	  (and (not (< number 10000))
	       (> number 999999)
	       (eql 0 (rem number 1000000))))
      'singleposition
      'multiplepositions))

(defun Carrier-ID-Code (reln)
  "Find the DOMAIN feature."
  (fetch-subc-feature '|domain| reln))

(defun Causal-Relation-Q-Code (process)
  "Is it a causal relation?"
  (if (term-type-p process '|causalRelation|) 'causalrelation 'notcausalrelation))

(defun Causation-And-Behalf-Q-Code (process)
  "Look for a client feature."
  (if (fetch-subc-feature '|client| process) 'behalf))

(defun Causation-And-Concession-Q-Code (process)
  "Look for a CONCESSIVE feature."
  (if (fetch-subc-feature '|concessive| process) 'concession))

(defun Causation-And-Purpose-Q-Code (process)
  "Look for a PURPOSE feature."
  (if (fetch-subc-feature '|purpose| process) 'purpose))

(defun Causation-And-Reason-Q-Code (process)
  "Look for a REASON feature."
  (if (fetch-subc-feature '|reason| process) 'reason))

(defun Causation-Q-Code (process)
  "Look for a causation feature, or for a beneficiary."
  (if (and (fetch-subc-feature '|causalRelation| process)
	   (not (pledged-p (fetch-subc-feature '|causalRelation| process))))
      'cause))

	  
(defun Causative-Mention-Q-Code (causer activity)
  "WITHHOLD only if causer is unknown."
  (declare (ignore activity))
  (if (eq causer '?actor) 'withhold))

  
(defun Cause-Condition-Q-Code (part1 part2)
  "See if the parent is a CAUSE."
  (declare (ignore part2)) 
  (if (term-type-p (term-graph-parent (symbol-value part1)) '|causeEffect|)
      'causecondition 
      'noncausecondition))

(defun Cause-ID-Code (process)
  "Return the causation feature."
  (or (fetch-relation-spec process '|causalRelation| 'Cause-ID)
      (fetch-relation-spec process '|beneficiary| 'Cause-ID)))

(defun Caused-Process-Q-Code (process)
  "Is the action directed?  If not, is it a mental activity?"
   (if (or (term-type-p process '|AffectingAction|)
	   (term-type-p process '|MentalActive|)) 'caused 'independent))

;(defun Causer-ID-Code (process)
;  "Return the actor."
;  (or (fetch-subc-feature 'actor process)
;      (if (term-type-p process 'mental-active)
;	  (fetch-subc-feature 'senser process))))

;  mick patch 11/15/90 - allow instrument to become agent
;  See also Agent-Mention-Q-Code, Know-Manner-Q-Code
(defun Causer-ID-Code (process)
  "Return the actor."
  (or (fetch-subc-feature '|actor| process)
      (if (term-type-p process '|MentalActive|)
	  (fetch-subc-feature '|senser| process))
      (fetch-subc-feature '|instrumental| process)))

; john patch 20/5/96 - was unprepared for numbers...
(defun Circumstance-Q-Code (arg)
  "Is its type circumstantial?"
  (cond ((numberp arg)
	 'notcircumstance)
	((or (term-type-p arg '|Circumstantial|)
	     (term-type-p (term-graph-parent (symbol-value arg)) '|Circumstantial|)
	     (when (term-graph-parent (symbol-value arg)) 
	       (term-role-p 
		(term-graph-parent (symbol-value arg)) arg '|Circumstantial|)))
	 'circumstance)
	(t 'notcircumstance)))

(defun Circumstantial-Domain-ID-Code (reln)
  "Return the DOMAIN feature."
  (fetch-subc-feature '|domain| reln))

(defun Circumstantial-Q-Code (reln)
  "We just look to see if the variable--a relation--is a circumstantial relation."
  (if (term-type-p reln '|Circumstantial|) 'circumstantial 'noncircumstantial))

(defun Circumstantial-Range-ID-Code (reln)
  "Return the RANGE feature."
  (fetch-subc-feature '|range| reln)
  )

(defun Circumstantial-Relation-And-Range-ID-Code (reln)
  "Return the relation: its range comes with it."
  (setf temp (or (fetch-subc-feature '|attribute| reln) (fetch-subc-feature '|space#temporalLocating| reln)))
  temp)

(defun Circumstantial-Relation-ID-Code (reln)
  "Just give the relation back."
  reln)

(defun Circumstantial-Theme-Q-Code (circumstance spec)
  "If CIRCUMSTANCE is a member of the THEME feature of spec, then CONTEXT."
  (if (fetch-feature '|theme| spec) 
      (dolist (theme (term-graph-symbol (symbol-value (fetch-feature '|theme| spec))))
	(if (or (term-eq-p circumstance theme)
		(term-eq-p (global-fetch-feature '|range| theme) circumstance)
		(term-eq-p (fetch-subc-feature '|range| circumstance) theme))
	    (return 'context)))))

(defun Class-Q-Code (range)
  "If the argument is NOT a QUALITY, then return CLASS."
  (if (term-type-p range '|SimpleQuality|) 'nonclass 'class))

(defun Clause-As-Nominal-Q-Code (onus)
  "If it's a process....but not a relational-process(?)"
  (if (and (term-type-p onus '|Process|)
	   (not (term-type-p onus '|BeingANDHaving|)) )
      'nominal-clause 
      'nonnominal-clause))

(defun Cliency-Q-Code (service )
  "look for a beneficiary--only works for CREATION."
  (if (fetch-feature '|beneficiary| service) 'cliency))

(defun Client-ID-Code (service)
  "Return BENEFICIARY."
  (fetch-feature '|beneficiary| service))

(defun Cognition-Q-Code (process)
  "If PROCESS is a COGNITION...."
  (if (term-type-p process '|Cognition|) 'cognition 'noncognition))

(defun Cognitive-Phenomenon-ID-Code (process)
  "Return the PHENOMENON feature."
   (fetch-subc-feature '|phenomenon| process))

(defun Cognitive-Phenomenon-Q-Code (process) 
  "If there is a PHENOMENON feature...."
  (if (and (fetch-subc-feature '|phenomenon| process)
	   (not (term-type-p (fetch-subc-feature '|phenomenon| process) '|Process|)))
      'phenomenon))

(defun Colour-Modification-Q-Code (object)
  "Look for a COLOUR (COLOR) feature."
  (if (fetch-subc-feature '|ColorPropertyAscription| object) 'colour))

(defun Colour-Modifier-ID-Code (object)
  "Return the color feature."
  (fetch-subc-feature '|ColorPropertyAscription| object))

(defun Command-Offer-Q-Code (report)
  "If the type of the projector of REPORT is Non-Addressee-Oriented, then not command offer."
  (if (or (and (term-type-p (term-graph-parent (symbol-value report)) '|Internal|)
	       (not (term-type-p (term-graph-parent (symbol-value report)) '|ReactionAndEmotion|)))
	  (term-type-p (term-graph-parent (symbol-value report)) '|NonAddressing|))
      'notcommandoffer
      'commandoffer))

(defun Command-Q-Code (speech-act)
  "Is the type of the speech-act term COMMAND?"
  (if (term-type-p speech-act '|uio#Directive|) 'command 'nocommand))

(defun Command-Responsible-ID-Code (speechact)
  "Pick up the HEARER feature, if any.  If not, value will default."
  (fetch-feature '|hearer| speechact))

(defun Comparison-Representative-ID-Code (concept)
  "A guess--depends on representation chosen.
   Here we return the RANGE of an expected IDENTITY involving CONCEPT as DOMAIN."
   (fetch-subc-feature '|range| (term-graph-parent (symbol-value concept))))

(defun Complex-Thing-Part1-ID-Code (onus)
  "Pop the first member of the set, and use its id."
  (if (or (term-type-p onus '|Conjunction|)
	  (term-type-p onus '|Disjunction|))
      (fetch-feature '|domain| onus)
      (if (symbolp (first (term-graph-symbol (symbol-value onus))))
      (first (term-graph-symbol (symbol-value onus)))
      (term-graph-id (first (term-graph-symbol (symbol-value onus)))))))

(defun Complex-Thing-Part2-ID-Code (onus)
  "Modify the spec:  the first elt is consumed.  If one only is left, give back its id."
  (if (or (term-type-p onus '|Conjunction|)
	  (term-type-p onus '|Disjunction|))
      (fetch-feature '|range| onus)
      (if (< 2 (length (term-graph-symbol (symbol-value onus))))
	  (and (setf (term-graph-symbol (symbol-value onus)) 
		     (rest (term-graph-symbol (symbol-value onus))))
	       onus)
	  (if (symbolp (second (term-graph-symbol (symbol-value onus))))
	      (second (term-graph-symbol (symbol-value onus)))
	      (term-graph-id (second (term-graph-symbol (symbol-value onus)))))
	  )))

(defun ComplexCardinalDigit-Q-Code (number)
  "Is it less than 10?"
  (if (< number 10) 'simplex 'complex))

(defun Conceptual-Correlate-ID-Code (spec) spec)

;(defun Conceptual-Correlate-ID-Code (spec)
;  "Get the symbol of this term, unless it's a set."
;  (if (and (boundp spec) 
;	   (eq (type-of (symbol-value spec)) 'symbol))
;      (symbol-value spec)
;      (if (and (boundp spec)
;	       (not (term-type-p spec 'um-set))
;	       (eq (type-of (symbol-value spec)) 'term-graph))
;	  (term-graph-symbol (symbol-value spec))
;	  spec)))

(defun Concession-Q-Code (reln)
  "If the type of RELN is CONCESSIVE...."
  (if (term-type-p reln '|concessive|) 'concession 'nonconcession))

(defun Concession-ID-Code (process)
  "Return the CONCESSION feature."
  (fetch-relation-spec process '|concession| 'Concession-ID))

(defun Concessive-Condition-Q-Code (part1 part2)
  "See if the parent is a CONCESSIVE."
  (declare (ignore part2))
  (if (term-type-p (term-graph-parent (symbol-value part1)) '|uio#RSTConcessive|)
      'concessive 
      'notconcessive))

(defun Concessive-Relation-Q-Code (reln)
  "If the type of RELN is CONCESSIVE...."
  (if (term-type-p reln '|concessive|) 'concession 'notconcession))

(defun Conditional-Relation-Q-Code (reln)
  "If the type of RELN is LOGICAL...."
  "Or CONDITION...UM reln added 7/90" 
  (if (or (term-type-p reln '|Extension|)
	  (term-type-p reln '|condition|))
      'condition 
      'notcondition))

(defun Conditioning-Q-Code (part1 part2)
  "Ask if the conjunctive relation governing the two parts is a conditioning relation."
  ;(declare (ignore part2)) 
  (if (and (or (term-type-p (term-graph-parent (symbol-value part1)) '|causalRelation|)
	       (term-type-p (term-graph-parent (symbol-value part1)) '|uio#LogicalCondition|)   ;;; REUTERS
	       (term-type-p (term-graph-parent (symbol-value part1)) '|space#TemporalLocating|)   ;;; EX-SET-8
	       (term-type-p (term-graph-parent (symbol-value part1)) 'static-spatial)
	       (term-type-p (term-graph-parent (symbol-value part1)) '|generalizedMeans|)   ;;; FINANCE DOMAIN
	       (term-eq-p part2 (fetch-subc-feature '|causalRelation| part1))
	       (term-eq-p part2 (fetch-subc-feature '|space#TemporalLocating| part1))
	       (term-eq-p part2 (fetch-subc-feature 'static-spatial part1)))               ;;; EX-SET-13
	   (not (dolist (graph *plan-graphs*)
		  (if (and (consp (term-graph-symbol (symbol-value graph)))
			   (member part1 (term-graph-symbol (symbol-value graph)))
			   (or (term-type-p graph '|UMSet|)
			       (term-type-p graph '|DisjunctiveSet|)))
		      (return t)))))
      'conditioning
      'nonconditioning))

(defun Conjunctive-Extension-Q-Code (initiating continuing)
  "If type of continuing is SET, or the pair are elements of a set, then conjunctive.
   Also, if INITIATING & CONTINUING are related by CONJUNCTION."
  (if (or (term-type-p continuing '|UMSet|)
	  (term-type-p initiating '|Conjunction|)
	  (dolist (graph *plan-graphs*)
	    (if (and (term-type-p graph '|UMSet|)
		     (not (term-type-p graph '|DisjunctiveSet|)) ;; 2.0
		     (consp (term-graph-symbol (symbol-value graph)))
;		     (member initiating (term-graph-symbol (symbol-value graph)))
		     (member continuing (term-graph-symbol (symbol-value graph))))
		(return 't))
	    (if (and (term-type-p graph '|Conjunction|)
		     (term-eq-p initiating (fetch-subc-feature '|domain| graph))
		     (term-eq-p continuing (fetch-subc-feature '|range| graph)))
		(return 't))))
      'conjunctive 
      'nonconjunctive))


(defun Conjunctive-Relation-ID-Code (process)
  "Two cases:  when the conjunctive relation is a feature of the process, and when the 
   process is a feature (domain or range) of a rhetorical relation."
  (or (fetch-subc-feature '|uio#RhetoricalRelation| process)
      (fetch-subc-feature-name '|Circumstantial| (term-graph-parent (symbol-value process)))
      (term-graph-parent (symbol-value process))))

(defun Conjunctive-Relation-Q-Code (process)
  "Is there a conjunctive relation feature?"
  (if (and (fetch-subc-feature '|uio#RhetoricalRelation| process)
	   (not (term-role-p process 
			     (fetch-subc-feature '|uio#RhetoricalRelation| process) 
			     '|uio#RSTMeans|)))
      'conjunctive))

(defun Consciousness-Q-Code (obj) 
  "Is it a conscious being?"
  (if (term-type-p obj '|ConsciousBeing|) 'conscious 'nonconscious))

(defun Contrastive-Q-Code (reln) 
  "Is RELN a CONTRASTIVE?"
  ;;(if (term-type-p reln '|contrastive|) 'contrastive 'notcontrastive))
  (if (term-type-p reln '|uio#RSTContrastive|) 'contrastive 'notcontrastive))

(defun Contrastive-Extension-Q-Code (initiating continuing)
  "If type of relation between initiating and continuing is RST-Contrastive"
  (declare (ignore initiating))
  (if (term-type-p (term-graph-parent (symbol-value continuing)) '|uio#RSTContrastive|)
      'contrastive 
      'noncontrastive))

(defun Countability-Q-Code (thing)
  "COUNTABLE if not MASS."
  (if (term-type-p thing '|mass|) 'mass 'countable))

(defun Current-Representative-ID-Code (obj) obj)

(defun Dependent-Beta-Theme-Q-Code (dependent spec)
  "CONTEXT, if there is a THEME feature in SPEC involving DEPENDENT."
  (if (fetch-feature '|theme| spec) 
      (dolist (theme (term-graph-symbol (symbol-value (fetch-feature '|theme| spec))))
	(if (term-eq-p dependent theme)
	    (return 'context)))))

(defun Destination-Process-Q-Code (locativerelation place)
  "Is the relation one of destination?"
  (declare (ignore place)) 
  (if (term-type-p locativerelation '|space#destination|)
      'destination
      'nondestination))

(defun Destination-Q-Code (reln process)
  "Is the relation a destination?"
  (declare (ignore process))
  (if (term-type-p reln '|space#destination|)
      'destination 
      'notdestination))

(defun DigitValue-Q-Code (number)
  "just give it back as a word."
  (case number
    (1 'one)
    (2 'two)
    (3 'three)
    (4 'four)
    (5 'five)
    (6 'six)
    (7 'seven)
    (8 'eight)
    (9 'nine)
    (t 'zero)))

(defun Dimension-ID-Code (property)
  "Not clear:  for the time being, give the relation of which PROPERTY is the range."
  (term-graph-parent (symbol-value property)))

(defun Dimension-Relation-Q-Code (relation range)
  "Use type of RANGE."


  (let ((dimension (if (term-type-p range '|UMSet|)
		       (first (term-graph-symbol (symbol-value range)))
		       range)))
				(setf test (cond ((term-type-p dimension '|SpacePoint|) 'ZeroDimension)
	  ((term-type-p dimension '|OneOrTwoDLocation|) 'OneTwoDimension)
	  ((term-type-p dimension '|ThreeDLocation|) 'ThreeDimension)))
    test))

(defun Disjunctive-Extension-Q-Code (initiating continuing)
  "If type of continuing is DISJUNCTIVE-SET (SPL) or DISJUNCTION (UM), then DISJUNCTIVE."

  (if (or (term-type-p continuing '|DisjunctiveSet|)
	  (term-type-p continuing '|Disjunction|)
	  (dolist (graph *plan-graphs*)
	    (if (and (term-type-p graph '|DisjunctiveSet|)
		     (consp (term-graph-symbol (symbol-value graph)))
;	  	     (member initiating (term-graph-symbol (symbol-value graph)))
		     (member continuing (term-graph-symbol (symbol-value graph))))
		(return 't))
	    (if (and (term-type-p graph '|Disjunction|)
		     (term-eq-p initiating (fetch-subc-feature '|domain| graph))
		     (term-eq-p continuing (fetch-subc-feature '|range| graph)))
		(return 't))))
      'disjunctive 
      'nondisjunctive))

(defun Disliking-Q-Code (mentalprocess)
  "Is it of type LIKING?"
  (if (term-type-p mentalprocess '|Disliking|) 'disliking 'nondisliking))

(defun Dual-Process-Part1-ID-Code (onus)
  "For rhetorical relations, return DOMAIN (unless it is nil).
   For ONUS which is a set, return the next element."
  (if (or (term-type-p onus '|uio#RhetoricalRelation|)
	  (term-type-p onus '|temporalRelation|)                                 ;;; PRIMER-49
	  (term-type-p onus 'static-spatial)                                    ;;; EX-SET-13
	  (term-type-p onus '|Conjunction|)
	  (term-type-p onus '|Disjunction|))
      (let* ((result (fetch-feature '|domain| onus)))
	(modification-specification-id-code 
	 nil 
	 (if result result
	   (fetch-feature '|range| onus))))
      (if (or (term-type-p onus '|UMSet|)
	      (term-type-p onus '|DisjunctiveSet|))
	  (dolist (term (term-graph-symbol (symbol-value onus)))
	    (if (not (member term *consumed-terms*))
		(return term)))
	  ;;; Default for, e.g., temporal roles: after, when, etc.
	  onus)))

(defun Dual-Process-Part2-ID-Code (onus part1)
  "For rhetorical relations, return RANGE.
   For ONUS which is a set, return either the last element or ONUS."
  (if (or (term-type-p onus '|uio#RhetoricalRelation|)
	  (term-type-p onus '|temporalRelation|)                                 ;;; PRIMER-49
	  (term-type-p onus 'static-spatial)   
	  (term-type-p onus '|Conjunction|)
	  (term-type-p onus '|Disjunction|))
      (modification-specification-id-code nil (fetch-feature '|range| onus))
      (if (or (term-type-p onus '|UMSet|)
	      (term-type-p onus '|DisjunctiveSet|))
	  (if (< (+ (position part1 (term-graph-symbol (symbol-value onus))) 2)
		 (length (term-graph-symbol (symbol-value onus))))
	      onus
	      (first (last (term-graph-symbol (symbol-value onus)))))
	  (or (fetch-subc-feature '|space#temporalOrdering| onus)                      ;;; PRIMER-49
	      (fetch-subc-feature 'static-spatial onus)                         ;;; EX-SET-13
	      (fetch-subc-feature '|condition| onus)))))

(defun Duration-Q-Code (reln range)
  "Depends on type of RELN."
  (declare (ignore range))
  (if (term-type-p reln '|exhaustiveDuration|) 'duration
      (if (term-type-p reln '|nonExhautiveDuration|) 'nonduration)))

(defun Elaborated-Thing-ID-Code (spec)
  "Pick up the DOMAIN of the elaboration."
  (fetch-subc-feature '|domain| spec))

(defun Elaborating-Thing-ID-Code (spec)
  "Pick up the RANGE of the elaboration."
  (fetch-subc-feature '|range| spec))

(defun Elaboration-Q-Code (part1 part2)
  "Are PART1 and PART2 related by an ELABORATION?"
  (declare (ignore part2))
  (if (term-type-p (term-graph-parent (symbol-value part1)) '|uio#RSTElaboration|)
      'same
      'distinct)) 

(defun Empty-Gender-Multiplicity-Q-Code (obj)
  "First cut: no :favor-q feature & :express-type is NO."
  (if (not
	(or (fetch-feature 'favor-q obj)
	  (not (eq (fetch-feature 'express-type obj) 'no))))
      'empty))

(defun Empty-Number-Q-Code (obj)
  "If EITHER there are no features (or :identifiability identifiable only)
                   and the goodie has been mentioned before
                       and nothing more recently mentioned has same number & gender
                   or it has the most general type
      OR :EXPRESS-TYPE is not YES, and MULTIPLICITY-Q is given, then EMPTY."
;;; or it should have been anyway. But making this change causes 
  ;;; far too many empty's to be returned and wrecks lots of examples.
  ;;; So revert to the original definition. This entire thing should
  ;;; be overhauled sometime in any case.

  (if (or (and (or (null (term-graph-features (symbol-value obj)))
		   (equal (term-graph-features (symbol-value obj)) 
			  '((identifiability-q . identifiable))))
	       (if (eq obj (first *consumed-terms*))
		   (or (dolist (term (rest *consumed-terms*) nil)
			 (if (term-eq-p term obj)
			     (return t))
			 (if (and (term-type-p term '|SimpleThing|) 
				  (eq (operator-run `(gender-q ,term))
				      (operator-run `(gender-q ,obj)))
				  (eq (operator-run `(multiplicity-q ,term))
				      (operator-run `(multiplicity-q ,obj))))
			     (return nil)))
		       (eq (term-graph-type (symbol-value obj)) 'thing))))
	  (and (eq (fetch-feature 'express-type obj) 'no)
	       (fetch-feature 'multiplicity-q obj)))
      'empty))

(defun Enabling-Q-Code (manner process)
  "Is MANNER an ENABLEMENT?"
  (declare (ignore process))
  (if (term-type-p manner '|enablement|) 'enabling 'nonenabling))


(defun Event-Q-Code (item)
  (if (term-type-p item '|Process|) 'event 'object))

(defun Example-Elaboration-Q-Code (one two)
  "If the parent is an EXEMPLIFICATION...."
  (declare (ignore two))
  (if (term-type-p (term-graph-parent (symbol-value one)) '|uio#Exemplification|) 
      'example 
      'notexample))

(defun Exceed-Q-Code (one two)
  "Depends on the kind of parent relation."
  (if (or (and (term-type-p (term-graph-parent (symbol-value one)) '|GreaterThanComparison|)
	       (term-role-p (term-graph-parent (symbol-value one)) one '|greater|))
	  (and (term-type-p (term-graph-parent (symbol-value two)) '|LessThanComparison|)
	       (term-role-p (term-graph-parent (symbol-value two)) two '|lesser|)))
	  'exceed
	  'nonexceed))

(defun Existent-ID-Code (reln)
  "Return the DOMAIN."
  (fetch-subc-feature '|participantInConfiguration| reln))

(defun Exist-Speech-Act-Q-Code (onus)
  "SPEECHACT, if PROCESS"
;;;  (if (or (term-type-p onus '|Process|)
  (if (or (term-type-p onus '|Configuration|)
	  (and  (term-type-p onus '|UMSet|)
;;;		(term-type-p (first (term-graph-symbol (symbol-value onus))) '|Process|)))
		(term-type-p (first (term-graph-symbol (symbol-value onus))) '|Configuration|)))
      'Speechact 'nospeechact))

(defun Existential-Q-Code (reln)
  "We just look to see if the variable--a relation--is an existence."
  (if (term-type-p reln '|Existence|) 'existential 'nonexistential))

(defun Exists-Focuser-Q-Code (process)
  "Look for a LOGICAL-PROPERTY-ASCRIPTION feature."
  (if (fetch-feature '|LogicalPropertyAscription| process) 'focuser-exists))

(defun Experiential-Complexity-Q-Code (arg)
  "Experientially complex if arg is a set.  Then we express each member."
  (if (and (boundp arg)
	   (typep (symbol-value arg) 'term-graph)
	   (or (and (term-type-p arg '|UMSet|)
		    (consp (term-graph-symbol (symbol-value arg)))              ;;; EX-SET-10
		    (not (and (eq 'included (member-set-q-code '|speaker| arg))   ;;; EX-SET-13
			      (eq 'included (member-set-q-code '|hearer| arg)))))
	       (term-type-p arg '|Conjunction|)
	       (term-type-p arg '|Disjunction|)
	       (term-type-p arg '|DisjunctiveSet|)))
      'complex
      'simplex))

	  
(defun Express-Hearer-Q-Code (hearer onus)
  "Depends on the speech-act type of ONUS."
  (declare (ignore hearer))
  (if (term-type-p (fetch-feature '|uio#SpeechAct| onus) '|uio#Question|)
      'expresshearer
      'withholdhearer))

(defun Extent-Q-Code (reln)
  "We just look to see if the variable--a relation--is an extent."
  (if (term-type-p reln '|extent|) 'extent 'nonextent))

(defun Extracted-Variable-ID-Code (process)
  "If NOT a process involved in a RHETORICAL-RELATION, get the parent term.
   If it is such a process, identify a point of overlap."
  (if (and (boundp process)
	   (typep (symbol-value process) 'term-graph))
      (if (term-type-p (term-graph-parent (symbol-value process)) '|uio#RhetoricalRelation|)
	  (first 
	    (intersection 
	      (remove nil
		      (mapcar #'(lambda (a-feature) 
				  (if (or (term-type-p (first a-feature) '|participantInConfiguration|)
					  (term-type-p (first a-feature) '|place|))
				      (term-graph-symbol (rest a-feature))))
			      (term-graph-features (symbol-value process))))
	      (remove nil 
		      (mapcar #'(lambda (a-feature) 
				  (if (or (term-type-p (first a-feature) '|participantInConfiguration|)
					  (term-type-p (first a-feature) '|place|)
					  (term-type-p (first a-feature) '|BeingANDHaving|));;;To pick up other connexions
				      (term-graph-symbol (rest a-feature))))
			      (term-graph-features 
				(symbol-value 
				  (fetch-subc-feature 
				    '|domain| 
				    (term-graph-parent (symbol-value process)))))))))
	  (term-graph-symbol (symbol-value (term-graph-parent (symbol-value process)))))
      (term-graph-symbol (symbol-value (term-graph-parent (get-symbol-term process))))))


(defun Favor-Q-Code (object feature)
  "Look for a name feature, or a date."
  (case feature
    (propernoun
      (if (or (fetch-feature '|Name| object)
	      (term-type-p object '|date|))
	  'classify 'outclassify))
    (t nil)))

(defun Fearing-Q-Code (mentalprocess)
  "Is it of type FEARING?"
  (if (term-type-p mentalprocess '|Fearing|) 'fearing 'nonfearing))

(defun Focuser-ID-Code (process)
  "Return the LOGICAL-PROPERTY-ASCRIPTION feature."
  (fetch-feature '|LogicalPropertyAscription| process))

(defun Formal-Register-Q-Code (reln)
  "Look for a between feature."
  (if (term-type-p reln '|formal| ) 'formal 'nonformal))

(defun Gender-Q-Code (obj)
  "Go by types MALE and FEMALE, otherwise NEUTRAL."
  (if (term-type-p obj '|Female|) 'female
      (if (term-type-p obj '|Male|) 'male 
	  'neutral)))

(defun General-Subclass-Q-Code (item)

  (when (fetch-feature '|ClassAscription| item) 'subclass))

(defun Generalization-Direction-Q-Code (conjunctive)
  "Depends on nature of CONJUNCTIVE."
  (if (term-type-p conjunctive '|Elaboration|) 'example 'generalization))

(defun Generalized-Modification-Q-Code (object)
  "Is there any kind of modification (ascription, possession)?"
  (if (or (fetch-relation '|PropertyAscription| object)
	  (fetch-relation '|GeneralizedPossession| object)
	  (functional-role-p object)) 
      'possessor))

(defun Hearer-ID-Code (onus)
  "Return the HEARER feature?"
  (fetch-feature '|hearer| onus))

(defun Horizontal-Orientation-Q-Code (reln range)
  "Use the type of RELN."
  ;(declare (ignore range))
  (setf t1 (fetch-subc-feature '|space#hasSpatialModality| range))
  (when (not t1)   (setf t1 (fetch-feature '|space#hasSpatialModality| (fetch-subc-feature '|attribute| range))) )

  (if (term-type-p t1 '|space#FrontalProjection|) 'facing
      (if (term-type-p t1 '|space#BackProjection|) 'behind)))

(defun Hundred-Digit-ID-Code (number)
  "Return the number of hundreds."
  (floor (mod number 1000) 100))

(defun Hundred-Digit-Q-Code (number)
  "Only if greater than 99."
  (if (> number 99) 'yes 'no))

(defun Hundred-Position-ID-Code (number)
  "Return the integer part of the quotient, mod thousands."
  (floor (mod number 1000) 100))

(defun Hundred-Position-Q-Code (entity)
  "Only if greater than 99."
  (if (or (and (> entity 1000)
	       (> (- entity (* (floor entity 1000) 1000)) 99))
	  (and (< entity 1000)
	       (> entity 99)))
      'hundredposition 
      'nothundredposition))

(defun Hundred-Unit-Q-Code (entity)
  "Only if greater than 100."
  (if (>= entity 100) 'hundred 'nothundred))

(defun Identifiability-Q-Code (item &optional (recursive-limit nil))
  "Check identifiability globally as well as local feature, as well as memebership in 
   *CONSUMED-TERMS*.   Identifiability of possessed items follows that of possessor; 
   property-ascription special-cased with RECURSIVE-LIMIT to control degree of transitivity of
   identifiability."
  (if (or (fetch-feature 'identifiable item)  ;;; not in use
	  (global-fetch-feature 'identifiable item)
	  (fetch-subc-feature '|space#hasSpatialModality| item) ;; for relata with a modality, left, right, etc. ND, Aug09
	  (fetch-subc-feature '|OwnedBy| item)
;	  (term-type-p item 'process)
	  (term-eq-p item '|hearer|)
	  (term-eq-p item '|speaker|)
	  (term-type-p item '|relativeDay|)
	  (if (and (not recursive-limit)
		   (fetch-relation '|GeneralizedPossession| item)
		   (or (fetch-subc-feature '|domain| 
					   (fetch-relation '|GeneralizedPossession| item))
		       (fetch-relation '|GeneralizedPossession| item)))
	      (Identifiability-Q-Code 
		(or (fetch-subc-feature '|domain| 
					   (fetch-relation '|GeneralizedPossession| item))
		       (fetch-relation '|GeneralizedPossession| item))
		recursive-limit))
	  (if (and (not recursive-limit)
		   (fetch-relation '|GeneralizedPossessionInverse| item)
		   (or (fetch-subc-feature '|domain| 
			 (fetch-relation '|GeneralizedPossessionInverse| item))
		       (fetch-relation '|GeneralizedPossessionInverse| item)))
	      (Identifiability-Q-Code 
		(or (fetch-subc-feature '|domain| 
					(fetch-relation '|GeneralizedPossessionInverse| item))
		       (fetch-relation '|GeneralizedPossessionInverse| item))
		recursive-limit))
	  (if (and (not recursive-limit) 
		   (term-type-p item '|GeneralizedPossession|)
		   (functional-role-p item))
	      (Identifiability-Q-Code (fetch-subc-feature '|domain| item) recursive-limit))
	  (if (and (not recursive-limit) 
		   (term-type-p item '|GeneralizedPossession| t))
	      (dolist (term *plan-graphs*)
		(if (and (term-eq-p term item)
			 (functional-role-p term))
			 (return (Identifiability-Q-Code (fetch-subc-feature '|domain| term)
							 recursive-limit)))))
	  (if (and (fetch-relation '|PropertyAscription| item)
		   (fetch-subc-feature '|domain| (fetch-relation '|PropertyAscription| item)))
	      (Identifiability-Q-Code 
		(fetch-subc-feature '|domain| (fetch-relation '|PropertyAscription| item)) t))
	  (dolist (term *consumed-terms*)
	    (if (and (not (eq term item))
		     (term-eq-p term item))
		(return t))))
      'identifiable
      (or (global-fetch-feature 'identifiability-q item)
	  (if (and (term-type-p item '|spatialTemporalCircumstance|)
		   (fetch-subc-feature '|range| item))
	      (global-fetch-feature 'identifiability-q (fetch-subc-feature '|range| item))))))

(defun Identified-ID-Code (identity)
  "Return the domain of the IDENTITY."
  (fetch-subc-feature '|domain| identity))

(defun Identifier-ID-Code (identity)
  "Return the range of the IDENTITY."
  (fetch-subc-feature '|range| identity))

(defun Identity-Q-Code (reln)
  "We just look to see if the variable--a relation--is an identity."
  (if (term-type-p reln 'UM-Identity) 'identity 'nonidentity))

(defun Identity-Questioning-Q-Code (referrent)
  "Provisional:  if the parent relation is an IDENTITY, then IDENTITYQUESTIONING."
  (if (term-type-p (term-graph-parent (symbol-value referrent)) 'Identity)
      'identityquestioning
      'nonidentityquestioning))

(defun Inflected-Form-Q-Code (quality)
  
  (let ((features (fetch-lexicon-info quality 'features)))
    (when features
      (when (not (member 'more-most features)) 'inflected))))

(defun Item-Value-ID-Code (item property)
  "Give back the item."
  (declare (ignore property))
  item)

(defun Joint-Regard-Q-Code (reln)
  "It is joint for contrastive and elaboration relations."
  (if (or (term-type-p reln '|uio#RSTContrastive|)
	  (term-type-p reln '|uio#RSTElaboration|)) 'joint 'notjoint))

#+ignore
(defun Know-Manner-Q-Code (process)
  "Look for a manner (means) feature."
  (if (or (fetch-subc-feature '|generalizedMeans| process)
	  (fetch-subc-feature '|similarity| process))
      'known))

; mick patch 11/15/90 - allow instrument to become agent 
;  See also Agent-Mention-Q-Code, Causer-ID-Code
(defun Know-Manner-Q-Code (process)
  "Look for a manner (means) feature.(unless it is to be an actor)"
  ;; problem: if there are other generalised means, they should become manner
  (if (or (and (fetch-subc-feature '|generalizedMeans| process)
	       ;; following eliminates manner if instrument is to be actor
	       (not (and (fetch-subc-feature '|instrumental| process)
			 (term-type-p process '|DoingANDHappening|)
			 (not (fetch-subc-feature '|actor| process)))))
	  (fetch-subc-feature '|similarity| process))
       'known))
 
(defun Latitude-Direction-Specification-Q-Code (concept)
  "Negative numbers are south latitudes."
  (if  (< concept 0) 'southwards 'northwards))

(defun Liking-Q-Code (mentalprocess)
  "Is it of type LIKING?"
  (if (term-type-p mentalprocess '|Liking|) 'liking 'nonliking))


(defun new-Location-Classification-ID-Code (object)
  "Return the spatial feature of the object."
  (fetch-subc-feature '|space#placement| object))


(defun Location-Classification-ID-Code (object)
  "Return the spatial feature of the object."
  (fetch-relation-spec object 'static-spatial 'LOCATION-CLASSIFICATION-ID))

(defun new-Location-Classification-Q-Code (object)
  "Look for a spatial feature of the object."
  (if (and (fetch-subc-feature '|space#placement| object)
	   (not (pledged-p (fetch-subc-feature '|space#placement| object)))) 'location))

(defun Location-Classification-Q-Code (object)
  "Look for a spatial feature of the object."
  (if (and (fetch-subc-feature 'static-spatial object)
	   (not (pledged-p (fetch-subc-feature 'static-spatial object)))) 'location))


(defun Location-Q-Code (reln)
  "We just look to see if the variable--a relation--is a location."
   (if (term-type-p reln '|GeneralizedLocating|) 'location 'nonlocation))

(defun Location-Relation-Specificity-Q-Code (locn)
  "This should involve all sorts of neat reasoning that includes decisions
   on the basis of the process type; location participants are less likely
   to require explicitly specified relations. Meanwhile however....
   If the RANGE of the location relation LOCN is RELATIVE, then UNSPECIFIED." 
  (if (or (term-type-p locn '|RelativeSpatialTemporal|)
	  (term-type-p locn '|SimpleQuality|)
	  (and (term-type-p locn '|Date|)
	       (not (or (fetch-subc-feature '|Day| locn)
			(fetch-subc-feature '|Month| locn)
			(fetch-subc-feature '|Year| locn)))))
      'unspecified 
      'specified))

(defun LocationType-Q-Code (object)
  (if (term-type-p object '|Date|) 'temporal 'spatial))

(defun LocativeAddress-Q-Code (object)
  "Look for a date."
  (if (or (and (term-type-p object '|Date|)
	       (or (fetch-subc-feature '|Day| object)
		   (fetch-subc-feature '|Month| object))
	       (or (fetch-subc-feature '|Month| object)
		   (fetch-subc-feature '|Year| object)))
	  (and (term-type-p object '|ZeroDLocation|)
	       (fetch-subc-feature '|Latitude| object)
	       (fetch-subc-feature '|Longitude| object)))
      'locativeaddress 
      'notlocativeaddress))

(defun Logical-Condition-Q-Code (part1 part2)
  "See if the parent is a LOGICAL-CONDITION."
  "Or a CONDITION."
  ;(declare (ignore part2))
  (if (or (term-type-p (term-graph-parent (symbol-value part1)) '|uio#LogicalCondition|)
	  (term-type-p (term-graph-parent (symbol-value part1)) '|condition|)
	  (if (fetch-subc-feature '|condition| part1) 
	      (term-eq-p part2 (fetch-subc-feature '|condition| part1))))
      'logicalcondition 
      'nonlogicalcondition))

(defun Longitude-Direction-Specification-Q-Code (concept)
  "Negative numbers are west longitudes."
  (if (< concept 0) 'westwards 'eastwards))

(defun Manipulation-Q-Code (reln range)
  "If type of RELN is INSTRUMENTAL...."
  (declare (ignore range))
  (if (term-type-p reln '|instrumental|) 'manipulation 'nonmanipulation))

(defun Manner-Condition-Q-Code (part1 part2)
  "See if the parent is a MANNER-CONDITION."
  (declare (ignore part2)) 
  (if (or (term-type-p (term-graph-parent (symbol-value part1)) '|manner|)
	  (term-type-p (term-graph-parent (symbol-value part1)) '|uio#RSTMeans|))
      'mannercondition 
      'nonmannercondition))

(defun Manner-Q-Code (reln process)
  "If the type of RELN is MANNER...."
  (declare (ignore process))
  (if (or (term-type-p reln '|manner|)
	  (term-type-p reln '|agentive|)) 'manner 'nonmanner))

(defun Material-Act-Category-ID-Code (process)
  "Return the actee."
  (fetch-subc-feature '|actee| process))

(defun Material-Act-Category-Q-Code (process)
  "Look for nondirectedness."
  (if (and (term-type-p process '|NonAffectingAction|)
	   (fetch-subc-feature '|actee| process))
      'actspecified 
      'notactspecified))

(defun Material-Class-ID-Code (obj)
  "Return the material property."
   (or (fetch-subc-feature '|MaterialPropertyAscription| obj)
       (fetch-subc-feature '|domain| (fetch-relation '|MaterialPropertyAscription| obj))))

(defun Material-Classification-Q-Code (obj)
  "Look for a MATERIAL-PROPERTY-ASCRIPTION feature."
  (if (fetch-relation '|MaterialPropertyAscription| obj) 'material))

(defun Matter-ID-Code (process)
  "Return the role feature of the entry term with its value."
  (fetch-relation-spec process '|SubjectMatter| 'matter-ID))

(defun Matter-Q-Code (process)
  "Look for a subject matter feature."
  (if (fetch-relation '|SubjectMatter| process) 'matter 'nonmatter))

(defun Matter-Relation-Q-Code (reln)
  "Is it a subject-Matter relation?"
  (if (term-type-p reln '|SubjectMatter|) 'matterrelation 'notmatterrelation))

(defun Means-Of-Transportation-Q-Code (object)
  "Is it a vessel?"
  (if (term-type-p object '|Ship|) 'meansoftransportation 'notmeansoftransportation))

(defun Means-Q-Code (manner process)
  "Check that the feature is a (generalized) means."
  (declare (ignore process))
  (if (or (term-type-p manner '|uio#RSTMeans|)
	  (and (term-type-p  manner '|generalizedMeans|)
	       (not (term-type-p manner '|manner|))))
      'means
      'nonmeans))

#+ignore
(defun Medium-Mention-Q-Code (spec)
  "Just look to see if there is an ACTEE role."
  (if (fetch-subc-feature '|actee| spec) 'mention))

(defun Medium-Mention-Q-Code (spec)
  "Just look to see if there is an ACTEE or ACTOR role, depending on the type of action."
  (if (or (and (term-type-p spec '|NonAffectingAction|)
	       (fetch-subc-feature '|actor| spec))
	  (and (term-type-p spec '|AffectingAction|)
	       (fetch-subc-feature '|actee| spec)))
      'mention))

;(defun Member-Set-Q-Code (thing1 thing2)
;  "The first test is EQ of term-symbol or term-id.  (Use Same-As-Q-Code)
;   Also, we look for a part/whole (thing1/thing2) relation."
;  (if (or (eq (Same-As-Q-Code thing1 thing2) 'same)
;	  (term-role-p thing1 thing2 'generalized-possession t)
;	  (if (and (symbolp thing1)
;		   (boundp thing1)
;		   (typep (symbol-value thing1) 'term-graph)) 
;	      (term-eq-p thing2 (fetch-subc-feature 'part thing1)))
;	  (if (and (symbolp thing2)
;		   (boundp thing2)
;		   (typep (symbol-value thing2) 'term-graph)
;		   (fetch-reified-relation 'part-whole thing2)) 
;	      (term-eq-p 
;		thing1 
;		(fetch-subc-feature 'domain (fetch-reified-relation 'part-whole thing2)))))
;      'included))

;(defun Member-Set-Q-Code (thing1 thing2)
;  "The first test is EQ of term-symbol or term-id.  (Use Same-As-Q-Code)
;   Also, we look for a part/whole (thing1/thing2) relation."
;  (if (or (eq (Same-As-Q-Code thing1 thing2) 'same)
;	  (term-role-p thing1 thing2 'part-whole t)
;	  (if (and (symbolp thing1)
;		   (boundp thing1)
;		   (typep (symbol-value thing1) 'term-graph)) 
;	      (term-eq-p thing2 (fetch-subc-feature 'part thing1)))
;	  (if (and (symbolp thing2)
;		   (boundp thing2)
;		   (typep (symbol-value thing2) 'term-graph)
;		   (or (fetch-reified-relation 'part-whole thing2)
;		       (fetch-reified-relation '|GeneralizedRoleRelation| thing2))) 
;	      (term-eq-p 
;		thing1 
;		(fetch-subc-feature 
;		  'domain 
;		  (or (fetch-reified-relation 'part-whole thing2)
;		      (fetch-reified-relation '|GeneralizedRoleRelation| thing2)))))
;;	  (dolist (plan *plan-graphs*)
;;	    ;;; A set of cases to minimaze global search....
;;	    (if (term-graph-features (symbol-value plan))
;;		(if (and (term-role-p thing1 plan 'generalized-possession t)
;;			 (term-role-p plan thing2 'generalized-possession t))
;;		    (return t))))
;	  )
;      'included))

(defun Member-Set-Q-Code (thing1 thing2)
  "The first test is EQ of term-symbol or term-id.  (Use Same-As-Q-Code)
   Also, we look for a part/whole (thing1/thing2) relation."
  (if (or (eq (Same-As-Q-Code thing1 thing2) 'same)
	  (term-role-p thing1 thing2 '|PartWhole| t)
	  (if (and (symbolp thing1)
		   (boundp thing1)
		   (typep (symbol-value thing1) 'term-graph)) 
	      (term-eq-p thing2 (fetch-subc-feature '|Part| thing1)))
	  (if (and (symbolp thing2)
		   (boundp thing2)
		   (typep (symbol-value thing2) 'term-graph)
		   (or (fetch-reified-relation '|PartWhole| thing2)
		       (fetch-reified-relation '|GeneralizedRoleRelation| thing2))) 
	      (term-eq-p 
		thing1 
		(fetch-subc-feature 
		  '|domain| 
		  (or (fetch-reified-relation '|PartWhole| thing2)
		      (fetch-reified-relation '|GeneralizedRoleRelation| thing2)))))
	  (if (and (symbolp thing1)                                            ;;;  EX-SET-4, EX-SET-13
		   (boundp thing1)
		   (typep (symbol-value thing1) 'term-graph)
		   (eq (term-graph-type (symbol-value thing1)) '|UMSet|))
	      (dolist (elt (term-graph-symbol (symbol-value thing1)))
		(if (member-set-q-code elt thing2) (return t))))
	  (if (and (symbolp thing2)                                            ;;;  EX-SET-4, EX-SET-13                      
		   (boundp thing2)
		   (typep (symbol-value thing2) 'term-graph)
		   (eq (term-graph-type (symbol-value thing2)) '|UMSet|))
	      (dolist (elt (term-graph-symbol (symbol-value thing2)))
		(if (member-set-q-code elt thing1) (return t))))
;	  (dolist (plan *plan-graphs*)
;	     A set of cases to minimaze global search....
;	    (if (term-graph-features (symbol-value plan))
;		(if (and (term-role-p thing1 plan 'generalized-possession t)
;			 (term-role-p plan thing2 'generalized-possession t))
;		    (return t))))
	  )
      'included))

(defun Mental-Process-Q-Code (process)
  "Is the process mental?"
  (if (term-type-p process '|Internal|) 'mental 'nonmental))

(defun Million-Position-ID-Code (number)
  "Return the integer part of the quotient."
  (floor number 1000000))

(defun Million-Position-Q-Code (number)
  "Only if greater than 999999."
  (if (> number 999999) 'millionposition 'notmillionposition))

; john patch 20/5/96 - was just wrong I think...
(defun Million-Unit-Q-Code (entity)
  "Only if greater than 1000000."
  (if (>= entity 1000000) 'million 'notmillion))

(defun Minimal-Attention-Q-Code (obj)
  "If OBJ has no features, then minimal."
  (if (and (symbolp obj)
	   (boundp obj)
	   (typep (symbol-value obj) 'term-graph)
	   (null (term-graph-features (symbol-value obj))))
      'minimal
      'nonminimal))

(defun Modal-Necessity-Q-Code (modality process)
  "NECESSITY, if the type of MODALITY is."
  (declare (ignore process))
  (if (term-type-p modality '|Necessity|) 'necessity 'nonnecessity))

(defun Modality-Conditionality-Q-Code (modality)
  "Ask if MODALITY is CONDITIONAL."
  (if (term-type-p modality '|Conditional|) 
      'modalityconditional 
      'modalitynonconditional))

(defun Modality-ID-Code (process)
  "Return the modality feature."
  (fetch-subc-feature '|ModalPropertyAscription| process))

(defun Modality-Q-Code (process)
  "Look for a modality feature."
  (if (fetch-subc-feature '|ModalPropertyAscription| process ) 'modal))

(defun MODIFICATION-SPECIFICATION-ID-CODE (position hub)
  (specification-modification position hub))

(defun Modifying-Relation-Q-Code (concept)
  "Modifying if CONCEPT is a RELATIONAL-PROCESS."
  (if (term-type-p concept '|BeingANDHaving| t) 'modifying 'nonmodifying))

(defun Multiple-Process-Q-Code (onus)
  "Check the clause-complexity feature of onus.
   Verbal and mental processes with, respectively, saying or phenomenon roles which
   are processes will be multiple.
   Also, any ONUS which is a SET (or DISJUNCTIVE-SET) will be multiple."
  (if (or (term-type-p onus '|UMSet|)
	  (term-type-p onus '|DisjunctiveSet|)
	  (term-type-p onus '|space#temporalOrdering|)                                            ;;; PRIMER-49
	  (and (fetch-subc-feature '|space#temporalOrdering| onus)                                ;;; PRIMER-49
	       (or (term-type-p (fetch-subc-feature '|space#temporalOrdering| onus) '|Process|)     
		   (eq 'multiple (multiple-process-q-code (fetch-subc-feature '|space#temporalOrdering| onus))))
	       (not (pledged-p (fetch-subc-feature '|space#temporalOrdering| onus))))
	  (and (fetch-subc-feature 'static-spatial onus)                                   ;;; EX-SET-13
	       (or (term-type-p (fetch-subc-feature 'static-spatial onus) '|Process|)
		   (eq 'multiple (multiple-process-q-code (fetch-subc-feature 'static-spatial onus))))
	       (not (pledged-p (fetch-subc-feature 'static-spatial onus))))
	  (and (fetch-subc-feature '|condition| onus)
	       (or (term-type-p (fetch-subc-feature '|condition| onus) '|Process|)
		   (multiple-process-q-code (fetch-subc-feature '|space#temporalOrdering| onus))) ;;; PRIMER-49
	       (not (pledged-p (fetch-subc-feature '|condition| onus))))
	  (term-type-p onus '|Conjunction|)     ;;; The logical way to form sets of processes!
	  (term-type-p onus '|Disjunction|)
	  (and (term-type-p onus '|External|)
	       (term-type-p (fetch-subc-feature '|saying| onus) '|Process|)
	       (not (< 1 (count onus *consumed-terms*))))   ;;;  TO ALLOW DIRECT QUOTE:  
	                                                    ;;;  ONE AND THE SAME PROCESS IS BOTH MULTIPLE AND SINGLE
	  (and (term-type-p onus '|Internal|)
	       (term-type-p (fetch-subc-feature '|phenomenon| onus) '|Process|))
	  (and (term-type-p onus '|uio#RhetoricalRelation|)
;	       (not (term-type-p onus 'generalized-means))
	       (fetch-subc-feature '|domain| onus)  ;;;  Elaboration, for instance, may
	       (fetch-subc-feature '|range| onus))) ;;;  have just RANGE filled, and is
      'multiple                                   ;;;  in that case not MULTIPLE.
      'single))

(defun Multiplicity-Q-Code (thing)
  "Look for number.  If THING isn't a plan, look for a plan for it.
   Even if it isn't a plan, look at other TERM-EQ-P plans for multiplicity
   (this is important for agreement in number in dependent clauses)."
  (if (and (symbolp thing)
	   (boundp thing)
	   (typep (symbol-value thing) 'term-graph))
      (if (or (eq (fetch-feature-symbol '|Number| thing) 'multiple)
	      (and (fetch-subc-feature '|QuantityAscription| thing)
		   (let ((n (fetch-subc-feature '|QuantityAscription| thing)))
		     (if (numberp n)
			 (< 1 n)
		       (and (term-type-p n '|Number|)              ;; 1.1 R3b11
			    (numberp (fetch-feature 'value n))
			    (< 1 (fetch-feature 'value n))))))
	      (term-type-p thing '|UMSet|)
	      (dolist (graph *plan-graphs*)
		(if (and (not (eq thing graph))
			 (term-eq-p thing graph)
			 (or (eq (fetch-feature 'multiplicity-q graph) 'multiple)
			     (eq (fetch-feature-symbol '|Number| graph) 'multiple)
			     (term-type-p graph '|UMSet|)))
		    (return t))))
	  'multiple)
      (if (null thing)
	  nil
	  (Multiplicity-Q-Code
	    (dolist (graph *plan-graphs*)
	      (if (term-eq-p graph thing)
		  (return)))))))

(defun NUMBER-VALUE-ID-CODE (spec)
  (if (and (symbolp spec)
	   (boundp spec)
	   (typep (symbol-value spec) 'term-graph)
	   (term-type-p spec '|Number|))
      (fetch-feature 'value spec)
    spec))

(defun NUMBER-RELATIVITY-Q-CODE (number)
  (if (or (numberp number)
	  (and (symbolp number)
	       (boundp number)
	       (typep (symbol-value number) 'term-graph)
	       (term-type-p number '|Number|)))
      'absolute 'relative))


(defun Necessity-Q-Code (conjunctive)
  "Provisional implementation:  a guess."
  (if (and (term-type-p conjunctive '|conjunctive|)
	   (term-type-p conjunctive '|Causal|)) 
      'necessity 
      'nonecessity))

(defun Number-Focusing-Extent-Q-Code (numrel)
  "Depends on type of NUMREL."
  (if (term-type-p numrel '|Exactly|)
      'morefocused
      'lessfocused))

(defun Number-Focusing-ID-Code (number)
  "Find the appropriate feature that is filled by NUMBER."
  (dolist (graph *plan-graphs*)
	(if (term-role-p graph number '|NumberFocusing|)
	    (return (dolist (feature (term-graph-features (symbol-value graph)))
		      (when (eq (rest feature) number)
			(return (first feature))))))))

(defun Number-Focusing-Q-Code (numrel)
  "Provisional implementation:  a guess."
  (if (dolist (graph *plan-graphs*)
	(if (term-role-p graph numrel '|NumberFocusing|)
	    (return t)))
      'focusing
      'nonfocusing))

(defun Number-Of-Thousands-Q-Code (number)
  "Just say the right thing about the number of thousands."
  (let ((thousands (if (< number 1000)
		       number 
		       (floor number 1000))))
    (cond ((< thousands 10) 'subten)
	  ((> thousands 99) 'hundreds)
	  (t 'supraten))))

(defun Number-Range-Direction-Q-Code (numrel bogus-numrel)
  "Provisional implementation:  a guess."
  (declare (ignore bogus-numrel))
  (if (or (term-type-p numrel 'more-than)
	  (term-type-p numrel '|AtLeast|))
      'above
      'below))

(defun Operand-ID-Code (reln)
  "RELN is the ID of a reified relation spec.  Return the value of its range feature."
  (setf t1 (encaps-fetch-subc-feature '|range| (encaps-fetch-subc-feature-2 '|attribute| reln)))
  (setf t2 (fetch-subc-feature '|range| reln))
  (setf ret t2)
  (if (eq nil ret) 
      (setf ret (fetch-subc-feature '|space#temporalLocating| reln))) 
  (or ret reln)
  )

(defun old-Operand-ID-Code (reln)
  "RELN is the ID of a reified relation spec.  Return the value of its range feature."
  (setf t1 (fetch-subc-feature '|range| reln))
  (or (fetch-subc-feature '|range| reln)
      reln))

;;if there is no range feature, the term is probably a new spl term -> go into  substructure
(defun w-Operand-ID-Code (reln)
  "RELN is the ID of a reified relation spec.  Return the value of its range feature."
  (setf t1 (fetch-subc-feature '|range| reln))
  (setf t2 (fetch-subc-feature '|attribute| reln))
  (setf t3 (fetch-subc-feature '|range| t2))
  nil
)


(defun lala-operand-id-code (reln) 
  (setf t1 (fetch-subc-feature '|range| reln)) 
  (setf t2 (fetch-subc-feature '|attribute| reln)) 
  (if (not t1) (setf t3 (fetch-subc-feature '|range| t2)) (setf t3 nil))
  (setf ret (if t1 t1 (if t3 t3 reln))) 
  ret
)



(defun Operator-ID-Code (reln)
  "RELN is the ID of a reified relation spec.  Return its type." 
(or (fetch-subc-feature '|space#hasSpatialModality| reln) reln))

 
(defun orientation-q-code (reln range) 
  
  (setf temp1 (term-graph-parent (symbol-value range)))
  (setf temp2 (fetch-subc-feature '|space#hasSpatialModality| temp1))

  
  (if (term-type-p temp2 '|space#ProjectionRelation|) (setf ret 'oriented) (setf ret 'nonoriented))
  ret
  )


(defun old-Orientation-Q-Code (reln range)
  "If RELN is an ORIENTING. then ORIENTED."
  (declare (ignore range)) 
  (if (term-type-p reln '|orienting|) 'oriented 'nonoriented))

(defun Orientation-Specification-Q-Code (reln range)
  "If the type of RELN is Horizontal, but not facing,behind or between...."
  (setf t1 (fetch-subc-feature '|space#hasSpatialModality| range))
  (when (not t1)   (setf t1 (fetch-feature '|space#hasSpatialModality| (fetch-subc-feature '|attribute| range))) )

  (if (and (term-type-p t1 '|space#HorizontalProjection|)
			(or (term-type-p t1 '|space#FrontProjection|)
			    (term-type-p t1 '|space#BackProjection|)
			    (term-type-p t1 '|between|)))
      'specified 
      'notspecified))

;;; Following implementation is completely off. Have not yet
;;; checked why it was done or what taking it out entails
;;; for ISI-released examples. John:IPSI:Nov94
#+ignore
(defun Paragraph-Theme-Exist-Q-Code (process)
  "Do NOT, repeat NOT Look for a THEME feature!"
  (if (fetch-feature '|theme| process) 'exists))

(defun Paragraph-Theme-Exist-Q-Code (process)
  "Do NOT, repeat NOT Look for a THEME feature!"
  nil)

(defun Paragraph-Theme-ID-Code (process)
  "Return the THEME."
  (first (term-graph-symbol (symbol-value (fetch-feature '|theme| process)))))

(defun Parallel-Q-Code (reln range)
  "Depends on type of RELN."
  (declare (ignore range)) 
  (if (term-type-p reln '|parallelExtent|) 'parallel
      (if (term-type-p reln '|nonparallelExtent|) 'nonparallel)))

(defun Participation-Q-Code (reln range)
  "If RELN is of type INCLUSIVE."
  (declare (ignore range)) 
  (if (term-type-p reln '|inclusive|) 'participation 'nonparticipation))

(defun Partitive-ID-Code (ps)
  "Just get the parent term."
  (term-graph-parent (symbol-value ps)))

(defun Path-Inclusion-Q-Code (path1 path2)
  "CONTAINED if PATH1 is thematic."
  (if (eq path1 'thematic-path) 'contained 
      (if (eq path2 'thematic-path) 'notcontained)))

(defun Perception-Q-Code (mental-process)
  "If type of MENTAL-PROCESS is perception...."
    (if (term-type-p mental-process '|Perception|) 'perception 'notperception))

(defun Perceptive-Reactive-Phenomenon-ID-Code (mental-process)
  "Return the PHENOMENON feature."
  (fetch-subc-feature '|phenomenon| mental-process))

(defun Perceptive-Reactive-Phenomenon-Q-Code (mental-process)
  "Look for a PHENOMENON feature (which is not a process)."
  (if (and (fetch-subc-feature '|phenomenon| mental-process)
	   (not (term-type-p (fetch-subc-feature '|phenomenon| mental-process) '|Process|)))
      'phenomenon 
      'notphenomenon))

(defun Period-Extremal-Q-Code (relation)
  "Is the relation extremal?"
  (if (term-type-p relation '|extremal|) 'periodextremal 'notperiodextremal))

(defun Period-Modification-Part-ID-Code (obj)
  "Return the PERIOD feature."
  (fetch-subc-feature '|temporalRelation| obj))

;(defun Period-Modification-Q-Code (obj)
;  "Look for a PERIOD feature."
;  (if (fetch-subc-feature 'temporal-relation obj) 'period))

(defun Period-Modification-Q-Code (obj)
  "Look for a PERIOD feature."
  (if (fetch-subc-feature '|TemporalNonOrdering| obj) 'period))

(defun Phoric-Representative-Selection-Q-Code (concept)
  "Provisional implementation:  a guess."
  (if (term-type-p (term-graph-parent (symbol-value concept)) '|identity|) 
      'phoric 
      'notphoric))


(defun Place-Qual-ID-Code (obj)
  "Return a spatial-locating relation, reifying if necessary."
   (fetch-relation-spec obj '|SpatialLocating| 'PLACE-QUAL-ID))

(defun Place-Qual-Q-Code (obj)
  "Look for an orientation."  ; generalized to handle more spatial mods -- BK 
  (if (and (fetch-subc-feature '|SpatialLocating| obj)
	   (not (pledged-p (fetch-subc-feature '|SpatialLocating| obj)))
	   (if (fetch-subc-feature 'static-spatial obj)
	       (not (term-eq-p
		      (fetch-subc-feature '|SpatialLocating| obj) 
		      (fetch-subc-feature 'static-spatial obj)))
	       t))
      'place))






(defun Polarity-ID-Code (speech-act)
  "Find the polarity feature of the speech-act term."
  (or (global-fetch-feature '|polarity| speech-act)
      (fetch-feature '|polarity| (get-symbol-term speech-act))))

(defun Polarity-Value-Q-Code (polarity)
  "Return the type of the polarity."
  (term-graph-type (symbol-value polarity)))

(defun Polarity-Variable-Q-Code (polarity)
  "Is the polarity variable?"
  (if (term-type-p polarity 'polarity-variable)
      'polarityvariable
      'notpolarityvariable))

(defun Portion-Modification-ID-Code (obj)
  "Invert & return the reified generalized possession feature."
  (invert-reified-relation-spec 
    (fetch-reified-relation '|GeneralizedPossession| obj) 
    'portion-modification-id))

(defun Portion-Modification-Q-Code (obj)
  "Is there an inverse generalized-possession feature?"
  (if (and (fetch-reified-relation '|GeneralizedPossession| obj)
	   (eq (term-graph-symbol 
		 (symbol-value
		   (fetch-subc-feature 
		     '|range| (fetch-reified-relation '|GeneralizedPossession| obj))))
	       (term-graph-symbol (symbol-value obj)))
	   (dolist (graph *plan-graphs* t)
	     (if (and (term-eq-p graph 
				 (fetch-subc-feature 
			      '|domain| (fetch-reified-relation '|GeneralizedPossession| obj)))
		      (member graph *consumed-terms*))
		 (return nil))))
      'portion))

(defun Portion-Modification-ID-Code (obj)
  "What is the portion feature?"
  (fetch-feature '|PartOf| obj))

(defun Portion-Modification-ID-Code (obj)
  "What is the portion feature?"
  (or (if (fetch-feature '|PartOf| obj) 
	  (fetch-relation-spec obj '|PartOf| 'Portion-Modification-ID))
      (if (term-type-p obj '|Part|)
	  (fetch-relation-spec obj '|domain| 'Portion-Modification-ID))
      (if (term-type-p obj '|PartOf|)
	  (fetch-relation-spec obj '|range| 'Portion-Modification-ID))
      (if (term-type-p obj '|participantInConfiguration|)
	  (fetch-relation-spec obj '|range| 'Portion-Modification-ID))))

(defun Portion-Modification-Q-Code (obj)
  "Is there a part-of feature?"
  (if (fetch-feature '|PartOf| obj) 'portion))

(defun Portion-Modification-Q-Code (obj)
  "Is there a part-of feature?
   Or is OBJ a functional relation (:domain only) of type PART?"
  (if (or (and (fetch-feature '|PartOf| obj) 
	       (not (pledged-p (fetch-feature '|PartOf| obj))))
	  (and (or (term-type-p obj '|Part|)
		   (term-type-p obj '|PartOf|)
		   (term-type-p obj '|participantInConfiguration|))
	       (functional-role-p obj)))
      'portion))

(defun Position-ID-Code (address)
  "Find day, if month/day, else find month for month/year."
  (or (fetch-feature '|Day| address)
      (fetch-feature '|Month| address)
      (fetch-feature '|Longitude| address)))

(defun Position-Specific-Spatial-Address-Type-Q-Code (concept)
 "Provisional implementation:  a guess."
 (if (and (term-type-p concept '|ZeroDLocation|)
	  (fetch-subc-feature '|Latitude|  concept)
	  (fetch-subc-feature '|Longitude| concept))
     '|LatitudeLongitude|
     'other))

(defun Possessed-ID-Code (relation)
  "We've already determined that RELATION is one of generalized possession, so RANGE is the 
   acceptable feature."
  (fetch-subc-feature '|range| relation))

(defun Possession-Onset-Q-Code (process)
  "Is there a recipient?"
  (if (or (fetch-feature '|beneficiary| process)
	  (fetch-feature '|recipient|   process)) 'possessioncreated))

(defun Possessive-Q-Code (reln)
  "We just look to see if the variable--a relation--is a kind of generalized possession."
  (if (term-type-p reln '|GeneralizedPossession|) 'possessive 'notpossessive))

(defun Possessor-ID-Code (relation)
  "We've already determined that RELATION is one of generalized possession, so DOMAIN is the 
   acceptable feature."
  (fetch-subc-feature '|domain| relation))

  
(defun Possessor-Mod-ID-Code (object)
  "Return the possessor feature."
  (or (if (fetch-reified-relation '|Ownership| object) 
	  (fetch-subc-feature '|domain| (fetch-reified-relation '|Ownership| object)))
      (if (fetch-relation '|OwnedBy| object) 
	  (or (fetch-subc-feature '|range| (fetch-relation '|OwnedBy| object))
	      (fetch-relation '|OwnedBy| object)))		  
      (fetch-subc-feature 'possessor object)
      (if (fetch-relation '|GeneralizedPossession| object) 
	  (fetch-subc-feature '|domain| (fetch-relation '|GeneralizedPossession| object)))
      (fetch-subc-feature '|domain| object)))

(defun Possessor-Rel-ID-Code (object)	;; duplication of possessor-mod-id 
  "Return the possessor feature."
  (or (if (fetch-reified-relation '|Ownership| object) 
	  (fetch-subc-feature '|domain| (fetch-reified-relation '|Ownership| object)))
      (if (fetch-relation '|OwnedBy| object)
	  (or (fetch-subc-feature '|range| (fetch-relation '|OwnedBy| object))
	      (fetch-relation '|OwnedBy| object)))	  
      (fetch-subc-feature 'possessor object)
      (if (fetch-relation '|GeneralizedPossession| object) 
	  (fetch-subc-feature '|domain| (fetch-relation '|GeneralizedPossession| object)))
      (fetch-subc-feature '|domain| object)))	  
	  
(defun Possessor-Modification-Q-Code (object)
  "Is there a possessor feature?
   Or is this a functional role?"
  (if (fetch-relation '|Ownership| object) 'possessor))

(defun Possessor-Modification-Q-Code (object)
  "Is there a possessor feature?"
  (if (or (fetch-reified-relation '|Ownership| object)
	  (fetch-relation '|OwnedBy| object)
	  (fetch-minimal-relation '|GeneralizedRoleRelation| object)
	  (and (not (term-type-p object '|PropertyAscription|))
	       (fetch-subc-feature '|domain| object)
	       (not (fetch-subc-feature '|range| object))))
      'possessor))

(defun Possessor-Questioning-Q-Code (item)
  "Depends on role-relation ITEM is involved in."
  (if (term-role-p item (term-graph-parent (symbol-value item)) '|GeneralizedPossession|)
      'possessorquestioning
      'nonpossessorquestioning))

(defun Possibility-Q-Code (modality process)
  "POSSIBILITY, if the type of MODALITY is."
  (declare (ignore process))
  (when (term-type-p modality '|Possibility|) 'possibility))

(defun Posterior-Q-Code (time-relation)
  "Is TIME-RELATION posterior?"
  (if (term-type-p time-relation '|posterior|) 'posterior 'notposterior))

(defun Potential-Representative-ID-Code (obj) obj)

(defun Precede-Q-Code (time-1 time-2)
  "Compare what is known about the times"
  (if (term-type-p time-1 '|Past|)
      (if (not (term-type-p time-2 '|Past|)) 'precedes)
      (if (term-type-p time-1 '|Present|)
	  (if (term-type-p time-2 '|Future|) 'precedes)
	  (if (not (term-type-p time-1 '|Future|))
	      (if (term-type-p time-2 '|Future|) 'precedes)))))

(defun Preexist-Q-Code (goal process)
  "Only in a Creative-Material-Action does GOAL follow PROCESS!"
  (declare (ignore goal))
  (if (term-type-p process '|CreativeMaterialAction|) 'nonexistent 'existed))

(defun Prefer-Mention-Agent-Q-Code (actor activity)
  "Mention, especially notionally in imperatives, unless actor is unknown."
  (if (and 
       (not (term-type-p (fetch-feature 'Speech-Act-id activity) '|uio#Directive|))
       (or  (term-eq-p actor '?actor)
	    (term-eq-p actor '?sayer)))
      'withhold 
      'mention))

(defun Prefer-Mention-Medium-Q-Code (medium activity)
  "Mention, unless medium is unknown."
  (declare (ignore activity)) 
  (if (or (term-eq-p medium '?sayer)
	  (term-eq-p medium '?medium)
	  (term-eq-p medium '?senser))
      'withhold))

(defun Presentational-Complexity-Q-Code (arg)
  "Look for an ELABORATION type."
  (if (term-type-p arg '|uio#Elaboration|)
      'complex))

(defun Presentational-Relation-Q-Code (arg)
  "Look for an ELABORATION type."
  (if (term-type-p arg '|Elaboration|)
      'presentational
      'notpresentational))

(defun Presuppose-Existence-Q-Code (obj)
  "Yes, if this is an assertion."
  obj ;;;we don't need the parameter?
  (if (term-type-p (fetch-feature '|uio#SpeechAct|  *plan*) '|uio#Statement|) 
      'presupposed 'notpresupposed))

(defun Process-ID-Code (onus)
  "Find the process feature of the entry term, 
   or find the range of a rhetorical relation, or give the entry term back."
  (or (fetch-feature '|Process| onus)
      (if (term-type-p onus '|uio#RSTMeans|) (fetch-subc-feature '|domain| onus))
      (if (term-type-p onus '|uio#RhetoricalRelation|) 
	  (if (term-type-p (fetch-subc-feature '|range| onus) nil)
	      (first (get-top-level-terms (fetch-subc-feature '|range| onus)))
	      (fetch-subc-feature '|range| onus)))
      onus))

(defun Process-Manner-ID-Code (process)
  "Return the manner (means) feature, reifying if necessary."
  (if (fetch-subc-feature '|generalizedMeans| process)
      (if (fetch-subc-feature '|manner| process) 
	  (fetch-subc-feature '|manner| process) ;(fetch-relation-spec process 'manner 'Process-Manner-ID)
	  (fetch-relation-spec process '|generalizedMeans| 'Process-Manner-ID))
      (fetch-relation-spec process '|similarity| 'Process-Manner-ID)))

(defun Process-Modification-ID-Code (onus)
  "Return all process features and non-minimal reified relations."
  (let ((processes 
	  (remove-duplicates 
	    (remove nil 
		    (list (fetch-feature '|Process| onus)
			  (fetch-non-minimal-reified-relation '|BeingANDHaving| onus)
			  (fetch-non-minimal-reified-relation '|DoingANDHappening| onus)
			  (fetch-subc-feature '|uio#RSTElaboration| onus))))))
    (if processes
	(if (< 1 (length processes))
	    (make-term-graph-id 
	      :symbol processes :type '|UMSet| :id '|UMSet| :features nil :parent onus)
	    (first processes))
	(let ((role 
		(gentemp (symbol-name (fetch-subc-feature-name '|BeingANDHaving| onus)))))
	(make-term-graph-id 
	  :symbol (gentemp "ROLE-RELATION") 
	  :type '|GeneralizedRoleRelation|
	  :features `((|domain| . ,(symbol-value (make-term-graph-id 
				   :symbol (term-graph-symbol (symbol-value onus))
				   :id (term-graph-symbol (symbol-value onus)))))
		      (range . ,(symbol-value (fetch-subc-feature '|BeingANDHaving| onus)))
		      (temporal-definiteness-specification-q . specified)
		      (temporal-definiteness-q . definite)
		      (role-playing 
			. 
			,(symbol-value 
			   (make-term-graph-id 
			     :symbol role
			     :id (fetch-subc-feature-name '|BeingANDHaving| onus)
			     :type (fetch-subc-feature-name '|BeingANDHaving| onus)
			     :features 
			     `((relations 
				 . 
				 ,(symbol-value
				    (make-term-graph-id 
				      :type '|UMSet|
				      :symbol 
				      `(,(make-term-graph-id
					   :symbol (gentemp "ROLE-RELATION")
					   :type '|GeneralizedRoleRelation|
					   :features 
					   `((|domain| 
					       . 
					       ,(symbol-value 
						  (make-term-graph-id 
						    :symbol
						    (term-graph-symbol (symbol-value onus))
						    :id 
						    (term-graph-symbol 
						      (symbol-value onus)))))
					     (|range| 
					       .
					       ,(symbol-value 
						  (make-term-graph-id
						    :symbol role
						    :id (fetch-subc-feature-name 
							  '|BeingANDHaving| 
							  onus)))))))))))
			     ))))
	  :parent onus)))))

(defun Process-Modification-ID-Code (onus)
  "Return all process features and non-minimal reified relations."
  (let ((processes 
	  (remove-duplicates 
	    (remove nil 
		    (list (fetch-feature '|Process| onus)
			  (fetch-non-minimal-reified-relation '|BeingANDHaving| onus)
			  (fetch-non-minimal-reified-relation '|DoingANDHappening| onus)
			  (fetch-subc-feature '|uio#RSTElaboration| onus))))))
    (if processes
	(if (< 1 (length processes))
	    (make-term-graph-id 
	      :symbol processes :type '|UMSet| :id '|UMSet| :features nil :parent onus)
	    (first processes))
	(let ((role 
		(gentemp (symbol-name (fetch-subc-feature-name '|BeingANDHaving| onus)))))
	  (make-term-graph-id 
	    :symbol (gentemp "ROLE-RELATION") 
	    :type '|ClassAscription|
	    :features `((|domain| 
			  . 
			  ,(symbol-value 
			     (make-term-graph-id 
			       :symbol role
			       :id (fetch-subc-feature-name '|BeingANDHaving| onus)
			       :type (fetch-subc-feature-name '|BeingANDHaving| onus)
			       :features 
			       `((relations 
				   . 
				   ,(symbol-value
				      (make-term-graph-id 
					:type '|UMSet|
					:symbol 
					`(,(make-term-graph-id
					     :symbol (gentemp "ROLE-RELATION")
					     :type '|GeneralizedRoleRelation|
					     :features 
					     `((|domain| 
						 . 
						 ,(symbol-value 
						    (make-term-graph-id 
						      :symbol
						      (term-graph-symbol (symbol-value onus))
						      :id 
						      (term-graph-symbol 
							(symbol-value onus)))))
					       (range 
						 .
						 ,(symbol-value 
						    (make-term-graph-id
						      :symbol role
						      :id (fetch-subc-feature-name 
							    '|BeingANDHaving| 
							    onus)))))))))))
			       )))
			(range . ,(symbol-value (fetch-subc-feature '|BeingANDHaving| onus)))
			(temporal-definiteness-specification-q . specified)
			(temporal-definiteness-q . definite))
	    :parent onus)))))

(defun Process-Modification-Q-Code (onus)
  "Look for a process feature or a non-minimal reified relation or an elaboration."
  (let (process)
    (if (and (setf process 
		   (or (fetch-feature '|Process| onus)
		       (fetch-non-minimal-reified-relation '|BeingANDHaving| onus)
		       (fetch-non-minimal-reified-relation '|DoingANDHappening| onus)
		       (fetch-subc-feature '|uio#RSTElaboration| onus)
		       (and (not (KB-superp 
				   (fetch-subc-feature-name '|BeingANDHaving| onus) 
				   '|QuantityAscription|))
			    (not (KB-superp 
				   (fetch-subc-feature-name '|BeingANDHaving| onus) 
				   '|ElementOf|))
				(not (KB-superp 
				   (fetch-subc-feature-name '|BeingANDHaving| onus) 
				   '|OwnedBy|))   
				(not (KB-superp 
				   (fetch-subc-feature-name '|BeingANDHaving| onus) 
				   '|PartOf|)) 				   
			    (not (term-role-p onus 
					      (fetch-subc-feature '|BeingANDHaving| onus)
					      '|participantInConfiguration|))
			    (fetch-subc-feature '|BeingANDHaving| onus))))
	     (not (pledged-p process)))
      'process)))

(defun Process-Q-Code (item)
  "Just go by the type of ITEM."
  (if (and (term-type-p item '|Process|)
	   (not (term-role-p (term-graph-parent (symbol-value item)) item '|standard|)))
      'process 
      'nonprocess))

(defun Process-Regulated-Q-Code (process)
  "If it's a circumstantial...."
  (if (term-type-p process '|Circumstantial|) 'processregulated 'notprocessregulated))

(defun Processual-Q-Code (onus)
  "If it's a process...."
  (if (term-type-p onus '|Process|) 'processual 'nonprocessual))

(defun Projected-ID-Code (process)
  "Return the saying or phenomenon feature."
  (or (fetch-subc-feature '|saying| process)
      (fetch-subc-feature '|phenomenon| process)))

(defun Projection-Q-Code (onus)
  "Type of ONUS should indicate projection (e.g. verbal and mental processes)."
  (if (or (and (term-type-p onus '|External|)
	       (term-type-p (fetch-subc-feature '|saying| onus) '|Process|))
	  (term-type-p onus '|Internal|))
      'projected
      'notprojected))

(defun Projector-ID-Code (process)
  "Return the process."
  process)

(defun Projector-Time-ID-Code (projected)
  "Return the time of the projecting process."
  (or (fetch-feature 'reference-time-id (term-graph-parent (symbol-value projected)))
      'timenow))

(defun Prominent-Purpose-Relation-Q-Code (cause)

  (if (term-type-p (fetch-subc-feature '|range| cause) '|Process|) 'nonprominent 'prominent))

; john patch 20/5/96 - was unprepared for numbers...
(defun Property-Q-Code (onus)
  "Is it a quality?"
  (cond ((numberp onus)
	 'notproperty)
	((or (term-type-p onus '|SimpleQuality|)
	     (and (term-type-p onus '|PropertyAscription|)
		  (fetch-subc-feature '|domain| onus)
		  (not (fetch-subc-feature '|range| onus)))
	     (when (term-type-p (term-graph-parent (symbol-value onus)) '|SimpleThing|)
	       (and (term-type-p onus '|Process|)
		    (fetch-subc-feature '|participantInConfiguration| onus))))
	 'property)
	(t
	 'notproperty)))

(defun PLACEMENTIDEA (onus)
  "Which placement does the figure have?"
  (fetch-subc-feature '|attribute| onus))
  
(defun Property-Quality-Q-Code (onus)
  "Is it a quality?"
  (if (or (term-type-p onus '|manner|)
	  (term-type-p onus '|SimpleQuality|)
	  (term-type-p (fetch-subc-feature '|range| onus) '|SimpleQuality|))
      'property 
      'notproperty))

(defun Proposal-Q-Code (action)
  "If the type of the projector of ACTION is Non-Addressee-Oriented, then not proposal."
  (if (or (and (term-type-p (term-graph-parent (symbol-value action)) 
			    '|Internal|)
	       (not (term-type-p (term-graph-parent (symbol-value action)) 
				 '|ReactionAndEmotion|))
	       (fetch-subc-feature 'event-time action))
	  (term-type-p (term-graph-parent (symbol-value action)) 
		       '|NonAddressing|))
      'notproposal
      'proposal))

(defun Propositionality-Q-Code (onus)
  "Propositional, if it's a process and not a participant."
  (if (and (term-type-p onus '|Process|)
	   (term-graph-parent (symbol-value onus))
	   (if (not (term-role-p (term-graph-parent (symbol-value onus)) onus '|saying|))
	       (not 
		 (or (term-role-p (term-graph-parent (symbol-value onus)) onus '|participantInConfiguration|)
		     (term-role-p 
		       (term-graph-parent (symbol-value onus)) onus 'role-relation))) 
	       t))
      'propositional 
      'nonpropositional))

(defun Provenance-ID-Code (obj)
  "Return the feature."
  (fetch-relation-range '|ProvenancePropertyAscription| obj))

(defun Provenance-Modification-Q-Code (obj)
  "Look for a PROVENANCE feature."
  (if (fetch-minimal-relation '|ProvenancePropertyAscription| obj) 'Provenance))

(defun Purpose-Condition-Q-Code (part1 part2)
  "See if the parent is a PURPOSE."
  (declare (ignore part2))
  (if (term-type-p (term-graph-parent (symbol-value part1)) '|purpose|)
      'purposecondition 
      'nonpurposecondition))

#+ignore
(defun Purpose-ID-Code (process)
  "Return the purpose feature."
  (fetch-subc-feature '|purpose| process))

(defun Purpose-ID-Code (process)
  "Return the purpose feature."
  (fetch-relation-spec process '|purpose| 'Purpose-ID))

(defun Purpose-Q-Code (relation)
  "Is the relation a purpose feature?"
  (if (term-type-p relation '|purpose|) 'purpose 'nonpurpose))

(defun Purpose-Relation-Q-Code (relation)
  "Is the relation a purpose feature?"
  (if (term-type-p relation '|purpose|) 'purpose 'notpurpose))

(defun Quantification-ID-Code (spec)
  "Return the quantity-ascription feature."
  (fetch-subc-feature '|QuantityAscription| spec)
  ;(fetch-relation-spec spec '|QuantityAscription| 'QUANTIFICATION-ID)
)

(defun Quantification-Q-Code (spec)
  "Is there a quantity ascription?"
  (if (fetch-subc-feature '|QuantityAscription| spec) 'quantified))

(defun Quantity-Q-Code (object)
  "Is it a number?"
  (if (numberp object) 'quantity 'notquantity))

(defun Quantity-Selection-ID-Code (object)
  "Look for quantity feature."
  (fetch-subc-feature 'quantity-selection object))

(defun Quantity-Selection-Q-Code (object)
  "Look for quantity feature."
  (if (fetch-subc-feature 'quantity-selection object) 'quantity))

(defun Question-Q-Code (speech-act)
  "Is the type of the speech-act term QUESTION?"
  (if (term-type-p speech-act '|uio#Question|) 'question 'nonquestion))

(defun Question-Item-ID-Code (onus)
  "Return an appropriate participant, or else give back ONUS."
  (or (fetch-subc-feature '|domain| onus)
      (fetch-subc-feature '|actor| onus)
      onus))

(defun Quotation-Q-Code (projected projector)
  
  (declare (ignore projected))
  (when (fetch-subc-feature 'quote projector) 'quotation))

(defun Reaction-Q-Code (mentalprocess)
  "Is it of type REACTION?"
  (if (term-type-p mentalprocess '|ReactionAndEmotion|) 'reaction 'nonreaction))

(defun Reader-Knowledge-Path-ID-Code (subject1 subject2)
  "If the subjects are the same, then identify this as the thematic path."
  (if (term-eq-p subject1 subject2) 'thematic-path 'included-path))

(defun Reason-Q-Code (relation)
  "Is the relation a reason feature?"
  (if (term-type-p relation '|reason|) 'reason 'nonreason))

(defun Reason-ID-Code (process)
  "Return the REASON feature."
  (fetch-relation-spec process '|reason| 'Reason-ID))

(defun ReExpression-Q-Code (conjunctive)
  "Depends on type of conjunctive."
  (if (term-type-p conjunctive '|uio#Restatement|) 'reexpression 'notreexpression))

(defun Reference-Time-ID-Code (occurrence)
  "Find the event time feature of the occurrence term."
  (or (fetch-feature 'event-time occurrence)
      (fetch-feature 'event-time (get-symbol-term occurrence))))

(defun Referent-Specificity-Q-Code (reln range)
  "Look for a between feature."
  (declare (ignore range))
  (if (term-type-p reln '|SpecificMatter|) 
      'highspecificty
      (when (term-type-p reln '|DiffuseMatter|) 'lowspecificity)))

(defun Region-ID-Code (address)
  "find year, if month/year, else find month for day/month."
  (or (fetch-feature '|Year| address)
      (fetch-feature '|Month| address)
      (fetch-feature '|Latitude|  address)))

(defun Related-Conjunctive-ID-Code (conjrel)
  "If a DOMAIN role is specified, use that.  Otherwise, gensym."
  (or (fetch-feature '|domain| conjrel)
      (gentemp)))

(defun Relative-Position-Q-Code (relation)
  "This first approach just asks if RELATION is a RHETORICAL-RELATION."
  (if (term-type-p relation '|uio#RhetoricalRelation|) 'immediate 'notimmediate))

(defun Relative-Pronoun-Selection-Q-Code (participant)
  "See if the grand-parent of PARTICIPANT is an ELABORATION."
  (if  (and (term-graph-parent (symbol-value participant)) 
	    (term-graph-parent (symbol-value (term-graph-parent (symbol-value participant)))) 
	    (term-type-p 
	      (term-graph-parent (symbol-value (term-graph-parent (symbol-value participant))))
	      '|uio#RSTElaboration|))
      'which
      'that))

(defun Report-ID-Code (process)
  "Return the SAYING feature."
  (fetch-subc-feature '|saying| process))

(defun Report-Q-Code (process)
  "Use the parent of process to decide."
  (if (or (term-type-p (term-graph-parent (symbol-value process)) '|External|)
	  (term-type-p (term-graph-parent (symbol-value process)) '|Internal|))
      'report
      'nonreport))

(defun Resemblance-Q-Code (manner)
  ;;; Implemented for MANNER defined by PROCESS-MANNER-ID-CODE;
  ;;; seeking SIMILARITY role.
  (if (and (fetch-feature '|domain| manner) 
	   (fetch-feature '|range| manner) 
	   (term-eq-p (fetch-subc-feature '|similarity| (fetch-feature '|domain| manner))
		 (fetch-feature '|range| manner)))
      'resemblance))

(defun Restatement-Elaboration-Q-Code (one two)
  "If the parent is an EXEMPLIFICATION...."
  (declare (ignore two))
  (if (term-type-p (term-graph-parent (symbol-value one)) '|uio#Restatement|) 
      'restatement 
      'notrestatement))

(defun Role-ID-Code (process)
  "Return the role-playing feature."
  (fetch-relation-spec process '|RolePlaying| 'Role-ID))

(defun Role-Q-Code (process)
  "Look to see if there is a role-playing feature."
  (if (fetch-relation '|RolePlaying| process) 'rolerestricted 'notrolerestricted))

(defun Role-Relation-Q-Code (reln)
  "Look to see if the variable--a relation--is a role-playing."
  (if (term-type-p reln '|RolePlaying|) 'rolerelation 'notrolerelation))

(defun Same-As-Q-Code (first second)
  "Either first and second should be EQ, or their symbols should be."
  (or (if (and (boundp first)
	   (not (symbolp (symbol-value first))))
      (if (and (boundp second)
	       (not (symbolp (symbol-value second))))
	  (if (eq (term-graph-symbol (symbol-value first))
		  (term-graph-symbol (symbol-value second))) 'same)
	  (if (eq (term-graph-symbol (symbol-value first)) second) 'same))
      (if (and (boundp second)
	       (not (symbolp (symbol-value second))))
	  (if (eq first (term-graph-symbol (symbol-value second))) 'same)
	  (if (eq first second) 'same)))
      (if (and (boundp first)
	       (boundp second)
	       (typep (symbol-value first) 'term-graph)
	       (typep (symbol-value second) 'term-graph)
	       (eq (term-graph-type (symbol-value first)) '|UMSet|)
	       (eq (term-graph-type (symbol-value second)) '|UMSet|)
	       (not (set-difference 
		      (term-graph-symbol (symbol-value first)) 
		      (term-graph-symbol (symbol-value second)) 
		      :test 'term-eq-p)))
	  'same)))

(defun Sayer-ID-Code (process)
  "Return the SAYER feature."
  (fetch-subc-feature '|sayer| process))

(defun Scalability-Q-Code (dimension)
  "If the DIMENSION's range is a SCALABLE-QUALITY, then SCALABLE."
  (let ((quality (fetch-subc-feature '|range| dimension)))
    (if quality
	(if (term-type-p quality '|ScalableQuality|) 'scalable 'nonscalable))))

(defun Scaling-Q-Code (dimension)
  "If the DIMENSION is a scaled-comparison, then SCALED."
  (if (term-type-p dimension '|ScaledComparison|)
      'scaled 
      'nonscaled))

(defun Selection-Particularity-Q-Code (obj1 obj2)
  "For now, if it's unique, based on determiner."
  (declare (ignore obj2))
  (if (eq (fetch-feature '|determiner| obj1) 'one) 'particular))

(defun Senser-ID-Code (mentalprocess)
  "Return the senser feature."
  (fetch-subc-feature '|senser| mentalprocess))

(defun Sequence-Q-Code (conjunctive)
  "Depends on type of conjunctive."
  (if (term-type-p conjunctive '|anterior|) 'notsequence 'sequence))

(defun Set-Totality-Q-Code (ob1 ob2)
  "Who knows?"
  (declare (ignore ob2)) 
  (if (eq (fetch-feature '|determiner| ob1) 'all) 'total))

(defun Similarity-Relation-Q-Code (reln)
  "Is it a similarity relation?"
  (if (term-type-p reln '|similarity|) 'similarityrelation 'notsimilarityrelation))

(defun Singularity-Q-Code (item)
  "If (Multiplicity-Q ITEM) = MULTIPLE or type of item is QUALITY, then NONSINGULAR."
  (if (or (eq (Multiplicity-Q-code item) 'multiple)
	  (term-type-p item '|SimpleQuality|)
	  (term-type-p item '|Substance|)
	  ;(term-type-p item 'process)
	  (eq (fetch-feature-symbol '|Number| item) 'multiple)
	  (eq (fetch-feature-symbol '|Number| (get-symbol-term item)) 'multiple))
      'nonsingular))

(defun Size-Modification-Q-Code (obj)
  "Look for a SIZE feature."
  (if (fetch-minimal-relation '|SizePropertyAscription| obj) 'Size))

(defun Size-Modifier-ID-Code (obj)
  "Return the SIZE feature."
  (or (fetch-subc-feature '|SizePropertyAscription| obj)
      (fetch-feature '|range| (fetch-reified-relation '|SizePropertyAscription| obj))))

(defun old-Source-Destination-Process-Q-Code (locativerelation place)
  "Is the relation one of source-destination?"
  (declare (ignore place)) 
  (if (term-type-p locativerelation 'Source-Destination)
      'sourcedestination
      'nonsourcedestination))


(defun Source-Destination-Q-Code (reln process)
  "Is the relation one of source-destination?" 
  (if (or (term-role-p  process reln '|space#destination| nil)(term-role-p  process reln '|space#source| nil) 
 (term-role-p  process reln '|space#GeneralDirectional| nil))   'sourcedestination 'nonsourcedestination))

(defun Source-Destination-Process-Q-Code (reln process)
  "Is the relation one of source-destination?"
  (declare (ignore process))
  (if (or (term-type-p reln '|space#source|) (term-type-p reln '|space#destination|))
      'sourcedestination 
      'nonsourcedestination))

(defun Source-Process-Q-Code (locativerelation place)
  "Is the relation one of source?"
  (declare (ignore place)) 
  (if (term-type-p locativerelation '|space#source|)
      'source
      'nonsource))

(defun Source-Q-Code (reln process)
  "Is the relation a source?"
  (declare (ignore process)) 
  (if (term-type-p reln '|space#source|) 'source 'notsource))

(defun Space-Condition-Q-Code (part1 part2)
  "See if the parent is a SPACE-CONDITION."
  (if (or (term-eq-p part2 (fetch-subc-feature 'spatial-relation part1))
	  (and (term-type-p (term-graph-parent (symbol-value part1)) 'spatial-relation)
	       (fetch-subc-feature '|range| (term-graph-parent (symbol-value part1)))
	       (term-type-p (fetch-subc-feature '|range| (term-graph-parent (symbol-value part1))) '|Process|)))
      'spacecondition 
      'nonspacecondition))

(defun Spatial-Extent-ID-Code (process)
  "Return the spatial extent feature of the entry term with its value."
  (fetch-relation-spec process '|space#spatialExtent| 'SPATIAL-EXTENT-ID))

(defun Spatial-Extent-Specification-Q-Code (process)
  "Look for a spatial extent feature of the entry term."
  (if (fetch-relation '|space#spatialExtent| process) 'spatialextent))


;Returns a GeneralizedLocation that is connected to the main process through an "attribute" relation

(defun spatial-location-id-code (p)
(setf tt (fetch-subc-feature '|attribute| p))
(setf ttt (fetch-subc-feature '|range| (fetch-subc-feature '|attribute| p)))
tt
)


(defun sub-Spatial-Location-ID-Code (process)
  "Return a (possibly new) reified spatial location relation.
   Special case:  if the range is a relative location, then return it alone, eliminating the 
   relation." 
  (if (or (term-type-p (or (fetch-subc-feature '|attribute| process)
		       (fetch-subc-feature '|range| (fetch-relation '|attribute| process)))
		       '|GUMThing|)
	  (term-type-p (or (fetch-subc-feature '|attribute| process)
		       (fetch-subc-feature '|range| (fetch-relation '|attribute| process)))
		       '|SimpleQuality|)
          (term-type-p (fetch-subc-feature '|attribute| process) '|space#GeneralizedLocation|)
          )
      (or (fetch-subc-feature '|attribute| process)
	  (fetch-relation '|range| (fetch-subc-feature '|attribute| process))
          (fetch-relation '|attribute| process))
      (fetch-relation-spec process '|attribute| 'SPATIAL-LOCATION-ID)))
	  
(defun old-Spatial-Location-ID-Code (process)
  "Return a (possibly new) reified spatial location relation.
   Special case:  if the range is a relative location, then return it alone, eliminating the 
   relation."
  (if (or (term-type-p (or (fetch-subc-feature '|attribute| process)
		       (fetch-subc-feature '|range| (fetch-relation '|attribute| process)))
		       '|RelativeSpatialTemporal|)
	  (term-type-p (or (fetch-subc-feature '|attribute| process)
		       (fetch-subc-feature '|range| (fetch-relation '|attribute| process)))
		       '|SimpleQuality|))
      (or (fetch-subc-feature '|attribute| process)
	  (fetch-subc-feature '|range| (fetch-relation '|attribute| process)))
      (fetch-relation-spec process '|attribute| 'SPATIAL-LOCATION-ID)))

;(defun Spatial-Location-Specification-Q-Code (process)
;  "Look for a spatial location feature of the entry term."
;  (if (and (fetch-relation '|attribute| process)
;	   (if (fetch-subc-feature '|attribute| process)
;	       (not (pledged-p (fetch-subc-feature '|attribute| process)))
;	       t)
;	   (if (fetch-reified-relation '|attribute| process)
;	       (not (pledged-p (fetch-feature '|range| (fetch-reified-relation '|attribute| process))))
;	       t))
;      (if (and (term-type-p process '|Relating|)(not (term-type-p process '|Identity|))  ) 'nospatiallocation 'spatiallocation)))

;(defun Spatial-Location-Specification-Q-Code (process)
;  "Look for a spatial location feature of the entry term."
;  (if (term-type-p process '|space#SpatialLocating|) 'spatiallocation 'nospatiallocation))  
	  
(defun old-Spatial-Location-Specification-Q-Code (process)
  "Look for a spatial location feature of the entry term." 
  (if (and (fetch-relation '|attribute| process)
	   (if (fetch-subc-feature '|attribute| process)
	       (not (pledged-p (fetch-subc-feature '|attribute| process)))
	       t)
	   (if (fetch-reified-relation '|attribute| process)
	       (not (pledged-p (fetch-feature '|range| (fetch-reified-relation '|attribute| process))))
	       t))
      'spatiallocation))


(defun Spatio-Temporal-Type-Q-Code (reln)
  "Classify on the basis of type." 
  (if (term-type-p reln '|space#SpatialModality|)  'spatial 'temporal)) 

(defun old-Spatio-Temporal-Type-Q-Code (reln)
  "Classify on the basis of type."
  (if (term-type-p reln 'spatial-relation) 'spatial
      (if (term-type-p reln '|temporalRelation|) 'temporal)))

(defun Speaker-ID-Code (onus)
  "Return the SPEAKER feature?"
  (fetch-feature '|speaker| onus))

(defun Speech-Act-ID-Code (onus)
 (or (term-type-p onus '|UMSet|)
	      (term-type-p onus '|DisjunctiveSet|))
  "Find the speech-act feature of the entry term."
  (or (fetch-feature 'Speech-Act onus)
      (fetch-feature 'Speech-Act (get-symbol-term onus))
      (if (term-type-p onus '|BeingANDHaving|)
	  (fetch-feature 'Speech-Act-id (fetch-subc-feature '|domain| onus)))
      (if (or (term-type-p onus '|UMSet|)
	      (term-type-p onus '|DisjunctiveSet|))
	  (fetch-feature 'Speech-Act-id (first (term-graph-symbol (symbol-value onus)))))))


(defun Speaking-Time-ID-Code (speechact)
  "Return the time of the speech-act, or else TIMENOW."
  (or (fetch-feature 'speakingtime speechact)
      (fetch-feature 'speakingtime (get-symbol-term speechact))))

(defun Standard-ID-Code (quality dimension)
  "Return the STANDARD role of DIMENSION."
  (declare (ignore quality))
  (fetch-subc-feature '|standard| dimension))

(defun Standard-Mention-Q-Code (spec standard)
  "Mention, unless the special case."
  (declare (ignore spec))
  (when (term-eq-p standard '?standard) 'withhold))

(defun Standard-Value-ID-Code (standard property)
  "Let the standard speak for itself."
  (declare (ignore property))
  standard)

(defun Statement-Q-Code (speech-act)
  "Is the type of the speech-act term ASSERT?"
  (if (or (term-type-p speech-act '|uio#Directive| t)
		(term-type-p speech-act '|uio#Question| t))
  'nonstatement 'statement))

(defun Static-Condition-Q-Code (process)
  "Is the process relational?"
   (if (term-type-p process '|BeingANDHaving|) 'static 'nonstatic))

(defun Status-Modification-Q-Code (obj)
  "Look for any PROPERTY-ASCRIPTION feature, making sure you only pick out DOMAINs."
  (if (and (fetch-minimal-relation '|PropertyAscription| obj)
	   (and (not  (pledged-p (fetch-minimal-relation '|PropertyAscription| obj)))
		(if (fetch-feature '|range| (fetch-minimal-relation '|PropertyAscription| obj))
		    (not (pledged-p (fetch-feature 
				      '|range| 
				      (fetch-minimal-relation '|PropertyAscription| obj))))
		    t))
	   (if (fetch-subc-feature '|domain| 
			       (fetch-minimal-relation '|PropertyAscription| obj))
	       (eq (term-graph-symbol 
		 (symbol-value 
		   (fetch-subc-feature '|domain| 
				       (fetch-minimal-relation '|PropertyAscription| obj))))
	       (term-graph-symbol (symbol-value obj)))
	       t))
      'Status))

(defun Status-Modifier-ID-Code (obj)
  "Return the feature."
  (fetch-relation-range '|PropertyAscription| obj))

(defun Subclass-ID-Code (item)

  (fetch-feature '|ClassAscription| item))

(defun SubTen-Position-ID-Code (number)
  "The remainder of NUMBER when divided by ten gives the sub-ten position."
  (rem number 10))

;;; John: Apr96: this was also just wrong, failing to take account of teens.
(defun SubTen-Position-Q-Code (number)
  "If there's a remainder of NUMBER when divided by ten, there's a sub-ten position."
  (let ((supraten (supraten-position-id-code number)))
    (if (and (< supraten 20)(> supraten 10))
	'notsubtenposition
      (if (not (eql 0 (rem number 10)))
	  'subtenposition
	'notsubtenposition))))

;;; John: Apr96: this was also just wrong, failing to take account of teens.
(defun SupraTen-Position-ID-Code (number)
  "A wierd function of NUMBER gives the supra-ten position."
  (let ((tens (rem number 100)))
    (if (and (> tens 10)(< tens 20))
	tens
      (rem (- number (rem number 10)) 100))))

;;; John: Apr96: this was also just wrong, failing to take account of teens.
(defun SupraTen-Position-Q-Code (number)
  "A wierd function of NUMBER tells if there are or are not supra-ten positions."
  (if (eql (supraten-position-id-code number) 0)
      'notsupratenposition
      'supratenposition))

(defun Symbol-ID-Code (identity)
  "Return the domain of the IDENTITY."
  (fetch-subc-feature '|domain| identity))

(defun Symbolization-Q-Code (reln)
  "Depends on type of RELN."
  (if (term-type-p reln 'Symbolization) 'symbolization 'notsymbolization))

(defun Symbolized-ID-Code (identity)
  "Return the range of the IDENTITY."
  (or (fetch-subc-feature '|range| identity)
      (term-graph-parent (symbol-value identity))))

(defun Taxis-Prominence-Q-Code (part1 part2)
  "Probably depends on the type of conjunctive relation.
   For the time being, if they are just set elements, return NOTLESS."
  (if (or (term-type-p (term-graph-parent (symbol-value part1)) '|uio#symmetricRhetoricalRelation|)
	  (term-type-p (term-graph-parent (symbol-value part1)) '|Conjunction|)
	  (term-type-p (term-graph-parent (symbol-value part1)) '|Disjunction|)
	  (and (term-type-p (term-graph-parent (symbol-value part1)) '|uio#RhetoricalRelation|)
	       (term-type-p (term-graph-parent (symbol-value part1)) '|uio#Restatement|))
	  (and (or (term-type-p (term-graph-parent (symbol-value part1)) '|uio#RhetoricalRelation|)
		   (term-type-p (term-graph-parent (symbol-value part1)) '|space#temporalOrdering|)  ;;; PRIMER-49
		   (term-type-p (term-graph-parent (symbol-value part1)) 'static-spatial))
	       (eq part1 
		   (fetch-subc-feature '|domain| (term-graph-parent (symbol-value part1)))))
	  (and (fetch-subc-feature '|space#temporalOrdering| part1)                                  ;;; PRIMER-49
	       (or (term-type-p (fetch-subc-feature '|space#temporalOrdering| part1) '|Process|)
		   (eq 'multiple (multiple-process-q-code (fetch-subc-feature '|space#temporalOrdering| part1)))))
	  (and (fetch-subc-feature 'static-spatial part1)                                     ;;; EX-SET-13
	       (or (term-type-p (fetch-subc-feature 'static-spatial part1) '|Process|)
		   (eq 'multiple (multiple-process-q-code (fetch-subc-feature 'static-spatial part1)))))
	  (and (fetch-subc-feature '|condition| part1)
	       (or (term-type-p (fetch-subc-feature '|condition| part1) '|Process|)
		   (eq 'multiple (multiple-process-q-code (fetch-subc-feature '|condition| part1)))))
	  (dolist (graph *plan-graphs*)
	    (and (or (and (term-type-p graph '|UMSet|)
			  (not (term-type-p graph '|OrderedSet|)))
		     (term-type-p graph '|DisjunctiveSet|))
		 (or (member part1 (term-graph-symbol (symbol-value graph)))
		     (term-eq-p part1 graph))
		 (or (member part2 (term-graph-symbol (symbol-value graph)))
		     (term-eq-p part2 graph))
		 (return 't))))
      'notless
      'less))

(defun Teen-Q-Code (entity)
  "Just compare to twelve."  
  (if  (> entity 12) 'teen 'notteen))

(defun Teen-Type-Q-Code (entity)
  "Just give ENTITY back, spelled out."  
  (case entity
    (13 'thirteen)
    (14 'fourteen)
    (15 'fifteen)
    (16 `sixteen)
    (17 'seventeen)
    (18 'eighteen)
    (otherwise 'nineteen)))

(defun Temporal-Definiteness-Specification-Q-Code (process)
  "Look for an event-time feature of the entry term."
  (if (or (fetch-subc-feature 'event-time process)
	  (if (get-symbol-term process) 
	      (fetch-subc-feature 'event-time (get-symbol-term process)))) 
      'specified 
      'notspecified))

(defun Temporal-Enhancement-Q-Code (alphaid betaid)
  "Depends on relation between ALPHAID and BETAID."
  (if (term-role-p alphaid betaid '|temporalRelation|) 
      'temporalsuccession 
      'nottemporalsuccession))

(defun Temporal-Extent-ID-Code (process)
  "Return the temporal extent feature of the entry term and its value."
  (fetch-relation-spec process '|temporalExtent| 'TEMPORAL-EXTENT-ID))

(defun Temporal-Extent-Specification-Q-Code (process)
  "Look for a temporal extent feature of the entry term."
  (if (fetch-relation '|temporalExtent| process) 'temporalextent))

;;(defun old-Temporal-Location-ID-Code (process)
(defun Temporal-Location-ID-Code (process)
  "Return a (possibly new) reified temporal location relation.
   Special case:  if the range is a relative location, then return it alone, eliminating the 
   relation."
  (let ((time (or (fetch-subc-feature '|space#TemporalLocating| process) 
		  (fetch-subc-feature '|range| (fetch-relation '|space#TemporalLocating| process)))))
    (if (or (term-type-p time '|RelativeSpatialTemporal|)
;	    (and (term-type-p time 'date)
;		 (not (or (fetch-subc-feature 'day time)
;			  (fetch-subc-feature 'month time)
;			  (fetch-subc-feature 'year time))))
	    )
	time
	(fetch-relation-spec process '|space#TemporalLocating| 'TEMPORAL-LOCATION-ID))))

(defun Temporal-Location-Specification-Q-Code (process)
  "Look for a temporal location feature of the entry term." 
  (if (and (fetch-relation '|space#TemporalLocating| process)
	   (if (fetch-subc-feature '|space#TemporalLocating| process)
	       (not (pledged-p (fetch-subc-feature '|space#TemporalLocating| process)))
	       t)
	   (if (fetch-reified-relation '|space#TemporalLocating| process)
	       (not (pledged-p (fetch-feature '|range| (fetch-reified-relation '|space#TemporalLocating| process))))
	       t)  
		   )
      'temporallocation))

(defun Temporal-Qual-Q-Code (obj)
  "Look for a temporal-locating relation."  ;; Should we also find reified relations? -- BK
  (if (and (fetch-subc-feature '|space#TemporalLocating| obj)
	   (not (pledged-p (fetch-subc-feature '|space#TemporalLocating| obj))))
      'temporal))

(defun Ten-Digit-ID-Code (number)
  "Return the number of tens."
  (floor (mod number 100) 10))

(defun Ten-Digit-Q-Code (number)
  "Only if greater than 10."
  (if (> number 9) 'yes 'no))

;;;================================================================================
;;; KOMET-PENMAN: 
;;; Lexical resolution now done by separate code module 

(defun term-resolve-id-code (concept association)
  (LEXICAL-TERM-RESOLUTION concept association))

;;;================================================================================
		      
(defun Thousand-Digit-ID-Code (number)
  "Return the number of thousands."
  (floor number 1000))

(defun Thousand-Digit-Q-Code (number)
  "Only if greater than 999."
  (if (> number 999) 'yes 'no))

(defun Thousand-Position-ID-Code (number)
  "Return the integer part of the quotient."
  (mod (floor number 1000) 1000))

(defun Thousand-Position-Q-Code (entity)
  "Only if greater than 999."
  (if (or (and (> entity 1000000)
	       (> (- entity (* (floor entity 1000000) 1000000)) 999))
	  (and (< entity 1000000)
	       (> entity 999)))
      'thousandposition 
      'notthousandposition))

;;; ----------------------------------------
;;; Hacked: john. The numbers were misbehaving.
(defun Thousand-Unit-Q-Code (number)
  "Only if greater than 1000." 
  (if (>= number 1000)  'thousand 'notthousand))

(defun thousand-unit-value-id-code (number)
  (cond ((>= number 1000)
	 (floor (/ number 1000)))
	(t 
	  number)))
;;;----------------------------------------

(defun Time-Condition-Q-Code (part1 part2)
  "See if the parent is a TIME-CONDITION."
  "Or if there's a TEMPORAL-RELATION feature."
  ;(declare (ignore part2))
  (if (or (term-eq-p part2 (fetch-subc-feature '|temporalRelation| part1))
	  (and (term-type-p (term-graph-parent (symbol-value part1)) '|temporalRelation|)
	       (fetch-subc-feature '|range| (term-graph-parent (symbol-value part1)))
	       (term-type-p (fetch-subc-feature '|range| (term-graph-parent (symbol-value part1))) '|Process|)))
      'timecondition 
      'nontimecondition))

(defun Time-Dimension-Q-Code (temporal-relation range)
  "Depends on the type of RANGE."
  (declare (ignore temporal-relation))
  (if (term-type-p range '|OneOrTwoDTime|) 'onetwodimension
      (if (term-type-p range '|ThreeDTime|) 'threedimension)))


(defun Time-Inclusion-Q-Code (reln)
 (when (term-type-p reln '|temporalInclusive|) 'included))


(defun Time-In-Relation-ID-Code (speaking-time event-time tempo1)
  "Favor event-time over speaking-time until more is needed--or understood."
  (declare (ignore speaking-time tempo1))
  event-time)

(defun Time-In-Relation-To-Speaking-Time-ID-Code (speaking-time speech-act)
  "Favor event-time over speaking-time until more is needed."
  (declare (ignore speaking-time))
  (fetch-feature 'event-time 
		     (term-graph-parent (symbol-value (if (boundp speech-act) speech-act
							  (get-symbol-term speech-act))))))

(defun Time-Ordering-Q-Code (time-relation)
  "Check if TIMERELATION is ordered."
  (if  (term-type-p time-relation '|space#temporalOrdering|) 'ordered 'unordered))

(defun Time-Period-Mod-ID-Code (obj)
  "Return a temporal-locating relation, reifying if necessary."
  (fetch-relation-spec obj '|space#TemporalLocating| 'TIME-QUAL-ID))

(defun Time-Point-Q-Code (temporal-relation range)
  "Depends on the type of RANGE."
  (declare (ignore temporal-relation))
  (if (term-type-p range '|TimePoint|) 'point 'interval))

;(defun Time-Precedence-Q-Code (temporal-relation)
;  "Depends on type of relation."
;  (if (term-type-p temporal-relation 'anterior) 'precedence 'noprecedence))

(defun Time-Precedence-Q-Code (temporal-relation)
  "Depends on type of relation."
  (if (and (term-type-p temporal-relation '|space#temporalOrdering|)
	   (not (term-type-p temporal-relation '|concurrent|))) 'precedence 'noprecedence))

;(defun Time-Precede-Q-Code (temporal-relation)
;  "Depends on type of relation."
;  (if (term-type-p temporal-relation 'anterior) 'subsequent 'notsubsequent))

(defun Time-Precede-Q-Code (temporal-relation)
  "Depends on type of relation."
  (if (term-type-p temporal-relation '|posterior|) 'subsequent 'notsubsequent))

(defun Time-Relation-Q-Code (temporal-relation)
  "Depends on type of relation."
  (if (term-type-p temporal-relation '|space#TemporalLocating|) 'timerelation 'nottimerelation))

(defun Time-Q-Code (obj)
  "Is OBJ a time (time-placement)?"
  (if (term-type-p obj 'time-placement) 'time 'notime))

(defun Type-ID-Code (ps)
  "Give back the object of the membership relation."
  (fetch-subc-feature '|ElementOf| ps))

(defun Type-Q-Code (ps)
  "Is PS a kind of set?"
  (if (term-type-p ps '|OrderedSet|) 'type 'nontype))

(defun Typic-Q-Code (item)
  "Is there a membership relation?"
  (if (fetch-subc-feature '|ElementOf| item)
      'typic 'nontypic))

(defun Umty-Type-Q-Code (entity)
  "How many times ten is it?" 
  (case (floor (/ entity 10))
    (2 'twenty)
    (3 'thirty)
    (4 'forty)
    (5 'fifty)
    (6 `sixty)
    (7 'seventy)
    (8 'eighty)
    (otherwise 'ninety)))

(defun Units-Digit-ID-Code (number)
  "This is the number MOD power-of-ten."
  (mod number 10))

(defun Unit-Value-ID-Code (number)
  "This is the basis of the Western number system (English), changing
   the base list changes the units the number system works on."
  (cond ((< number 20)
	 number)
	((>= number 1000000)
	 (floor (/ number 1000000)))
	((>= number 1000)
	 (floor (/ number 1000)))
	((>= number 100)
	 (floor (/ number 100)))
	((>= number 10)
	 number)
	(t number)))

(defun Unrealized-Situation-Q-Code (item)
  "Depends upon presence of relevant time information."
  (if (or (fetch-feature 'event-time item)
	  (fetch-feature 'reference-time-id item))
      'notunrealized 
      'unrealized))

(defun Use-Modification-ID-Code (obj)
  "Return the USE-PROPERTY-ASCRIPTION feature."
   (fetch-relation '|UsePropertyAscription| obj))

(defun Use-Modification-Q-Code (obj)
  "Look for a USE-PROPERTY-ASCRIPTION feature."
  (if (fetch-relation '|UsePropertyAscription| obj) 'use))

(defun Verbal-Act-Category-ID-Code (process)
  "Return the SAYING feature."
  (fetch-relation-range '|saying| process))

(defun Verbal-Act-Category-Q-Code (process)
  "Does the process have a SAYING?"
  (if (and (fetch-relation '|saying| process)
	   (not (term-type-p (fetch-relation-range '|saying| process) '|Process|)))
      'actspecified))

(defun Verbal-Process-Q-Code (process)
  "Is the process verbal?"
  (if (term-type-p process '|External|) 'verbal 'notverbal))

(defun Vertical-Orientation-Q-Code (reln range)
  "Use the type of RELN."
  (declare (ignore range))
  (if (term-type-p reln '|above|) 'above
      (if (term-type-p reln '|below|) 'below)))

(defun Volitionality-Q-Code (modalproperty participant)
  "See if the feature introducing the MODALPROPERTY is VOLITIONAL." 
  (declare (ignore participant))
  (if (term-type-p modalproperty '|volitional|) 'volitional 'nonvolitional))

(defun Wh-Concept-Q-Code (wh)
  "Depends on type of WH."
  (cond ((term-type-p wh '|TemporalObject| wh) 'time)
	((term-type-p wh '|SpatialObject| wh) 'space)
	((term-role-p wh (term-graph-parent (symbol-value wh)) '|causalRelation|) 'cause)
	(t 'manner)))


;;; The following are the morphology inquiry implementations necessary
;;; at least for English.


(defun adjective-lexical-word-class-q-code (lex-entry)
  (lexical-class-ascertainer lex-entry '(er-est r-st *er-*est)))

(defun nerb-lexical-word-class-q-code (lex-entry)
  (lexical-class-ascertainer lex-entry '(noun verb)))


(defun noun-lexical-word-class-q-code (lex-entry)
  (lexical-class-ascertainer lex-entry '(s es)))



(defun verb-lexical-regularity-q-code (lex-entry)
  (case (lexical-class-ascertainer lex-entry '(regular irr *-irr s-irr es-irr))
    (irr 'irregular)
    ((*-irr s-irr es-irr) 'semi)
    (regular 'regular)))


(defun verb-class-q-code (lex-entry)
  (lexical-class-ascertainer lex-entry '(Es-Ed S-*ed S-Ed S-d *-Irr S-irr Es-Irr)))

(defun second-final-letter-q-code (lex-entry)
  (let* ((spelling (fetch-lexicon-info lex-entry 'spelling))
	 (chopped-length (1- (length spelling))))
    (if (member (subseq spelling (1- chopped-length) chopped-length) 
		'("a" "e" "o" "u")
		:test #'string-equal)
	'aeou
	'non-aeou)))

(defun final-letter-y-q-code (lex-entry)
  (let* ((spelling (fetch-lexicon-info lex-entry 'spelling))
	 (chopped-length (1- (length spelling))))
    (if (string-equal (subseq spelling chopped-length (length spelling))
		      "y")
	'y
	'non-y)))

(defun final-letter-e-q-code (lex-entry)
  (let* ((spelling (fetch-lexicon-info lex-entry 'spelling))
	 (chopped-length (1- (length spelling))))
    (if (string-equal (subseq spelling chopped-length (length spelling))
		      "e")
	'final-e
	'non-final-e)))

(defun exist-selected-stem-q-code (function)
  (if (and *activate-word-rank-and-below* 
	   (find-association function 'term))
      'stem
      'no-stem))

	  
	  
	  
;;; ----------------------------------------------------------------------
;;; Process premodification additions... added Bremen, June99

(defun PREMOD-SCOPE-INCLUDES-Q-CODE (process property)
  ;; event is the entire configuration that is being used as a premodifier, 
  ;; property is the particular property that the point of overlap is being
  ;; interrogated for... Need first to get the point of overlap therefore...
  ;; don't need to do any checks since these were done to get here and the
  ;; configuration can only be one of the mentioned process types. Ignore
  ;; complexing for present...
  (let ((poo (or (fetch-feature '|actee| process)
                 (fetch-feature '|phenomenon| process)
                 (fetch-feature '|saying| process))))
    (if 
        (fetch-feature 
         ;; should be a nicer way to do the following...
         (case property
           (age '|AgePropertyAscription|)
           (size '|SizePropertyAscription|)
           (provenance '|ProvenancePropertyAscription|)
           (colour '|ColorPropertyAscription|)
           (material '|MaterialPropertyAscription|))
         poo)
        'includes
      'not-includes)))

(defun Process-Pre-Modification-Q-Code (onus)
  "Look for a process feature or a non-minimal reified relation or an elaboration."
  (let* ((relations (fetch-feature 'relations onus))
         (term (if (and (symbolp relations)
                        (boundp relations)
                        (term-graph-p (symbol-value relations)))
                   (symbol-value relations)
                 nil))
         (process (if (and (term-graph-p term)
                           (equal (term-graph-type term) '|UMSet|)
                           (= (length (term-graph-symbol term)) 1))
                      (first (term-graph-symbol term))
                    nil)))
    (if (and process
             (not (pledged-p process))
             (or (and (term-type-p process '|AffectingAction|)
                      (fetch-feature '|actee| process)
                      (term-eq-p (fetch-feature '|actee| process) onus))
                 (and (term-type-p process '|SayingANDSensing|)
                      (fetch-feature '|phenomenon| process)
                      (term-eq-p (fetch-feature '|phenomenon| process) onus))
                 (and (term-type-p process '|SayingANDSensing|)
                      (fetch-feature '|saying| process)
                      (term-eq-p (fetch-feature '|saying| process) onus)))
             ;; also need to put a check in here about minimality, i.e., that the
             ;; candidate configuration fits into a premodifier, which means that
             ;; it should be minimal apart from the :actee/:phenomenon/:saying
             ;; info...
             (minimal-reified-term-p (symbol-value process))
             )
        'configuration
      nil)))
        
(defun Process-Pre-Modification-ID-Code (onus)
  "Look for a process feature or a non-minimal reified relation or an elaboration."
   (let* ((relations (fetch-feature 'relations onus))
         (term (if (and (symbolp relations)
                        (boundp relations)
                        (term-graph-p (symbol-value relations)))
                   (symbol-value relations)
                 nil)))
    (if (and (term-graph-p term)
             (equal (term-graph-type term) '|UMSet|)
             (= (length (term-graph-symbol term)) 1))
        (first (term-graph-symbol term))
      (fetch-feature 'relations onus))))	 


;;; new implementations for spatialmodality


(defun print-hash-entry (key value)
    (format t "The value associated with the key ~S is ~S~%" key value))


(defmacro SUBCLASS-INQ (name type pos neg)
`(defun ,name (process)
   (if (not (fetch-subc-feature '|attribute| process)) ,neg
   (if (term-type-p (retrieve-spatial-modality (symbol-value process)) ,type) ,pos ,neg))))


(defun retrieve-spatial-modality (minirange)
(setf temp (fetch-subc-feature '|attribute| (term-graph-symbol minirange)))
(if temp (fetch-subc-feature '|space#hasSpatialModality| temp) (fetch-subc-feature '|space#hasSpatialModality| (term-graph-symbol minirange)))
)
#|
(defun spatial-modality-q-code (onus)
(fetch-subc-feature '|attribute|  onus)
(setf att (fetch-subc-feature '|attribute| onus))
(if att (if (fetch-subc-feature '|space#hasSpatialModality| att) 'SPATIALMODALITY 'NOSPATIALMODALITY) 'NOSPATIALMODALITY)
)

(SUBCLASS-INQ CONTAINMENT-Q-CODE '|space#Containment| 'CONTAINMENT 'NOCONTAINMENT)



(defun functional-spatial-modality-q-code (onus)
  (setf modal (retrieve-spatial-modality (symbol-value onus)))
  (setf ret (if  (term-type-p (retrieve-spatial-modality (symbol-value onus)) '|space#FunctionalSpatialModality|) 'FUNCTIONALSPATIALMODALITY 'NOFUNCTIONALSPATIALMODALITY))
  
  ret)

 
(SUBCLASS-INQ DISJOINTNESS-Q-code '|space#Disjointness| 'DISJOINTNESS 'NODISJOINTNESS)

(SUBCLASS-INQ DISTRIBUTION-Q-code '|space#Distribution| 'DISTRIBUTION 'NODISTRIBUTION)

(SUBCLASS-INQ SHAPE-COMMITTING-Q-code '|space#ShapeCommitting| 'SHAPECOMMITTING 'NOSHAPECOMMITTING )

(SUBCLASS-INQ CONNECTION-Q-code '|space#Connection| 'CONNECTION 'NOCONNECTION)

(SUBCLASS-INQ VERTICAL-PROJECTION-Q-code '|space#VerticalProjection| 'VERTICAL 'NOVERTICAL)

(SUBCLASS-INQ FRONTAL-PROJECTION-Q-CODE '|space#FrontalProjection| 'FRONTAL 'NOFRONTAL)

(SUBCLASS-INQ FRONT-PROJECTION-Q-CODE '|space#FrontProjection| 'FRONT 'NOFRONT)

(SUBCLASS-INQ LEFT-PROJECTION-Q-CODE '|space#LeftProjection| 'LEFT 'NOLEFT)

(SUBCLASS-INQ ABOVE-PROJECTION-Q-CODE '|space#AboveProjection| 'ABOVE 'NOABOVE)

(SUBCLASS-INQ BELOW-PROJECTION-Q-CODE '|space#BelowProjection| 'BELOW 'NOBELOW)


(SUBCLASS-INQ OVER-PROJECTION-Q-CODE '|space#OverProjectionExternal| 'OVER 'NOOVER)

(SUBCLASS-INQ OVERLAP-Q-CODE '|space#Overlap| 'OVERLAP 'NOOVERLAP)

(SUBCLASS-INQ PARTSM-Q-CODE '|space#Parthood|  'PARTSM 'NOPARTSM)

(SUBCLASS-INQ SURROUNDING-Q-CODE '|space#Surrounding| 'SURROUNDING 'NOSURROUNDING)

(SUBCLASS-INQ NONPROJECTION-AXIAL-Q-CODE '|space#NonProjectionAxial| 'NONPROJECTIONAXIAL 'NONONPROJECTIONAXIAL)

(SUBCLASS-INQ DIRECTIONAL-RELATION-Q-CODE '|space#DirectionalRelation| 'DIRECTIONALRELATION 'NODIRECTIONALRELATION)

(SUBCLASS-INQ SEQUENTIAL-Q-CODE '|space#Sequential| 'SEQUENTIAL 'NOSEQUENTIAL)

(SUBCLASS-INQ SPECIFIC-DIRECTIONAL-Q-CODE '|space#SpecificDirectional| 'SPECIFIC 'NOSPECIFIC)

(SUBCLASS-INQ CARDINAL-DIRECTIONAL-Q-CODE '|space#CardinalDirectional| 'CARDINAL 'NOCARDINAL)

(SUBCLASS-INQ EAST-Q-CODE '|space#East| 'EAST 'NOEAST)
(SUBCLASS-INQ WEST-Q-CODE '|space#West| 'WEST 'NOWEST)
(SUBCLASS-INQ NORTH-Q-CODE '|space#North| 'NORTH 'NONORTH)
(SUBCLASS-INQ SOUTH-Q-CODE  '|space#South| 'SOUTH 'NOSOUTH)

;;(SUBCLASS-INQ RELATIVE-NONPROJECTION-AXIAL-Q-CODE '|space#RelativeNonProjectionAxial| 'RELATIVE 'NORELATIVE)
(SUBCLASS-INQ RELATIVE-NONPROJECTION-AXIAL-Q-CODE '|space#RelativeNonProjectionAxial| 'YES 'NO)

(SUBCLASS-INQ CONTROL-Q-CODE '|space#Control| 'CONTROL 'NOCONTROL )

(SUBCLASS-INQ PROXIMAL-Q-CODE '|space#Proximal| 'PROXIMAL 'NOPROXIMAL)

(SUBCLASS-INQ DISTAL-Q-CODE '|space#Distal| 'DISTAL 'NODISTAL)



(defun functional-controlled-external-q-code (onus)
   (retrieve-spatial-modality (symbol-value onus))
   (setf ret (if (term-type-p (retrieve-spatial-modality (symbol-value onus)) '|space#FunctionalControlledExternal|) 'FUNCTIONALCONTROLLEDEXTERNAL 'NOFUNCTIONALCONTROLLEDEXTERNAL))
ret
)

(SUBCLASS-INQ FUNCTIONAL-CONTROLLED-INTERNAL-Q-CODE '|space#FunctionalControlledInternal| 'FUNCTIONALCONTROLLEDINTERNAL 'NOFUNCTIONALCONTROLLEDINTERNAL)

(SUBCLASS-INQ DENIAL-OF-FUNCTIONAL-CONTROL-Q-CODE '|space#DenialOfFunctionalControl| 'DENIAL 'NODENIAL)

(SUBCLASS-INQ multiple-directional-q-code '|space#MultipleDirectional| 'MULTIPLE 'NOMULTIPLE)

(SUBCLASS-INQ path-representing-internal-q-code '|space#PathRepresentingInternal| 'PATHREPRESENTINGINTERNAL 'NOPATHREPRESENTINGINTERNAL)

(SUBCLASS-INQ path-representing-external-q-code '|space#PathRepresentingExternal| 'PATHREPRESENTINGEXTERNAL 'NOPATHREPRESENTINGEXTERNAL)

(SUBCLASS-INQ part-sm-q-code '|space#Parthood| 'PARTSM 'NOPARTSM)

(SUBCLASS-INQ spatial-distance-modality-q-code '|space#SpatialDistanceModality| 'SPATIALDISTANCEMODALITY 'NOSPATIALDISTANCEMODALITY)

(SUBCLASS-INQ PROJECTION-RELATION-Q-CODE '|space#ProjectionRelation| 'yes 'no)
|#
#|
(defun qualitative-distance-q-code (onus) 
  (setf modal (retrieve-spatial-modality (symbol-value onus)))
  (setf ret (if (term-type-p (retrieve-spatial-modality (symbol-value onus)) '|space#QualitativeDistance|) 'QUALITATIVEDISTANCE 'NOQUALITATIVEDISTANCE))
  ret)
|#

#|
(SUBCLASS-INQ OVER-PROJECTION-Q-CODE '|space#OverProjectionExternal| 'OVER 'NOOVER)

(SUBCLASS-INQ UNDER-PROJECTION-Q-CODE '|space#UnderProjectionExternal| 'UNDER 'NOUNDER)

;(SUBCLASS-INQ KPML::QUALITATIVE-DISTANCE-Q-CODE '|space#QualitativeDistance| 'QUALITATIVEDISTANCE 'NOQUALITATIVEDISTANCE)

(SUBCLASS-INQ KPML::QUANTITATIVE-DISTANCE-Q-CODE '|space#QuantitativeDistance| 'QUANTITATIVEDISTANCE 'NOQUANTITATIVEDISTANCE)
|#

(defun SPATIAL-EXTENT-SM-Q-CODE (onus)
(if (fetch-subc-feature '|space#spatialExtent| onus) 'spatialextent 'nospatialextent)
(if (not (fetch-subc-feature '|attribute| onus)) 'NOSPATIALEXTENT
(progn
(setf temp (retrieve-spatial-modality (symbol-value onus)))
(if (not temp) 'NOSPATIALEXTENT 
(progn (setf rel (fetch-relation '|space#spatialExtent| temp))
(setf ret (if (eq nil rel) 'NOSPATIALEXTENT 'SPATIALEXTENT))
ret
))))
)

(defun SPATIAL-EXTENT-SM-ID-CODE (onus)
(setf temp (retrieve-spatial-modality (symbol-value onus)))
(setf temp (fetch-subc-feature '|space#spatialExtent| temp))
temp
)

(defun spatial-attribute-id-code (process)
(fetch-subc-feature '|attribute| process))

(defun extreme-position-q-code (onus)
(if (not (fetch-subc-feature '|attribute| onus)) 'NOEXTREME
(progn
(setf sm (retrieve-spatial-modality (symbol-value onus)))
(if (not sm) 'NOEXTREME
(progn (setf temp (or (fetch-subc-feature '|space#extremePositionOnAxis| (retrieve-spatial-modality (symbol-value onus)))
 (fetch-subc-feature '|space#extremeDistancePosition| (retrieve-spatial-modality (symbol-value onus)))))

(if (eq nil temp) 'NOEXTREME 'EXTREME)  
)))))

(defun extreme-position-on-axis-Q-code (onus)
(setf temp (fetch-subc-feature '|space#extremePositionOnAxis| (retrieve-spatial-modality (symbol-value onus))))
(if (eq nil temp)  'NOONAXIS 'ONAXIS))

(defun extreme-position-id-code (onus)
(setf epa (fetch-subc-feature '|space#extremePositionOnAxis| (retrieve-spatial-modality (symbol-value onus))))
(setf edp (fetch-subc-feature '|space#extremeDistancePosition| (retrieve-spatial-modality (symbol-value onus))))

(if (eq nil epa ) edp epa))


(defmacro SUBCLASS-INQ2 (name type pos neg)
`(defun ,name (process) 
   (if (eq nil (term-graph-symbol (symbol-value process))) ,neg
   (if (term-type-p (fetch-subc-feature '|space#hasSpatialModality| (term-graph-symbol (symbol-value process))) ,type) ,pos ,neg))))

  
(SUBCLASS-INQ2 proximal-phrase-Q-code '|space#Proximal| 'yes 'no)
(SUBCLASS-INQ2 distal-phrase-Q-code '|space#Distal| 'yes 'no)
(SUBCLASS-INQ2 projection-relation-phrase-Q-code '|space#ProjectionRelation| 'yes 'no)
(SUBCLASS-INQ2 fci-phrase-Q-code '|space#FunctionalControlledInternal| 'yes 'no)
(SUBCLASS-INQ2 fce-phrase-Q-code '|space#FunctionalControlledExternal| 'yes 'no)
(SUBCLASS-INQ2 path-representing-phrase-Q-code '|space#PathRepresenting| 'yes 'no)
(SUBCLASS-INQ2 under-projection-phrase-Q-code '|space#UnderProjectionExternal| 'yes 'no)
(SUBCLASS-INQ2 over-projection-phrase-Q-code '|space#OverProjectionExternal| 'yes 'no)
(SUBCLASS-INQ2 directional-relation-phrase-Q-code '|space#DirectionalRelation| 'yes 'no)
(SUBCLASS-INQ2 part-sm-phrase-Q-code '|space#Part| 'yes 'no)
(SUBCLASS-INQ2 surrounding-phrase-Q-code '|space#Surrounding| 'yes 'no)
(SUBCLASS-INQ2 distribution-phrase-Q-code '|space#Distribution| 'yes 'no)
(SUBCLASS-INQ2 dfc-phrase-Q-code '|space#DenialOfFunctionalControl| 'yes 'no)
(SUBCLASS-INQ2 nonprojection-axial-phrase-Q-code '|space#NonProjectionAxial| 'yes 'no)
(SUBCLASS-INQ2 sequential-phrase-Q-code '|space#Sequential| 'yes 'no)

(SUBCLASS-INQ2 VERTICAL-PROJECTION-Phrase-Q-code '|space#VerticalProjection| 'yes 'no)
(SUBCLASS-INQ2 FRONTAL-PROJECTION-Phrase-Q-CODE '|space#FrontalProjection| 'yes 'no)
(SUBCLASS-INQ2 LATERAL-PROJECTION-Phrase-Q-CODE '|space#FrontalProjection| 'yes 'no)
(SUBCLASS-INQ2 FRONT-PROJECTION-Phrase-Q-CODE '|space#FrontProjection| 'yes 'NO)
(SUBCLASS-INQ2 LEFT-PROJECTION-Phrase-Q-CODE '|space#LeftProjection| 'yes 'NO)
(SUBCLASS-INQ2 RIGHT-PROJECTION-Phrase-Q-CODE '|space#RightProjection| 'yes 'NO)
(SUBCLASS-INQ2 ABOVE-PROJECTION-Phrase-Q-CODE '|space#AboveProjection| 'yes 'NO)
(SUBCLASS-INQ2 other-vertical-PROJECTION-phrase-Q-CODE '|space#VerticalProjection| 'yes 'no)
(SUBCLASS-INQ2 SPECIFIC-DIRECTIONAL-Phrase-Q-CODE '|space#SpecificDirectional| 'yes 'NO)
(SUBCLASS-INQ2 CARDINAL-DIRECTIONAL-Phrase-Q-CODE '|space#CardinalDirectional| 'yes 'NO)
(SUBCLASS-INQ2 EAST-Phrase-Q-CODE '|space#East| 'yes 'NO)
(SUBCLASS-INQ2 WEST-Phrase-Q-CODE '|space#West| 'yes 'NO)
(SUBCLASS-INQ2 NORTH-Phrase-Q-CODE '|space#North| 'yes 'NO)
(SUBCLASS-INQ2 SOUTH-Phrase-Q-CODE  '|space#South| 'yes 'NO)
(SUBCLASS-INQ2 RELATIVE-NONPROJECTION-AXIAL-phrase-Q-CODE '|space#RelativeNonProjectionAxial| 'yes 'NO)
(SUBCLASS-INQ2 multiple-directional-phrase-q-code '|space#MultipleDirectional| 'yes 'NO)

(SUBCLASS-INQ2 path-past-q-code '|space#PathPast| 'past 'nonpast)
(SUBCLASS-INQ2 path-representing-internal-q-code '|space#PathRepresentingInternal| 'internal 'noninternal)
(SUBCLASS-INQ2 distancing-directional-q-code '|space#GeneralDirectionalDistancing| 'distancing 'nondistancing)
(SUBCLASS-INQ2 approaching-directional-q-code '|space#GeneralDirectionalApproaching| 'approaching 'nonapproaching)
(SUBCLASS-INQ2 nearing-directional-q-code '|space#GeneralDirectionalNearing| 'nearing 'nonnearing)
(SUBCLASS-INQ2 path-containment-q-code '|space#PathContainment| 'containment 'noncontainment)
(SUBCLASS-INQ2 right-external-q-code '|space#RightProjectionExternal| 'external 'nonexternal)
(SUBCLASS-INQ2 left-external-q-code '|space#LeftProjectionExternal| 'external 'nonexternal)
(SUBCLASS-INQ2 general-directional-phrase-q-code '|space#CardinalDirectional| 'nogeneral 'general)

 
;;macro for qs:

(defmacro spatial-attribute-relation (name type pos neg)
`(defun ,name (process)
   (if (fetch-subc-feature ,type process) ,pos ,neg)))

(defmacro fetch-spatial-attribute (name type)
`(defun ,name (process)
   (fetch-subc-feature ,type process)))

;; Redefined several inquiries to make SPLs with and without a GeneralizedRoute component equally interpretable; Nina D. 04.07.09
   
;; Chooser inquiries...

(spatial-attribute-relation orientation-end-attribute-q-code '|space#orientationEnd| 'orientationend 'noorientationend)
(spatial-attribute-relation route-attribute-q-code '|space#route| 'route 'noroute)
(spatial-attribute-relation orientation-start-attribute-q-code '|space#orientationStart| 'orientationstart 'noorientationstart)
(spatial-attribute-relation placement-attribute-q-code '|space#placement| 'placement 'noplacement)

(defun direction-attribute-q-code (process)
(if (or (not (equal nil (fetch-feature '|space#direction| (fetch-feature '|space#route| process)))) 
	(fetch-subc-feature '|space#direction| process)) 'direction 'nodirection)
		   )
		   
(defun path-placement-attribute-q-code (process)
(if (or (not (equal nil (fetch-feature '|space#pathPlacement| (fetch-feature '|space#route| process)))) 
	(fetch-subc-feature '|space#pathPlacement| process)) 'pathplacement 'nopathplacement)
		   )

(defun next-path-placement-attribute-q-code (process)
(if (or (not (equal nil (fetch-feature '|space#nextPathPlacement| (fetch-feature '|space#route| process)))) 
	(fetch-subc-feature '|space#nextPathPlacement| process)) 'nextpathplacement 'nonextpathplacement)
		   )

(defun path-indication-attribute-q-code (process)
(if (or (not (equal nil (fetch-feature '|space#pathIndication| (fetch-feature '|space#route| process)))) 
	(fetch-subc-feature '|space#pathIndication| process)) 'pathindication 'nopathindication)
		   )		   
		   
(defun destination-attribute-q-code (process)
(if (or (not (equal nil (fetch-feature '|space#destination| (fetch-feature '|space#route| process)))) 
	(fetch-subc-feature '|space#destination| process)) 'destination 'nodestination)
		   )		   

(defun next-path-indication-attribute-q-code (process)
(if (or (not (equal nil (fetch-feature '|space#nextPathIndication| (fetch-feature '|space#route| process)))) 
	(fetch-subc-feature '|space#nextPathIndication| process)) 'nextpathindication 'nonextpathindication)
		   )

(defun source-attribute-q-code (process)
(if (or (not (equal nil (fetch-feature '|space#source| (fetch-feature '|space#route| process)))) 
	(fetch-subc-feature '|space#source| process)) 'source 'nosource)
		   )


;; Identifying inquiries...

(fetch-spatial-attribute orientation-end-attribute-id-code '|space#orientationEnd|)
(fetch-spatial-attribute orientation-start-attribute-id-code '|space#orientationStart|)
(fetch-spatial-attribute placement-attribute-id-code '|space#placement|)

(defun source-attribute-id-code (p)
(setf tt (fetch-subc-feature '|space#source| p))
(if (equal tt nil ) (source-route p) tt))

(defun source-route (p)
(setf ttt (fetch-subc-feature '|space#source| (fetch-subc-feature '|space#route| p)))
ttt)

(defun next-path-indication-attribute-id-code (p)
(setf tt (fetch-subc-feature '|space#nextPathIndication| p))
(if (equal tt nil ) (next-path-indication-route p) tt))

(defun next-path-indication-route (p)
(setf ttt (fetch-subc-feature '|space#nextPathIndication| (fetch-subc-feature '|space#route| p)))
ttt)

(defun destination-attribute-id-code (p)
(setf tt (fetch-subc-feature '|space#destination| p))
(if (equal tt nil ) (destination-route p) tt))

(defun destination-route (p)
(setf ttt (fetch-subc-feature '|space#destination| (fetch-subc-feature '|space#route| p)))
ttt)

(defun path-indication-attribute-id-code (p)
(setf tt (fetch-subc-feature '|space#pathIndication| p))
(if (equal tt nil ) (path-indication-route p) tt))

(defun path-indication-route (p)
(setf ttt (fetch-subc-feature '|space#pathIndication| (fetch-subc-feature '|space#route| p)))
ttt)

(defun next-path-placement-attribute-id-code (p)
(setf tt (fetch-subc-feature '|space#nextPathPlacement| p))
(if (equal tt nil ) (next-path-placement-route p) tt))

(defun next-path-placement-route (p)
(setf ttt (fetch-subc-feature '|space#nextPathPlacement| (fetch-subc-feature '|space#route| p)))
ttt)

(defun direction-attribute-id-code (p)
(setf tt (fetch-subc-feature '|space#direction| p))
(if (equal tt nil ) (direction-route p) tt))

(defun direction-route (p)
(setf ttt (fetch-subc-feature '|space#direction| (fetch-subc-feature '|space#route| p)))
ttt)

(defun path-placement-attribute-id-code (p)
(setf tt (fetch-subc-feature '|space#pathPlacement| p))
(if (equal tt nil ) (path-placement-route p) tt))

(defun path-placement-route (p)
(setf ttt (fetch-subc-feature '|space#pathPlacement| (fetch-subc-feature '|space#route| p)))
ttt)

(defun route-attribute-id-code (p)
(setf tt (fetch-subc-feature '|attribute| p))
(setf ttt (fetch-subc-feature '|attribute| (fetch-subc-feature '|attribute| p)))
ttt)

;; Some further inquiries....

(defun spatial-np-q-code (onus)
(if (fetch-subc-feature '|attribute| onus) 'spatialnp 'nospatialnp))

(defun quantitative-distance-ID-Code (onus)
  "Return the quantitative-distance feature."
  (fetch-feature '|space#quantitativeDistanceExtent| 'quantitative-distance-id))
  ;(fetch-relation-spec onus '|space#quantitativeDistanceExtent| 'Quantitative-Distance-ID))
  
(defun qualitative-distance-ID-Code (onus)
  "Return the qualitative-distance feature."
  (fetch-relation-spec onus '|space#QualitativeDistance| 'Qualitative-Distance-ID)) 
 
(defun mention-distance-q-code (onus)
(if (or (term-type-p onus '|space#QualitativeDistance| ) 
(term-type-p onus '|space#QuantitativeDistance| )) 'mentiondistance 'notmentiondistance))

(defun distance-type-q-code (onus)
(if (fetch-subc-feature '|space#qualitativeDistanceExtent| onus) 'qualitative 'quantitative))
;(if (fetch-subc-feature '|space#quantitativeDistanceExtent| onus) 'quantitative)) 

;;;expressvocative added by katerina 26/5/2000
(defun Express-Hearer-Q-Code (hearer onus)
  "Depends on the speech-act type of ONUS."
  (declare (ignore hearer))
  (if (term-type-p (fetch-feature 'speech-act onus) 'question)
      'expresshearer
     (or 'withholdhearer
	'expressvocative)))
	
(defun modality-relatum-q-code (onus)
(if (fetch-subc-feature '|space#hasSpatialModality| onus)	
	'modality 'nomodality))
	
	