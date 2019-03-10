(in-package :penman)

(defun PROCESSIDEA (CONFIGURATION)
	"Which process does the figure have?"
	(fetch-feature '|processInConfiguration| CONFIGURATION))

(defun ISTHERELOCATUM (CONFIGURATION)
  "Is there a locatum in the configuration?"
  (cond ((LOCATUMIDEA CONFIGURATION) 'yes)
	(t 'no)))
	
(defun LOCATUMIDEA (PROCESS)
	"Which locatum does the figure have?"
	(fetch-feature '|space#locatum| PROCESS))

(defun PLACEMENTIDEA (onus)
  "Which placement does the figure have?"
  (fetch-subc-feature '|space#placement| onus))

(defun ROUTEIDEA (onus)
  "Which placement does the figure have?"
  (fetch-subc-feature '|space#route| onus))

(defun ISTHEREACTOR (CONFIGURATION)
  "Is there an actor in the configuration?"
  (cond ((ACTORIDEA CONFIGURATION) 'yes)
	(t 'no)))
  
(defun ACTORIDEA (onus)
  "Which actor does the figure have?"
  (fetch-subc-feature '|actor| onus))

(defun ISTHEREACTEE (CONFIGURATION)
  "Is there an actee in the configuration?"
  (cond ((ACTEEIDEA CONFIGURATION) 'yes)
	(t 'no)))
  
(defun ACTEEIDEA (onus)
  "Which actee does the figure have?"
  (fetch-subc-feature '|actee| onus))

(defun ISTHERESENSER (CONFIGURATION)
  "Is there an senser in the configuration?"
  (cond ((SENSERIDEA CONFIGURATION) 'yes)
	(t 'no)))
  
(defun SENSERIDEA (onus)
  "Which senser does the figure have?"
  (fetch-subc-feature '|senser| onus))

(defun ISTHEREPHENOMENON (CONFIGURATION)
  "Is there an phenomenon in the configuration?"
  (cond ((PHENOMENONIDEA CONFIGURATION) 'yes)
	(t 'no)))

(defun PHENOMENONIDEA (onus)
  "Which phenomenon does the figure have?"
  (fetch-subc-feature '|phenomenon| onus))

(defun ISTHEREEXISTENT (CONFIGURATION)
  "Is there an existent in the configuration?"
  (cond ((EXISTENTIDEA CONFIGURATION) 'yes)
	(t 'no)))

(defun EXISTENTIDEA (onus)
  "Which existent does the figure have?"
  (fetch-subc-feature '|existent| onus))

(defun ATTRIBUENDIDEA (onus)
  "Which attribuend does the figure have?"
  (fetch-subc-feature '|attribuend| onus))

(defun ATTRIBUTEIDEA (onus)
  "Which attribute does the figure have?"
  (fetch-subc-feature '|attribute| onus))

(defun NEXTIDEA (onus)
  "Which next does the phenomenon have?"
  (fetch-subc-feature '|next| onus))

(defun CIRCUMSTANCEIDEA (ELEMENT)
	""
	(fetch-subc-feature '|circumstanceInConfiguration| ELEMENT))

(defun SPATIALMODALITYIDEA (location)
	""
	(fetch-feature '|space#hasSpatialModality| location))

(defun ISTHEREMODIFICATION (onus)
  "Is there a modification in the location?"
  (cond ((MODIFICATIONIDEA onus) 'yes)
	(t 'no)))

(defun MODIFICATIONIDEA (onus)
	""
	(fetch-feature '|hasModification| onus))

(defun QUANTITYIDEA (onus)
	""
	(fetch-feature 'quantity onus))

(defun ISTHERERELATUM (location)
  "Is there a relatum in the location?"
  (cond ((RELATUMIDEA location) 'yes)
	(t 'no)))

(defun RELATUMIDEA (onus)
	"Which relatum does the location have?"
	(fetch-feature '|space#relatum| onus))

(defun ISTHEREMODALITY (location)
  "Is there a spatial modality in the location?"
  (cond ((MODALITYIDEA location) 'yes)
	(t 'no)))

(defun MODALITYIDEA (onus)
	"Which spatial modality does the location have?"
	(fetch-feature '|space#hasSpatialModality| onus))

(defun ISTHERESOURCE (route)
  "Is there a source in the route?"
  (cond ((SOURCEIDEA route) 'yes)
	(t 'no)))
	
(defun SOURCEIDEA (onus)
  "Which source does the route have?"
  (fetch-subc-feature '|space#source| onus))

(defun ISTHEREDESTINATION (route)
  "Is there a destination in the route?"
  (cond ((DESTINATIONIDEA route) 'yes)
	(t 'no)))
	
(defun DESTINATIONIDEA (onus)
  "Which destination does the route have?"
  (fetch-subc-feature '|space#destination| onus))

(defun ISTHEREPATHPLACEMENT (route)
  "Is there a path placement in the route?"
  (cond ((PATHPLACEMENTIDEA route) 'yes)
	(t 'no)))
  
(defun PATHPLACEMENTIDEA (route)
  "Which path placement does the route have?"
  (fetch-subc-feature '|space#pathPlacement| route))

(defun ISTHEREORIENTATIONDIRECTION (route)
  "Is there an orientation direction in the route?"
  (cond ((ORIENTATIONDIRECTIONIDEA route) 'yes)
	(t 'no)))
  
(defun ORIENTATIONDIRECTIONIDEA (onus)
  "Which orientation direction does the figure have?"
  (fetch-subc-feature '|space#orientationDirection| onus))
 
; (defun ISTHEREROUTE (location)
  ; "Is there a route in the location?"
  ; (cond ((ROUTEIDEA location) 'yes)
	; (t 'no)))

(defun ISTHERECIRCUMSTANCE (element)
	""
	; (if (fetch-feature '|space#hasSpatialModality| element) 'yes 'no)
	; ; 'no
	(cond ((CIRCUMSTANCEIDEA element) 'yes)
	(t 'no))
	)

(defun LOCATIONSTATUS (element)
	""
	(if (term-type-p element '|space#GeneralizedLocation|) 'location 'not-location))

(defun CONFIGURATIONTYPE (element)
	"??"
	(cond
		((term-type-p element 
		'|Existence|) 'existence)
		((term-type-p element 
		'|GeneralizedLocating|) 'spatial-locating)
		((term-type-p element 
		'|BeingANDHaving|) 'relating)
		((term-type-p element 
		'|space#AffectingDirectedMotion|) 'affecting-directed-motion)
		((term-type-p element 
		'|space#NonAffectingDirectedMotion|) 'non-affecting-directed-motion)
		((term-type-p element 
		'|space#NonAffectingOrientationChange|) 'non-affecting-orientation-change)
		((term-type-p element 
		'|space#AffectingMotion|) 'affecting-motion)
		((term-type-p element 
		'|Perception|) 'perception)
		((term-type-p element
		'|Liking|) 'liking)
		(t nil)))

(defun SPATIALMODALITYTYPE (element)
	"??"
	(cond
		((term-type-p element 
		'|space#AboveProjectionExternal|) 'above-projection-external)
		((term-type-p element 
		'|space#AboveProjectionInternal|) 'above-projection-internal)
		((term-type-p element 
		'|space#BackProjectionExternal|) 'back-projection-external)
		((term-type-p element 
		'|space#BackProjectionInternal|) 'back-projection-internal)
		((term-type-p element 
		'|space#Central|) 'central)
		((term-type-p element 
		'|space#Connection|) 'connection)
		((term-type-p element 
		'|space#Containment|) 'containment)
		((term-type-p element 
		'|space#DenialOfFunctionalControl|) 'denial-of-functional-control)
		((term-type-p element 
		'|space#Distal|) 'distal)
		((term-type-p element 
		'|space#Distribution|) 'distribution)
		((term-type-p element 
		'|space#FrontProjectionExternal|) 'front-projection-external)
		((term-type-p element 
		'|space#GeneralDirectionalDistancing|) 'general-directional-distancing)
		((term-type-p element 
		'|space#GeneralDirectionalNearing|) 'general-directional-nearing)
		((term-type-p element 
		'|space#North|) 'north)
		((term-type-p element 
		'|space#MultipleDirectional|) 'multiple-directional)
		((term-type-p element 
		'|space#PathRepresenting|) 'path-representing)
		((term-type-p element 
		'|space#Proximal|) 'proximal)
		((term-type-p element 
		'|space#QuantitativeDistance|) 'quantitative-distance)
		((term-type-p element 
		'|space#RelativeNonProjectionAxial|) 'relative-non-projection-axial)
		((term-type-p element 
		'|space#RightProjection|) 'right-projection)
		((term-type-p element 
		'|space#Surrounding|) 'surrounding)
		(t 'no-spatial-modality)))

(defun ISDESTINATIONRECURSIVE (MINORPROCESS)
  "Is destination recursive?"
  (cond ((DESTINATIONRECURSIVITY MINORPROCESS) 'yes)
	(t 'no)))

; kill all these -phrase's below?
	
(defun DESTINATIONRECURSIVITY (MINORPROCESS)
	"Is destination recursive?"
	(fetch-feature 'recursivity MINORPROCESS))
	
(defun APEPHRASE (ELEMENT)
	"Is the prepositional phrase of this type?"
	(if (term-type-p element '|space#AboveProjectionExternal|) 'yes 'no))

(defun APIPHRASE (ELEMENT)
	"Is the prepositional phrase of this type?"
	(if (term-type-p element '|space#AboveProjectionInternal|) 'yes 'no))

(defun BPEPHRASE (ELEMENT)
	"Is the prepositional phrase of this type?"
	(if (term-type-p element '|space#BackProjectionExternal|) 'yes 'no))

(defun BPIPHRASE (ELEMENT)
	"Is the prepositional phrase of this type?"
	(if (term-type-p element '|space#BackProjectionInternal|) 'yes 'no))

(defun CENTRALPHRASE (ELEMENT)
	"Is the prepositional phrase of this type?"
	(if (term-type-p element '|space#Central|) 'yes 'no))

(defun CONNECTIONPHRASE (ELEMENT)
	"Is the prepositional phrase of this type?"
	(if (term-type-p element '|space#Connection|) 'yes 'no))

(defun CONTAINMENTPHRASE (ELEMENT)
	"Is the prepositional phrase of this type?"
	(if (term-type-p element '|space#Containment|) 'yes 'no))

(defun DOFCPHRASE (ELEMENT)
	"Is the prepositional phrase of this type?"
	(if (term-type-p element '|space#DenialOfFunctionalControl|) 'yes 'no))

(defun DISTALPHRASE (ELEMENT)
	"Is the prepositional phrase of this type?"
	(if (term-type-p element '|space#Distal|) 'yes 'no))

(defun DISTRIBUTIONPHRASE (ELEMENT)
	"Is the prepositional phrase of this type?"
	(if (term-type-p element '|space#Distribution|) 'yes 'no))

(defun FPEPHRASE (ELEMENT)
	"Is the prepositional phrase of this type?"
	(if (term-type-p element '|space#FrontProjectionExternal|) 'yes 'no))

(defun NORTHINTERNALPHRASE (ELEMENT)
	"Is the prepositional phrase of this type?"
	(if (term-type-p element '|space#NorthInternal|) 'yes 'no))

(defun PARTHOODPHRASE (ELEMENT)
	"Is the prepositional phrase of this type?"
	(if (term-type-p element '|space#Parthood|) 'yes 'no))

(defun PROXIMALPHRASE (ELEMENT)
	"Is the prepositional phrase of this type?"
	; (print "HelloWorld")
	(if (term-type-p Element '|space#Proximal|) 'yes 'no))

(defun QUANTITATIVEDISTANCEPHRASE (ELEMENT)
	"Is the prepositional phrase of this type?"
	(if (term-type-p element '|space#QuantitativeDistance|) 'yes 'no))

(defun RNPAPHRASE (ELEMENT)
	"Is the prepositional phrase of this type?"
	(if (term-type-p element '|space#RelativeNonProjectionAxial|) 'yes 'no))

(defun SUPPORTPHRASE (ELEMENT)
	"Is the prepositional phrase of this type?"
	(if (term-type-p element '|space#Support|) 'yes 'no))

(defun SURROUNDINGPHRASE (ELEMENT)
	"Is the prepositional phrase of this type?"
	(if (term-type-p element '|space#Surrounding|) 'yes 'no))

(defun UPEPHRASE (ELEMENT)
	"Is the prepositional phrase of this type?"
	(if (term-type-p element '|space#UnderProjectionExternal|) 'yes 'no))

(defun NAMEGENDER (CLASS)
  ""
  (let ((features (fetch-lexicon-info CLASS 'features)))
    (if features
      (if (member 'feminine features)
		'feminine
		'masculine)
	  'masculine)))

(defun MINORPROCESS-PREPOSITION (MINORPROCESS)
  ""
  (let ((features (fetch-lexicon-info MINORPROCESS 'features)))
    (if features
      (cond
		((member 'preposition-by features) 'preposition-by)
		((member 'preposition-of features) 'preposition-of)
		((member 'preposition-with features) 'preposition-with)
		((member 'preposition-in features) 'preposition-in)
		((member 'preposition-for features) 'preposition-for)
		((member 'preposition-until features) 'preposition-until)
		(t 'not-prepositioned)))))

(defun NAMEIDENTIFIABILITY (CLASS)
	""
	(if (equal (fetch-feature 'identifiability CLASS) 'not-identifiable)
	'not-identifiable
	'identifiable))

(defun CLASSNAME (concept association)
	"all inquiries that have idea in the name must fetch features this way"
	(LEXICAL-TERM-RESOLUTION concept association))

(defun CLASSNAME2 (element element-slot)
	""
	(lexical-term-resolution element element-slot))

(defun REFERENCETYPE (element)
	""
	(if (term-type-p element '|NamedObject|)
		'thing
		'class))
; This is a temporary fix. Basically, KPML generates 'things'
; if the function is of type named-object or any of its sub-types
; and 'classes' if the function has any other type. However,
; the system Reference-Type deals with the difference
; between "I like coffee" vs. "I like this coffee". This
; should not be an ontological difference, but a grammatical
; one, whose answer is provided by the SPL expression. How 
; to implement an inquiry to look for an answer inside the 
; SPL and not in GUM?

(defun COMPLEXITY (ELEMENT)
	""
	(if (fetch-feature 'next ELEMENT)
	'complex
	'simplex))
; currently, 'next' is not being used in SPLs, as it would
; also cause an 'invalid keyword' warning. Thus, we implemented
; complex nominal groups (e.g. with 'and') as a single lexical
; item and so all nominal groups in the grammar are 'simplex'es.

(defun CONFIGURATIONSTATUS (element)
	"all with status in name this way 1"
	(if (term-type-p element '|Configuration|)
		'configuration
		'not-configuration
		)
	)

(defun CIRCUMSTANCESTATUS (element)
	"all with status in name this way"
	(if (term-type-p element '|Circumstance|)
		'circumstance
		'not-circumstance)
	)

(defun term-resolve-id-code (concept association)
  (LEXICAL-TERM-RESOLUTION concept association))

(defun QUANTITATIVEDE (ELEMENT)
	""
	(if (term-type-p element '|space#QuantitativeSpatialTemporal|) 'yes 'no))

(defun ACCESSIBILITY (ELEMENT) ; ok, this is workig!
	"Is modification of type 'accessibility'?"
	(cond ((ACCESSIBILITYIDEA element) 'yes)
	(t 'no)))

(defun ACCESSIBILITYIDEA (onus) ; ok, this is workig!
  "If modification is accessibility, grab semantics."
  (fetch-subc-feature 'accessibility onus))

(defun ACCESSIBILITYLEVEL (MODIFICATION)
	"Once modification is accessibility, which accessibility level is it?"
	(print "the semantics for accessibility-level inquiry is:")
	(print modification)
	(print "the returned value is:")
	(print (term-type-p MODIFICATION 'accessibility))
	(term-type-p MODIFICATION 'accessibility))
	
(defun QUALITATIVEDE (ELEMENT)
	""
	(if (term-type-p element '|space#QualitativeSpatialTemporal|) 'yes 'no))

(defun RECIPROCAL (ELEMENT)
	""
	(if (term-type-p element '|space#Reciprocal|) 'yes 'no))

(defun SPATIALPERSPECTIVE (ELEMENT)
	""
	(if (term-type-p element '|space#SpatialPerspective|) 'yes 'no))

(defun CASE-PRESELECTION (ELEMENT)
	""
	'not-prepositioned)