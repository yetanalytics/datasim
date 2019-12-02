(ns com.yetanalytics.datasim.xapi.statement.generation)

;; TODO: pull "seeds" from xapi-schema generators when appropraite
;; - ie, don't need to sample from EVERY possible language tag, just the ones xapi-schema is happy with

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Summary
;;
;; -> search statement for existing info relevant to `stmt-path`
;;    -> may refine profile search
;;    -> may influence generation
;;
;; -> search profile for canidate when appropriate baed on `stmt-path`
;;    -> may return data to be compared against any/all/none
;;
;; -> resolution of any/all/none and comparison to profile search if something found
;;
;; -> if all 3 checks above return nada
;;    -> fallback to generation baed on
;;       -> `stmt-path`
;;       -> existing info relevant to `stmt-path`
;;          -> in same container as `stmt-path`
;;          -> outside of container of `stmt-path`
;;
;; FIXME: account for actors alignment per IRI within selection process
;;  - can just pass to `random/choose` along with the `rng`
;; FIXME: account for pattern ancestors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resolution of `objectStatementRefTemplate`
;;
;;  -> check if `objectStatementRefTemplate` is an IRI
;;
;;     -> yes, lookup template id from iri-map
;;           -> look back into history for statement which conforms to tempalte id
;;              -> found
;;                    -> parse "id", set as ["object" "id"]
;;                    -> set ["object" "objectType"] to "StatementRef"
;;              -> not found
;;                    -> warn: unable to find historic statement which matched the reference tempalte id
;;                    -> generate-statement for template-id
;;                    -> parse "id" from generation, set as ["object" "id"]
;;                    -> set ["object" "objectType"] to "StatementRef"
;;
;;     -> no, throw bc profile validation is broken 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resolution of `contextStatementRefTemplate`
;;
;;  -> check if `contextStatementRefTemplate` is an IRI
;;
;;     -> yes, lookup tempalte id from iri-map
;;           -> look back into history for statement which conforms to tempalte id
;;              -> found
;;                    -> parse "id", set as ["context" "statement" "id"]
;;                    -> set ["context" "statement" "objectType"] to "StatementRef"
;;              -> not found
;;                    -> warn: unable to find historic statement which matched the reference tempalte id
;;                    -> generate-statement for template-id
;;                    -> parse "id", set as ["context" "statement" "id"]
;;                    -> set ["context" "statement" "objectType"] to "StatementRef"
;;
;;     -> no, throw bc profile validation is broken 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inference on path to derive context for generation
;;
;;  -> inference returns context
;;      -> check if stmt already contains some other value within container `stmt-path` points at
;;      -> search for appropriate thing within profiles based on `stmt-path` + other values within container
;;      -> or generate based on context, bind to var prior to comparison, if not found in profiles
;;          -> compare to any/all/none
;;              -> nil, work off of values within none
;;                  -> there are some none values
;;                      -> shuffle components of none vals to create vals akin but not = to any of them ( •_•)>⌐■-■ ... (¬‿¬)
;;                  -> there are no none values
;;                      -> throw, generation was unable to even create fallback value based on context
;;              -> non-nil, return and set at stmt-path
;;
;;  -> inference returns nil
;;      -> check any/all/none for non-nil val
;;          -> any = non-nil
;;              -> return and set at stmt-path
;;              -> continue to all
;;          -> all = non-nil
;;              -> return and set at stmt-path
;;              -> continue to none
;;          -> none = non-nil
;;              -> shuffle components, return, set at stmt-path
;;              -> search profile for appropriate type of thing based on `stmt-path`
;;              -> throw, inference/generation was unable to
;;                  -> create fallback
;;                  -> any/all/none did not provide any vals
;;                  -> unable to find thing of correct type for `stmt-path` in profiles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rule points to a determining property
;;
;;  -> check if property was set at top level from statement template
;;     -> yes
;;         -> leave it as is
;;     -> no
;;         -> inference on path
;;         -> generation
;;         -> comparison to any/all/none
;;         -> add return to statement at path
;;
;; rule point to NON determining property
;;
;;  -> inference on path
;;  -> generation
;;  -> comparison to any/all/none
;;  -> add return to statement at path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; path point to an IRI property
;;
;;  -> was there any/all/none?
;;      -> yes
;;          -> does val resolve to iri-map item?
;;              -> yes, lookup then use
;;              -> no, use as is
;;      -> no
;;          -> is the IRI within a container with already set vals?
;;              -> yes
;;                  -> use to help refine profile search for example of appropriate thing
;;              -> no
;;                  -> generic search over profile for example of appropriate thing
;;          -> profile search returned anything?
;;              -> yes
;;                  -> use it
;;              -> no
;;                  -> generate based on inference given `stmt-path`
;;                  -> (str default-base-uri "property-type/" unique rng string val
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; path points to an extension
;;
;;  -> was there some `nested` reference?
;;      -> yes
;;          -> was it an iri?
;;              -> yes
;;                  -> found in iri map?
;;                      -> yes
;;                          -> parse "schema" or "inlineSchema"
;;                          -> generate based on properties found in the schema
;;                          -> compare to any/all/none
;;                          -> set as val at `stmt-path`
;;                      -> no
;;                          -> warn: reference to extension not found in profile
;;                          -> generate fallback extension
;;                              -> {"id" (str default-base-uri "extensions/")}
;;                          -> any/all/none or fallback
;;                          -> set as val at `stmt-path`
;;              -> no
;;                  -> was it splat?
;;                      -> yes
;;                          -> check for any existing extenion already in stmt
;;                          -> generate fallback if necessary
;;                          -> any/all/none or existing or fallback
;;                          -> use to update val at `stmt-path`
;;                      -> no
;;                          -> check for any existing extenion already in stmt
;;                          -> generate fallback extension if necessary
;;                          -> any/all/none or existing or fallback
;;                          -> use to update val at `stmt-path`
;;      -> no
;;          -> check for any existing extenion already in stmt
;;          -> generate fallback extension if necessary
;;          -> any/all/none or existing or fallback
;;          -> use to update val at `stmt-path`
;;
;; TODO: account for `recommendedActivityTypes` or `recommendedVerbs` expansion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; path ponits to a numeric field
;;
;;  -> are there any inherent constraints given the property?
;;      -> yes
;;          -> keep track of for use in rng generation
;;          -> check for further constraints established by container
;;      -> no
;;          -> check container for constraints
;;
;;  -> are there any existing constraints found within container?
;;      -> yes, use as constraints within rng-generation
;;          -> min?
;;          -> max?
;;          -> min and max?
;;      -> no, use inherent constraints only in generation
;;
;;  -> gen based on derived constraints if any
;;      -> comparison to any/all/none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; path ponits to a string field
;;
;; -> inference -> gen-rng-string -> compare to any/all/none
;;
;; TODO: gen-sym but deterministic
;; - rng
;; - context from `stmt-path`
;;   - is it an email?
;;   - is it a correct response pattern?
;;   - is it an arbitrary string?
;;
;; path points to a boolean field
;;
;; -> does any part of the existing statement determine what the bool should be?
;; -> does any/all/none provide any guidence?
;; -> if no to either ^, rng-selection of either true or false
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
