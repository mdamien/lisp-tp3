;;Algorithme du parcours en profondeur d'abord de la base des regles
;DFS_engine(facts rules &aux applied)
;	applicable_rules = search_conflict(facts rules applied)
;	back_facts = facts
;	if applicable_rules not NULL then
;		foreach rule_item in applicable_rules do 
;			facts = backs_facts
;			foreach action_item in induction(item) do 
;				facts = facts + execution(action_item)
;			endfor
;			DFS_engine(facts rules (cons applied rule_item))
;		endfor
;	endif
;end

(defun DFS_engine (facts rules &optional applied)
	(let ((applicable_rules (search_candidates facts rules applied)) (back_facts facts))
		(if (null applicable_rules) (setq applicable_rules (take_first *rules* applied)))
		(when (not (null applicable_rules))
			(dolist (rule_item applicable_rules)
				(setq facts back_facts)
				(dolist (action_item (get_actions rule_item))
					(print action_item)
					(if (execute_action action_item facts) T (return-from DFS_engine *solutions*))
				)
				(push rule_item applied)
				(DFS_engine facts rules applied)
			)
		)
	)
)



;;Algorithme de recherche de règles candidates
;search_candidates (fact rules applied &aux out)
;	si rules est vide alors 
;		retourner out
;	sinon
;		flag = true
; 		studied_rule = car(rules)
;		if id(studied_rule) not in applied then
; 			premises_studied_rule = car (studied_rule)
;			foreach premise in premises_studied_rule do 
;				if premise not in facts then
;					flag = false
;					sortir de la boucle
;			if flag == true then
;				push(studied_rule, out)		
;		return search_candidates (fact cdr(rules) applied out)


(defun search_candidates (facts rules applied &optional out)
	(cond
		((null rules) (return-from search_candidates out))
		(t
			(let ((flag T)(studied_rule (car rules)))
				(when (not (member studied_rule applied))
					(let ((premises_studied_rule (get_premises studied_rule)))
						(dolist (premise premises_studied_rule)
							(when (not (verify_prem facts premises_studied_rule))
								(setq flag NIL)
								(return)
							)
						)
						(if flag (setq out (nconc out (list studied_rule))))
					)
				)
			)
			(search_candidates facts (cdr rules) applied out)
		)
	)
)

;;;L'algorithme ci-dessous fonctionne mais n'est pas utilisable dans le cas d'un système expert de niveau 0+
;;Algorithme de la fonction d'inclusion
;in? (element, set)
;	if set is empty then
; 		return NIL
;	else
;		flag = true
; 		studied = car(set)
;		if length(element) != length(studied) then
;			return NIL
;		for i=0 to length(studied) do
;			if element[i] != item[i] then
;				flag=false 
;				sortir de la boucle
;		if flag == true then
;			return true
;		else
;			return in(element, cdr(set))

;(defun in? (element set)
;	(cond
;		((null set) NIL)
;		(t
;			(let ((flag T)(studied (car set)))
;				(if (not (eq (length element) (length studied))) (return-from in? NIL))
;				(loop for i from 0 to (- (length studied) 1) do
;					(when (not (equal (nth i element) (nth i studied)))
;						(setq flag NIL)
;						(break)
;					)
;				)
;				(if flag T (in? element (cdr set)))
;			)
;		)
;	)
;)		


;;Algorithme de la fonction de vérification de premises
(defun verify_prem (facts premises)
	(dolist (premise premises)
		(cond 
			((null facts) NIL)
			((equal (car premise) 'is-known) (if (get_fact_value (in_fact? (cadr premise) facts) facts) T (return-from verify_prem NIL)))
			((equal (car premise) 'is-unknown) (if (get_fact_value (in_fact? (cadr premise) facts) facts) (return-from verify_prem NIL) T))
			(t  
				(let ((val_fact_1 (get_fact_value (in_fact? (cadr premise) facts) facts)) (val_fact_2))
					(if val_fact_1 
						(if (and (not (stringp (caddr premise))) (not (numberp (caddr premise)))) 
							(progn
								(setq val_fact_2 (get_fact_value (in_fact? (cadr premise) facts) facts))
								(if val_fact_2
									(if (apply (car premise) (list val_fact_1 val_fact_2)) T (return-from verify_prem NIL))
									(return-from verify_prem NIL)
								)
							)
							(if (apply (car premise) (list val_fact_1 (caddr premise))) T (return-from verify_prem NIL))
						)
						(return-from verify_prem NIL)
					)	
				)
			)
		)
	)
	T	
)

;;Fonction qui retourne la première règle qui n'est pas dans la liste des règles déjà appliquées

(defun take_first (rules applied)
	(cond
		((null rules) NIL)
		((not (member (car rules) applied)) (list (car rules)))
		(t 
			(take_first (cdr rules) applied)
		)
	)
)

;;Fonction qui retourne l'identifiant d'un fait en fonction de la variable
;(defun get_var (fact bfacts)
;	(cond 
;		((null bfacts) NIL)
;		((equal (car (symbol-value (car bfacts))) fact) (car bfacts))
;		(T (get_var fact (cdr bfacts)))
;	)
;)

;;Fonction get_fact qui retourne le fait à paritr de son id
(defun get_fact (id_fact facts)
	(if (member id_fact facts) (symbol-value id_fact))
)

;;Fonction get_fact_value qui retourne la valeur d'un fait si elle existe
(defun get_fact_value (id_fact facts)
	(if (member id_fact facts) (if (cadr (symbol-value id_fact)) (caddr (symbol-value id_fact))))
)

;;Fonction get_premises qui retourne les premises d'une règle
(defun get_premises (id_rule)
	(if (member id_rule *rules*) (cadr (symbol-value id_rule)))
)

;;Fonction get_actions qui retourne les actions induites par une règle
(defun get_actions (id_rule)
	(if (member id_rule *rules*) (caddr (symbol-value id_rule)))
)

;;Fonction get_actions qui retourne les actions induites par une règle
;(defun get_actions (rule)
;	(if (memberRules? rule *rules*) (caddr rule))
;)

;(defun memberRules? (rule rules)
;	(cond 
;		((null rules) NIL)
;		((equal (car rule) (car (car rules))) T)
;		(t (memberRules? rule (cdr rules)))
;	)
;)

;;Fonction set_value qui assigne une valeur à un fait
(defun set_value (id_fact value)
	(setf (cadr (symbol-value id_fact)) T)
	(setf (caddr (symbol-value id_fact)) value)
	id_fact
)

;;Fonction unset_value qui enlève une valeur à un fait
(defun unset_value (id_fact)
	(setf (cadr (symbol-value id_fact)) NIL)
	(setf (caddr (symbol-value id_fact)) 42)
	id_fact
)


;;Fonction qui pose une question
(defun ask_question (fact bfacts question)
	(print question)
	(let ((answer (read))(id_fact (in_fact? fact bfacts)))
		;La ligne du dessous sert à mettre en string si nécessaire
		(if (not (numberp answer)) (setq answer (string answer)))
		(cond
		 	(id_fact 
				(setf (cadr (get_fact id_fact bfacts)) T)
				(setf (caddr (get_fact id_fact bfacts)) answer)
				(symbol-value id_fact)
			)
			(t 
				(create_fact fact t answer)
			)		
		)
	)
)


;;Fonction qui cherche un fait dans la base de faits
(defun in_fact? (fact bfacts)
	(cond
		((null bfacts) NIL)
		((equal (car (symbol-value (car bfacts))) fact) (car bfacts))
		(t (in_fact? fact (cdr bfacts)))
	)
)

;;Fonction qui créer un fait
(defun create_fact (fact &optional known? value)
	(let ((id_fact (gentemp "F")))
		(set id_fact (list fact known? value))
		(setq *facts* (nconc *facts* (list id_fact)))
		id_fact
	)
)

;;Fonction qui créer les règles
(defun create_rule (premise induction)
	(let ((id_rule (gentemp "R")))
		(set id_rule (list id_rule premise induction))
		(setq *rules* (nconc *rules* (list id_rule)))
		id_rule
	)
)

;;Algorithme d'execution des actions induites par l'application des règles
(defun execute_action (action facts)
	(case (car action)
		(solution (print (cadr action)) (setq *solutions* (nconc *solutions* (list (cadr action))))
			(print "Souhaitez-vous chercher d'autres solutions ? Y/N")
			(setq answer (read))
			(if (or (equal answer 'y) (equal answer 'Y)) T (return-from execute_action NIL))
		)
		(set 
			(let ((id_fact (in_fact? (cadr action) facts)))
				(if id_fact (set_value id_fact (caddr action)) (create_fact facts (cadr action) T (caddr action)))
			)
			T	
		)
		(ask-question 
			(ask_question (cadr action) facts (caddr action))
			T
		)
	)
	T
)

;Questions
(
(ERREUR_NAVIGATEUR "Quelle erreur est présente dans la navigateur ?")
(CHANGE_RECENT "Avez-vous récemment changer de DNS ?")
(HEURES_DEPUIS_CHANGEMENT "De combien de temps date ce changement ?")
(
(RENOUVELLEMENT_DNS "Avez-vous renouveler votre domaine ?")
(ACCES_PAR_IP_POSSIBLE "Si vous avez l'IP du service, pouvez-vous y acceder par adresse IP?")
(SITE_COURANTS_ACCESSIBLES "Pouvez-vous accéder à vos sites courants tel que google.fr, apple.fr ou viedemerde.com ?")
(PB_HEBERGEUR "Votre hebergeur à t-il reporté des problémes sur son infrastructure ?")
(SERVEUR_APP "Quel serveur d'application utilisez vous ? Apache, NGinx, ...")
)


(create_rule '((= ERREUR_NAVIGATEUR ERREUR_DNS)) '((set PROBLEME_DNS "True")))
(create_rule '((equal PROBLEME_DNS "True") (equal CHANGE_RECENT "True") (< HEURES_DEPUIS_CHANGEMENT 6)) '((solution "Probléme de propagation DNS")))
(create_rule '((equal RENOUVELLEMENT_DNS "False")) '((solution "Probléme de renouvellement DNS")))
(create_rule '((equal ACCES_PAR_IP_POSSIBLE "True")) '((solution "Autre probléme DNS")))
(create_rule '((equal SITE_COURANTS_ACCESSIBLES "False")) '((ask-question RENOUVELLEMENT_DNS "Avez-vous renouveler votre domaine ?")))
(create_rule '((= DNS_FAILED 650)) '((solution "Probléme hebergeur")))
(create_rule '((= ERREUR_NAVIGATEUR 502) (= SERVEUR_APP NGINX)) '((solution "Probléme de configuration NGINX")))
(create_rule '((= ERREUR_NAVIGATEUR 500)) '((solution "Probléme applicatif")))

;;Algorithme de la fonction de vérification de prémise
;(defun verify_prem (facts premise)
;	(cond 
;		((null facts) NIL)
;		(t 
;			(let ((studied_fact (get_fact (car facts) facts)))
;				(when (equal (car studied_fact) (cadr premise))
;					(if (not (cadr studied_fact)) (return-from verify_prem NIL))
;					(setf (cadr premise) (caddr studied_fact))
;					(return-from verify_prem (apply (car premise) (cdr premise)))
;				)
;			)
;			(verify_prem (cdr facts) premise)
;		)
;	)
;)


