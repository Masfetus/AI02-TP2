; Pour exécuter le test :
;   - Lancer AI02-TP2.lpr avec Allegro Common Lisp
;   - Accéder à la console de débogage
;   - Entrer (askQuestions) dans la console


(setq relations '((I E) (S N) (F T) (J P))) ; Conserver l'ordre des éléments dans les listes (premier élément = réponse positive)
(defun askQuestions() ; Procédure principale à appeler pour démarrer le test
    (let ((currentQuestion nil) ; Question posée : (F T "Il est important de faire plaisir aux autres")
          (questions (initListFromFile "../data/questions.dat")) ; Récupération de la liste construite à partir du fichier
          (currentProfile nil)
          (resultat '( (I . 0) (E . 0) (S . 0) (N . 0) (F . 0) (T . 0) (J . 0) (P . 0)))
          (rates (initListFromFile "../data/rates.dat"))) ; T si l'utilisateur a répondu OUI/VRAI, NIL sinon

    (setq questions (reverse questions)) ; On reverse les questions (facultatif)
    (until (null (setq currentQuestion (pop questions))) ; On pose les questions jusqu'à consommer l'ensemble de ces questions
           (format t "~a (VRAI/FAUX) : " (caddr currentQuestion))
           (if (eq (setq answer (readAnswer)) T) 
                (setq currentProfile (assoc (car currentQuestion) resultat))
                (setq currentProfile (assoc (cadr (assoc (car currentQuestion) relations)) resultat))
           )
           (setf (cdr currentProfile) (+ (cdr currentProfile) 1)) ;On incrémente le score du type correspondant
    )    
    (setq profile (determineProfil resultat relations))  ; On a un profil ici retourné sous forme de liste : (I N S J)
    (setq profile-symbol (intern (format nil "~{~A~}" profile))) ; On transforme les éléments de la liste en un symbole : (I N S J) => 'INSJ
    (format t "~%----------------------------------------------------------------------------------------------------------------------~%")
    (format t "Selon le test MBTI, vous êtes ~{~A~}.~%Félicitations ! Vous représentez ~d % de la population !~%" profile (cdr (assoc profile-symbol rates)))
    (setq groups (checkGroups profile (initListFromFile "../data/rules.dat"))) ; On va ici déterminer les groups correspondants au profil
    (format t "Vous êtes ~a mais avant tout ~a.~%" (string (cadr groups)) (string (car groups))) ; Le groupe caractéristique se trouvant toujours en premier élément
    (format t "Pour avoir plus de renseignements : https://www.16personalities.com/~a-personality~%" (string-downcase (string profile-symbol)))
    (format t "----------------------------------------------------------------------------------------------------------------------~%")
    'ASKQUESTIONS)
)
(defun checkGroups(BdF BdR)
    (let ((result BdF) ; Base de faits en résultat (Analyste Innovateur I N F J) par exemple
          (continuer T) ; Détecter un changement dans la base de faits, auquel cas on réexamine les règles
          (currentRule nil) ; Règle en cours de vérification
          (rulesToCheck BdR)) ; Règles restantes à analyser
        (while (eq continuer T)
            (progn 
                (setq continuer nil)
                (setq rulesToCheck BdR)
                (while (not (null rulesToCheck)) ; Tant qu'il reste des règles à examiner
                    (setq currentRule (car rulesToCheck)) ; On commence par la première règle
                    (let ((profileCondition (cadr currentRule)) ; On récupère les faits qui doivent être vrais
                        (conclusion (caddr currentRule))) ; On récupère la conclusion si jamais tous les faits sont présents
                        (if (= (length (intersection profileCondition result)) (length profileCondition))
                        ; Si l'intersection entre les faits à vérifier et les faits présents est de même taille que les faits à vérifier, c'est qu'ils sont tous présents
                            (progn 
                                (pushnew conclusion result) ; On ajoute à la base de faits 
                                (setq continuer T) ; On annonce qu'il y a eu un changement
                                (setf BdR (remove (car currentRule) BdR :key #'car)) ; On retire la règle vérifiée de la base de règles
                            )                        
                        )
                        (setq rulesToCheck (cdr rulesToCheck)) ; On avance dans les règles restantes à analyser
                    )
                )
            )
        )
    result)
)
(defun determineProfil(resultats typesRegle)
    (let ((result '()))
        (dolist (typeRelation typesRegle) ; On va se baser sur les types contradictoires (qui ne peuvent pas être tous deux présents : I et E)
            (let ((typeA (car typeRelation)) ; Premier type
                  (typeB (cadr typeRelation))) ; Deuxième type
                (if (>= (cdr (assoc typeA resultats)) (cdr (assoc typeB resultats))) ; On ajoute ici le type ayant obtenu le score maximal
                    (push typeA result)
                    (push typeB result)
                )
            
            )
        )
        (reverse result)
    )
)
(defun initListFromFile(path)
    (let* ((in (open path)) ; Ouverture du fichier
        (currentLine (read in nil)) ; On commence à lire la première ligne
        (resultList nil))
    (until (null currentLine) ; On parse toutes les lignes du fichier
        (progn 
            (push currentLine resultList)
            (setq currentLine (read in nil)) ;On désactive la génération d'erreur pour mettre la valeur à nil à la place
        )
    )
    (close in)
    resultList)
)
(defun readAnswer() ; Retourne T si la réponse fait partie de la liste ci-dessous (réponse positive), retourne NIL sinon
    (let ((result NIL))
        (setq result (member (read) '(VRAI V OUI YES Y O TRUE T)))
        (setq result (not (null result)))
    result)
)