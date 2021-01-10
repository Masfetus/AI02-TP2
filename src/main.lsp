(setq relations '((I E) (S N) (F T) (J P))) ; Conserver l'ordre des éléments dans les listes (premier élément = réponse positive)
(defun askQuestions()
    (setq profil '( (I . 0) (E . 0) (S . 0) (N . 0) (F . 0) (T . 0) (J . 0) (P . 0)))
    (let ((currentQuestion nil) ; Question posée : (F T "Il est important de faire plaisir aux autres")
          (questions (initListFromFile "../data/questions.dat")) ; Récupération de la liste construite à partir du fichier
          (answer nil)
          (currentProfile nil)) ; T si l'utilisateur a répondu OUI/VRAI, NIL sinon
    (setq questions (reverse questions)) ; On reverse les questions (facultatif)
    (until (null (setq currentQuestion (pop questions))) ; On pose les questions jusqu'à consommer l'ensemble de ces questions
           (format t "~a (VRAI/FAUX) : " (caddr currentQuestion))
           (if (eq (setq answer (readAnswer)) T) 
                (setq currentProfile (assoc (car currentQuestion) profil))
                (setq currentProfile (assoc (cadr (assoc (car currentQuestion) relations)) profil))
           )
           (setf (cdr currentProfile) (+ (cdr currentProfile) 1))
           (format t "~a ~%" profil)
    )
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