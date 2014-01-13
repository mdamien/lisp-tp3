(R1
( (= ERREUR_NAVIGATEUR "DNS"))
( (set PROBLEME_DNS "Oui"))
)
(R2
((= PROBLEME_DNS Oui) (= CHANGE_RECENT "Oui") (< HEURES_DEPUIS_CHANGEMENT 6)
((solution "Probléme de propagation DNS"))
)
(R3
((= RENOUVELLEMENT_DNS "Non"))
((solution "Probléme de renouvellement DNS"))
)
(R4
((= ACCES_PAR_IP_POSSIBLE "Oui"))
((solution "Autre probléme DNS: Verifier que vous êtes bien connecté à un serveur DNS fonctionnel ou connectez vous sur un service externe"))
)
(R5
((= SITE_COURANTS_ACCESSIBLES "Oui"))
((solution "Probléme général d'accés à Internet par le client: Essayez de rétablir votre connexion internet"))
)
(R6
((= PB_HEBERGEUR "Oui"))
((solution "Celà viens surement de votre hébergeur, contactez le"))
)
(R6.5
((= ERREUR_NAVIGATEUR 502))
((solution "Regardez les logs de votre serveur"))
)
(R7
((= ERREUR_NAVIGATEUR 502) (= SERVEUR_APP NGINX))
((solution "Il y a plusieurs solutions possibles:
* Nginx est en proxy avec Apache et Apache et injoignable (ou un autre serveur web)
* Configuration du buffer et du timeout insuffisante, essayez ces réglages"))
)
(R8
((= ERREUR_NAVIGATEUR 500))
((solution "Probléme applicatif: activez le debug de votre application (PHP, Java, ...) et verifiez les logs du serveur"))
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
(SERVEUR_APP "Quel serveur d'application utilisé vous ? Apache, NGinx, ...")
)