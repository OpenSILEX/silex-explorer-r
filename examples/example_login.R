# Exemple d'utilisation de la fonction login avec l'instance demo

# Spécifier les informations de connexion
id <- "ton_email@exemple.com"
password <- "ton_mot_de_passe"
instance <- "http://138.102.159.36:8084"
urlGraphql <- "http://138.102.159.37/graphql"

# Appeler la fonction login
session <- login(id = id, password = password, instance = instance, urlGraphql = urlGraphql)

# Afficher le token pour vérifier la connexion
print(session$token)

