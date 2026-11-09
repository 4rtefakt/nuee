# Réajuste tous les modèles des projets exportés et compare avec les résultats enregistrés.
#   Rscript moteur/tests/verifier_references.R <dossier des .json> <fichier .csv de résultats> [coeurs]
# Le Q2 est donné à part, hors de la comparaison : la validation croisée de Nuée (chaque groupe
# prédit par un modèle ajusté sans lui, mise à l'échelle comprise) n'est pas celle qui a écrit les
# fichiers.

args <- commandArgs(trailingOnly = TRUE)
dossier <- args[1]
sortie <- args[2]
coeurs <- if (length(args) > 2) as.integer(args[3]) else 1L

# Écart relatif maximal entre deux matrices (une colonne par composante), au signe près ;
# renvoie aussi si les signes enregistrés sont retrouvés.
ecart <- function(ref, r) {
  if (is.null(ref) || is.null(r) || !all(dim(ref) == dim(r))) return(c(ecart = Inf, signe = NA))
  pire <- 0
  memes <- TRUE
  for (a in seq_len(ncol(r))) {
    ok <- !is.na(ref[, a]) & !is.na(r[, a])
    if (!any(ok)) next
    d1 <- max(abs(ref[ok, a] - r[ok, a]))
    d2 <- max(abs(ref[ok, a] + r[ok, a]))
    memes <- memes && d1 <= d2
    pire <- max(pire, min(d1, d2) / max(abs(ref[ok, a]), 1e-12))
  }
  c(ecart = pire, signe = memes)
}

# Matrice enregistrée (une ligne par composante) en colonnes, limitée à A composantes.
colonnes <- function(M, A) {
  if (is.null(M)) return(NULL)
  M <- if (is.matrix(M)) M else matrix(M, nrow = 1)
  t(M[seq_len(min(A, nrow(M))), , drop = FALSE])
}

verifier_fichier <- function(f) {
  source("moteur/charger.R")
  p <- tryCatch(lire_projet(f), error = function(e) NULL)
  if (is.null(p)) return(data.frame(fichier = f, modele = NA, statut = "illisible"))
  lignes <- list()
  for (d in p$models) {
    base <- data.frame(fichier = f, modele = d$name %||% NA, type = d$type %||% NA,
                       version = d$version_origine %||% NA, A = d$A %||% NA, statut = "ok")
    if (!is.null(d$error)) { base$statut <- "erreur export"; lignes[[length(lignes) + 1]] <- base; next }
    if (!isTRUE(d$fitted) || !d$A) { base$statut <- "non ajusté"; lignes[[length(lignes) + 1]] <- base; next }
    r <- tryCatch(ajuster(p, d), error = function(e) conditionMessage(e))
    if (is.character(r)) {
      base$statut <- paste("erreur :", substr(r, 1, 80))
      lignes[[length(lignes) + 1]] <- base
      next
    }
    ref <- d$reference$components
    comp <- resume_composantes(r)
    val <- function(cle) vapply(ref, function(x) as.numeric(x[[cle]] %||% NA), 0)
    base$N <- r$N
    base$K <- r$K
    base$manquantes <- sum(r$MX == 0) + if (is.null(r$MY)) 0 else sum(r$MY == 0)
    ws <- d$solution$workset_x
    base$donnees <- if (is.null(ws) || !all(dim(as.matrix(ws)) == dim(r$x$X))) NA else
      max(abs(as.matrix(ws) - r$x$X), na.rm = TRUE) / max(abs(as.matrix(ws)), na.rm = TRUE)
    base$R2X <- max(abs(comp$R2Xcum - val("R2X(cum)")))
    base$R2Y <- if (!is.null(comp$R2Ycum)) max(abs(comp$R2Ycum - val("R2Y(cum)"))) else NA
    base$Q2 <- max(abs(comp$Q2 - val("Q2")))
    base$Q2cum <- max(abs(comp$Q2cum - val("Q2(cum)")))
    e <- ecart(colonnes(d$solution$T, r$A), r$T)
    base$T <- e[["ecart"]]
    base$signeT <- e[["signe"]]
    base$P <- ecart(colonnes(d$solution$P, r$A), r$P)[["ecart"]]
    if (!is.null(r$W)) base$W <- ecart(colonnes(d$solution$W, r$A), r$W)[["ecart"]]
    # statistiques dérivées, contre les valeurs stockées dans le fichier ou calculées par
    # usp_extract.py à partir de la solution stockée (formules vérifiées sur un rapport v17)
    s <- r$statistiques
    oc <- d$reference$obs_cols
    rel <- function(a, b) {
      if (is.null(a) || is.null(b) || length(a) != length(b)) return(NA)
      ok <- !is.na(a) & !is.na(b)
      if (!any(ok)) return(NA)
      max(abs(a[ok] - b[ok])) / max(abs(b[ok]), 1e-12)
    }
    base$valeur_propre <- rel(s$composantes$valeur_propre, val("Eigenvalue"))
    base$T2 <- rel(s$observations$T2, oc$T2)
    base$DModX_absolu <- rel(s$observations$DModX_absolu, oc[["DModX (absolute)"]])
    base$DModX_pondere <- rel(s$observations$DModX_pondere, oc[["DModX (weighted)"]])
    lim <- d$reference$limits
    base$T2Crit <- rel(s$limites[["T2Crit(95%)"]], lim[["T2Crit(95%)"]])
    base$DCrit <- rel(s$limites[["DCrit(5%)"]], lim[["DCrit(0.05)"]])
    if (identical(d$type, "PLS")) {
      rv <- d$reference$vars
      rx <- rv[vapply(rv, function(v) identical(v$Block, "X") && !identical(v$Term, "Constant"), TRUE)]
      base$VIP <- rel(s$variables$VIP, vapply(rx, function(v) as.numeric(v$VIP %||% NA), 0))
      cles <- paste("CoeffCS", d$reference$ynames)
      base$coefficients <- max(sapply(seq_along(cles), function(j)
        rel(s$coefficients[seq_len(r$K), 1 + 4 * (j - 1) + 1], vapply(rx, function(v) as.numeric(v[[cles[j]]] %||% NA), 0))))
      base$RMSEE <- rel(unname(s$RMSEE), unlist(lim$RMSEE))
    }
    for (dd in p$dendrograms) {
      if (!identical(as.numeric(dd$model), as.numeric(d$number))) next
      arbre <- classification(r, dd$components)
      stocke <- sort(as.matrix(dd$merges)[, 3])
      base$HCA <- rel(sort(arbre$hauteur_ward), stocke)
    }
    lignes[[length(lignes) + 1]] <- base
  }
  do.call(rbind, lapply(lignes, function(l) { l[setdiff(NOMS, names(l))] <- NA; l[NOMS] }))
}

NOMS <- c("fichier", "modele", "type", "version", "A", "statut", "N", "K", "manquantes", "donnees",
          "R2X", "R2Y", "Q2", "Q2cum", "T", "signeT", "P", "W", "valeur_propre", "T2", "DModX_absolu",
          "DModX_pondere", "T2Crit", "DCrit", "VIP", "coefficients", "RMSEE", "HCA")

fichiers <- list.files(dossier, "\\.json$", recursive = TRUE, full.names = TRUE)
# un seul exemplaire de chaque projet (le même fichier .usp est souvent copié plusieurs fois)
signature <- vapply(fichiers, function(f) {  # taille du fichier sans le chemin d'origine
  debut <- readChar(f, 4000, useBytes = TRUE)
  src <- regmatches(debut, regexpr('"source":"[^"]*"', debut, useBytes = TRUE))
  as.character(file.size(f) - nchar(src, type = "bytes"))
}, "")
fichiers <- fichiers[!duplicated(signature)]
cat(length(fichiers), "projets distincts\n")

if (coeurs > 1) {
  cl <- parallel::makeCluster(coeurs)
  parallel::clusterExport(cl, c("ecart", "colonnes", "NOMS"))
  res <- parallel::parLapplyLB(cl, fichiers, verifier_fichier)
  parallel::stopCluster(cl)
} else {
  res <- lapply(fichiers, verifier_fichier)
}
res <- do.call(rbind, res)
utils::write.csv(res, sortie, row.names = FALSE)

ok <- res$statut == "ok"
cat(sum(ok), "modèles réajustés,", sum(!ok), "autres :\n")
print(table(res$statut[!ok]))
seuil <- function(x, s) sum(!is.na(x) & x < s)
for (typ in c("PCA", "PLS")) {
  for (manq in c(FALSE, TRUE)) {
    s <- res[ok & res$type == typ & (res$manquantes > 0) == manq, ]
    if (!nrow(s)) next
    cat(sprintf("%s %s (%d) : données %d, R2X %d, R2Y %d, T %d, P %d (écart < 1e-4) ; signes de T identiques %d\n",
                typ, if (manq) "avec manquantes" else "complets", nrow(s), seuil(s$donnees, 1e-4),
                seuil(s$R2X, 1e-4), seuil(s$R2Y, 1e-4), seuil(s$T, 1e-4), seuil(s$P, 1e-4),
                sum(s$signeT %in% TRUE)))
  }
}
cat("\nQ2, hors de la comparaison (écart absolu au Q2 du fichier, plus grand sur les composantes : médiane / 90e centile) :\n")
for (typ in c("PCA", "PLS")) {
  for (manq in c(FALSE, TRUE)) {
    s <- res[ok & res$type == typ & (res$manquantes > 0) == manq, ]
    if (!nrow(s)) next
    q <- function(x) stats::quantile(x, c(0.5, 0.9), na.rm = TRUE, names = FALSE)
    cat(sprintf("  %s %-16s Q2 %.3f / %.3f, Q2(cum) %.3f / %.3f\n", typ, if (manq) "avec manquantes" else "complets",
                q(s$Q2)[1], q(s$Q2)[2], q(s$Q2cum)[1], q(s$Q2cum)[2]))
  }
}
cat("\nStatistiques dérivées (écart relatif < 1e-3 / comparés) :\n")
for (k in c("valeur_propre", "T2", "DModX_absolu", "DModX_pondere", "T2Crit", "DCrit", "VIP",
            "coefficients", "RMSEE", "HCA")) {
  for (manq in c(FALSE, TRUE)) {
    x <- res[[k]][ok & (res$manquantes > 0) == manq]
    cat(sprintf("  %-14s %-16s %4d / %d\n", k, if (manq) "avec manquantes" else "complets",
                seuil(x, 1e-3), sum(!is.na(x))))
  }
}
