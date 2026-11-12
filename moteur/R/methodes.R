# Le texte d'une section « Méthodes » pour un article, écrit d'après le modèle ajusté :
# données, prétraitement, méthode, validation croisée, qualité du modèle, logiciel, et les
# références correspondantes. En français (langue = "fr") ou en anglais ("en").
# Renvoie list(paragraphes, references).

REFERENCES <- list(
  acp = "Wold, S., Esbensen, K., & Geladi, P. (1987). Principal component analysis. Chemometrics and Intelligent Laboratory Systems, 2(1–3), 37–52. https://doi.org/10.1016/0169-7439(87)80084-9",
  pls = "Wold, S., Sjöström, M., & Eriksson, L. (2001). PLS-regression: a basic tool of chemometrics. Chemometrics and Intelligent Laboratory Systems, 58(2), 109–130. https://doi.org/10.1016/S0169-7439(01)00155-1",
  plsda = "Barker, M., & Rayens, W. (2003). Partial least squares for discrimination. Journal of Chemometrics, 17(3), 166–173. https://doi.org/10.1002/cem.785",
  opls = "Trygg, J., & Wold, S. (2002). Orthogonal projections to latent structures (O-PLS). Journal of Chemometrics, 16(3), 119–128. https://doi.org/10.1002/cem.695",
  nipals = "Nelson, P. R. C., Taylor, P. A., & MacGregor, J. F. (1996). Missing data methods in PCA and PLS: Score calculations with incomplete observations. Chemometrics and Intelligent Laboratory Systems, 35(1), 45–65. https://doi.org/10.1016/S0169-7439(96)00007-X",
  cv_anova = "Eriksson, L., Trygg, J., & Wold, S. (2008). CV-ANOVA for significance testing of PLS and OPLS models. Journal of Chemometrics, 22(11–12), 594–600. https://doi.org/10.1002/cem.1187",
  pareto = "van den Berg, R. A., Hoefsloot, H. C. J., Westerhuis, J. A., Smilde, A. K., & van der Werf, M. J. (2006). Centering, scaling, and transformations: improving the biological information content of metabolomics data. BMC Genomics, 7, 142. https://doi.org/10.1186/1471-2164-7-142",
  snv = "Barnes, R. J., Dhanoa, M. S., & Lister, S. J. (1989). Standard normal variate transformation and de-trending of near-infrared diffuse reflectance spectra. Applied Spectroscopy, 43(5), 772–777. https://doi.org/10.1366/0003702894202201",
  msc = "Geladi, P., MacDougall, D., & Martens, H. (1985). Linearization and scatter-correction for near-infrared reflectance spectra of meat. Applied Spectroscopy, 39(3), 491–500. https://doi.org/10.1366/0003702854248656",
  sg = "Savitzky, A., & Golay, M. J. E. (1964). Smoothing and differentiation of data by simplified least squares procedures. Analytical Chemistry, 36(8), 1627–1639. https://doi.org/10.1021/ac60214a047",
  r = "R Core Team (%s). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/",
  nuee = "4rtefakt (%s). Nuée%s [Computer software]. https://github.com/4rtefakt/nuee")

texte_methodes <- function(projet, d, m, langue = c("fr", "en"), version = "") {
  langue <- match.arg(langue)
  if (est_regression(d)) return(texte_methodes_regression(projet, d, m, langue, version))  # regression.R
  if (est_autre_analyse(d)) return(fonction_analyse("texte_methodes_", d$type)(projet, d, m, langue, version))  # analyses.R
  fr <- langue == "fr"
  s <- m$statistiques
  nombre <- function(x, chiffres = 2) formatC(x, format = "f", digits = chiffres, decimal.mark = if (fr) "," else ".")
  pourcent <- function(x) paste0(formatC(100 * x, format = "f", digits = if (100 * x < 10) 1 else 0, decimal.mark = if (fr) "," else "."), if (fr) " %" else "%")
  valeur_p <- function(p) if (is.na(p)) NA else if (p < 0.001) (if (fr) "p < 0,001" else "p < 0.001") else paste("p =", formatC(p, digits = 2, format = "g", decimal.mark = if (fr) "," else "."))
  liste <- function(x) if (length(x) <= 1) paste(x) else paste(paste(x[-length(x)], collapse = ", "), if (fr) "et" else "and", x[length(x)])
  cites <- character(0)
  cite <- function(cle, texte) { cites <<- union(cites, cle); texte }
  guillemets <- function(x) if (fr) sprintf("« %s »", x) else sprintf("“%s”", x)

  simples <- Filter(function(t) length(t$variables) == 1, d$x)
  uids <- unique(vapply(simples, function(t) as.numeric(t$variables), 0))
  qualitatives <- Filter(function(u) length(variable_du_projet(projet, u)$categories), uids)
  croises <- Filter(function(t) length(t$variables) > 1, d$x)
  total_obs <- length(projet$observations)
  manquantes_x <- 1 - mean(m$MX)
  opls <- isTRUE(d$opls)
  plsda <- isTRUE(d$plsda)
  pls <- identical(d$type, "PLS")

  # --- données
  p1 <- if (fr) sprintf("Le modèle porte sur %d observations%s et %d variables X%s%s.", m$N,
                        if (m$N < total_obs) sprintf(" (sur %d ; %d exclues)", total_obs, total_obs - m$N) else "",
                        length(uids), if (length(qualitatives)) sprintf(", dont %d qualitatives codées par des indicatrices 0/1", length(qualitatives)) else "",
                        if (length(croises)) sprintf(", avec %d termes croisés ou carrés", length(croises)) else "")
        else sprintf("The model uses %d observations%s and %d X variables%s%s.", m$N,
                     if (m$N < total_obs) sprintf(" (of %d; %d excluded)", total_obs, total_obs - m$N) else "",
                     length(uids), if (length(qualitatives)) sprintf(", %d of them qualitative, coded as 0/1 dummy variables", length(qualitatives)) else "",
                     if (length(croises)) sprintf(", with %d cross or squared terms", length(croises)) else "")
  if (plsda) {
    classes <- m$y$noms
    effectifs <- as.vector(table(factor(s$observations$classe, levels = classes)))
    classe <- if (!is.null(d$classe)) as.character(variable_du_projet(projet, d$classe)$ids[1]) else NULL
    p1 <- c(p1, if (fr) sprintf("Les %d classes%s : %s.", length(classes), if (!is.null(classe)) paste(" de", guillemets(classe)) else "",
                                liste(sprintf("%s (n = %d)", sub("^.*\\(([^()]*)\\)$", "\\1", classes), effectifs)))
                else sprintf("The %d classes%s: %s.", length(classes), if (!is.null(classe)) paste(" of", guillemets(classe)) else "",
                             liste(sprintf("%s (n = %d)", sub("^.*\\(([^()]*)\\)$", "\\1", classes), effectifs))))
  } else if (pls) {
    ny <- m$y$noms
    p1 <- c(p1, if (fr) sprintf("%s : %s.", if (length(ny) > 1) sprintf("Les %d variables Y", length(ny)) else "La variable Y", liste(guillemets(ny)))
                else sprintf("%s: %s.", if (length(ny) > 1) sprintf("The %d Y variables", length(ny)) else "The Y variable", liste(guillemets(ny))))
  }

  # --- prétraitement
  # (une transformation identité, x^1, n'en est pas une)
  tr <- Filter(Negate(is.null), lapply(simples, function(t) {
    code <- nom_transformation(t$transform)
    if (!is.null(code)) c(nom = as.character(t$name), tr = code, formule = formule_transformation(t$transform, fr = fr))
  }))
  p2 <- character(0)
  if (length(tr)) {
    par_type <- split(vapply(tr, `[[`, "", "nom"), vapply(tr, `[[`, "", "tr"))
    formules <- stats::setNames(vapply(tr, `[[`, "", "formule"), vapply(tr, `[[`, "", "tr"))
    noms_tr <- if (fr) c(log10 = "en logarithme décimal", racine2 = "en racine carrée", racine4 = "en racine quatrième", autre = "par une transformation enregistrée dans le projet")
               else c(log10 = "log10-transformed", racine2 = "square-root transformed", racine4 = "fourth-root transformed", autre = "transformed as stored in the project")
    morceaux <- vapply(names(par_type), function(k) {
      quoi <- if (k %in% names(noms_tr)) noms_tr[[k]] else sprintf(if (fr) "par %s" else "transformed by %s", formules[[k]])
      sprintf("%s %s", liste(guillemets(par_type[[k]])), quoi)
    }, "")
    p2 <- c(p2, if (fr) sprintf("Transformations : %s.", paste(morceaux, collapse = " ; ")) else sprintf("Transformations: %s.", paste(morceaux, collapse = "; ")))
  }
  if (length(d$spectres)) {
    etape <- function(e) switch(e$type,
      snv = cite("snv", if (fr) "SNV (Barnes et al., 1989)" else "SNV (Barnes et al., 1989)"),
      msc = cite("msc", if (fr) "MSC sur le spectre moyen (Geladi et al., 1985)" else "MSC against the mean spectrum (Geladi et al., 1985)"),
      detrend = cite("snv", if (fr) sprintf("detrend (polynôme de degré %d retiré ; Barnes et al., 1989)", e$degre)
                            else sprintf("detrending (degree-%d polynomial removed; Barnes et al., 1989)", e$degre)),
      ligne_base = if (fr) "retrait de la ligne de base (enveloppe convexe inférieure du spectre)" else "baseline removal (lower convex hull of the spectrum)",
      sg = cite("sg", if (fr) sprintf("%s de Savitzky–Golay (fenêtre de %d points, polynôme de degré %d ; Savitzky et Golay, 1964)",
                                       c("lissage", "dérivée première", "dérivée seconde", "dérivée troisième")[e$derivee + 1] %||% sprintf("dérivée %d", e$derivee), e$fenetre, e$degre)
                      else sprintf("Savitzky–Golay %s (%d-point window, degree-%d polynomial; Savitzky & Golay, 1964)",
                                   c("smoothing", "first derivative", "second derivative", "third derivative")[e$derivee + 1] %||% sprintf("derivative %d", e$derivee), e$fenetre, e$degre)))
    p2 <- c(p2, if (fr) sprintf("Chaque spectre (les %d variables X quantitatives, dans l'ordre du tableau) a d'abord été corrigé par %s.",
                                sum(vapply(simples, function(t) categorie(t) == 0, TRUE)), liste(vapply(d$spectres, etape, "")))
                else sprintf("Each spectrum (the %d quantitative X variables, in table order) was first corrected by %s.",
                             sum(vapply(simples, function(t) categorie(t) == 0, TRUE)), liste(vapply(d$spectres, etape, ""))))
  }
  types <- table(factor(m$x$type, levels = c("UV", "Par", "Ctr", "none", "fixe")))
  types <- types[types > 0]
  if ("Par" %in% names(types)) cites <- union(cites, "pareto")
  noms_ech <- if (fr) c(UV = "centrées et réduites (variance unité)", Par = "centrées et mises à l'échelle de Pareto (racine de l'écart-type ; van den Berg et al., 2006)",
                        Ctr = "centrées seulement", none = "ni centrées ni mises à l'échelle", fixe = "centrées et mises à l'échelle avec les valeurs enregistrées dans le projet")
              else c(UV = "mean-centred and scaled to unit variance", Par = "mean-centred and Pareto-scaled (square root of the standard deviation; van den Berg et al., 2006)",
                     Ctr = "mean-centred only", none = "neither centred nor scaled", fixe = "centred and scaled with the values stored in the project")
  if (length(types) == 1) {
    p2 <- c(p2, if (fr) sprintf("Les variables X ont été %s.", noms_ech[[names(types)]]) else sprintf("The X variables were %s.", noms_ech[[names(types)]]))
  } else {
    p2 <- c(p2, if (fr) sprintf("Mise à l'échelle des termes X : %s.", paste(sprintf("%d %s", types, noms_ech[names(types)]), collapse = " ; "))
                else sprintf("Scaling of the X terms: %s.", paste(sprintf("%d %s", types, noms_ech[names(types)]), collapse = "; ")))
  }
  # pondération par blocs et modificateurs (pretraitement.R : facteurs_ponderation), pour les
  # termes dont le poids est calculé
  for (b in list(list(termes = d$x, bloc = m$x, nom = "X"), if (pls && !plsda) list(termes = d$y, bloc = m$y, nom = "Y"))) {
    if (is.null(b)) next
    calcule <- b$bloc$type %in% c("UV", "Par", "Ctr", "none")
    blocs <- vapply(b$termes, function(t) { x <- t$scaling$block; if (length(x) && !is.na(x[1]) && nzchar(as.character(x[1]))) as.character(x[1]) else NA_character_ }, "")
    K_tous <- table(blocs[!is.na(blocs)])  # (K compte tous les termes du bloc, comme facteurs_ponderation)
    blocs[!calcule] <- NA
    mods <- vapply(b$termes, function(t) as.numeric(t$scaling$modifier %||% 1), 0)
    mods[!calcule] <- 1
    if (any(!is.na(blocs))) {
      K <- K_tous[unique(blocs[!is.na(blocs)])]
      nom_bloc <- ifelse(grepl("^[0-9]+$", names(K)), paste(if (fr) "bloc" else "block", names(K)), guillemets(names(K)))
      p2 <- c(p2, if (fr) sprintf("Les termes %s étaient répartis en %d bloc%s (%s) : le poids de chaque terme d'un bloc a été divisé par la racine carrée du nombre de termes du bloc, pour que chaque bloc pèse autant, quel que soit son nombre de termes (pondération par blocs).",
                                  b$nom, length(K), if (length(K) > 1) "s" else "", paste(sprintf("%s : %d terme%s", nom_bloc, K, ifelse(K > 1, "s", "")), collapse = " ; "))
                  else sprintf("The %s terms were divided into %d block%s (%s): the weight of each term of a block was divided by the square root of the block's number of terms, so that each block weighs the same whatever its number of terms (block scaling).",
                               b$nom, length(K), if (length(K) > 1) "s" else "", paste(sprintf("%s: %d term%s", nom_bloc, K, ifelse(K > 1, "s", "")), collapse = "; ")))
    }
    if (any(mods != 1)) {
      n <- table(signif(mods[mods != 1], 4))
      facteur <- trimws(formatC(as.numeric(names(n)), digits = 4, format = "fg", decimal.mark = if (fr) "," else "."))
      p2 <- c(p2, if (fr) sprintf("Le poids %s a aussi été multiplié par un modificateur (%s).", if (sum(n) > 1) sprintf("de %d termes %s", sum(n), b$nom) else sprintf("d'un terme %s", b$nom),
                                  paste(sprintf("×%s%s", facteur, ifelse(length(n) > 1, sprintf(" pour %d", n), "")), collapse = " ; "))
                  else sprintf("The weight of %s was also multiplied by a modifier (%s).", if (sum(n) > 1) sprintf("%d %s terms", sum(n), b$nom) else sprintf("one %s term", b$nom),
                               paste(sprintf("×%s%s", facteur, ifelse(length(n) > 1, sprintf(" for %d", n), "")), collapse = "; ")))
    }
  }

  # --- méthode
  nom_methode <- if (fr) {
    if (opls && plsda) cite("opls", "une OPLS-DA (projections orthogonales sur des structures latentes, en discrimination ; Trygg et Wold, 2002)")
    else if (opls) cite("opls", "une OPLS (projections orthogonales sur des structures latentes ; Trygg et Wold, 2002)")
    else if (plsda) cite("plsda", "une PLS-DA (analyse discriminante par moindres carrés partiels ; Barker et Rayens, 2003)")
    else if (pls) cite("pls", "une régression PLS (moindres carrés partiels ; Wold et al., 2001)")
    else cite("acp", "une analyse en composantes principales (ACP ; Wold et al., 1987)")
  } else {
    if (opls && plsda) cite("opls", "an OPLS-DA (orthogonal projections to latent structures discriminant analysis; Trygg & Wold, 2002)")
    else if (opls) cite("opls", "an OPLS (orthogonal projections to latent structures; Trygg & Wold, 2002)")
    else if (plsda) cite("plsda", "a PLS-DA (partial least squares discriminant analysis; Barker & Rayens, 2003)")
    else if (pls) cite("pls", "a PLS regression (partial least squares; Wold et al., 2001)")
    else cite("acp", "a principal component analysis (PCA; Wold et al., 1987)")
  }
  composantes <- if (opls) {
    Ap <- m$Ap %||% 1
    Ao <- m$A - Ap
    if (fr) sprintf("%s composante%s prédictive%s et %s orthogonale%s", if (Ap == 1) "une" else Ap, if (Ap > 1) "s" else "", if (Ap > 1) "s" else "",
                    if (Ao == 1) "une" else Ao, if (Ao > 1) "s" else "")
    else sprintf("%s predictive and %s orthogonal component%s", if (Ap == 1) "one" else Ap, if (Ao == 1) "one" else Ao, if (Ao > 1) "s" else "")
  } else if (fr) sprintf("%d composante%s", m$A, if (m$A > 1) "s" else "") else sprintf("%d component%s", m$A, if (m$A > 1) "s" else "")
  manq <- if (manquantes_x > 0) {
    cite("nipals", if (fr) sprintf(", qui tolère les valeurs manquantes (%s des valeurs X ; Nelson et al., 1996)", pourcent(manquantes_x))
                   else sprintf(", which handles missing values (%s of the X values; Nelson et al., 1996)", pourcent(manquantes_x)))
  } else ""
  G <- m$G
  par <- if (length(d$groupes_vc$by)) as.character(d$groupes_vc$by) else ""
  groupes <- if (par %in% c("id", "variable")) {
    if (fr) sprintf("%d groupes, un par valeur de %s (les observations de même valeur dans le même groupe)", G, guillemets(m$vc_par))
    else sprintf("%d groups, one per value of %s (observations with the same value in the same group)", G, guillemets(m$vc_par))
  } else if (par == "groups") {
    if (fr) sprintf("%d groupes, repris du fichier d'origine", G) else sprintf("%d groups, taken from the original file", G)
  } else if (fr) sprintf("%d groupes (l'observation i dans le groupe i modulo %d)", G, G) else sprintf("%d groups (observation i in group i modulo %d)", G, G)
  p3 <- if (fr) sprintf("Nous avons ajusté %s par l'algorithme NIPALS%s, avec %s. La validation croisée comptait %s ; chaque groupe a été prédit par un modèle ajusté sans lui, avec un centrage et une mise à l'échelle recalculés sur les autres observations.",
                        nom_methode, manq, composantes, groupes)
        else sprintf("We fitted %s with the NIPALS algorithm%s, with %s. Cross-validation used %s; each group was predicted by a model fitted without it, with centring and scaling recomputed on the other observations.",
                     nom_methode, manq, composantes, groupes)

  # --- qualité
  cp <- s$composantes
  dernier <- cp[nrow(cp), ]
  p4 <- if (fr) sprintf("Le modèle explique %s de la variance de X (R²X cumulé)%s ; son pouvoir prédictif en validation croisée est Q² = %s.",
                        pourcent(dernier$R2Xcum), if (!is.null(dernier$R2Ycum)) sprintf(" et %s de celle de Y (R²Y cumulé)", pourcent(dernier$R2Ycum)) else "",
                        nombre(dernier$Q2cum))
        else sprintf("The model explains %s of the variance of X (cumulative R²X)%s; its cross-validated predictive ability is Q² = %s.",
                     pourcent(dernier$R2Xcum), if (!is.null(dernier$R2Ycum)) sprintf(" and %s of that of Y (cumulative R²Y)", pourcent(dernier$R2Ycum)) else "",
                     nombre(dernier$Q2cum))
  if (pls && length(s$cv_anova) && (length(s$cv_anova) == 1 || (plsda && length(s$cv_anova) == 2))) {
    p <- s$cv_anova[[1]]$p[2]
    if (!is.na(p)) p4 <- c(p4, cite("cv_anova", if (fr) sprintf("CV-ANOVA (Eriksson et al., 2008) : F = %s, %s.", nombre(s$cv_anova[[1]]$F[2]), valeur_p(p))
                                                else sprintf("CV-ANOVA (Eriksson et al., 2008): F = %s, %s.", nombre(s$cv_anova[[1]]$F[2]), valeur_p(p))))
  }
  if (plsda && !is.null(s$classification)) {
    p4 <- c(p4, if (fr) sprintf("En validation croisée, %s des observations sont attribuées à leur classe (la plus grande valeur de Y prédite).", pourcent(s$classification$vc$global))
                else sprintf("In cross-validation, %s of the observations are assigned to their own class (largest predicted Y value).", pourcent(s$classification$vc$global)))
  }
  # --- jeu de test (jeu_test.R)
  if (!is.null(m$test)) {
    tt <- m$test
    detail <- if (plsda && !is.null(tt$classement)) {
      if (fr) sprintf("%s de ses observations sont attribuées à leur classe", pourcent(tt$classement$bien_classees))
      else sprintf("%s of its observations are assigned to their own class", pourcent(tt$classement$bien_classees))
    } else if (!is.null(tt$statistiques)) {
      st <- tt$statistiques
      liste(vapply(seq_len(nrow(st)), function(j) sprintf("%sRMSEP = %s, %s = %s, Q²%s = %s", if (nrow(st) > 1) paste0(guillemets(st$y[j]), " : ") else "",
                                                         nombre(st$RMSEP[j], 3), if (fr) "biais" else "bias", nombre(st$biais[j], 3),
                                                         if (fr) " externe" else "ext", nombre(st$Q2ext[j])), ""))
    } else {
      if (fr) sprintf("%d hors du domaine du modèle (DModX ou T²)", tt$hors_domaine) else sprintf("%d outside the model's domain (DModX or T²)", tt$hors_domaine)
    }
    p4 <- c(p4, if (fr) sprintf("Un jeu de test de %d observations, laissées hors du modèle et de la validation croisée, a ensuite été prédit : %s.", tt$n, detail)
                else sprintf("A test set of %d observations, left out of the model and of the cross-validation, was then predicted: %s.", tt$n, detail))
  }

  # --- logiciel
  annee_r <- R.version$year
  version_r <- paste(R.version$major, R.version$minor, sep = ".")
  annee_nuee <- format(Sys.Date(), "%Y")
  cites <- union(cites, c("r", "nuee"))
  p5 <- if (fr) sprintf("Les calculs ont été faits avec Nuée%s (4rtefakt, %s), dont le moteur est écrit en R (R %s ; R Core Team, %s).",
                        if (nzchar(version)) paste0(" ", version) else "", annee_nuee, version_r, annee_r)
        else sprintf("Computations were done with Nuée%s (4rtefakt, %s), whose engine is written in R (R %s; R Core Team, %s).",
                     if (nzchar(version)) paste0(" ", version) else "", annee_nuee, version_r, annee_r)

  references <- vapply(cites, function(k) switch(k, r = sprintf(REFERENCES$r, annee_r),
                                                 nuee = sprintf(REFERENCES$nuee, annee_nuee, if (nzchar(version)) sprintf(" (version %s)", version) else ""),
                                                 REFERENCES[[k]]), "")
  list(paragraphes = c(paste(p1, collapse = " "), paste(p2, collapse = " "), paste(p3, collapse = " "), paste(p4, collapse = " "), p5),
       references = unname(sort(references)))
}
