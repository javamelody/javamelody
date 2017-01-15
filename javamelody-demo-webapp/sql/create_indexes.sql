-- Script ecrit à la main pour la creation d'indexes specifiques (indexes uniques et indexes pour les performances des recherches)

-- index unique pour avoir une contrainte d'unicité et pour recherches par trigramme
create unique index AW_GRADE_TRIGRAMME_IDX on AW_GRADE (TRIGRAMME);

-- index pour recherches par nom
create index AW_PERSONNE_NOM_IDX on AW_PERSONNE (NOM);

