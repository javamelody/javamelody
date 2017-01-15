/****************************************************************/
/* Compte et droits pour client GWT                             */
/****************************************************************/
insert into AW_UTILISATEUR(UTILISATEUR_ID, IDENTIFIANT, MDP, NOM, PRENOM, EMAIL, MDPH, XDMAJ, XTOPSUP) 
	values (100, 'test', 'test', 'Test', '', 'a@test.fr', '098f6bcd4621d373cade4e832627b4f6', current_date, false);

insert into AW_PERMISSION (PERMISSION_ID, NOMPERMISSION, XDMAJ, XTOPSUP) 
	values (100, 'superAdmin', current_date, false);
insert into AW_PERMISSION (PERMISSION_ID, NOMPERMISSION, XDMAJ, XTOPSUP) 
	values (101, 'creerPersonne', current_date, false);
insert into AW_PERMISSION (PERMISSION_ID, NOMPERMISSION, XDMAJ, XTOPSUP) 
	values (102, 'modifierPersonne', current_date, false);
insert into AW_PERMISSION (PERMISSION_ID, NOMPERMISSION, XDMAJ, XTOPSUP) 
	values (103, 'supprimerPersonne', current_date, false);
insert into AW_PERMISSION (PERMISSION_ID, NOMPERMISSION, XDMAJ, XTOPSUP) 
	values (104, 'listerPersonnes', current_date, false);
insert into AW_PERMISSION (PERMISSION_ID, NOMPERMISSION, XDMAJ, XTOPSUP) 
	values (105, 'accesAnnuaire', current_date, false);

insert into AW_ROLE (ROLE_ID, NOM_ROLE, XDMAJ, XTOPSUP) 
	values (100, 'Testeur', current_date, false);

insert into AW_ROLE_UTILISATEUR (UTILISATEUR_ID, ROLE_ID) 
	values (100, 100);

insert into AW_ROLEPERMISSION (ROLE_ID, PERMISSION_ID) 
	values (100, 101);
insert into AW_ROLEPERMISSION (ROLE_ID, PERMISSION_ID) 
	values (100, 102);
insert into AW_ROLEPERMISSION (ROLE_ID, PERMISSION_ID) 
	values (100, 103);
insert into AW_ROLEPERMISSION (ROLE_ID, PERMISSION_ID) 
	values (100, 104);
insert into AW_ROLEPERMISSION (ROLE_ID, PERMISSION_ID) 
	values (100, 105);

/****************************************************************/
/* Personne (pour client léger)                                 */
/****************************************************************/
insert into AW_PERSONNE(PERSONNE_ID, NOM, PRENOM, CIVIL, DATE_NAISSANCE, SALAIRE, VERSION, GRADE_ID, XDMAJ, XTOPSUP) 
	values (1, 'D', 'B', true, null, null, current_date, null, current_date, false);


insert into AW_COMPETENCE_PERSO (COMPETENCE_PERSO_ID, LIBELLE, XDMAJ, XTOPSUP) 
values (1, 'Bricolage', current_date, false);
