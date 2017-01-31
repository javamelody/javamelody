/****************************************************************/
/* Base de donnees:      H2                                     */
/* Application:          appli1                                 */
/* Date de creation:     03/11/2014 16:49:31                    */
/****************************************************************/

/****************************************************************/
/* Sequences                                                    */
/****************************************************************/
create sequence AW_PERSONNE_SEQ start with 1000;
create sequence AW_ADRESSE_SEQ start with 1000;
create sequence AW_COMPETENCE_SEQ start with 1000;
create sequence AW_COMPETENCE_PERSO_SEQ start with 1000;
create sequence AW_PERSONNE_N_COMPETENCE_P_SEQ start with 1000;
create sequence AW_TELEPHONE_SEQ start with 1000;
create sequence AW_MAIL_SEQ start with 1000;

create sequence AW_UTILISATEUR_SEQ start with 1000;
create sequence AW_ROLE_SEQ start with 1000;
create sequence AW_PERMISSION_SEQ start with 1000;


/****************************************************************/
/* Table: AW_PERSONNE                                           */
/****************************************************************/
create table AW_PERSONNE
(
    PERSONNE_ID NUMBER(19) not null,
    NOM VARCHAR(100) not null,
    PRENOM VARCHAR(100),
    CIVIL NUMBER(1) not null,
    DATE_NAISSANCE TIMESTAMP,
    SALAIRE NUMBER(14,2),
    VERSION TIMESTAMP not null,
    GRADE_ID NUMBER(19),
    XDMAJ TIMESTAMP,
    XTOPSUP NUMBER(1) not null,
    constraint AW_PERSONNE_PK primary key (PERSONNE_ID)
);

create index AW_PERSONNE_GRADE_IDX on AW_PERSONNE (GRADE_ID);


comment on table AW_PERSONNE is 'Une personne';
comment on column AW_PERSONNE.NOM is 'le nom de la personne';
comment on column AW_PERSONNE.PRENOM is 'le prénom de la personne';
comment on column AW_PERSONNE.CIVIL is 'indicateur si la personne est civile ou non';
comment on column AW_PERSONNE.DATE_NAISSANCE is 'la date de naissance de la personne';

/****************************************************************/
/* Table: AW_COMPETENCE_ID                                      */
/****************************************************************/
create table AW_COMPETENCE_ID
(
    PERSONNE_ID NUMBER(19) not null,
    COMPETENCE_ID NUMBER(19) not null,
    constraint AW_COMPETENCE_ID_PK primary key (PERSONNE_ID, COMPETENCE_ID)
);

/****************************************************************/
/* Table: AW_ADRESSE                                            */
/****************************************************************/
create table AW_ADRESSE
(
    adresse_id NUMBER(19) not null,
    RUE VARCHAR(100) not null,
    CODEPOSTAL NUMBER(5) not null,
    VILLE VARCHAR(100) not null,
    PERSONNE_ID NUMBER(19) not null,
    XDMAJ TIMESTAMP,
    XTOPSUP NUMBER(1) not null,
    constraint AW_ADRESSE_PK primary key (adresse_id)
);

create index AW_ADRESSE_PERSONNE_ID_IDX on AW_ADRESSE (PERSONNE_ID);


comment on table AW_ADRESSE is 'une adresse de personne';
comment on column AW_ADRESSE.RUE is 'la rue de l''adresse';
comment on column AW_ADRESSE.VILLE is 'la ville de l''adresse';


/****************************************************************/
/* Table: AW_COMPETENCE                                         */
/****************************************************************/
create table AW_COMPETENCE
(
    COMPETENCE_ID NUMBER(19) not null,
    LIBELLE VARCHAR(100) not null,
    XDMAJ TIMESTAMP,
    XTOPSUP NUMBER(1) not null,
    constraint AW_COMPETENCE_PK primary key (COMPETENCE_ID)
);

comment on table AW_COMPETENCE is 'une compétence';
comment on column AW_COMPETENCE.LIBELLE is 'le libellé de la compétence';


/****************************************************************/
/* Table: AW_COMPETENCE_PERSO                                   */
/****************************************************************/
create table AW_COMPETENCE_PERSO
(
    COMPETENCE_PERSO_ID NUMBER(19) not null,
    LIBELLE VARCHAR(100),
    XDMAJ TIMESTAMP,
    XTOPSUP NUMBER(1) not null,
    constraint AW_COMPETENCE_PERSO_PK primary key (COMPETENCE_PERSO_ID)
);




/****************************************************************/
/* Table: AW_PERSONNE_N_COMPETENCE_PERSO                        */
/****************************************************************/
create table AW_PERSONNE_N_COMPETENCE_PERSO
(
    PERSONNE_N_COMPETENCE_PERSO_ID NUMBER(19) not null,
    COMPETENCEPERSO_ID NUMBER(19) not null,
    PERSONNE_ID NUMBER(19) not null,
    XDMAJ TIMESTAMP,
    XTOPSUP NUMBER(1) not null,
    constraint AW_PERSONNE_N_COMPETENCE_PE_PK primary key (PERSONNE_N_COMPETENCE_PERSO_ID)
);

create index AW_PERSONNE_N_COMPETENCEPE_IDX on AW_PERSONNE_N_COMPETENCE_PERSO (COMPETENCEPERSO_ID);
create index AW_PERSONNE_N_PERSONNE_IDX on AW_PERSONNE_N_COMPETENCE_PERSO (PERSONNE_ID);





/****************************************************************/
/* Table: AW_TELEPHONE                                          */
/****************************************************************/
create table AW_TELEPHONE
(
    TELEPHONE_ID NUMBER(19) not null,
    PRO NUMBER(1) not null,
    NUMERO VARCHAR(100) not null,
    PERSONNE_ID NUMBER(19) not null,
    XDMAJ TIMESTAMP,
    XTOPSUP NUMBER(1) not null,
    constraint AW_TELEPHONE_PK primary key (TELEPHONE_ID)
);

create index AW_TELEPHONE_PERSONNE_IDX on AW_TELEPHONE (PERSONNE_ID);


comment on table AW_TELEPHONE is 'Contact téléphonique';


/****************************************************************/
/* Table: AW_MAIL                                               */
/****************************************************************/
create table AW_MAIL
(
    MAIL_ID NUMBER(19) not null,
    PRO NUMBER(1) not null,
    MAIL VARCHAR(100) not null,
    PERSONNE_ID NUMBER(19) not null,
    XDMAJ TIMESTAMP,
    XTOPSUP NUMBER(1) not null,
    constraint AW_MAIL_PK primary key (MAIL_ID)
);

create index AW_MAIL_PERSONNE_IDX on AW_MAIL (PERSONNE_ID);






/****************************************************************/
/* Table: AW_UTILISATEUR                                        */
/****************************************************************/
create table AW_UTILISATEUR
(
    UTILISATEUR_ID NUMBER(19) not null,
    IDENTIFIANT VARCHAR(100) not null,
    MDP VARCHAR(100) not null,
    NOM VARCHAR(100) not null,
    PRENOM VARCHAR(100) not null,
    EMAIL VARCHAR(100) not null,
    MDPH VARCHAR(100) not null,
    XDMAJ TIMESTAMP,
    XTOPSUP NUMBER(1) not null,
    constraint AW_UTILISATEUR_PK primary key (UTILISATEUR_ID)
);


comment on column AW_UTILISATEUR.IDENTIFIANT is 'l''identifiant de connexion de l''utilisateur';
comment on column AW_UTILISATEUR.MDP is 'le mot de passe de connexion de l''utilisateur';
comment on column AW_UTILISATEUR.NOM is 'le nom de l''utilisateur';
comment on column AW_UTILISATEUR.PRENOM is 'le prénom de l''utilisateur';
comment on column AW_UTILISATEUR.EMAIL is 'l''adresse email de l''utilisateur';

/****************************************************************/
/* Table: AW_ROLE_UTILISATEUR                                   */
/****************************************************************/
create table AW_ROLE_UTILISATEUR
(
    UTILISATEUR_ID NUMBER(19) not null,
    ROLE_ID NUMBER(19) not null,
    constraint AW_ROLE_UTILISATEUR_PK primary key (UTILISATEUR_ID, ROLE_ID)
);

/****************************************************************/
/* Table: AW_ROLE                                               */
/****************************************************************/
create table AW_ROLE
(
    ROLE_ID NUMBER(19) not null,
    NOM_ROLE VARCHAR(100) not null,
    XDMAJ TIMESTAMP,
    XTOPSUP NUMBER(1) not null,
    constraint AW_ROLE_PK primary key (ROLE_ID)
);


comment on column AW_ROLE.NOM_ROLE is 'le nom du rôle';

/****************************************************************/
/* Table: AW_ROLEPERMISSION                                     */
/****************************************************************/
create table AW_ROLEPERMISSION
(
    ROLE_ID NUMBER(19) not null,
    PERMISSION_ID NUMBER(19) not null,
    constraint AW_ROLEPERMISSION_PK primary key (ROLE_ID, PERMISSION_ID)
);

/****************************************************************/
/* Table: AW_PERMISSION                                         */
/****************************************************************/
create table AW_PERMISSION
(
    PERMISSION_ID NUMBER(19) not null,
    NomPERMISSION VARCHAR(100) not null,
    XDMAJ TIMESTAMP,
    XTOPSUP NUMBER(1) not null,
    constraint AW_PERMISSION_PK primary key (PERMISSION_ID)
);


comment on column AW_PERMISSION.NomPERMISSION is 'le nom de la permission';




/****************************************************************/
/* Constraints                                                  */
/****************************************************************/
alter table AW_PERSONNE add constraint PERSONNE_GRADE_FK foreign key (GRADE_ID) references AW_GRADE (GRADE_ID);
alter table AW_COMPETENCE_ID add constraint COMPETENCE_ID_PERSONNE_FK foreign key (PERSONNE_ID) references AW_PERSONNE (PERSONNE_ID);
alter table AW_COMPETENCE_ID add constraint COMPETENCE_ID_COMPETENCE_FK foreign key (COMPETENCE_ID) references AW_COMPETENCE (COMPETENCE_ID);
alter table AW_ADRESSE add constraint ADRESSE_PERSONNE_ID_FK foreign key (PERSONNE_ID) references AW_PERSONNE (PERSONNE_ID);
alter table AW_PERSONNE_N_COMPETENCE_PERSO add constraint PERSONNE_N_CO_COMPETENCEPER_FK foreign key (COMPETENCEPERSO_ID) references AW_COMPETENCE_PERSO (COMPETENCE_PERSO_ID);
alter table AW_PERSONNE_N_COMPETENCE_PERSO add constraint PERSONNE_N_CO_PERSONNE_FK foreign key (PERSONNE_ID) references AW_PERSONNE (PERSONNE_ID);
alter table AW_TELEPHONE add constraint TELEPHONE_PERSONNE_FK foreign key (PERSONNE_ID) references AW_PERSONNE (PERSONNE_ID);
alter table AW_MAIL add constraint MAIL_PERSONNE_FK foreign key (PERSONNE_ID) references AW_PERSONNE (PERSONNE_ID);
alter table AW_ROLE_UTILISATEUR add constraint ROLE_UTILISAT_UTILISATEUR_FK foreign key (UTILISATEUR_ID) references AW_UTILISATEUR (UTILISATEUR_ID);
alter table AW_ROLE_UTILISATEUR add constraint ROLE_UTILISAT_ROLE_FK foreign key (ROLE_ID) references AW_ROLE (ROLE_ID);
alter table AW_ROLEPERMISSION add constraint ROLEPERMISSIO_ROLE_FK foreign key (ROLE_ID) references AW_ROLE (ROLE_ID);
alter table AW_ROLEPERMISSION add constraint ROLEPERMISSIO_PERMISSION_FK foreign key (PERMISSION_ID) references AW_PERMISSION (PERMISSION_ID);

