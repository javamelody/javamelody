/****************************************************************/
/* Base de donnees:      H2                                     */
/* Application:          referentiel1                           */
/* Date de creation:     03/11/2014 16:49:15                    */
/****************************************************************/

/****************************************************************/
/* Sequences                                                    */
/****************************************************************/
create sequence AW_GRADE_SEQ start with 1000;


/****************************************************************/
/* Table: AW_GRADE                                              */
/****************************************************************/
create table AW_GRADE
(
    GRADE_ID NUMBER(19) not null,
    LIBELLE VARCHAR(100) not null,
    TRIGRAMME VARCHAR(3) not null,
    XDMAJ TIMESTAMP not null,
    XTOPSUP NUMBER(1) not null,
    constraint AW_GRADE_PK primary key (GRADE_ID)
);

comment on table AW_GRADE is 'un grade (référence)';
comment on column AW_GRADE.LIBELLE is 'le libellé du grade';
comment on column AW_GRADE.TRIGRAMME is 'le trigramme du grade';




/****************************************************************/
/* Constraints                                                  */
/****************************************************************/

