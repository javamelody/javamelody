insert into AW_GRADE(GRADE_ID, LIBELLE, TRIGRAMME, XDMAJ, XTOPSUP) 
	values (nextval('AW_GRADE_SEQ'), 'Colonel', 'COL', current_date, false);

insert into AW_GRADE(GRADE_ID, LIBELLE, TRIGRAMME, XDMAJ, XTOPSUP) 
	values (nextval('AW_GRADE_SEQ'), 'Lieutenant', 'LTN', current_date, false);

insert into AW_GRADE(GRADE_ID, LIBELLE, TRIGRAMME, XDMAJ, XTOPSUP) 
	values (nextval('AW_GRADE_SEQ'), 'Sergent', 'SGT', current_date, false);
