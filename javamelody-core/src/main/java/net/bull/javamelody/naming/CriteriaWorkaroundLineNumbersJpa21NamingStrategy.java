/*
 * Copyright © 2016 DV Bern AG, Switzerland
 *
 * Das vorliegende Dokument, einschliesslich aller seiner Teile, ist urheberrechtlich
 * geschützt. Jede Verwertung ist ohne Zustimmung der DV Bern AG unzulässig. Dies gilt
 * insbesondere für Vervielfältigungen, die Einspeicherung und Verarbeitung in
 * elektronischer Form. Wird das Dokument einem Kunden im Rahmen der Projektarbeit zur
 * Ansicht übergeben, ist jede weitere Verteilung durch den Kunden an Dritte untersagt.
 */

package net.bull.javamelody.naming;
import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaUpdate;

/**
 * JPA2.1 brought {@link CriteriaUpdate} and {@link CriteriaDelete}.
 *
 * @deprecated reason: see {@link CriteriaWorkaroundLineNumbersJpa20NamingStrategy}.
 */
@Deprecated
public class CriteriaWorkaroundLineNumbersJpa21NamingStrategy extends CriteriaWorkaroundLineNumbersJpa20NamingStrategy {

	@Override
	protected boolean isCriteriaArg(Object arg) {
		return super.isCriteriaArg(arg) || arg instanceof CriteriaUpdate || arg instanceof CriteriaDelete;
	}

	@Override
	protected String getCriteriaCreateQueryArgName(Class<?> criteriaQueryClass) {
		if (CriteriaUpdate.class.isAssignableFrom(criteriaQueryClass)) {
			return CriteriaUpdate.class.getSimpleName();
		}
		if (CriteriaDelete.class.isAssignableFrom(criteriaQueryClass)) {
			return CriteriaDelete.class.getSimpleName();
		}
		return super.getCriteriaCreateQueryArgName(criteriaQueryClass);
	}
}
