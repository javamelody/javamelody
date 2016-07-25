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
import java.lang.reflect.Method;

import javax.persistence.criteria.CriteriaDelete;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.CriteriaUpdate;

import net.bull.javamelody.JpaMethod;

/**
 * Hibernate (and maybe others) does not provide a usable toString() implementation for
 * {@link CriteriaQuery}, {@link CriteriaDelete} and {@link CriteriaUpdate}.
 *
 * The <b>bad!</b> workaround provided in this naming strategy is to look into the current stacktrace
 * and search for the first non-proxy class and use ist stacktrace entry.
 * For all non-criteria cases, the name computed by {@link DefaultJpaNamingStrategy} is used.

 * @deprecated Will be removed once hibernate (the sole(?) reason for this workaround) will implement a usable
 * implementation of CriteriaQuery.toString()
 */
@Deprecated
public class CriteriaWorkaroundLineNumbersJpa20NamingStrategy extends DefaultJpaNamingStrategy {

	@Override
	public String getQueryRequestName(JpaMethod jpaMethod, Method javaMethod, Object[] args) {
		// Handle CriteriaQuery stuff differently: else we would get
		// a new generic name (similar to Object.toString()) for every instance of the query
		if ("createQuery".equals(javaMethod.getName()) && args.length >= 1 && isCriteriaArg(args[0])) {
			return getCriteriaQueryRequestName(args[0].getClass());
		}
		return super.getQueryRequestName(jpaMethod, javaMethod, args);
	}

	protected boolean isCriteriaArg(Object arg) {
		return arg instanceof CriteriaQuery;
	}

	protected String getCriteriaCreateQueryArgName(Class<?> criteriaQueryClass) {
		if (CriteriaQuery.class.isAssignableFrom(criteriaQueryClass)) {
			return CriteriaQuery.class.getSimpleName();
		}
		return criteriaQueryClass.getSimpleName();
	}

	/**
	 * Analyze the stacktrace of the current thread and (hopefully) find the fist
	 * entry that is not from JavaMelody itself and is not from other proxies
	 */
	protected String getCriteriaQueryRequestName(Class<?> criteriaQueryClass) {
		String methodName = getCriteriaCreateQueryArgName(criteriaQueryClass);

		StackTraceElement[] stack = Thread.currentThread().getStackTrace();
		for (StackTraceElement element : stack) {
			String stackEntryclassName = element.getClassName();
			if (stackEntryclassName.startsWith(Thread.class.getCanonicalName())
							|| stackEntryclassName.startsWith("net.bull.javamelody")
							// TODO: discover proxy methods in JDKs of other vendors
							// or at least implement a way to let the user provide a method to skip entries
							|| stackEntryclassName.startsWith("com.sun.proxy.$Proxy")) {
				continue;
			}
			//TODO: element.toString() contains line number => a monitor name for a specific query might change
			// although the query itself did not change.
			// Problem: I cannot think of a better way to uniquely identify a query
			return methodName + '(' + element.toString() + ')';
		}
		return methodName + "(<unknown location>)";
	}


}
