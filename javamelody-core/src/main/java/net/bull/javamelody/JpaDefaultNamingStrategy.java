/*
 * Copyright © 2016 DV Bern AG, Switzerland
 *
 * Das vorliegende Dokument, einschliesslich aller seiner Teile, ist urheberrechtlich
 * geschützt. Jede Verwertung ist ohne Zustimmung der DV Bern AG unzulässig. Dies gilt
 * insbesondere für Vervielfältigungen, die Einspeicherung und Verarbeitung in
 * elektronischer Form. Wird das Dokument einem Kunden im Rahmen der Projektarbeit zur
 * Ansicht übergeben, ist jede weitere Verteilung durch den Kunden an Dritte untersagt.
 */

package net.bull.javamelody;
import java.lang.reflect.Method;

/**
 * Default naming strategy.
 */
public class JpaDefaultNamingStrategy implements JpaNamingStrategy {
	@Override
	public String getRequestName(JpaMethod jpaMethod, Method javaMethod, Object[] args) {
		switch (jpaMethod) {
		case CREATE_QUERY:
		case CREATE_NAMED_QUERY:
		case CREATE_NATIVE_QUERY:
		case CREATE_STORED_PROCEDURE_QUERY:
		case CREATE_NAMED_STORED_PROCEDURE_QUERY:
			return getQueryRequestName(jpaMethod, javaMethod, args);
		case FIND:
			return getMethodWithClassArgRequestName(jpaMethod, javaMethod, args);
		case MERGE:
		case PERSIST:
		case REFRESH:
		case REMOVE:
		case DETACH:
		case LOCK:
			return getMethodWithEntityArgRequestName(jpaMethod, javaMethod, args);
		case FLUSH:
			return getNoArgsRequestName(jpaMethod, javaMethod);
		case OTHER:
		default:
			return getOtherRequestName(jpaMethod, javaMethod, args);
		}
	}

	protected String getOtherRequestName(JpaMethod jpaMethod, Method javaMethod, Object[] args) {
		int argsLen = args == null ? 0 : args.length;
		return "other: " + javaMethod.getName() + "(?" + argsLen + "?)";
	}

	protected String getQueryRequestName(JpaMethod jpaMethod, Method javaMethod, Object[] args) {
		final StringBuilder requestName = new StringBuilder();
		String methodName = javaMethod.getName();
		requestName.append(methodName, "create".length(), methodName.length());
		appendArgs(requestName, args);
		return requestName.toString();
	}

	protected String getMethodWithClassArgRequestName(JpaMethod jpaMethod, Method javaMethod, Object[] args) {
		final String requestName = javaMethod.getName() + '(' + ((Class<?>)args[0]).getSimpleName() + ')';
		return requestName;
	}

	protected String getMethodWithEntityArgRequestName(JpaMethod jpaMethod, Method javaMethod, Object[] args) {
		final String requestName = javaMethod.getName() + '(' + args[0].getClass().getSimpleName() + ')';
		return requestName;
	}

	protected String getNoArgsRequestName(JpaMethod jpaMethod, Method javaMethod) {
		return javaMethod.getName() + "()";
	}

	protected void appendArgs(StringBuilder requestName, Object[] args) {
		requestName.append('(');
		if (args != null) {
			String separator = "";
			for (final Object arg : args) {
				requestName.append(separator);
				separator = ", ";
				if (arg instanceof Class) {
					requestName.append(((Class<?>) arg).getSimpleName());
				} else {
					requestName.append(arg);
				}
			}
		}
		requestName.append(')');
	}

}
