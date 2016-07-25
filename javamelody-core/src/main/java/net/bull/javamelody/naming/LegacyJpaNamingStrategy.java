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

import javax.persistence.Query;

import net.bull.javamelody.JpaMethod;

/**
 * @deprecated Please try to migrate to {@link DefaultJpaNamingStrategy} asap.
 * The new strategy mimics the naming implemented by this class but is a cleaner implementation.
 */
@Deprecated
public class LegacyJpaNamingStrategy implements JpaNamingStrategy {

	@Override
	public String getRequestName(JpaMethod jpaMethod, Method javaMethod, Object[] args) {
		if (isCreateSomeQuery(jpaMethod, javaMethod, args)) {
			return getQueryRequestName(jpaMethod, javaMethod, args);
		}
		if (isOneOfFindMethods(jpaMethod, javaMethod, args)) {
			return getFindRequestName(jpaMethod, javaMethod, args);
		}
		if (isMergePersistRefreshRemoveDetachOrLockMethod(jpaMethod, javaMethod, args)) {
			return getMergePersistRefreshRemoveDetachOrLockRequestName(jpaMethod, javaMethod, args);
		}
		if (isFlushMethod(jpaMethod, javaMethod, args)) {
			return getFlushRequestName(jpaMethod, javaMethod, args);
		}
		return javaMethod.getName() + "(?" + args.length + "?)";
	}


	public boolean isCreateSomeQuery(JpaMethod jpaMethod, Method javaMethod, Object[] args) {
		final String methodName = javaMethod.getName();
		final Class<?> returnType = javaMethod.getReturnType();
		final boolean methodNameOk = "createQuery".equals(methodName)
						|| "createNamedQuery".equals(methodName)
						|| "createNativeQuery".equals(methodName)
						// JavaEE 7:
						|| "createStoredProcedureQuery".equals(methodName)
						|| "createNamedStoredProcedureQuery".equals(methodName);
		return methodNameOk && returnType != null && Query.class.isAssignableFrom(returnType);
	}

	public String getQueryRequestName(JpaMethod jpaMethod, Method javaMethod, Object[] args) {
		final StringBuilder requestName = new StringBuilder();
		String methodName = javaMethod.getName();
		requestName.append(methodName, "create".length(), methodName.length());
		appendArgs(requestName, args);
		return requestName.toString();
	}

	public boolean isOneOfFindMethods(JpaMethod jpaMethod, Method javaMethod, Object[] args) {
		return "find".equals(javaMethod.getName()) && args != null && args.length > 0
						&& args[0] instanceof Class;
	}

	public String getFindRequestName(JpaMethod jpaMethod, Method javaMethod, Object[] args) {
		return "find(" + ((Class<?>) args[0]).getSimpleName() + ')';
	}

	public boolean isMergePersistRefreshRemoveDetachOrLockMethod(JpaMethod jpaMethod, Method javaMethod, Object[] args) {
		String methodName = javaMethod.getName();
		return args != null && args.length > 0
						&& ("merge".equals(methodName) || "persist".equals(methodName)
						|| "refresh".equals(methodName) || "remove".equals(methodName)
						|| "detach".equals(methodName) || "lock".equals(methodName));
	}

	public String getMergePersistRefreshRemoveDetachOrLockRequestName(JpaMethod jpaMethod, Method javaMethod, Object[] args) {
		final String requestName = javaMethod.getName() + '(' + args[0].getClass().getSimpleName() + ')';
		return requestName;
	}

	public boolean isFlushMethod(JpaMethod jpaMethod, Method javaMethod, Object[] args) {
		return "flush".equals(javaMethod.getName());
	}

	public String getFlushRequestName(JpaMethod jpaMethod, Method javaMethod, Object[] args) {
		return "flush()";
	}

	private void appendArgs(StringBuilder requestName, Object[] args) {
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
