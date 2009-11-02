/*
 * Copyright 2008-2009 by Emeric Vernat, Bull
 *
 *     This file is part of Java Melody.
 *
 * Java Melody is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Java Melody is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Java Melody.  If not, see <http://www.gnu.org/licenses/>.
 */
package net.bull.javamelody;

import java.lang.reflect.Field;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.Collections;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NameClassPair;
import javax.naming.NamingException;
import javax.servlet.ServletContext;
import javax.sql.DataSource;

/**
 * Classe utilitaire pour JdbcWrapper.
 * @author Emeric Vernat
 */
final class JdbcWrapperHelper {
	private JdbcWrapperHelper() {
		super();
	}

	static Map<String, DataSource> getDataSources() throws NamingException {
		final Map<String, DataSource> dataSources = new HashMap<String, DataSource>(2);
		final InitialContext initialContext = new InitialContext();
		final String datasourcesParameter = Parameters.getParameter(Parameter.DATASOURCES);
		if (datasourcesParameter == null) {
			for (final NameClassPair nameClassPair : Collections.list(initialContext
					.list("java:comp/env/jdbc"))) {
				// note: il ne suffit pas de tester
				// (DataSource.class.isAssignableFrom(Class.forName(nameClassPair.getClassName())))
				// car nameClassPair.getClassName() vaut "javax.naming.LinkRef" sous jboss 5.1.0.GA
				// par exemple, donc on fait le lookup pour voir
				final String jndiName;
				if (nameClassPair.getName().startsWith("java:")) {
					// pour glassfish v3
					jndiName = nameClassPair.getName();
				} else {
					jndiName = "java:comp/env/jdbc/" + nameClassPair.getName();
				}
				final Object value = initialContext.lookup(jndiName);
				if (value instanceof DataSource) {
					dataSources.put(jndiName, (DataSource) value);
				}
			}
		} else if (!datasourcesParameter.trim().isEmpty()) {
			for (final String datasource : datasourcesParameter.split(",")) {
				final String jndiName = datasource.trim();
				// ici, on n'ajoute pas java:/comp/env
				// et on suppose qu'il n'en faut pas ou que cela a été ajouté dans le paramétrage
				final DataSource dataSource = (DataSource) initialContext.lookup(jndiName);
				dataSources.put(jndiName, dataSource);
			}
		}
		return Collections.unmodifiableMap(dataSources);
	}

	@SuppressWarnings("all")
	// CHECKSTYLE:OFF
	static Object changeContextWritable(ServletContext servletContext, Object securityToken)
			throws NoSuchFieldException, ClassNotFoundException, IllegalAccessException,
			NamingException {
		// cette méthode ne peut pas être utilisée avec un simple JdbcDriver
		assert servletContext != null;
		if (servletContext.getServerInfo().contains("Tomcat")
				&& System.getProperty("jonas.name") == null) {
			// on n'exécute cela que si c'est tomcat
			// et si ce n'est pas tomcat dans jonas
			final Field field = Class.forName("org.apache.naming.ContextAccessController")
					.getDeclaredField("readOnlyContexts");
			setFieldAccessible(field);
			@SuppressWarnings("unchecked")
			final Hashtable<String, Object> readOnlyContexts = (Hashtable<String, Object>) field
					.get(null);
			// contextPath vaut /myapp par exemple
			final String contextName = "/Catalina/localhost"
					+ Parameters.getContextPath(servletContext);
			if (securityToken == null) {
				// on rend le contexte writable
				return readOnlyContexts.remove(contextName);
			}
			// on remet le contexte not writable comme avant
			readOnlyContexts.put(contextName, securityToken);

			return null;
		} else if (servletContext.getServerInfo().contains("jetty")) {
			// on n'exécute cela que si c'est jetty
			final Context jdbcContext = (Context) new InitialContext().lookup("java:comp");
			final Field field = getAccessibleField(jdbcContext, "_env");
			@SuppressWarnings("unchecked")
			final Hashtable<Object, Object> env = (Hashtable<Object, Object>) field
					.get(jdbcContext);
			if (securityToken == null) {
				// on rend le contexte writable
				return env.remove("org.mortbay.jndi.lock");
			}
			// on remet le contexte not writable comme avant
			env.put("org.mortbay.jndi.lock", securityToken);

			return null;
		}
		return null;
	}

	static Field getAccessibleField(Object object, String fieldName) {
		assert fieldName != null;
		Class<?> classe = object.getClass();
		Field result = null;
		do {
			for (final Field field : classe.getDeclaredFields()) {
				if (fieldName.equals(field.getName())) {
					result = field;
					break;
				}
			}
			classe = classe.getSuperclass();
		} while (result == null && classe != null);

		assert result != null;
		setFieldAccessible(result);
		return result;
	}

	private static void setFieldAccessible(final Field field) {
		AccessController.doPrivileged(new PrivilegedAction<Object>() { // pour findbugs
					/** {@inheritDoc} */
					public Object run() {
						field.setAccessible(true);
						return null;
					}
				});
	}
	// CHECKSTYLE:ON
}
