/*
 * Copyright 2008-2012 by Emeric Vernat
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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.naming.Binding;
import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.Reference;

/**
 * Informations sur un binding JNDI.
 * @author Emeric Vernat
 */
class JndiBinding implements Serializable {
	private static final long serialVersionUID = 1L;

	private static final String JNDI_PREFIX = "java:";

	private final String name;
	private final String className;
	private final String contextPath;
	private final String value;

	JndiBinding(String name, String className, String contextPath, String value) {
		super();
		this.name = name;
		this.className = className;
		this.contextPath = contextPath;
		this.value = value;
	}

	String getValue() {
		return value;
	}

	String getName() {
		return name;
	}

	String getClassName() {
		return className;
	}

	String getContextPath() {
		return contextPath;
	}

	static String normalizePath(String path) {
		if (path != null) {
			return path;
		} else if (Parameters.getServletContext().getServerInfo().contains("GlassFish")) {
			// dans glassfish 3.0.1, context.listBindings("java:") ne retourne aucun binding à part
			// lui-même, donc par défaut dans glassfish on prend le path "comp" et non ""
			return "comp";
		}
		return "";
	}

	static List<JndiBinding> listBindings(String path) throws NamingException {
		return listBindings(new InitialContext(), path);
	}

	static List<JndiBinding> listBindings(Context context, String path) throws NamingException {
		final String normalizedPath = normalizePath(path);
		final String jndiName;
		if (Parameters.getServletContext().getServerInfo().contains("WebLogic")) {
			// path + '/' nécessaire pour WebLogic 10.3.1.0 mais pas supporté dans JBoss
			jndiName = JNDI_PREFIX + normalizedPath + '/';
		} else {
			jndiName = JNDI_PREFIX + normalizedPath;
		}
		final List<JndiBinding> result = new ArrayList<JndiBinding>();
		final NamingEnumeration<Binding> enumeration = context.listBindings(jndiName);
		try {
			while (enumeration.hasMore()) {
				try {
					final Binding binding = enumeration.next();
					final JndiBinding jndiBinding = createJndiBinding(normalizedPath, binding);
					// si on veux corriger http://java.net/jira/browse/GLASSFISH-12831
					// sous glassfish 3.0.1 et non 3.1, les bindings d'un contexte contienne le contexte lui-même
					//		if (jndiBinding.getName().length() == 0) {
					//			return;
					//		}
					result.add(jndiBinding);
				} catch (final Exception e) {
					// catch Exception et non catch NamingException car glassfish 3.1 par exemple
					// lance parfois des RuntimeException encapsulant des NamingException lors du next()
					continue;
				}
			}
		} finally {
			// Comme indiqué dans la javadoc enumeration.close() n'est pas nécessaire après que hasMore()
			// a retourné false. De plus, cela provoquerait une exception dans glassfish 3.0.1
			// "javax.naming.OperationNotSupportedException: close() not implemented"
			// enumeration.close();

			context.close();
		}
		return result;
	}

	private static JndiBinding createJndiBinding(String path, Binding binding) {
		final String name = getBindingName(path, binding);
		final String className = binding.getClassName();
		final Object object = binding.getObject();
		final String contextPath;
		String value;
		if (object instanceof Context
				// "javax.naming.Context".equals(className) nécessaire pour le path "comp" dans JBoss 6.0
				|| "javax.naming.Context".equals(className)
				// pour jetty :
				|| object instanceof Reference
				&& "javax.naming.Context".equals(((Reference) object).getClassName())) {
			if (path.length() > 0) {
				contextPath = path + '/' + name;
			} else {
				// nécessaire pour jonas 5.1.0
				contextPath = name;
			}
			value = null;
		} else {
			contextPath = null;
			value = formatValue(object);
		}
		return new JndiBinding(name, className, contextPath, value);
	}

	private static String formatValue(Object object) {
		String value;
		try {
			if (object instanceof Collection) {
				final StringBuilder sb = new StringBuilder();
				sb.append('[');
				boolean first = true;
				for (final Object aItem : (Collection<?>) object) {
					if (first) {
						first = false;
					} else {
						sb.append(",\n");
					}
					sb.append(String.valueOf(aItem));
				}
				sb.append(']');
				value = sb.toString();
			} else {
				value = String.valueOf(object);
			}
		} catch (final Exception e) {
			value = e.toString();
		}
		return value;
	}

	private static String getBindingName(String path, Binding binding) {
		// nécessaire pour glassfish 3.0.1
		String result = binding.getName();
		if (result.startsWith(JNDI_PREFIX)) {
			result = result.substring(JNDI_PREFIX.length());
		}
		if (result.startsWith(path)) {
			result = result.substring(path.length());
		}
		if (result.length() > 0 && result.charAt(0) == '/') {
			result = result.substring(1);
		}
		return result;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[name=" + getName() + ", className=" + getClassName()
				+ ", contextPath=" + getContextPath() + ']';
	}
}
