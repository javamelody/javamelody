/*
 * Copyright 2008-2019 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.bull.javamelody.internal.model;

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

import net.bull.javamelody.internal.common.Parameters;

/**
 * Informations sur un binding JNDI.
 * @author Emeric Vernat
 */
public class JndiBinding implements Serializable {
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

	public String getValue() {
		return value;
	}

	public String getName() {
		return name;
	}

	public String getClassName() {
		return className;
	}

	public String getContextPath() {
		return contextPath;
	}

	public static String normalizePath(String path) {
		if (path != null) {
			return path;
		} else if (Parameters.getServletContext().getServerInfo().contains("GlassFish")) {
			// dans glassfish 3.0.1, context.listBindings("java:") ne retourne aucun binding à part
			// lui-même, donc par défaut dans glassfish on prend le path "comp" et non ""
			return "comp";
		}
		return "";
	}

	public static List<JndiBinding> listBindings(String path) throws NamingException {
		return listBindings(new InitialContext(), path);
	}

	public static List<JndiBinding> listBindings(Context context, String path)
			throws NamingException {
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
					//		if (jndiBinding.getName().isEmpty()) {
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
		final String value;
		if (object instanceof Context
				// "javax.naming.Context".equals(className) nécessaire pour le path "comp" dans JBoss 6.0
				|| "javax.naming.Context".equals(className)
				// pour jetty :
				|| object instanceof Reference
						&& "javax.naming.Context".equals(((Reference) object).getClassName())) {
			if (!path.isEmpty()) {
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
					sb.append(aItem);
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
		if (!result.isEmpty() && result.charAt(0) == '/') {
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
