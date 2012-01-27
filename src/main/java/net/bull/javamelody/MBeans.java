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

import java.lang.management.ManagementFactory;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import javax.management.Attribute;
import javax.management.InstanceNotFoundException;
import javax.management.JMException;
import javax.management.MBeanAttributeInfo;
import javax.management.MBeanInfo;
import javax.management.MBeanServer;
import javax.management.MBeanServerFactory;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.TabularData;

/**
 * Objet récupérant une instance de MBeanServer lors de sa construction
 * et permettant de récupérer différentes données sur les MBeans.
 * @author Emeric Vernat
 */
final class MBeans {
	private static final String JAVA_LANG_MBEAN_DESCRIPTION = "Information on the management interface of the MBean";
	private final MBeanServer mbeanServer;

	MBeans() {
		this(getPlatformMBeanServer());
	}

	MBeans(MBeanServer mbeanServer) {
		super();
		this.mbeanServer = mbeanServer;
	}

	Set<ObjectName> getTomcatThreadPools() throws MalformedObjectNameException {
		return mbeanServer.queryNames(new ObjectName("*:type=ThreadPool,*"), null);
	}

	Set<ObjectName> getTomcatGlobalRequestProcessors() throws MalformedObjectNameException {
		return mbeanServer.queryNames(new ObjectName("*:type=GlobalRequestProcessor,*"), null);
	}

	Object getAttribute(ObjectName name, String attribute) throws JMException {
		return mbeanServer.getAttribute(name, attribute);
	}

	private static void initJRockitMBeansIfNeeded() {
		// si jrockit, on initialise les MBeans spécifiques jrockit lors de la première demande
		if (System.getProperty("java.vendor").contains("BEA")) {
			try {
				// initialisation des MBeans jrockit comme indiqué dans http://blogs.oracle.com/hirt/jrockit/
				try {
					getPlatformMBeanServer().getMBeanInfo(
							new ObjectName("bea.jrockit.management:type=JRockitConsole"));
				} catch (final InstanceNotFoundException e1) {
					getPlatformMBeanServer().createMBean("bea.jrockit.management.JRockitConsole",
							null);
					LOG.debug("JRockit MBeans initialized");
				}
			} catch (final JMException e) {
				throw new IllegalStateException(e);
			}
		}
	}

	Map<String, Map<String, List<ObjectName>>> getMapObjectNamesByDomainAndFirstProperty() {
		initJRockitMBeansIfNeeded();

		// TreeMap et non HashMap ou LinkedHashMap pour trier par ordre des clés
		final Map<String, Map<String, List<ObjectName>>> mapObjectNamesByDomainAndFirstProperty = new TreeMap<String, Map<String, List<ObjectName>>>();
		final Set<ObjectName> names = mbeanServer.queryNames(null, null);
		for (final ObjectName name : names) {
			final String domain = name.getDomain();
			if ("jboss.deployment".equals(domain)) {
				// la partie "jboss.deployment" dans JBoss (5.0.x) est plutôt inutile et trop lourde
				continue;
			}
			Map<String, List<ObjectName>> mapObjectNamesByFirstProperty = mapObjectNamesByDomainAndFirstProperty
					.get(domain);
			if (mapObjectNamesByFirstProperty == null) {
				mapObjectNamesByFirstProperty = new TreeMap<String, List<ObjectName>>();
				mapObjectNamesByDomainAndFirstProperty.put(domain, mapObjectNamesByFirstProperty);
			}
			final String keyPropertyListString = name.getKeyPropertyListString();
			final String firstPropertyValue;
			final int indexOf = keyPropertyListString.indexOf('=');
			if (indexOf == -1) {
				// n'arrive probablement pas, mais au cas où
				firstPropertyValue = null;
			} else {
				firstPropertyValue = name.getKeyProperty(keyPropertyListString
						.substring(0, indexOf));
			}
			if ("Servlet".equals(firstPropertyValue) && "jonas".equals(domain)) {
				// la partie "jonas:j2eeType=Servlet" dans Jonas (5.1.0) est trop lourde
				continue;
			}
			List<ObjectName> objectNames = mapObjectNamesByFirstProperty.get(firstPropertyValue);
			if (objectNames == null) {
				objectNames = new ArrayList<ObjectName>();
				mapObjectNamesByFirstProperty.put(firstPropertyValue, objectNames);
			}
			objectNames.add(name);
		}
		return mapObjectNamesByDomainAndFirstProperty;
	}

	MBeanInfo getMBeanInfo(ObjectName name) throws JMException {
		return mbeanServer.getMBeanInfo(name);
	}

	Map<String, Object> getAttributes(ObjectName name, MBeanAttributeInfo[] attributeInfos)
			throws JMException {
		final List<String> attributeNames = new ArrayList<String>(attributeInfos.length);
		for (final MBeanAttributeInfo attribute : attributeInfos) {
			// on ne veut pas afficher l'attribut password, jamais
			// (notamment, dans users tomcat ou dans datasources tomcat)
			if (attribute.isReadable() && !"password".equalsIgnoreCase(attribute.getName())) {
				attributeNames.add(attribute.getName());
			}
		}
		final String[] attributeNamesArray = attributeNames.toArray(new String[attributeNames
				.size()]);
		// issue 116: asList sur mbeanServer.getAttributes(name, attributeNamesArray) n'existe qu'en java 1.6
		final List<Object> attributes = mbeanServer.getAttributes(name, attributeNamesArray);
		final Map<String, Object> result = new TreeMap<String, Object>();
		for (final Object object : attributes) {
			final Attribute attribute = (Attribute) object;
			final Object value = convertValueIfNeeded(attribute.getValue());
			result.put(attribute.getName(), value);
		}
		return result;
	}

	String formatAttributeValue(Object attributeValue) {
		try {
			if (attributeValue instanceof List) {
				final StringBuilder sb = new StringBuilder();
				sb.append('[');
				boolean first = true;
				for (final Object value : (List<?>) attributeValue) {
					if (first) {
						first = false;
					} else {
						sb.append(",\n");
					}
					sb.append(String.valueOf(value));
				}
				sb.append(']');
				return sb.toString();
			}
			return String.valueOf(attributeValue);
		} catch (final Exception e) {
			return e.toString();
		}
	}

	String formatMBeansDescription(String description) {
		// les descriptions des MBeans de java.lang n'apportent aucune information utile
		if (description == null || JAVA_LANG_MBEAN_DESCRIPTION.equals(description)) {
			return null;
		}
		int indexOf = description.indexOf("  ");
		if (indexOf != -1) {
			// certaines descriptions de Tomcat 6 contiennent de nombreux espaces qui se suivent
			final StringBuilder sb = new StringBuilder(description);
			while (indexOf != -1) {
				sb.deleteCharAt(indexOf);
				indexOf = sb.indexOf("  ");
			}
			return sb.toString();
		}
		return description;
	}

	private Object convertValueIfNeeded(Object value) {
		if (value instanceof CompositeData) {
			final CompositeData data = (CompositeData) value;
			final Map<String, Object> values = new TreeMap<String, Object>();
			for (final String key : data.getCompositeType().keySet()) {
				values.put(key, convertValueIfNeeded(data.get(key)));
			}
			return values;
		} else if (value instanceof CompositeData[]) {
			final List<Object> list = new ArrayList<Object>();
			for (final CompositeData data : (CompositeData[]) value) {
				list.add(convertValueIfNeeded(data));
			}
			return list;
		} else if (value instanceof Object[]) {
			return Arrays.asList((Object[]) value);
		} else if (value instanceof TabularData) {
			final TabularData tabularData = (TabularData) value;
			return convertValueIfNeeded(tabularData.values());
		} else if (value instanceof Collection) {
			final List<Object> list = new ArrayList<Object>();
			for (final Object data : (Collection<?>) value) {
				list.add(convertValueIfNeeded(data));
			}
			return list;
		}
		return convertJRockitValueIfNeeded(value);
	}

	private static Object convertJRockitValueIfNeeded(Object value) {
		if (value instanceof double[]) {
			// pour jrockit MBeans
			final List<Double> list = new ArrayList<Double>();
			for (final double data : (double[]) value) {
				list.add(data);
			}
			return list;
		} else if (value instanceof int[]) {
			// pour jrockit MBeans
			final List<Integer> list = new ArrayList<Integer>();
			for (final int data : (int[]) value) {
				list.add(data);
			}
			return list;
		}
		return value;
	}

	static String getConvertedAttributes(String jmxValueParameter) {
		initJRockitMBeansIfNeeded();

		final StringBuilder sb = new StringBuilder();
		boolean first = true;
		final List<MBeanServer> mBeanServers = getMBeanServers();
		for (final String mbeansAttribute : jmxValueParameter.split("[|]")) {
			final int lastIndexOfPoint = mbeansAttribute.lastIndexOf('.');
			if (lastIndexOfPoint <= 0) {
				throw new IllegalArgumentException(mbeansAttribute);
			}
			final String name = mbeansAttribute.substring(0, lastIndexOfPoint);
			final String attribute = mbeansAttribute.substring(lastIndexOfPoint + 1);
			// on ne veut pas afficher l'attribut password, jamais
			// (notamment, dans users tomcat ou dans datasources tomcat)
			if ("password".equalsIgnoreCase(attribute)) {
				throw new IllegalArgumentException(name + '.' + attribute);
			}
			if (first) {
				first = false;
			} else {
				sb.append('|');
			}
			InstanceNotFoundException instanceNotFoundException = null;
			for (final MBeanServer mbeanServer : mBeanServers) {
				try {
					final MBeans mbeans = new MBeans(mbeanServer);
					final Object jmxValue = mbeans.convertValueIfNeeded(mbeans.getAttribute(
							new ObjectName(name), attribute));
					sb.append(jmxValue);
					instanceNotFoundException = null;
					// ObjectName trouvé dans ce MBeanServer, inutile de chercher dans les suivants
					// où il n'est d'ailleurs pas
					break;
				} catch (final InstanceNotFoundException e) {
					// ObjectName non trouvé dans ce MBeanServer, donc on cherche dans le suivant
					// (nécessaire pour JBoss 5.0.x)
					instanceNotFoundException = e;
					continue;
				} catch (final JMException e) {
					throw new IllegalArgumentException(name + '.' + attribute, e);
				}
			}
			if (instanceNotFoundException != null) {
				throw new IllegalArgumentException(name + '.' + attribute,
						instanceNotFoundException);
			}
		}
		return sb.toString();
	}

	String getAttributeDescription(String name, MBeanAttributeInfo[] attributeInfos) {
		for (final MBeanAttributeInfo attributeInfo : attributeInfos) {
			if (name.equals(attributeInfo.getName())) {
				return attributeInfo.getDescription();
			}
		}
		return null;
	}

	/**
	 * Retourne le javax.management.MBeanServer de la plateforme.
	 * @return MBeanServer
	 */
	static MBeanServer getPlatformMBeanServer() {
		return ManagementFactory.getPlatformMBeanServer();
		// alternative (sauf pour Hudson/Jenkins slaves):
		//		final List<MBeanServer> mBeanServers = MBeanServerFactory.findMBeanServer(null);
		//		if (!mBeanServers.isEmpty()) {
		//			// il existe déjà un MBeanServer créé précédemment par Tomcat ou bien ci-dessous
		//			return mBeanServers.get(0);
		//		}
		//		final MBeanServer server = MBeanServerFactory.createMBeanServer();
		//		return server;
	}

	/**
	 * Retourne la liste de tous les javax.management.MBeanServer.
	 * @return List
	 */
	static List<MBeanServer> getMBeanServers() {
		// par exemple avec JBoss 5.0.x, il y a un MBeanServer de la plateforme (defaultDomain null)
		// et un MBeanServer de JBoss (defaultDomain "jboss")
		return MBeanServerFactory.findMBeanServer(null);
	}
}
