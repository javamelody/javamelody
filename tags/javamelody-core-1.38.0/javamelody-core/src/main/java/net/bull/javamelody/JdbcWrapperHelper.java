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

import java.lang.reflect.Field;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.Collections;
import java.util.Hashtable;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NameClassPair;
import javax.naming.NamingException;
import javax.naming.NoInitialContextException;
import javax.servlet.ServletContext;
import javax.sql.DataSource;

import org.apache.tomcat.dbcp.dbcp.BasicDataSource;

/**
 * Classe utilitaire pour JdbcWrapper.
 * @author Emeric Vernat
 */
final class JdbcWrapperHelper {
	private static final String MAX_ACTIVE_PROPERTY_NAME = "maxActive";
	private static final Map<String, DataSource> SPRING_DATASOURCES = new LinkedHashMap<String, DataSource>();
	private static final Map<String, DataSource> JNDI_DATASOURCES_BACKUP = new LinkedHashMap<String, DataSource>();
	private static final BasicDataSourcesProperties TOMCAT_BASIC_DATASOURCES_PROPERTIES = new BasicDataSourcesProperties();
	private static final BasicDataSourcesProperties DBCP_BASIC_DATASOURCES_PROPERTIES = new BasicDataSourcesProperties();
	private static final BasicDataSourcesProperties TOMCAT_JDBC_DATASOURCES_PROPERTIES = new BasicDataSourcesProperties();

	/**
	 * Propriétés des BasicDataSources si elles viennent de Tomcat-DBCP ou de DBCP seul.
	 * @author Emeric Vernat
	 */
	private static class BasicDataSourcesProperties {
		private final Map<String, Map<String, Object>> properties = new LinkedHashMap<String, Map<String, Object>>();

		BasicDataSourcesProperties() {
			super();
		}

		boolean isEmpty() {
			return properties.isEmpty();
		}

		int getMaxActive() {
			int result = 0;
			for (final Map<String, Object> dataSourceProperties : properties.values()) {
				final Integer maxActive = (Integer) dataSourceProperties
						.get(MAX_ACTIVE_PROPERTY_NAME);
				if (maxActive == null) {
					return -1;
				}
				result += maxActive;
			}
			return result;
		}

		Map<String, Map<String, Object>> getDataSourcesProperties() {
			final Map<String, Map<String, Object>> result = new LinkedHashMap<String, Map<String, Object>>();
			for (final Map.Entry<String, Map<String, Object>> entry : properties.entrySet()) {
				result.put(entry.getKey(), Collections.unmodifiableMap(entry.getValue()));
			}
			return Collections.unmodifiableMap(result);
		}

		void put(String dataSourceName, String key, Object value) {
			Map<String, Object> dataSourceProperties = properties.get(dataSourceName);
			if (dataSourceProperties == null) {
				dataSourceProperties = new LinkedHashMap<String, Object>();
				properties.put(dataSourceName, dataSourceProperties);
			}
			dataSourceProperties.put(key, value);
		}
	}

	private JdbcWrapperHelper() {
		super();
	}

	static void registerSpringDataSource(String name, DataSource dataSource) {
		SPRING_DATASOURCES.put(name, dataSource);
	}

	static void rebindDataSource(ServletContext servletContext, String jndiName,
			DataSource dataSource, DataSource dataSourceProxy) throws Throwable {
		final Object lock = changeContextWritable(servletContext, null);
		final InitialContext initialContext = new InitialContext();
		initialContext.rebind(jndiName, dataSourceProxy);
		JNDI_DATASOURCES_BACKUP.put(jndiName, dataSource);
		changeContextWritable(servletContext, lock);
		initialContext.close();
	}

	static void rebindInitialDataSources(ServletContext servletContext) throws Throwable {
		try {
			final InitialContext initialContext = new InitialContext();
			for (final Map.Entry<String, DataSource> entry : JNDI_DATASOURCES_BACKUP.entrySet()) {
				final String jndiName = entry.getKey();
				final DataSource dataSource = entry.getValue();
				final Object lock = changeContextWritable(servletContext, null);
				initialContext.rebind(jndiName, dataSource);
				changeContextWritable(servletContext, lock);
			}
			initialContext.close();
		} finally {
			JNDI_DATASOURCES_BACKUP.clear();
		}
	}

	static Map<String, DataSource> getJndiAndSpringDataSources() throws NamingException {
		Map<String, DataSource> dataSources;
		try {
			dataSources = new LinkedHashMap<String, DataSource>(getJndiDataSources());
		} catch (final NoInitialContextException e) {
			dataSources = new LinkedHashMap<String, DataSource>();
		}
		dataSources.putAll(SPRING_DATASOURCES);
		return dataSources;
	}

	static Map<String, DataSource> getJndiDataSources() throws NamingException {
		final Map<String, DataSource> dataSources = new LinkedHashMap<String, DataSource>(2);
		final String datasourcesParameter = Parameters.getParameter(Parameter.DATASOURCES);
		if (datasourcesParameter == null) {
			dataSources.putAll(getJndiDataSourcesAt("java:comp/env/jdbc"));
			// pour jboss sans jboss-env.xml ou sans resource-ref dans web.xml :
			dataSources.putAll(getJndiDataSourcesAt("java:/jdbc"));
			// pour JavaEE 6 :
			// (voir par exemple http://smokeandice.blogspot.com/2009/12/datasourcedefinition-hidden-gem-from.html)
			dataSources.putAll(getJndiDataSourcesAt("java:global/jdbc"));
			// pour WebLogic 10 et WebSphere 7, cf issue 68
			dataSources.putAll(getJndiDataSourcesAt("jdbc"));
		} else if (datasourcesParameter.trim().length() != 0) { // NOPMD
			final InitialContext initialContext = new InitialContext();
			for (final String datasource : datasourcesParameter.split(",")) {
				final String jndiName = datasource.trim();
				// ici, on n'ajoute pas java:/comp/env
				// et on suppose qu'il n'en faut pas ou que cela a été ajouté dans le paramétrage
				final DataSource dataSource = (DataSource) initialContext.lookup(jndiName);
				dataSources.put(jndiName, dataSource);
			}
			initialContext.close();
		}
		return Collections.unmodifiableMap(dataSources);
	}

	private static Map<String, DataSource> getJndiDataSourcesAt(String jndiPrefix)
			throws NamingException {
		final InitialContext initialContext = new InitialContext();
		final Map<String, DataSource> dataSources = new LinkedHashMap<String, DataSource>(2);
		try {
			for (final NameClassPair nameClassPair : Collections.list(initialContext
					.list(jndiPrefix))) {
				// note: il ne suffit pas de tester
				// (DataSource.class.isAssignableFrom(Class.forName(nameClassPair.getClassName())))
				// car nameClassPair.getClassName() vaut "javax.naming.LinkRef" sous jboss 5.1.0.GA
				// par exemple, donc on fait le lookup pour voir
				final String jndiName;
				if (nameClassPair.getName().startsWith("java:")) {
					// pour glassfish v3
					jndiName = nameClassPair.getName();
				} else {
					jndiName = jndiPrefix + '/' + nameClassPair.getName();
				}
				final Object value = initialContext.lookup(jndiName);
				if (value instanceof DataSource) {
					dataSources.put(jndiName, (DataSource) value);
				}
			}
		} catch (final NamingException e) {
			// le préfixe ("comp/env/jdbc", "/jdbc" ou "java:global/jdbc", etc) n'existe pas dans jndi,
			// (dans glassfish 3.0.1, c'est une NamingException et non une NameNotFoundException)
			return dataSources;
		}
		initialContext.close();
		return dataSources;
	}

	static int getMaxConnectionCount() {
		if (!TOMCAT_BASIC_DATASOURCES_PROPERTIES.isEmpty()) {
			return TOMCAT_BASIC_DATASOURCES_PROPERTIES.getMaxActive();
		} else if (!DBCP_BASIC_DATASOURCES_PROPERTIES.isEmpty()) {
			return DBCP_BASIC_DATASOURCES_PROPERTIES.getMaxActive();
		} else if (!TOMCAT_JDBC_DATASOURCES_PROPERTIES.isEmpty()) {
			return TOMCAT_JDBC_DATASOURCES_PROPERTIES.getMaxActive();
		}
		return -1;
	}

	static Map<String, Map<String, Object>> getBasicDataSourceProperties() {
		if (!TOMCAT_BASIC_DATASOURCES_PROPERTIES.isEmpty()) {
			return TOMCAT_BASIC_DATASOURCES_PROPERTIES.getDataSourcesProperties();
		} else if (!DBCP_BASIC_DATASOURCES_PROPERTIES.isEmpty()) {
			return DBCP_BASIC_DATASOURCES_PROPERTIES.getDataSourcesProperties();
		} else if (!TOMCAT_JDBC_DATASOURCES_PROPERTIES.isEmpty()) {
			return TOMCAT_JDBC_DATASOURCES_PROPERTIES.getDataSourcesProperties();
		}
		return Collections.emptyMap();
	}

	static void pullDataSourceProperties(String name, DataSource dataSource) {
		final String dataSourceClassName = dataSource.getClass().getName();
		if ("org.apache.tomcat.dbcp.dbcp.BasicDataSource".equals(dataSourceClassName)
				&& dataSource instanceof BasicDataSource) {
			pullTomcatDbcpDataSourceProperties(name, dataSource);
		} else if ("org.apache.commons.dbcp.BasicDataSource"
				.equals(dataSource.getClass().getName())
				&& dataSource instanceof org.apache.commons.dbcp.BasicDataSource) {
			pullCommonsDbcpDataSourceProperties(name, dataSource);
		} else if ("org.apache.tomcat.jdbc.pool.DataSource".equals(dataSource.getClass().getName())
				&& dataSource instanceof org.apache.tomcat.jdbc.pool.DataSource) {
			pullTomcatJdbcDataSourceProperties(name, dataSource);
		}
	}

	private static void pullTomcatDbcpDataSourceProperties(String name, DataSource dataSource) {
		// si tomcat et si dataSource standard, alors on récupère des infos
		final BasicDataSource tcDataSource = (BasicDataSource) dataSource;
		final BasicDataSourcesProperties properties = TOMCAT_BASIC_DATASOURCES_PROPERTIES;
		// basicDataSource.getNumActive() est en théorie égale à USED_CONNECTION_COUNT à un instant t,
		// numIdle + numActive est le nombre de connexions ouvertes dans la bdd pour ce serveur à un instant t

		// les propriétés généralement importantes en premier (se méfier aussi de testOnBorrow)
		properties.put(name, MAX_ACTIVE_PROPERTY_NAME, tcDataSource.getMaxActive());
		properties.put(name, "poolPreparedStatements", tcDataSource.isPoolPreparedStatements());

		properties.put(name, "defaultCatalog", tcDataSource.getDefaultCatalog());
		properties.put(name, "defaultAutoCommit", tcDataSource.getDefaultAutoCommit());
		properties.put(name, "defaultReadOnly", tcDataSource.getDefaultReadOnly());
		properties.put(name, "defaultTransactionIsolation",
				tcDataSource.getDefaultTransactionIsolation());
		properties.put(name, "driverClassName", tcDataSource.getDriverClassName());
		properties.put(name, "initialSize", tcDataSource.getInitialSize());
		properties.put(name, "maxIdle", tcDataSource.getMaxIdle());
		properties.put(name, "maxOpenPreparedStatements",
				tcDataSource.getMaxOpenPreparedStatements());
		properties.put(name, "maxWait", tcDataSource.getMaxWait());
		properties.put(name, "minEvictableIdleTimeMillis",
				tcDataSource.getMinEvictableIdleTimeMillis());
		properties.put(name, "minIdle", tcDataSource.getMinIdle());
		properties.put(name, "numTestsPerEvictionRun", tcDataSource.getNumTestsPerEvictionRun());
		properties.put(name, "testOnBorrow", tcDataSource.getTestOnBorrow());
		properties.put(name, "testOnReturn", tcDataSource.getTestOnReturn());
		properties.put(name, "testWhileIdle", tcDataSource.getTestWhileIdle());
		properties.put(name, "timeBetweenEvictionRunsMillis",
				tcDataSource.getTimeBetweenEvictionRunsMillis());
		properties.put(name, "validationQuery", tcDataSource.getValidationQuery());
	}

	private static void pullCommonsDbcpDataSourceProperties(String name, DataSource dataSource) {
		// si dbcp et si dataSource standard, alors on récupère des infos
		final org.apache.commons.dbcp.BasicDataSource dbcpDataSource = (org.apache.commons.dbcp.BasicDataSource) dataSource;
		final BasicDataSourcesProperties properties = DBCP_BASIC_DATASOURCES_PROPERTIES;
		// basicDataSource.getNumActive() est en théorie égale à USED_CONNECTION_COUNT à un instant t,
		// numIdle + numActive est le nombre de connexions ouvertes dans la bdd pour ce serveur à un instant t

		// les propriétés généralement importantes en premier (se méfier aussi de testOnBorrow)
		properties.put(name, MAX_ACTIVE_PROPERTY_NAME, dbcpDataSource.getMaxActive());
		properties.put(name, "poolPreparedStatements", dbcpDataSource.isPoolPreparedStatements());

		properties.put(name, "defaultCatalog", dbcpDataSource.getDefaultCatalog());
		properties.put(name, "defaultAutoCommit", dbcpDataSource.getDefaultAutoCommit());
		properties.put(name, "defaultReadOnly", dbcpDataSource.getDefaultReadOnly());
		properties.put(name, "defaultTransactionIsolation",
				dbcpDataSource.getDefaultTransactionIsolation());
		properties.put(name, "driverClassName", dbcpDataSource.getDriverClassName());
		properties.put(name, "initialSize", dbcpDataSource.getInitialSize());
		properties.put(name, "maxIdle", dbcpDataSource.getMaxIdle());
		properties.put(name, "maxOpenPreparedStatements",
				dbcpDataSource.getMaxOpenPreparedStatements());
		properties.put(name, "maxWait", dbcpDataSource.getMaxWait());
		properties.put(name, "minEvictableIdleTimeMillis",
				dbcpDataSource.getMinEvictableIdleTimeMillis());
		properties.put(name, "minIdle", dbcpDataSource.getMinIdle());
		properties.put(name, "numTestsPerEvictionRun", dbcpDataSource.getNumTestsPerEvictionRun());
		properties.put(name, "testOnBorrow", dbcpDataSource.getTestOnBorrow());
		properties.put(name, "testOnReturn", dbcpDataSource.getTestOnReturn());
		properties.put(name, "testWhileIdle", dbcpDataSource.getTestWhileIdle());
		properties.put(name, "timeBetweenEvictionRunsMillis",
				dbcpDataSource.getTimeBetweenEvictionRunsMillis());
		properties.put(name, "validationQuery", dbcpDataSource.getValidationQuery());
	}

	private static void pullTomcatJdbcDataSourceProperties(String name, DataSource dataSource) {
		// si tomcat-jdbc, alors on récupère des infos
		final org.apache.tomcat.jdbc.pool.DataSource jdbcDataSource = (org.apache.tomcat.jdbc.pool.DataSource) dataSource;
		final BasicDataSourcesProperties properties = TOMCAT_JDBC_DATASOURCES_PROPERTIES;
		// basicDataSource.getNumActive() est en théorie égale à USED_CONNECTION_COUNT à un instant t,
		// numIdle + numActive est le nombre de connexions ouvertes dans la bdd pour ce serveur à un instant t

		// les propriétés généralement importantes en premier (se méfier aussi de testOnBorrow)
		properties.put(name, MAX_ACTIVE_PROPERTY_NAME, jdbcDataSource.getMaxActive());

		properties.put(name, "defaultCatalog", jdbcDataSource.getDefaultCatalog());
		properties.put(name, "defaultAutoCommit", jdbcDataSource.getDefaultAutoCommit());
		properties.put(name, "defaultReadOnly", jdbcDataSource.getDefaultReadOnly());
		properties.put(name, "defaultTransactionIsolation",
				jdbcDataSource.getDefaultTransactionIsolation());
		properties.put(name, "driverClassName", jdbcDataSource.getDriverClassName());
		properties.put(name, "connectionProperties", jdbcDataSource.getConnectionProperties());
		properties.put(name, "initSQL", jdbcDataSource.getInitSQL());
		properties.put(name, "initialSize", jdbcDataSource.getInitialSize());
		properties.put(name, "maxIdle", jdbcDataSource.getMaxIdle());
		properties.put(name, "maxWait", jdbcDataSource.getMaxWait());
		properties.put(name, "maxAge", jdbcDataSource.getMaxAge());
		properties.put(name, "faireQueue", jdbcDataSource.isFairQueue());
		properties.put(name, "jmxEnabled", jdbcDataSource.isJmxEnabled());
		properties.put(name, "minEvictableIdleTimeMillis",
				jdbcDataSource.getMinEvictableIdleTimeMillis());
		properties.put(name, "minIdle", jdbcDataSource.getMinIdle());
		properties.put(name, "numTestsPerEvictionRun", jdbcDataSource.getNumTestsPerEvictionRun());
		properties.put(name, "testOnBorrow", jdbcDataSource.isTestOnBorrow());
		properties.put(name, "testOnConnect", jdbcDataSource.isTestOnConnect());
		properties.put(name, "testOnReturn", jdbcDataSource.isTestOnReturn());
		properties.put(name, "testWhileIdle", jdbcDataSource.isTestWhileIdle());
		properties.put(name, "timeBetweenEvictionRunsMillis",
				jdbcDataSource.getTimeBetweenEvictionRunsMillis());
		properties.put(name, "validationInterval", jdbcDataSource.getValidationInterval());
		properties.put(name, "validationQuery", jdbcDataSource.getValidationQuery());
		properties.put(name, "validatorClassName", jdbcDataSource.getValidatorClassName());
	}

	@SuppressWarnings("all")
	// CHECKSTYLE:OFF
	private static Object changeContextWritable(ServletContext servletContext, Object lock)
			throws NoSuchFieldException, ClassNotFoundException, IllegalAccessException,
			NamingException {
		// cette méthode ne peut pas être utilisée avec un simple JdbcDriver
		assert servletContext != null;
		final String serverInfo = servletContext.getServerInfo();
		if ((serverInfo.contains("Tomcat") || serverInfo.contains("vFabric"))
				&& System.getProperty("jonas.name") == null) {
			// on n'exécute cela que si c'est tomcat
			// et si ce n'est pas tomcat dans jonas
			final Field field = Class.forName("org.apache.naming.ContextAccessController")
					.getDeclaredField("readOnlyContexts");
			setFieldAccessible(field);
			@SuppressWarnings("unchecked")
			final Hashtable<String, Object> readOnlyContexts = (Hashtable<String, Object>) field
					.get(null);
			// la clé dans cette Hashtable est normalement
			// "/Catalina/" + hostName + Parameters.getContextPath(servletContext) ;
			// hostName vaut en général "localhost" (ou autre selon le Host dans server.xml)
			// et contextPath vaut "/myapp" par exemple ;
			// la valeur est un securityToken
			if (lock == null) {
				// on utilise clear et non remove au cas où le host ne soit pas localhost dans server.xml
				// (cf issue 105)
				final Hashtable<String, Object> clone = new Hashtable<String, Object>(
						readOnlyContexts);
				readOnlyContexts.clear();
				return clone;
			}
			// on remet le contexte not writable comme avant
			@SuppressWarnings("unchecked")
			final Hashtable<String, Object> myLock = (Hashtable<String, Object>) lock;
			readOnlyContexts.putAll(myLock);

			return null;
		} else if (serverInfo.contains("jetty")) {
			// on n'exécute cela que si c'est jetty
			final Context jdbcContext = (Context) new InitialContext().lookup("java:comp");
			final Field field = getAccessibleField(jdbcContext, "_env");
			@SuppressWarnings("unchecked")
			final Hashtable<Object, Object> env = (Hashtable<Object, Object>) field
					.get(jdbcContext);
			if (lock == null) {
				// on rend le contexte writable
				return env.remove("org.mortbay.jndi.lock");
			}
			// on remet le contexte not writable comme avant
			env.put("org.mortbay.jndi.lock", lock);

			return null;
		}
		return null;
	}

	static Object getFieldValue(Object object, String fieldName) throws IllegalAccessException {
		return getAccessibleField(object, fieldName).get(object);
	}

	static void setFieldValue(Object object, String fieldName, Object value)
			throws IllegalAccessException {
		getAccessibleField(object, fieldName).set(object, value);
	}

	private static Field getAccessibleField(Object object, String fieldName) {
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
					@Override
					public Object run() {
						field.setAccessible(true);
						return null;
					}
				});
	}
	// CHECKSTYLE:ON
}
