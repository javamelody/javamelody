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
package net.bull.javamelody;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Proxy;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Hashtable;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NameClassPair;
import javax.naming.NamingException;
import javax.naming.NoInitialContextException;
import javax.naming.Referenceable;
import javax.servlet.ServletContext;
import javax.sql.DataSource;

import org.apache.tomcat.dbcp.dbcp.BasicDataSource;

/**
 * Classe utilitaire pour {@link JdbcWrapper}.
 * @author Emeric Vernat
 */
final class JdbcWrapperHelper {
	private static final String MAX_ACTIVE_PROPERTY_NAME = "maxActive";
	private static final Map<String, DataSource> SPRING_DATASOURCES = new LinkedHashMap<>();
	private static final Map<String, DataSource> JNDI_DATASOURCES_BACKUP = new LinkedHashMap<>();
	private static final Map<String, DataSource> REWRAPPED_DATASOURCES_BACKUP = new LinkedHashMap<>();
	private static final BasicDataSourcesProperties TOMCAT_BASIC_DATASOURCES_PROPERTIES = new BasicDataSourcesProperties();
	private static final BasicDataSourcesProperties DBCP_BASIC_DATASOURCES_PROPERTIES = new BasicDataSourcesProperties();
	private static final BasicDataSourcesProperties TOMCAT_JDBC_DATASOURCES_PROPERTIES = new BasicDataSourcesProperties();

	private static final Map<Class<?>, Constructor<?>> PROXY_CACHE = Collections
			.synchronizedMap(new WeakHashMap<Class<?>, Constructor<?>>());

	/**
	 * Propriétés des BasicDataSources si elles viennent de Tomcat-DBCP ou de DBCP seul.
	 * @author Emeric Vernat
	 */
	private static class BasicDataSourcesProperties {
		private final Map<String, Map<String, Object>> properties = new LinkedHashMap<>();

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
			final Map<String, Map<String, Object>> result = new LinkedHashMap<>();
			for (final Map.Entry<String, Map<String, Object>> entry : properties.entrySet()) {
				result.put(entry.getKey(), Collections.unmodifiableMap(entry.getValue()));
			}
			return Collections.unmodifiableMap(result);
		}

		void put(String dataSourceName, String key, Object value) {
			Map<String, Object> dataSourceProperties = properties.get(dataSourceName);
			if (dataSourceProperties == null) {
				dataSourceProperties = new LinkedHashMap<>();
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

	static void registerRewrappedDataSource(String name, DataSource dataSource) {
		REWRAPPED_DATASOURCES_BACKUP.put(name, dataSource);
	}

	static Map<String, DataSource> getRewrappedDataSources() {
		return REWRAPPED_DATASOURCES_BACKUP;
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
			dataSources = new LinkedHashMap<>(getJndiDataSources());
		} catch (final NoInitialContextException e) {
			dataSources = new LinkedHashMap<>();
		}
		dataSources.putAll(SPRING_DATASOURCES);
		return dataSources;
	}

	static Map<String, DataSource> getJndiDataSources() throws NamingException {
		final Map<String, DataSource> dataSources = new LinkedHashMap<>(2);
		final String datasourcesParameter = Parameter.DATASOURCES.getValue();
		if (datasourcesParameter == null) {
			dataSources.putAll(getJndiDataSourcesAt("java:comp/env/jdbc"));
			// pour jboss sans jboss-env.xml ou sans resource-ref dans web.xml :
			dataSources.putAll(getJndiDataSourcesAt("java:/jdbc"));
			// pour JavaEE 6 :
			// (voir par exemple http://smokeandice.blogspot.com/2009/12/datasourcedefinition-hidden-gem-from.html)
			dataSources.putAll(getJndiDataSourcesAt("java:global/jdbc"));
			// pour WebLogic 10 et WebSphere 7, cf issue 68
			dataSources.putAll(getJndiDataSourcesAt("jdbc"));
		} else if (!datasourcesParameter.trim().isEmpty()) {
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
		final Map<String, DataSource> dataSources = new LinkedHashMap<>(2);
		try {
			for (final NameClassPair nameClassPair : Collections
					.list(initialContext.list(jndiPrefix))) {
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

	// CHECKSTYLE:OFF
	static void pullDataSourceProperties(String name, DataSource dataSource) {
		// CHECKSTYLE:ON
		final String dataSourceClassName = dataSource.getClass().getName();
		if ("org.apache.tomcat.dbcp.dbcp.BasicDataSource".equals(dataSourceClassName)
				&& dataSource instanceof BasicDataSource) {
			pullTomcatDbcpDataSourceProperties(name, dataSource);
		} else if ("org.apache.tomcat.dbcp.dbcp2.BasicDataSource".equals(dataSourceClassName)
				&& dataSource instanceof org.apache.tomcat.dbcp.dbcp2.BasicDataSource) {
			pullTomcatDbcp2DataSourceProperties(name, dataSource);
		} else if ("org.apache.commons.dbcp.BasicDataSource".equals(dataSourceClassName)
				&& dataSource instanceof org.apache.commons.dbcp.BasicDataSource) {
			pullCommonsDbcpDataSourceProperties(name, dataSource);
		} else if ("org.apache.commons.dbcp2.BasicDataSource".equals(dataSourceClassName)
				&& dataSource instanceof org.apache.commons.dbcp2.BasicDataSource) {
			pullCommonsDbcp2DataSourceProperties(name, dataSource);
		} else if ("org.apache.tomcat.jdbc.pool.DataSource".equals(dataSourceClassName)
				&& dataSource instanceof org.apache.tomcat.jdbc.pool.DataSource) {
			pullTomcatJdbcDataSourceProperties(name, dataSource);
		}
	}

	private static void pullTomcatDbcpDataSourceProperties(String name, DataSource dataSource) {
		// si tomcat et si dataSource standard, alors on récupère des infos
		final BasicDataSource tomcatDbcpDataSource = (BasicDataSource) dataSource;
		final BasicDataSourcesProperties properties = TOMCAT_BASIC_DATASOURCES_PROPERTIES;
		// basicDataSource.getNumActive() est en théorie égale à USED_CONNECTION_COUNT à un instant t,
		// numIdle + numActive est le nombre de connexions ouvertes dans la bdd pour ce serveur à un instant t

		// les propriétés généralement importantes en premier (se méfier aussi de testOnBorrow)
		properties.put(name, MAX_ACTIVE_PROPERTY_NAME, tomcatDbcpDataSource.getMaxActive());
		properties.put(name, "poolPreparedStatements",
				tomcatDbcpDataSource.isPoolPreparedStatements());

		properties.put(name, "defaultCatalog", tomcatDbcpDataSource.getDefaultCatalog());
		properties.put(name, "defaultAutoCommit", tomcatDbcpDataSource.getDefaultAutoCommit());
		properties.put(name, "defaultReadOnly", tomcatDbcpDataSource.getDefaultReadOnly());
		properties.put(name, "defaultTransactionIsolation",
				tomcatDbcpDataSource.getDefaultTransactionIsolation());
		properties.put(name, "driverClassName", tomcatDbcpDataSource.getDriverClassName());
		properties.put(name, "initialSize", tomcatDbcpDataSource.getInitialSize());
		properties.put(name, "maxIdle", tomcatDbcpDataSource.getMaxIdle());
		properties.put(name, "maxOpenPreparedStatements",
				tomcatDbcpDataSource.getMaxOpenPreparedStatements());
		properties.put(name, "maxWait", tomcatDbcpDataSource.getMaxWait());
		properties.put(name, "minEvictableIdleTimeMillis",
				tomcatDbcpDataSource.getMinEvictableIdleTimeMillis());
		properties.put(name, "minIdle", tomcatDbcpDataSource.getMinIdle());
		properties.put(name, "numTestsPerEvictionRun",
				tomcatDbcpDataSource.getNumTestsPerEvictionRun());
		properties.put(name, "testOnBorrow", tomcatDbcpDataSource.getTestOnBorrow());
		properties.put(name, "testOnReturn", tomcatDbcpDataSource.getTestOnReturn());
		properties.put(name, "testWhileIdle", tomcatDbcpDataSource.getTestWhileIdle());
		properties.put(name, "timeBetweenEvictionRunsMillis",
				tomcatDbcpDataSource.getTimeBetweenEvictionRunsMillis());
		properties.put(name, "validationQuery", tomcatDbcpDataSource.getValidationQuery());
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

	private static void pullTomcatDbcp2DataSourceProperties(String name, DataSource dataSource) {
		// si tomcat et si dataSource standard, alors on récupère des infos
		final org.apache.tomcat.dbcp.dbcp2.BasicDataSource tomcatDbcp2DataSource = (org.apache.tomcat.dbcp.dbcp2.BasicDataSource) dataSource;
		final BasicDataSourcesProperties properties = TOMCAT_BASIC_DATASOURCES_PROPERTIES;
		// basicDataSource.getNumActive() est en théorie égale à USED_CONNECTION_COUNT à un instant t,
		// numIdle + numActive est le nombre de connexions ouvertes dans la bdd pour ce serveur à un instant t

		// les propriétés généralement importantes en premier (se méfier aussi de testOnBorrow)
		properties.put(name, MAX_ACTIVE_PROPERTY_NAME, tomcatDbcp2DataSource.getMaxTotal());
		properties.put(name, "poolPreparedStatements",
				tomcatDbcp2DataSource.isPoolPreparedStatements());

		properties.put(name, "defaultCatalog", tomcatDbcp2DataSource.getDefaultCatalog());
		properties.put(name, "defaultAutoCommit", tomcatDbcp2DataSource.getDefaultAutoCommit());
		properties.put(name, "defaultReadOnly", tomcatDbcp2DataSource.getDefaultReadOnly());
		properties.put(name, "defaultTransactionIsolation",
				tomcatDbcp2DataSource.getDefaultTransactionIsolation());
		properties.put(name, "driverClassName", tomcatDbcp2DataSource.getDriverClassName());
		properties.put(name, "initialSize", tomcatDbcp2DataSource.getInitialSize());
		properties.put(name, "maxIdle", tomcatDbcp2DataSource.getMaxIdle());
		properties.put(name, "maxOpenPreparedStatements",
				tomcatDbcp2DataSource.getMaxOpenPreparedStatements());
		properties.put(name, "maxWait", tomcatDbcp2DataSource.getMaxWaitMillis());
		properties.put(name, "minEvictableIdleTimeMillis",
				tomcatDbcp2DataSource.getMinEvictableIdleTimeMillis());
		properties.put(name, "minIdle", tomcatDbcp2DataSource.getMinIdle());
		properties.put(name, "numTestsPerEvictionRun",
				tomcatDbcp2DataSource.getNumTestsPerEvictionRun());
		properties.put(name, "testOnBorrow", tomcatDbcp2DataSource.getTestOnBorrow());
		properties.put(name, "testOnReturn", tomcatDbcp2DataSource.getTestOnReturn());
		properties.put(name, "testWhileIdle", tomcatDbcp2DataSource.getTestWhileIdle());
		properties.put(name, "timeBetweenEvictionRunsMillis",
				tomcatDbcp2DataSource.getTimeBetweenEvictionRunsMillis());
		properties.put(name, "validationQuery", tomcatDbcp2DataSource.getValidationQuery());
	}

	private static void pullCommonsDbcp2DataSourceProperties(String name, DataSource dataSource) {
		// si dbcp et si dataSource standard, alors on récupère des infos
		final org.apache.commons.dbcp2.BasicDataSource dbcp2DataSource = (org.apache.commons.dbcp2.BasicDataSource) dataSource;
		final BasicDataSourcesProperties properties = DBCP_BASIC_DATASOURCES_PROPERTIES;
		// basicDataSource.getNumActive() est en théorie égale à USED_CONNECTION_COUNT à un instant t,
		// numIdle + numActive est le nombre de connexions ouvertes dans la bdd pour ce serveur à un instant t

		// les propriétés généralement importantes en premier (se méfier aussi de testOnBorrow)
		properties.put(name, MAX_ACTIVE_PROPERTY_NAME, dbcp2DataSource.getMaxTotal());
		properties.put(name, "poolPreparedStatements", dbcp2DataSource.isPoolPreparedStatements());

		properties.put(name, "defaultCatalog", dbcp2DataSource.getDefaultCatalog());
		properties.put(name, "defaultAutoCommit", dbcp2DataSource.getDefaultAutoCommit());
		properties.put(name, "defaultReadOnly", dbcp2DataSource.getDefaultReadOnly());
		properties.put(name, "defaultTransactionIsolation",
				dbcp2DataSource.getDefaultTransactionIsolation());
		properties.put(name, "driverClassName", dbcp2DataSource.getDriverClassName());
		properties.put(name, "initialSize", dbcp2DataSource.getInitialSize());
		properties.put(name, "maxIdle", dbcp2DataSource.getMaxIdle());
		properties.put(name, "maxOpenPreparedStatements",
				dbcp2DataSource.getMaxOpenPreparedStatements());
		properties.put(name, "maxWait", dbcp2DataSource.getMaxWaitMillis());
		properties.put(name, "minEvictableIdleTimeMillis",
				dbcp2DataSource.getMinEvictableIdleTimeMillis());
		properties.put(name, "minIdle", dbcp2DataSource.getMinIdle());
		properties.put(name, "numTestsPerEvictionRun", dbcp2DataSource.getNumTestsPerEvictionRun());
		properties.put(name, "testOnBorrow", dbcp2DataSource.getTestOnBorrow());
		properties.put(name, "testOnReturn", dbcp2DataSource.getTestOnReturn());
		properties.put(name, "testWhileIdle", dbcp2DataSource.getTestWhileIdle());
		properties.put(name, "timeBetweenEvictionRunsMillis",
				dbcp2DataSource.getTimeBetweenEvictionRunsMillis());
		properties.put(name, "validationQuery", dbcp2DataSource.getValidationQuery());
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
	private static Object changeContextWritable(ServletContext servletContext, Object lock)
			throws NoSuchFieldException, ClassNotFoundException, IllegalAccessException,
			NamingException {
		// cette méthode ne peut pas être utilisée avec un simple JdbcDriver
		assert servletContext != null;
		final String serverInfo = servletContext.getServerInfo();
		if (serverInfo.contains("jetty")) {
			// on n'exécute cela que si c'est jetty
			final Context jdbcContext = (Context) new InitialContext().lookup("java:comp");
			final Field field = getAccessibleField(jdbcContext, "_env");
			@SuppressWarnings("unchecked")
			final Hashtable<Object, Object> env = (Hashtable<Object, Object>) field
					.get(jdbcContext);
			if (lock == null) {
				// on rend le contexte writable
				Object result = env.remove("org.mortbay.jndi.lock");
				if (result == null) {
					result = env.remove("org.eclipse.jndi.lock");
				}
				return result;
			}
			// on remet le contexte not writable comme avant
			env.put("org.mortbay.jndi.lock", lock);
			env.put("org.eclipse.jndi.lock", lock);

			return null;
		}
		// si on arrive là (pas rewrap et pas jetty),
		// alors on suppose que cela pourrait être Tomcat
		// (serverInfo.contains("Tomcat") || serverInfo.contains("vFabric") || serverInfo.contains("SpringSource tc Runtime"))
		// même si serverInfo a été modifié dans ServerInfo.properties
		try {
			final Field field = Class.forName("org.apache.naming.ContextAccessController")
					.getDeclaredField("readOnlyContexts");
			setFieldAccessible(field);
			@SuppressWarnings("unchecked")
			final Map<String, Object> readOnlyContexts = 
                                (Map<String, Object>) field.get(null);
			// la clé dans cette Hashtable est normalement
			// "/Catalina/" + hostName + Parameters.getContextPath(servletContext) ;
			// hostName vaut en général "localhost" (ou autre selon le Host dans server.xml)
			// et contextPath vaut "/myapp" par exemple ;
			// la valeur est un securityToken
			if (lock == null) {
				// on utilise clear et non remove au cas où le host ne soit pas localhost dans server.xml
				// (cf issue 105)
				final Hashtable<String, Object> clone = new Hashtable<>(readOnlyContexts);
				readOnlyContexts.clear();
				return clone;
			}
			// on remet le contexte not writable comme avant
			@SuppressWarnings("unchecked")
			final Hashtable<String, Object> myLock = (Hashtable<String, Object>) lock;
			readOnlyContexts.putAll(myLock);

			return null;
		} catch (final Exception e) {
			// tant pis pour le lock jndi, ce n'était pas Tomcat finalement
			return null;
		}
	}

	static Object getFieldValue(Object object, String fieldName) throws IllegalAccessException {
		return getAccessibleField(object, fieldName).get(object);
	}

	static void setFieldValue(Object object, String fieldName, Object value)
			throws IllegalAccessException {
		getAccessibleField(object, fieldName).set(object, value);
	}

	static boolean hasField(Object object, String fieldName) {
		return getField(object, fieldName) != null;
	}

	private static Field getAccessibleField(Object object, String fieldName) {
		final Field result = getField(object, fieldName);
		assert result != null;
		setFieldAccessible(result);
		return result;
	}

	private static Field getField(Object object, String fieldName) {
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

	static void clearProxyCache() {
		PROXY_CACHE.clear();
	}

	@SuppressWarnings("unchecked")
	static <T> T createProxy(T object, InvocationHandler invocationHandler,
			List<Class<?>> interfaces) {
		final Class<?> objectClass = object.getClass();
		// ce handler désencapsule les InvocationTargetException des 3 proxy
		// avant : return (T) Proxy.newProxyInstance(objectClass.getClassLoader(), getObjectInterfaces(objectClass, interfaces), ih);
		// maintenant (optimisé) :
		Constructor<?> constructor = PROXY_CACHE.get(objectClass);
		if (constructor == null) {
			final Class<?>[] interfacesArray = getObjectInterfaces(objectClass, interfaces);
			constructor = getProxyConstructor(objectClass, interfacesArray);
			// si interfaces est non null, la classe n'est en théorie pas suffisante comme clé,
			// mais ce n'est pas un cas courant
			if (interfaces == null) {
				PROXY_CACHE.put(objectClass, constructor);
			}
		}
		try {
			return (T) constructor.newInstance(new Object[] { invocationHandler });
		} catch (final Exception e) {
			throw new IllegalStateException(e);
		}
	}

	private static Constructor<?> getProxyConstructor(Class<?> objectClass,
			Class<?>[] interfacesArray) {
		final ClassLoader classLoader = objectClass.getClassLoader();
		try {
			final Constructor<?> constructor = Proxy.getProxyClass(classLoader, interfacesArray)
					.getConstructor(InvocationHandler.class);
			// issue 475: workaround for a j.l.r.Proxy change in Java 8 - proxy class for non-public interface is non-public now.
			// Ref: https://netbeans.org/bugzilla/show_bug.cgi?id=229191
			// and http://hg.netbeans.org/jet-main/rev/3238e03c676f
			constructor.setAccessible(true);
			return constructor;
		} catch (final NoSuchMethodException e) {
			throw new IllegalStateException(e);
		}
	}

	private static Class<?>[] getObjectInterfaces(Class<?> objectClass, List<Class<?>> interfaces) {
		// Rq: object.getClass().getInterfaces() ne suffit pas pour Connection dans Tomcat
		// car la connection est une instance de PoolGuardConnectionWrapper
		// et connection.getClass().getInterfaces() est vide dans ce cas
		final List<Class<?>> myInterfaces;
		if (interfaces == null) {
			myInterfaces = new ArrayList<>(Arrays.asList(objectClass.getInterfaces()));
			Class<?> classe = objectClass.getSuperclass();
			while (classe != null) {
				final Class<?>[] classInterfaces = classe.getInterfaces();
				if (classInterfaces.length > 0) {
					final List<Class<?>> superInterfaces = Arrays.asList(classInterfaces);
					// removeAll d'abord car il ne faut pas de doublon dans la liste
					myInterfaces.removeAll(superInterfaces);
					myInterfaces.addAll(superInterfaces);
				}
				classe = classe.getSuperclass();
			}
			// on ignore l'interface javax.naming.Referenceable car sinon le rebind sous jetty appelle
			// referenceable.getReference() et devient inutile
			myInterfaces.remove(Referenceable.class);
		} else {
			myInterfaces = interfaces;
		}
		return myInterfaces.toArray(new Class<?>[0]);
	}
}
