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

import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.DriverPropertyInfo;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.util.Collections;
import java.util.Properties;
import java.util.logging.Logger;

import net.bull.javamelody.internal.common.LOG;
import net.bull.javamelody.internal.common.Parameters;

/**
 * Driver jdbc "proxy" pour le monitoring.
 * C'est la classe de ce driver qui doit être déclarée si un driver jdbc et non une dataSource est utilisé.
 * Dans la déclaration une propriété jdbc 'driver' doit contenir le nom de la classe du vrai driver.
 * D'autres propriétés jdbc comme url, username ou password peuvent être déclarées.
 * @author Emeric Vernat
 */
public class JdbcDriver implements Driver {
	// cette classe est publique pour être déclarée dans une configuration jdbc
	static final JdbcDriver SINGLETON = new JdbcDriver();

	// initialisation statique du driver
	static {
		try {
			DriverManager.registerDriver(SINGLETON);
			LOG.debug("JDBC driver registered");

			// on désinstalle et on réinstalle les autres drivers pour que le notre soit en premier
			// (notamment, si le jar du driver contient un fichier java.sql.Driver dans META-INF/services
			// pour initialiser automatiquement le driver contenu dans le jar)
			for (final Driver driver : Collections.list(DriverManager.getDrivers())) {
				if (driver != SINGLETON) {
					DriverManager.deregisterDriver(driver);
					DriverManager.registerDriver(driver);
				}
			}
		} catch (final SQLException e) {
			// ne peut arriver
			throw new IllegalStateException(e);
		}
	}

	/** {@inheritDoc} */
	@Override
	public Connection connect(String url, Properties info) throws SQLException {
		if ("false".equals(info.get("javamelody"))) {
			// if property javamelody=false then it's not for us
			// (we pass here from the DriverManager.getConnection below)
			return null;
		}
		String myUrl = url;
		// we load first the driver class from the info or the url, to be sure that it will be found
		String proxiedDriver = info.getProperty("driver");
		if (proxiedDriver == null && myUrl != null) {
			// if not in the info, the driver class could also be passed at the end of the url, for example ...?driver=org.h2.Driver
			final int index = myUrl.indexOf("driver=");
			if (index != -1) {
				proxiedDriver = myUrl.substring(index + "driver=".length());
				myUrl = myUrl.substring(0, index - 1);
			}
		}
		if (proxiedDriver == null) {
			// if the driver is not defined in the info or in the url
			// it could still be found automatically if the driver is in the classpath
			// or (in WEB-INF/lib and if the jdbc drivers are not loaded by the JDK before this webapp)
			// but we don't want to create proxies and increment counts for the connections inside datasources
			// so we only accept and go further if driver is defined in the info or in the url
			return null;
		}
		try {
			// on utilise Thread.currentThread().getContextClassLoader() car le driver peut ne pas être
			// dans le même classLoader que les classes de javamelody
			// Class driverClass =
			Class.forName(proxiedDriver, true, Thread.currentThread().getContextClassLoader());
			// et non Class.forName(proxiedDriver);
		} catch (final ClassNotFoundException e) {
			throw new SQLException(e.getMessage(), e);
		}

		final Properties myInfo = (Properties) info.clone();
		myInfo.remove("driver");
		myInfo.put("javamelody", "false");
		Parameters.initJdbcDriverParameters(myUrl, myInfo);
		// we could call driverClass.newInstance().connect(myUrl, myInfo)
		// possibly by looking the driver which accepts the url in DriverManager.getDrivers()
		// but we prefer calling the standard DriverManager.getConnection(myUrl, myInfo)
		return JdbcWrapper.SINGLETON
				.createConnectionProxy(DriverManager.getConnection(myUrl, myInfo));
	}

	/** {@inheritDoc} */
	@Override
	public boolean acceptsURL(String url) throws SQLException {
		// test sur dbcp nécessaire pour le cas où le monitoring est utilisé avec le web.xml global
		// et le répertoire lib global de tomcat et également pour les anomalies 1&2 (sonar, grails)
		// (rq: Thread.currentThread().getStackTrace() a été mesuré à environ 3 micro-secondes)
		for (final StackTraceElement element : Thread.currentThread().getStackTrace()) {
			if (element.getClassName().endsWith("dbcp.BasicDataSource")) {
				return false;
			}
		}
		return true;
	}

	/** {@inheritDoc} */
	@Override
	public int getMajorVersion() {
		return -1;
	}

	/** {@inheritDoc} */
	@Override
	public int getMinorVersion() {
		return -1;
	}

	/** {@inheritDoc} */
	@Override
	public DriverPropertyInfo[] getPropertyInfo(String url, Properties info) throws SQLException {
		return new DriverPropertyInfo[0];
	}

	/** {@inheritDoc} */
	@Override
	public boolean jdbcCompliant() {
		return true;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[lastConnectUrl=" + Parameters.getLastConnectUrl()
				+ ", lastConnectInfo=" + Parameters.getLastConnectInfo() + ']';
	}

	/**
	 * Définition de la méthode getParentLogger ajoutée dans l'interface Driver en jdk 1.7.
	 * @return Logger
	 * @throws SQLFeatureNotSupportedException e
	 */
	@Override
	public Logger getParentLogger() throws SQLFeatureNotSupportedException {
		return Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);
	}
}
