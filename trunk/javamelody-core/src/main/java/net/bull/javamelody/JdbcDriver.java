/*
 * Copyright 2008-2010 by Emeric Vernat
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

import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.DriverPropertyInfo;
import java.sql.SQLException;
import java.util.Properties;

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

	private String lastConnectUrl;
	private Properties lastConnectInfo;

	// initialisation statique du driver
	static {
		try {
			DriverManager.registerDriver(SINGLETON);
		} catch (final SQLException e) {
			// ne peut arriver
			throw new IllegalStateException(e);
		}
	}

	void deregister() {
		try {
			DriverManager.deregisterDriver(this);
		} catch (final SQLException e) {
			// ne peut arriver
			throw new IllegalStateException(e);
		}
	}

	String getLastConnectUrl() {
		return lastConnectUrl;
	}

	Properties getLastConnectInfo() {
		return lastConnectInfo;
	}

	/** {@inheritDoc} */
	public Connection connect(String url, Properties info) throws SQLException {
		final String proxiedDriver = info.getProperty("driver");
		if (proxiedDriver == null) {
			// si pas de propriété driver alors ce n'est pas pour nous
			// (on passe ici lors du DriverManager.getConnection ci-dessous)
			return null;
		}
		try {
			Class.forName(proxiedDriver);
		} catch (final ClassNotFoundException e) {
			// Rq: le constructeur de SQLException avec message et cause n'existe qu'en jdk 1.6
			final SQLException ex = new SQLException(e.getMessage());
			ex.initCause(e);
			throw ex; // NOPMD
		}
		final Properties tmp = (Properties) info.clone();
		tmp.remove("driver");
		lastConnectUrl = url;
		lastConnectInfo = tmp;
		return JdbcWrapper.SINGLETON.createConnectionProxy(DriverManager.getConnection(url, tmp));
	}

	/** {@inheritDoc} */
	public boolean acceptsURL(String url) throws SQLException {
		// test sur dbcp nécessaire pour le cas où le monitoring est utilisé avec le web.xml global
		// et le répertoire lib global de tomcat et également pour les anomalies 1&2 (sonar, grails)
		for (final StackTraceElement element : Thread.currentThread().getStackTrace()) {
			if (element.getClassName().endsWith("dbcp.BasicDataSource")) {
				return false;
			}
		}
		return true;
	}

	/** {@inheritDoc} */
	public int getMajorVersion() {
		return -1;
	}

	/** {@inheritDoc} */
	public int getMinorVersion() {
		return -1;
	}

	/** {@inheritDoc} */
	public DriverPropertyInfo[] getPropertyInfo(String url, Properties info) throws SQLException {
		return new DriverPropertyInfo[0];
	}

	/** {@inheritDoc} */
	public boolean jdbcCompliant() {
		return true;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return getClass().getSimpleName() + "[lastConnectUrl=" + getLastConnectUrl()
				+ ", lastConnectInfo=" + getLastConnectInfo() + ']';
	}
}
