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

import java.sql.Statement;
import java.util.Date;

/**
 * Implémentation test de bean Spring.
 * (implémente une interface pour permettre l'AOP Spring car sinon il faut CGLIB)
 * @author Emeric Vernat
 */
// on pourrait utiliser l'annotation @MonitoredWithSpring,
// mais MonitoredWithInterfacePointcut suffit dans spring-context.xml
public class SpringTestFacadeImpl implements SpringTestFacade {
	/**
	 * {@inheritDoc}
	 */
	public Date nowWithSql() throws Exception {
		final javax.sql.DataSource ds = (javax.sql.DataSource) new javax.naming.InitialContext()
				.lookup("java:comp/env/jdbc/TestDB");
		final java.sql.Connection connection = ds.getConnection();
		connection.setAutoCommit(false);
		try {
			final Statement statement = connection.createStatement();
			try {
				// 1 seconde pour avoir une requête sql
				statement.executeQuery("select sleep(.01)");
				for (int i = 0; i < 5; i++) {
					statement.executeQuery("select sleep(.02)");
				}
			} finally {
				statement.close();
			}
		} finally {
			connection.rollback();
			connection.close();
		}

		return new Date();
	}

	/**
	 * {@inheritDoc}
	 */
	public Date now() {
		return new Date();
	}

	/**
	 * {@inheritDoc}
	 */
	public void throwError() {
		throw new Error("test");
	}
}
