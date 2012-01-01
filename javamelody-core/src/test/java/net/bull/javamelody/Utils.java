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

import java.util.HashSet;

import javax.servlet.FilterConfig;
import javax.servlet.ServletContext;

import org.jrobin.core.RrdBackendFactory;
import org.jrobin.core.RrdException;
import org.jrobin.core.RrdFileBackendFactory;

/**
 * Classe utilitaire pour les tests unitaires.
 * @author Emeric Vernat
 */
final class Utils {
	private static final String SYSTEM_ACTIONS_PROPERTY_NAME = Parameters.PARAMETER_SYSTEM_PREFIX
			+ Parameter.SYSTEM_ACTIONS_ENABLED.getCode();

	static {
		// pour les tests unitaires, on utilise des backends "FILE" et non "NIO" pour que les tests s'exécutent plus vite
		try {
			RrdBackendFactory.setDefaultFactory(RrdFileBackendFactory.NAME);
		} catch (final RrdException e) {
			throw new IllegalStateException(e);
		}
	}

	private Utils() {
		super();
	}

	static void setProperty(Parameter parameter, String value) {
		setProperty(Parameters.PARAMETER_SYSTEM_PREFIX + parameter.getCode(), value);
	}

	static void setProperty(String string, String value) {
		if (value == null) {
			System.getProperties().remove(string);
		} else {
			System.setProperty(string, value);
		}
	}

	static void initialize() {
		for (final Object systemProperty : new HashSet<Object>(System.getProperties().keySet())) {
			if (systemProperty.toString().startsWith(Parameters.PARAMETER_SYSTEM_PREFIX)) {
				System.getProperties().remove(systemProperty.toString());
			}
		}
		Parameters.initialize((FilterConfig) null);
		Parameters.initialize((ServletContext) null);
		// pour avoir les informations sur les connections, l'initialisation de la classe JdbcWrapper
		// doit se faire avec les actions systèmes activées
		System.setProperty(SYSTEM_ACTIONS_PROPERTY_NAME, "true");
		JdbcWrapper.USED_CONNECTION_INFORMATIONS.clear();
		System.getProperties().remove(SYSTEM_ACTIONS_PROPERTY_NAME);
	}
}
