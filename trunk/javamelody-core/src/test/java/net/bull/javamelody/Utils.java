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

/**
 * Classe utilitaire pour les tests unitaires.
 * @author Emeric Vernat
 */
final class Utils {
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
		JdbcWrapper.USED_CONNECTION_INFORMATIONS.clear();
	}
}
