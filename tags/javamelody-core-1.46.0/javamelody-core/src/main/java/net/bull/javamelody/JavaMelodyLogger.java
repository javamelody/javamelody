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

import javax.servlet.http.HttpServletRequest;

/**
 * Interface commune aux 3 implémentations de Loggers JavaMelody.
 * @author lastmike, Emeric Vernat
 */
public interface JavaMelodyLogger {
	/**
	 * Nom du logger interne.
	 */
	String INTERNAL_LOGGER_NAME = "net.bull.javamelody";

	/**
	 * Log interne en niveau debug.
	 * @param msg Message
	 */
	void debug(String msg);

	/**
	 * Log interne en niveau debug.
	 * @param msg Message
	 * @param throwable Throwable
	 */
	void debug(String msg, Throwable throwable);

	/**
	 * Log interne en niveau info.
	 * @param msg Message
	 * @param throwable Throwable
	 */
	void info(String msg, Throwable throwable);

	/**
	 * Log interne en niveau warn.
	 * @param msg Message
	 * @param throwable Throwable
	 */
	void warn(String msg, Throwable throwable);

	/**
	 * Log les détails de l'exécution d'une requête http en niveau info.
	 * @param httpRequest Requête http
	 * @param requestName Nom de la requête
	 * @param duration Durée
	 * @param systemError Si erreur systême
	 * @param responseSize Taille de la réponse
	 * @param loggerName Nom du logger à utiliser
	 */
	void logHttpRequest(HttpServletRequest httpRequest, String requestName, long duration,
			boolean systemError, int responseSize, String loggerName);
}
